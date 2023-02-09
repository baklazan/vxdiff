use super::range_map::RangeMap;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Prev,
    Next,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BorderSide {
    First,
    Last,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Nid {
    Basic(usize),
    Cover(usize),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpanderId(pub Vec<u8>);

pub trait Nodeish {
    fn get_owned_byte_offsets(&self) -> Option<[Range<usize>; 2]>;
}

struct NodeMeta<N> {
    node: N,
    // usize::MAX if none
    spatial_file_id: usize,
    cover: Option<usize>,
}

struct CoverMeta<N> {
    node: N,
    covered_first: usize,
    covered_last: usize,
}

struct FileMeta {
    is_open: bool,
    header_nid: Nid,
    content_nid_range: Range<usize>,
    expanded_byte_sets: [RangeMap<()>; 2],
    byte_to_nid_maps: [RangeMap<Nid>; 2],
}

pub struct GenericDocument<N> {
    nodes: Vec<NodeMeta<N>>,
    covers: HashMap<usize, CoverMeta<N>>,
    next_cover_id: usize,
    files: Vec<FileMeta>,
    original_expanders: HashMap<ExpanderId, Range<usize>>,
}

pub struct FrozenDocument {
    open_files: Vec<bool>,
    expanders: HashMap<ExpanderId, Vec<bool>>,
}

impl<N: Nodeish> GenericDocument<N> {
    pub fn get(&self, nid: Nid) -> &N {
        match nid {
            Nid::Basic(nid) => &self.nodes[nid].node,
            Nid::Cover(cid) => &self.covers[&cid].node,
        }
    }

    pub fn get_expanded_byte_set(&self, file_id: usize, side: usize) -> &RangeMap<()> {
        &self.files[file_id].expanded_byte_sets[side]
    }

    pub fn get_byte_to_nid_map(&self, file_id: usize, side: usize) -> &RangeMap<Nid> {
        &self.files[file_id].byte_to_nid_maps[side]
    }

    pub fn first_visible_node(&self, side: BorderSide) -> Option<Nid> {
        match side {
            BorderSide::First => self.next_visible_node(Nid::Basic(usize::MAX), Direction::Next),
            BorderSide::Last => self.next_visible_node(Nid::Basic(self.nodes.len()), Direction::Prev),
        }
    }

    pub fn next_visible_node(&self, nid: Nid, dir: Direction) -> Option<Nid> {
        let one = match dir {
            Direction::Prev => usize::MAX,
            Direction::Next => 1,
        };
        let mut pos = match (nid, dir) {
            (Nid::Basic(nid), _) => nid,
            (Nid::Cover(cid), Direction::Prev) => self.covers[&cid].covered_first,
            (Nid::Cover(cid), Direction::Next) => self.covers[&cid].covered_last,
        };
        pos = pos.wrapping_add(one);
        loop {
            if pos >= self.nodes.len() {
                return None;
            }
            let f = self.nodes[pos].spatial_file_id;
            if f != usize::MAX {
                let fm = &self.files[f];
                if !fm.is_open && fm.content_nid_range.contains(&pos) {
                    pos = match dir {
                        Direction::Prev => fm.content_nid_range.start - 1,
                        Direction::Next => fm.content_nid_range.end,
                    };
                    continue;
                }
            }
            return match self.nodes[pos].cover {
                Some(cid) => Some(Nid::Cover(cid)),
                None => Some(Nid::Basic(pos)),
            };
        }
    }

    pub fn compare_nodes(&self, a: Nid, b: Nid) -> Ordering {
        let process = |nid| match nid {
            Nid::Basic(nid) => nid,
            Nid::Cover(cid) => self.covers[&cid].covered_first,
        };
        process(a).cmp(&process(b))
    }

    #[allow(dead_code)] // TODO
    pub fn is_visible(&self, nid: Nid) -> bool {
        let (basic, pos) = match nid {
            Nid::Basic(nid) => (true, nid),
            Nid::Cover(cid) => (false, self.covers[&cid].covered_first),
        };
        if basic && self.nodes[pos].cover.is_some() {
            return false;
        }
        let f = self.nodes[pos].spatial_file_id;
        if f != usize::MAX {
            let fm = &self.files[f];
            if !fm.is_open && fm.content_nid_range.contains(&pos) {
                return false;
            }
        }
        true
    }

    pub fn get_expander_hidden_count(&self, expander_nid: Nid) -> usize {
        let Nid::Cover(cid) = expander_nid else { panic!("not Nid::Cover") };
        let meta = &self.covers[&cid];
        meta.covered_last - meta.covered_first + 1
    }

    pub fn expand(&mut self, expander_nid: Nid, count: usize, dir: Direction) -> Nid {
        let Nid::Cover(cid) = expander_nid else { panic!("not Nid::Cover") };
        let meta = self.covers.get_mut(&cid).unwrap();
        let first = meta.covered_first;
        let last = meta.covered_last;
        let hidden_count = last - first + 1;
        if hidden_count >= count + 2 {
            match dir {
                Direction::Prev => {
                    meta.covered_first += count;
                    self.uncover_nodes(first, first + count - 1);
                }
                Direction::Next => {
                    meta.covered_last -= count;
                    self.uncover_nodes(last - count + 1, last);
                }
            }
            Nid::Cover(cid)
        } else {
            self.uncover_nodes(first, last);
            self.covers.remove(&cid);
            Nid::Basic(first)
        }
    }

    fn uncover_nodes(&mut self, first: usize, last: usize) {
        for pos in first..=last {
            self.nodes[pos].cover = None;
            if let Some(ranges) = self.nodes[pos].node.get_owned_byte_offsets() {
                let file_meta = &mut self.files[self.nodes[pos].spatial_file_id];
                for side in 0..2 {
                    file_meta.expanded_byte_sets[side].set(ranges[side].clone(), Some(()));
                    file_meta.byte_to_nid_maps[side].set(ranges[side].clone(), Some(Nid::Basic(pos)));
                }
            }
        }
    }

    pub fn get_open_file(&self, file_id: usize) -> bool {
        self.files[file_id].is_open
    }

    pub fn set_open_file(&mut self, file_id: usize, value: bool) {
        self.files[file_id].is_open = value;
    }

    #[allow(dead_code)] // TODO
    pub fn find_file_header(&self, file_id: usize) -> Nid {
        self.files[file_id].header_nid
    }

    pub fn get_spatial_file_id(&self, nid: Nid) -> Option<usize> {
        let pos = match nid {
            Nid::Basic(nid) => nid,
            Nid::Cover(cid) => self.covers[&cid].covered_first,
        };
        let f = self.nodes[pos].spatial_file_id;
        if f == usize::MAX {
            None
        } else {
            Some(f)
        }
    }

    pub fn freeze(&self) -> FrozenDocument {
        let mapper = |(k, v): (&ExpanderId, &Range<usize>)| -> (ExpanderId, Vec<bool>) {
            let v = self.nodes[v.clone()].iter().map(|node| node.cover.is_some()).collect();
            (k.clone(), v)
        };
        FrozenDocument {
            open_files: self.files.iter().map(|f| f.is_open).collect(),
            expanders: self.original_expanders.iter().map(mapper).collect(),
        }
    }
}

pub struct GenericDocumentBuilder<N> {
    doc: GenericDocument<N>,
}

impl<N: Nodeish> GenericDocumentBuilder<N> {
    pub fn new() -> Self {
        Self {
            doc: GenericDocument {
                nodes: Default::default(),
                files: Default::default(),
                covers: Default::default(),
                next_cover_id: Default::default(),
                original_expanders: Default::default(),
            },
        }
    }

    pub fn next_nid(&self) -> usize {
        self.doc.nodes.len()
    }

    pub fn current_file_id(&self) -> usize {
        self.doc.files.len() - 1
    }

    pub fn add_node(&mut self, node: N) {
        let spatial_file_id = self.doc.files.len() - 1;
        if let Some(ranges) = node.get_owned_byte_offsets() {
            let file_meta = &mut self.doc.files[spatial_file_id];
            for side in 0..2 {
                file_meta.expanded_byte_sets[side].set(ranges[side].clone(), Some(()));
                file_meta.byte_to_nid_maps[side].set(ranges[side].clone(), Some(Nid::Basic(self.doc.nodes.len())));
            }
        }
        self.doc.nodes.push(NodeMeta {
            node,
            spatial_file_id,
            cover: None,
        });
        self.doc.files[spatial_file_id].content_nid_range.end = self.doc.nodes.len();
    }

    pub fn add_file(&mut self, is_open: bool, header_node: N) {
        self.doc.files.push(FileMeta {
            is_open,
            header_nid: Nid::Basic(self.doc.nodes.len()),
            content_nid_range: (self.doc.nodes.len() + 1)..(self.doc.nodes.len() + 1),
            expanded_byte_sets: Default::default(),
            byte_to_nid_maps: Default::default(),
        });
        self.add_node(header_node);
    }

    pub fn add_expander(&mut self, first: usize, last: usize, node: N) {
        let cid = self.doc.next_cover_id;
        self.doc.next_cover_id += 1;

        assert!(first <= last);
        for nid in first..=last {
            assert_eq!(
                self.doc.nodes[nid].spatial_file_id,
                self.doc.nodes[first].spatial_file_id
            );
            assert!(self.doc.nodes[nid].node.get_owned_byte_offsets().is_some());
            assert!(self.doc.nodes[nid].cover.is_none());
            self.doc.nodes[nid].cover = Some(cid);
        }
        self.doc.covers.insert(
            cid,
            CoverMeta {
                node,
                covered_first: first,
                covered_last: last,
            },
        );
        for side in 0..2 {
            let start = self.doc.nodes[first].node.get_owned_byte_offsets().unwrap()[side].start;
            let end = self.doc.nodes[last].node.get_owned_byte_offsets().unwrap()[side].end;
            let file_id = self.doc.nodes[first].spatial_file_id;
            self.doc.files[file_id].expanded_byte_sets[side].set(start..end, None);
            self.doc.files[file_id].byte_to_nid_maps[side].set(start..end, Some(Nid::Cover(cid)));
        }
    }

    pub fn add_original_expander(&mut self, expander_id: ExpanderId, range: Range<usize>) {
        self.doc.original_expanders.insert(expander_id, range);
    }

    pub fn into_inner(self) -> GenericDocument<N> {
        self.doc
    }
}

pub struct FrozenDocumentReader<'a>(pub Option<&'a FrozenDocument>);

impl<'a> FrozenDocumentReader<'a> {
    pub fn was_file_open(&self, file_id: usize) -> Option<bool> {
        self.0.map(|frozen| frozen.open_files[file_id])
    }

    pub fn get_expander_state(&self, expander_id: &ExpanderId) -> Option<Vec<bool>> {
        self.0.and_then(|frozen| frozen.expanders.get(expander_id).cloned())
    }

    pub fn assert_files_length(&self, expected: usize) {
        self.0.map(|frozen| assert_eq!(frozen.open_files.len(), expected));
    }
}
