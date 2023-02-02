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
pub struct Nid(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpanderId(pub Vec<u8>);

pub trait Nodeish {
    fn get_owned_byte_offsets(&self) -> Option<[Range<usize>; 2]>;
}

struct NodeMeta<N> {
    node: N,
    // usize::MAX if none
    spatial_file_id: usize,
    collapsed: bool,
    // only meaningful if collapsed is true and either sibling's collapsed is false
    collapsed_from_to: usize,
    replacement_expander: Option<N>,
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
    files: Vec<FileMeta>,
    original_expanders: HashMap<ExpanderId, Range<usize>>,
}

pub struct FrozenDocument {
    open_files: Vec<bool>,
    expanders: HashMap<ExpanderId, Vec<bool>>,
}

impl<N: Nodeish> GenericDocument<N> {
    pub fn get(&self, nid: Nid) -> &N {
        let meta = &self.nodes[nid.0];
        meta.replacement_expander.as_ref().unwrap_or(&meta.node)
    }

    pub fn get_expanded_byte_set(&self, file_id: usize, side: usize) -> &RangeMap<()> {
        &self.files[file_id].expanded_byte_sets[side]
    }

    pub fn get_byte_to_nid_map(&self, file_id: usize, side: usize) -> &RangeMap<Nid> {
        &self.files[file_id].byte_to_nid_maps[side]
    }

    pub fn first_visible_node(&self, side: BorderSide) -> Option<Nid> {
        match side {
            BorderSide::First => self.next_visible_node(Nid(usize::MAX), Direction::Next),
            BorderSide::Last => self.next_visible_node(Nid(self.nodes.len()), Direction::Prev),
        }
    }

    pub fn next_visible_node(&self, nid: Nid, dir: Direction) -> Option<Nid> {
        let one = match dir {
            Direction::Prev => usize::MAX,
            Direction::Next => 1,
        };
        let mut pos = nid.0.wrapping_add(one);
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
            if self.nodes[pos].collapsed {
                pos = self.nodes[pos].collapsed_from_to.wrapping_add(one);
                continue;
            }
            return Some(Nid(pos));
        }
    }

    pub fn compare_nodes(&self, a: Nid, b: Nid) -> Ordering {
        a.0.cmp(&b.0)
    }

    pub fn is_visible(&self, nid: Nid) -> bool {
        if self.nodes[nid.0].collapsed {
            return false;
        }
        let f = self.nodes[nid.0].spatial_file_id;
        if f != usize::MAX {
            let fm = &self.files[f];
            if !fm.is_open && fm.content_nid_range.contains(&nid.0) {
                return false;
            }
        }
        true
    }

    fn get_expander_last(&self, expander_nid: Nid) -> usize {
        if let Some(node) = self.nodes.get(expander_nid.0 + 1) {
            if node.collapsed {
                return node.collapsed_from_to;
            }
        }
        expander_nid.0
    }

    pub fn get_expander_hidden_count(&self, expander_nid: Nid) -> usize {
        assert!(self.nodes[expander_nid.0].replacement_expander.is_some());
        assert!(!self.nodes[expander_nid.0].collapsed);
        self.get_expander_last(expander_nid) - expander_nid.0 + 1
    }

    pub fn expand(&mut self, expander_nid: Nid, count: usize, dir: Direction) -> Nid {
        let expander_payload = self.nodes[expander_nid.0].replacement_expander.take().unwrap();
        let first = expander_nid.0;
        let last = self.get_expander_last(expander_nid);
        let hidden_count = last - first + 1;
        if hidden_count >= count + 2 {
            match dir {
                Direction::Prev => {
                    self.unhide_nodes(first, first + count - 1);
                    self.hide_nodes(first + count, last, expander_payload)
                }
                Direction::Next => {
                    self.unhide_nodes(last - count + 1, last);
                    self.hide_nodes(first, last - count, expander_payload)
                }
            }
        } else {
            self.unhide_nodes(first, last);
            expander_nid
        }
    }

    fn unhide_nodes(&mut self, first: usize, last: usize) {
        for pos in first..=last {
            self.nodes[pos].collapsed = false;
            self.nodes[pos].collapsed_from_to = usize::MAX;
            self.nodes[pos].replacement_expander = None;
            if let Some(ranges) = self.nodes[pos].node.get_owned_byte_offsets() {
                let file_meta = &mut self.files[self.nodes[pos].spatial_file_id];
                for side in 0..2 {
                    file_meta.expanded_byte_sets[side].set(ranges[side].clone(), Some(()));
                    file_meta.byte_to_nid_maps[side].set(ranges[side].clone(), Some(Nid(pos)));
                }
            }
        }
    }

    fn hide_nodes(&mut self, first: usize, last: usize, expander_payload: N) -> Nid {
        assert!(first <= last);
        self.nodes[first].collapsed = false;
        self.nodes[first].collapsed_from_to = usize::MAX;
        self.nodes[first].replacement_expander = Some(expander_payload);
        for nid in (first + 1)..=last {
            self.nodes[nid].collapsed = true;
        }
        if first != last {
            self.nodes[first + 1].collapsed_from_to = last;
            self.nodes[last].collapsed_from_to = first + 1;
        }
        for side in 0..2 {
            let start = self.nodes[first].node.get_owned_byte_offsets().unwrap()[side].start;
            let end = self.nodes[last].node.get_owned_byte_offsets().unwrap()[side].end;
            let file_id = self.nodes[first].spatial_file_id;
            self.files[file_id].expanded_byte_sets[side].set(start..end, None);
            self.files[file_id].byte_to_nid_maps[side].set(start..end, Some(Nid(first)));
        }
        Nid(first)
    }

    pub fn get_open_file(&self, file_id: usize) -> bool {
        self.files[file_id].is_open
    }

    pub fn set_open_file(&mut self, file_id: usize, value: bool) {
        self.files[file_id].is_open = value;
    }

    pub fn find_file_header(&self, file_id: usize) -> Nid {
        self.files[file_id].header_nid
    }

    pub fn get_spatial_file_id(&self, nid: Nid) -> Option<usize> {
        let f = self.nodes[nid.0].spatial_file_id;
        if f == usize::MAX {
            None
        } else {
            Some(f)
        }
    }

    pub fn freeze(&self) -> FrozenDocument {
        let mapper = |(k, v): (&ExpanderId, &Range<usize>)| -> (ExpanderId, Vec<bool>) {
            let v = self.nodes[v.clone()]
                .iter()
                .map(|node| node.collapsed || node.replacement_expander.is_some())
                .collect();
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
                file_meta.byte_to_nid_maps[side].set(ranges[side].clone(), Some(Nid(self.doc.nodes.len())));
            }
        }
        self.doc.nodes.push(NodeMeta {
            node,
            spatial_file_id,
            collapsed: false,
            collapsed_from_to: usize::MAX,
            replacement_expander: None,
        });
        self.doc.files[spatial_file_id].content_nid_range.end = self.doc.nodes.len();
    }

    pub fn add_file(&mut self, is_open: bool, header_node: N) {
        self.doc.files.push(FileMeta {
            is_open,
            header_nid: Nid(self.doc.nodes.len()),
            content_nid_range: (self.doc.nodes.len() + 1)..(self.doc.nodes.len() + 1),
            expanded_byte_sets: Default::default(),
            byte_to_nid_maps: Default::default(),
        });
        self.add_node(header_node);
    }

    pub fn hide_nodes(&mut self, first: usize, last: usize, expander_payload: N) {
        for nid in first..=last {
            assert_eq!(
                self.doc.nodes[nid].spatial_file_id,
                self.doc.nodes[first].spatial_file_id
            );
            assert!(self.doc.nodes[nid].node.get_owned_byte_offsets().is_some());
        }
        self.doc.hide_nodes(first, last, expander_payload);
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

    pub fn assert_files_length(&self, expected_length: usize) {
        self.0
            .map(|frozen| assert_eq!(frozen.open_files.len(), expected_length));
    }
}
