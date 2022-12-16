mod clipboard;
mod gui_layout;
mod line_layout;
mod range_map;
mod text_input;

use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use super::config::{Config, SearchCaseSensitivity, Style};
use clipboard::copy_to_clipboard;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use gui_layout::gui_layout;
use line_layout::{layout_line, LineCell, LineLayout};
use range_map::RangeMap;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::{self, Write as _};
use std::ops::{DerefMut as _, Range};
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use text_input::TextInput;
use tui::style::{Color, Modifier};
use unicode_width::UnicodeWidthStr as _;

fn vec_of<T>(length: usize, func: impl Fn() -> T) -> Vec<T> {
    (0..length).map(|_| func()).collect()
}

struct ExtendedDiffSectionSide<'a> {
    file_id: usize,
    byte_range: Range<usize>,
    highlight_ranges: &'a [Range<usize>],
}

struct ExtendedDiffFileSide<'a> {
    filename: &'a str,
    content: &'a str,
    line_offsets: Vec<usize>,
}

impl<'a> ExtendedDiffFileSide<'a> {
    fn byte_offset_to_line_number(&self, offset: usize) -> usize {
        if offset == 0 {
            // binary_search() does not guarantee which exact match is returned.
            1
        } else {
            self.line_offsets.binary_search(&offset).unwrap_or_else(|i| i - 1)
        }
    }
}

struct ExtendedDiff<'a> {
    section_sides: Vec<[Option<ExtendedDiffSectionSide<'a>>; 2]>,
    file_sides: Vec<[ExtendedDiffFileSide<'a>; 2]>,
    sections: &'a [Section],
    files: &'a [FileDiff],
}

impl<'a> ExtendedDiff<'a> {
    fn section_side(&self, section_id: usize, side: usize) -> &ExtendedDiffSectionSide<'a> {
        self.section_sides[section_id][side].as_ref().unwrap()
    }
}

fn make_extended_diff<'a>(
    diff: &'a Diff,
    file_input: &'a [[&'a str; 2]],
    file_names: &'a [[&'a str; 2]],
) -> ExtendedDiff<'a> {
    let mut extended_diff = ExtendedDiff {
        section_sides: vec_of(diff.sections.len(), Default::default),
        file_sides: (0..diff.files.len())
            .map(|file_id| {
                [0, 1].map(|side| ExtendedDiffFileSide {
                    filename: file_names[file_id][side],
                    content: file_input[file_id][side],
                    line_offsets: vec![0, 0],
                })
            })
            .collect(),
        sections: &diff.sections,
        files: &diff.files,
    };

    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        for side in 0..2 {
            for &(op, section_id) in ops {
                if op.movement()[side] == 0 {
                    continue;
                }
                let section_side = &diff.sections[section_id].sides[side];
                extended_diff.section_sides[section_id][side] = Some(ExtendedDiffSectionSide {
                    file_id,
                    byte_range: section_side.byte_range.clone(),
                    highlight_ranges: &section_side.highlight_ranges,
                });
            }
            for (i, c) in file_input[file_id][side].char_indices() {
                if c == '\n' {
                    extended_diff.file_sides[file_id][side].line_offsets.push(i + 1);
                }
            }
            if file_input[file_id][side].ends_with('\n') {
                extended_diff.file_sides[file_id][side].line_offsets.pop();
            }
        }
    }

    extended_diff
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Direction {
    Prev,
    Next,
}

use Direction::{Next, Prev};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BorderSide {
    First,
    Last,
}

use BorderSide::{First, Last};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HalfLineStyle {
    Equal,
    Change,
    Move,
    Phantom,
    Padding,
}

#[derive(Clone)]
enum TextSource {
    Section(usize),
    Fabricated(String),
}

#[derive(Clone)]
struct WrappedHalfLine {
    style: HalfLineStyle,
    offset_override_for_selection: Option<usize>,
    source: TextSource,
    offset: usize,
}

#[derive(Clone)]
enum UILine {
    Sides([WrappedHalfLine; 2]),
    FileHeaderLine(usize),
    ExpanderLine(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Nid(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LeafPosition {
    parent: Nid,
    line_index: usize,
}

#[derive(Clone)]
struct BranchNode {
    children: Vec<Nid>,
    visible: Range<usize>,
}

#[derive(Clone)]
struct PaddedGroupRawElement {
    style: HalfLineStyle,
    // TODO: Rename. It's also used for search and jumps (Some vs None).
    offset_override_for_selection: Option<usize>,
    source: TextSource,
}

#[derive(Clone)]
struct PaddedGroupNode {
    raw_elements: [Vec<PaddedGroupRawElement>; 2],
    end_offsets_for_selection: [usize; 2],
    cached_wrap: RefCell<Option<(usize, Vec<UILine>)>>,
}

#[derive(Clone)]
enum Node {
    Branch(BranchNode),
    PaddedGroup(PaddedGroupNode),
    FileHeaderLine(usize),
    ExpanderLine(usize),
}

impl Node {
    fn new_branch() -> Node {
        Node::Branch(BranchNode {
            children: Default::default(),
            visible: 0..0,
        })
    }

    fn as_branch(&self) -> &BranchNode {
        match self {
            Node::Branch(node) => node,
            _ => panic!("expected Branch node"),
        }
    }

    fn as_branch_mut(&mut self) -> &mut BranchNode {
        match self {
            Node::Branch(node) => node,
            _ => panic!("expected Branch node"),
        }
    }
}

struct NodeMeta {
    value: Node,
    parent: Option<Nid>,
    index_in_parent: usize,
}

struct Tree {
    nodes: Vec<NodeMeta>,
    root: Nid,
}

impl Tree {
    fn new(root_node: Node) -> Tree {
        Tree {
            nodes: vec![NodeMeta {
                value: root_node,
                parent: None,
                index_in_parent: usize::MAX,
            }],
            root: Nid(0),
        }
    }

    fn node(&self, nid: Nid) -> &Node {
        &self.nodes[nid.0].value
    }

    fn node_mut(&mut self, nid: Nid) -> &mut Node {
        &mut self.nodes[nid.0].value
    }

    fn node_meta(&self, nid: Nid) -> &NodeMeta {
        &self.nodes[nid.0]
    }

    fn parent(&self, nid: Nid) -> Option<Nid> {
        self.node_meta(nid).parent
    }

    fn sibling(&self, nid: Nid, dir: Direction) -> Option<Nid> {
        let my_meta = self.node_meta(nid);
        let parent = self.node(my_meta.parent?).as_branch();
        let next_index = match dir {
            Next => my_meta.index_in_parent + 1,
            Prev => my_meta.index_in_parent.wrapping_sub(1),
        };
        if parent.visible.contains(&next_index) {
            Some(parent.children[next_index])
        } else {
            None
        }
    }

    /// Returns the first/last visible child of `nid`, if it exists.
    fn bordering_child(&self, nid: Nid, side: BorderSide) -> Option<Nid> {
        if let Node::Branch(node) = self.node(nid) {
            if !node.visible.is_empty() {
                return Some(
                    node.children[match side {
                        First => node.visible.start,
                        Last => node.visible.end - 1,
                    }],
                );
            }
        }
        None
    }

    fn index_path(&self, nid: Nid) -> Vec<usize> {
        let mut result = vec![];
        let mut nid = nid;
        while let Some(parent) = self.parent(nid) {
            result.push(self.node_meta(nid).index_in_parent);
            nid = parent;
        }
        result.reverse();
        result
    }

    fn compare_nodes(&self, a: Nid, b: Nid) -> Ordering {
        if a == b {
            Ordering::Equal
        } else {
            self.index_path(a).cmp(&self.index_path(b))
        }
    }

    fn compare_leaves(&self, a: LeafPosition, b: LeafPosition) -> Ordering {
        if a.parent == b.parent {
            a.line_index.cmp(&b.line_index)
        } else {
            self.compare_nodes(a.parent, b.parent)
        }
    }

    fn add_child(&mut self, parent: Nid, value: Node) -> Nid {
        let nid = Nid(self.nodes.len());
        let mut parent_branch_node = self.node_mut(parent).as_branch_mut();
        parent_branch_node.visible.end += 1;
        let index_in_parent = parent_branch_node.children.len();
        parent_branch_node.children.push(nid);
        self.nodes.push(NodeMeta {
            value,
            parent: Some(parent),
            index_in_parent,
        });
        nid
    }

    fn add_children(&mut self, parent: Nid, values: Vec<Node>) {
        for value in values {
            self.add_child(parent, value);
        }
    }

    // Future note: We don't need removing nodes yet, but we could implement it as:
    // - replace the node in self.nodes with some kind of "dummy node"
    // - add the nid to a list of "unused nids", to be recycled in add_child
    // - fix `children` in the parent
    // - fix `index_in_parent` in all next siblings
}

fn add_padded_group_to_range_maps(
    diff: &ExtendedDiff,
    node: &Node,
    visible: bool,
    nid: Nid,
    visible_byte_sets: &mut [[RangeMap<()>; 2]],
    byte_to_nid_maps: &mut [[RangeMap<Nid>; 2]],
) {
    if let Node::PaddedGroup(node) = node {
        for side in 0..2 {
            for raw_element in &node.raw_elements[side] {
                if raw_element.offset_override_for_selection.is_none() {
                    if let TextSource::Section(section_id) = raw_element.source {
                        let section_side = diff.section_side(section_id, side);
                        let visibility = if visible { Some(()) } else { None };
                        visible_byte_sets[section_side.file_id][side].set(section_side.byte_range.clone(), visibility);
                        byte_to_nid_maps[section_side.file_id][side].set(section_side.byte_range.clone(), Some(nid));
                    }
                }
            }
        }
    } else {
        panic!("add_padded_group_to_range_maps needs a Node::PaddedGroup");
    }
}

fn build_initial_tree(config: &Config, diff: &ExtendedDiff) -> (Tree, Vec<[RangeMap<()>; 2]>, Vec<[RangeMap<Nid>; 2]>) {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum SectionType {
        MatchEqual,
        MatchUnequal,
        InsertDelete,
        Phantom,
    }

    let is_move = |(op, section_id): (DiffOp, usize)| -> bool {
        op != DiffOp::Match
            && diff.section_sides[section_id][0].is_some()
            && diff.section_sides[section_id][1].is_some()
    };

    let is_continuing_move = |a: (DiffOp, usize), b: (DiffOp, usize)| -> bool {
        let other_side = if a.0 == DiffOp::Insert { 0 } else { 1 };
        is_move(a)
            && is_move(b)
            && a.0 == b.0
            && diff.section_side(a.1, other_side).byte_range.end == diff.section_side(b.1, other_side).byte_range.start
    };

    let get_type = |(op, section_id): (DiffOp, usize)| -> SectionType {
        if op == DiffOp::Match {
            if diff.sections[section_id].equal {
                SectionType::MatchEqual
            } else {
                SectionType::MatchUnequal
            }
        } else {
            if config.phantom_rendering && is_move((op, section_id)) {
                SectionType::Phantom
            } else {
                SectionType::InsertDelete
            }
        }
    };

    struct PaddedGroupBuilder<'a> {
        diff: &'a ExtendedDiff<'a>,
        offsets: &'a mut [usize; 2],
        raw_elements: [Vec<PaddedGroupRawElement>; 2],
    }

    impl<'a> PaddedGroupBuilder<'a> {
        fn new<'b>(diff: &'b ExtendedDiff, offsets: &'b mut [usize; 2]) -> PaddedGroupBuilder<'b> {
            PaddedGroupBuilder {
                diff,
                offsets,
                raw_elements: [vec![], vec![]],
            }
        }

        fn add_fabricated(mut self, style: HalfLineStyle, side: usize, content: String) -> Self {
            self.raw_elements[side].push(PaddedGroupRawElement {
                style,
                offset_override_for_selection: Some(self.offsets[side]),
                source: TextSource::Fabricated(content),
            });
            self
        }

        fn add_section(
            mut self,
            styles: [HalfLineStyle; 2],
            (op, section_id): (DiffOp, usize),
            both_sides: bool,
        ) -> Self {
            for side in 0..2 {
                if both_sides || op.movement()[side] != 0 {
                    let style = styles[side];
                    let section_side = self.diff.section_side(section_id, side);
                    let offset_override_for_selection = if op.movement()[side] != 0 {
                        self.offsets[side] = section_side.byte_range.end;
                        None
                    } else {
                        Some(self.offsets[side])
                    };
                    self.raw_elements[side].push(PaddedGroupRawElement {
                        style,
                        offset_override_for_selection,
                        source: TextSource::Section(section_id),
                    });
                    let file_side = &self.diff.file_sides[section_side.file_id][side];
                    if !file_side.content[section_side.byte_range.clone()].ends_with('\n') {
                        self = self.add_fabricated(style, side, "No newline at end of file".to_string());
                    }
                }
            }
            self
        }

        fn build(self) -> Node {
            Node::PaddedGroup(PaddedGroupNode {
                raw_elements: self.raw_elements,
                end_offsets_for_selection: *self.offsets,
                cached_wrap: RefCell::new(None),
            })
        }
    }

    let get_description_of_move = |(op, section_id): (DiffOp, usize), render_side: usize| -> String {
        let other_side = if op == DiffOp::Delete { 1 } else { 0 };
        let section_other_side = diff.section_side(section_id, other_side);
        let start = section_other_side.byte_range.start;
        let file_other_side = &diff.file_sides[section_other_side.file_id][other_side];
        let filename = &file_other_side.filename;
        let line_number = file_other_side.byte_offset_to_line_number(start);
        let direction = match (other_side, render_side) {
            (0, 1) => "Moved from",
            (1, 0) => "Moved to",
            _ => "Preview of",
        };
        format!("{direction} {filename}:{line_number}")
    };

    struct FileBuilder<'a> {
        diff: &'a ExtendedDiff<'a>,
        tree: &'a mut Tree,
        visible_byte_sets: &'a mut [[RangeMap<()>; 2]],
        byte_to_nid_maps: &'a mut [[RangeMap<Nid>; 2]],
        offsets: [usize; 2],
        file_content_nid: Nid,
    }

    fn add_node_to_range_maps(b: &mut FileBuilder, nid: Nid) {
        match b.tree.node(nid) {
            Node::Branch(node) => {
                // Making a copy to avoid borrow issues with b.tree.
                let children = node.children[node.visible.clone()].to_vec();
                for child in children {
                    add_node_to_range_maps(b, child);
                }
            }
            node @ Node::PaddedGroup(_) => {
                add_padded_group_to_range_maps(b.diff, node, true, nid, b.visible_byte_sets, b.byte_to_nid_maps);
            }
            Node::FileHeaderLine(_) | Node::ExpanderLine(_) => unreachable!(),
        }
    }

    let build_match_equal = |b: &mut FileBuilder, ops: &[(DiffOp, usize)], is_first: bool, is_last: bool| {
        let length = ops.len();
        let mut padded_groups = vec![];
        for &op in ops {
            padded_groups.push(
                PaddedGroupBuilder::new(diff, &mut b.offsets)
                    .add_section([HalfLineStyle::Equal; 2], op, true)
                    .build(),
            );
        }

        let context_before = if is_first { 0 } else { config.context_lines };
        let context_after = if is_last { 0 } else { config.context_lines };
        if length > context_before + context_after + 1 {
            let upper_nid = b.tree.add_child(b.file_content_nid, Node::new_branch());
            b.tree.add_children(upper_nid, padded_groups.clone());
            b.tree.node_mut(upper_nid).as_branch_mut().visible = 0..context_before;
            add_node_to_range_maps(b, upper_nid);

            let hidden_count = length - context_before - context_after;
            let expander_nid = b.tree.add_child(b.file_content_nid, Node::ExpanderLine(hidden_count));
            for padded_group in &padded_groups[context_before..(length - context_after)] {
                add_padded_group_to_range_maps(
                    b.diff,
                    padded_group,
                    false,
                    expander_nid,
                    b.visible_byte_sets,
                    b.byte_to_nid_maps,
                );
            }

            let lower_nid = b.tree.add_child(b.file_content_nid, Node::new_branch());
            b.tree.add_children(lower_nid, padded_groups);
            b.tree.node_mut(lower_nid).as_branch_mut().visible = (length - context_after)..length;
            add_node_to_range_maps(b, lower_nid);
        } else {
            for padded_group in padded_groups {
                let nid = b.tree.add_child(b.file_content_nid, padded_group);
                add_node_to_range_maps(b, nid);
            }
        }
    };

    let build_match_unequal = |b: &mut FileBuilder, op: (DiffOp, usize)| {
        let nid = b.tree.add_child(
            b.file_content_nid,
            PaddedGroupBuilder::new(diff, &mut b.offsets)
                .add_section([HalfLineStyle::Change; 2], op, true)
                .build(),
        );
        add_node_to_range_maps(b, nid);
    };

    let build_insert_delete = |b: &mut FileBuilder, ops: &[(DiffOp, usize)]| {
        let mut pgb = PaddedGroupBuilder::new(diff, &mut b.offsets);
        for i in 0..ops.len() {
            let style = if is_move(ops[i]) {
                HalfLineStyle::Move
            } else {
                HalfLineStyle::Change
            };
            if is_move(ops[i]) && (i == 0 || !is_continuing_move(ops[i - 1], ops[i])) {
                let side = if ops[i].0 == DiffOp::Delete { 0 } else { 1 };
                pgb = pgb.add_fabricated(style, side, get_description_of_move(ops[i], side));
            }
            pgb = pgb.add_section([style; 2], ops[i], false);
        }
        let nid = b.tree.add_child(b.file_content_nid, pgb.build());
        add_node_to_range_maps(b, nid);
    };

    let build_phantom = |b: &mut FileBuilder, op: (DiffOp, usize), prev_op: Option<(DiffOp, usize)>| {
        let styles = if op.0 == DiffOp::Delete {
            [HalfLineStyle::Move, HalfLineStyle::Phantom]
        } else {
            [HalfLineStyle::Phantom, HalfLineStyle::Move]
        };
        if prev_op.is_none() || !is_continuing_move(prev_op.unwrap(), op) {
            let mut pgb = PaddedGroupBuilder::new(diff, &mut b.offsets);
            for side in 0..2 {
                pgb = pgb.add_fabricated(styles[side], side, get_description_of_move(op, side));
            }
            b.tree.add_child(b.file_content_nid, pgb.build());
        }
        let nid = b.tree.add_child(
            b.file_content_nid,
            PaddedGroupBuilder::new(diff, &mut b.offsets)
                .add_section(styles, op, true)
                .build(),
        );
        add_node_to_range_maps(b, nid);
    };

    let mut tree = Tree::new(Node::new_branch());
    let mut visible_byte_sets = vec_of(diff.files.len(), Default::default);
    let mut byte_to_nid_maps = vec_of(diff.files.len(), Default::default);

    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let file_nid = tree.add_child(tree.root, Node::new_branch());
        tree.add_child(file_nid, Node::FileHeaderLine(file_id));
        let file_content_nid = tree.add_child(file_nid, Node::new_branch());
        let is_open = config.open_all_files || diff.files.len() == 1;
        tree.node_mut(file_nid).as_branch_mut().visible = if is_open { 0..2 } else { 0..1 };

        let mut b = FileBuilder {
            diff,
            tree: &mut tree,
            visible_byte_sets: &mut visible_byte_sets,
            byte_to_nid_maps: &mut byte_to_nid_maps,
            offsets: [0; 2],
            file_content_nid,
        };
        let mut op_index = 0;
        while op_index < ops.len() {
            let begin = op_index;
            let section_type = get_type(ops[begin]);
            let mut end = begin + 1;
            if section_type == SectionType::MatchEqual || section_type == SectionType::InsertDelete {
                while end < ops.len() && get_type(ops[end]) == section_type {
                    end += 1;
                }
            }
            op_index = end;

            match section_type {
                SectionType::MatchEqual => build_match_equal(&mut b, &ops[begin..end], begin == 0, end == ops.len()),
                SectionType::MatchUnequal => build_match_unequal(&mut b, ops[begin]),
                SectionType::InsertDelete => build_insert_delete(&mut b, &ops[begin..end]),
                SectionType::Phantom => build_phantom(&mut b, ops[begin], begin.checked_sub(1).map(|prev| ops[prev])),
            }
        }
    }

    (tree, visible_byte_sets, byte_to_nid_maps)
}

fn get_file_id_from_tree_ancestors(tree: &Tree, mut nid: Nid) -> Option<usize> {
    loop {
        if let Node::Branch(node) = tree.node(nid) {
            if !node.children.is_empty() {
                if let &Node::FileHeaderLine(file_id) = tree.node(node.children[0]) {
                    return Some(file_id);
                }
            }
        }
        if let Some(parent_nid) = tree.parent(nid) {
            nid = parent_nid;
        } else {
            return None;
        }
    }
}

fn layout_diff_line(
    diff: &ExtendedDiff,
    side: usize,
    source: &TextSource,
    search_highlights: &[[Vec<Range<usize>>; 2]],
    offset: usize,
    wrap_width: usize,
) -> LineLayout {
    match source {
        &TextSource::Section(section_id) => {
            let section_side = diff.section_side(section_id, side);
            layout_line(
                diff.file_sides[section_side.file_id][side].content,
                section_side.highlight_ranges,
                search_highlights.get(section_side.file_id).map_or(&[], |a| &a[side]),
                offset,
                section_side.byte_range.end,
                wrap_width,
            )
        }
        TextSource::Fabricated(content) => layout_line(content, &[], &[], offset, content.len(), wrap_width),
    }
}

fn wrap_one_side(diff: &ExtendedDiff, node: &PaddedGroupNode, side: usize, wrap_width: usize) -> Vec<WrappedHalfLine> {
    let mut out: Vec<WrappedHalfLine> = vec![];
    for raw_element in &node.raw_elements[side] {
        let Range { start: mut pos, end } = match &raw_element.source {
            &TextSource::Section(section_id) => diff.section_side(section_id, side).byte_range.clone(),
            TextSource::Fabricated(content) => 0..content.len(),
        };

        while pos != end {
            let next =
                layout_diff_line(diff, side, &raw_element.source, &[], pos, wrap_width).offset_after_with_newline;
            let (slice_source, slice_offset) = match &raw_element.source {
                &TextSource::Section(section_id) => (TextSource::Section(section_id), pos),
                TextSource::Fabricated(content) => (TextSource::Fabricated(content[pos..next].to_string()), 0),
            };
            out.push(WrappedHalfLine {
                style: raw_element.style,
                offset_override_for_selection: raw_element.offset_override_for_selection,
                source: slice_source,
                offset: slice_offset,
            });
            pos = next;
        }
    }
    out
}

struct TreeView<'a> {
    diff: &'a ExtendedDiff<'a>,
    tree: &'a Tree,
    wrap_width: usize,
}

impl<'a> TreeView<'a> {
    /// Calls func with node's current UILine list, recomputing it if needed.
    /// Panics if node is a branch node.
    /// Don't call with_ui_lines recursively (from inside func).
    fn with_ui_lines<Ret>(&self, node: &Node, func: impl FnOnce(&[UILine]) -> Ret) -> Ret {
        match node {
            Node::Branch(_) => panic!("unexpected Branch node"),
            Node::PaddedGroup(node) => match node.cached_wrap.borrow_mut().deref_mut() {
                Some((w, l)) if *w == self.wrap_width => func(l),
                cached_wrap => {
                    let wrapped_sides =
                        [0, 1].map(|side| (side, wrap_one_side(self.diff, node, side, self.wrap_width)));
                    let len = std::cmp::max(wrapped_sides[0].1.len(), wrapped_sides[1].1.len());
                    let padded_wrapped_sides = wrapped_sides.map(|(side, whls)| {
                        // TODO: temporary padding indicators
                        let padding = WrappedHalfLine {
                            style: HalfLineStyle::Padding,
                            offset_override_for_selection: Some(node.end_offsets_for_selection[side]),
                            source: TextSource::Fabricated("@".to_string()),
                            offset: 0,
                        };
                        whls.into_iter().chain(std::iter::repeat(padding))
                    });
                    let [side0, side1] = padded_wrapped_sides;
                    let lines: Vec<UILine> = std::iter::zip(side0, side1)
                        .map(|(line0, line1)| UILine::Sides([line0, line1]))
                        .take(len)
                        .collect();
                    let result = func(&lines);
                    *cached_wrap = Some((self.wrap_width, lines));
                    result
                }
            },
            Node::FileHeaderLine(file_id) => func(&[UILine::FileHeaderLine(*file_id)]),
            Node::ExpanderLine(0) => func(&[]),
            Node::ExpanderLine(hidden_count) => func(&[UILine::ExpanderLine(*hidden_count)]),
        }
    }

    /// Returns the first/last visible leaf under `nid`, if it exists.
    fn bordering_leaf_under(&self, nid: Nid, side: BorderSide) -> Option<LeafPosition> {
        let node = self.tree.node(nid);
        if let Node::Branch(_) = node {
            let mut child_option_nid = self.tree.bordering_child(nid, side);
            while let Some(child_nid) = child_option_nid {
                if let Some(lp) = self.bordering_leaf_under(child_nid, side) {
                    return Some(lp);
                }
                let dir = match side {
                    First => Next,
                    Last => Prev,
                };
                child_option_nid = self.tree.sibling(child_nid, dir);
            }
            None
        } else {
            self.with_ui_lines(node, |lines| {
                if lines.is_empty() {
                    None
                } else {
                    Some(LeafPosition {
                        parent: nid,
                        line_index: match side {
                            First => 0,
                            Last => lines.len() - 1,
                        },
                    })
                }
            })
        }
    }

    /// Precondition: `leaf` is visible.
    /// Returns the next visible leaf in this direction. If this is the first/last leaf, returns None.
    fn next_leaf(&self, leaf: LeafPosition, dir: Direction) -> Option<LeafPosition> {
        let next_line_index = match dir {
            Next => leaf.line_index + 1,
            Prev => leaf.line_index.wrapping_sub(1),
        };
        let parent = self.tree.node(leaf.parent);
        if next_line_index < self.with_ui_lines(parent, |lines| lines.len()) {
            return Some(LeafPosition {
                parent: leaf.parent,
                line_index: next_line_index,
            });
        }

        // Loop invariant: We already tried everything under `nid`.
        let mut nid = leaf.parent;
        loop {
            if let Some(sibling) = self.tree.sibling(nid, dir) {
                let side = match dir {
                    Next => First,
                    Prev => Last,
                };
                if let Some(lp) = self.bordering_leaf_under(sibling, side) {
                    return Some(lp);
                } else {
                    nid = sibling;
                }
            } else {
                if let Some(parent) = self.tree.parent(nid) {
                    nid = parent;
                } else {
                    return None;
                }
            }
        }
    }

    /// Precondition: `leaf` is visible.
    /// Returns the next visible leaf moving in `direction` by `count`. If it reaches the
    /// first/last visible leaf of the tree, it stays there.
    /// Returns the new position and how much we moved.
    fn next_leaves_max(&self, leaf: LeafPosition, offset: usize, dir: Direction) -> LeafPosition {
        let mut leaf = leaf;
        for _ in 0..offset {
            if let Some(next) = self.next_leaf(leaf, dir) {
                leaf = next;
            } else {
                return leaf;
            }
        }
        leaf
    }

    /// Preconditions: `from` and `to` are visible. `from <= to` in tree order.
    fn leaf_distance(&self, from: LeafPosition, to: LeafPosition) -> usize {
        let mut from = from;
        let mut result = 0;
        while from != to {
            from = self.next_leaf(from, Next).unwrap();
            result += 1;
        }
        result
    }
}

type TheResult = Result<(), Box<dyn Error>>;

fn print_plainly(tree_view: &TreeView, nid: Nid, output: &mut impl io::Write) -> TheResult {
    let node = tree_view.tree.node(nid);
    if let Node::Branch(node) = node {
        // clone() explanation: https://stackoverflow.com/q/63685464
        for i in node.visible.clone() {
            print_plainly(tree_view, node.children[i], output)?;
        }
        return Ok(());
    }
    tree_view.with_ui_lines(node, |lines| -> TheResult {
        for line in lines {
            match line {
                UILine::Sides(sides) => {
                    let render_half = |side: usize| -> String {
                        let layout = layout_diff_line(
                            tree_view.diff,
                            side,
                            &sides[side].source,
                            &[],
                            sides[side].offset,
                            tree_view.wrap_width,
                        );
                        fn show_cell(cell: &LineCell) -> String {
                            format!(
                                "{}{}{}",
                                if cell.highlight { "\x1b[1m" } else { "" },
                                cell.egc,
                                if cell.highlight { "\x1b[0m" } else { "" }
                            )
                        }
                        layout.cells.iter().map(show_cell).collect::<String>()
                            + &" ".repeat(tree_view.wrap_width - layout.cells.len())
                    };
                    writeln!(output, "{} | {}", render_half(0), render_half(1))?;
                }
                UILine::FileHeaderLine(file_id) => {
                    writeln!(output, "file #{}", file_id)?;
                }
                UILine::ExpanderLine(hidden_count) => {
                    writeln!(output, "...{} hidden lines...", hidden_count)?;
                }
            }
        }
        Ok(())
    })
}

pub fn print_side_by_side_diff_plainly(
    diff: &Diff,
    mut config: Config,
    file_input: &[[&str; 2]],
    file_names: &[[&str; 2]],
    output: &mut impl io::Write,
) -> TheResult {
    // Gotta expand the file headers in order to see any content.
    config.open_all_files = true;

    let diff = make_extended_diff(diff, file_input, file_names);
    let (tree, _, _) = build_initial_tree(&config, &diff);

    let tree_view = TreeView {
        tree: &tree,
        diff: &diff,
        wrap_width: 80,
    };

    print_plainly(&tree_view, tree.root, output)
}

fn compute_wrap_width(layout: &[Range<usize>; 10]) -> usize {
    layout[3].end - layout[3].start
}

fn main_gui_layout(terminal_width: usize, line_number_width: usize, show_cursor: bool) -> [Range<usize>; 10] {
    // TODO: Configurable wrap_width behavior (e.g. fixed 80, multiples of 10, fluid)
    let constraints = [
        Some(if show_cursor { 1 } else { 0 }),
        Some(line_number_width),
        Some(1),
        None,
        Some(1),
        Some(1),
        Some(1),
        None,
        Some(1),
        Some(line_number_width),
    ];
    gui_layout(constraints, 0..terminal_width)
}

#[derive(Clone)]
enum MouseCell {
    Inert,
    Button(Rc<dyn Fn(&mut State)>),
    Text { file_id: usize, side: usize, offset: usize },
}

#[derive(Clone)]
struct ButtonInfo {
    id: (Nid, usize),
    func: Rc<dyn Fn(&mut State)>,
}

struct RenderedInfo {
    mouse_cells: Vec<Vec<MouseCell>>,
    mouse_pseudocell_after: Vec<[MouseCell; 2]>,
    terminal_cursor: Option<(u16, u16)>,
    button_hints: Vec<Option<ButtonInfo>>,
}

struct SelectionState {
    file_id: usize,
    side: usize,
    selecting: bool,
    start_offset: usize,
    current_offset: usize,
}

struct SearchQuery {
    pattern: String,
    case_sensitive: bool,
    regexp: bool,
}

#[derive(PartialEq, Eq)]
struct SearchMatch {
    file_id: usize,
    offset: usize,
    parent: Nid,
}

enum EventResult {
    Nothing,
    Bell,
    Quit,
}

enum GuiMode {
    Default,
    Jump {
        input: TextInput,
    },
    Search {
        input: TextInput,
        direction: Direction,
        case_sensitivity: SearchCaseSensitivity,
        regexp: bool,
        old_scroll_pos: LeafPosition,
        old_cursor_pos: LeafPosition,
        // new_scroll_pos: LeafPosition,
        // new_cursor_pos: LeafPosition,
    },
}

struct State<'a> {
    config: Config,
    tree: Tree,
    visible_byte_sets: Vec<[RangeMap<()>; 2]>,
    byte_to_nid_maps: Vec<[RangeMap<Nid>; 2]>,
    diff: &'a ExtendedDiff<'a>,
    line_number_width: usize,
    scroll_pos: LeafPosition,
    cursor_pos: LeafPosition,
    wrap_width: usize,
    scroll_height: usize,
    selection: Option<SelectionState>,
    search_query: Option<SearchQuery>,
    search_matches: [Vec<SearchMatch>; 2],
    search_highlights: Vec<[Vec<Range<usize>>; 2]>,
    mode: GuiMode,
    focused_side: usize,
    button_hint_chars: Vec<char>,
    reverse_button_hint_chars: HashMap<char, usize>,
}

fn update_selection_position(selection: &mut SelectionState, rendered: &RenderedInfo, x: usize, y: usize) {
    let mut mx = x;
    let mut my = y;
    while my < rendered.mouse_cells.len() {
        if mx < rendered.mouse_cells[my].len() {
            if let MouseCell::Text { file_id, side, offset } = rendered.mouse_cells[my][mx] {
                if file_id == selection.file_id && side == selection.side {
                    selection.current_offset = offset;
                    return;
                }
            }
            mx += 1;
        } else {
            if let MouseCell::Text { file_id, side, offset } = rendered.mouse_pseudocell_after[my][selection.side] {
                if file_id == selection.file_id && side == selection.side {
                    selection.current_offset = offset;
                    return;
                }
            }
            mx = 0;
            my += 1;
        }
    }
    let mut my = y;
    while my > 0 {
        my -= 1;
        if let MouseCell::Text { file_id, side, offset } = rendered.mouse_pseudocell_after[my][selection.side] {
            if file_id == selection.file_id && side == selection.side {
                selection.current_offset = offset;
                return;
            }
        }
    }
}

fn is_search_case_sensitive(sensitivity: SearchCaseSensitivity, pattern: &str) -> bool {
    match sensitivity {
        SearchCaseSensitivity::CaseSensitive => true,
        SearchCaseSensitivity::CaseInsensitive => false,
        SearchCaseSensitivity::DependsOnPattern => pattern.chars().any(char::is_uppercase),
    }
}

fn tui_style(style: Style) -> tui::style::Style {
    let mut modifier = Modifier::empty();
    modifier.set(Modifier::BOLD, style.bold.unwrap_or(false));
    modifier.set(Modifier::UNDERLINED, style.underlined.unwrap_or(false));
    modifier.set(Modifier::DIM, style.dim.unwrap_or(false));
    modifier.set(Modifier::ITALIC, style.italic.unwrap_or(false));
    modifier.set(Modifier::CROSSED_OUT, style.crossed_out.unwrap_or(false));
    tui::style::Style {
        fg: style.fg,
        bg: style.bg,
        add_modifier: modifier,
        sub_modifier: Modifier::empty(),
    }
}

fn usto16(number: usize) -> u16 {
    u16::try_from(number).unwrap()
}

fn u16tos(number: u16) -> usize {
    usize::try_from(number).unwrap()
}

fn buffer_write(buffer: &mut tui::buffer::Buffer, x: usize, y: usize, string: impl AsRef<str>, style: Style) {
    buffer.set_string(usto16(x), usto16(y), string, tui_style(style));
}

impl<'a> State<'a> {
    fn tree_view(&self) -> TreeView {
        TreeView {
            tree: &self.tree,
            diff: self.diff,
            wrap_width: self.wrap_width,
        }
    }

    fn move_pos(&self, pos: LeafPosition, offset: usize, dir: Direction) -> LeafPosition {
        self.tree_view().next_leaves_max(pos, offset, dir)
    }

    fn scroll_by(&mut self, offset: usize, dir: Direction) {
        self.scroll_pos = self.move_pos(self.scroll_pos, offset, dir);
        self.cursor_pos = self.move_pos(self.cursor_pos, offset, dir);
        self.fix_scroll_invariants(false);
    }

    fn move_cursor_by(&mut self, offset: usize, dir: Direction) {
        if !self.config.show_cursor {
            return;
        }
        self.cursor_pos = self.move_pos(self.cursor_pos, offset, dir);
        self.fix_scroll_invariants(true);
    }

    fn toggle_open_file(&mut self, file_header_nid: Nid) {
        let &Node::FileHeaderLine(file_id) = self.tree.node(file_header_nid) else { return };
        let file_nid = self.tree.parent(file_header_nid).unwrap();
        let file_branch_node = self.tree.node_mut(file_nid).as_branch_mut();
        file_branch_node.visible.end = match file_branch_node.visible.end {
            1 => 2,
            2 => 1,
            _ => panic!("logic error: bad file_branch_node.visible.end"),
        };
        if get_file_id_from_tree_ancestors(&self.tree, self.cursor_pos.parent) == Some(file_id) {
            self.cursor_pos = LeafPosition {
                parent: file_header_nid,
                line_index: 0,
            };
        }
        self.fix_scroll_invariants(false);
    }

    fn expand_sibling(&mut self, expander: Nid, count: usize, dir: Direction) {
        let branch_nid = self.tree.sibling(expander, dir).unwrap();
        let branch_node = self.tree.node_mut(branch_nid).as_branch_mut();
        let old_visible = branch_node.visible.clone();
        let newly_visible = match dir {
            Prev => {
                branch_node.visible.end += count;
                old_visible.end..branch_node.visible.end
            }
            Next => {
                branch_node.visible.start -= count;
                branch_node.visible.start..old_visible.start
            }
        };
        let branch_node = self.tree.node(branch_nid).as_branch();
        for &child in &branch_node.children[newly_visible] {
            add_padded_group_to_range_maps(
                self.diff,
                self.tree.node(child),
                true,
                child,
                &mut self.visible_byte_sets,
                &mut self.byte_to_nid_maps,
            );
        }
    }

    fn expand_expander(&mut self, mut expander: LeafPosition, count: usize, dir: Direction) {
        let expander_nid = expander.parent;
        let hidden_count = match self.tree.node(expander_nid) {
            Node::ExpanderLine(hidden_count) => *hidden_count,
            _ => return,
        };
        assert!(self.tree.compare_leaves(self.scroll_pos, expander) != Ordering::Greater);
        let expander_distance = self.tree_view().leaf_distance(self.scroll_pos, expander);
        if hidden_count >= count + 2 {
            *self.tree.node_mut(expander_nid) = Node::ExpanderLine(hidden_count - count);
            self.expand_sibling(expander_nid, count, dir);
        } else {
            self.expand_sibling(expander_nid, hidden_count, Next);
            let this_leaf = expander;
            let next_leaf = self.tree_view().next_leaf(this_leaf, Next).unwrap();
            if self.scroll_pos == this_leaf {
                self.scroll_pos = next_leaf;
            }
            if self.cursor_pos == this_leaf {
                self.cursor_pos = next_leaf;
            }
            *self.tree.node_mut(expander_nid) = Node::ExpanderLine(0);
            expander = next_leaf;
        }
        self.scroll_pos = self.move_pos(expander, expander_distance, Prev);
        self.fix_scroll_invariants(false);
        self.perform_search();
    }

    fn fix_scroll_invariants(&mut self, prefer_changing_scroll: bool) {
        // scroll_pos <= tree.end - (scroll_height-1)
        let top_bottom_dist = self.scroll_height - 1;
        let bottom_pos = self.move_pos(self.scroll_pos, top_bottom_dist, Next);
        self.scroll_pos = self.move_pos(bottom_pos, top_bottom_dist, Prev);

        // scroll_pos <= cursor_pos <= bottom_pos
        if self.tree.compare_leaves(self.cursor_pos, self.scroll_pos) == Ordering::Less {
            if prefer_changing_scroll {
                self.scroll_pos = self.cursor_pos;
            } else {
                self.cursor_pos = self.scroll_pos;
            }
        }
        if self.tree.compare_leaves(self.cursor_pos, bottom_pos) == Ordering::Greater {
            if prefer_changing_scroll {
                self.scroll_pos = self.move_pos(self.cursor_pos, top_bottom_dist, Prev);
            } else {
                self.cursor_pos = bottom_pos;
            }
        }
    }

    fn resize(&mut self, wrap_width: usize, scroll_height: usize) {
        if (self.wrap_width, self.scroll_height) == (wrap_width, scroll_height) {
            return;
        }
        self.wrap_width = wrap_width;
        self.scroll_height = scroll_height;
        fn fix_position(tree_view: &TreeView, pos: LeafPosition) -> LeafPosition {
            let parent = tree_view.tree.node(pos.parent);
            let new_len = tree_view.with_ui_lines(parent, |lines| lines.len());
            LeafPosition {
                parent: pos.parent,
                line_index: std::cmp::min(pos.line_index, new_len - 1),
            }
        }
        self.scroll_pos = fix_position(&self.tree_view(), self.scroll_pos);
        self.cursor_pos = fix_position(&self.tree_view(), self.cursor_pos);
        self.fix_scroll_invariants(false);
    }

    fn perform_search(&mut self) {
        self.search_matches = [vec![], vec![]];
        self.search_highlights = vec_of(self.diff.files.len(), Default::default);

        let (pattern, case_sensitive, regexp) = 'out: {
            match self.mode {
                GuiMode::Search {
                    ref input,
                    case_sensitivity,
                    regexp,
                    ..
                } if self.config.search_incremental => {
                    let pattern = input.get_content();
                    if !pattern.is_empty() {
                        let case_sensitive = is_search_case_sensitive(case_sensitivity, pattern);
                        break 'out (pattern, case_sensitive, regexp);
                    }
                }
                _ => {}
            }
            if let Some(ref sq) = self.search_query {
                break 'out (&sq.pattern, sq.case_sensitive, sq.regexp);
            }
            return;
        };

        let (pattern, regexp) = if !regexp && !case_sensitive {
            (Cow::Owned(regex::escape(pattern)), true)
        } else {
            (Cow::Borrowed(pattern), regexp)
        };

        enum Matcher<'a> {
            Regex(regex::Regex),
            Memmem(memchr::memmem::Finder<'a>),
        }

        let matcher = if regexp {
            let regex = regex::RegexBuilder::new(&pattern)
                .case_insensitive(!case_sensitive)
                .multi_line(true)
                .build();

            match regex {
                Ok(regex) => Matcher::Regex(regex),
                Err(_) => {
                    // TODO: show the error (if non-incremental search).
                    return;
                }
            }
        } else {
            Matcher::Memmem(memchr::memmem::Finder::new(pattern.as_bytes()))
        };

        for file_id in 0..self.diff.files.len() {
            for side in 0..2 {
                for (range, ()) in self.visible_byte_sets[file_id][side].ranges() {
                    let subcontent = &self.diff.file_sides[file_id][side].content[range.clone()];

                    let mut process_match = |start: usize, end: usize| {
                        if start != end {
                            let highlights = &mut self.search_highlights[file_id][side];
                            if highlights.last().map(|range| range.end) == Some(start) {
                                highlights.last_mut().unwrap().end = end;
                            } else {
                                highlights.push(start..end);
                            }
                        }
                        let offset = if start == range.end {
                            // Deal with patterns like "^" or "$" which can match an empty string
                            // at the end of `subcontent`. This offset technically belongs to the
                            // following expander line's Nid, or at EOF, to no Nid.
                            //
                            // For normal cases where subcontent ends with a newline, just ignore
                            // the match. E.g. regex "$" also matches before the newline anyway.
                            // Otherwise, go back one character. So regex "$" can jump to the end
                            // of the last line, even if it's wrapped and has no newline.
                            //
                            // I don't care about "\A" and "\z" as long as they don't panic.
                            if subcontent.ends_with('\n') {
                                return;
                            }
                            start - subcontent.chars().next_back().unwrap().len_utf8()
                        } else {
                            start
                        };
                        let my_match = SearchMatch {
                            file_id,
                            offset,
                            parent: self.byte_to_nid_maps[file_id][side].get(offset).unwrap(),
                        };
                        match self.search_matches[side].last() {
                            Some(last_match) if *last_match == my_match => {}
                            _ => self.search_matches[side].push(my_match),
                        }
                    };

                    match matcher {
                        Matcher::Regex(ref regex) => {
                            for mat in regex.find_iter(subcontent) {
                                process_match(range.start + mat.start(), range.start + mat.end());
                            }
                        }
                        Matcher::Memmem(ref finder) => {
                            for mat in finder.find_iter(subcontent.as_bytes()) {
                                process_match(range.start + mat, range.start + mat + pattern.len());
                            }
                        }
                    }
                }
            }
        }
    }

    fn find_leaf_position_within_parent(&self, side: usize, parent: Nid, byte_offset: usize) -> LeafPosition {
        let line_index = self.tree_view().with_ui_lines(self.tree.node(parent), |lines| {
            lines
                .binary_search_by(|ui_line| match ui_line {
                    UILine::Sides(sides) => {
                        let whl = &sides[side];
                        whl.offset_override_for_selection
                            .unwrap_or(whl.offset)
                            .cmp(&byte_offset)
                    }
                    UILine::ExpanderLine(_) => Ordering::Equal,
                    _ => unreachable!(),
                })
                .unwrap_or_else(|i| i - 1)
        });
        LeafPosition { parent, line_index }
    }

    fn find_leaf_position_of_line(&self, file_id: usize, side: usize, line_number: usize) -> LeafPosition {
        let line_offsets = &self.diff.file_sides[file_id][side].line_offsets;
        let byte_offset = line_offsets[std::cmp::min(line_number, line_offsets.len() - 1)];
        let parent = self.byte_to_nid_maps[file_id][side].get(byte_offset).unwrap();
        self.find_leaf_position_within_parent(side, parent, byte_offset)
    }

    fn next_or_same_search_result(&self, start: LeafPosition, dir: Direction) -> Option<LeafPosition> {
        let candidates = [0, 1].map(|side| {
            let compare_with_start = |probe: &SearchMatch| {
                self.tree.compare_nodes(probe.parent, start.parent).then_with(|| {
                    self.find_leaf_position_within_parent(side, probe.parent, probe.offset)
                        .line_index
                        .cmp(&start.line_index)
                })
            };
            let matches = &self.search_matches[side];
            let match_index = match matches.binary_search_by(compare_with_start) {
                Ok(exact_index) => exact_index,
                Err(between_index) => match dir {
                    Prev => between_index.wrapping_sub(1),
                    Next => between_index,
                },
            };
            let my_match = matches.get(match_index)?;
            Some(self.find_leaf_position_within_parent(side, my_match.parent, my_match.offset))
            // TODO: Should it return results in closed files?
        });
        match candidates {
            [None, None] => None,
            [Some(x), None] => Some(x),
            [None, Some(y)] => Some(y),
            [Some(x), Some(y)] => match (dir, self.tree.compare_leaves(x, y)) {
                (Prev, Ordering::Less | Ordering::Equal) => Some(y),
                (Prev, Ordering::Greater) => Some(x),
                (Next, Ordering::Less | Ordering::Equal) => Some(x),
                (Next, Ordering::Greater) => Some(y),
            },
        }
    }

    fn go_to_next_result(&mut self, at_least: usize, dir: Direction) {
        let start = self.move_pos(self.scroll_pos, at_least, dir);
        if let Some(next) = self.next_or_same_search_result(start, dir) {
            // TODO: Open the file if it's closed.
            self.scroll_pos = next;
            self.cursor_pos = next;
            // TODO: Commented out: self.fix_scroll_invariants(true);
        }
    }

    fn enter_search_mode(&mut self, direction: Direction) {
        self.mode = GuiMode::Search {
            input: TextInput::new(),
            direction,
            case_sensitivity: self.config.search_default_case_sensitivity,
            regexp: self.config.search_default_regexp,
            old_scroll_pos: self.scroll_pos,
            old_cursor_pos: self.cursor_pos,
            // new_scroll_pos: self.scroll_pos,
            // new_cursor_pos: self.cursor_pos,
        };
    }

    fn handle_key_event(&mut self, event: KeyEvent, rendered: &RenderedInfo) -> Result<EventResult, Box<dyn Error>> {
        if event.modifiers.contains(KeyModifiers::ALT) {
            if let KeyCode::Char(c) = event.code {
                if let Some(&hint) = self.reverse_button_hint_chars.get(&c.to_ascii_uppercase()) {
                    if let Some(button) = rendered.button_hints[hint].as_ref() {
                        (button.func)(self);
                        return Ok(EventResult::Nothing);
                    }
                }
            }
            return Ok(EventResult::Bell);
        }
        match event.code {
            KeyCode::Esc | KeyCode::Char('q') => return Ok(EventResult::Quit),
            KeyCode::Char('c') if event.modifiers.contains(KeyModifiers::CONTROL) => return Ok(EventResult::Quit),
            KeyCode::Up => self.scroll_by(1, Prev),
            KeyCode::Down => self.scroll_by(1, Next),
            KeyCode::PageUp | KeyCode::Char('b') => self.scroll_by(self.scroll_height, Prev),
            KeyCode::PageDown | KeyCode::Char(' ') => self.scroll_by(self.scroll_height, Next),
            KeyCode::Char('w') | KeyCode::Char('k') => self.move_cursor_by(1, Prev),
            KeyCode::Char('s') | KeyCode::Char('j') => self.move_cursor_by(1, Next),
            // TODO: temporary key assignment for 'z', 'x', 'c'
            KeyCode::Char('z') if self.config.show_cursor => self.toggle_open_file(self.cursor_pos.parent),
            // TODO: configurable number of lines
            // TODO: if at beginning xor end (zero context lines on one side), don't show that button
            KeyCode::Char('x') if self.config.show_cursor => self.expand_expander(self.cursor_pos, 3, Prev),
            KeyCode::Char('c') if self.config.show_cursor => self.expand_expander(self.cursor_pos, 3, Next),
            KeyCode::Char('p') if event.modifiers.contains(KeyModifiers::CONTROL) => panic!("intentional panic"),
            KeyCode::F(3) if event.modifiers.contains(KeyModifiers::SHIFT) => self.go_to_next_result(1, Prev),
            KeyCode::F(3) => self.go_to_next_result(1, Next),
            KeyCode::Char('N') => self.go_to_next_result(1, Prev),
            KeyCode::Char('n') => self.go_to_next_result(1, Next),
            KeyCode::Char('g') => {
                self.mode = GuiMode::Jump {
                    input: TextInput::new(),
                };
            }
            KeyCode::Char('/') => self.enter_search_mode(Next),
            KeyCode::Char('?') => self.enter_search_mode(Prev),
            _ => return Ok(EventResult::Bell),
        }
        Ok(EventResult::Nothing)
    }

    fn handle_mouse_event(
        &mut self,
        event: MouseEvent,
        rendered: &RenderedInfo,
    ) -> Result<EventResult, Box<dyn Error>> {
        let x = std::cmp::min(u16tos(event.column), rendered.mouse_cells[0].len() - 1);
        let y = std::cmp::min(u16tos(event.row), rendered.mouse_cells.len() - 1);
        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => match rendered.mouse_cells[y][x] {
                MouseCell::Text { file_id, side, offset } => {
                    self.selection = Some(SelectionState {
                        file_id,
                        side,
                        selecting: true,
                        start_offset: offset,
                        current_offset: offset,
                    });
                }
                MouseCell::Button(ref fun) => fun(self),
                MouseCell::Inert => {}
            },
            MouseEventKind::Drag(MouseButton::Left) => match self.selection {
                Some(ref mut selection) if selection.selecting => {
                    update_selection_position(selection, rendered, x, y);
                }
                _ => {}
            },
            MouseEventKind::Up(MouseButton::Left) => match self.selection {
                Some(ref mut selection) if selection.selecting => {
                    update_selection_position(selection, rendered, x, y);
                    selection.selecting = false;
                    let from = std::cmp::min(selection.start_offset, selection.current_offset);
                    let to = std::cmp::max(selection.start_offset, selection.current_offset);
                    copy_to_clipboard(
                        &self.config.clipboard_mechanism,
                        &self.diff.file_sides[selection.file_id][selection.side].content[from..to],
                    )?;
                }
                _ => {}
            },
            MouseEventKind::ScrollUp => self.scroll_by(self.config.mouse_wheel_scroll_lines, Prev),
            MouseEventKind::ScrollDown => self.scroll_by(self.config.mouse_wheel_scroll_lines, Next),
            _ => {}
        }
        Ok(EventResult::Nothing)
    }

    fn handle_event(&mut self, event: Event, rendered: &RenderedInfo) -> Result<EventResult, Box<dyn Error>> {
        match self.mode {
            GuiMode::Default => match event {
                Event::Key(e) => self.handle_key_event(e, rendered),
                Event::Mouse(e) => self.handle_mouse_event(e, rendered),
                _ => Ok(EventResult::Nothing),
            },
            GuiMode::Jump { ref mut input } => {
                // TODO: Shouldn't we also handle mouse events in jump mode?
                if let Event::Key(event) = &event {
                    match event.code {
                        KeyCode::Esc => {
                            self.mode = GuiMode::Default;
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Backspace if input.get_content().is_empty() => {
                            self.mode = GuiMode::Default;
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Char('c') if event.modifiers.contains(KeyModifiers::CONTROL) => {
                            return Ok(EventResult::Quit)
                        }
                        KeyCode::Char('l') => {
                            self.focused_side = 0;
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Char('r') => {
                            self.focused_side = 1;
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Tab => {
                            self.focused_side = 1 - self.focused_side;
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Enter => {
                            if let Ok(line_number) = input.get_content().parse::<usize>() {
                                let file_id =
                                    get_file_id_from_tree_ancestors(&self.tree, self.cursor_pos.parent).unwrap_or(0);
                                let pos = self.find_leaf_position_of_line(file_id, self.focused_side, line_number);
                                // TODO: Open the file if it's closed.
                                self.scroll_pos = pos;
                                self.cursor_pos = pos;
                                self.fix_scroll_invariants(true);
                            }
                            self.mode = GuiMode::Default;
                            return Ok(EventResult::Nothing);
                        }
                        _ => {}
                    }
                }
                let validator = |string: &str| string.chars().all(|c| c.is_ascii_digit());
                match input.handle_event(&event, validator) {
                    text_input::TextInputEventResult::Handled => Ok(EventResult::Nothing),
                    text_input::TextInputEventResult::Unhandled => Ok(EventResult::Bell),
                }
            }
            GuiMode::Search {
                ref mut input,
                ref mut case_sensitivity,
                ref mut regexp,
                direction,
                old_scroll_pos,
                old_cursor_pos,
                ..
            } => {
                // TODO: Shouldn't we also handle mouse events in search mode?
                if let Event::Key(kevent) = &event {
                    match kevent.code {
                        KeyCode::Esc => {
                            self.scroll_pos = old_scroll_pos;
                            self.cursor_pos = old_cursor_pos;
                            self.mode = GuiMode::Default;
                            self.perform_search();
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Backspace if input.get_content().is_empty() => {
                            self.scroll_pos = old_scroll_pos;
                            self.cursor_pos = old_cursor_pos;
                            self.mode = GuiMode::Default;
                            self.perform_search();
                            return Ok(EventResult::Nothing);
                        }
                        KeyCode::Char('c') if kevent.modifiers.contains(KeyModifiers::CONTROL) => {
                            return Ok(EventResult::Quit);
                        }
                        KeyCode::Char('s') if kevent.modifiers.contains(KeyModifiers::CONTROL) => {
                            *case_sensitivity = match is_search_case_sensitive(*case_sensitivity, input.get_content()) {
                                true => SearchCaseSensitivity::CaseInsensitive,
                                false => SearchCaseSensitivity::CaseSensitive,
                            };
                        }
                        KeyCode::Char('r') if kevent.modifiers.contains(KeyModifiers::CONTROL) => {
                            *regexp = !*regexp;
                        }
                        KeyCode::Enter => {
                            let pattern = input.get_content();
                            let at_least;
                            if pattern.is_empty() {
                                at_least = 1;
                            } else {
                                self.search_query = Some(SearchQuery {
                                    pattern: pattern.to_owned(),
                                    case_sensitive: is_search_case_sensitive(*case_sensitivity, pattern),
                                    regexp: *regexp,
                                });
                                at_least = 0;
                            }
                            self.scroll_pos = old_scroll_pos;
                            self.cursor_pos = old_cursor_pos;
                            self.mode = GuiMode::Default;
                            self.perform_search();
                            self.go_to_next_result(at_least, direction);
                            return Ok(EventResult::Nothing);
                        }
                        // TODO: up/down = search history
                        _ => match input.handle_event(&event, |_| true) {
                            text_input::TextInputEventResult::Handled => {}
                            text_input::TextInputEventResult::Unhandled => return Ok(EventResult::Bell),
                        },
                    }
                    if self.config.search_incremental {
                        self.perform_search();
                        // TODO: move?
                    }
                }
                Ok(EventResult::Nothing)
            }
        }
    }

    fn render_half_line(
        &self,
        pos: LeafPosition,
        sides: &[WrappedHalfLine; 2],
        side: usize,
        content_screen_x: usize,
        line_number_screen_x: usize,
        screen_y: usize,
        buffer: &mut tui::buffer::Buffer,
        rendered: &mut RenderedInfo,
    ) {
        let theme = &self.config.theme;
        let whl = &sides[side];
        let file_id_for_rendering: Option<usize>;

        match whl.source {
            TextSource::Section(section_id) => {
                let file_id = self.diff.section_side(section_id, side).file_id;
                file_id_for_rendering = Some(file_id);

                let file_side = &self.diff.file_sides[file_id][side];
                let line_number = file_side.byte_offset_to_line_number(whl.offset);
                let mut line_number_str = line_number.to_string();
                if whl.offset != file_side.line_offsets[line_number] {
                    line_number_str = "+".repeat(line_number_str.len());
                }
                let line_number_style = match whl.style {
                    HalfLineStyle::Phantom => theme.line_numbers_phantom,
                    _ => theme.line_numbers_default,
                };
                let screen_x = line_number_screen_x - line_number_str.len();
                buffer_write(buffer, screen_x, screen_y, line_number_str, line_number_style);
            }
            TextSource::Fabricated(..) => file_id_for_rendering = None,
        }

        let layout = layout_diff_line(
            self.diff,
            side,
            &whl.source,
            &self.search_highlights,
            whl.offset,
            self.wrap_width,
        );

        // TODO: The loop in get_file_id_from_tree_ancestors runs at most a few times, but it might be nicer to avoid it.
        let file_id_for_selection = get_file_id_from_tree_ancestors(&self.tree, pos.parent)
            .expect("UILine::Sides was used outside of a file_content_node");

        let eol_cell = LineCell {
            egc: " ".to_string(),
            highlight: self.config.highlight_newlines && layout.newline_highlight,
            search_highlight: false,
            fabricated_symbol: false,
            offset: layout.offset_after_except_newline,
        };

        rendered.mouse_pseudocell_after[screen_y][side] = MouseCell::Text {
            file_id: file_id_for_selection,
            side,
            offset: whl
                .offset_override_for_selection
                .unwrap_or(layout.offset_after_with_newline),
        };

        for x in 0..self.wrap_width {
            let LineCell {
                egc,
                highlight,
                search_highlight,
                fabricated_symbol,
                offset,
            } = layout.cells.get(x).unwrap_or(&eol_cell).clone();

            let selected = match &self.selection {
                Some(selection) => {
                    file_id_for_selection == selection.file_id
                        && file_id_for_rendering == Some(selection.file_id)
                        && whl.offset_override_for_selection.is_none()
                        && side == selection.side
                        && offset >= std::cmp::min(selection.start_offset, selection.current_offset)
                        && offset < std::cmp::max(selection.start_offset, selection.current_offset)
                }
                None => false,
            };

            let screen_x = content_screen_x + x;

            rendered.mouse_cells[screen_y][screen_x] = MouseCell::Text {
                file_id: file_id_for_selection,
                side,
                offset: whl.offset_override_for_selection.unwrap_or(offset),
            };

            // tui-rs encodes double-width characters as one normal cell and
            // one default cell (containing a " "). See Buffer::set_stringn()
            // and Cell::reset(). We don't call set_stringn here, but let's try
            // to match its result. The " " in the second cell won't be printed
            // because Buffer::diff skips over it.
            let mut cell = tui::buffer::Cell::default();
            if !egc.is_empty() {
                let mut style = Style::default();
                style = style.patch(match (whl.style, side != 0) {
                    (HalfLineStyle::Equal, _) => theme.text_equal,
                    (HalfLineStyle::Padding, _) => theme.text_padding,
                    (HalfLineStyle::Change, false) => theme.text_change_old,
                    (HalfLineStyle::Change, true) => theme.text_change_new,
                    (HalfLineStyle::Move, false) => theme.text_move_old,
                    (HalfLineStyle::Move, true) => theme.text_move_new,
                    (HalfLineStyle::Phantom, false) => theme.text_phantom_old,
                    (HalfLineStyle::Phantom, true) => theme.text_phantom_new,
                });
                if highlight {
                    style = style.patch(match (whl.style, side != 0) {
                        (HalfLineStyle::Equal, _) => Style::default(),
                        (HalfLineStyle::Padding, _) => Style::default(),
                        (HalfLineStyle::Change, false) => theme.highlight_change_old,
                        (HalfLineStyle::Change, true) => theme.highlight_change_new,
                        (HalfLineStyle::Move, false) => theme.highlight_move_old,
                        (HalfLineStyle::Move, true) => theme.highlight_move_new,
                        (HalfLineStyle::Phantom, false) => theme.highlight_phantom_old,
                        (HalfLineStyle::Phantom, true) => theme.highlight_phantom_new,
                    });
                }
                if fabricated_symbol {
                    style = style.patch(theme.fabricated_symbol);
                }
                if search_highlight {
                    style = style.patch(theme.search_highlight);
                }
                if selected {
                    // TODO: Do some magic color blending.
                    style = style.patch(theme.select_highlight);
                }
                cell.symbol = egc;
                cell.set_style(tui_style(style));
            }
            *buffer.get_mut(usto16(screen_x), usto16(screen_y)) = cell;
        }
    }

    fn render(
        &mut self,
        term_width: usize,
        term_height: usize,
        old_rendered: Option<&RenderedInfo>,
        buffer: &mut tui::buffer::Buffer,
    ) -> RenderedInfo {
        let layout = main_gui_layout(term_width, self.line_number_width, self.config.show_cursor);
        self.resize(compute_wrap_width(&layout), term_height);
        let [xcursor, xnuml, _, xmainl, _, xsep, _, xmainr, _, xnumr] = layout;

        let mut rendered = RenderedInfo {
            mouse_cells: vec![vec![MouseCell::Inert; term_width]; term_height],
            mouse_pseudocell_after: vec![[MouseCell::Inert, MouseCell::Inert]; term_height],
            terminal_cursor: None,
            button_hints: vec![None; self.button_hint_chars.len()],
        };

        type ButtonDef = ((Nid, usize), Range<usize>, usize, String, Rc<dyn Fn(&mut State)>);
        let mut buttons: Vec<ButtonDef> = vec![];

        let mut pos = self.scroll_pos;
        let real_scroll_height = match self.mode {
            GuiMode::Default => self.scroll_height,
            GuiMode::Jump { .. } => self.scroll_height - 1,
            GuiMode::Search { .. } => self.scroll_height - 1,
        };
        for y in 0..real_scroll_height {
            if pos == self.cursor_pos && self.config.show_cursor {
                buffer_write(buffer, xcursor.start, y, ">", self.config.theme.cursor);
            }
            let parent_node = self.tree.node(pos.parent);
            let tree_view = self.tree_view();
            tree_view.with_ui_lines(parent_node, |lines| match lines[pos.line_index] {
                UILine::Sides(ref sides) => {
                    for side in 0..2 {
                        self.render_half_line(
                            pos,
                            sides,
                            side,
                            [xmainl.start, xmainr.start][side],
                            [xnuml.end, xnumr.end][side],
                            y,
                            buffer,
                            &mut rendered,
                        );
                    }
                    buffer_write(buffer, xsep.start, y, tui::symbols::line::VERTICAL, Style::default());
                }
                UILine::FileHeaderLine(file_id) => {
                    let file_header_nid = pos.parent;
                    let file_nid = self.tree.parent(file_header_nid).unwrap();
                    let is_open = match self.tree.node(file_nid).as_branch().visible.end {
                        1 => false,
                        2 => true,
                        _ => panic!("unexpected node.visible of FileHeaderLine's parent"),
                    };
                    let string = format!(
                        "{} vs {} (press z to {})",
                        self.diff.file_sides[file_id][0].filename,
                        self.diff.file_sides[file_id][1].filename,
                        if is_open { "close" } else { "open" }
                    );
                    let style = Style {
                        fg: Some(Color::Black),
                        bg: Some(if is_open { Color::Green } else { Color::Red }),
                        ..Style::default()
                    };
                    buffer_write(buffer, 1, y, string, style);
                    let button_text = if is_open { "Close".to_owned() } else { "Open".to_owned() };
                    let button_pos = (term_width - 1 - button_text.width() - 4)..(term_width - 1);
                    let button_fn = Rc::new(move |s: &mut State| s.toggle_open_file(pos.parent));
                    buttons.push(((pos.parent, 0), button_pos, y, button_text, button_fn));
                }
                UILine::ExpanderLine(hidden_count) => {
                    let hline = tui::symbols::line::HORIZONTAL.repeat(term_width - 1);
                    buffer_write(buffer, 1, y, hline, Style::default());
                    let up_text = "+3↑".to_owned();
                    let all_text = format!("Expand {hidden_count} matching lines");
                    let down_text = "+3↓".to_owned();
                    let constraints = [
                        Some(1),
                        None,
                        Some(up_text.width() + 4),
                        Some(1),
                        Some(all_text.width() + 4),
                        Some(1),
                        Some(down_text.width() + 4),
                        None,
                    ];
                    let [_, _, up_pos, _, all_pos, _, down_pos, _] = gui_layout(constraints, 0..term_width);
                    let up_fn = Rc::new(move |s: &mut State| s.expand_expander(pos, 3, Prev));
                    let all_fn = Rc::new(move |s: &mut State| s.expand_expander(pos, hidden_count, Next));
                    let down_fn = Rc::new(move |s: &mut State| s.expand_expander(pos, 3, Next));
                    buttons.push(((pos.parent, 0), up_pos, y, up_text, up_fn));
                    buttons.push(((pos.parent, 1), all_pos, y, all_text, all_fn));
                    buttons.push(((pos.parent, 2), down_pos, y, down_text, down_fn));
                }
            });
            if let Some(next) = tree_view.next_leaf(pos, Next) {
                pos = next;
            } else {
                break;
            }
        }

        let mut old_button_hints: HashMap<(Nid, usize), usize> = HashMap::new();
        if let Some(old_rendered) = old_rendered {
            for (hint, button) in old_rendered.button_hints.iter().enumerate() {
                if let Some(button) = button {
                    old_button_hints.insert(button.id, hint);
                }
            }
        }

        let mut unused_hints: HashSet<usize> = (0..self.button_hint_chars.len()).collect();
        for (id, ..) in &buttons {
            if let Some(hint) = old_button_hints.get(id) {
                unused_hints.remove(hint);
            }
        }

        let mut unused_hints: Vec<usize> = unused_hints.into_iter().collect();
        unused_hints.sort();
        unused_hints.reverse();

        for (id, x_range, y, text, func) in buttons {
            let theme = &self.config.theme;
            buffer_write(buffer, x_range.start, y, format!("   {text} "), theme.button);
            if let Some(hint) = old_button_hints.get(&id).cloned().or_else(|| unused_hints.pop()) {
                let hint_char = self.button_hint_chars[hint];
                buffer_write(buffer, x_range.start + 1, y, format!("{hint_char}"), theme.button_hint);
                rendered.button_hints[hint] = Some(ButtonInfo { id, func: func.clone() });
            }
            for x in x_range {
                rendered.mouse_cells[y][x] = MouseCell::Button(func.clone());
            }
        }

        match self.mode {
            GuiMode::Default => {}
            GuiMode::Jump { ref mut input } => {
                let y = self.scroll_height - 1;
                let text = "Go to line: ";
                let text_left = "L=left";
                let text_right = "R=right";
                let constraints = [
                    Some(text.len()),
                    None,
                    Some(1),
                    Some(text_left.len()),
                    Some(1),
                    Some(text_right.len()),
                ];
                let [text_pos, input_pos, _, left_pos, _, right_pos] = gui_layout(constraints, 0..term_width);
                buffer_write(buffer, text_pos.start, y, text, Style::default());
                let input_area = tui::layout::Rect {
                    x: usto16(input_pos.start),
                    y: usto16(y),
                    width: usto16(input_pos.end - input_pos.start),
                    height: 1,
                };
                input.render(input_area, buffer, &mut rendered.terminal_cursor);
                let style = |active| Style {
                    bold: Some(active),
                    crossed_out: Some(!active),
                    ..Style::default()
                };
                buffer_write(buffer, left_pos.start, y, text_left, style(self.focused_side == 0));
                buffer_write(buffer, right_pos.start, y, text_right, style(self.focused_side == 1));
            }
            GuiMode::Search {
                ref mut input,
                direction,
                case_sensitivity,
                regexp,
                ..
            } => {
                let y = self.scroll_height - 1;
                let prefix_text = match direction {
                    Next => "/",
                    Prev => "?",
                };
                let regex_text = "C-R=regex";
                let case_text = "C-S=case-sensitive";
                let constraints = [
                    Some(prefix_text.len()),
                    None,
                    Some(1),
                    Some(regex_text.len()),
                    Some(1),
                    Some(case_text.len()),
                ];
                let [prefix_pos, input_pos, _, regex_pos, _, case_pos] = gui_layout(constraints, 0..term_width);
                buffer_write(buffer, prefix_pos.start, y, prefix_text, Style::default());
                let input_area = tui::layout::Rect {
                    x: usto16(input_pos.start),
                    y: usto16(y),
                    width: usto16(input_pos.end - input_pos.start),
                    height: 1,
                };
                input.render(input_area, buffer, &mut rendered.terminal_cursor);
                let style = |active| Style {
                    bold: Some(active),
                    crossed_out: Some(!active),
                    ..Style::default()
                };
                let case_sensitive = is_search_case_sensitive(case_sensitivity, input.get_content());
                buffer_write(buffer, regex_pos.start, y, regex_text, style(regexp));
                buffer_write(buffer, case_pos.start, y, case_text, style(case_sensitive));
            }
        }

        rendered
    }
}

// This hack is needed because tui-rs doesn't have a way to get Buffer from Frame. The only thing
// that can be done with a Frame is to render a Widget. So we use this adapter to convert an
// arbitrary closure to a Widget.
// TODO: This was pretty painful. Switch from tui-rs to notcurses?
struct WidgetWrapper<F>(F);
impl<F> tui::widgets::Widget for WidgetWrapper<F>
where
    F: FnOnce(tui::layout::Rect, &mut tui::buffer::Buffer),
{
    fn render(self, area: tui::layout::Rect, buf: &mut tui::buffer::Buffer) {
        (self.0)(area, buf)
    }
}

type TheTerminal = tui::terminal::Terminal<tui::backend::CrosstermBackend<io::Stdout>>;

pub fn run_tui(
    diff: &Diff,
    config: Config,
    file_input: &[[&str; 2]],
    file_names: &[[&str; 2]],
    terminal: &mut TheTerminal,
) -> TheResult {
    let diff = make_extended_diff(diff, file_input, file_names);

    let button_hint_chars: Vec<char> = {
        let mut seen = HashSet::new();
        (config.button_hint_chars)
            .chars()
            .map(|c| c.to_ascii_uppercase())
            .filter(|&c| seen.insert(c))
            .collect()
    };

    let reverse_button_hint_chars: HashMap<char, usize> = button_hint_chars
        .iter()
        .cloned()
        .enumerate()
        .map(|(n, c)| (c, n))
        .collect();

    let mut state = {
        // TODO: If we have per-file wrap_width later, we could have per-file-side line_number_width too.
        let line_number_width = diff
            .file_sides
            .iter()
            .flatten()
            .map(|file_side| file_side.line_offsets.len() - 1)
            .max()
            .unwrap_or(0)
            .to_string()
            .len();

        let (initial_tree, visible_byte_sets, byte_to_nid_maps) = build_initial_tree(&config, &diff);
        let size = terminal.size()?;
        let initial_wrap_width = compute_wrap_width(&main_gui_layout(
            u16tos(size.width),
            line_number_width,
            config.show_cursor,
        ));
        let initial_scroll = TreeView {
            tree: &initial_tree,
            diff: &diff,
            wrap_width: initial_wrap_width,
        }
        .bordering_leaf_under(initial_tree.root, First)
        .unwrap();
        State {
            config,
            tree: initial_tree,
            visible_byte_sets,
            byte_to_nid_maps,
            diff: &diff,
            line_number_width,
            scroll_pos: initial_scroll,
            cursor_pos: initial_scroll,
            wrap_width: initial_wrap_width,
            scroll_height: u16tos(size.height),
            selection: None,
            search_query: None,
            search_matches: [vec![], vec![]],
            search_highlights: Default::default(),
            mode: GuiMode::Default,
            focused_side: 1,
            button_hint_chars,
            reverse_button_hint_chars,
        }
    };

    // TODO
    if false {
        for file_id in 0..diff.files.len() {
            for side in 0..2 {
                for (range, ()) in state.visible_byte_sets[file_id][side].ranges() {
                    eprintln!(
                        "{file_id} {side} visible range: {range:?} {:?}",
                        &file_input[file_id][side][range.clone()]
                    );
                }
                for (range, nid) in state.byte_to_nid_maps[file_id][side].ranges() {
                    eprintln!("{file_id} {side} range {range:?} belongs to nid {}", nid.0);
                }
            }
        }
    }

    let mut rendered = None;

    loop {
        terminal.draw(|frame| {
            let render = |size: tui::layout::Rect, buffer: &mut tui::buffer::Buffer| {
                rendered = Some(state.render(u16tos(size.width), u16tos(size.height), rendered.as_ref(), buffer));
            };

            frame.render_widget(WidgetWrapper(render), frame.size());

            if let Some((x, y)) = rendered.as_ref().unwrap().terminal_cursor {
                frame.set_cursor(x, y);
            }
        })?;

        let rendered_ref = rendered.as_ref().unwrap();

        loop {
            match state.handle_event(crossterm::event::read()?, rendered_ref)? {
                EventResult::Nothing => {}
                EventResult::Bell => write!(terminal.backend_mut(), "\x07")?,
                EventResult::Quit => return Ok(()),
            }
            if !crossterm::event::poll(std::time::Duration::ZERO)? {
                break;
            }
        }
    }
}

pub fn run_in_terminal(f: impl FnOnce(&mut TheTerminal) -> TheResult) -> TheResult {
    crossterm::terminal::enable_raw_mode()?;
    crossterm::execute!(
        io::stdout(),
        crossterm::terminal::EnterAlternateScreen,
        crossterm::event::EnableMouseCapture
    )?;
    let backend = tui::backend::CrosstermBackend::new(io::stdout());
    let mut terminal = tui::Terminal::new(backend)?;

    fn reset_terminal() -> TheResult {
        crossterm::terminal::disable_raw_mode()?;
        crossterm::execute!(
            io::stdout(),
            crossterm::event::DisableMouseCapture,
            crossterm::terminal::LeaveAlternateScreen
        )?;
        Ok(())
    }

    let original_hook = Arc::new(Mutex::new(std::panic::take_hook()));
    let original_hook_ref = original_hook.clone();
    std::panic::set_hook(Box::new(move |panic_info| {
        reset_terminal().unwrap();
        original_hook_ref.lock().unwrap()(panic_info);
    }));

    let result = f(&mut terminal);

    std::panic::set_hook(std::mem::replace(&mut *original_hook.lock().unwrap(), Box::new(|_| {})));

    reset_terminal()?;

    result
}
