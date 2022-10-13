mod range_map;

use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use crossterm::event::{KeyCode, KeyModifiers};
use range_map::RangeMap;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, Write as _};
use std::ops::{DerefMut as _, Range};
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use tui::style::{Color, Modifier, Style};
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr as _;

struct Theme {
    cursor: Style,
    line_numbers_default: Style,
    line_numbers_phantom: Style,
    text_equal: Style,
    text_padding: Style,
    text_change_old: Style,
    text_change_new: Style,
    text_move_old: Style,
    text_move_new: Style,
    text_phantom_old: Style,
    text_phantom_new: Style,
    highlight_change_old: Style,
    highlight_change_new: Style,
    highlight_move_old: Style,
    highlight_move_new: Style,
    highlight_phantom_old: Style,
    highlight_phantom_new: Style,
    fabricated_symbol: Style,
}

#[allow(dead_code)]
fn default_theme() -> Theme {
    let highlight = Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED);
    Theme {
        cursor: Style::default().fg(Color::White).bg(Color::Blue),
        line_numbers_default: Style::default(),
        line_numbers_phantom: Style::default().fg(Color::Cyan),
        text_equal: Style::default(),
        text_padding: Style::default().fg(Color::DarkGray),
        text_change_old: Style::default().fg(Color::Red),
        text_change_new: Style::default().fg(Color::Green),
        text_move_old: Style::default().fg(Color::Yellow),
        text_move_new: Style::default().fg(Color::Yellow),
        text_phantom_old: Style::default().fg(Color::Cyan).add_modifier(Modifier::DIM),
        text_phantom_new: Style::default().fg(Color::Cyan).add_modifier(Modifier::DIM),
        highlight_change_old: highlight,
        highlight_change_new: highlight,
        highlight_move_old: highlight,
        highlight_move_new: highlight,
        highlight_phantom_old: highlight,
        highlight_phantom_new: highlight,
        fabricated_symbol: Style::default().bg(Color::Cyan),
    }
}

#[allow(dead_code)]
fn new_theme() -> Theme {
    let black = Color::Indexed(16); // #000000
    let white = Color::Indexed(253); // #DADADA
    let darkest_red = Color::Indexed(52); // #5F0000
    let darkest_green = Color::Indexed(22); // #005F00
    let darkest_yellow = Color::Indexed(58); // #5F5F00
    let darkest_cyan = Color::Indexed(23); // #005F5F
    let highlight = Style::default().add_modifier(Modifier::BOLD);
    Theme {
        cursor: Style::default().fg(Color::White).bg(Color::Blue),
        line_numbers_default: Style::default(),
        line_numbers_phantom: Style::default().fg(Color::Cyan),
        text_equal: Style::default().fg(white).bg(black),
        text_padding: Style::default().fg(Color::DarkGray).bg(Color::Indexed(238)),
        text_change_old: Style::default().fg(white).bg(darkest_red),
        text_change_new: Style::default().fg(white).bg(darkest_green),
        text_move_old: Style::default().fg(white).bg(darkest_yellow),
        text_move_new: Style::default().fg(white).bg(darkest_yellow),
        text_phantom_old: Style::default().fg(white).bg(darkest_cyan).add_modifier(Modifier::DIM),
        text_phantom_new: Style::default().fg(white).bg(darkest_cyan).add_modifier(Modifier::DIM),
        highlight_change_old: highlight.bg(Color::Indexed(88)),
        highlight_change_new: highlight.bg(Color::Indexed(28)),
        highlight_move_old: highlight.bg(Color::Indexed(100)),
        highlight_move_new: highlight.bg(Color::Indexed(100)),
        highlight_phantom_old: highlight.bg(Color::Indexed(30)),
        highlight_phantom_new: highlight.bg(Color::Indexed(30)),
        fabricated_symbol: highlight.fg(Color::Cyan),
    }
}

enum ClipboardMechanism {
    /// Copy to clipboard using the OSC 52 escape sequence.
    /// Needs support from the terminal.
    /// See also: https://github.com/zyedidia/micro/blob/master/runtime/help/copypaste.md
    Terminal,

    /// Copy to clipboard by running a helper such as xclip, xsel or pbcopy.
    ExternalHelper,

    /// Don't copy to system clipboard.
    None,
}

struct Config {
    context_lines: usize,
    mouse_wheel_scroll_lines: usize,
    phantom_rendering: bool,
    highlight_newlines: bool,
    theme: Theme,
    clipboard_mechanism: ClipboardMechanism,
}

fn default_config() -> Config {
    Config {
        context_lines: 3,
        mouse_wheel_scroll_lines: 3,
        phantom_rendering: true,
        highlight_newlines: false,
        theme: new_theme(),
        clipboard_mechanism: if std::env::var_os("VXDIFF_EXTCOPY").is_some() {
            ClipboardMechanism::ExternalHelper
        } else {
            ClipboardMechanism::Terminal
        },
    }
}

struct ExtendedDiffSectionSide<'a> {
    file_id: usize,
    byte_range: Range<usize>,
    highlight_bounds: &'a [usize],
    highlight_first: bool,
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
        section_sides: (0..diff.sections.len()).map(|_| [None, None]).collect(),
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
                    byte_range: section_side.highlight_bounds[0]..*section_side.highlight_bounds.last().unwrap(),
                    highlight_bounds: &section_side.highlight_bounds,
                    highlight_first: section_side.highlight_first,
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

#[derive(Clone, Copy, PartialEq, Eq)]
struct Nid(usize);

#[derive(Clone, Copy, PartialEq, Eq)]
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
                    if !self.diff.file_sides[section_side.file_id][side].content.ends_with('\n') {
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
    let mut visible_byte_sets: Vec<_> = diff.files.iter().map(|_| [0, 1].map(|_| RangeMap::new())).collect();
    let mut byte_to_nid_maps: Vec<_> = diff.files.iter().map(|_| [0, 1].map(|_| RangeMap::new())).collect();

    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let file_nid = tree.add_child(tree.root, Node::new_branch());
        tree.add_child(file_nid, Node::FileHeaderLine(file_id));
        let file_content_nid = tree.add_child(file_nid, Node::new_branch());
        tree.node_mut(file_nid).as_branch_mut().visible = 0..1;

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

#[derive(Clone)]
struct LineCell {
    /// Double-width characters are represented by a first LineCell with some egc and a second
    /// LineCell with an empty string.
    egc: String,
    offset: usize,
    highlight: bool,
    fabricated_symbol: bool,
}

struct LineLayout {
    cells: Vec<LineCell>,
    newline_highlight: bool,
    offset_after_except_newline: usize,
    offset_after_with_newline: usize,
}

fn layout_one_line_raw(
    input: &str,
    highlight_bounds: &[usize],
    highlight_first: bool,
    mut offset: usize,
    end: usize,
    wrap_width: usize,
) -> LineLayout {
    let mut cells = vec![];

    let mut highlight_bounds_index = highlight_bounds.partition_point(|&point| point <= offset) - 1;
    let mut highlight = highlight_first ^ (highlight_bounds_index % 2 == 1);

    let new_layout = |cells, newline_highlight, offset_after_except_newline, offset_after_with_newline| LineLayout {
        cells,
        newline_highlight,
        offset_after_except_newline,
        offset_after_with_newline,
    };

    // The algorithm uses `unicode-segmentation` to split the line into EGCs, and `unicode-width`
    // to compute the width of each EGC. I don't know if it's correct (precisely matches what
    // terminal emulators do), but at least it matches the behavior of our library (tui-rs).
    for egc in input[offset..end].graphemes(true) {
        assert!(highlight_bounds_index + 1 < highlight_bounds.len());
        if offset == highlight_bounds[highlight_bounds_index + 1] {
            highlight_bounds_index += 1;
            highlight = !highlight;
        }

        if egc == "\n" {
            return new_layout(cells, highlight, offset, offset + egc.len());
        }

        assert!(!egc.contains('\n'));

        let split = |string: &str| (string.chars().map(|c| c.to_string()).collect(), true);

        let (cell_strings, fabricated_symbol) = match egc {
            "\t" => split(&" ".repeat(8 - cells.len() % 8)),
            _ => {
                assert!(!egc.contains('\t'));
                let egc_width = egc.width();
                match egc_width {
                    // Replace nonprintable characters with an escape sequence. escape_debug is
                    // nicer than escape_default because it shows zero bytes as \0, not \u{0}.
                    0 => split(&egc.escape_debug().to_string()),
                    1 => (vec![egc.to_string()], false),
                    2 => (vec![egc.to_string(), "".to_string()], false),
                    _ => panic!("egc width was {egc_width}"),
                }
            }
        };

        assert!(cell_strings.len() <= wrap_width);

        if cells.len() + cell_strings.len() > wrap_width {
            return new_layout(cells, highlight, offset, offset);
        }

        for cell_string in cell_strings {
            cells.push(LineCell {
                egc: cell_string,
                offset,
                highlight,
                fabricated_symbol,
            });
        }

        offset += egc.len();
    }

    new_layout(cells, highlight, offset, offset)
}

fn layout_one_line(
    diff: &ExtendedDiff,
    side: usize,
    source: &TextSource,
    offset: usize,
    wrap_width: usize,
) -> LineLayout {
    match source {
        &TextSource::Section(section_id) => {
            let section_side = diff.section_side(section_id, side);
            layout_one_line_raw(
                diff.file_sides[section_side.file_id][side].content,
                section_side.highlight_bounds,
                section_side.highlight_first,
                offset,
                section_side.byte_range.end,
                wrap_width,
            )
        }
        TextSource::Fabricated(content) => {
            layout_one_line_raw(content, &[0, content.len()], false, offset, content.len(), wrap_width)
        }
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
            let next = layout_one_line(diff, side, &raw_element.source, pos, wrap_width).offset_after_with_newline;
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
                        let layout = layout_one_line(
                            tree_view.diff,
                            side,
                            &sides[side].source,
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
    file_input: &[[&str; 2]],
    file_names: &[[&str; 2]],
    output: &mut impl io::Write,
) -> TheResult {
    let diff = make_extended_diff(diff, file_input, file_names);
    let config = default_config();
    let mut tree = build_initial_tree(&config, &diff).0;

    // Gotta expand the file headers in order to see any content.
    let mut child_option_nid = tree.bordering_child(tree.root, First);
    while let Some(child_nid) = child_option_nid {
        let child_branch_node = tree.node_mut(child_nid).as_branch_mut();
        assert!(child_branch_node.visible == (0..1));
        assert!(child_branch_node.children.len() == 2);
        child_branch_node.visible = 0..2;
        child_option_nid = tree.sibling(child_nid, Next);
    }

    let tree_view = TreeView {
        tree: &tree,
        diff: &diff,
        wrap_width: 80,
    };

    print_plainly(&tree_view, tree.root, output)
}

struct SelectionState {
    file_id: usize,
    side: usize,
    selecting: bool,
    start_offset: usize,
    current_offset: usize,
}

struct State<'a> {
    tree: Tree,
    visible_byte_sets: Vec<[RangeMap<()>; 2]>,
    byte_to_nid_maps: Vec<[RangeMap<Nid>; 2]>,
    diff: &'a ExtendedDiff<'a>,
    scroll_pos: LeafPosition,
    cursor_pos: LeafPosition,
    wrap_width: usize,
    scroll_height: usize,
    selection: Option<SelectionState>,
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
        self.cursor_pos = self.move_pos(self.cursor_pos, offset, dir);
        self.fix_scroll_invariants(true);
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

    fn expand_expander(&mut self, expander: Nid, count: usize, dir: Direction) {
        let hidden_count = match self.tree.node(expander) {
            Node::ExpanderLine(hidden_count) => *hidden_count,
            _ => return,
        };
        let cursor_distance = self.tree_view().leaf_distance(self.scroll_pos, self.cursor_pos);
        if hidden_count >= count + 2 {
            *self.tree.node_mut(expander) = Node::ExpanderLine(hidden_count - count);
            self.expand_sibling(expander, count, dir);
        } else {
            self.expand_sibling(expander, hidden_count, Next);
            let this_leaf = LeafPosition {
                parent: expander,
                line_index: 0,
            };
            let next_leaf = self.tree_view().next_leaf(this_leaf, Next).unwrap();
            if self.scroll_pos == this_leaf {
                self.scroll_pos = next_leaf;
            }
            if self.cursor_pos == this_leaf {
                self.cursor_pos = next_leaf;
            }
            *self.tree.node_mut(expander) = Node::ExpanderLine(0);
        }
        self.scroll_pos = self.move_pos(self.cursor_pos, cursor_distance, Prev);
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

fn copy_to_clipboard(mechanism: &ClipboardMechanism, text: &str) -> TheResult {
    match mechanism {
        ClipboardMechanism::Terminal => {
            // https://terminalguide.namepad.de/seq/osc-52/
            io::stdout().write(format!("\x1b]52;c;{}\x1b\\", base64::encode(text)).as_bytes())?;
        }
        ClipboardMechanism::ExternalHelper => {
            let mut found_candidate = false;
            let mut try_candidate = |cmd: &str, args: &[&str]| -> TheResult {
                if !found_candidate {
                    match std::process::Command::new(cmd)
                        .args(args)
                        .current_dir("/")
                        .stdin(std::process::Stdio::piped())
                        .spawn()
                    {
                        Ok(mut child) => {
                            found_candidate = true;
                            // TODO: We should use threads or select() or something.
                            child.stdin.take().unwrap().write(text.as_bytes())?;
                            let status = child.wait()?;
                            if !status.success() {
                                return TheResult::Err(
                                    std::io::Error::new(
                                        std::io::ErrorKind::Other,
                                        &*format!("{cmd} exited with status {status}"),
                                    )
                                    .into(),
                                );
                            }
                        }
                        Err(err) => {
                            if err.kind() != std::io::ErrorKind::NotFound {
                                return TheResult::Err(err.into());
                            }
                        }
                    }
                }
                Ok(())
            };
            // https://github.com/neovim/neovim/blob/cd96fe06e188bcd6e64f78cb078a307fb45f31f0/runtime/autoload/provider/clipboard.vim
            #[cfg(target_os = "macos")]
            try_candidate("pbcopy", &[])?;
            #[cfg(not(target_os = "macos"))]
            {
                if std::env::var_os("WAYLAND_DISPLAY").is_some() {
                    try_candidate("wl-copy", &["--primary", "--type", "text/plain"])?;
                }
                if std::env::var_os("DISPLAY").is_some() {
                    try_candidate("xclip", &["-i", "-selection", "primary"])?;
                    try_candidate("xsel", &["-i", "-p"])?;
                }
                try_candidate("lemonade", &["copy"])?;
                try_candidate("doitclient", &["wclip"])?;
                try_candidate("termux-clipboard-set", &[])?;
                if std::env::var_os("TMUX").is_some() {
                    try_candidate("tmux", &["load-buffer", "-"])?;
                }
            }
        }
        ClipboardMechanism::None => {}
    }
    Ok(())
}

type TheTerminal = tui::terminal::Terminal<tui::backend::CrosstermBackend<io::Stdout>>;

fn usto16(number: usize) -> u16 {
    u16::try_from(number).unwrap()
}
fn u16tos(number: u16) -> usize {
    usize::try_from(number).unwrap()
}

pub fn run_tui(
    diff: &Diff,
    file_input: &[[&str; 2]],
    file_names: &[[&str; 2]],
    terminal: &mut TheTerminal,
) -> TheResult {
    let diff = make_extended_diff(diff, file_input, file_names);
    let config = default_config();

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

    // TODO: Configurable wrap_width behavior (e.g. fixed 80, multiples of 10, fluid)
    let compute_wrap_width = |terminal_width: usize| (terminal_width - 6 - line_number_width * 2) / 2;

    let mut size = terminal.size()?;

    let mut state = {
        let (initial_tree, visible_byte_sets, byte_to_nid_maps) = build_initial_tree(&config, &diff);
        let initial_wrap_width = compute_wrap_width(u16tos(size.width));
        let initial_scroll = TreeView {
            tree: &initial_tree,
            diff: &diff,
            wrap_width: initial_wrap_width,
        }
        .bordering_leaf_under(initial_tree.root, First)
        .unwrap();
        State {
            tree: initial_tree,
            visible_byte_sets,
            byte_to_nid_maps,
            diff: &diff,
            scroll_pos: initial_scroll,
            cursor_pos: initial_scroll,
            wrap_width: initial_wrap_width,
            scroll_height: u16tos(size.height),
            selection: None,
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

    #[derive(Clone)]
    enum MouseCell {
        Inert,
        Button(Rc<dyn Fn(&mut State)>),
        Text { file_id: usize, side: usize, offset: usize },
    }

    loop {
        let mut mouse_cells: Vec<Vec<MouseCell>> = vec![];
        let mut mouse_pseudocell_after: Vec<[MouseCell; 2]> = vec![];

        let render = |new_size: tui::layout::Rect, buffer: &mut tui::buffer::Buffer| {
            if new_size != size {
                size = new_size;
                state.resize(compute_wrap_width(u16tos(size.width)), u16tos(size.height));
            }

            mouse_cells = vec![vec![MouseCell::Inert; u16tos(size.width)]; u16tos(size.height)];
            mouse_pseudocell_after = vec![[MouseCell::Inert, MouseCell::Inert]; u16tos(size.height)];

            let mut pos = state.scroll_pos;
            for y in 0..state.scroll_height {
                if pos == state.cursor_pos {
                    buffer.set_string(0, usto16(y), ">", config.theme.cursor);
                }
                let parent_node = state.tree.node(pos.parent);
                let tree_view = state.tree_view();
                tree_view.with_ui_lines(parent_node, |lines| match &lines[pos.line_index] {
                    UILine::Sides(sides) => {
                        for side in 0..2 {
                            let whl = &sides[side];
                            let (file_id_for_rendering, line_number_str) = match whl.source {
                                TextSource::Section(section_id) => {
                                    let file_id = diff.section_side(section_id, side).file_id;
                                    let file_side = &diff.file_sides[file_id][side];
                                    let line_number = file_side.byte_offset_to_line_number(whl.offset);
                                    let line_number_str = if whl.offset == file_side.line_offsets[line_number] {
                                        format!("{line_number:>line_number_width$}")
                                    } else {
                                        let my_width = line_number.to_string().len();
                                        " ".repeat(line_number_width - my_width) + &"+".repeat(my_width)
                                    };
                                    (Some(file_id), line_number_str)
                                }
                                TextSource::Fabricated(..) => (None, " ".repeat(line_number_width)),
                            };
                            let lx = 1 + side * (line_number_width + 1 + state.wrap_width + 3 + state.wrap_width + 1);
                            let line_number_style = match whl.style {
                                HalfLineStyle::Phantom => config.theme.line_numbers_phantom,
                                _ => config.theme.line_numbers_default,
                            };
                            buffer.set_string(usto16(lx), usto16(y), line_number_str, line_number_style);

                            let screen_start_x = 1 + line_number_width + 1 + side * (state.wrap_width + 3);

                            let layout = layout_one_line(&diff, side, &whl.source, whl.offset, state.wrap_width);

                            // TODO: This loop runs at most a few times, but it might be nicer to avoid it.
                            let file_id_for_selection = {
                                let mut nid = pos.parent;
                                loop {
                                    if tree_view.tree.node_meta(nid).index_in_parent == 1 {
                                        let sibling_nid = tree_view.tree.sibling(nid, Prev).unwrap();
                                        if let &Node::FileHeaderLine(file_id) = tree_view.tree.node(sibling_nid) {
                                            break file_id;
                                        }
                                    }
                                    if let Some(parent_nid) = tree_view.tree.parent(nid) {
                                        nid = parent_nid;
                                    } else {
                                        panic!("UILine::Sides was used outside of a file_content_node");
                                    }
                                }
                            };

                            let eol_cell = LineCell {
                                egc: " ".to_string(),
                                highlight: config.highlight_newlines && layout.newline_highlight,
                                fabricated_symbol: false,
                                offset: layout.offset_after_except_newline,
                            };

                            mouse_pseudocell_after[y][side] = MouseCell::Text {
                                file_id: file_id_for_selection,
                                side,
                                offset: whl
                                    .offset_override_for_selection
                                    .unwrap_or(layout.offset_after_with_newline),
                            };

                            for x in 0..state.wrap_width {
                                let LineCell {
                                    egc,
                                    highlight,
                                    fabricated_symbol,
                                    offset,
                                } = layout.cells.get(x).unwrap_or(&eol_cell).clone();

                                let selected = match &state.selection {
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

                                let x = screen_start_x + x;

                                mouse_cells[y][x] = MouseCell::Text {
                                    file_id: file_id_for_selection,
                                    side,
                                    offset: whl.offset_override_for_selection.unwrap_or(offset),
                                };

                                let x = usto16(x);
                                let y = usto16(y);
                                // tui-rs encodes double-width characters as one normal cell and
                                // one default cell (containing a " "). See Buffer::set_stringn()
                                // and Cell::reset(). We don't call set_stringn here, but let's try
                                // to match its result. The " " in the second cell won't be printed
                                // because Buffer::diff skips over it.
                                *buffer.get_mut(x, y) = if egc.is_empty() {
                                    tui::buffer::Cell::default()
                                } else {
                                    let mut style = Style::default();
                                    style = style.patch(match (whl.style, side != 0) {
                                        (HalfLineStyle::Equal, _) => config.theme.text_equal,
                                        (HalfLineStyle::Padding, _) => config.theme.text_padding,
                                        (HalfLineStyle::Change, false) => config.theme.text_change_old,
                                        (HalfLineStyle::Change, true) => config.theme.text_change_new,
                                        (HalfLineStyle::Move, false) => config.theme.text_move_old,
                                        (HalfLineStyle::Move, true) => config.theme.text_move_new,
                                        (HalfLineStyle::Phantom, false) => config.theme.text_phantom_old,
                                        (HalfLineStyle::Phantom, true) => config.theme.text_phantom_new,
                                    });
                                    if highlight {
                                        style = style.patch(match (whl.style, side != 0) {
                                            (HalfLineStyle::Equal, _) => Style::default(),
                                            (HalfLineStyle::Padding, _) => Style::default(),
                                            (HalfLineStyle::Change, false) => config.theme.highlight_change_old,
                                            (HalfLineStyle::Change, true) => config.theme.highlight_change_new,
                                            (HalfLineStyle::Move, false) => config.theme.highlight_move_old,
                                            (HalfLineStyle::Move, true) => config.theme.highlight_move_new,
                                            (HalfLineStyle::Phantom, false) => config.theme.highlight_phantom_old,
                                            (HalfLineStyle::Phantom, true) => config.theme.highlight_phantom_new,
                                        });
                                    }
                                    if fabricated_symbol {
                                        style = style.patch(config.theme.fabricated_symbol);
                                    }
                                    if selected {
                                        // TODO: Fix this and make it configurable too.
                                        if style.fg == None || style.fg == Some(Color::Reset) {
                                            style = style.fg(Color::Black);
                                        }
                                        style = style.bg(Color::White);
                                    }
                                    let mut cell = tui::buffer::Cell::default();
                                    cell.symbol = egc;
                                    cell.set_style(style);
                                    cell
                                };
                            }
                        }
                        buffer.set_string(
                            usto16(1 + line_number_width + 1 + state.wrap_width + 1),
                            usto16(y),
                            tui::symbols::line::VERTICAL,
                            Default::default(),
                        );
                    }
                    &UILine::FileHeaderLine(file_id) => {
                        let file_header_nid = pos.parent;
                        let file_nid = state.tree.parent(file_header_nid).unwrap();
                        let is_open = match state.tree.node(file_nid).as_branch().visible.end {
                            1 => false,
                            2 => true,
                            _ => panic!("unexpected node.visible of FileHeaderLine's parent"),
                        };
                        buffer.set_string(
                            1,
                            usto16(y),
                            format!(
                                "{} vs {} (press z to {})",
                                file_names[file_id][0],
                                file_names[file_id][1],
                                if is_open { "close" } else { "open" }
                            ),
                            Style::default()
                                .fg(Color::Black)
                                .bg(if is_open { Color::Green } else { Color::Red }),
                        );
                    }
                    UILine::ExpanderLine(hidden_count) => {
                        buffer.set_string(
                            1,
                            usto16(y),
                            format!("...{} hidden lines... (press x/c to expand)", hidden_count),
                            Style::default().fg(Color::Black).bg(Color::White),
                        );
                    }
                });
                if let Some(next) = tree_view.next_leaf(pos, Next) {
                    pos = next;
                } else {
                    break;
                }
            }
        };

        terminal.draw(|frame| {
            frame.render_widget(WidgetWrapper(render), frame.size());
            // TODO: eventually might want to call frame.set_cursor() for /search etc
        })?;

        let update_selection_position = |selection: &mut SelectionState, x: usize, y: usize| {
            let mut mx = x;
            let mut my = y;
            while my < u16tos(size.height) {
                if let MouseCell::Text { file_id, side, offset } = mouse_cells[my][mx] {
                    if file_id == selection.file_id && side == selection.side {
                        selection.current_offset = offset;
                        return;
                    }
                }
                mx += 1;
                if mx == u16tos(size.width) {
                    if let MouseCell::Text { file_id, side, offset } = mouse_pseudocell_after[my][selection.side] {
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
                if let MouseCell::Text { file_id, side, offset } = mouse_pseudocell_after[my][selection.side] {
                    if file_id == selection.file_id && side == selection.side {
                        selection.current_offset = offset;
                        return;
                    }
                }
            }
        };

        loop {
            match crossterm::event::read()? {
                crossterm::event::Event::Key(e) => match e.code {
                    KeyCode::Esc | KeyCode::Char('q') => return Ok(()),
                    KeyCode::Char('c') if e.modifiers.contains(KeyModifiers::CONTROL) => return Ok(()),
                    KeyCode::Up => state.scroll_by(1, Prev),
                    KeyCode::Down => state.scroll_by(1, Next),
                    KeyCode::PageUp | KeyCode::Char('b') => state.scroll_by(state.scroll_height, Prev),
                    KeyCode::PageDown | KeyCode::Char(' ') => state.scroll_by(state.scroll_height, Next),
                    KeyCode::Char('w') | KeyCode::Char('k') => state.move_cursor_by(1, Prev),
                    KeyCode::Char('s') | KeyCode::Char('j') => state.move_cursor_by(1, Next),
                    // TODO: temporary key assignment for 'z', 'x', 'c'
                    KeyCode::Char('z') => {
                        if let Node::FileHeaderLine(_) = state.tree.node(state.cursor_pos.parent) {
                            let file_nid = state.tree.parent(state.cursor_pos.parent).unwrap();
                            let file_branch_node = state.tree.node_mut(file_nid).as_branch_mut();
                            file_branch_node.visible.end = match file_branch_node.visible.end {
                                1 => 2,
                                2 => 1,
                                _ => panic!("logic error: bad file_branch_node.visible.end"),
                            };
                            state.fix_scroll_invariants(false);
                        }
                    }
                    // TODO: configurable number of lines
                    // TODO: if at beginning xor end (zero context lines on one side), don't show that button
                    KeyCode::Char('x') => state.expand_expander(state.cursor_pos.parent, 3, Prev),
                    KeyCode::Char('c') => state.expand_expander(state.cursor_pos.parent, 3, Next),
                    KeyCode::Char('p') if e.modifiers.contains(KeyModifiers::CONTROL) => panic!("intentional panic"),
                    // TODO: Do it properly.
                    KeyCode::Char('g') => {
                        let mut buffer = String::new();
                        loop {
                            if let crossterm::event::Event::Key(e) = crossterm::event::read()? {
                                match e.code {
                                    KeyCode::Esc | KeyCode::Char('q') => break,
                                    KeyCode::Char('c') if e.modifiers.contains(KeyModifiers::CONTROL) => return Ok(()),
                                    KeyCode::Char(c) => buffer.push(c),
                                    KeyCode::Backspace if !buffer.is_empty() => {
                                        buffer.pop();
                                    }
                                    KeyCode::Enter => {
                                        if let Ok(line_number) = buffer.parse::<usize>() {
                                            let line_offsets = &diff.file_sides[0][1].line_offsets;
                                            let line_number = std::cmp::min(line_number, line_offsets.len() - 1);
                                            let byte_offset = line_offsets[line_number];
                                            let nid = state.byte_to_nid_maps[0][1].get(byte_offset).unwrap();
                                            // TODO: Find correct line_index.
                                            state.scroll_pos = LeafPosition {
                                                parent: nid,
                                                line_index: 0,
                                            };
                                            state.cursor_pos = LeafPosition {
                                                parent: nid,
                                                line_index: 0,
                                            };
                                            state.fix_scroll_invariants(true);
                                        } else {
                                            write!(terminal.backend_mut(), "\x07")?;
                                        }
                                        break;
                                    }
                                    _ => write!(terminal.backend_mut(), "\x07")?,
                                }
                            }
                        }
                    }
                    _ => write!(terminal.backend_mut(), "\x07")?,
                },
                crossterm::event::Event::Mouse(e) => {
                    let x = u16tos(e.column);
                    let y = u16tos(e.row);
                    match e.kind {
                        crossterm::event::MouseEventKind::Down(crossterm::event::MouseButton::Left) => {
                            match mouse_cells[y][x] {
                                MouseCell::Text { file_id, side, offset } => {
                                    state.selection = Some(SelectionState {
                                        file_id,
                                        side,
                                        selecting: true,
                                        start_offset: offset,
                                        current_offset: offset,
                                    });
                                }
                                MouseCell::Button(ref fun) => fun(&mut state),
                                MouseCell::Inert => {}
                            }
                        }
                        crossterm::event::MouseEventKind::Drag(crossterm::event::MouseButton::Left) => {
                            match state.selection {
                                Some(ref mut selection) if selection.selecting => {
                                    update_selection_position(selection, x, y);
                                }
                                _ => {}
                            }
                        }
                        crossterm::event::MouseEventKind::Up(crossterm::event::MouseButton::Left) => {
                            match state.selection {
                                Some(ref mut selection) if selection.selecting => {
                                    update_selection_position(selection, x, y);
                                    selection.selecting = false;
                                    let from = std::cmp::min(selection.start_offset, selection.current_offset);
                                    let to = std::cmp::max(selection.start_offset, selection.current_offset);
                                    copy_to_clipboard(
                                        &config.clipboard_mechanism,
                                        &file_input[selection.file_id][selection.side][from..to],
                                    )?;
                                }
                                _ => {}
                            }
                        }
                        crossterm::event::MouseEventKind::ScrollUp => {
                            state.scroll_by(config.mouse_wheel_scroll_lines, Prev);
                        }
                        crossterm::event::MouseEventKind::ScrollDown => {
                            state.scroll_by(config.mouse_wheel_scroll_lines, Next);
                        }
                        _ => {}
                    };
                }
                _ => {}
            };
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
