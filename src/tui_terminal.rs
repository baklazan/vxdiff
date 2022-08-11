use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use crossterm::{self, event::KeyCode};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, Write as _};
use std::ops::{DerefMut as _, Range};
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr as _;

struct ExtendedDiffSectionSide {
    file_id: usize,
    byte_range: Range<usize>,
    // TODO: If this encoding works out in practice, replace text_with_words in Diff with this.
    highlight_points: Vec<(usize, bool)>,
}

struct ExtendedDiffFileSide<'a> {
    filename: String,
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
    section_sides: Vec<[Option<ExtendedDiffSectionSide>; 2]>,
    file_sides: Vec<[ExtendedDiffFileSide<'a>; 2]>,
    sections: &'a [Section<'a>],
    files: &'a [FileDiff],
}

fn make_extended_diff<'a>(diff: &'a Diff, file_input: &'a [[&'a str; 2]]) -> ExtendedDiff<'a> {
    let mut extended_diff = ExtendedDiff {
        section_sides: (0..diff.sections.len()).map(|_| [None, None]).collect(),
        file_sides: (0..diff.files.len())
            .map(|file_id| {
                [0, 1].map(|side| ExtendedDiffFileSide {
                    filename: String::from("input.txt"),
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
            let mut offset = 0;
            for &(op, section_id) in ops {
                if op.movement()[side] == 0 {
                    continue;
                }
                let offset_start = offset;
                let mut highlight_points = vec![];
                for &(highlight, text) in &diff.sections[section_id].sides[side].text_with_words {
                    highlight_points.push((offset, highlight));
                    for ch in text.bytes() {
                        offset += 1;
                        if ch == b'\n' {
                            extended_diff.file_sides[file_id][side].line_offsets.push(offset);
                        }
                    }
                }
                extended_diff.section_sides[section_id][side] = Some(ExtendedDiffSectionSide {
                    file_id,
                    byte_range: offset_start..offset,
                    highlight_points,
                });
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

#[derive(Clone)]
enum WrappedHalfLine {
    FromInput { section_id: usize, offset: usize },
    Fabricated { content: String },
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
struct PaddedGroupNode {
    equal: bool,
    relevant_ops: [Vec<(DiffOp, usize)>; 2],
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

fn build_padded_group(sections: &[Section], ops: &[(DiffOp, usize)]) -> Node {
    let mut equal = true;
    let mut relevant_ops = [vec![], vec![]];
    for &(op, section_id) in ops {
        for side in 0..2 {
            if op.movement()[side] != 0 {
                relevant_ops[side].push((op, section_id));
            }
        }
        if op != DiffOp::Match || !sections[section_id].equal {
            equal = false;
        }
    }
    Node::PaddedGroup(PaddedGroupNode {
        equal,
        relevant_ops,
        cached_wrap: RefCell::new(None),
    })
}

fn build_initial_tree(diff: &ExtendedDiff) -> Tree {
    fn same_section_group(sections: &[Section], a: (DiffOp, usize), b: (DiffOp, usize)) -> bool {
        (a.0 == DiffOp::Match && b.0 == DiffOp::Match && sections[a.1].equal && sections[b.1].equal)
            || (a.0 != DiffOp::Match && b.0 != DiffOp::Match)
    }

    let mut tree = Tree::new(Node::new_branch());

    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let file_nid = tree.add_child(tree.root, Node::new_branch());
        tree.add_child(file_nid, Node::FileHeaderLine(file_id));
        let file_content_nid = tree.add_child(file_nid, Node::new_branch());
        tree.node_mut(file_nid).as_branch_mut().visible = 0..1;

        let mut op_index = 0;
        while op_index < ops.len() {
            let begin = op_index;
            let mut end = begin + 1;
            while end < ops.len() && same_section_group(diff.sections, ops[begin], ops[end]) {
                end += 1;
            }
            op_index = end;

            // TODO: ghost rendering (blue rendering) will be this:
            // - if Insert/Delete is a move from/to (both sides are non-empty), same_section_group
            //   returns false.
            // - build_padded_group stops checking movement() for those groups.

            let section_group_is_match_equal = ops[begin].0 == DiffOp::Match && diff.sections[ops[begin].1].equal;

            if section_group_is_match_equal {
                let length = end - begin;
                let mut padded_groups = vec![];
                for i in begin..end {
                    padded_groups.push(build_padded_group(diff.sections, &ops[i..i + 1]));
                }
                // TODO: One-sided expansion and context at beginning/end of file.

                let context = 3; // TODO configurable
                if length > 2 * context + 1 {
                    let upper_nid = tree.add_child(file_content_nid, Node::new_branch());
                    tree.add_children(upper_nid, padded_groups.clone());
                    tree.node_mut(upper_nid).as_branch_mut().visible = 0..context;

                    tree.add_child(file_content_nid, Node::ExpanderLine(length - 2 * context));

                    let lower_nid = tree.add_child(file_content_nid, Node::new_branch());
                    tree.add_children(lower_nid, padded_groups);
                    tree.node_mut(lower_nid).as_branch_mut().visible = (length - context)..length;
                } else {
                    tree.add_children(file_content_nid, padded_groups);
                }
            } else {
                tree.add_child(file_content_nid, build_padded_group(diff.sections, &ops[begin..end]));
            }
        }
    }

    tree
}

struct LineCell {
    /// Double-width characters are represented by a first LineCell with some egc and a second
    /// LineCell with an empty string.
    egc: String,
    offset: usize,
    highlight: bool,
    fabricated: bool,
}

struct LineLayout {
    cells: Vec<LineCell>,
    newline_highlight: bool,
}

fn layout_one_line(
    input: &str,
    mut highlight_points: &[(usize, bool)],
    mut offset: usize,
    end: usize,
    wrap_width: usize,
) -> (LineLayout, usize, bool) {
    let mut cells = vec![];

    let highlight_points_index = highlight_points.partition_point(|&(point, _)| point <= offset);
    assert!(highlight_points_index > 0);
    let mut highlight = highlight_points[highlight_points_index - 1].1;
    highlight_points = &highlight_points[highlight_points_index..];

    let new_layout = |cells, newline_highlight| LineLayout {
        cells,
        newline_highlight,
    };

    // The algorithm uses `unicode-segmentation` to split the line into EGCs, and `unicode-width`
    // to compute the width of each EGC. I don't know if it's correct (precisely matches what
    // terminal emulators do), but at least it matches the behavior of our library (tui-rs).
    for egc in input[offset..end].graphemes(true) {
        if !highlight_points.is_empty() && offset == highlight_points[0].0 {
            highlight = highlight_points[0].1;
            highlight_points = &highlight_points[1..];
        }

        if egc == "\n" {
            return (new_layout(cells, highlight), offset + egc.len(), true);
        }

        assert!(!egc.contains('\n'));

        let split = |string: &str| (string.chars().map(|c| c.to_string()).collect(), true);

        let (cell_strings, fabricated) = match egc {
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
            return (new_layout(cells, highlight), offset, false);
        }

        for cell_string in cell_strings {
            cells.push(LineCell {
                egc: cell_string,
                offset,
                highlight,
                fabricated,
            });
        }

        offset += egc.len();
    }

    (new_layout(cells, highlight), offset, false)
}

fn wrap_one_side(diff: &ExtendedDiff, node: &PaddedGroupNode, side: usize, wrap_width: usize) -> Vec<WrappedHalfLine> {
    let fabricate = |out: &mut Vec<WrappedHalfLine>, mut content: &str| {
        while !content.is_empty() {
            let (_, offset, _) = layout_one_line(content, &[(0, false)], 0, content.len(), wrap_width);
            out.push(WrappedHalfLine::Fabricated {
                content: content[0..offset].to_string(),
            });
            content = &content[offset..];
        }
    };

    let mut out: Vec<WrappedHalfLine> = vec![];
    let mut last_move: Option<(DiffOp, usize)> = None;
    for &(op, section_id) in &node.relevant_ops[side] {
        let section_side = diff.section_sides[section_id][side].as_ref().unwrap();

        if op != DiffOp::Match {
            if let Some(section_other_side) = &diff.section_sides[section_id][1 - side] {
                let Range { start, end } = section_other_side.byte_range;
                let file_other_side = &diff.file_sides[section_other_side.file_id][1 - side];
                if last_move != Some((op, start)) {
                    let direction = if op == DiffOp::Insert { "Moved from" } else { "Moved to" };
                    let filename = &file_other_side.filename;
                    let line_number = file_other_side.byte_offset_to_line_number(start);
                    fabricate(&mut out, &format!("{direction} {filename}:{line_number}"));
                }
                last_move = Some((op, end));
            } else {
                last_move = None;
            }
        } else {
            last_move = None;
        }

        let mut offset = section_side.byte_range.start;
        let mut ends_with_newline = true;
        while offset != section_side.byte_range.end {
            out.push(WrappedHalfLine::FromInput { section_id, offset });
            (_, offset, ends_with_newline) = layout_one_line(
                diff.file_sides[section_side.file_id][side].content,
                &section_side.highlight_points,
                offset,
                section_side.byte_range.end,
                wrap_width,
            );
        }

        if !ends_with_newline {
            fabricate(&mut out, &"No newline at end of file");
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
                    // TODO: temporary padding indicators
                    let padding = WrappedHalfLine::Fabricated {
                        content: "@".to_string(),
                    };
                    let wrapped_sides = [0, 1].map(|side| wrap_one_side(self.diff, &node, side, self.wrap_width));
                    let len = std::cmp::max(wrapped_sides[0].len(), wrapped_sides[1].len());
                    let padded_wrapped_sides =
                        wrapped_sides.map(|side| side.into_iter().chain(std::iter::repeat(padding.clone())));
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

    fn get_wrapped_half_line_layout(&self, whl: &WrappedHalfLine, side: usize) -> LineLayout {
        match whl {
            &WrappedHalfLine::FromInput { section_id, offset } => {
                let section_side = self.diff.section_sides[section_id][side].as_ref().unwrap();
                layout_one_line(
                    self.diff.file_sides[section_side.file_id][side].content,
                    &section_side.highlight_points,
                    offset,
                    section_side.byte_range.end,
                    self.wrap_width,
                )
            }
            WrappedHalfLine::Fabricated { content } => {
                layout_one_line(content, &[(0, false)], 0, content.len(), self.wrap_width)
            }
        }
        .0
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
                        let layout = tree_view.get_wrapped_half_line_layout(&sides[side], side);
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
    output: &mut impl io::Write,
) -> TheResult {
    let diff = make_extended_diff(diff, file_input);
    let mut tree = build_initial_tree(&diff);

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

struct State<'a> {
    tree: Tree,
    diff: &'a ExtendedDiff<'a>,
    scroll_pos: LeafPosition,
    cursor_pos: LeafPosition,
    wrap_width: usize,
    scroll_height: usize,
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

    fn expand_expander(&mut self, expander: Nid, count: usize, dir: Direction) {
        let hidden_count = match self.tree.node(expander) {
            Node::ExpanderLine(hidden_count) => *hidden_count,
            _ => return,
        };
        let cursor_distance = self.tree_view().leaf_distance(self.scroll_pos, self.cursor_pos);
        if hidden_count >= count + 2 {
            *self.tree.node_mut(expander) = Node::ExpanderLine(hidden_count - count);
            let sibling_nid = self.tree.sibling(expander, dir).unwrap();
            let sibling_node = self.tree.node_mut(sibling_nid).as_branch_mut();
            match dir {
                Prev => sibling_node.visible.end += count,
                Next => sibling_node.visible.start -= count,
            };
        } else {
            let sibling_nid = self.tree.sibling(expander, Next).unwrap();
            let sibling_node = self.tree.node_mut(sibling_nid).as_branch_mut();
            sibling_node.visible.start -= hidden_count;
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

pub fn run_tui(diff: &Diff, file_input: &[[&str; 2]], terminal: &mut TheTerminal) -> TheResult {
    let mut size = terminal.size()?;

    let diff = make_extended_diff(diff, file_input);
    let initial_tree = build_initial_tree(&diff);
    let initial_scroll = TreeView {
        tree: &initial_tree,
        diff: &diff,
        wrap_width: 80,
    }
    .bordering_leaf_under(initial_tree.root, First)
    .unwrap();
    let mut state = State {
        tree: initial_tree,
        diff: &diff,
        scroll_pos: initial_scroll,
        cursor_pos: initial_scroll,
        wrap_width: 80,
        scroll_height: usize::try_from(size.height)?,
    };

    loop {
        let render = |new_size: tui::layout::Rect, buffer: &mut tui::buffer::Buffer| {
            if new_size != size {
                // TODO: resize, and afterwards:
                // size = new_size;
                // state.wrap_width = ...; state.scroll_height = ...;
            }

            let mut pos = state.scroll_pos;
            for y in 0..state.scroll_height {
                if pos == state.cursor_pos {
                    buffer.set_string(
                        0,
                        u16::try_from(y).unwrap(),
                        ">",
                        tui::style::Style::default()
                            .fg(tui::style::Color::White)
                            .bg(tui::style::Color::Blue),
                    );
                }
                let parent_node = state.tree.node(pos.parent);
                let tree_view = state.tree_view();
                tree_view.with_ui_lines(parent_node, |lines| match &lines[pos.line_index] {
                    UILine::Sides(sides) => {
                        let equal = match parent_node {
                            Node::PaddedGroup(node) => node.equal,
                            _ => panic!("wat"),
                        };
                        for side in 0..2 {
                            let layout = tree_view.get_wrapped_half_line_layout(&sides[side], side);
                            for (x, cell) in layout.cells.into_iter().enumerate() {
                                let LineCell {
                                    egc,
                                    highlight,
                                    fabricated,
                                    offset: _,
                                } = cell;
                                let x = 1 + side * (state.wrap_width + 3) + x;
                                let x = u16::try_from(x).unwrap();
                                let y = u16::try_from(y).unwrap();
                                // tui-rs encodes double-width characters as one normal cell and
                                // one default cell (containing a " "). See Buffer::set_stringn()
                                // and Cell::reset(). We don't call set_stringn here, but let's try
                                // to match its result. The " " in the second cell won't be printed
                                // because Buffer::diff skips over it.
                                *buffer.get_mut(x, y) = if egc.is_empty() {
                                    tui::buffer::Cell::default()
                                } else {
                                    tui::buffer::Cell {
                                        symbol: egc,
                                        fg: if equal {
                                            tui::style::Color::Reset
                                        } else if side == 0 {
                                            tui::style::Color::Red
                                        } else {
                                            tui::style::Color::Green
                                        },
                                        bg: if fabricated {
                                            tui::style::Color::Cyan
                                        } else {
                                            tui::style::Color::Reset
                                        },
                                        modifier: if highlight {
                                            tui::style::Modifier::BOLD | tui::style::Modifier::UNDERLINED
                                        } else {
                                            tui::style::Modifier::empty()
                                        },
                                    }
                                };
                            }
                        }
                        buffer.set_string(
                            u16::try_from(1 + state.wrap_width + 1).unwrap(),
                            u16::try_from(y).unwrap(),
                            tui::symbols::line::VERTICAL,
                            Default::default(),
                        );
                    }
                    UILine::FileHeaderLine(file_id) => {
                        let file_header_nid = pos.parent;
                        let file_nid = state.tree.parent(file_header_nid).unwrap();
                        let is_open = match state.tree.node(file_nid).as_branch().visible.end {
                            1 => false,
                            2 => true,
                            _ => panic!("unexpected node.visible of FileHeaderLine's parent"),
                        };
                        buffer.set_string(
                            1,
                            u16::try_from(y).unwrap(),
                            format!(
                                "file #{} (press z to {})",
                                file_id,
                                if is_open { "close" } else { "open" }
                            ),
                            tui::style::Style::default()
                                .fg(tui::style::Color::Black)
                                .bg(if is_open {
                                    tui::style::Color::Green
                                } else {
                                    tui::style::Color::Red
                                }),
                        );
                    }
                    UILine::ExpanderLine(hidden_count) => {
                        buffer.set_string(
                            1,
                            u16::try_from(y).unwrap(),
                            format!("...{} hidden lines... (press x/c to expand)", hidden_count),
                            tui::style::Style::default()
                                .fg(tui::style::Color::Black)
                                .bg(tui::style::Color::White),
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

        loop {
            match crossterm::event::read()? {
                crossterm::event::Event::Key(e) => match e.code {
                    KeyCode::Esc | KeyCode::Char('q') => return Ok(()),
                    KeyCode::Up => state.scroll_by(1, Prev),
                    KeyCode::Down => state.scroll_by(1, Next),
                    KeyCode::PageUp => state.scroll_by(state.scroll_height, Prev),
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
                            state.fix_scroll_invariants(false); // TODO: test this once we have multiple files
                        }
                    }
                    // TODO: configurable number of lines
                    KeyCode::Char('x') => state.expand_expander(state.cursor_pos.parent, 3, Prev),
                    KeyCode::Char('c') => state.expand_expander(state.cursor_pos.parent, 3, Next),
                    _ => write!(terminal.backend_mut(), "\x07")?,
                },
                crossterm::event::Event::Mouse(e) => {
                    // TODO
                }
                crossterm::event::Event::Resize(_, _) => {}
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

    // TODO: Register a panic/signal/??? handler that cleans up the terminal.

    let result = f(&mut terminal);

    terminal.show_cursor()?;
    crossterm::terminal::disable_raw_mode()?;
    crossterm::execute!(
        io::stdout(),
        crossterm::event::DisableMouseCapture,
        crossterm::terminal::LeaveAlternateScreen
    )?;

    result
}
