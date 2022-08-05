use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use crossterm::{self, event::KeyCode};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, Write as _};
use std::ops::{DerefMut as _, Range};

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
struct HalfLine {
    // TODO: line_number
    text_with_words: Vec<(bool, String)>, // TODO: byte offset info and proper lifetime
    newline_highlight: bool,
}

#[derive(Clone, Default)]
struct WrappedHalfLine {
    // TODO: line_number
    text_with_words: Vec<(bool, String)>, // TODO: byte offset info and proper lifetime
    newline_highlight: bool,
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
    original: [Vec<HalfLine>; 2],
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
    let mut lines = [vec![], vec![]];
    for &(op, section_id) in ops {
        // TODO: change movement() return type and make it public
        let movement = match op {
            DiffOp::Delete => [1, 0],
            DiffOp::Insert => [0, 1],
            DiffOp::Match => [1, 1],
        };
        let section = &sections[section_id];
        // TODO: Add "moved to/from" info line.
        // - precompute file index & lineno for each section * side
        // - track local lineno to handle sequences of same moves
        for side in 0..2 {
            if movement[side] == 0 {
                continue;
            }
            let mut current_line: Vec<(bool, String)> = vec![];
            for &(highlight, text) in &section.sides[side].text_with_words {
                for part_with_eol in text.split_inclusive('\n') {
                    let (part, eol) = match part_with_eol.strip_suffix('\n') {
                        Some(part) => (part, true),
                        None => (part_with_eol, false),
                    };
                    current_line.push((highlight, part.to_string()));
                    if eol {
                        lines[side].push(HalfLine {
                            text_with_words: current_line,
                            newline_highlight: highlight,
                        });
                        current_line = vec![];
                    }
                }
            }
            if !current_line.is_empty() {
                lines[side].push(HalfLine {
                    text_with_words: current_line,
                    newline_highlight: false,
                });
                lines[side].push(HalfLine {
                    text_with_words: vec![(false, "No newline at end of file".to_string())],
                    newline_highlight: false,
                });
            }
        }
        if op != DiffOp::Match || !section.equal {
            equal = false;
        }
    }
    Node::PaddedGroup(PaddedGroupNode {
        equal,
        original: lines,
        cached_wrap: RefCell::new(None),
    })
}

fn build_initial_tree(diff: &Diff) -> Tree {
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
            while end < ops.len() && same_section_group(&diff.sections, ops[begin], ops[end]) {
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
                    padded_groups.push(build_padded_group(&diff.sections, &ops[i..i + 1]));
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
                tree.add_child(file_content_nid, build_padded_group(&diff.sections, &ops[begin..end]));
            }
        }
    }

    tree
}

fn wrap_one_side(lines: &Vec<HalfLine>, wrap_width: usize) -> Vec<WrappedHalfLine> {
    // TODO: actually wrap
    let mut out: Vec<WrappedHalfLine> = vec![];
    for halfline in lines {
        out.push(WrappedHalfLine {
            text_with_words: halfline.text_with_words.clone(),
            newline_highlight: halfline.newline_highlight,
        });
    }
    out
}

impl Node {
    /// Calls func with this node's current UILine list, recomputing it if needed.
    /// Panics if self is a branch node.
    /// Don't call with_ui_lines recursively (from inside func).
    fn with_ui_lines<Ret>(&self, wrap_width: usize, func: impl FnOnce(&[UILine]) -> Ret) -> Ret {
        match self {
            Node::Branch(_) => panic!("unexpected Branch node"),
            Node::PaddedGroup(node) => match node.cached_wrap.borrow_mut().deref_mut() {
                Some((w, l)) if *w == wrap_width => func(l),
                cached_wrap => {
                    let padding = WrappedHalfLine {
                        // TODO: temporary padding indicators
                        text_with_words: vec![(false, "@".to_string())],
                        newline_highlight: false,
                    };
                    let wrapped_sides = [0, 1].map(|side| wrap_one_side(&node.original[side], wrap_width));
                    let len = std::cmp::max(wrapped_sides[0].len(), wrapped_sides[1].len());
                    let padded_wrapped_sides =
                        wrapped_sides.map(|side| side.into_iter().chain(std::iter::repeat(padding.clone())));
                    let [side0, side1] = padded_wrapped_sides;
                    let lines: Vec<UILine> = std::iter::zip(side0, side1)
                        .map(|(line0, line1)| UILine::Sides([line0, line1]))
                        .take(len)
                        .collect();
                    let result = func(&lines);
                    *cached_wrap = Some((wrap_width, lines));
                    result
                }
            },
            Node::FileHeaderLine(file_id) => func(&[UILine::FileHeaderLine(*file_id)]),
            Node::ExpanderLine(0) => func(&[]),
            Node::ExpanderLine(hidden_count) => func(&[UILine::ExpanderLine(*hidden_count)]),
        }
    }
}

type TheResult = Result<(), Box<dyn Error>>;

fn print_plainly(tree: &Tree, nid: Nid, wrap_width: usize, output: &mut impl io::Write) -> TheResult {
    let node = tree.node(nid);
    if let Node::Branch(node) = node {
        // clone() explanation: https://stackoverflow.com/q/63685464
        for i in node.visible.clone() {
            print_plainly(tree, node.children[i], wrap_width, output)?;
        }
        return Ok(());
    }
    node.with_ui_lines(wrap_width, |lines| -> TheResult {
        for line in lines {
            match line {
                UILine::Sides(sides) => {
                    let render_half = |whl: &WrappedHalfLine| -> String {
                        let mut res = String::new();
                        let mut length = 0;
                        for (highlight, part) in &whl.text_with_words {
                            if *highlight {
                                res.push_str("\x1b[1m");
                            }
                            res.push_str(part);
                            if *highlight {
                                res.push_str("\x1b[0m");
                            }
                            length += part.len();
                        }
                        res.push_str(&" ".repeat(wrap_width - length));
                        res
                    };
                    writeln!(output, "{} | {}", render_half(&sides[0]), render_half(&sides[1]))?;
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

pub fn print_side_by_side_diff_plainly(diff: &Diff, output: &mut impl io::Write) -> TheResult {
    let mut tree = build_initial_tree(diff);

    // Gotta expand the file headers in order to see any content.
    let mut child_option_nid = tree.bordering_child(tree.root, First);
    while let Some(child_nid) = child_option_nid {
        let child_branch_node = tree.node_mut(child_nid).as_branch_mut();
        assert!(child_branch_node.visible == (0..1));
        assert!(child_branch_node.children.len() == 2);
        child_branch_node.visible = 0..2;
        child_option_nid = tree.sibling(child_nid, Next);
    }

    print_plainly(&tree, tree.root, 80, output)
}

struct TreeView<'a> {
    tree: &'a Tree,
    wrap_width: usize,
}

impl<'a> TreeView<'a> {
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
            node.with_ui_lines(self.wrap_width, |lines| {
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
        if next_line_index < parent.with_ui_lines(self.wrap_width, |lines| lines.len()) {
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

struct State {
    tree: Tree,
    scroll_pos: LeafPosition,
    cursor_pos: LeafPosition,
    wrap_width: usize,
    scroll_height: usize,
}

impl State {
    fn tree_view(&self) -> TreeView {
        TreeView {
            tree: &self.tree,
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

pub fn run_tui(diff: &Diff, terminal: &mut TheTerminal) -> TheResult {
    let mut size = terminal.size()?;

    let initial_tree = build_initial_tree(diff);
    let initial_scroll = TreeView {
        tree: &initial_tree,
        wrap_width: 80,
    }
    .bordering_leaf_under(initial_tree.root, First)
    .unwrap();
    let mut state = State {
        tree: initial_tree,
        scroll_pos: initial_scroll,
        cursor_pos: initial_scroll,
        wrap_width: 80,
        scroll_height: usize::try_from(size.height)?,
    };
    // recompute_wrap(&state.tree, 80);

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
                fn get_style(highlight: bool, side: usize, equal: bool) -> tui::style::Style {
                    let mut style = tui::style::Style::default();
                    if highlight {
                        style = style.add_modifier(tui::style::Modifier::BOLD | tui::style::Modifier::UNDERLINED);
                    }
                    if !equal {
                        style = style.fg([tui::style::Color::Red, tui::style::Color::Green][side]);
                    }
                    style
                }
                let parent_node = state.tree.node(pos.parent);
                parent_node.with_ui_lines(state.wrap_width, |lines| match &lines[pos.line_index] {
                    UILine::Sides(sides) => {
                        let equal = match parent_node {
                            Node::PaddedGroup(node) => node.equal,
                            _ => panic!("wat"),
                        };
                        for side in 0..2 {
                            let spans = sides[side]
                                .text_with_words
                                .iter()
                                .map(|(highlight, part)| {
                                    tui::text::Span::styled(part, get_style(*highlight, side, equal))
                                })
                                .collect();
                            buffer.set_spans(
                                u16::try_from(1 + side * (state.wrap_width + 3)).unwrap(),
                                u16::try_from(y).unwrap(),
                                &tui::text::Spans(spans),
                                u16::try_from(state.wrap_width).unwrap(),
                            );
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
                if let Some(next) = state.tree_view().next_leaf(pos, Next) {
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
