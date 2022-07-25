use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use crossterm::{self, event::KeyCode};
use std::cell::RefCell;
use std::error::Error;
use std::io::{self, Write as _};
use std::ops::Range;
use tui;

type TheResult = Result<(), Box<dyn Error>>;

type TheTerminal = tui::terminal::Terminal<tui::backend::CrosstermBackend<io::Stdout>>;

fn same_section_group(sections: &Vec<Section>, a: (DiffOp, usize), b: (DiffOp, usize)) -> bool {
    (a.0 == DiffOp::Match && b.0 == DiffOp::Match && sections[a.1].equal && sections[b.1].equal)
        || (a.0 != DiffOp::Match && b.0 != DiffOp::Match)
}

#[derive(Clone)]
enum Node {
    Branch(BranchNode),
    PaddedGroup(PaddedGroupNode),
    FileHeaderLine(usize),
    ExpanderLine(usize),
}

impl Node {
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

#[derive(Clone)]
struct BranchNode {
    children: Vec<Node>,
    visible: Range<usize>,
}

#[derive(Clone)]
struct PaddedGroupNode {
    equal: bool,
    original: [Vec<HalfLine>; 2],
    cached_wrap: RefCell<Option<(usize, Vec<WrappedLine>)>>,
}

#[derive(Clone)]
struct HalfLine {
    // TODO: line_number
    text_with_words: Vec<(bool, String)>, // TODO: byte offset info and proper lifetime
    newline_highlight: bool,
}

type WrappedLine = [WrappedHalfLine; 2];

#[derive(Clone, Default)]
struct WrappedHalfLine {
    // TODO: line_number
    text_with_words: Vec<(bool, String)>, // TODO: byte offset info and proper lifetime
    newline_highlight: bool,
}

fn build_padded_group(sections: &Vec<Section>, ops: &[(DiffOp, usize)]) -> Node {
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

fn build_initial_tree(diff: &Diff) -> Node {
    fn make_branch(visible: Range<usize>, children: Vec<Node>) -> Node {
        Node::Branch(BranchNode { children, visible })
    }

    let mut root_children = vec![];
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let mut file_children = vec![];

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
                    file_children.push(make_branch(0..context, padded_groups.clone()));
                    file_children.push(Node::ExpanderLine(length - 2 * context));
                    file_children.push(make_branch(length - context..length, padded_groups));
                } else {
                    file_children.extend(padded_groups);
                }
            } else {
                file_children.push(build_padded_group(&diff.sections, &ops[begin..end]));
            }
        }

        let file_header_node = Node::FileHeaderLine(file_id);
        let file_content_node = make_branch(0..file_children.len(), file_children);
        let file_node = make_branch(0..1, vec![file_header_node, file_content_node]);
        root_children.push(file_node)
    }

    make_branch(0..root_children.len(), root_children)
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

impl PaddedGroupNode {
    /// Calls func with an up-to-date wrapped line list, refreshing the cache if needed. Panics if
    /// the RefCell is currently borrowed. So don't call with_wrapped_lines() again inside func.
    fn with_wrapped_lines<Ret>(&self, wrap_width: usize, func: impl FnOnce(&Vec<WrappedLine>) -> Ret) -> Ret {
        let mut wrap = self.cached_wrap.borrow_mut();
        match wrap.as_ref() {
            Some((w, l)) if *w == wrap_width => func(l),
            _ => {
                let mut lines: Vec<WrappedLine> = vec![];
                for side in 0..2 {
                    let wrapped_side = wrap_one_side(&self.original[side], wrap_width);
                    if lines.len() < wrapped_side.len() {
                        // TODO: temporary padding indicators
                        lines.resize_with(wrapped_side.len(), || {
                            [(); 2].map(|_| WrappedHalfLine {
                                text_with_words: vec![(false, "@".to_string())],
                                newline_highlight: false,
                            })
                        });
                    }
                    for (i, line) in wrapped_side.into_iter().enumerate() {
                        lines[i][side] = line;
                    }
                }
                let result = func(&lines);
                *wrap = Some((wrap_width, lines));
                result
            }
        }
    }
}

fn print_plainly(node: &Node, wrap_width: usize, output: &mut impl io::Write) -> TheResult {
    match node {
        Node::Branch(node) => {
            // clone() explanation: https://stackoverflow.com/q/63685464
            for i in node.visible.clone() {
                print_plainly(&node.children[i], wrap_width, output)?;
            }
        }
        Node::PaddedGroup(node) => {
            node.with_wrapped_lines(wrap_width, |lines| -> TheResult {
                for line in lines {
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
                    writeln!(output, "{} | {}", render_half(&line[0]), render_half(&line[1]))?;
                }
                Ok(())
            })?;
        }
        Node::FileHeaderLine(file_id) => {
            writeln!(output, "file #{}", file_id)?;
        }
        Node::ExpanderLine(hidden_count) => {
            writeln!(output, "...{} hidden lines...", hidden_count)?;
        }
    }
    Ok(())
}

pub fn print_side_by_side_diff_plainly(diff: &Diff, output: &mut impl io::Write) -> TheResult {
    let mut tree = build_initial_tree(diff);

    // Gotta expand the file headers in order to see any content.
    for child in &mut tree.as_branch_mut().children {
        let child = child.as_branch_mut();
        assert!(child.visible == (0..1));
        assert!(child.children.len() == 2);
        child.visible = 0..2;
    }

    print_plainly(&tree, 80, output)
}

/// Returns `if cond { true_iter } else { false_iter }` except that it works even if they have
/// different types. Loosely based on <https://stackoverflow.com/q/29760668>.
fn either_iterator<T>(
    cond: bool,
    true_iter: impl IntoIterator<Item = T>,
    false_iter: impl IntoIterator<Item = T>,
) -> impl Iterator<Item = T> {
    let a = (if cond { Some(true_iter) } else { None }).into_iter().flatten();
    let b = (if !cond { Some(false_iter) } else { None }).into_iter().flatten();
    a.chain(b)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Direction {
    Prev,
    Next,
}

use Direction::{Next, Prev};

impl Direction {
    fn opposite(self) -> Direction {
        match self {
            Prev => Next,
            Next => Prev,
        }
    }
}

type Position = Vec<usize>;

struct Mover<'a> {
    position: Position,
    /// Invariant: If we're not at a WrappedLine, nodes.len() == position.len()+1.
    /// If we're at a WrappedLine, nodes.len() == position.len() because WrappedLine is not a Node.
    nodes: Vec<&'a Node>,
    wrap_width: usize,
}

impl<'a> Mover<'a> {
    fn new(position: &Position, tree: &'a Node, wrap_width: usize) -> Mover<'a> {
        Mover {
            position: position.clone(),
            nodes: position_to_nodes(tree, &position),
            wrap_width,
        }
    }

    /// Returns the current node, but if the current position is a WrappedLine, returns its parent (PaddedGroup).
    fn last_node(&self) -> &'a Node {
        self.nodes.last().unwrap()
    }

    /// Precondition: Current position is a visible leaf.
    /// Moves to the next visible leaf in this direction. If there is none, moves to the root and returns false.
    #[must_use]
    fn go_next(&mut self, dir: Direction) -> bool {
        assert!(self.position.len() != 0);
        while self.position.len() != 0 {
            let depth = self.position.len() - 1;
            let original_child_index = self.position[depth];
            match self.nodes[depth] {
                Node::Branch(node) => {
                    self.position.pop();
                    self.nodes.pop();
                    for candidate in either_iterator(
                        dir == Next,
                        original_child_index + 1..node.visible.end,
                        (node.visible.start..original_child_index).rev(),
                    ) {
                        self.position.push(candidate);
                        self.nodes.push(&node.children[candidate]);
                        if self.go_boundary_leaf(dir) {
                            return true;
                        }
                        self.nodes.pop();
                        self.position.pop();
                    }
                }
                Node::PaddedGroup(node) => {
                    let next_index = if dir == Next {
                        original_child_index + 1
                    } else {
                        original_child_index.wrapping_sub(1)
                    };
                    if node.with_wrapped_lines(self.wrap_width, |lines| next_index < lines.len()) {
                        self.position[depth] = next_index;
                        return true;
                    }
                    self.position.pop();
                }
                Node::FileHeaderLine(_) | Node::ExpanderLine(_) => panic!("that node can't have children"),
            }
        }
        false
    }

    /// Precondition: Current position is a visible leaf.
    /// Moves in `direction` by `count`. If it reaches the first/last visible leaf of the tree, it stays there.
    /// Returns how much we moved.
    fn go_next_max(&mut self, count: usize, dir: Direction) -> usize {
        assert!(self.position.len() != 0);
        for i in 0..count {
            if !self.go_next(dir) {
                assert!(self.go_boundary_leaf(dir.opposite()));
                return i;
            }
        }
        count
    }

    /// Moves to the first (for Next) or last (for Prev) leaf which is a
    /// descendant (or equal) of the current position.
    /// If there is no visible leaf under this position, returns false and stays put.
    #[must_use]
    fn go_boundary_leaf(&mut self, dir: Direction) -> bool {
        if self.position.len() == self.nodes.len() {
            // We must be in a WrappedLine, which is a leaf.
            return true;
        }

        let depth = self.position.len();
        assert!(self.nodes.len() == depth + 1);
        match self.nodes[depth] {
            Node::Branch(node) => {
                for i in either_iterator(dir == Next, node.visible.clone(), node.visible.clone().rev()) {
                    self.position.push(i);
                    self.nodes.push(&node.children[i]);
                    if self.go_boundary_leaf(dir) {
                        return true;
                    }
                    self.nodes.pop();
                    self.position.pop();
                }
                false
            }
            Node::PaddedGroup(node) => node.with_wrapped_lines(self.wrap_width, |lines| {
                if lines.is_empty() {
                    false
                } else {
                    self.position.push(if dir == Next { 0 } else { lines.len() - 1 });
                    true
                }
            }),
            Node::FileHeaderLine(_) | Node::ExpanderLine(_) => true,
        }
    }
}

fn position_to_nodes<'a>(tree: &'a Node, position: &Position) -> Vec<&'a Node> {
    let mut out: Vec<&'a Node> = vec![tree];
    for i in 0..position.len() {
        match out[i] {
            Node::Branch(node) => {
                if let Some(child_node) = node.children.get(position[i]) {
                    out.push(child_node);
                } else {
                    panic!("invalid position: out of bounds");
                }
            }
            Node::PaddedGroup(_) => {
                if i != position.len() - 1 {
                    panic!("invalid position: WrappedLine has no children");
                }
            }
            Node::FileHeaderLine(_) | Node::ExpanderLine(_) => panic!("invalid position: leaf node has no children"),
        }
    }
    out
}

fn find_node_mut<'a>(tree: &'a mut Node, branches: &[usize]) -> &'a mut Node {
    let mut node = tree;
    for &child_index in branches {
        node = &mut node.as_branch_mut().children[child_index];
    }
    node
}

struct State {
    tree: Node,
    scroll_pos: Position,
    cursor_pos: Position,
    wrap_width: usize,
    scroll_height: usize,
}

fn move_position(position: &mut Position, tree: &Node, wrap_width: usize, offset: usize, dir: Direction) {
    let mut mover = Mover::new(position, tree, wrap_width);
    mover.go_next_max(offset, dir);
    *position = mover.position;
}

impl State {
    fn scroll_by(&mut self, offset: usize, dir: Direction) {
        move_position(&mut self.scroll_pos, &self.tree, self.wrap_width, offset, dir);
        move_position(&mut self.cursor_pos, &self.tree, self.wrap_width, offset, dir);
        self.fix_scroll_invariants(false);
    }

    fn move_cursor_by(&mut self, offset: usize, dir: Direction) {
        move_position(&mut self.cursor_pos, &self.tree, self.wrap_width, offset, dir);
        self.fix_scroll_invariants(true);
    }

    fn fix_scroll_invariants(&mut self, prefer_changing_scroll: bool) {
        // scroll_pos <= tree.end - (scroll_height-1)
        let top_bottom_dist = self.scroll_height - 1;
        move_position(&mut self.scroll_pos, &self.tree, self.wrap_width, top_bottom_dist, Next);
        let bottom_pos = self.scroll_pos.clone();
        move_position(&mut self.scroll_pos, &self.tree, self.wrap_width, top_bottom_dist, Prev);

        // scroll_pos <= cursor_pos <= bottom_pos
        if self.cursor_pos < self.scroll_pos {
            if prefer_changing_scroll {
                self.scroll_pos = self.cursor_pos.clone();
            } else {
                self.cursor_pos = self.scroll_pos.clone();
            }
        }
        if self.cursor_pos > bottom_pos {
            if prefer_changing_scroll {
                self.scroll_pos = self.cursor_pos.clone();
                move_position(&mut self.scroll_pos, &self.tree, self.wrap_width, top_bottom_dist, Prev);
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

pub fn run_tui(diff: &Diff, terminal: &mut TheTerminal) -> TheResult {
    let mut size = terminal.size()?;

    let initial_tree = build_initial_tree(diff);
    let initial_scroll = {
        let mut mover = Mover::new(&vec![], &initial_tree, 80);
        assert!(mover.go_boundary_leaf(Next));
        mover.position
    };
    let mut state = State {
        tree: initial_tree,
        scroll_pos: initial_scroll.clone(),
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

            let mut mover = Mover::new(&state.scroll_pos, &state.tree, state.wrap_width);
            for y in 0..state.scroll_height {
                let is_cursor = mover.position.len() == state.cursor_pos.len()
                    && mover.position.iter().rev().eq(state.cursor_pos.iter().rev());
                if is_cursor {
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
                match mover.last_node() {
                    Node::Branch(_) => panic!("logic error: Position is a BranchNode"),
                    Node::PaddedGroup(node) => node.with_wrapped_lines(state.wrap_width, |lines| {
                        let line = &lines[*mover.position.last().unwrap()];
                        for side in 0..2 {
                            let spans = line[side]
                                .text_with_words
                                .iter()
                                .map(|(highlight, part)| {
                                    tui::text::Span::styled(part, get_style(*highlight, side, node.equal))
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
                    }),
                    Node::FileHeaderLine(file_id) => {
                        let is_open = match &mover.nodes[mover.nodes.len() - 2].as_branch().visible.end {
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
                    Node::ExpanderLine(hidden_count) => {
                        buffer.set_string(
                            1,
                            u16::try_from(y).unwrap(),
                            format!("...{} hidden lines... (press x/c to expand)", hidden_count),
                            tui::style::Style::default()
                                .fg(tui::style::Color::Black)
                                .bg(tui::style::Color::White),
                        );
                    }
                }
                if !mover.go_next(Next) {
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
                    KeyCode::Char('z') => {
                        // TODO: key assignment
                        let nodes = position_to_nodes(&state.tree, &state.cursor_pos);
                        if let Node::FileHeaderLine(_) = nodes.last().unwrap() {
                            let file_node =
                                find_node_mut(&mut state.tree, &state.cursor_pos[0..state.cursor_pos.len() - 1])
                                    .as_branch_mut();
                            file_node.visible.end = match file_node.visible.end {
                                1 => 2,
                                2 => 1,
                                _ => panic!("logic error: bad file_node.visible.end"),
                            };
                            state.fix_scroll_invariants(false); // TODO: test this once we have multiple files
                        }
                    }
                    KeyCode::Char('x') => {
                        // TODO: key assignment
                        let nodes = position_to_nodes(&state.tree, &state.cursor_pos);
                        if let Node::ExpanderLine(hidden_count) = nodes.last().unwrap() {
                            let hidden_count = *hidden_count; // TODO: rust syntax???
                            if hidden_count >= 5 {
                                // TODO: expand everything if <= 4
                                let my_index = *state.cursor_pos.last().unwrap();
                                let parent_node =
                                    find_node_mut(&mut state.tree, &state.cursor_pos[0..state.cursor_pos.len() - 1])
                                        .as_branch_mut();
                                // TODO: configurable number of lines
                                parent_node.children[my_index] = Node::ExpanderLine(hidden_count - 3);
                                parent_node.children[my_index - 1].as_branch_mut().visible.end += 3;
                                // TODO: adjust scroll_pos to keep cursor on the same level.
                                // (for now we at least keep cursor in view)
                                state.fix_scroll_invariants(true);
                            }
                        }
                    }
                    KeyCode::Char('c') => {
                        // TODO: key assignment
                        let nodes = position_to_nodes(&state.tree, &state.cursor_pos);
                        if let Node::ExpanderLine(hidden_count) = nodes.last().unwrap() {
                            let hidden_count = *hidden_count; // TODO: rust syntax???
                            if hidden_count >= 5 {
                                // TODO: expand everything if <= 4
                                let my_index = *state.cursor_pos.last().unwrap();
                                let parent_node =
                                    find_node_mut(&mut state.tree, &state.cursor_pos[0..state.cursor_pos.len() - 1])
                                        .as_branch_mut();
                                // TODO: configurable number of lines
                                parent_node.children[my_index] = Node::ExpanderLine(hidden_count - 3);
                                parent_node.children[my_index + 1].as_branch_mut().visible.start -= 3;
                            }
                        }
                    }
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
