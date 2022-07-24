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
    BranchNode(BranchNode),
    PaddedGroupNode(PaddedGroupNode),
    FileHeaderLineNode(usize),
    ExpanderLineNode(usize),
}

#[derive(Clone)]
struct BranchNode {
    children: Vec<Node>,
    hidden: Range<usize>,
}

impl BranchNode {
    fn new(children: Vec<Node>, hidden: Range<usize>) -> Node {
        Node::BranchNode(BranchNode { children, hidden })
    }
}

#[derive(Clone)]
struct PaddedGroupNode {
    equal: bool,
    original: [Vec<HalfLine>; 2],
    cached_wrap: RefCell<Option<(usize, Vec<WrappedLine>)>>,
}

#[derive(Clone)]
struct HalfLine {
    // line_number TODO
    text_with_words: Vec<(bool, String)>, // TODO
    newline_highlight: bool,
}

#[derive(Clone, Default)]
struct WrappedLine {
    content: [WrappedHalfLine; 2], // TODO
}

#[derive(Clone, Default)]
struct WrappedHalfLine {
    // line_number TODO
    text_with_words: Vec<(bool, String)>, // TODO
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
    Node::PaddedGroupNode(PaddedGroupNode {
        equal,
        original: lines,
        cached_wrap: RefCell::new(None),
    })
}

fn build_initial_tree(diff: &Diff) -> Node {
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
                    file_children.push(BranchNode::new(padded_groups.clone(), context..length));
                    file_children.push(Node::ExpanderLineNode(length - 2 * context));
                    file_children.push(BranchNode::new(padded_groups, length - context..length));
                } else {
                    file_children.extend(padded_groups);
                }
            } else {
                file_children.push(build_padded_group(&diff.sections, &ops[begin..end]));
            }
        }

        let file_header_node = Node::FileHeaderLineNode(file_id);
        let file_content_node = BranchNode::new(file_children, 0..0);
        let file_node = BranchNode::new(vec![file_header_node, file_content_node], 1..2);
        root_children.push(file_node)
    }

    BranchNode::new(root_children, 0..0)
}

fn wrap_one_side(lines: &Vec<HalfLine>, wrap_width: usize) -> Vec<WrappedHalfLine> {
    // TODO: actually wrap
    let mut out: Vec<WrappedHalfLine> = vec![];
    for halfline in lines {
        let HalfLine {
            text_with_words,
            newline_highlight,
        } = halfline.clone();
        out.push(WrappedHalfLine {
            text_with_words,
            newline_highlight,
        });
    }
    out
}

fn recompute_wrap(node: &Node, wrap_width: usize) {
    match node {
        Node::BranchNode(node) => {
            for child in &node.children {
                recompute_wrap(child, wrap_width);
            }
        }
        Node::PaddedGroupNode(node) => {
            let mut wrap = node.cached_wrap.borrow_mut();
            match *wrap {
                Some((w, _)) if w == wrap_width => {
                    return;
                }
                _ => {}
            }
            let mut lines: Vec<WrappedLine> = vec![];
            for side in 0..2 {
                let wrapped_side = wrap_one_side(&node.original[side], wrap_width);
                if lines.len() < wrapped_side.len() {
                    lines.resize_with(wrapped_side.len(), Default::default);
                }
                for (i, line) in wrapped_side.into_iter().enumerate() {
                    lines[i].content[side] = line;
                }
            }
            *wrap = Some((wrap_width, lines));
        }
        _ => {}
    }
}

fn print_plainly(node: &Node, output: &mut impl io::Write) -> TheResult {
    match node {
        Node::BranchNode(node) => {
            for (i, child) in node.children.iter().enumerate() {
                if !node.hidden.contains(&i) {
                    print_plainly(child, output)?;
                }
            }
        }
        Node::PaddedGroupNode(node) => {
            // TODO
        }
        Node::FileHeaderLineNode(file_id) => {
            writeln!(output, "file #{}", file_id)?;
        }
        Node::ExpanderLineNode(hidden_count) => {
            writeln!(output, "...{} hidden lines...", hidden_count)?;
        }
    }
    Ok(())
}

pub fn print_side_by_side_diff_plainly(diff: &Diff, output: &mut impl io::Write) -> TheResult {
    let mut tree = build_initial_tree(diff);
    recompute_wrap(&mut tree, 80);
    print_plainly(&tree, output)
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct Position {
    branches: Vec<usize>,
    wrapped_line_index: Option<usize>,
}

impl Position {
    fn inc(&mut self, tree: &Node, wrap_width: usize, offset: isize) -> isize {
        // TODO???
        fn recurse(node: &Node, node_depth: usize, position: &mut Position, offset: isize) -> isize {
            match node {
                Node::BranchNode(node) => {}
                Node::PaddedGroupNode(node) => {}
                _ => {}
            }
            todo!()
        }

        let mut nodes = position_to_nodes(tree, self);

        for _ in 0..offset.unsigned_abs() {
            let node = nodes.last().unwrap();
            recompute_wrap(node, wrap_width);
            if let Node::PaddedGroupNode(node) = node {
                let old = self.wrapped_line_index.unwrap();
                let end = if offset < 0 {
                    0
                } else {
                    node.cached_wrap.borrow().as_ref().unwrap().1.len() - 1
                };
                if old != end {
                    self.wrapped_line_index = Some(if offset < 0 { old - 1 } else { old + 1 });
                    continue;
                }
            }
            //
            nodes.pop();
        }

        // TODO
        todo!()
    }
}

fn first_leaf_position(tree: &Node) -> Position {
    let mut out: Position = Default::default();
    let mut node = tree;
    while let Node::BranchNode(branch_node) = node {
        // TODO: what about hidden?
        out.branches.push(0);
        node = &branch_node.children[0];
    }
    if matches!(node, Node::PaddedGroupNode(_)) {
        out.wrapped_line_index = Some(0);
    }
    out
}

fn position_to_nodes<'a>(tree: &'a Node, position: &Position) -> Vec<&'a Node> {
    let mut out: Vec<&'a Node> = vec![tree];
    let mut node = tree;
    for &child_index in &position.branches {
        if let Node::BranchNode(branch_node) = node {
            if let Some(child_node) = branch_node.children.get(child_index) {
                node = child_node;
                out.push(child_node);
            } else {
                panic!("invalid position: out of bounds");
            }
        } else {
            panic!("invalid position: not a branch node");
        }
    }
    out
}

fn find_node_mut<'a>(tree: &'a mut Node, branches: &[usize]) -> &'a mut Node {
    let mut node = tree;
    for &child_index in branches {
        if let Node::BranchNode(branch_node) = node {
            if let Some(child_node) = branch_node.children.get_mut(child_index) {
                node = child_node;
            } else {
                panic!("invalid position: out of bounds");
            }
        } else {
            panic!("invalid position: not a branch node");
        }
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

impl Node {
    fn move_position(&self, self_depth: usize, position: &mut Position, offset: isize) -> isize {
        // TODO
        offset
    }
}

impl State {
    fn scroll_by(&mut self, offset: isize) {
        self.scroll_pos.inc(&self.tree, self.wrap_width, offset);
        self.cursor_pos.inc(&self.tree, self.wrap_width, offset);
        self.tree.move_position(0, &mut self.scroll_pos, offset);
        self.tree.move_position(0, &mut self.cursor_pos, offset);
        self.fix_scroll_invariants(false);
    }

    fn move_cursor_by(&mut self, offset: isize) {
        self.tree.move_position(0, &mut self.cursor_pos, offset);
        self.fix_scroll_invariants(true);
    }

    fn fix_scroll_invariants(&mut self, prefer_changing_scroll: bool) {
        // scroll_pos <= tree.end - (scroll_height-1)
        let top_to_bottom = isize::try_from(self.scroll_height - 1).unwrap();
        self.tree.move_position(0, &mut self.scroll_pos, top_to_bottom);
        let bottom_pos = self.scroll_pos.clone();
        self.tree.move_position(0, &mut self.scroll_pos, -top_to_bottom);

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
                self.tree.move_position(0, &mut self.scroll_pos, -top_to_bottom);
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
    let initial_scroll = first_leaf_position(&initial_tree);
    let mut state = State {
        tree: initial_tree,
        scroll_pos: initial_scroll.clone(),
        cursor_pos: initial_scroll,
        wrap_width: 80,
        scroll_height: usize::try_from(size.height)?,
    };
    recompute_wrap(&mut state.tree, 80);

    loop {
        let render = |new_size: tui::layout::Rect, buffer: &mut tui::buffer::Buffer| {
            if new_size != size {
                // TODO: resize, and afterwards:
                // size = new_size;
                // state.wrap_width = ...; state.scroll_height = ...;
            }

            // TODO: render
            buffer.set_string(0, 0, "hello", Default::default());

            let mut pos = state.cursor_pos.clone();
            for y in 0..state.scroll_height {
                let nodes = position_to_nodes(&state.tree, &pos);
                let node = nodes.last().unwrap();
                match node {
                    Node::BranchNode(_) => panic!("logic error: Position is a BranchNode"),
                    Node::PaddedGroupNode(node) => {
                        // TODO
                    }
                    Node::FileHeaderLineNode(file_id) => {
                        buffer.set_string(
                            0,
                            u16::try_from(y).unwrap(),
                            format!("file #{}", file_id),
                            Default::default(),
                        );
                    }
                    Node::ExpanderLineNode(hidden_count) => {
                        buffer.set_string(
                            0,
                            u16::try_from(y).unwrap(),
                            format!("...{} hidden lines...", hidden_count),
                            Default::default(),
                        );
                    }
                }
                for side in 0..2 {}
                pos.inc(&state.tree, state.wrap_width, 1);
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
                    KeyCode::Up => state.scroll_by(-1),
                    KeyCode::Down => state.scroll_by(1),
                    KeyCode::PageUp => state.scroll_by(-isize::try_from(size.height)?),
                    KeyCode::PageDown | KeyCode::Char(' ') => state.scroll_by(isize::try_from(size.height)?),
                    KeyCode::Char('w') | KeyCode::Char('k') => state.move_cursor_by(-1),
                    KeyCode::Char('s') | KeyCode::Char('j') => state.move_cursor_by(1),
                    KeyCode::Char('z') => {
                        // TODO: key assignment
                        let nodes = position_to_nodes(&state.tree, &state.cursor_pos);
                        if let Node::FileHeaderLineNode(_) = nodes.last().unwrap() {
                            let mut file_node = find_node_mut(&mut state.tree, &[state.cursor_pos.branches[0]]);
                            if let Node::BranchNode(file_node) = file_node {
                                file_node.hidden = if file_node.hidden.is_empty() { 1..2 } else { 0..0 };
                                state.fix_scroll_invariants(false); // TODO: test this once we have multiple files
                            } else {
                                panic!("logic error: file_node is not a BranchNode");
                            }
                        }
                    }
                    KeyCode::Char('c') => {
                        // TODO: key assignment
                        let nodes = position_to_nodes(&state.tree, &state.cursor_pos);
                        if let Node::ExpanderLineNode(hidden_count) = nodes.last().unwrap() {
                            if *hidden_count >= 5 {
                                // TODO: expand everything if <= 4
                                let mut pos = state.cursor_pos.clone();
                                *pos.branches.last_mut().unwrap() += 1;
                                let mut next_branch_node = find_node_mut(&mut state.tree, &pos.branches);
                                if let Node::BranchNode(next_branch_node) = next_branch_node {
                                    next_branch_node.hidden.start -= 3; // TODO: configurable
                                } else {
                                    panic!("logic error: expander is not followed by a BranchNode");
                                }
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
