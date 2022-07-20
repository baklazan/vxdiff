use super::algorithm::{Diff, DiffOp, FileDiff, Section};
use std::error::Error;
use std::fmt::Write;
use std::io;
use std::ops::{Range, RangeInclusive};

type TheResult = Result<(), Box<dyn Error>>;

struct RenderContext<'a> {
    diff: &'a Diff<'a>,

    // indexed same as Diff.files
    file_details: Vec<Option<FileDetails>>,
}

struct FileDetails {
    expanded: bool,

    // TODO
    section_groups: Vec<SectionGroup>,
}

struct SectionGroup {
    section_ids: Vec<(DiffOp, usize)>,
    // TODO: what are these? line numbers? indexes into section_ids?
    // TODO: remove Option and rely on is_empty()?
    hidden_lines: Option<RangeInclusive<usize>>,
}

fn create_file_details(diff: &Diff, file_id: usize) -> FileDetails {
    let mut section_groups = vec![];

    let ops = &diff.files[file_id].ops;
    let mut i = 0;
    while i < ops.len() {
        let begin = i;
        let mut end = begin + 1;
        while end < ops.len() && same_section_group(&diff.sections, ops[begin], ops[end]) {
            end += 1;
        }
        i = end;

        section_groups.push(SectionGroup {
            section_ids: ops[begin..end].to_vec(),
            hidden_lines: None, // TODO
        });
    }

    FileDetails {
        expanded: false,
        section_groups,
    }
}

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
    cached_wrap: Option<(usize, Vec<WrappedLine>)>, // TODO
}

#[derive(Clone)]
struct HalfLine {
    // line_number TODO
    text_with_words: Vec<(bool, String)>, // TODO
    newline_highlight: bool,
}

#[derive(Clone)]
struct WrappedLine {
    content: [String; 2], // TODO
}

fn build_padded_group(sections: &Vec<Section>, ops: &[(DiffOp, usize)]) -> Node {
    todo!();
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
        cached_wrap: None,
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
            // - build_padded_group stops checking movement().

            let section_group_is_match_equal = ops[begin].0 == DiffOp::Match && diff.sections[ops[begin].1].equal;

            if section_group_is_match_equal {
                let length = end - begin;
                let mut padded_groups = vec![];
                for i in begin..end {
                    padded_groups.push(build_padded_group(&diff.sections, &ops[i..i + 1]));
                }

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

pub fn run_tui(diff: &Diff) -> TheResult {
    /*
    let uistate = UiState {
        expanded_files: vec![false; diff.files.len()],
        // TODO
        collapsed_equal_sections: diff.sections.iter().map(|s| None).collect(),
    };
    let root_node = Node::RootNode(RootNode { diff });
    dbg!(root_node.child_count(&uistate));
    let _foo = root_node.build_child(&uistate, 0);
    */
    Ok(())
}
