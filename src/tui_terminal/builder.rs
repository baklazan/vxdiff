use super::doc::{ExpanderId, FrozenDocument, FrozenDocumentReader, GenericDocument, GenericDocumentBuilder, Nodeish};
use super::{ExtendedDiff, HalfLineStyle, PaddedGroupNode, PaddedGroupRawElement, TextSource};
use crate::algorithm::{DiffOp, FileDiff};
use crate::config::Config;
use std::ops::Range;

pub(super) enum Node {
    PaddedGroup(PaddedGroupNode),
    FileHeaderLine(usize),
    Expander(ExpanderId),
}

impl Nodeish for Node {
    fn get_owned_byte_offsets(&self) -> Option<[Range<usize>; 2]> {
        match self {
            Node::PaddedGroup(node) => Some(node.owned_byte_offsets.clone()),
            _ => None,
        }
    }
}

pub(super) type Document = GenericDocument<Node>;

type DocumentBuilder = GenericDocumentBuilder<Node>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SectionType {
    MatchEqual,
    MatchUnequal,
    InsertDelete,
    Phantom,
}

struct Builder<'a> {
    config: &'a Config,
    diff: &'a ExtendedDiff<'a>,
    frozen: Option<&'a FrozenDocument>,
    doc: DocumentBuilder,
    offsets: [usize; 2],
}

impl<'a> Builder<'a> {
    fn is_move(&self, (op, section_id): (DiffOp, usize)) -> bool {
        op != DiffOp::Match
            && self.diff.section_sides[section_id][0].is_some()
            && self.diff.section_sides[section_id][1].is_some()
    }

    fn is_continuing_move(&self, a: (DiffOp, usize), b: (DiffOp, usize)) -> bool {
        let other_side = if a.0 == DiffOp::Insert { 0 } else { 1 };
        self.is_move(a)
            && self.is_move(b)
            && a.0 == b.0
            && self.diff.section_side(a.1, other_side).byte_range.end
                == self.diff.section_side(b.1, other_side).byte_range.start
    }

    fn get_type(&self, (op, section_id): (DiffOp, usize)) -> SectionType {
        if op == DiffOp::Match {
            if self.diff.sections[section_id].equal {
                SectionType::MatchEqual
            } else {
                SectionType::MatchUnequal
            }
        } else {
            if self.config.phantom_rendering && self.is_move((op, section_id)) {
                SectionType::Phantom
            } else {
                SectionType::InsertDelete
            }
        }
    }

    fn get_description_of_move(&self, (op, section_id): (DiffOp, usize), render_side: usize) -> String {
        let other_side = if op == DiffOp::Delete { 1 } else { 0 };
        let section_other_side = self.diff.section_side(section_id, other_side);
        let start = section_other_side.byte_range.start;
        let file_other_side = &self.diff.file_sides[section_other_side.file_id][other_side];
        let filename = &file_other_side.filename;
        let line_number = file_other_side.byte_offset_to_line_number(start);
        let direction = match (other_side, render_side) {
            (0, 1) => "Moved from",
            (1, 0) => "Moved to",
            _ => "Preview of",
        };
        format!("{direction} {filename}:{line_number}")
    }

    fn new_padded_group(&self) -> PaddedGroupNode {
        PaddedGroupNode {
            raw_elements: Default::default(),
            owned_byte_offsets: self.offsets.map(|offset| offset..offset),
            cached_wrap: Default::default(),
        }
    }

    fn add_fabricated(&self, node: &mut PaddedGroupNode, style: HalfLineStyle, side: usize, content: String) {
        node.raw_elements[side].push(PaddedGroupRawElement {
            style,
            offset_override_for_selection: Some(self.offsets[side]),
            source: TextSource::Fabricated(content),
        });
    }

    fn add_section(
        &mut self,
        node: &mut PaddedGroupNode,
        styles: [HalfLineStyle; 2],
        (op, section_id): (DiffOp, usize),
        both_sides: bool,
    ) {
        for side in 0..2 {
            if both_sides || op.movement()[side] != 0 {
                let style = styles[side];
                let section_side = self.diff.section_side(section_id, side);
                let offset_override_for_selection = if op.movement()[side] != 0 {
                    // TODO: more asserts
                    self.offsets[side] = section_side.byte_range.end;
                    None
                } else {
                    Some(self.offsets[side])
                };
                node.raw_elements[side].push(PaddedGroupRawElement {
                    style,
                    offset_override_for_selection,
                    source: TextSource::Section(section_id),
                });
                node.owned_byte_offsets[side].end = self.offsets[side];
                let owned_byte_offsets = node.owned_byte_offsets[side].clone();
                let file_side = &self.diff.file_sides[section_side.file_id][side];
                if !file_side.content[section_side.byte_range.clone()].ends_with('\n') {
                    self.add_fabricated(node, style, side, "No newline at end of file".to_string());
                }
            }
        }
    }

    fn build_match_equal(&mut self, ops: &[(DiffOp, usize)], is_first: bool, is_last: bool) {
        let nid_base = self.doc.next_nid();
        let mut node_offsets = vec![];

        for &op in ops {
            let mut node = self.new_padded_group();
            self.add_section(&mut node, [HalfLineStyle::Equal; 2], op, true);
            node_offsets.push(node.owned_byte_offsets.clone());
            self.doc.add_node(Node::PaddedGroup(node));
        }

        let length = ops.len();
        let context_before = if is_first { 0 } else { self.config.context_lines };
        let context_after = if is_last { 0 } else { self.config.context_lines };
        if length <= context_before + context_after + 1 {
            return;
        }

        let nid_range = (nid_base + context_before)..(nid_base + length - context_after);

        let first_offsets = &node_offsets[nid_range.start - nid_base];
        let last_offsets = &node_offsets[nid_range.end - 1 - nid_base];
        let expander_id = [
            self.doc.current_file_id(),
            nid_range.len(),
            first_offsets[0].start,
            last_offsets[0].end,
            first_offsets[1].start,
            last_offsets[1].end,
        ];
        let expander_id = ExpanderId(expander_id.map(usize::to_le_bytes).concat());

        self.doc.add_original_expander(expander_id.clone(), nid_range.clone());

        let state = FrozenDocumentReader(self.frozen)
            .get_expander_state(&expander_id)
            .unwrap_or_else(|| vec![true; nid_range.len()]);
        let mut last = false;
        let mut start = 0;
        for (i, value) in state.into_iter().chain(std::iter::once(false)).enumerate() {
            let nid = nid_range.start + i;
            if value != last {
                if value {
                    start = nid;
                } else {
                    let expander_node = Node::Expander(expander_id.clone());
                    self.doc.add_expander(start, nid - 1, expander_node);
                }
            }
            last = value;
        }
    }

    fn build_match_unequal(&mut self, op: (DiffOp, usize)) {
        let mut node = self.new_padded_group();
        self.add_section(&mut node, [HalfLineStyle::Change; 2], op, true);
        self.doc.add_node(Node::PaddedGroup(node));
    }

    fn build_insert_delete(&mut self, ops: &[(DiffOp, usize)]) {
        let mut node = self.new_padded_group();
        for i in 0..ops.len() {
            let style = if self.is_move(ops[i]) {
                HalfLineStyle::Move
            } else {
                HalfLineStyle::Change
            };
            if self.is_move(ops[i]) && (i == 0 || !self.is_continuing_move(ops[i - 1], ops[i])) {
                let side = if ops[i].0 == DiffOp::Delete { 0 } else { 1 };
                self.add_fabricated(&mut node, style, side, self.get_description_of_move(ops[i], side));
            }
            self.add_section(&mut node, [style; 2], ops[i], false);
        }
        self.doc.add_node(Node::PaddedGroup(node));
    }

    fn build_phantom(&mut self, op: (DiffOp, usize), prev_op: Option<(DiffOp, usize)>) {
        let styles = if op.0 == DiffOp::Delete {
            [HalfLineStyle::Move, HalfLineStyle::Phantom]
        } else {
            [HalfLineStyle::Phantom, HalfLineStyle::Move]
        };
        if prev_op.is_none() || !self.is_continuing_move(prev_op.unwrap(), op) {
            let mut node = self.new_padded_group();
            for side in 0..2 {
                self.add_fabricated(&mut node, styles[side], side, self.get_description_of_move(op, side));
            }
            self.doc.add_node(Node::PaddedGroup(node));
        }
        let mut node = self.new_padded_group();
        self.add_section(&mut node, styles, op, true);
        self.doc.add_node(Node::PaddedGroup(node));
    }
}

pub(super) fn build_document(
    config: &crate::config::Config,
    diff: &crate::tui_terminal::ExtendedDiff,
    frozen: Option<&FrozenDocument>,
) -> Document {
    FrozenDocumentReader(frozen).assert_files_length(diff.files.len());

    let default_is_open = config.open_all_files || diff.files.len() == 1;

    let mut b = Builder {
        config,
        diff,
        frozen,
        doc: DocumentBuilder::new(),
        offsets: [0; 2],
    };

    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let is_open = FrozenDocumentReader(frozen)
            .was_file_open(file_id)
            .unwrap_or(default_is_open);
        b.doc.add_file(is_open, Node::FileHeaderLine(file_id));
        b.offsets = [0; 2];

        let mut op_index = 0;
        while op_index < ops.len() {
            let begin = op_index;
            let section_type = b.get_type(ops[begin]);
            let mut end = begin + 1;
            if section_type == SectionType::MatchEqual || section_type == SectionType::InsertDelete {
                while end < ops.len() && b.get_type(ops[end]) == section_type {
                    end += 1;
                }
            }
            op_index = end;

            match section_type {
                SectionType::MatchEqual => b.build_match_equal(&ops[begin..end], begin == 0, end == ops.len()),
                SectionType::MatchUnequal => b.build_match_unequal(ops[begin]),
                SectionType::InsertDelete => b.build_insert_delete(&ops[begin..end]),
                SectionType::Phantom => b.build_phantom(ops[begin], begin.checked_sub(1).map(|prev| ops[prev])),
            }
        }
    }

    b.doc.into_inner()
}
