use index_vec::{index_vec, IndexVec};

use crate::algorithm::{indices::LineIndex, PartitionedText};

use super::TScore;

// For two consecutive lines, compute how much we don't want to
// start/end a big block (a moved block, a multiline indel etc.)
// between these two lines
pub(in crate::algorithm) struct LineBoundsScoring {
    bound_scores: Vec<[IndexVec<LineIndex, TScore>; 2]>,
}

impl LineBoundsScoring {
    const PENALTY_PER_BYTE: TScore = -0.000001;

    pub(in crate::algorithm) fn new(text_lines: &[[PartitionedText; 2]]) -> Self {
        let bound_scores = text_lines
            .iter()
            .map(|file_lines| {
                [0, 1].map(|side| {
                    let mut side_scores = index_vec![];
                    let side_lines = &file_lines[side];
                    for line in 0..=file_lines[side].part_count() {
                        let previous_length = if line == 0 {
                            0
                        } else {
                            side_lines.part_bounds[line] - side_lines.part_bounds[line - 1]
                        };
                        let next_length = if line + 1 >= side_lines.part_count() {
                            0
                        } else {
                            side_lines.part_bounds[line + 1] - side_lines.part_bounds[line]
                        };
                        side_scores.push(Self::PENALTY_PER_BYTE * (usize::min(previous_length, next_length) as TScore));
                    }
                    side_scores
                })
            })
            .collect();
        LineBoundsScoring { bound_scores }
    }

    pub(in crate::algorithm) fn score_side(&self, side: usize, file_id: usize, line_bound: LineIndex) -> TScore {
        self.bound_scores[file_id][side][line_bound]
    }

    pub(in crate::algorithm) fn score(&self, file_ids: [usize; 2], line_bound: [LineIndex; 2]) -> TScore {
        self.bound_scores[file_ids[0]][0][line_bound[0]] + self.bound_scores[file_ids[1]][1][line_bound[1]]
    }
}
