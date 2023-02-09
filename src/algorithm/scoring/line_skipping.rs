use crate::algorithm::indices::LineIndex;

use super::{line_bounds_scoring::LineBoundsScoring, TScore};

pub const SKIP_SIMILARITY_THRESHOLD: f64 = 0.2;
pub(in crate::algorithm) struct LineGapsScoring<'a> {
    bounds_scoring: &'a LineBoundsScoring,
}

impl<'a> LineGapsScoring<'a> {
    pub fn new(bounds_scoring: &'a LineBoundsScoring) -> Self {
        LineGapsScoring { bounds_scoring }
    }

    const LINE_GAP_EDGE_COST: TScore = -2.0;
    const LINE_GAP_CONTINUATION_COST: TScore = -0.2;

    pub fn gap_edge(&self, side: usize, file_id: usize, line_index: LineIndex) -> TScore {
        self.bounds_scoring.score_side(side, file_id, line_index) + Self::LINE_GAP_EDGE_COST
    }

    pub fn gap_continuation(&self) -> TScore {
        Self::LINE_GAP_CONTINUATION_COST
    }
}
