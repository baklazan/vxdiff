use std::ops::{Index, IndexMut};

use super::scoring::DpSubstate;

#[derive(Clone)]
pub(super) struct DpStateVec {
    substates_count: usize,
    internal: Vec<DpSubstate>,
    index_offset: usize,
}

impl DpStateVec {
    pub fn new(length: usize, substates_count: usize) -> Self {
        DpStateVec {
            internal: vec![DpSubstate::default(); length * substates_count],
            substates_count,
            index_offset: 0,
        }
    }

    pub fn new_with_offset(length: usize, substates_count: usize, index_offset: usize) -> Self {
        DpStateVec {
            substates_count,
            internal: vec![DpSubstate::default(); length * substates_count],
            index_offset,
        }
    }
}

impl Index<usize> for DpStateVec {
    type Output = [DpSubstate];

    fn index(&self, mut index: usize) -> &Self::Output {
        index -= self.index_offset;
        &self.internal[(index * self.substates_count)..((index + 1) * self.substates_count)]
    }
}

impl IndexMut<usize> for DpStateVec {
    fn index_mut(&mut self, mut index: usize) -> &mut Self::Output {
        index -= self.index_offset;
        &mut self.internal[(index * self.substates_count)..((index + 1) * self.substates_count)]
    }
}
