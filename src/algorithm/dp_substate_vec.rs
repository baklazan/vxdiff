use std::ops::{Index, IndexMut};

use super::scoring::DpSubstate;

#[derive(Clone)]
pub(super) struct DpStateVec {
    substates_count: usize,
    internal: Vec<DpSubstate>,
}

impl DpStateVec {
    pub fn new(length: usize, substates_count: usize) -> Self {
        DpStateVec {
            internal: vec![DpSubstate::default(); length * substates_count],
            substates_count,
        }
    }

    pub fn len(&self) -> usize {
        self.internal.len() / self.substates_count
    }
}

impl Index<usize> for DpStateVec {
    type Output = [DpSubstate];

    fn index(&self, index: usize) -> &Self::Output {
        &self.internal[(index * self.substates_count)..((index + 1) * self.substates_count)]
    }
}

impl IndexMut<usize> for DpStateVec {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.internal[(index * self.substates_count)..((index + 1) * self.substates_count)]
    }
}
