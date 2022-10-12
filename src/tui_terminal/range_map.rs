use std::collections::BTreeMap;
use std::iter::Peekable;
use std::ops::Range;

/// RangeMap<T> is like a BTreeMap<usize, T> which can quickly:
///
/// - get the value at a given key
/// - set a contiguous range of keys to the same value
/// - erase a contiguous range of keys
/// - iterate over contiguous ranges of keys that have the same value
pub struct RangeMap<T> {
    /// Invariant: first entry's value is Some.
    /// Invariant: last entry's value is None.
    /// Invariant: neighbors have different values.
    points: BTreeMap<usize, Option<T>>,
}

impl<T: Eq + Copy> RangeMap<T> {
    pub fn new() -> RangeMap<T> {
        RangeMap {
            points: Default::default(),
        }
    }

    pub fn get(&self, key: usize) -> Option<T> {
        match self.points.range(..=key).next_back() {
            Some((_k, &Some(v))) => Some(v),
            Some((_k, None)) => None,
            None => None,
        }
    }

    pub fn set(&mut self, key_range: Range<usize>, value: Option<T>) {
        if key_range.is_empty() {
            return;
        }
        let Range { start, end } = key_range;
        let end_value = self.get(end);

        // Rust doesn't have C++ `std::map::erase(iterator, iterator)` :(
        // https://users.rust-lang.org/t/removing-range-of-elements-from-btreemap/51582
        // This workaround is O(log(mapsize) * distance) rather than C++'s
        // O(log(mapsize) + distance), but oh well.
        let to_remove: Vec<_> = self.points.range(start..=end).map(|(&k, _v)| k).collect();
        for key in to_remove {
            self.points.remove(&key);
        }

        let mut insert = |key, value| {
            if self.get(key) != value {
                self.points.insert(key, value);
            }
        };
        insert(start, value);
        insert(end, end_value);
    }

    pub fn ranges(&self) -> impl Iterator<Item = (Range<usize>, T)> + '_ {
        Ranges(self.points.iter().peekable())
    }
}

struct Ranges<P>(P);

impl<'a, T: 'a + Copy, I: Iterator<Item = (&'a usize, &'a Option<T>)>> Iterator for Ranges<Peekable<I>> {
    type Item = (Range<usize>, T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (&key, value) = self.0.next()?;
            let (&next_key, _) = self.0.peek()?;
            if let &Some(value) = value {
                return Some((key..next_key, value));
            }
        }
    }
}

// TODO: Add tests.
