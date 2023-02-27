use crate::config::Style;
use std::ops::Range;
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr as _;

#[derive(Clone)]
pub(super) struct LineCell {
    /// Double-width characters are represented by a first LineCell with some egc and a second
    /// LineCell with an empty string.
    pub egc: String,
    pub offset: usize,
    pub change_highlight: bool,
    pub search_highlight: bool,
    pub syntax_highlight: Style,
    pub fabricated_symbol: bool,
}

pub(super) struct LineLayout {
    pub cells: Vec<LineCell>,
    pub newline_highlight: bool,
    pub offset_after_except_newline: usize,
    pub offset_after_with_newline: usize,
}

struct HighlightState<'a, T, F: Fn(&T) -> &Range<usize>> {
    ranges: &'a [T],
    get_range: F,
    index: usize,
    active: bool,
}

impl<'a, T, F: Fn(&T) -> &Range<usize>> HighlightState<'a, T, F> {
    fn new(ranges: &'a [T], offset: usize, get_range: F) -> HighlightState<'a, T, F> {
        let index = ranges.partition_point(|range| get_range(range).end <= offset);
        let active = index < ranges.len() && get_range(&ranges[index]).contains(&offset);
        HighlightState {
            ranges,
            get_range,
            index,
            active,
        }
    }

    fn advance(&mut self, offset: usize) {
        if self.index < self.ranges.len() && (self.get_range)(&self.ranges[self.index]).end <= offset {
            self.index += 1;
        }
        self.active = self.index < self.ranges.len() && (self.get_range)(&self.ranges[self.index]).contains(&offset);
    }
}

pub(super) fn layout_line(
    input: &str,
    change_ranges: &[Range<usize>],
    search_ranges: &[Range<usize>],
    syntax_ranges: &[(Range<usize>, Style)],
    mut offset: usize,
    end: usize,
    max_width: usize,
) -> LineLayout {
    let mut cells = vec![];

    let mut change_state = HighlightState::new(change_ranges, offset, |r| r);
    let mut search_state = HighlightState::new(search_ranges, offset, |r| r);
    let mut syntax_state = HighlightState::new(syntax_ranges, offset, |r| &r.0);

    let mut found_newline_length = 0;

    // The algorithm uses `unicode-segmentation` to split the line into EGCs, and `unicode-width`
    // to compute the width of each EGC. I don't know if it's correct (precisely matches what
    // terminal emulators do), but at least it matches the behavior of our library (tui-rs).
    for egc in input[offset..end].graphemes(true) {
        change_state.advance(offset);
        search_state.advance(offset);
        syntax_state.advance(offset);

        if egc == "\n" {
            found_newline_length = 1;
            break;
        }

        assert!(!egc.contains('\n'));

        let split = |string: &str| (string.chars().map(|c| c.to_string()).collect(), true);

        let (cell_strings, fabricated_symbol) = match egc {
            "\t" => split(&" ".repeat(8 - cells.len() % 8)),
            _ => {
                assert!(!egc.contains('\t'));
                let egc_width = egc.width();
                match egc_width {
                    // Replace nonprintable characters with an escape sequence. escape_debug is
                    // nicer than escape_default because it shows zero bytes as \0, not \u{0}.
                    0 => split(&egc.escape_debug().to_string()),
                    1 => (vec![egc.to_string()], false),
                    2 => (vec![egc.to_string(), "".to_string()], false),
                    _ => panic!("egc width was {egc_width}"),
                }
            }
        };

        assert!(cell_strings.len() <= max_width);

        if cells.len() + cell_strings.len() > max_width {
            break;
        }

        for cell_string in cell_strings {
            cells.push(LineCell {
                egc: cell_string,
                offset,
                change_highlight: change_state.active,
                search_highlight: search_state.active,
                syntax_highlight: if syntax_state.active {
                    syntax_ranges[syntax_state.index].1
                } else {
                    Default::default()
                },
                fabricated_symbol,
            });
        }

        offset += egc.len();
    }

    LineLayout {
        cells,
        newline_highlight: change_state.active,
        offset_after_except_newline: offset,
        offset_after_with_newline: offset + found_newline_length,
    }
}
