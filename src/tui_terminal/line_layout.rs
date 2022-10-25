use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr as _;

#[derive(Clone)]
pub(super) struct LineCell {
    /// Double-width characters are represented by a first LineCell with some egc and a second
    /// LineCell with an empty string.
    pub egc: String,
    pub offset: usize,
    pub highlight: bool,
    pub search_highlight: bool,
    pub fabricated_symbol: bool,
}

pub(super) struct LineLayout {
    pub cells: Vec<LineCell>,
    pub newline_highlight: bool,
    pub offset_after_except_newline: usize,
    pub offset_after_with_newline: usize,
}

pub(super) fn layout_line(
    input: &str,
    highlight_bounds: &[usize],
    highlight_first: bool,
    search_bounds: &[usize],
    mut offset: usize,
    end: usize,
    max_width: usize,
) -> LineLayout {
    let mut cells = vec![];

    let mut highlight_bounds_index = highlight_bounds.partition_point(|&point| point <= offset) - 1;
    let mut highlight = highlight_first ^ (highlight_bounds_index % 2 == 1);

    let mut search_bounds_index = search_bounds.partition_point(|&point| point <= offset);
    let mut search_highlight = search_bounds_index % 2 == 1;

    let mut found_newline_length = 0;

    // The algorithm uses `unicode-segmentation` to split the line into EGCs, and `unicode-width`
    // to compute the width of each EGC. I don't know if it's correct (precisely matches what
    // terminal emulators do), but at least it matches the behavior of our library (tui-rs).
    for egc in input[offset..end].graphemes(true) {
        assert!(highlight_bounds_index + 1 < highlight_bounds.len());
        if offset == highlight_bounds[highlight_bounds_index + 1] {
            highlight_bounds_index += 1;
            highlight = !highlight;
        }

        if search_bounds.get(search_bounds_index).cloned() == Some(offset) {
            search_bounds_index += 1;
            search_highlight = !search_highlight;
        }

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
                highlight,
                search_highlight,
                fabricated_symbol,
            });
        }

        offset += egc.len();
    }

    LineLayout {
        cells,
        newline_highlight: highlight,
        offset_after_except_newline: offset,
        offset_after_with_newline: offset + found_newline_length,
    }
}
