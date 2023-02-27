use super::line_layout::{layout_line, LineCell};
use crossterm::event::{Event, KeyCode, KeyModifiers};

pub struct TextInput {
    old_width: usize,
    content: String,
    line_layout: Vec<LineCell>,
    offsets: Vec<usize>,
    scroll_x: usize,
    cursor_offset: usize,
    cursor_x: usize,
    pub style: tui::style::Style,
}

pub enum TextInputEventResult {
    Handled,
    Unhandled,
}

impl TextInput {
    pub fn new() -> TextInput {
        TextInput {
            old_width: 1,
            content: String::new(),
            line_layout: vec![],
            offsets: vec![0],
            scroll_x: 0,
            cursor_offset: 0,
            cursor_x: 0,
            style: Default::default(),
        }
    }

    // TextInput does not implement tui::widgets::Widget because its render() takes `self`. Ugh.
    // Plus we want a `cursor` output.
    pub fn render(&mut self, area: tui::layout::Rect, buf: &mut tui::buffer::Buffer, cursor: &mut Option<(u16, u16)>) {
        assert_eq!(area.height, 1);
        assert_ne!(area.width, 0);
        let width = usize::try_from(area.width).unwrap();

        if width != self.old_width {
            self.old_width = width;
            self.fix_scroll_invariants();
        }

        for x in 0..width {
            let line_cell = self.line_layout.get(x + self.scroll_x);
            let mut egc = line_cell.map_or(" ", |line_cell| &line_cell.egc);
            if x == width - 1 {
                if let Some(next_cell) = self.line_layout.get(x + self.scroll_x + 1) {
                    if next_cell.egc.is_empty() {
                        egc = " ";
                    }
                }
            }
            let mut cell = tui::buffer::Cell::default();
            if !egc.is_empty() {
                cell.symbol = egc.to_owned();
                cell.set_style(self.style);
                // Else keep default Cell.symbol, which is " ".
            }
            *buf.get_mut(area.x + u16::try_from(x).unwrap(), area.y) = cell;
        }

        let cursor_x_rel = self.cursor_x - self.scroll_x;
        *cursor = Some((area.x + u16::try_from(cursor_x_rel).unwrap(), area.y));
    }

    pub fn handle_event(&mut self, event: &Event, validator: impl Fn(&str) -> bool) -> TextInputEventResult {
        match event {
            Event::Key(e) => match e.code {
                KeyCode::Char(c) if e.modifiers.difference(KeyModifiers::SHIFT).is_empty() => {
                    self.write(&c.to_string(), validator);
                    TextInputEventResult::Handled
                }
                KeyCode::Tab => {
                    self.write("\t", validator);
                    TextInputEventResult::Handled
                }
                KeyCode::Insert => {
                    // TODO: temporary test.
                    self.write("\x1f", validator);
                    TextInputEventResult::Handled
                }
                KeyCode::Backspace => {
                    let mut x = self.cursor_x.saturating_sub(1);
                    while !self.is_valid_x(x) {
                        x -= 1;
                    }
                    self.erase(self.offsets[x], self.cursor_offset, validator);
                    TextInputEventResult::Handled
                }
                KeyCode::Delete => {
                    let mut x = std::cmp::min(self.cursor_x + 1, self.line_layout.len());
                    while !self.is_valid_x(x) {
                        x += 1;
                    }
                    self.erase(self.cursor_offset, self.offsets[x], validator);
                    TextInputEventResult::Handled
                }
                KeyCode::Left => {
                    let mut x = self.cursor_x.saturating_sub(1);
                    while !self.is_valid_x(x) {
                        x -= 1;
                    }
                    self.set_cursor_x(x);
                    TextInputEventResult::Handled
                }
                KeyCode::Right => {
                    let mut x = std::cmp::min(self.cursor_x + 1, self.line_layout.len());
                    while !self.is_valid_x(x) {
                        x += 1;
                    }
                    self.set_cursor_x(x);
                    TextInputEventResult::Handled
                }
                KeyCode::Home => {
                    self.set_cursor_offset(0);
                    TextInputEventResult::Handled
                }
                KeyCode::End => {
                    self.set_cursor_offset(self.content.len());
                    TextInputEventResult::Handled
                }
                // TODO: handle Up/Down/Enter/Esc/Ctrl-C in the caller(s).
                _ => TextInputEventResult::Unhandled,
            },
            Event::Paste(string) => {
                // TODO: strip final newlines
                // TODO: enable bracketed paste
                self.write(string, validator);
                TextInputEventResult::Handled
            }
            _ => TextInputEventResult::Unhandled,
        }
    }

    pub fn get_content(&self) -> &str {
        &self.content
    }

    fn write(&mut self, string: &str, validator: impl Fn(&str) -> bool) {
        let new_content = format!(
            "{}{}{}",
            &self.content[..self.cursor_offset],
            string,
            &self.content[self.cursor_offset..]
        );
        if !validator(&new_content) {
            return;
        }
        self.set_content(new_content, self.cursor_offset + string.len());
    }

    fn erase(&mut self, start: usize, end: usize, validator: impl Fn(&str) -> bool) {
        let new_content = format!("{}{}", &self.content[..start], &self.content[end..]);
        if !validator(&new_content) {
            return;
        }
        self.set_content(new_content, start);
    }

    pub fn set_content(&mut self, content: String, cursor_offset: usize) {
        self.content = content;
        self.line_layout = layout_line(&self.content, &[], &[], &[], 0, self.content.len(), usize::MAX).cells;
        self.offsets = self.line_layout.iter().map(|c| c.offset).collect();
        self.offsets.push(self.content.len());
        self.set_cursor_offset(cursor_offset);
    }

    pub fn set_cursor_x(&mut self, x: usize) {
        self.set_cursor_offset(self.offsets[x]);
    }

    pub fn set_cursor_offset(&mut self, offset: usize) {
        assert!(self.content.is_char_boundary(offset));
        self.cursor_offset = offset;
        // This ensures that is_valid_x(cursor_x) is always true.
        self.cursor_x = self.offsets.partition_point(|&p| p < offset);
        self.fix_scroll_invariants();
    }

    fn fix_scroll_invariants(&mut self) {
        if self.cursor_x < self.scroll_x {
            self.scroll_x = self.cursor_x;
        }

        if self.scroll_x + self.old_width < self.cursor_x + 1 {
            self.scroll_x = self.cursor_x + 1 - self.old_width;
            while !self.is_valid_x(self.scroll_x) {
                self.scroll_x += 1;
            }
        }
    }

    fn is_valid_x(&self, x: usize) -> bool {
        x == 0 || self.offsets[x] != self.offsets[x - 1]
    }
}
