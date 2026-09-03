use crate::ResolvedPicoStr;

pub struct Writer<T> {
    /// The current writing mode.
    mode: Mode,
    /// Whether to pretty-print output.
    pretty: bool,
    /// The buffer to which encoded data is written.
    buf: T,
    /// The state of the element that is currently being written.
    state: State,
    /// The nesting depth of elements.
    depth: usize,
}

impl<T: Buffer> Writer<T> {
    /// Create a new writer with a specific mode.
    pub fn new(mode: Mode) -> Self {
        let mut buf = T::default();
        Self {
            mode,
            pretty: true,
            buf: T::default(),
            state: State::Elem,
            depth: 0,
        }
    }

    /// Create a new HTML writer.
    pub fn html() -> Self {
        Self::new(Mode::Html)
    }

    /// Create a new XML writer.
    pub fn xml() -> Self {
        Self::new(Mode::Xml)
    }

    // TODO: Provide an API for switching modes.
    // For example for integrating SVG, MathML, or other foreign XML based
    // formats into HTML.

    // TODO: HTML doctype and XML declaration.

    /// Finish writing and return the buffer.
    pub fn finish(self) -> T {
        assert_eq!(self.state, State::Elem);
        assert_eq!(self.depth, 0);
        self.buf
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Mode {
    Html,
    Xml,
}

pub trait Buffer: Sized + Default {
    /// Write a string. When writing to an attribute value, XML metacharacters
    /// are escaped.
    fn push_str(&mut self, value: &str);

    /// Write a character. When writing to an attribute value, XML
    /// metacharacters are escaped.
    fn push_char(&mut self, value: char) {
        self.push_str(value.encode_utf8(&mut [0; 4]));
    }
}

impl Buffer for String {
    fn push_str(&mut self, value: &str) {
        self.push_str(value);
    }
}

impl Buffer for Vec<u8> {
    fn push_str(&mut self, value: &str) {
        self.extend_from_slice(value.as_bytes());
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum State {
    /// Immediately behind the start tag or a previous attribute with no
    /// trailing white-space.
    Attr,
    /// The start tag of the element has been closed and children have possibly
    /// been written to it.
    Elem,
}

impl<T: Buffer> Writer<T> {
    pub fn start_elem(&mut self, name: &str) {
        if self.state == State::Attr {
            self.buf.push_str(">");
            self.state = State::Elem;
        }

        self.indent();

        self.buf.push_str("<");
        self.buf.push_str(name);

        self.depth += 1;
        self.state = State::Attr;
    }

    pub fn end_elem(&mut self, name: &str) {
        assert_ne!(self.depth, 0);

        match self.mode {
            Mode::Html => {
                if self.state == State::Attr {
                    self.buf.push_str(">");
                }

                self.buf.push_str("</");
                self.buf.push_str(name);
                self.buf.push_str(">");
            }
            Mode::Xml => {
                // Use self-closing tags in XML mode.
                if self.state == State::Attr {
                    self.buf.push_str("/>");
                } else {
                    self.buf.push_str("</");
                    self.buf.push_str(name);
                    self.buf.push_str(">");
                }
            }
        }

        self.depth -= 1;
        self.state = State::Elem;

        self.indent();
    }

    pub fn end_void_elem(&mut self) {
        assert_eq!(self.state, State::Attr);

        match self.mode {
            Mode::Html => self.buf.push_str(">"),
            Mode::Xml => self.buf.push_str("/>"),
        }

        self.depth -= 1;
        self.state = State::Elem;
    }

    fn indent(&mut self) {
        if self.pretty {
            self.buf.push_str("\n");
            for _ in 0..self.depth {
                self.buf.push_str("  ");
            }
        }
    }
}

/// Append `value` to a buffer, escaping the XML metacharacters that
/// `xmlwriter`'s raw attribute writer leaves untouched (it only escapes the
/// quotation mark). Otherwise a value such as a link URL containing `&` or `<`
/// yields malformed SVG. Runs of non-escaped characters are appended in one go.
fn write_escaped(
    buf: &mut impl Buffer,
    mut value: &str,
    escape: impl Fn(u8) -> Option<&'static str>,
) {
    for (i, b) in value.bytes().enumerate() {
        if let Some(escaped) = escape(b) {
            buf.push_str(&value[..i]);
            buf.push_str(escaped);
        }

        // Escaped characters are always one byte.
        value = &value[i + 1..];
    }
    if !value.is_empty() {
        buf.push_str(value);
    }
}

fn escape_in_attr(byte: u8) -> Option<&'static str> {
    match byte {
        b'"' => Some("&quot"),
        b'&' => Some("&amp;"),
        b'<' => Some("&lt;"),
        _ => None,
    }
}

fn escape_in_text(byte: u8) -> Option<&'static str> {
    match byte {
        b'&' => Some("&amp;"),
        b'<' => Some("&lt;"),
        b'>' => Some("&gt;"),
        _ => None,
    }
}

mod html {
    //! Defines syntactical properties of HTML tags, attributes, and text.

    /// Check whether a character is in a tag name.
    pub const fn is_valid_in_tag_name(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '-'
    }

    /// Check whether a character is valid in an attribute name.
    pub const fn is_valid_in_attribute_name(c: char) -> bool {
        match c {
            // These are forbidden.
            '\0' | ' ' | '"' | '\'' | '>' | '/' | '=' => false,
            c if is_whatwg_control_char(c) => false,
            c if is_whatwg_non_char(c) => false,
            // _Everything_ else is allowed, including U+2029 paragraph
            // separator. Go wild.
            _ => true,
        }
    }

    /// Check whether a character can be an used in an attribute value without
    /// escaping.
    ///
    /// See <https://html.spec.whatwg.org/multipage/syntax.html#attributes-2>
    pub const fn is_valid_in_attribute_value(c: char) -> bool {
        match c {
            // Ampersands are sometimes legal (i.e. when they are not _ambiguous
            // ampersands_) but it is not worth the trouble to check for that.
            '&' => false,
            // Quotation marks are not allowed in double-quote-delimited attribute
            // values.
            '"' => false,
            // All other text characters are allowed.
            c => is_w3c_text_char(c),
        }
    }

    /// Check whether a character can be an used in normal text without
    /// escaping.
    pub const fn is_valid_in_normal_element_text(c: char) -> bool {
        match c {
            // Ampersands are sometimes legal (i.e. when they are not _ambiguous
            // ampersands_) but it is not worth the trouble to check for that.
            '&' => false,
            // Less-than signs are not allowed in text.
            '<' => false,
            // All other text characters are allowed.
            c => is_w3c_text_char(c),
        }
    }

    /// Check if something is valid text in HTML.
    pub const fn is_w3c_text_char(c: char) -> bool {
        match c {
            // Non-characters are obviously not text characters.
            c if is_whatwg_non_char(c) => false,
            // Control characters are disallowed, except for whitespace.
            c if is_whatwg_control_char(c) => c.is_ascii_whitespace(),
            // Everything else is allowed.
            _ => true,
        }
    }

    const fn is_whatwg_non_char(c: char) -> bool {
        match c {
            '\u{fdd0}'..='\u{fdef}' => true,
            // Non-characters matching xxFFFE or xxFFFF up to x10FFFF (inclusive).
            c if c as u32 & 0xfffe == 0xfffe && c as u32 <= 0x10ffff => true,
            _ => false,
        }
    }

    const fn is_whatwg_control_char(c: char) -> bool {
        match c {
            // C0 control characters.
            '\u{00}'..='\u{1f}' => true,
            // Other control characters.
            '\u{7f}'..='\u{9f}' => true,
            _ => false,
        }
    }
}
