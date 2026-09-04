//! This modules provides the primitives for building fool-proof mixed HTML,
//! XHTML, and XML writers.

/// A HTML and XML writer.
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
    pub fn new(mode: Mode, buf: T) -> Self {
        Self {
            mode,
            pretty: true,
            buf,
            state: State::Elem,
            depth: 0,
        }
    }

    /// Create a new HTML writer.
    pub fn html() -> Self
    where
        T: Default,
    {
        Self::new(Mode::Html, T::default())
    }

    /// Create a new XML writer.
    pub fn xml() -> Self
    where
        T: Default,
    {
        Self::new(Mode::Xml, T::default())
    }

    /// Set the pretty flag in builder style.
    pub fn with_pretty(mut self, pretty: bool) -> Self {
        self.pretty = pretty;
        self
    }

    /// Set the pretty flag.
    pub fn set_pretty(&mut self, pretty: bool) {
        self.pretty = pretty;
    }

    /// Set the pretty flag in builder style.
    pub fn with_mode(mut self, mode: Mode) -> Self {
        self.mode = mode;
        self
    }

    /// Set the mode flag.
    pub fn set_mode(&mut self, mode: Mode) {
        self.mode = mode;
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

/// The writing mode.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Mode {
    Html,
    Xml,
}

pub trait Buffer: Sized {
    /// Write a string. When writing to an attribute value, XML metacharacters
    /// are escaped.
    fn push_str(&mut self, value: &str);

    /// Write a character. When writing to an attribute value, XML
    /// metacharacters are escaped.
    fn push_char(&mut self, value: char) {
        self.push_str(value.encode_utf8(&mut [0; 4]));
    }
}

impl<T: Buffer> Buffer for &mut T {
    fn push_str(&mut self, value: &str) {
        T::push_str(self, value);
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

        self.indent();
    }

    // TODO: Add trait for
    pub fn attr(&mut self, name: &str, value: impl BufferDisplay) {
        assert_eq!(self.state, State::Attr);

        self.buf.push_str(" ");
        self.buf.push_str(name);
        self.buf.push_str("=\"");
        match self.mode {
            Mode::Html => {
                value.fmt(Escaped(buf, html::is_valid_in_attr_name));
            }
            Mode::Xml => {
                value.fmt(Escaped(buf, xml::is_valid_in_attr_name));
            }
        }
        self.buf.push_str("\"");
    }

    pub fn empty_attr(&mut self, name: &str) {}

    fn indent(&mut self) {
        if self.pretty {
            self.buf.push_str("\n");
            for _ in 0..self.depth {
                self.buf.push_str("  ");
            }
        }
    }
}

trait BufferDisplay {
    fn fmt(&self, buf: &mut impl Buffer);
}

struct Escaped<B, E>(B, E);

impl<B, E> Buffer for Escaped<B, E>
where
    B: Buffer,
    E: Fn(char) -> bool + Copy,
{
    fn push_str(&mut self, value: &str) {
        write_escaped(&self.0, value, self.1)?;
    }
}

/// Append `value` to a buffer, escaping the XML metacharacters that
/// `xmlwriter`'s raw attribute writer leaves untouched (it only escapes the
/// quotation mark). Otherwise a value such as a link URL containing `&` or `<`
/// yields malformed SVG. Runs of non-escaped characters are appended in one go.
fn write_escaped(
    buf: &mut impl Buffer,
    mut value: &str,
    valid: impl Fn(char) -> bool,
) -> Result<(), Error> {
    while let Some((i, c)) = value.char_indices().find(|(_, c)| !valid(*c)) {
        buf.push_str(&value[..i]);
        escape(buf, c)?;

        value = &value[i + c.len_utf8()..];
    }

    if !value.is_empty() {
        buf.push_str(value);
    }

    Ok(())
}

/// Escape a character.
fn escape(buf: &mut impl Buffer, c: char) -> Result<(), Error> {
    // See <https://html.spec.whatwg.org/multipage/syntax.html#syntax-charref>
    match c {
        // Write common named character references.
        '&' => buf.push_str("&amp;"),
        '<' => buf.push_str("&lt;"),
        '>' => buf.push_str("&gt;"),
        '"' => buf.push_str("&quot;"),
        '\'' => buf.push_str("&apos;"),

        // Write a hexadeciaml numeric character reference.
        _ if html::is_w3c_text_char(c) && c != '\r' => {
            fn hex_digit(nibble: u8) -> u8 {
                match nibble {
                    0..10 => b'0' + nibble,
                    _ => b'a' + (nibble - 10),
                }
            }

            let digits = u32::to_be_bytes(c.into())
                .map(|byte| [hex_digit(byte >> 4), hex_digit(byte & 0x0F)]);
            // The digits are all valid ASCII hex characters.
            let str = std::str::from_utf8(digits.as_flattened()).unwrap();

            buf.push_str("&#x");
            buf.push_str(str.trim_start_matches('0'));
            buf.push_str(";");
        }
        _ => return Err(Error::Unencodable(c)),
    }

    Ok(())
}

pub enum Error {
    InvalidInTagName(char),
    InvalidInAttrName(char),
    Unencodable(char),
}

pub mod html {
    //! Defines syntactical properties of HTML tags, attributes, and text.

    /// Check whether a character is valid in a tag name.
    pub const fn is_valid_in_tag_name(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '-'
    }

    /// Check whether a character is valid in an attribute name.
    pub const fn is_valid_in_attr_name(c: char) -> bool {
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
    pub const fn is_valid_in_attr_value(c: char) -> bool {
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
    pub const fn is_valid_in_normal_elem_text(c: char) -> bool {
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

pub mod xml {
    //! Defines syntactical properties of XML tags, attributes, text, and CDATA.

    /// Check whether a character is valid in a tag name.
    pub const fn is_valid_in_tag_name(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '-'
    }

    /// Check whether a character is valid in an attribute name.
    pub const fn is_valid_in_attr_name(c: char) -> bool {
        todo!()
    }

    /// Check whether a character can be an used in an attribute value without
    /// escaping.
    ///
    /// See <https://www.w3.org/TR/REC-xml/#NT-AttValue>
    pub const fn is_valid_in_attr_value(c: char) -> bool {
        match c {
            // Ampersands are not allowed in attribute values.
            '&' => false,
            // Less-than signs are not allowed in attribute values.
            '<' => false,
            // Quotation marks are not allowed in double-quote-delimited
            // attribute values.
            '"' => false,
            // Everything else is allowed.
            _ => true,
        }
    }

    /// Check whether a character can be an used in normal text without
    /// escaping.
    pub const fn is_valid_in_normal_elem_text(c: char) -> bool {
        todo!()
    }

    /// Check whether a character can be an used in cdata without escaping.
    pub const fn is_valid_in_cdata(c: char) -> bool {
        todo!()
    }
}
