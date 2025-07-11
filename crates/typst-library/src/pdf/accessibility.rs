use std::num::NonZeroU32;

use ecow::EcoString;
use typst_macros::{cast, elem, func, Cast};
use typst_utils::NonZeroExt;

use crate::foundations::{Content, NativeElement, Smart};
use crate::introspection::Locatable;
use crate::model::TableCell;

// TODO: docs
#[elem(Locatable)]
pub struct PdfTagElem {
    #[default(PdfTagKind::NonStruct)]
    pub kind: PdfTagKind,

    /// An alternate description.
    pub alt: Option<EcoString>,
    /// Exact replacement for this structure element and its children.
    pub actual_text: Option<EcoString>,
    /// The expanded form of an abbreviation/acronym.
    pub expansion: Option<EcoString>,

    /// The content to underline.
    #[required]
    pub body: Content,
}

// TODO: docs
/// PDF structure elements
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PdfTagKind {
    // grouping elements
    /// (Part)
    Part,
    /// (Article)
    Art,
    /// (Section)
    Sect,
    /// (Division)
    Div,
    /// (Block quotation)
    BlockQuote,
    /// (Caption)
    Caption,
    /// (Table of contents)
    TOC,
    /// (Table of contents item)
    TOCI,
    /// (Index)
    Index,
    /// (Nonstructural element)
    NonStruct,
    /// (Private element)
    Private,

    // paragraph like elements
    /// (Heading)
    H { title: Option<EcoString> },
    /// (Heading level 1)
    H1 { title: Option<EcoString> },
    /// (Heading level 2)
    H2 { title: Option<EcoString> },
    /// (Heading level 3)
    H4 { title: Option<EcoString> },
    /// (Heading level 4)
    H3 { title: Option<EcoString> },
    /// (Heading level 5)
    H5 { title: Option<EcoString> },
    /// (Heading level 6)
    H6 { title: Option<EcoString> },
    /// (Paragraph)
    P,

    // list elements
    /// (List)
    L { numbering: ListNumbering },
    /// (List item)
    LI,
    /// (Label)
    Lbl,
    /// (List body)
    LBody,

    // table elements
    /// (Table)
    Table,
    /// (Table row)
    TR,
    /// (Table header)
    TH { scope: TableHeaderScope },
    /// (Table data cell)
    TD,
    /// (Table header row group)
    THead,
    /// (Table body row group)
    TBody,
    /// (Table footer row group)
    TFoot,

    // inline elements
    /// (Span)
    Span,
    /// (Quotation)
    Quote,
    /// (Note)
    Note,
    /// (Reference)
    Reference,
    /// (Bibliography Entry)
    BibEntry,
    /// (Code)
    Code,
    /// (Link)
    Link,
    /// (Annotation)
    Annot,

    /// (Ruby)
    Ruby,
    /// (Ruby base text)
    RB,
    /// (Ruby annotation text)
    RT,
    /// (Ruby punctuation)
    RP,

    /// (Warichu)
    Warichu,
    /// (Warichu text)
    WT,
    /// (Warichu punctuation)
    WP,

    /// (Figure)
    Figure,
    /// (Formula)
    Formula,
    /// (Form)
    Form,
}

cast! {
    PdfTagKind,
    self => match self {
        PdfTagKind::Part => "part".into_value(),
        _ => todo!(),
    },
    "part" => Self::Part,
    // TODO
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ListNumbering {
    /// No numbering.
    None,
    /// Solid circular bullets.
    Disc,
    /// Open circular bullets.
    Circle,
    /// Solid square bullets.
    Square,
    /// Decimal numbers.
    Decimal,
    /// Lowercase Roman numerals.
    LowerRoman,
    /// Uppercase Roman numerals.
    UpperRoman,
    /// Lowercase letters.
    LowerAlpha,
    /// Uppercase letters.
    UpperAlpha,
}

/// Mark content as a PDF artifact.
/// TODO: maybe generalize this and use it to mark html elements with `aria-hidden="true"`?
#[elem(Locatable)]
pub struct ArtifactElem {
    /// The artifact kind.
    #[default(ArtifactKind::Other)]
    pub kind: ArtifactKind,

    /// The content that is an artifact.
    #[required]
    pub body: Content,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Cast)]
pub enum ArtifactKind {
    /// Page header artifacts.
    Header,
    /// Page footer artifacts.
    Footer,
    /// Other page artifacts.
    Page,
    /// Other artifacts.
    #[default]
    Other,
}

// TODO: feature gate
/// Explicity define this cell as a header cell.
#[func]
pub fn header_cell(
    #[named]
    #[default(NonZeroU32::ONE)]
    level: NonZeroU32,
    #[named]
    #[default]
    scope: TableHeaderScope,
    /// The table cell.
    cell: TableCell,
) -> Content {
    cell.with_kind(Smart::Custom(TableCellKind::Header(level, scope)))
        .pack()
}

// TODO: feature gate
/// Explicity define this cell as a data cell.
#[func]
pub fn data_cell(
    /// The table cell.
    cell: TableCell,
) -> Content {
    cell.with_kind(Smart::Custom(TableCellKind::Data)).pack()
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TableCellKind {
    Header(NonZeroU32, TableHeaderScope),
    Footer,
    #[default]
    Data,
}

/// The scope of a table header cell.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Cast)]
pub enum TableHeaderScope {
    /// The header cell refers to both the row and the column.
    Both,
    /// The header cell refers to the column.
    #[default]
    Column,
    /// The header cell refers to the row.
    Row,
}

impl TableHeaderScope {
    pub fn refers_to_column(&self) -> bool {
        match self {
            TableHeaderScope::Both => true,
            TableHeaderScope::Column => true,
            TableHeaderScope::Row => false,
        }
    }

    pub fn refers_to_row(&self) -> bool {
        match self {
            TableHeaderScope::Both => true,
            TableHeaderScope::Column => false,
            TableHeaderScope::Row => true,
        }
    }
}
