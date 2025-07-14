use std::num::NonZeroU32;

use typst_macros::{elem, func, Cast};
use typst_utils::NonZeroExt;

use crate::foundations::{Content, NativeElement, Smart};
use crate::introspection::Locatable;
use crate::model::TableCell;

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

// Used to delimit content for tagged PDF.
#[elem(Locatable)]
pub struct PdfMarkerTag {
    #[required]
    pub kind: PdfMarkerTagKind,
    #[required]
    pub body: Content,
}

macro_rules! pdf_marker_tag {
    ($(#[doc = $doc:expr] $variant:ident,)+) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Cast)]
        pub enum PdfMarkerTagKind {
            $(
                #[doc = $doc]
                $variant
            ),+
        }

        impl PdfMarkerTag {
            $(
                #[doc = $doc]
                #[allow(non_snake_case)]
                pub fn $variant(body: Content) -> Content {
                    Self::new(PdfMarkerTagKind::$variant, body).pack()
                }
            )+
        }
    }
}

pdf_marker_tag! {
    /// `TOC`
    OutlineBody,
    /// `Figure`
    FigureBody,
    /// `Lbl` (marker) of the list item
    ListItemLabel,
    /// `LBody` of the enum item
    ListItemBody,
}
