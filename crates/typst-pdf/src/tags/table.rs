use std::num::{NonZeroU32, NonZeroUsize};

use krilla::tagging::{
    TableCellHeaders, TableCellSpan, TableDataCell, TableHeaderCell, TagBuilder, TagId,
    TagKind,
};
use typst_library::foundations::{Packed, Smart, StyleChain};
use typst_library::model::{TableCell, TableCellKind, TableElem, TableHeaderScope};

use crate::tags::{TableId, TagNode};

pub(crate) struct TableCtx {
    pub(crate) id: TableId,
    pub(crate) table: Packed<TableElem>,
    rows: Vec<Vec<GridCell>>,
}

impl TableCtx {
    pub(crate) fn new(id: TableId, table: Packed<TableElem>) -> Self {
        Self { id, table: table.clone(), rows: Vec::new() }
    }

    fn get(&self, x: usize, y: usize) -> Option<&TableCtxCell> {
        let cell = self.rows.get(y)?.get(x)?;
        self.resolve_cell(cell)
    }

    fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut TableCtxCell> {
        let cell = self.rows.get_mut(y)?.get_mut(x)?;
        match cell {
            GridCell::Cell(cell) => {
                // HACK: Workaround for the second mutable borrow when resolving
                // the spanned cell.
                Some(unsafe { std::mem::transmute(cell) })
            }
            &mut GridCell::Spanned(x, y) => self.rows[y][x].as_cell_mut(),
            GridCell::Missing => None,
        }
    }

    pub(crate) fn contains(&self, cell: &Packed<TableCell>) -> bool {
        let x = cell.x(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let y = cell.y(StyleChain::default()).unwrap_or_else(|| unreachable!());
        self.get(x, y).is_some()
    }

    fn resolve_cell<'a>(&'a self, cell: &'a GridCell) -> Option<&'a TableCtxCell> {
        match cell {
            GridCell::Cell(cell) => Some(cell),
            &GridCell::Spanned(x, y) => self.rows[y][x].as_cell(),
            GridCell::Missing => None,
        }
    }

    pub(crate) fn insert(&mut self, cell: Packed<TableCell>, nodes: Vec<TagNode>) {
        let x = cell.x(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let y = cell.y(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let rowspan = cell.rowspan(StyleChain::default());
        let colspan = cell.colspan(StyleChain::default());
        let kind = cell.kind(StyleChain::default());

        // Extend the table grid to fit this cell.
        let required_height = y + rowspan.get();
        let required_width = x + colspan.get();
        if self.rows.len() < required_height {
            self.rows
                .resize(required_height, vec![GridCell::Missing; required_width]);
        }
        let row = &mut self.rows[y];
        if row.len() < required_width {
            row.resize_with(required_width, || GridCell::Missing);
        }

        // Store references to the cell for all spanned cells.
        for i in y..y + rowspan.get() {
            for j in x..x + colspan.get() {
                self.rows[i][j] = GridCell::Spanned(x, y);
            }
        }

        self.rows[y][x] = GridCell::Cell(TableCtxCell {
            x: x as u32,
            y: y as u32,
            rowspan,
            colspan,
            kind,
            headers: TableCellHeaders::NONE,
            nodes,
        });
    }

    pub(crate) fn build_table(mut self, mut nodes: Vec<TagNode>) -> Vec<TagNode> {
        // Table layouting ensures that there are no overlapping cells, and that
        // any gaps left by the user are filled with empty cells.
        if self.rows.is_empty() {
            return nodes;
        }
        let height = self.rows.len();
        let width = self.rows[0].len();

        // Only generate row groups such as `THead`, `TFoot`, and `TBody` if
        // there are no rows with mixed cell kinds.
        let mut gen_row_groups = true;
        let row_kinds = (self.rows.iter())
            .map(|row| {
                row.iter()
                    .filter_map(|cell| self.resolve_cell(cell))
                    .map(|cell| cell.kind)
                    .fold(Smart::Auto, |a, b| {
                        if let Smart::Custom(TableCellKind::Header(_, scope)) = b {
                            gen_row_groups &= scope == TableHeaderScope::Column;
                        }
                        if let (Smart::Custom(a), Smart::Custom(b)) = (a, b) {
                            gen_row_groups &= a == b;
                        }
                        a.or(b)
                    })
                    .unwrap_or(TableCellKind::Data)
            })
            .collect::<Vec<_>>();

        // Fixup all missing cell kinds.
        for (row, row_kind) in self.rows.iter_mut().zip(row_kinds.iter().copied()) {
            let default_kind =
                if gen_row_groups { row_kind } else { TableCellKind::Data };
            for cell in row.iter_mut() {
                let Some(cell) = cell.as_cell_mut() else { continue };
                cell.kind = cell.kind.or(Smart::Custom(default_kind));
            }
        }

        // Explicitly set the headers attribute for cells.
        for x in 0..width {
            let mut column_header = None;
            for y in 0..height {
                self.resolve_cell_headers(
                    (x, y),
                    &mut column_header,
                    TableHeaderScope::refers_to_column,
                );
            }
        }
        for y in 0..height {
            let mut row_header = None;
            for x in 0..width {
                self.resolve_cell_headers(
                    (x, y),
                    &mut row_header,
                    TableHeaderScope::refers_to_row,
                );
            }
        }

        let mut chunk_kind = row_kinds[0];
        let mut row_chunk = Vec::new();
        for (row, row_kind) in self.rows.into_iter().zip(row_kinds) {
            let row_nodes = row
                .into_iter()
                .filter_map(|cell| {
                    let cell = cell.into_cell()?;
                    let span = TableCellSpan {
                        rows: cell.rowspan.try_into().unwrap_or(NonZeroU32::MAX),
                        cols: cell.colspan.try_into().unwrap_or(NonZeroU32::MAX),
                    };
                    let tag = match cell.unwrap_kind() {
                        TableCellKind::Header(_, scope) => {
                            let id = table_cell_id(self.id, cell.x, cell.y);
                            let scope = table_header_scope(scope);
                            TagKind::TH(
                                TableHeaderCell::new(scope)
                                    .with_span(span)
                                    .with_headers(cell.headers),
                            )
                            .with_id(Some(id))
                        }
                        TableCellKind::Footer | TableCellKind::Data => TagKind::TD(
                            TableDataCell::new()
                                .with_span(span)
                                .with_headers(cell.headers),
                        )
                        .into(),
                    };

                    Some(TagNode::Group(tag, cell.nodes))
                })
                .collect();

            let row = TagNode::Group(TagKind::TR.into(), row_nodes);

            // Push the `TR` tags directly.
            if !gen_row_groups {
                nodes.push(row);
                continue;
            }

            // Generate row groups.
            if !should_group_rows(chunk_kind, row_kind) {
                let tag = match chunk_kind {
                    TableCellKind::Header(..) => TagKind::THead,
                    TableCellKind::Footer => TagKind::TFoot,
                    TableCellKind::Data => TagKind::TBody,
                };
                nodes.push(TagNode::Group(tag.into(), std::mem::take(&mut row_chunk)));

                chunk_kind = row_kind;
            }
            row_chunk.push(row);
        }

        if !row_chunk.is_empty() {
            let tag = match chunk_kind {
                TableCellKind::Header(..) => TagKind::THead,
                TableCellKind::Footer => TagKind::TFoot,
                TableCellKind::Data => TagKind::TBody,
            };
            nodes.push(TagNode::Group(tag.into(), row_chunk));
        }

        nodes
    }

    fn resolve_cell_headers<F>(
        &mut self,
        (x, y): (usize, usize),
        current_header: &mut Option<(NonZeroU32, TagId)>,
        refers_to_dir: F,
    ) where
        F: Fn(&TableHeaderScope) -> bool,
    {
        let table_id = self.id;
        let Some(cell) = self.get_mut(x, y) else { return };

        if let Some((prev_level, cell_id)) = current_header.clone() {
            // The `Headers` attribute is also set for parent headers.
            let mut is_parent_header = true;
            if let TableCellKind::Header(level, scope) = cell.unwrap_kind() {
                if refers_to_dir(&scope) {
                    is_parent_header = prev_level < level;
                }
            }

            if is_parent_header && !cell.headers.ids.contains(&cell_id) {
                cell.headers.ids.push(cell_id.clone());
            }
        }

        if let TableCellKind::Header(level, scope) = cell.unwrap_kind() {
            if refers_to_dir(&scope) {
                let tag_id = table_cell_id(table_id, x as u32, y as u32);
                *current_header = Some((level, tag_id));
            }
        }
    }
}

#[derive(Clone, Default)]
enum GridCell {
    Cell(TableCtxCell),
    Spanned(usize, usize),
    #[default]
    Missing,
}

impl GridCell {
    fn as_cell(&self) -> Option<&TableCtxCell> {
        if let Self::Cell(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_cell_mut(&mut self) -> Option<&mut TableCtxCell> {
        if let Self::Cell(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn into_cell(self) -> Option<TableCtxCell> {
        if let Self::Cell(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct TableCtxCell {
    x: u32,
    y: u32,
    rowspan: NonZeroUsize,
    colspan: NonZeroUsize,
    kind: Smart<TableCellKind>,
    headers: TableCellHeaders,
    nodes: Vec<TagNode>,
}

impl TableCtxCell {
    fn unwrap_kind(&self) -> TableCellKind {
        self.kind.unwrap_or_else(|| unreachable!())
    }
}

fn should_group_rows(a: TableCellKind, b: TableCellKind) -> bool {
    match (a, b) {
        (TableCellKind::Header(..), TableCellKind::Header(..)) => true,
        (TableCellKind::Footer, TableCellKind::Footer) => true,
        (TableCellKind::Data, TableCellKind::Data) => true,
        (_, _) => false,
    }
}

fn table_cell_id(table_id: TableId, x: u32, y: u32) -> TagId {
    let mut bytes = [0; 12];
    bytes[0..4].copy_from_slice(&table_id.0.to_ne_bytes());
    bytes[4..8].copy_from_slice(&x.to_ne_bytes());
    bytes[8..12].copy_from_slice(&y.to_ne_bytes());
    TagId::from_bytes(&bytes)
}

fn table_header_scope(scope: TableHeaderScope) -> krilla::tagging::TableHeaderScope {
    match scope {
        TableHeaderScope::Both => krilla::tagging::TableHeaderScope::Both,
        TableHeaderScope::Column => krilla::tagging::TableHeaderScope::Column,
        TableHeaderScope::Row => krilla::tagging::TableHeaderScope::Row,
    }
}
