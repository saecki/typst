use std::io::Write as _;
use std::num::NonZeroU32;

use az::SaturatingAs;
use krilla::tagging::{
    TableCellSpan, TableDataCell, TableHeaderCell, TagBuilder, TagId, TagIdRefs, TagKind,
};
use smallvec::SmallVec;
use typst_library::foundations::{Packed, Smart, StyleChain};
use typst_library::model::TableCell;
use typst_library::pdf::{TableCellKind, TableHeaderScope};

use crate::tags::{TableId, TagNode};

pub(crate) struct TableCtx {
    pub(crate) id: TableId,
    pub(crate) summary: Option<String>,
    rows: Vec<Vec<GridCell>>,
}

impl TableCtx {
    pub(crate) fn new(id: TableId, summary: Option<String>) -> Self {
        Self { id, summary, rows: Vec::new() }
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
        let x = cell.x.get(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let y = cell.y.get(StyleChain::default()).unwrap_or_else(|| unreachable!());
        self.get(x, y).is_some()
    }

    fn resolve_cell<'a>(&'a self, cell: &'a GridCell) -> Option<&'a TableCtxCell> {
        match cell {
            GridCell::Cell(cell) => Some(cell),
            &GridCell::Spanned(x, y) => self.rows[y][x].as_cell(),
            GridCell::Missing => None,
        }
    }

    pub(crate) fn insert(&mut self, cell: &TableCell, nodes: Vec<TagNode>) {
        let x = cell.x.get(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let y = cell.y.get(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let rowspan = cell.rowspan.get(StyleChain::default());
        let colspan = cell.colspan.get(StyleChain::default());
        let kind = cell.kind.get(StyleChain::default());

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
            x: x.saturating_as(),
            y: y.saturating_as(),
            rowspan: rowspan.try_into().unwrap_or(NonZeroU32::MAX),
            colspan: colspan.try_into().unwrap_or(NonZeroU32::MAX),
            kind,
            headers: TagIdRefs::NONE,
            nodes,
        });
    }

    pub(crate) fn build_table(mut self, mut nodes: Vec<TagNode>) -> TagNode {
        // Table layouting ensures that there are no overlapping cells, and that
        // any gaps left by the user are filled with empty cells.
        if self.rows.is_empty() {
            return TagNode::Group(TagKind::Table(self.summary).into(), nodes);
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
            let mut column_header = Vec::new();
            for y in 0..height {
                self.resolve_cell_headers(
                    (x, y),
                    &mut column_header,
                    TableHeaderScope::refers_to_column,
                );
            }
        }
        for y in 0..height {
            let mut row_header = Vec::new();
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
                    let span = TableCellSpan { rows: cell.rowspan, cols: cell.colspan };
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

                    // Wrap content in a paragraph.
                    // TODO: maybe avoid nested paragraphs?
                    let par = TagNode::Group(TagKind::P.into(), cell.nodes);
                    Some(TagNode::Group(tag, vec![par]))
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

        TagNode::Group(TagKind::Table(self.summary).into(), nodes)
    }

    fn resolve_cell_headers<F>(
        &mut self,
        (x, y): (usize, usize),
        current_header: &mut Vec<(NonZeroU32, TagId)>,
        refers_to_dir: F,
    ) where
        F: Fn(&TableHeaderScope) -> bool,
    {
        let table_id = self.id;
        let Some(cell) = self.get_mut(x, y) else { return };

        let mut new_header = None;
        if let TableCellKind::Header(level, scope) = cell.unwrap_kind() {
            if refers_to_dir(&scope) {
                // Remove all headers that are the same or a lower level.
                while current_header.pop_if(|(l, _)| *l >= level).is_some() {}

                let tag_id = table_cell_id(table_id, cell.x, cell.y);
                new_header = Some((level, tag_id));
            }
        }

        if let Some((_, cell_id)) = current_header.last() {
            if !cell.headers.ids.contains(&cell_id) {
                cell.headers.ids.push(cell_id.clone());
            }
        }

        current_header.extend(new_header);
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
    rowspan: NonZeroU32,
    colspan: NonZeroU32,
    kind: Smart<TableCellKind>,
    headers: TagIdRefs,
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
    let mut buf = SmallVec::new();
    _ = write!(&mut buf, "{}x{x}y{y}", table_id.0);
    TagId::from_smallvec(buf)
}

fn table_header_scope(scope: TableHeaderScope) -> krilla::tagging::TableHeaderScope {
    match scope {
        TableHeaderScope::Both => krilla::tagging::TableHeaderScope::Both,
        TableHeaderScope::Column => krilla::tagging::TableHeaderScope::Column,
        TableHeaderScope::Row => krilla::tagging::TableHeaderScope::Row,
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use typst_library::foundations::Content;

    use super::*;

    #[track_caller]
    fn test(table: TableCtx, exp_tag: TagNode) {
        let tag = table.build_table(Vec::new());
        assert_eq!(exp_tag, tag);
    }

    #[track_caller]
    fn table<const SIZE: usize>(cells: [TableCell; SIZE]) -> TableCtx {
        let mut table = TableCtx::new(TableId(324), Some("summary".into()));
        for cell in cells {
            table.insert(&cell, Vec::new());
        }
        table
    }

    #[track_caller]
    fn header_cell(
        (x, y): (usize, usize),
        level: u32,
        scope: TableHeaderScope,
    ) -> TableCell {
        TableCell::new(Content::default())
            .with_x(Smart::Custom(x))
            .with_y(Smart::Custom(y))
            .with_kind(Smart::Custom(TableCellKind::Header(
                NonZeroU32::new(level).unwrap(),
                scope,
            )))
    }

    #[track_caller]
    fn footer_cell(x: usize, y: usize) -> TableCell {
        TableCell::new(Content::default())
            .with_x(Smart::Custom(x))
            .with_y(Smart::Custom(y))
            .with_kind(Smart::Custom(TableCellKind::Footer))
    }

    fn cell(x: usize, y: usize) -> TableCell {
        TableCell::new(Content::default())
            .with_x(Smart::Custom(x))
            .with_y(Smart::Custom(y))
            .with_kind(Smart::Custom(TableCellKind::Data))
    }

    fn empty_cell(x: usize, y: usize) -> TableCell {
        TableCell::new(Content::default())
            .with_x(Smart::Custom(x))
            .with_y(Smart::Custom(y))
            .with_kind(Smart::Auto)
    }

    fn table_tag<const SIZE: usize>(nodes: [TagNode; SIZE]) -> TagNode {
        let tag = TagKind::Table(Some("summary".into()));
        TagNode::Group(tag.into(), nodes.into())
    }

    fn thead<const SIZE: usize>(nodes: [TagNode; SIZE]) -> TagNode {
        TagNode::Group(TagKind::THead.into(), nodes.into())
    }

    fn tbody<const SIZE: usize>(nodes: [TagNode; SIZE]) -> TagNode {
        TagNode::Group(TagKind::TBody.into(), nodes.into())
    }

    fn tfoot<const SIZE: usize>(nodes: [TagNode; SIZE]) -> TagNode {
        TagNode::Group(TagKind::TFoot.into(), nodes.into())
    }

    fn trow<const SIZE: usize>(nodes: [TagNode; SIZE]) -> TagNode {
        TagNode::Group(TagKind::TR.into(), nodes.into())
    }

    fn th<const SIZE: usize>(
        (x, y): (u32, u32),
        scope: TableHeaderScope,
        headers: [(u32, u32); SIZE],
    ) -> TagNode {
        let scope = table_header_scope(scope);
        let id = table_cell_id(TableId(324), x, y);
        let ids = headers
            .map(|(x, y)| table_cell_id(TableId(324), x, y))
            .into_iter()
            .collect();
        TagNode::Group(
            TagKind::TH(TableHeaderCell::new(scope).with_headers(TagIdRefs { ids }))
                .with_id(Some(id)),
            vec![TagNode::Group(TagKind::P.into(), Vec::new())],
        )
    }

    fn td<const SIZE: usize>(headers: [(u32, u32); SIZE]) -> TagNode {
        let ids = headers
            .map(|(x, y)| table_cell_id(TableId(324), x, y))
            .into_iter()
            .collect();
        TagNode::Group(
            TagKind::TD(TableDataCell::new().with_headers(TagIdRefs { ids })).into(),
            vec![TagNode::Group(TagKind::P.into(), Vec::new())],
        )
    }

    #[test]
    fn simple_table() {
        #[rustfmt::skip]
        let table = table([
            header_cell((0, 0), 1, TableHeaderScope::Column),
            header_cell((1, 0), 1, TableHeaderScope::Column),
            header_cell((2, 0), 1, TableHeaderScope::Column),

            cell(0, 1),
            cell(1, 1),
            cell(2, 1),

            cell(0, 2),
            cell(1, 2),
            cell(2, 2),
        ]);

        #[rustfmt::skip]
        let tag = table_tag([
            thead([trow([
                th((0, 0), TableHeaderScope::Column, []),
                th((1, 0), TableHeaderScope::Column, []),
                th((2, 0), TableHeaderScope::Column, []),
            ])]),
            tbody([
                trow([
                    td([(0, 0)]),
                    td([(1, 0)]),
                    td([(2, 0)]),
                ]),
                trow([
                    td([(0, 0)]),
                    td([(1, 0)]),
                    td([(2, 0)]),
                ]),
            ]),
        ]);

        test(table, tag);
    }

    #[test]
    fn header_row_and_column() {
        #[rustfmt::skip]
        let table = table([
            header_cell((0, 0), 1, TableHeaderScope::Column),
            header_cell((1, 0), 1, TableHeaderScope::Column),
            header_cell((2, 0), 1, TableHeaderScope::Column),

            header_cell((0, 1), 1, TableHeaderScope::Row),
            cell(1, 1),
            cell(2, 1),

            header_cell((0, 2), 1, TableHeaderScope::Row),
            cell(1, 2),
            cell(2, 2),
        ]);

        #[rustfmt::skip]
        let tag = table_tag([
            trow([
                th((0, 0), TableHeaderScope::Column, []),
                th((1, 0), TableHeaderScope::Column, []),
                th((2, 0), TableHeaderScope::Column, []),
            ]),
            trow([
                th((0, 1), TableHeaderScope::Row, [(0, 0)]),
                td([(1, 0), (0, 1)]),
                td([(2, 0), (0, 1)]),
            ]),
            trow([
                th((0, 2), TableHeaderScope::Row, [(0, 0)]),
                td([(1, 0), (0, 2)]),
                td([(2, 0), (0, 2)]),
            ]),
        ]);

        test(table, tag);
    }

    #[test]
    fn complex_tables() {
        #[rustfmt::skip]
        let table = table([
            header_cell((0, 0), 1, TableHeaderScope::Column),
            header_cell((1, 0), 1, TableHeaderScope::Column),
            header_cell((2, 0), 1, TableHeaderScope::Column),

            header_cell((0, 1), 2, TableHeaderScope::Column),
            header_cell((1, 1), 2, TableHeaderScope::Column),
            header_cell((2, 1), 2, TableHeaderScope::Column),

            cell(0, 2),
            empty_cell(1, 2), // the type of empty cells is inferred from the row
            cell(2, 2),

            header_cell((0, 3), 2, TableHeaderScope::Column),
            header_cell((1, 3), 2, TableHeaderScope::Column),
            empty_cell(2, 3), // the type of empty cells is inferred from the row

            cell(0, 4),
            cell(1, 4),
            empty_cell(2, 4),

            empty_cell(0, 5), // the type of empty cells is inferred from the row
            footer_cell(1, 5),
            footer_cell(2, 5),
        ]);

        #[rustfmt::skip]
        let tag = table_tag([
            thead([
                trow([
                    th((0, 0), TableHeaderScope::Column, []),
                    th((1, 0), TableHeaderScope::Column, []),
                    th((2, 0), TableHeaderScope::Column, []),
                ]),
                trow([
                    th((0, 1), TableHeaderScope::Column, [(0, 0)]),
                    th((1, 1), TableHeaderScope::Column, [(1, 0)]),
                    th((2, 1), TableHeaderScope::Column, [(2, 0)]),
                ]),
            ]),
            tbody([
                trow([
                    td([(0, 1)]),
                    td([(1, 1)]),
                    td([(2, 1)]),
                ]),
            ]),
            thead([
                trow([
                    th((0, 3), TableHeaderScope::Column, [(0, 0)]),
                    th((1, 3), TableHeaderScope::Column, [(1, 0)]),
                    th((2, 3), TableHeaderScope::Column, [(2, 0)]),
                ]),
            ]),
            tbody([
                trow([
                    td([(0, 3)]),
                    td([(1, 3)]),
                    td([(2, 3)]),
                ]),
            ]),
            tfoot([
                trow([
                    td([(0, 3)]),
                    td([(1, 3)]),
                    td([(2, 3)]),
                ]),
            ]),
        ]);

        test(table, tag);
    }
}
