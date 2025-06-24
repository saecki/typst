use std::cell::OnceCell;
use std::collections::HashMap;

use ecow::EcoString;
use krilla::page::Page;
use krilla::surface::Surface;
use krilla::tagging::{
    ArtifactType, ContentTag, Identifier, Node, TableCellSpan, TableDataCell,
    TableHeaderCell, TableHeaderScope, Tag, TagBuilder, TagGroup, TagKind, TagTree,
};
use typst_library::foundations::{Content, LinkMarker, Packed, StyleChain};
use typst_library::introspection::Location;
use typst_library::model::{
    Destination, FigureCaption, FigureElem, HeadingElem, Outlinable, OutlineElem,
    OutlineEntry, TableCell, TableElem, TableHLine, TableVLine,
};
use typst_library::pdf::{ArtifactElem, ArtifactKind, PdfTagElem, PdfTagKind};
use typst_library::visualize::ImageElem;

use crate::convert::GlobalContext;
use crate::link::LinkAnnotation;

pub(crate) struct Tags {
    /// The intermediary stack of nested tag groups.
    pub(crate) stack: Vec<StackEntry>,
    /// A list of placeholders corresponding to a [`TagNode::Placeholder`].
    pub(crate) placeholders: Vec<OnceCell<Node>>,
    pub(crate) in_artifact: Option<(Location, ArtifactKind)>,
    pub(crate) link_id: LinkId,

    /// The output.
    pub(crate) tree: Vec<TagNode>,
}

pub(crate) struct StackEntry {
    pub(crate) loc: Location,
    pub(crate) kind: StackEntryKind,
    pub(crate) nodes: Vec<TagNode>,
}

pub(crate) enum StackEntryKind {
    Standard(Tag),
    Link(LinkId, Packed<LinkMarker>),
    Table(TableCtx),
    TableCell(Packed<TableCell>),
}

pub(crate) struct TableCtx {
    table: Packed<TableElem>,
    rows: Vec<Vec<Option<(Packed<TableCell>, Tag, Vec<TagNode>)>>>,
}

impl TableCtx {
    fn insert(&mut self, cell: Packed<TableCell>, nodes: Vec<TagNode>) {
        let x = cell.x(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let y = cell.y(StyleChain::default()).unwrap_or_else(|| unreachable!());
        let rowspan = cell.rowspan(StyleChain::default()).get();
        let colspan = cell.colspan(StyleChain::default()).get();

        // TODO: possibly set internal field on TableCell when resolving
        // the cell grid.
        let is_header = false;
        let span = TableCellSpan { rows: rowspan as i32, cols: colspan as i32 };
        let tag = if is_header {
            let scope = TableHeaderScope::Column; // TODO
            TagKind::TH(TableHeaderCell::new(scope).with_span(span))
        } else {
            TagKind::TD(TableDataCell::new().with_span(span))
        };

        let required_height = y + rowspan;
        if self.rows.len() < required_height {
            self.rows.resize_with(required_height, Vec::new);
        }

        let required_width = x + colspan;
        let row = &mut self.rows[y];
        if row.len() < required_width {
            row.resize_with(required_width, || None);
        }

        row[x] = Some((cell, tag.into(), nodes));
    }

    fn build_table(self, mut nodes: Vec<TagNode>) -> Vec<TagNode> {
        // Table layouting ensures that there are no overlapping cells, and that
        // any gaps left by the user are filled with empty cells.
        for row in self.rows.into_iter() {
            let mut row_nodes = Vec::new();
            for (_, tag, nodes) in row.into_iter().flatten() {
                row_nodes.push(TagNode::Group(tag, nodes));
            }

            // TODO: generate `THead`, `TBody`, and `TFoot`
            nodes.push(TagNode::Group(TagKind::TR.into(), row_nodes));
        }

        nodes
    }
}

pub(crate) enum TagNode {
    Group(Tag, Vec<TagNode>),
    Leaf(Identifier),
    /// Allows inserting a placeholder into the tag tree.
    /// Currently used for [`krilla::page::Page::add_tagged_annotation`].
    Placeholder(Placeholder),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LinkId(u32);

#[derive(Clone, Copy)]
pub(crate) struct Placeholder(usize);

impl Tags {
    pub(crate) fn new() -> Self {
        Self {
            stack: Vec::new(),
            placeholders: Vec::new(),
            in_artifact: None,

            tree: Vec::new(),
            link_id: LinkId(0),
        }
    }

    pub(crate) fn reserve_placeholder(&mut self) -> Placeholder {
        let idx = self.placeholders.len();
        self.placeholders.push(OnceCell::new());
        Placeholder(idx)
    }

    pub(crate) fn init_placeholder(&mut self, placeholder: Placeholder, node: Node) {
        self.placeholders[placeholder.0]
            .set(node)
            .map_err(|_| ())
            .expect("placeholder to be uninitialized");
    }

    pub(crate) fn take_placeholder(&mut self, placeholder: Placeholder) -> Node {
        self.placeholders[placeholder.0]
            .take()
            .expect("initialized placeholder node")
    }

    pub(crate) fn is_root(&self) -> bool {
        self.stack.is_empty()
    }

    /// Returns the current parent's list of children and the structure type ([Tag]).
    /// In case of the document root the structure type will be `None`.
    pub(crate) fn parent(&mut self) -> (Option<&mut StackEntryKind>, &mut Vec<TagNode>) {
        if let Some(entry) = self.stack.last_mut() {
            (Some(&mut entry.kind), &mut entry.nodes)
        } else {
            (None, &mut self.tree)
        }
    }

    pub(crate) fn push(&mut self, node: TagNode) {
        self.parent().1.push(node);
    }

    pub(crate) fn build_tree(&mut self) -> TagTree {
        let children = std::mem::take(&mut self.tree)
            .into_iter()
            .map(|node| self.resolve_node(node))
            .collect::<Vec<_>>();
        TagTree::from(children)
    }

    /// Resolves [`Placeholder`] nodes.
    fn resolve_node(&mut self, node: TagNode) -> Node {
        match node {
            TagNode::Group(tag, nodes) => {
                let children = nodes
                    .into_iter()
                    .map(|node| self.resolve_node(node))
                    .collect::<Vec<_>>();
                Node::Group(TagGroup::with_children(tag, children))
            }
            TagNode::Leaf(identifier) => Node::Leaf(identifier),
            TagNode::Placeholder(placeholder) => self.take_placeholder(placeholder),
        }
    }

    fn context_supports(&self, _tag: &StackEntryKind) -> bool {
        // TODO: generate using: https://pdfa.org/resource/iso-ts-32005-hierarchical-inclusion-rules/
        true
    }

    fn next_link_id(&mut self) -> LinkId {
        self.link_id.0 += 1;
        self.link_id
    }
}

/// Marked-content may not cross page boundaries: restart tag that was still open
/// at the end of the last page.
pub(crate) fn restart_open(gc: &mut GlobalContext, surface: &mut Surface) {
    // TODO: somehow avoid empty marked-content sequences
    if let Some((loc, kind)) = gc.tags.in_artifact {
        start_artifact(gc, surface, loc, kind);
    } else if let Some(entry) = gc.tags.stack.last_mut() {
        let id = surface.start_tagged(ContentTag::Other);
        entry.nodes.push(TagNode::Leaf(id));
    }
}

/// Marked-content may not cross page boundaries: end any open tag.
pub(crate) fn end_open(gc: &mut GlobalContext, surface: &mut Surface) {
    if !gc.tags.stack.is_empty() || gc.tags.in_artifact.is_some() {
        surface.end_tagged();
    }
}

/// Add all annotations that were found in the page frame.
pub(crate) fn add_annotations(
    gc: &mut GlobalContext,
    page: &mut Page,
    annotations: HashMap<LinkId, LinkAnnotation>,
) {
    for annotation in annotations.into_values() {
        let LinkAnnotation { placeholder, alt, rect, quad_points, target } = annotation;
        let annot = krilla::annotation::Annotation::new_link(
            krilla::annotation::LinkAnnotation::new(rect, Some(quad_points), target),
            alt,
        );
        let annot_id = page.add_tagged_annotation(annot);
        gc.tags.init_placeholder(placeholder, Node::Leaf(annot_id));
    }
}

pub(crate) fn handle_start(
    gc: &mut GlobalContext,
    surface: &mut Surface,
    elem: &Content,
) {
    if gc.tags.in_artifact.is_some() {
        // Don't nest artifacts
        return;
    }

    let loc = elem.location().unwrap();

    if let Some(artifact) = elem.to_packed::<ArtifactElem>() {
        end_open(gc, surface);
        let kind = artifact.kind(StyleChain::default());
        start_artifact(gc, surface, loc, kind);
        return;
    }

    let tag: Tag = if let Some(pdf_tag) = elem.to_packed::<PdfTagElem>() {
        let kind = pdf_tag.kind(StyleChain::default());
        match kind {
            PdfTagKind::Part => TagKind::Part.into(),
            _ => todo!(),
        }
    } else if let Some(heading) = elem.to_packed::<HeadingElem>() {
        let level = heading.level();
        let name = heading.body.plain_text().to_string();
        match level.get() {
            1 => TagKind::H1(Some(name)).into(),
            2 => TagKind::H2(Some(name)).into(),
            3 => TagKind::H3(Some(name)).into(),
            4 => TagKind::H4(Some(name)).into(),
            5 => TagKind::H5(Some(name)).into(),
            // TODO: when targeting PDF 2.0 headings `> 6` are supported
            _ => TagKind::H6(Some(name)).into(),
        }
    } else if let Some(_) = elem.to_packed::<OutlineElem>() {
        TagKind::TOC.into()
    } else if let Some(_) = elem.to_packed::<OutlineEntry>() {
        TagKind::TOCI.into()
    } else if let Some(_) = elem.to_packed::<FigureElem>() {
        let alt = None; // TODO
        TagKind::Figure.with_alt_text(alt)
    } else if let Some(image) = elem.to_packed::<ImageElem>() {
        let alt = image.alt(StyleChain::default()).map(|s| s.to_string());

        end_open(gc, surface);
        let id = surface.start_tagged(ContentTag::Other);
        let mut node = TagNode::Leaf(id);

        if let Some(StackEntryKind::Standard(parent)) = gc.tags.parent().0 {
            if parent.kind == TagKind::Figure && parent.alt_text.is_none() {
                // HACK: set alt text of outer figure tag, if the contained image
                // has alt text specified
                parent.alt_text = alt;
            } else {
                node = TagNode::Group(TagKind::Figure.with_alt_text(alt), vec![node]);
            }
        } else {
            node = TagNode::Group(TagKind::Figure.with_alt_text(alt), vec![node]);
        }
        gc.tags.push(node);

        return;
    } else if let Some(_) = elem.to_packed::<FigureCaption>() {
        TagKind::Caption.into()
    } else if let Some(link) = elem.to_packed::<LinkMarker>() {
        let link_id = gc.tags.next_link_id();
        push_stack(gc, surface, loc, StackEntryKind::Link(link_id, link.clone()));
        return;
    } else if let Some(table) = elem.to_packed::<TableElem>() {
        let ctx = TableCtx { table: table.clone(), rows: Vec::new() };
        push_stack(gc, surface, loc, StackEntryKind::Table(ctx));
        return;
    } else if let Some(cell) = elem.to_packed::<TableCell>() {
        push_stack(gc, surface, loc, StackEntryKind::TableCell(cell.clone()));
        return;
    } else if let Some(_) = elem.to_packed::<TableHLine>() {
        end_open(gc, surface);
        start_artifact(gc, surface, loc, ArtifactKind::Other);
        return;
    } else if let Some(_) = elem.to_packed::<TableVLine>() {
        end_open(gc, surface);
        start_artifact(gc, surface, loc, ArtifactKind::Other);
        return;
    } else {
        return;
    };

    push_stack(gc, surface, loc, StackEntryKind::Standard(tag));
}

fn push_stack(
    gc: &mut GlobalContext,
    surface: &mut Surface,
    loc: Location,
    kind: StackEntryKind,
) {
    if !gc.tags.context_supports(&kind) {
        // TODO: error or warning?
    }

    // close previous marked-content and open a nested tag.
    end_open(gc, surface);
    let id = surface.start_tagged(krilla::tagging::ContentTag::Other);
    gc.tags
        .stack
        .push(StackEntry { loc, kind, nodes: vec![TagNode::Leaf(id)] });
}

pub(crate) fn handle_end(gc: &mut GlobalContext, surface: &mut Surface, loc: Location) {
    if let Some((l, _)) = gc.tags.in_artifact {
        if l == loc {
            gc.tags.in_artifact = None;
            surface.end_tagged();
            if let Some(entry) = gc.tags.stack.last_mut() {
                let id = surface.start_tagged(ContentTag::Other);
                entry.nodes.push(TagNode::Leaf(id));
            }
        }
        return;
    }

    let Some(entry) = gc.tags.stack.pop_if(|e| e.loc == loc) else {
        return;
    };

    surface.end_tagged();

    let node = match entry.kind {
        StackEntryKind::Standard(tag) => TagNode::Group(tag, entry.nodes),
        StackEntryKind::Link(_, link) => {
            let alt = link.alt.as_ref().map(EcoString::to_string);
            let tag = TagKind::Link.with_alt_text(alt);
            let mut node = TagNode::Group(tag, entry.nodes);
            // Wrap link in reference tag, if it's not a url.
            if let Destination::Position(_) | Destination::Location(_) = link.dest {
                node = TagNode::Group(TagKind::Reference.into(), vec![node]);
            }
            node
        }
        StackEntryKind::Table(ctx) => {
            let summary = ctx.table.summary(StyleChain::default()).map(EcoString::into);
            let nodes = ctx.build_table(entry.nodes);
            TagNode::Group(TagKind::Table(summary).into(), nodes)
        }
        StackEntryKind::TableCell(cell) => {
            let parent = gc.tags.stack.last_mut().expect("table");
            let StackEntryKind::Table(table_ctx) = &mut parent.kind else {
                unreachable!("expected table")
            };

            table_ctx.insert(cell, entry.nodes);

            // TODO: somehow avoid empty marked-content sequences
            let id = surface.start_tagged(ContentTag::Other);
            gc.tags.push(TagNode::Leaf(id));
            return;
        }
    };

    gc.tags.push(node);
    if !gc.tags.is_root() {
        // TODO: somehow avoid empty marked-content sequences
        let id = surface.start_tagged(ContentTag::Other);
        gc.tags.push(TagNode::Leaf(id));
    }
}

fn start_artifact(
    gc: &mut GlobalContext,
    surface: &mut Surface,
    loc: Location,
    kind: ArtifactKind,
) {
    let ty = artifact_type(kind);
    let id = surface.start_tagged(ContentTag::Artifact(ty));
    gc.tags.push(TagNode::Leaf(id));
    gc.tags.in_artifact = Some((loc, kind));
}

fn artifact_type(kind: ArtifactKind) -> ArtifactType {
    match kind {
        ArtifactKind::Header => ArtifactType::Header,
        ArtifactKind::Footer => ArtifactType::Footer,
        ArtifactKind::Page => ArtifactType::Page,
        ArtifactKind::Other => ArtifactType::Other,
    }
}
