use std::cell::OnceCell;
use std::collections::HashMap;
use std::num::NonZeroU32;

use ecow::EcoString;
use krilla::configure::Validator;
use krilla::page::Page;
use krilla::surface::Surface;
use krilla::tagging::{
    ArtifactType, ContentTag, Identifier, ListNumbering, Node, SpanTag, TableDataCell,
    Tag, TagBuilder, TagGroup, TagKind, TagTree,
};
use typst_library::diag::SourceResult;
use typst_library::foundations::{
    Content, LinkMarker, NativeElement, Packed, RefableProperty, Settable,
    SettableProperty, StyleChain,
};
use typst_library::introspection::Location;
use typst_library::layout::RepeatElem;
use typst_library::model::{
    Destination, EnumElem, FigureCaption, FigureElem, HeadingElem, ListElem, Outlinable,
    OutlineEntry, ParElem, TableCell, TableElem, TermsElem,
};
use typst_library::pdf::{ArtifactElem, ArtifactKind, PdfMarkerTag, PdfMarkerTagKind};
use typst_library::visualize::ImageElem;

use crate::convert::GlobalContext;
use crate::link::LinkAnnotation;
use crate::tags::list::ListCtx;
use crate::tags::outline::OutlineCtx;
use crate::tags::table::TableCtx;

mod list;
mod outline;
mod table;

pub(crate) fn handle_start(
    gc: &mut GlobalContext,
    surface: &mut Surface,
    elem: &Content,
) -> SourceResult<()> {
    let loc = elem.location().expect("elem to be locatable");
    let elem_id = gc.tags.tag_map.len();
    let name = elem.elem().name();
    for _ in 0..gc.tags.tag_indent {
        eprint!("  ");
    }
    eprintln!("\x1b[33mSTART\x1b[0m {elem_id} {name}");
    let indent = gc.tags.tag_indent;
    gc.tags.tag_indent += 1;
    gc.tags.tag_map.insert(loc, (indent, elem_id, name));

    if gc.tags.in_artifact.is_some() {
        // Don't nest artifacts
        return Ok(());
    }

    if let Some(artifact) = elem.to_packed::<ArtifactElem>() {
        let kind = artifact.kind.get(StyleChain::default());
        push_artifact(gc, surface, loc, kind);
        return Ok(());
    } else if let Some(_) = elem.to_packed::<RepeatElem>() {
        push_artifact(gc, surface, loc, ArtifactKind::Other);
        return Ok(());
    }

    let tag: Tag = if let Some(tag) = elem.to_packed::<PdfMarkerTag>() {
        match tag.kind {
            PdfMarkerTagKind::OutlineBody => {
                push_stack(gc, loc, StackEntryKind::Outline(OutlineCtx::new()))?;
                return Ok(());
            }
            PdfMarkerTagKind::FigureBody => TagKind::Figure.into(),
            PdfMarkerTagKind::ListItemLabel => {
                push_stack(gc, loc, StackEntryKind::ListItemLabel)?;
                return Ok(());
            }
            PdfMarkerTagKind::ListItemBody => {
                push_stack(gc, loc, StackEntryKind::ListItemBody)?;
                return Ok(());
            }
            PdfMarkerTagKind::Label => TagKind::Lbl.into(),
        }
    } else if let Some(entry) = elem.to_packed::<OutlineEntry>() {
        push_stack(gc, loc, StackEntryKind::OutlineEntry(entry.clone()))?;
        return Ok(());
    } else if let Some(_list) = elem.to_packed::<ListElem>() {
        let numbering = ListNumbering::Circle; // TODO: infer numbering from `list.marker`
        push_stack(gc, loc, StackEntryKind::List(ListCtx::new(numbering)))?;
        return Ok(());
    } else if let Some(_enumeration) = elem.to_packed::<EnumElem>() {
        let numbering = ListNumbering::Decimal; // TODO: infer numbering from `enum.numbering`
        push_stack(gc, loc, StackEntryKind::List(ListCtx::new(numbering)))?;
        return Ok(());
    } else if let Some(_enumeration) = elem.to_packed::<TermsElem>() {
        let numbering = ListNumbering::None;
        push_stack(gc, loc, StackEntryKind::List(ListCtx::new(numbering)))?;
        return Ok(());
    } else if let Some(_) = elem.to_packed::<FigureElem>() {
        // Wrap the figure tag and the sibling caption in a container, if the
        // caption is contained within the figure like recommended for tables
        // screen readers might ignore it.
        // TODO: maybe this could be a `NonStruct` tag?
        TagKind::P.into()
    } else if let Some(_) = elem.to_packed::<FigureCaption>() {
        TagKind::Caption.into()
    } else if let Some(image) = elem.to_packed::<ImageElem>() {
        let alt = image.alt.get_as_ref().map(|s| s.to_string());

        let figure_tag = (gc.tags.parent())
            .and_then(StackEntryKind::as_standard_mut)
            .filter(|tag| tag.kind == TagKind::Figure);
        if let Some(figure_tag) = figure_tag {
            // Set alt text of outer figure tag, if not present.
            if figure_tag.alt_text.is_none() {
                figure_tag.alt_text = alt;
            }
            return Ok(());
        } else {
            TagKind::Figure.with_alt_text(alt)
        }
    } else if let Some(table) = elem.to_packed::<TableElem>() {
        let table_id = gc.tags.next_table_id();
        let summary = table.summary.get_as_ref().map(|s| s.to_string());
        let ctx = TableCtx::new(table_id, summary);
        push_stack(gc, loc, StackEntryKind::Table(ctx))?;
        return Ok(());
    } else if let Some(cell) = elem.to_packed::<TableCell>() {
        let table_ctx = gc.tags.parent_table();

        // Only repeated table headers and footer cells are layed out multiple
        // times. Mark duplicate headers as artifacts, since they have no
        // semantic meaning in the tag tree, which doesn't use page breaks for
        // it's semantic structure.
        if table_ctx.is_some_and(|ctx| ctx.contains(cell)) {
            // TODO: currently the first layouted cell is picked to be part of
            // the tag tree, for repeating footers this will be the cell on the
            // first page. Maybe it should be the cell on the last page, but that
            // would require more changes in the layouting code, or a pre-pass
            // on the frames to figure out if there are other footers following.
            push_artifact(gc, surface, loc, ArtifactKind::Other);
        } else {
            push_stack(gc, loc, StackEntryKind::TableCell(cell.clone()))?;
        }
        return Ok(());
    } else if let Some(heading) = elem.to_packed::<HeadingElem>() {
        let level = heading.level().try_into().unwrap_or(NonZeroU32::MAX);
        let name = heading.body.plain_text().to_string();
        TagKind::Hn(level, Some(name)).into()
    } else if let Some(_) = elem.to_packed::<ParElem>() {
        TagKind::P.into()
    } else if let Some(link) = elem.to_packed::<LinkMarker>() {
        let link_id = gc.tags.next_link_id();
        push_stack(gc, loc, StackEntryKind::Link(link_id, link.clone()))?;
        return Ok(());
    } else {
        return Ok(());
    };

    push_stack(gc, loc, StackEntryKind::Standard(tag))?;

    Ok(())
}

pub(crate) fn handle_end(gc: &mut GlobalContext, surface: &mut Surface, loc: Location) {
    if let Some((indent, elem_id, name)) = gc.tags.tag_map.get(&loc) {
        gc.tags.tag_indent -= 1;
        for _ in 0..gc.tags.tag_indent {
            eprint!("  ");
        }
        if *indent == gc.tags.tag_indent {
            eprintln!("\x1b[32mEND\x1b[0m   {elem_id} {name}");
        } else {
            gc.tags.tag_indent += 1;
            eprintln!("\x1b[31mMISMATCHED\x1b[0m {elem_id} {name}");
        }
    } else {
        eprintln!("\x1b[31mUNMATCHED\x1b[0m");
    };

    if let Some((l, _)) = gc.tags.in_artifact {
        if l == loc {
            pop_artifact(gc, surface);
        }
        return;
    }

    let Some(entry) = gc.tags.stack.pop_if(|e| e.loc == loc) else {
        return;
    };

    let node = match entry.kind {
        StackEntryKind::Standard(tag) => TagNode::Group(tag, entry.nodes),
        StackEntryKind::Outline(ctx) => ctx.build_outline(entry.nodes),
        StackEntryKind::OutlineEntry(outline_entry) => {
            let parent = gc.tags.stack.last_mut().and_then(|parent| {
                let ctx = parent.kind.as_outline_mut()?;
                Some((&mut parent.nodes, ctx))
            });
            let Some((parent_nodes, outline_ctx)) = parent else {
                // PDF/UA compliance of the structure hierarchy is checked
                // elsewhere. While this doesn't make a lot of sense, just
                // avoid crashing here.
                let tag = TagKind::TOCI.into();
                gc.tags.push(TagNode::Group(tag, entry.nodes));
                return;
            };

            outline_ctx.insert(parent_nodes, outline_entry, entry.nodes);
            return;
        }
        StackEntryKind::Table(ctx) => ctx.build_table(entry.nodes),
        StackEntryKind::TableCell(cell) => {
            let Some(table_ctx) = gc.tags.parent_table() else {
                // PDF/UA compliance of the structure hierarchy is checked
                // elsewhere. While this doesn't make a lot of sense, just
                // avoid crashing here.
                let tag = TagKind::TD(TableDataCell::new()).into();
                gc.tags.push(TagNode::Group(tag, entry.nodes));
                return;
            };

            table_ctx.insert(&cell, entry.nodes);
            return;
        }
        StackEntryKind::List(list) => list.build_list(entry.nodes),
        StackEntryKind::ListItemLabel => {
            let list_ctx = gc.tags.parent_list().expect("parent list");
            list_ctx.push_label(entry.nodes);
            return;
        }
        StackEntryKind::ListItemBody => {
            let list_ctx = gc.tags.parent_list().expect("parent list");
            list_ctx.push_body(entry.nodes);
            return;
        }
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
    };

    gc.tags.push(node);
}

fn push_stack(
    gc: &mut GlobalContext,
    loc: Location,
    kind: StackEntryKind,
) -> SourceResult<()> {
    if !gc.tags.context_supports(&kind) {
        if gc.options.standards.config.validator() == Validator::UA1 {
            // TODO: error
        } else {
            // TODO: warning
        }
    }

    gc.tags.stack.push(StackEntry { loc, kind, nodes: Vec::new() });

    Ok(())
}

fn push_artifact(
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

fn pop_artifact(gc: &mut GlobalContext, surface: &mut Surface) {
    surface.end_tagged();
    gc.tags.in_artifact = None;
}

pub(crate) fn page_start(gc: &mut GlobalContext, surface: &mut Surface) {
    if let Some((_, kind)) = gc.tags.in_artifact {
        let ty = artifact_type(kind);
        let id = surface.start_tagged(ContentTag::Artifact(ty));
        gc.tags.push(TagNode::Leaf(id));
    }
}

pub(crate) fn page_end(gc: &mut GlobalContext, surface: &mut Surface) {
    if gc.tags.in_artifact.is_some() {
        surface.end_tagged();
    }
}

/// Add all annotations that were found in the page frame.
pub(crate) fn add_annotations(
    gc: &mut GlobalContext,
    page: &mut Page,
    annotations: Vec<LinkAnnotation>,
) {
    for annotation in annotations.into_iter() {
        let LinkAnnotation { id: _, placeholder, alt, rect, quad_points, target } =
            annotation;
        let annot = krilla::annotation::Annotation::new_link(
            krilla::annotation::LinkAnnotation::new(rect, Some(quad_points), target),
            alt,
        );
        let annot_id = page.add_tagged_annotation(annot);
        gc.tags.init_placeholder(placeholder, Node::Leaf(annot_id));
    }
}

pub(crate) struct Tags {
    /// The intermediary stack of nested tag groups.
    pub(crate) stack: Vec<StackEntry>,
    /// A list of placeholders corresponding to a [`TagNode::Placeholder`].
    pub(crate) placeholders: Vec<OnceCell<Node>>,
    pub(crate) in_artifact: Option<(Location, ArtifactKind)>,
    /// Used to group multiple link annotations using quad points.
    pub(crate) link_id: LinkId,
    /// Used to generate IDs referenced in table `Headers` attributes.
    /// The IDs must be document wide unique.
    pub(crate) table_id: TableId,

    /// The output.
    pub(crate) tree: Vec<TagNode>,

    tag_map: HashMap<Location, (u32, usize, &'static str)>,
    tag_indent: u32,
}

impl Tags {
    pub(crate) fn new() -> Self {
        Self {
            stack: Vec::new(),
            placeholders: Vec::new(),
            in_artifact: None,

            tree: Vec::new(),
            link_id: LinkId(0),
            table_id: TableId(0),

            tag_map: HashMap::new(),
            tag_indent: 0,
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

    pub(crate) fn parent(&mut self) -> Option<&mut StackEntryKind> {
        self.stack.last_mut().map(|e| &mut e.kind)
    }

    pub(crate) fn parent_table(&mut self) -> Option<&mut TableCtx> {
        self.parent()?.as_table_mut()
    }

    pub(crate) fn parent_list(&mut self) -> Option<&mut ListCtx> {
        self.parent()?.as_list_mut()
    }

    pub(crate) fn find_parent_link(&self) -> Option<(LinkId, &Packed<LinkMarker>)> {
        self.stack.iter().rev().find_map(|entry| entry.kind.as_link())
    }

    pub(crate) fn push(&mut self, node: TagNode) {
        if let Some(entry) = self.stack.last_mut() {
            entry.nodes.push(node);
        } else {
            self.tree.push(node);
        }
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

    fn next_table_id(&mut self) -> TableId {
        self.table_id.0 += 1;
        self.table_id
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TableId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct LinkId(u32);

#[derive(Debug)]
pub(crate) struct StackEntry {
    pub(crate) loc: Location,
    pub(crate) kind: StackEntryKind,
    pub(crate) nodes: Vec<TagNode>,
}

#[derive(Debug)]
pub(crate) enum StackEntryKind {
    Standard(Tag),
    Outline(OutlineCtx),
    OutlineEntry(Packed<OutlineEntry>),
    Table(TableCtx),
    TableCell(Packed<TableCell>),
    List(ListCtx),
    ListItemLabel,
    ListItemBody,
    Link(LinkId, Packed<LinkMarker>),
}

impl StackEntryKind {
    pub(crate) fn as_standard_mut(&mut self) -> Option<&mut Tag> {
        if let Self::Standard(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn as_outline_mut(&mut self) -> Option<&mut OutlineCtx> {
        if let Self::Outline(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn as_table_mut(&mut self) -> Option<&mut TableCtx> {
        if let Self::Table(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn as_list_mut(&mut self) -> Option<&mut ListCtx> {
        if let Self::List(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn as_link(&self) -> Option<(LinkId, &Packed<LinkMarker>)> {
        if let Self::Link(id, link) = self {
            Some((*id, link))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum TagNode {
    Group(Tag, Vec<TagNode>),
    Leaf(Identifier),
    /// Allows inserting a placeholder into the tag tree.
    /// Currently used for [`krilla::page::Page::add_tagged_annotation`].
    Placeholder(Placeholder),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Placeholder(usize);

/// Automatically calls [`Surface::end_tagged`] when dropped.
pub(crate) struct TagHandle<'a, 'b> {
    surface: &'b mut Surface<'a>,
    /// Whether this tag handle started the marked content sequence, and should
    /// thus end it when it is dropped.
    started: bool,
}

impl Drop for TagHandle<'_, '_> {
    fn drop(&mut self) {
        if self.started {
            self.surface.end_tagged();
        }
    }
}

impl<'a> TagHandle<'a, '_> {
    pub(crate) fn surface<'c>(&'c mut self) -> &'c mut Surface<'a> {
        self.surface
    }
}

/// Returns a [`TagHandle`] that automatically calls [`Surface::end_tagged`]
/// when dropped.
pub(crate) fn start_marked<'a, 'b>(
    gc: &mut GlobalContext,
    surface: &'b mut Surface<'a>,
) -> TagHandle<'a, 'b> {
    start_content(gc, surface, ContentTag::Other)
}

/// Returns a [`TagHandle`] that automatically calls [`Surface::end_tagged`]
/// when dropped.
pub(crate) fn start_span<'a, 'b>(
    gc: &mut GlobalContext,
    surface: &'b mut Surface<'a>,
    span: SpanTag,
) -> TagHandle<'a, 'b> {
    start_content(gc, surface, ContentTag::Span(span))
}

/// Returns a [`TagHandle`] that automatically calls [`Surface::end_tagged`]
/// when dropped.
pub(crate) fn start_artifact<'a, 'b>(
    gc: &mut GlobalContext,
    surface: &'b mut Surface<'a>,
    kind: ArtifactKind,
) -> TagHandle<'a, 'b> {
    let ty = artifact_type(kind);
    start_content(gc, surface, ContentTag::Artifact(ty))
}

fn start_content<'a, 'b>(
    gc: &mut GlobalContext,
    surface: &'b mut Surface<'a>,
    content: ContentTag,
) -> TagHandle<'a, 'b> {
    let content = if gc.tags.in_artifact.is_some() {
        return TagHandle { surface, started: false };
    } else if let Some(StackEntryKind::Table(_)) = gc.tags.stack.last().map(|e| &e.kind) {
        // Mark any direct child of a table as an aritfact. Any real content
        // will be wrapped inside a `TableCell`.
        ContentTag::Artifact(ArtifactType::Other)
    } else {
        content
    };
    let id = surface.start_tagged(content);
    gc.tags.push(TagNode::Leaf(id));
    TagHandle { surface, started: true }
}

fn artifact_type(kind: ArtifactKind) -> ArtifactType {
    match kind {
        ArtifactKind::Header => ArtifactType::Header,
        ArtifactKind::Footer => ArtifactType::Footer,
        ArtifactKind::Page => ArtifactType::Page,
        ArtifactKind::Other => ArtifactType::Other,
    }
}

trait PropertyGetAsRef<E, T, const I: u8> {
    fn get_as_ref(&self) -> Option<&T>;
}

impl<E, T, const I: u8> PropertyGetAsRef<E, T, I> for Settable<E, I>
where
    E: NativeElement,
    E: SettableProperty<I, Type = Option<T>>,
    E: RefableProperty<I>,
{
    fn get_as_ref(&self) -> Option<&T> {
        self.get_ref(StyleChain::default()).as_ref()
    }
}
