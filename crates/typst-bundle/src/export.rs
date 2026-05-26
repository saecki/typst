use comemo::{Track, Tracked};
use ecow::EcoString;
use indexmap::IndexMap;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use rustc_hash::FxBuildHasher;
use typst_html::HtmlElement;
use typst_layout::PagedDocument;
use typst_library::diag::{At, ParallelCollectCombinedResult, SourceResult, StrResult};
use typst_library::foundations::{Bytes, Fold, Smart};
use typst_library::introspection::Location;
use typst_library::model::{LateLinkResolver, PagedFormatOptions};
use typst_pdf::{PdfOptions, Timestamp};
use typst_render::RenderOptions;
use typst_syntax::{Span, VirtualPath};
use typst_utils::Scalar;

use crate::{Bundle, BundleDocument, BundleFile, PagedFormatOptions};

/// A raw mapping from paths to bytes.
pub type VirtualFs = IndexMap<VirtualPath, Bytes, FxBuildHasher>;

/// Exports a bundle into a raw virtual file system.
#[typst_macros::time(name = "export bundle")]
pub fn export(bundle: &Bundle, options: &ExternalOptions) -> SourceResult<VirtualFs> {
    bundle
        .files
        .par_iter()
        .map(|(path, file)| {
            let data = match file {
                BundleFile::Document(doc) => {
                    let link_resolver =
                        LateLinkResolver::new(Some(path), bundle.introspector.as_ref());
                    export_document(doc, options, link_resolver.track())
                }
                BundleFile::Asset(bytes) => Ok(bytes.clone()),
            };
            data.map(|data| (path.clone(), data))
        })
        .collect_combined_result()
}

/// External settings for bundle export.
#[derive(Debug)]
pub struct ExternalOptions {
    /// Options for exporting PDF documents.
    pub pdf: ExternalPdfOptions,
}

/// External settings for PDF export.
#[derive(Debug)]
pub struct ExternalPdfOptions {
    /// If not `None`, shall be the creation timestamp of the document. It will
    /// only be used if `set document(date: ..)` is `auto`.
    pub timestamp: Option<Timestamp>,
}

impl ExternalPdfOptions {
    pub fn export(self) -> StrResult<PdfOptions<'static>> {
        let standards =
            self.options.standard.map(typst_pdf::PdfStandards::new).transpose()?;
        Ok(PdfOptions {
            ident: Smart::Auto,
            timestamp: self.timestamp,
            page_ranges: self.options.pages,
            standards: standards.unwrap_or_default(),
            tagged: self.options.tagged.unwrap_or(true),
        })
    }
}

/// Exports a single document.
fn export_document(
    doc: &BundleDocument,
    ext_opts: &ExternalOptions,
    link_resolver: Tracked<LateLinkResolver>,
) -> SourceResult<Bytes> {
    match doc {
        BundleDocument::Paged(doc, extras) => match extras.format {
            PagedFormatOptions::Pdf(doc_opts) => {
                // TODO: Store span of document eleme somewhere.
                let options = ExternalPdfOptions {
                    options: ext_opts.pdf.options.fold(doc_opts),
                    ..ext_opts.pdf
                }
                .export()
                .at(Span::detached())?;
                export_pdf(doc, &options, &extras.anchors, link_resolver)
            }
            PagedFormatOptions::Png(doc_opts) => {
                let options = ext_opts.png.fold(doc_opts);
                // This is the default value of: 144ppi == 2ppt
                let pixel_per_pt = options.pixel_per_pt.unwrap_or(Scalar::new(2.0));
                export_png(doc, pixel_per_pt)
            }
            PagedFormatOptions::Svg(doc_opts) => {
                export_svg(doc, &doc_opts.svg, &extras.anchors, link_resolver)
            }
        },
        BundleDocument::Html(doc, doc_opts) => export_html(doc.root(), link_resolver),
    }
}

/// Exports a PDF document.
#[comemo::memoize]
#[typst_macros::time(name = "export pdf")]
fn export_pdf(
    doc: &PagedDocument,
    options: &PdfOptions,
    anchors: &[(Location, EcoString)],
    link_resolver: Tracked<LateLinkResolver>,
) -> SourceResult<Bytes> {
    typst_pdf::pdf_in_bundle(doc, options, anchors, link_resolver).map(Bytes::new)
}

/// Exports a PNG document.
#[comemo::memoize]
#[typst_macros::time(name = "export png")]
fn export_png(doc: &PagedDocument, pixel_per_pt: Scalar) -> SourceResult<Bytes> {
    let opts = RenderOptions {
        pixel_per_pt: pixel_per_pt.get() as f32,
        render_bleed: false,
    };
    typst_render::render(&doc.pages()[0], &opts)
        .encode_png()
        .map(Bytes::new)
        .map_err(|_| "failed to encode PNG")
        .at(Span::detached())
}

/// Exports an SVG document.
#[comemo::memoize]
#[typst_macros::time(name = "export svg")]
fn export_svg(
    doc: &PagedDocument,
    options: &SvgDocumentOptions,
    anchors: &[(Location, EcoString)],
    link_resolver: Tracked<LateLinkResolver>,
) -> SourceResult<Bytes> {
    let anchors = anchors
        .iter()
        .filter_map(|(loc, name)| {
            // We only support a single page at the moment and all anchor
            // location should point into it, so it's safe to extract just the
            // point using the document's introspector.
            let point = doc.introspector().position(*loc)?.point;
            Some((point, name.clone()))
        })
        .collect::<Vec<_>>();
    Ok(Bytes::from_string(typst_svg::svg_in_bundle(
        &doc.pages()[0],
        &anchors,
        link_resolver,
    )))
}

/// Exports an HTML document.
///
/// This function takes the root element rather than the document because it
/// doesn't need the metadata or introspector and this way, it can be memoized.
/// Bringing the HTML introspector across the memoization boundary is a little
/// trickier than the paged one because the HTML document is mutated after being
/// built (for linking), which means it's not 100% derived from the document.
#[comemo::memoize]
#[typst_macros::time(name = "export html")]
fn export_html(
    root: &HtmlElement,
    link_resolver: Tracked<LateLinkResolver>,
    pretty: bool,
) -> SourceResult<Bytes> {
    typst_html::html_in_bundle(root, link_resolver, pretty).map(Bytes::from_string)
}
