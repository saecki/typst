use ecow::EcoString;
use krilla::action::{Action, LinkAction};
use krilla::annotation::Target;
use krilla::configure::Validator;
use krilla::destination::XyzDestination;
use krilla::geom as kg;
use typst_library::layout::{Abs, Point, Position, Size};
use typst_library::model::Destination;

use crate::convert::{FrameContext, GlobalContext};
use crate::tags::{self, Placeholder, StackEntryKind, TagNode};
use crate::util::{AbsExt, PointExt};

pub(crate) struct LinkAnnotation {
    pub(crate) id: tags::LinkId,
    pub(crate) placeholder: Placeholder,
    pub(crate) alt: Option<String>,
    pub(crate) rect: kg::Rect,
    pub(crate) quad_points: Vec<kg::Point>,
    pub(crate) target: Target,
}

pub(crate) fn handle_link(
    fc: &mut FrameContext,
    gc: &mut GlobalContext,
    dest: &Destination,
    size: Size,
) {
    let target = match dest {
        Destination::Url(u) => {
            Target::Action(Action::Link(LinkAction::new(u.to_string())))
        }
        Destination::Position(p) => match pos_to_target(gc, *p) {
            Some(target) => target,
            None => return,
        },
        Destination::Location(loc) => {
            if let Some(nd) = gc.loc_to_names.get(loc) {
                // If a named destination has been registered, it's already guaranteed to
                // not point to an excluded page.
                Target::Destination(krilla::destination::Destination::Named(nd.clone()))
            } else {
                let pos = gc.document.introspector.position(*loc);
                match pos_to_target(gc, pos) {
                    Some(target) => target,
                    None => return,
                }
            }
        }
    };

    let entry = gc.tags.stack.last_mut().expect("a link parent");
    let StackEntryKind::Link(link_id, ref link) = entry.kind else {
        unreachable!("expected a link parent")
    };
    let alt = link.alt.as_ref().map(EcoString::to_string);

    let rect = to_rect(fc, size);
    let quadpoints = quadpoints(rect);

    // Unfortunately quadpoints still aren't well supported by most PDF readers,
    // even by acrobat. Which is understandable since they were only introduced
    // in PDF 1.6 (2005) /s
    let should_use_quadpoints = gc.options.standards.config.validator() == Validator::UA1;
    match fc.get_link_annotation(link_id) {
        Some(annotation) if should_use_quadpoints => {
            // Update the bounding box and add the quadpoints to an existing link annotation.
            annotation.rect = bounding_rect(annotation.rect, rect);
            annotation.quad_points.extend_from_slice(&quadpoints);
        }
        _ => {
            let placeholder = gc.tags.reserve_placeholder();
            gc.tags.push(TagNode::Placeholder(placeholder));
            fc.push_link_annotation(LinkAnnotation {
                id: link_id,
                placeholder,
                rect,
                quad_points: quadpoints.to_vec(),
                alt,
                target,
            });
        }
    }
}

// Compute the bounding box of the transformed link.
fn to_rect(fc: &FrameContext, size: Size) -> kg::Rect {
    let mut min_x = Abs::inf();
    let mut min_y = Abs::inf();
    let mut max_x = -Abs::inf();
    let mut max_y = -Abs::inf();

    let pos = Point::zero();

    for point in [
        pos,
        pos + Point::with_x(size.x),
        pos + Point::with_y(size.y),
        pos + size.to_point(),
    ] {
        let t = point.transform(fc.state().transform());
        min_x.set_min(t.x);
        min_y.set_min(t.y);
        max_x.set_max(t.x);
        max_y.set_max(t.y);
    }

    let x1 = min_x.to_f32();
    let x2 = max_x.to_f32();
    let y1 = min_y.to_f32();
    let y2 = max_y.to_f32();

    kg::Rect::from_ltrb(x1, y1, x2, y2).unwrap()
}

fn bounding_rect(a: kg::Rect, b: kg::Rect) -> kg::Rect {
    kg::Rect::from_ltrb(
        a.left().min(b.left()),
        a.top().min(b.top()),
        a.right().max(b.right()),
        a.bottom().max(b.bottom()),
    )
    .unwrap()
}

fn quadpoints(rect: kg::Rect) -> [kg::Point; 4] {
    [
        kg::Point::from_xy(rect.left(), rect.bottom()),
        kg::Point::from_xy(rect.right(), rect.bottom()),
        kg::Point::from_xy(rect.right(), rect.top()),
        kg::Point::from_xy(rect.left(), rect.top()),
    ]
}

fn pos_to_target(gc: &mut GlobalContext, pos: Position) -> Option<Target> {
    let page_index = pos.page.get() - 1;
    let index = gc.page_index_converter.pdf_page_index(page_index)?;

    let dest = XyzDestination::new(index, pos.point.to_krilla());
    Some(Target::Destination(krilla::destination::Destination::Xyz(dest)))
}
