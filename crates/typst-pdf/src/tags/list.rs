use krilla::tagging::{ListNumbering, TagKind};

use crate::tags::TagNode;

pub(crate) struct ListCtx {
    numbering: ListNumbering,
    items: Vec<ListItem>,
}

struct ListItem {
    label: Vec<TagNode>,
    body: Option<Vec<TagNode>>,
    sub_list: Option<TagNode>,
}

impl ListCtx {
    pub(crate) fn new(numbering: ListNumbering) -> Self {
        Self { numbering, items: Vec::new() }
    }

    pub(crate) fn push_label(&mut self, nodes: Vec<TagNode>) {
        self.items.push(ListItem { label: nodes, body: None, sub_list: None });
    }

    pub(crate) fn push_body(&mut self, mut nodes: Vec<TagNode>) {
        let item = self.items.last_mut().expect("ListItemLabel");

        // Nested lists are expected to have the following structure:
        //
        // Typst code
        // ```
        // - a
        // - b
        //     - c
        //     - d
        // - e
        // ```
        //
        // Structure tree
        // ```
        // <L>
        //     <LI>
        //         <Lbl> `-`
        //         <LBody> `a`
        //     <LI>
        //         <Lbl> `-`
        //         <LBody> `b`
        //     <L>
        //         <LI>
        //             <Lbl> `-`
        //             <LBody> `c`
        //         <LI>
        //             <Lbl> `-`
        //             <LBody> `d`
        //     <LI>
        //         <Lbl> `-`
        //         <LBody> `d`
        // ```
        //
        // So move the nested list out of the list item.
        if let [_, TagNode::Group(tag, _)] = nodes.as_slice() {
            if matches!(tag.kind, TagKind::L(_)) {
                item.sub_list = nodes.pop();
            }
        }

        item.body = Some(nodes);
    }

    pub(crate) fn build_list(self, mut nodes: Vec<TagNode>) -> TagNode {
        for item in self.items.into_iter() {
            nodes.push(TagNode::Group(
                TagKind::LI.into(),
                vec![
                    TagNode::Group(TagKind::Lbl.into(), item.label),
                    TagNode::Group(TagKind::LBody.into(), item.body.unwrap_or_default()),
                ],
            ));
            if let Some(sub_list) = item.sub_list {
                nodes.push(sub_list);
            }
        }
        TagNode::Group(TagKind::L(self.numbering).into(), nodes)
    }
}
