use {
    gloo::utils::document,
    malarkdowney::{
        self,
        Block,
        linemarkdown::{
            code,
            emphasis,
            strong,
            text,
        },
    },
};

fn main() {
    console_error_panic_hook::set_once();
    let m =
        malarkdowney::build(
            vec![
                Block::Heading(1, vec![text("# What is this malarkdowney")]),
                Block::Heading(2, vec![text("## It is a visual markdown editor")]),
                Block::Line(vec![text(
                    //. .
                    "When editing markdown in common text editors, the mapping between the markdown and the rendered representation is hard to understand.  While not as bad as ReST or HTML, things like how many spaces are needed to make a line part of a block, whether a symbol needs escaping (and how to escape it) and whether ",
                ), code("()"), text(" or "), code("[]"), text(" comes first in a link are easy to confuse.")]),
                Block::Ul(vec![
                    //. .
                    vec![
                        //. .
                        Block::Line(vec![text(
                            //. .
                            "One solution many editors take is to offer a preview, either as a toggle button or as live preview in a separate pane.  A toggle button adds friction to editing though, and a separate live preview pane effectively halves available screen space.",
                        )])
                    ],
                    vec![
                        //. .
                        Block::Line(vec![text(
                            //. .
                            "Conversely, WYSIWYG rich text editors have the issue that because markup state transitions are invisible and zero-width, editing intent is ambiguous.",
                        )]),
                        Block::Line(vec![text(
                            //. .
                            "For example, if your cursor is between a normal letter and a bold letter and you press \"x\", should \"x\" be bold or not?  There's no way to convey what you want to the editor, and this is both frustrating and, depending on the editor, can lead to a lot of leftover junk markup.",
                        )])
                    ]
                ]),
                Block::Line(vec![
                    //. .
                    text(
                        "Malarkdowney sits in the middle: it applies the style to the markdown text as you write it without modifying any text - that is, ",
                    ),
                    strong(vec![text("all")]),
                    text(" "),
                    emphasis(vec![text("control")]),
                    text(" "),
                    code("characters"),
                    text(
                        " are always visible, simultaneously resolving cursor ambiguity and providing feedback on the final style.",
                    )
                ]),
                Block::Line(vec![text("(This text is editable.)")]),
            ],
        );
    document().append_child(&m).unwrap();
    document().append_child(&{
        let e = document().create_element("hr").unwrap();
        e
    }.into()).unwrap();
    document().append_child(&{
        let e = document().create_element("p").unwrap();
        e.class_list().add_1("footnote").unwrap();
        e.append_child(&{
            let e = document().create_element("span").unwrap();
            e.set_text_content(Some("From "));
            e
        }.into()).unwrap();
        e.append_child(&{
            let e = document().create_element("a").unwrap();
            e.set_attribute("href", "https://github.com/andrewbaxter/malarkdowney").unwrap();
            e.set_text_content(Some("andrewbaxter/malarkdowney"));
            e
        }.into()).unwrap();
        e.append_child(&{
            let e = document().create_element("span").unwrap();
            e.set_text_content(Some(", with love and bountiful markdowns"));
            e
        }.into()).unwrap();
        e
    }.into()).unwrap();
}
