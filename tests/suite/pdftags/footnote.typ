--- footnote-tags-basic pdftags(ua-1) ---
Footnote #footnote[Hi] in text.

--- footnote-tags-different-lang pdftags(ua-1) ---
Footnote #footnote[
  // The footnote number is still in english
  #set text(lang: "de")
  Hallo
] in text.

--- footnote-tags-ref-to-other-footnote pdftags(ua-1) ---
This #footnote[content]<note> and #footnote(<note>).
