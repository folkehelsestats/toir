// Page header with logo + footer for the PDF (Typst) output
#let hdir-dark = rgb("#04546c")
#set page(
  header: [
    #image("Hdir_logo.png", height: 0.8cm)
    #v(-4pt)
    #line(length: 100%, stroke: 0.5pt + rgb("#cfd8dc"))
  ],
  header-ascent: 30%,
  footer: context [
    #set text(size: 8pt, fill: rgb("#4d4d4d"))
    Tall om illegale rusmidler #h(1fr) #counter(page).display()
  ],
)
#show heading: set text(fill: rgb("#1c1c1c"))
#show link: set text(fill: hdir-dark)
#show figure.caption: set text(size: 8.5pt, fill: rgb("#4d4d4d"))
