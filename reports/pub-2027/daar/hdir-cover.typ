#let hdir-navy = rgb("#0e4a63")
#let hdir-dark = rgb("#04546c")

#page(header: none, footer: none, numbering: none)[
  #image("Hdir_logo.png", height: 1.1cm)
  #v(3.2cm)
  #text(size: 40pt, fill: hdir-navy, weight: "regular")[#hdir-report-title]
  #v(1.4cm)
  #text(size: 12pt, fill: hdir-dark, weight: "bold")[#hdir-report-label]

  #v(1fr)

  #text(size: 14pt, fill: hdir-dark, weight: "bold")[Innhold]
  #v(0.4cm)
  #set text(size: 11pt)
  #show outline.entry: set block(spacing: 1.1em)
  #outline(title: none, depth: 1, indent: 0pt)
]

#text(size: 11pt, weight: "bold")[#hdir-chapter-label]
#v(-0.3em)
#text(size: 24pt, weight: "bold")[#hdir-chapter-title]
#v(0.8em)
