#let hdir-report-title = [$report-title$]
#let hdir-report-label = [$report-label$]
#let hdir-chapter-label = [$subtitle$]
#let hdir-chapter-title = [$title$]

#show: doc => article(
$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(mainfont)$
  font: ("$mainfont$",),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
$if(section-numbering)$
  sectionnumbering: "$section-numbering$",
$endif$
  doc,
)
