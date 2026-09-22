# Global review of the English edition

## Scope completed

- Scientific and terminological review of all 21 English chapters.
- Validation of 76 cited bibliography keys against the BibTeX database.
- Validation of labels, cross-references, chapter order, and English headings.
- Full Biber build using `en_US` sorting.
- Stabilized XeLaTeX compilation and visual inspection of representative pages.

## Global terminology decisions

- Presence--background outputs are described as relative occurrence or
  environmental-suitability scores, not absolute occurrence probabilities.
- Standard deviation among algorithms is labeled algorithmic disagreement; it
  is not presented as a confidence interval.
- Scenario spread, GCM disagreement, environmental novelty, bias, and
  uncertainty retain distinct meanings.
- MESS and MOP are diagnostics of environmental support and novelty, not direct
  estimates of prediction error.
- Future and paleoclimate maps are conditional projections, not deterministic
  forecasts or evidence of historical occurrence.
- The accessible area is consistently denoted by \(M\).

## Build and QA result

- Final source: `species_distribution_modeling_en.tex`
- Final PDF: `output/pdf/species_distribution_modeling_en.pdf`
- Physical pages: 1,013
- Bibliography: 76 cited entries, no missing keys, no Biber warnings or errors
- XeLaTeX: no fatal errors, missing glyphs, overfull boxes, undefined citations,
  or undefined cross-references
- PDF metadata: title, author, subject, keywords, and creator embedded
- Page size: A4
- Visual QA: cover, front matter, contents, chapter openings, pedagogical boxes,
  equations, R listings, figures, long tables, glossary, essential reading, and
  bibliography inspected

## Submission-stage work still required

The compiled volume is a stable reviewed reading edition. A Springer Nature
submission package should additionally:

1. migrate from the project-specific `ueap-sdm` class to the current Springer
   Nature monograph template supplied for the contracted project;
2. add a concise abstract and keywords to each substantive chapter in the
   structure requested by the acquiring editor;
3. decide whether full R scripts remain in the printed volume or move to a
   versioned companion repository and supplementary archive—the current
   complete listings account for most of the 1,013 pages;
4. localize comments and reader-facing strings inside companion R scripts while
   preserving executable object names, paths, and data-field contracts;
5. complete permissions and accessibility review for all figures, datasets,
   third-party boundaries, and fonts; the present PDF is not tagged for screen
   readers;
6. perform professional copyediting and author proof review after the publisher
   applies its production template.

These are production and submission tasks rather than unresolved compilation
defects in the reviewed PDF.
