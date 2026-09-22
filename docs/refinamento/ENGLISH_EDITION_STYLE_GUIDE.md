# English edition: editorial and terminology guide

## Editorial target

The English edition is an authored scientific textbook for an international
readership. It uses clear US English, SI units, decimal points, Oxford commas,
and sentence-case headings. Translation must preserve scientific meaning rather
than Portuguese syntax. No claim, citation, numerical result, or executable
instruction may be added or removed merely to improve fluency.

## Stable terminology

| Portuguese source | English edition |
|---|---|
| modelagem de distribuição de espécies | species distribution modeling (SDM) |
| adequabilidade ambiental | environmental suitability |
| área acessível / área M | accessible area / M area |
| registro de ocorrência | occurrence record |
| presença-background | presence-background |
| validação cruzada espacial | spatial cross-validation |
| bloco espacial | spatial block |
| viés amostral | sampling bias |
| autocorrelação espacial | spatial autocorrelation |
| extrapolação | extrapolation |
| transferibilidade | transferability |
| incerteza algorítmica | algorithmic uncertainty |
| cenário climático | climate scenario |
| refúgio climático | climate refugium |
| limiar | threshold |
| sensibilidade | sensitivity |
| especificidade | specificity |
| curva de resposta | response curve |

Use *background*, not pseudo-absence, unless the sampling design actually
creates pseudo-absences. Use *prediction* for new locations or times and
*fitted value* for training observations. Species names remain italicized.

## LaTeX and reproducibility rules

- Preserve labels, citation keys, equations, paths, object names, seeds, and R
  syntax.
- Translate prose, headings, captions, table text, callout titles, exercises,
  code comments, plot labels, console messages, and pedagogical annotations.
- Do not translate package names, function names, filenames, data-field names,
  or quoted values that are part of the computational interface.
- Every chapter should ultimately contain a concise abstract and keywords for
  discoverability and SpringerLink metadata.
- The reading edition may retain the project class; the submission edition
  must later be migrated to the current Springer Nature monograph template.
