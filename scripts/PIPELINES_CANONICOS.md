# Registro de pipelines canônicos

Este arquivo distingue a cadeia usada pelo livro de exemplos simulados, versões
paralelas e scripts ainda em avaliação. A classificação não autoriza exclusão.

## Unidades 1–6

| Unidade | Cadeia canônica atual | Situação |
|---|---|---|
| 1 | `01_simulacao_nicho_ecologico.R` | Script único exibido no capítulo |
| 2 | `01_compilar_ocorrencias_dinizia.R` a `07_pipeline_unidade02_dinizia.R` | Cadeia contínua de *Dinizia excelsa* |
| 3 | `01_variaveis_ambientais_dinizia.R` | Cadeia do capítulo; o pipeline `10_` pertence a uma sequência didática paralela |
| 4 | `01_estrutura_pastas.R` a `06_pca_variaveis.R`, orquestrados por `08_pipeline_multicolinearidade.R` | O nome `07_pipeline_` citado anteriormente não existia e foi corrigido |
| 5 | `01_estrutura_pastas.R` a `06_pipeline_area_m_background.R` | Cadeia de área M e background usada no capítulo |
| 6 | `01_estrutura_pastas.R` a `07_pipeline_glm.R` | Primeira cadeia de modelagem completa |

## Regras

1. O pipeline canônico deve usar *Dinizia excelsa* quando o capítulo usa esse estudo de caso.
2. Scripts simulados permanecem disponíveis, mas devem ser identificados como exemplos complementares.
3. O pipeline deve ser executado a partir da raiz do projeto R.
4. Nenhum script instala pacotes automaticamente.
5. Dependências ausentes devem produzir mensagem explícita.
6. Etapas estocásticas devem declarar seed.
7. Arquivos de entrada e saída devem usar caminhos relativos à raiz.

## Próxima auditoria

## Unidades 7–17

| Unidade | Pipeline canônico | Tema |
|---|---|---|
| 7 | `08_pipeline_gam.R` | GAM |
| 8 | `08_pipeline_rf.R` | Random Forest |
| 9 | `08_pipeline_brt.R` | Boosted Regression Trees |
| 10 | `09_pipeline_maxent.R` | Maxent com `maxnet` |
| 11 | `08_pipeline_ensemble.R` | Ensemble e incerteza algorítmica |
| 12 | `08_pipeline_avaliacao.R` | Avaliação integrada |
| 13 | `08_pipeline_projecoes_futuras.R` | CMIP6 e cenários futuros |
| 14 | `07_pipeline_transferencia_extrapolacao.R` | MESS, MOP e extrapolação |
| 15 | `08_pipeline_paleoclima.R` | Paleoclima e estabilidade histórica |
| 16 | `09_pipeline_incertezas.R` | Propagação de incertezas |
| 17 | `09_pipeline_conservacao.R` | Conservação e priorização |

Os pipelines alternativos com numeração concorrente permanecem preservados para
auditoria. Eles não devem ser apresentados como cadeia principal sem revisão do
estudo de caso, das entradas e das saídas.

## Estado da migração

- Unidades 1–6: 29 scripts canônicos migrados.
- Unidades 7–17: 90 scripts canônicos migrados.
- Total: 119 scripts canônicos.
- Caminhos absolutos efetivos: zero.
- Instalações automáticas: zero.
- `rm(list = ls())`: zero.
- Etapas estocásticas detectadas sem seed: zero.
- Fontes encadeadas ausentes: zero.
