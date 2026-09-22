# Fase 1 — Saneamento estrutural e reprodutível

## Escopo

Esta fase corrige a infraestrutura do livro sem alterar silenciosamente dados,
resultados analíticos ou interpretações ecológicas.

## Baseline

- Fonte principal inicial: `apostila_sdm_r.tex`.
- Classe: `ueap-sdm.cls`.
- Engine do último log: XeLaTeX (TeX Live 2026).
- Bibliografia: `biblatex` com backend `biber`.
- PDF de referência: 474 páginas.
- Scripts R auditados: 199.
- Figuras em `figuras/`: 298.
- Arquivos em `dados/`: 303.
- Arquivos em `resultados/`: 244.

## Regras de preservação

1. Não alterar valores científicos sem executar e validar o pipeline pertinente.
2. Não alterar dados brutos.
3. Não excluir scripts concorrentes antes de identificar o pipeline canônico.
4. Toda modularização LaTeX deve preservar a ordem textual.
5. Caminhos novos devem ser relativos à raiz do projeto.
6. Scripts não devem instalar pacotes automaticamente.
7. Etapas estocásticas devem declarar uma seed documentada.

## Sequência de trabalho

1. Corrigir erros de compilação conhecidos.
2. Remover ambiguidades da bibliografia.
3. Modularizar o manuscrito por front matter, capítulos e back matter.
4. Introduzir infraestrutura portátil para os scripts R.
5. Identificar o pipeline canônico de cada unidade.
6. Executar testes estruturais e compilar uma versão limpa quando a engine estiver disponível.

## Registro inicial de riscos

- Quatro blocos `minted` incompatíveis com a classe baseada em `listings`.
- Oito chaves BibTeX duplicadas.
- Conteúdo dos capítulos concentrado no arquivo principal.
- 103 scripts com `setwd()` e caminhos absolutos.
- 199 scripts com `rm(list = ls())`.
- 151 scripts com instalação automática de pacotes.
- TinyTeX usado anteriormente não está disponível no ambiente atual.

## Progresso registrado

### Marco 1 — Compilação e bibliografia

- Os quatro ambientes `minted` foram substituídos por ambientes baseados em `listings`.
- Foi criado um ambiente `shellcode` para comandos de terminal.
- As sete entradas bibliográficas redundantes foram removidas.
- ScenarioMIP recebeu a chave própria `oneill2016scenariomip`.
- A validação estática não encontrou chaves duplicadas nem citações ausentes.

### Marco 2 — Modularização candidata

- O manuscrito foi dividido mecanicamente em 21 módulos em `manuscrito/capitulos/`.
- A concatenação dos módulos reproduz exatamente o manuscrito monolítico.
- `apostila_sdm_r_modular.tex` é o candidato modular. Sua ativação depende de compilação e comparação visual.

### Marco 3 — Saneamento inicial dos scripts

- `rm(list = ls())` foi removido dos 199 scripts.
- `scripts/_bootstrap.R` centraliza raiz, caminhos, entradas, diretórios, dependências e seed.
- Foram identificados 28 candidatos a pipeline e várias sequências concorrentes.
- Nenhum script foi excluído; a escolha canônica será feita por unidade.

### Marco 4 — Migração das cadeias canônicas

- Foram identificados 119 scripts canônicos nas Unidades 1–17.
- Todos usam o bootstrap comum.
- Não permanecem `setwd()`, caminhos absolutos efetivos, instalações automáticas
  ou limpeza global de objetos nessas cadeias.
- Dependências opcionais são informadas sem modificar o ambiente do usuário.
- Etapas estocásticas detectadas possuem seed explícita.
- Pipelines paralelos continuam preservados e aguardam classificação editorial.
