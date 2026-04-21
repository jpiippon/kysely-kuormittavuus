# AGENTS.md

## Project overview

This repository contains a small R survey analysis project about perceived childcare burden during the first 3 years of a child's life.

Project structure:
- raw data in `data/raw/`
- analysis scripts in `scripts/`
- outputs in `output/`

The main analysis pipeline is run from:
- `scripts/run_pipeline.R`

This project does **not** require Quarto validation.

---

## Core working principles

When editing this repository:

1. Keep the project modular.
2. Keep the folder name `scripts` unchanged.
3. Prefer small, targeted changes over broad rewrites.
4. Do not redesign the whole project structure unless explicitly asked.
5. Internal code names should be simple and analysis-friendly.
6. All reader-facing output should use proper Finnish with correct characters.

---

## Language conventions

### Internal code
- Use simple, safe variable names in ASCII where practical.
- Prefer short and readable internal names.
- Avoid unnecessarily complicated naming.

### Public-facing output
Use proper Finnish in:
- plot titles
- subtitles
- axis labels
- captions
- exported presentation tables

Do not leave mojibake or ASCII-only Finnish in visible output.

---

## R style conventions

- Use tidyverse
- Prefer native pipe `|>`
- Avoid loops when a tidyverse / functional approach is clearer
- Keep functions focused and readable
- Use short, useful comments only
- Prefer clear script organization over cleverness

---

## Project structure

Keep and use this structure:

- `scripts/00_helpers.R`
- `scripts/01_load_clean.R`
- `scripts/02_reshape_long.R`
- `scripts/03_summary_table.R`
- `scripts/04_figures.R`
- `scripts/run_pipeline.R`

Do not rename the `scripts` folder.

If refactoring:
- `scripts/run_pipeline.R` should remain the main orchestrator
- avoid unnecessary cross-sourcing between scripts
- prefer a clean dependency flow:
  load/clean -> reshape -> summary -> figures

---

## Data handling rules

- Treat the Google Forms CSV as UTF-8 input
- Trim whitespace from raw column names after import
- Use stable internal names after cleaning
- Do not impute missing burden values unless explicitly requested
- Later age intervals may be genuinely missing because the child had not yet reached that age or the respondent could not answer
- Always report non-missing `n` by age interval in summaries where relevant

---

## Validation workflow

Do not consider a task complete until the code has been run successfully.

### Required validation command

Run this from the project root:

```powershell
Rscript .\scripts\run_pipeline.R