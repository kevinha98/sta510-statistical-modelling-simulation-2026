# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Coursework for STA510 Statistical modelling and simulation (autumn 2026). Plain base-R scripts, no package/build system, no tests.

- `mandatoryN/` — one folder per graded assignment: `mandatoryN_2026.pdf` (the assignment), `mandatoryN_2026.md` (markdown extraction of that PDF — read this instead of the PDF; mandatory1 still has garbled inline math and keeps formula PNGs in `images/`, mandatory2's formulas are LaTeX checked against the PDF), `mandatoryN.R` (solution code), `mandatoryN_report.pdf` (theory answers/discussion; for mandatory2 built from `mandatory2_report.md`), `output/` (generated, committed).
- `R_examples/`, `Solutions/` (`R-code_setN.R` + `solution_setN.pdf`), `Lecture_Notes/` — course reference material. Reuse their methods and notation (inverse transform, acceptance-rejection, KDE, bootstrap, MC integration, importance sampling, MCMC, Markov) rather than inventing different approaches.

## Commands

```sh
cd mandatory1 && Rscript mandatory1.R       # run an assignment; writes to mandatory1/output/
Rscript -e "lintr::lint('mandatory1/mandatory1.R')"   # should report 0 lints
npx markdownlint-cli2 "**/*.md"             # should report 0 issues (rules in .markdownlint.json)
```

R 4.5.2 at `C:\Program Files\R\R-4.5.2`, not on PATH in plain PowerShell: call `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`. VS Code uses radian as the R terminal.

The mandatory2 report PDF is rendered outside the repo: `node build.js <report.md> <out.pdf>` in `~/.cache/sta510-report` (markdown-it + KaTeX, printed by headless Edge). Rebuild it after every edit to the report markdown, otherwise the committed PDF goes stale.

## Assignment rules (from the assignment PDFs)

- First line of every R file must be `rm(list=ls())`. The file must run cleanly top to bottom.
- Only problems marked **[R]** go in the R file; theory answers go in the report PDF. Comment each block with the subproblem it answers (`## Problem 1`, `# 1d(ii) ...`).
- Submissions are individual.

## Script template (follow `mandatory1/mandatory1.R` for new assignments)

The script header is designed to be plug-and-play on any machine; copy it for `mandatory2.R` etc.:

1. Create/prepend a writable `R_LIBS_USER`, set CRAN mirror, auto-install the few dependencies (mandatory1: only `MASS`).
2. `get_script_dir()` → `setwd()` so `output/` lands next to the script regardless of working directory (works for both `Rscript --file=` and `source()`).
3. `options(error=...)` closes open `sink()`s and graphics devices and exits non-zero when non-interactive.
4. `set.seed(510)`, `sink("output/console_log.txt", split = TRUE)`, open combined `pdf("output/mandatoryN_plots.pdf")`.
5. Every figure goes through `save_plot(idx, { ... })`, which writes `output/plot_%02d.png` and also draws into the combined PDF.
6. End with `dev.off(); sink()`.

`mandatory1/.lintr` disables `object_name_linter`, `commented_code_linter`, `infix_spaces_linter` (math-style names/spacing); reuse it for new assignment folders.

## Known non-issues

- Running via `source()` (VS Code "R Interactive") instead of `Rscript` drops the trailing `null device` / `1` lines from `console_log.txt` — not a regression, numbers are identical.
- `output/mandatoryN_plots.pdf` always shows as modified after a run because R's `pdf()` embeds a `/CreationDate`.
- In PowerShell, `Rscript ... 2>&1` reports the script's final `message()` as `NativeCommandError`; check `$LASTEXITCODE` instead.
- The report build prints Edge errors (`fallback_task_provider`, `GetUpdates ... ERR_IO_PENDING`); the PDF is still written correctly.
- The README embeds each assignment's `output/plot_*.png` and `report_preview/page_*.png`; update it when adding a new assignment, and re-render `report_preview/` (PyMuPDF `page.get_pixmap(dpi=110)`) whenever a report PDF is rebuilt.

## Commits

Commit messages are written in Norwegian: short imperative subject, then a longer narrative body explaining what was done and why.
