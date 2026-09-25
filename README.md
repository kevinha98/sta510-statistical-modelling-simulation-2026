# sta510-statistical-modelling-simulation-2026

Course material and assignments for STA510 Statistical modelling and simulation, autumn 2026.

## Mandatory assignment 1

Wind-speed simulation (inverse transform + acceptance-rejection), exponential
rainfall MLE/CI, ECDF/KDE/gamma model fitting, and Monte Carlo simulation of a
sum of independent random variables. Source: [mandatory1/mandatory1.R](mandatory1/mandatory1.R)
(run with `Rscript mandatory1.R`, seed `510`), full report:
[mandatory1/mandatory1_report.pdf](mandatory1/mandatory1_report.pdf).

### How to run

Requires only a base R install (any recent version) — no manual setup needed:

```sh
Rscript mandatory1.R
```

The script is plug-and-play: it auto-creates a writable personal R library if
the default one isn't writable, installs its one dependency (`MASS`)
automatically if missing, resolves `output/` relative to its own file location
so it works from any working directory, and cleans up open sinks/graphics
devices (and exits with a non-zero status) if it hits an error partway
through. Output (console log, per-figure PNGs, combined PDF) is written to
`mandatory1/output/`.

### Figures

| | |
| --- | --- |
| ![Problem 1d(i): inverse transform histogram vs true density](mandatory1/output/plot_01.png) | ![Problem 1d(ii): ECDF vs true CDF](mandatory1/output/plot_02.png) |
| Problem 1d(i): inverse-transform samples vs true density | Problem 1d(ii): ECDF vs true CDF |
| ![Problem 1f(i): acceptance-rejection histogram vs true density](mandatory1/output/plot_03.png) | ![Problem 3a: ECDF of rain vs fitted exponential CDF](mandatory1/output/plot_04.png) |
| Problem 1f(i): acceptance-rejection samples vs true density | Problem 3a: ECDF of rain vs fitted Exp(β̂) CDF |
| ![Problem 3b: rain histogram with fitted exponential density and KDE](mandatory1/output/plot_05.png) | ![Problem 3d: rain histogram with fitted gamma density and three KDE bandwidths](mandatory1/output/plot_06.png) |
| Problem 3b: rain histogram, fitted Exp density and KDE | Problem 3d: fitted Gamma density and KDE at three bandwidths |
| ![Problem 4b(i): simulated distribution of total annual precipitation Y](mandatory1/output/plot_07.png) | |
| Problem 4b(i): simulated distribution of Y = A+B+C+D | |
