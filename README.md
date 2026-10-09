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

### Report preview

All 11 pages of [mandatory1/mandatory1_report.pdf](mandatory1/mandatory1_report.pdf).
Click a page to open the PDF.

| | | | |
| --- | --- | --- | --- |
| [![Report page 1](mandatory1/report_preview/page_01.png)](mandatory1/mandatory1_report.pdf) | [![Report page 2](mandatory1/report_preview/page_02.png)](mandatory1/mandatory1_report.pdf) | [![Report page 3](mandatory1/report_preview/page_03.png)](mandatory1/mandatory1_report.pdf) | [![Report page 4](mandatory1/report_preview/page_04.png)](mandatory1/mandatory1_report.pdf) |
| [![Report page 5](mandatory1/report_preview/page_05.png)](mandatory1/mandatory1_report.pdf) | [![Report page 6](mandatory1/report_preview/page_06.png)](mandatory1/mandatory1_report.pdf) | [![Report page 7](mandatory1/report_preview/page_07.png)](mandatory1/mandatory1_report.pdf) | [![Report page 8](mandatory1/report_preview/page_08.png)](mandatory1/mandatory1_report.pdf) |
| [![Report page 9](mandatory1/report_preview/page_09.png)](mandatory1/mandatory1_report.pdf) | [![Report page 10](mandatory1/report_preview/page_10.png)](mandatory1/mandatory1_report.pdf) | [![Report page 11](mandatory1/report_preview/page_11.png)](mandatory1/mandatory1_report.pdf) | |

## Mandatory assignment 2

Monte Carlo integration with variance reduction (crude MC, antithetic
variables, importance sampling with a truncated exponential), a Weibull
renewal process and a seasonal non-homogeneous Poisson process simulated by
thinning, and non-parametric bootstrap CIs for the mean and median of the M1
rainfall data. Source: [mandatory2/mandatory2.R](mandatory2/mandatory2.R)
(run with `Rscript mandatory2.R`, seed `510`), full report:
[mandatory2/mandatory2_report.pdf](mandatory2/mandatory2_report.pdf).

### How to run

Base R only, so there is nothing to install:

```sh
Rscript mandatory2.R
```

Same plug-and-play setup as mandatory 1: `output/` is resolved relative to the
script, and open sinks/graphics devices are cleaned up on error. Output
(console log, per-figure PNGs, combined PDF) is written to
`mandatory2/output/`.

### Figures

| | |
| --- | --- |
| ![Problem 1a(ii): the integrand f(t) on 0 to 2](mandatory2/output/plot_01.png) | ![Problem 1d: f(t) and the importance function g(t)](mandatory2/output/plot_02.png) |
| Problem 1a(ii): integrand f(t) = e^(−t/2)/(1+t²) on [0, 2] | Problem 1d: f(t) and importance function g(t) |
| ![Problem 2b: one realisation of the Weibull renewal process](mandatory2/output/plot_03.png) | ![Problem 2e: seasonal failure intensity](mandatory2/output/plot_04.png) |
| Problem 2b: one realisation of the Weibull(2, 6) renewal process | Problem 2e: seasonal intensity λ(t) = 3 + 2cos(πt/6) |
| ![Problem 2e: one NHPP realisation simulated by thinning](mandatory2/output/plot_05.png) | ![Problem 3: bootstrap distributions of the sample mean and median](mandatory2/output/plot_06.png) |
| Problem 2e: one NHPP realisation (thinning) vs Λ(t) = E[N(t)] | Problem 3a/b: bootstrap means and medians with 95% percentile limits |

### Report preview

All 5 pages of [mandatory2/mandatory2_report.pdf](mandatory2/mandatory2_report.pdf).
Click a page to open the PDF.

| | | | |
| --- | --- | --- | --- |
| [![Report page 1](mandatory2/report_preview/page_01.png)](mandatory2/mandatory2_report.pdf) | [![Report page 2](mandatory2/report_preview/page_02.png)](mandatory2/mandatory2_report.pdf) | [![Report page 3](mandatory2/report_preview/page_03.png)](mandatory2/mandatory2_report.pdf) | [![Report page 4](mandatory2/report_preview/page_04.png)](mandatory2/mandatory2_report.pdf) |
| [![Report page 5](mandatory2/report_preview/page_05.png)](mandatory2/mandatory2_report.pdf) | | | |
