rm(list=ls()) # nolint: infix_spaces_linter. Exact form required by assignment.

## STA510 - Mandatory assignment 1, autumn 2026
## R-code for all problems marked [R]. Theory answers are in the accompanying
## report (mandatory1_report.pdf). Console output is echoed live and also
## written to output/console_log.txt; figures are saved both as individual
## PNGs and combined into output/mandatory1_plots.pdf.
##
## Plug-and-play: the block below auto-creates a writable personal library if
## the default one isn't writable, installs the one dependency (MASS) if it
## is missing, resolves output/ relative to this script's own folder (not the
## caller's working directory), and cleans up if the script errors partway.

## --- Portability setup ---------------------------------------------------

# Use a writable personal library if the default one needs admin rights
# (e.g. C:/Program Files/R/.../library on Windows).
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && !dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
if (nzchar(user_lib) && dir.exists(user_lib) && !(user_lib %in% .libPaths())) {
  .libPaths(c(user_lib, .libPaths()))
}
options(repos = c(CRAN = "https://cloud.r-project.org"))

# MASS is the only non-base dependency (Problem 3d, fitdistr()). It ships
# with almost every R install, but install it automatically if missing.
if (!requireNamespace("MASS", quietly = TRUE)) {
  message("Package 'MASS' not found - installing it now...")
  install.packages("MASS")
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Could not install/load 'MASS'. Please run ",
         "install.packages('MASS') manually and re-run this script.")
  }
}
library(MASS)

# Resolve output/ against this script's own folder, not the caller's cwd, so
# `Rscript mandatory1.R` works regardless of where it is invoked from.
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_flag) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_flag))))
  }
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]])))
  }
  getwd()  # interactive session: assume cwd is already correct
}
script_dir <- tryCatch(get_script_dir(), error = function(e) getwd())
if (dir.exists(script_dir) &&
      normalizePath(script_dir) != normalizePath(getwd())) {
  setwd(script_dir)
  message("Working directory set to script location: ", script_dir)
}

# If the script stops on an error, make sure no sink()/graphics device is
# left open (so a failed run never leaves the console silently redirected),
# and exit with a non-zero status when run non-interactively (Rscript) so
# automated checks can detect the failure instead of seeing a false "success".
options(error = function() {
  message("mandatory1.R stopped with an error - see message above.")
  while (sink.number() > 0) sink()
  while (dev.cur() > 1) dev.off()
  if (!interactive()) quit(status = 1, save = "no")
})

if (!dir.create("output", showWarnings = FALSE) && !dir.exists("output")) {
  stop("Could not create the 'output' directory - check write permissions ",
       "in: ", getwd())
}

## --- Analysis --------------------------------------------------------------

set.seed(510)  # fixed seed -> reproducible results, referenced in the report
sink("output/console_log.txt", split = TRUE)  # split: echo to console too

# Draws once into a standalone PNG (plot_<idx>.png), then re-draws the same
# (deterministic, RNG-free) plotting code into the already-open combined PDF
# device below. idx is passed explicitly (no <<-) to avoid mutable global state.
save_plot <- function(idx, expr) {
  fname <- sprintf("output/plot_%02d.png", idx)
  e <- substitute(expr)
  png(fname, width = 6, height = 5, units = "in", res = 150)
  eval.parent(e)
  dev.off()
  eval.parent(e)
}

pdf("output/mandatory1_plots.pdf", width = 6, height = 5)

## ---------------------------------------------------------------
## Problem 1
## ---------------------------------------------------------------

# pdf f(x;a) = c*(x-a)*(a+20-x) on [a, a+20], c = 3/4000 (see report 1a)
f_wind <- function(x, a) {
  ifelse(x >= a & x <= a + 20, (3 / 4000) * (x - a) * (a + 20 - x), 0)
}

# cdf F(x;a) on [a, a+20] (see report 1c)
F_wind <- function(x, a) {
  u <- pmin(pmax(x - a, 0), 20)
  (3 / 4000) * (10 * u^2 - u^3 / 3)
}

# 1d) Inverse cdf F^{-1}(p;a) via uniroot, used for inverse-transform sampling.
# Guards the boundary cases p<=0 / p>=1 directly, since uniroot() requires
# opposite-signed endpoints (probability-zero with runif(), but be safe).
Finv_wind <- function(p, a) {
  if (p <= 0) return(a)
  if (p >= 1) return(a + 20)
  uniroot(function(x) F_wind(x, a) - p, lower = a, upper = a + 20)$root
}

n <- 5000
a <- 5

U <- runif(n)
X_inv <- vapply(U, Finv_wind, numeric(1), a = a)

# 1d)i) histogram (Scott's rule) vs true density
save_plot(1, {
  hist(X_inv, breaks = "Scott", freq = FALSE,
       main = "Problem 1d(i): Inverse transform samples vs true density",
       xlab = "x", col = "grey85")
  curve(f_wind(x, a), add = TRUE, col = "red", lwd = 2, n = 500)
  legend("topright",
         legend = c("Simulated (histogram)", "True density f(x;a=5)"),
         fill = c("grey85", NA), border = c("black", NA),
         lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2), merge = TRUE)
})

# 1d)ii) ECDF vs true CDF
save_plot(2, {
  plot(ecdf(X_inv), main = "Problem 1d(ii): ECDF vs true CDF",
       xlab = "x", ylab = "F(x)", col = "black")
  curve(F_wind(x, a), add = TRUE, col = "red", lwd = 2, n = 500)
  legend("bottomright", legend = c("Empirical CDF", "True CDF F(x;a=5)"),
         col = c("black", "red"), lwd = 2)
})

# 1f) Acceptance-Rejection with uniform proposal g(x)=1/20 on [a,a+20]
M <- 1.5  # see report 1e)i): M = 20*max f(x;a) = 20*0.075 = 1.5

ar_sim_wind <- function(n, a, M) {
  samples <- numeric(n)
  n_accept <- 0
  n_proposals <- 0
  while (n_accept < n) {
    x <- a + 20 * runif(1)
    u <- runif(1)
    n_proposals <- n_proposals + 1
    if (u <= f_wind(x, a) / (M * (1 / 20))) {
      n_accept <- n_accept + 1
      samples[n_accept] <- x
    }
  }
  list(samples = samples, n_proposals = n_proposals)
}

ar_res <- ar_sim_wind(n, a, M)
X_ar <- ar_res$samples
emp_rate <- n / ar_res$n_proposals

cat(sprintf(
  "Problem 1f(ii): empirical rate = %.4f  |  theoretical rate = %.4f\n",
  emp_rate, 1 / M
))
cat(sprintf("Problem 1f(ii): proposals used = %d (expected ~ %d)\n",
            ar_res$n_proposals, round(n * M)))

# 1f)i) histogram of AR samples vs true density
save_plot(3, {
  hist(X_ar, breaks = "Scott", freq = FALSE,
       main = "Problem 1f(i): AR samples vs true density",
       xlab = "x", col = "grey85")
  curve(f_wind(x, a), add = TRUE, col = "red", lwd = 2, n = 500)
  legend("topright",
         legend = c("Simulated (histogram)", "True density f(x;a=5)"),
         fill = c("grey85", NA), border = c("black", NA),
         lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2), merge = TRUE)
})

## ---------------------------------------------------------------
## Problem 2
## ---------------------------------------------------------------

rain <- c(3.1, 7.2, 5.4, 12.0, 8.8, 4.3, 9.6, 6.1, 11.2, 2.9,
          7.5, 14.1, 5.0, 9.3, 6.8, 10.4, 3.7, 8.1, 13.5, 4.6,
          6.3, 11.8, 7.9, 5.5, 9.0, 12.7, 4.1, 8.4, 6.6, 10.1,
          3.4, 7.0, 5.8, 13.2, 9.7, 6.2, 11.5, 4.8, 8.9, 7.3)

n_rain <- length(rain)

# 2d)i) MLE of beta: beta_hat = xbar (see report 2a)
beta_hat <- mean(rain)

# 2d)ii) approximate 95% CI via CLT: beta_hat +/- 1.96*beta_hat/sqrt(n)
se_beta <- beta_hat / sqrt(n_rain)
ci_beta <- beta_hat + c(-1, 1) * 1.96 * se_beta

cat(sprintf("Problem 2d: beta_hat = %.3f mm (n = %d)\n", beta_hat, n_rain))
cat(sprintf("Problem 2d: approximate 95%% CI = (%.3f, %.3f) mm\n",
            ci_beta[1], ci_beta[2]))

## ---------------------------------------------------------------
## Problem 3
## ---------------------------------------------------------------

# 3a) ECDF of rain vs fitted Exp(beta_hat) CDF
save_plot(4, {
  plot(ecdf(rain), xlab = "rainfall (mm)", ylab = "F(x)", col = "black",
       main = "Problem 3a: ECDF of rain vs fitted Exp(beta_hat) CDF")
  curve(pexp(x, rate = 1 / beta_hat), add = TRUE, col = "red", lwd = 2, n = 500)
  legend("bottomright", legend = c("Empirical CDF", "Fitted Exp CDF"),
         col = c("black", "red"), lwd = 2)
})

# 3b) Histogram (Scott's rule) with fitted Exp density and default-bandwidth KDE
save_plot(5, {
  hist(rain, breaks = "Scott", freq = FALSE,
       main = "Problem 3b: Histogram of rain with fitted Exp density and KDE",
       xlab = "rainfall (mm)", col = "grey85")
  curve(dexp(x, rate = 1 / beta_hat), add = TRUE, col = "red", lwd = 2, n = 500)
  lines(density(rain), col = "blue", lwd = 2)
  legend("topright",
         legend = c("Fitted Exp(beta_hat) density", "KDE (default bandwidth)"),
         col = c("red", "blue"), lwd = 2)
})

# 3d) Fit a Gamma model via fitdistr() and compare KDE bandwidths
# (MASS was loaded, installing it first if needed, in the setup section above)
fit_gamma <- tryCatch(
  fitdistr(rain, "gamma"),
  error = function(e) {
    stop("MASS::fitdistr() failed to fit a Gamma to `rain`: ",
         conditionMessage(e))
  }
)
shape_hat <- fit_gamma$estimate["shape"]
rate_hat <- fit_gamma$estimate["rate"]

cat(sprintf(
  "Problem 3d: fitted Gamma shape_hat = %.4f, rate_hat = %.4f (mean = %.3f)\n",
  shape_hat, rate_hat, shape_hat / rate_hat
))

bw_default <- bw.nrd0(rain)
bw_small <- bw_default / 5   # clearly too small (undersmoothed)
bw_large <- bw_default * 4   # clearly too large (oversmoothed)

cat(sprintf(
  "Problem 3d: bandwidths used - small = %.3f, default = %.3f, large = %.3f\n",
  bw_small, bw_default, bw_large
))

save_plot(6, {
  hist(rain, breaks = "Scott", freq = FALSE,
       main = "Problem 3d: Fitted Gamma density and KDE at three bandwidths",
       xlab = "rainfall (mm)", col = "grey85")
  curve(dgamma(x, shape = shape_hat, rate = rate_hat),
        add = TRUE, col = "red", lwd = 2, n = 500)
  lines(density(rain, bw = bw_small), col = "blue", lwd = 2, lty = 2)
  lines(density(rain, bw = bw_default), col = "darkgreen", lwd = 2)
  lines(density(rain, bw = bw_large), col = "purple", lwd = 2, lty = 3)
  legend("topright",
         legend = c("Fitted Gamma density",
                    sprintf("KDE, bw = %.2f (too small)", bw_small),
                    sprintf("KDE, bw = %.2f (default)", bw_default),
                    sprintf("KDE, bw = %.2f (too large)", bw_large)),
         col = c("red", "blue", "darkgreen", "purple"),
         lwd = 2, lty = c(1, 2, 1, 3))
})

## ---------------------------------------------------------------
## Problem 4
## ---------------------------------------------------------------

n4 <- 5000
A <- rnorm(n4, mean = 400, sd = sqrt(900))
B <- rexp(n4, rate = 1 / 250)
C <- runif(n4, 50, 150)
D <- sample(c(80, 0), n4, replace = TRUE, prob = c(0.3, 0.7))
Y <- A + B + C + D

mean_Y <- mean(Y)
sd_Y <- sd(Y)
p_Y_gt_1050 <- mean(Y > 1050)

cat(sprintf(
  "Problem 4b(ii): simulated mean(Y) = %.2f, simulated sd(Y) = %.2f\n",
  mean_Y, sd_Y
))
cat(sprintf("Problem 4b(iii): estimated P(Y > 1050) = %.4f\n", p_Y_gt_1050))

# 4b)i) histogram of simulated Y
save_plot(7, {
  hist(Y, breaks = "Scott", freq = FALSE,
       main = "Problem 4b(i): Simulated distribution of Y = A+B+C+D",
       xlab = "Total annual precipitation Y (mm)", col = "grey85")
})

dev.off()  # close the combined mandatory1_plots.pdf device
sink()

message("Done. See output/console_log.txt, output/plot_01..07.png and ",
        "output/mandatory1_plots.pdf in: ", getwd())
