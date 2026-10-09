rm(list=ls())

# STA510 mandatory assignment 2 - code for the [R] parts only, theory answers
# and discussion are in mandatory2_report.pdf
# base R only, no extra packages needed

# so output/ ends up next to this script even if run from another folder
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
  getwd()
}
script_dir <- tryCatch(get_script_dir(), error = function(e) getwd())
if (dir.exists(script_dir) &&
      normalizePath(script_dir) != normalizePath(getwd())) {
  setwd(script_dir)
}

# don't leave sink()/a graphics device hanging open if something breaks
options(error = function() {
  while (sink.number() > 0) sink()
  while (dev.cur() > 1) dev.off()
  if (!interactive()) quit(status = 1, save = "no")
})

if (!dir.create("output", showWarnings = FALSE) && !dir.exists("output")) {
  stop("couldn't create output/ - check folder permissions")
}

set.seed(510)
sink("output/console_log.txt", split = TRUE)

# saves each plot as its own png, and also drops it into the combined pdf
save_plot <- function(idx, expr) {
  fname <- sprintf("output/plot_%02d.png", idx)
  e <- substitute(expr)
  png(fname, width = 6, height = 5, units = "in", res = 150)
  eval.parent(e)
  dev.off()
  eval.parent(e)
}

pdf("output/mandatory2_plots.pdf", width = 6, height = 5)

## Problem 1

f <- function(t) exp(-t / 2) / (1 + t^2)
n <- 10000

# reference value from numerical integration, only used for comparison
I_true <- integrate(f, 0, 2)$value
cat(sprintf("Problem 1: reference value integrate() = %.6f\n", I_true))

# 1a(i) crude MC: I = 2 * E[f(U)], U ~ U[0,2]
I_cmc <- function(n) {
  u <- runif(n, 0, 2)
  2 * mean(f(u))
}
cat(sprintf("Problem 1a(i): I_CMC = %.6f\n", I_cmc(n)))

# 1a(ii)
save_plot(1, {
  curve(f(x), from = 0, to = 2, lwd = 2, n = 500, xlab = "t", ylab = "f(t)",
        main = "Problem 1a(ii): f(t) = exp(-t/2)/(1+t^2)")
})

# 1c antithetic: pair each U with 2-U, f is monotone so f(U) and f(2-U)
# are negatively correlated (see 1b)
I_at <- function(n) {
  u <- runif(n / 2, 0, 2)
  2 * mean((f(u) + f(2 - u)) / 2)
}
cat(sprintf("Problem 1c: I_AT = %.6f\n", I_at(n)))

# importance function g = truncated exponential on [0,2] (see 1d/1e)
g <- function(t) exp(-t / 2) / (2 * (1 - exp(-1)))
G_inv <- function(u) -2 * log(1 - u * (1 - exp(-1)))

# 1d
save_plot(2, {
  curve(f(x), from = 0, to = 2, lwd = 2, n = 500, col = "black",
        ylim = c(0, 1.1), xlab = "t", ylab = "density",
        main = "Problem 1d: f(t) and importance function g(t)")
  curve(g(x), from = 0, to = 2, lwd = 2, n = 500, col = "red", add = TRUE)
  legend("topright", legend = c("f(t)", "g(t)"), col = c("black", "red"),
         lwd = 2)
})

# 1f importance sampling: T = G^-1(U) ~ g, I = E_g[f(T)/g(T)]
I_is <- function(n) {
  t <- G_inv(runif(n))
  mean(f(t) / g(t))
}
cat(sprintf("Problem 1f: I_IS = %.6f\n", I_is(n)))

# 1g 1000 replications of each estimator
n_rep <- 1000
reps <- cbind(CMC = replicate(n_rep, I_cmc(n)),
              AT = replicate(n_rep, I_at(n)),
              IS = replicate(n_rep, I_is(n)))
sds <- apply(reps, 2, sd)
tab1g <- data.frame(mean = colMeans(reps),
                    sd = sds,
                    var_reduction_pct = 100 * (1 - sds^2 / sds["CMC"]^2))
cat("Problem 1g: 1000 replications of each estimator\n")
print(round(tab1g, 6))

## Problem 2

k <- 2
lambda_w <- 6
t_end <- 60

# renewal process: keep adding Weibull interarrival times until we pass t_end
sim_renewal <- function(t_end) {
  times <- numeric(0)
  s <- rweibull(1, shape = k, scale = lambda_w)
  while (s <= t_end) {
    times <- c(times, s)
    s <- s + rweibull(1, shape = k, scale = lambda_w)
  }
  times
}

# 2a check of the theory values
ET <- lambda_w * gamma(1 + 1 / k)
SDT <- lambda_w * sqrt(gamma(1 + 2 / k) - gamma(1 + 1 / k)^2)
cat(sprintf("Problem 2a: E(T) = %.4f, SD(T) = %.4f months\n", ET, SDT))

# 2b one realisation on [0,60]
fail_times <- sim_renewal(t_end)
cat(sprintf("Problem 2b: number of failures in [0,60] = %d\n",
            length(fail_times)))
save_plot(3, {
  plot(stepfun(fail_times, 0:length(fail_times)), do.points = FALSE,
       xlim = c(0, t_end), lwd = 2, xlab = "t (months)", ylab = "N(t)",
       main = "Problem 2b: One realisation of the renewal process")
})

# 2c B realisations of N(60)
B <- 5000
N60 <- replicate(B, length(sim_renewal(t_end)))
cat(sprintf("Problem 2c(i): estimated E[N(60)] = %.4f (t/E(T) = %.4f)\n",
            mean(N60), t_end / ET))
cat(sprintf("Problem 2c(ii): estimated P(N(60) >= 15) = %.4f\n",
            mean(N60 >= 15)))

# NHPP with seasonal intensity, t = 0 is January
lambda_t <- function(t) 3 + 2 * cos(pi * t / 6)
Lambda_t <- function(t) 3 * t + 12 / pi * sin(pi * t / 6)
t_max <- 36
lambda_max <- 5   # max of lambda(t), cos = 1

cat(sprintf("Problem 2d: Lambda(36) = %.4f\n", Lambda_t(t_max)))

# thinning: simulate HPP with rate lambda_max, keep each point with
# probability lambda(t)/lambda_max
sim_nhpp <- function(t_max) {
  n_hpp <- rpois(1, lambda_max * t_max)
  t_hpp <- sort(runif(n_hpp, 0, t_max))
  t_hpp[runif(n_hpp) <= lambda_t(t_hpp) / lambda_max]
}

# 2e
save_plot(4, {
  curve(lambda_t(x), from = 0, to = t_max, lwd = 2, n = 500,
        xlab = "t (months, 0 = start of January)", ylab = "lambda(t)",
        main = "Problem 2e: Seasonal failure intensity", xaxt = "n")
  axis(1, at = seq(0, t_max, by = 6))
  abline(h = 3, lty = 3)
})

nhpp_times <- sim_nhpp(t_max)
cat(sprintf("Problem 2e: failures in one NHPP realisation on [0,36] = %d\n",
            length(nhpp_times)))
save_plot(5, {
  plot(stepfun(nhpp_times, 0:length(nhpp_times)), do.points = FALSE,
       xlim = c(0, t_max), lwd = 2, xlab = "t (months)", ylab = "N(t)",
       main = "Problem 2e: One NHPP realisation (thinning)", xaxt = "n")
  axis(1, at = seq(0, t_max, by = 6))
  curve(Lambda_t(x), from = 0, to = t_max, add = TRUE, col = "red",
        lwd = 2, lty = 2)
  rug(nhpp_times, col = "grey40")
  legend("topleft", legend = c("simulated N(t)", "Lambda(t) = E[N(t)]"),
         col = c("black", "red"), lwd = 2, lty = c(1, 2))
})

# 2f B realisations over [0,36]
nhpp_sims <- replicate(B, sim_nhpp(t_max), simplify = FALSE)
N36 <- sapply(nhpp_sims, length)
N3 <- sapply(nhpp_sims, function(x) sum(x <= 3))
cat(sprintf("Problem 2f(i): estimated E[N(36)] = %.4f (theory %.4f)\n",
            mean(N36), Lambda_t(t_max)))
cat(sprintf("Problem 2f(ii): estimated P(N(36) > 120) = %.4f (Poisson: %.4f)\n",
            mean(N36 > 120), 1 - ppois(120, Lambda_t(t_max))))
cat(sprintf("Problem 2f(iii): estimated E[N(3)] = %.4f (theory %.4f)\n",
            mean(N3), Lambda_t(3)))

## Problem 3

rain <- c(3.1, 7.2, 5.4, 12.0, 8.8, 4.3, 9.6, 6.1, 11.2, 2.9,
          7.5, 14.1, 5.0, 9.3, 6.8, 10.4, 3.7, 8.1, 13.5, 4.6,
          6.3, 11.8, 7.9, 5.5, 9.0, 12.7, 4.1, 8.4, 6.6, 10.1,
          3.4, 7.0, 5.8, 13.2, 9.7, 6.2, 11.5, 4.8, 8.9, 7.3)
n_rain <- length(rain)
B3 <- 2000

# resample the data with replacement and recompute the mean and median
mean_boot <- numeric(B3)
median_boot <- numeric(B3)
for (i in 1:B3) {
  x_star <- sample(rain, size = n_rain, replace = TRUE)
  mean_boot[i] <- mean(x_star)
  median_boot[i] <- median(x_star)
}

# 3a mean - percentile CI vs CLT interval from M1 2d (exponential model,
# se = beta_hat/sqrt(n))
ci_mean <- quantile(mean_boot, c(0.025, 0.975))
ci_clt <- mean(rain) + c(-1, 1) * 1.96 * mean(rain) / sqrt(n_rain)
cat(sprintf("Problem 3a: xbar = %.3f, bootstrap sd = %.4f\n",
            mean(rain), sd(mean_boot)))
cat(sprintf("Problem 3a: 95%% percentile bootstrap CI = (%.3f, %.3f)\n",
            ci_mean[1], ci_mean[2]))
cat(sprintf("Problem 3a: M1 2d CLT CI (exp. model) = (%.3f, %.3f)\n",
            ci_clt[1], ci_clt[2]))
cat(sprintf("Problem 3a: s/sqrt(n) = %.4f (for comparison)\n",
            sd(rain) / sqrt(n_rain)))

# 3b median
ci_median <- quantile(median_boot, c(0.025, 0.975))
cat(sprintf("Problem 3b: sample median = %.3f, bootstrap sd = %.4f\n",
            median(rain), sd(median_boot)))
cat(sprintf("Problem 3b: 95%% percentile bootstrap CI = (%.3f, %.3f)\n",
            ci_median[1], ci_median[2]))

save_plot(6, {
  op <- par(mfrow = c(1, 2))
  hist(mean_boot, breaks = 40, freq = FALSE, col = "grey85",
       main = "3a: bootstrap means", xlab = "mean of resample")
  abline(v = ci_mean, col = "red", lwd = 2, lty = 2)
  hist(median_boot, breaks = 40, freq = FALSE, col = "grey85",
       main = "3b: bootstrap medians", xlab = "median of resample")
  abline(v = ci_median, col = "red", lwd = 2, lty = 2)
  par(op)
})

dev.off()
sink()

message("done - see output/console_log.txt and output/mandatory2_plots.pdf")
