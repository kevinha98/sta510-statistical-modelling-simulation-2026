---
source: mandatory2_2026.pdf
total_pages: 3
extracted_at: 2026-10-09T08:38:56.865744
---

# Mandatory assignment 2

_STA510 Statistical modelling and simulation, autumn 2026._

Deadline: Saturday October 18th at 23:59 (Norwegian time).

Read carefully through the information about the mandatory assignments on Canvas. Notice in particular that the assignments should be solved individually.

Hand in on Canvas. Submissions should be of **either** of the following types

- Submit two files: One pdf-file with a report containing the answers to the theory questions, and one R-file including the R-code.

- Submit two files: One R markdown (Rmd) file containing both theory answers and R-code, and a pdf-file with the output you obtain when running (knitting) your R markdown file. See tutorial to get started.

The first line of R-code should be: `rm(list=ls())` . Check that the Rmd/R-code file runs before you submit it. Use comments in the R-code to clearly identify which question each part of the R-code belongs to. Also try to add some comments to explain important parts of the code. The file ending of the R-code file should be .Rmd, .R or .r. The report can be handwritten and scanned to pdf-file, or written in your choice of text editor and converted to pdf. Cite the sources you use.

Problems marked with an [R] should be solved in R, the others are theory questions. Each of the subproblems, e.g., points 1a), 1b), 2a), and so on, is given the same weight.

## **Problem 1:**

The value of the integral

$$
I = \int_0^2 \frac{e^{-t/2}}{1+t^2}\,dt
$$

should be estimated using Monte Carlo integration. Let $f(t) = \frac{e^{-t/2}}{1+t^2}$.

- a)[R] i) Implement a crude Monte Carlo estimator $\hat{I}_{CMC}$ to estimate $I$ based on $n = 10000$ independent uniformly distributed random variables $U_i \sim U[0,2]$. Report $\hat{I}_{CMC}$.

   ii) Plot $f(t)$ for $t \in [0,2]$.

- b) Is $f(t)$ monotone on $[0,2]$? Show analytically that $f'(t) < 0$ for all $t \in [0,2]$. Describe how we can estimate $I$ using antithetic random variables and why this approach is reasonable?

- c)[R] Implement a Monte Carlo estimator $\hat{I}_{AT}$ of $I$ using antithetic random variables based on $n/2 = 5000$ independent uniform random variables. Report $\hat{I}_{AT}$.

Now we try to improve the estimate of $I$ by importance sampling. Consider the truncated exponential proposal density:

$$
g(t) = \frac{e^{-t/2}}{2(1-e^{-1})}, \quad 0 \le t \le 2.
$$

- d)[R] Plot $f(t)$ and $g(t)$ for $t \in [0,2]$ in the same plot using different colours. Does $g(t)$ resemble $f(t)$ in shape? Is it a reasonable importance function? Why?

- e) Show that the inverse CDF of $g(t)$ is

   $$
   G^{-1}(U) = -2\log\bigl(1 - U(1-e^{-1})\bigr), \quad U \sim U(0,1).
   $$

- f)[R] Using $G^{-1}$ from e), implement importance sampling to estimate $I$ by $\hat{I}_{IS}$ based on $n = 10000$ random variables generated from $g(t)$. Report $\hat{I}_{IS}$.

- g)[R] Generate 1000 replications of $\hat{I}_{CMC}$, $\hat{I}_{AT}$ and $\hat{I}_{IS}$. Compare their means and standard deviations in a table. Comment on the variance reduction achieved by each method.

## **Problem 2:**

Wind turbines on the Norwegian coast require maintenance after each mechanical failure. Let $\{N(t) : t \ge 0\}$ be a renewal process counting the number of failures by time $t$ (in months). The times between successive failures $T_1, T_2, \ldots$ are iid with a Weibull distribution with shape parameter $k = 2$ and scale parameter $\lambda = 6$ months, with pdf:

$$
f(t) = \frac{k}{\lambda}\left(\frac{t}{\lambda}\right)^{k-1}\exp\left(-\left(\frac{t}{\lambda}\right)^{k}\right), \quad t > 0.
$$

Note: This is the standard parametrization of the Weibull distribution in R. Given the paramterization in the tablesformulas-sheet, we have $\alpha = \frac{1}{\lambda^k}$ and $\beta = k$.

- a) Find the expected time between failures $\mathrm{E}(T)$ and the standard deviation $\mathrm{SD}(T)$ given $k = 2$ and $\lambda = 6$. _Hint: See tablesformulas-sheet._

- b)[R] Simulate one realisation of the renewal process on $[0,60]$ months and plot $N(t)$. How many failures occurred?

- c)[R] Simulate $B = 5000$ realisations of $N(60)$ and estimate:

  - i) The expected number of failures $\mathrm{E}[N(60)]$ in 60 months.

  - ii) The probability $P(N(60) \ge 15)$ that at least 15 failures occur in 60 months.

Now suppose the failure rate is not constant but follows a seasonal pattern. The number of failures per month is modelled by a nonhomogeneous Poisson process (NHPP) with intensity:

$$
\lambda(t) = 3 + 2\cos\left(\frac{\pi t}{6}\right), \quad 0 \le t \le 36,
$$

where $t$ is measured in months with $t = 0$ corresponding to January.

- d) Show that the cumulative intensity function is

   $$
   \Lambda(t) = \int_0^t \lambda(u)\,du = 3t + \frac{12}{\pi}\sin\left(\frac{\pi t}{6}\right).
   $$

   What is the expected total number of failures over 3 years? What is the distribution of $N(36)$?

- e)[R] Plot $\lambda(t)$ over $[0,36]$. In which months is the failure rate highest and lowest? Simulate one realisation of the NHPP over $[0,36]$ months using the thinning method and visualise it.

- f)[R] Simulate $B = 5000$ realisations of the NHPP over $[0,36]$ and:

  - i) Estimate $\mathrm{E}[N(36)]$ empirically and compare with d).

  - ii) Estimate the probability that more than 120 failures occur over 3 years.

  - iii) Estimate the expected number of failures in the first quarter ($t \in [0,3]$).

## **Problem 3:**

We return to the wet-day rainfall data from Mandatory Assignment 1:

```r
rain <- c(3.1,  7.2,  5.4, 12.0,  8.8,  4.3,  9.6,  6.1, 11.2,  2.9,
          7.5, 14.1,  5.0,  9.3,  6.8, 10.4,  3.7,  8.1, 13.5,  4.6,
          6.3, 11.8,  7.9,  5.5,  9.0, 12.7,  4.1,  8.4,  6.6, 10.1,
          3.4,  7.0,  5.8, 13.2,  9.7,  6.2, 11.5,  4.8,  8.9,  7.3)
```

- a)[R] Using $B = 2000$ non-parametric bootstrap resamples, estimate the bootstrap standard deviation of the sample mean $\bar{X}$ and construct a 95% percentile bootstrap confidence interval for the population mean $\mu = \mathrm{E}(X)$. Compare the bootstrap confidence interval with the CLT-based interval from M1 Problem 2d).

- b)[R] Repeat a) for the sample median instead of the sample mean. Comment on how the bootstrap CI for the median compares to that for the mean.
