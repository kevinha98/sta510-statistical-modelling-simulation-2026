# STA510 Mandatory assignment 2 — report

All numbers below come from `mandatory2.R` (seed 510, base R). Figures are in
`output/plot_01.png` … `plot_06.png` (also combined in `output/mandatory2_plots.pdf`).

## Problem 1

**a)** Since $U\sim U[0,2]$ has density $1/2$, $I = 2\,\mathrm{E}[f(U)]$, so
$\hat I_{CMC} = \frac{2}{n}\sum_{i=1}^n f(U_i)$.
With $n=10000$: $\hat I_{CMC} = 0.7899$. (Numerical reference from `integrate()`: $I = 0.79547$.)
Plot of $f$: Figure 1.

**b)** Yes, $f$ is monotone (decreasing) on $[0,2]$. By the product rule

$$
f'(t) = e^{-t/2}\left[-\frac{1}{2(1+t^2)} - \frac{2t}{(1+t^2)^2}\right]
      = -\,\frac{e^{-t/2}\,(t^2 + 4t + 1)}{2(1+t^2)^2}.
$$

For $t\in[0,2]$ we have $e^{-t/2}>0$, $(1+t^2)^2>0$ and $t^2+4t+1\ge 1>0$, so $f'(t)<0$.

*Antithetic estimator:* if $U\sim U[0,2]$ then $2-U\sim U[0,2]$ as well, so $f(U)$ and $f(2-U)$ both have mean $I/2$. Generate $U_1,\dots,U_{n/2}$ and use

$$
\hat I_{AT} = \frac{2}{n/2}\sum_{i=1}^{n/2} \frac{f(U_i) + f(2-U_i)}{2}.
$$

This is unbiased and uses the same number ($n$) of function evaluations as the crude estimator. With $Y_1=f(U)$, $Y_2=f(2-U)$,
$\mathrm{Var}\big(\tfrac{Y_1+Y_2}{2}\big) = \tfrac{\sigma^2}{2}(1+\rho)$ where $\rho=\mathrm{Corr}(Y_1,Y_2)$.
Because $f$ is monotone, $f(U)$ and $f(2-U)$ are monotone functions of $U$ in opposite directions: a large value of one goes with a small value of the other, so $\rho<0$. The variance is therefore smaller than with $n$ independent draws ($\rho=0$). That is why the approach is reasonable here.

**c)** With $n/2=5000$ uniforms: $\hat I_{AT} = 0.7971$.

**d)** See Figure 2. Both $f$ and $g$ are positive and decreasing on $[0,2]$, so $g$ has roughly the same shape as $f$. $g$ puts the most mass where $f$ is largest (near $t=0$), which makes it a reasonable importance function. Also, $g>0$ wherever $f>0$, and the ratio

$$
\frac{f(t)}{g(t)} = \frac{2(1-e^{-1})}{1+t^2}\in[0.253,\,1.264]
$$

is bounded, so the importance weights cannot blow up. It is not perfect: $f$ decays faster than $g$ because of the extra factor $1/(1+t^2)$, so $g$ puts too much mass near $t=2$ and the ratio is not constant. Expect a variance reduction, but not an extreme one.

**e)** For $0\le t\le 2$,

$$
G(t) = \int_0^t \frac{e^{-s/2}}{2(1-e^{-1})}\,ds = \frac{\big[-2e^{-s/2}\big]_0^t}{2(1-e^{-1})} = \frac{1-e^{-t/2}}{1-e^{-1}}.
$$

(Check: $G(0)=0$, $G(2)=1$.) Solving $U=G(t)$:
$1-e^{-t/2} = U(1-e^{-1}) \Rightarrow e^{-t/2} = 1-U(1-e^{-1}) \Rightarrow t = -2\log\big(1-U(1-e^{-1})\big)$.
So $G^{-1}(U) = -2\log\big(1-U(1-e^{-1})\big)$, and by the inverse transform method $G^{-1}(U)\sim g$ when $U\sim U(0,1)$.

**f)** With $T_i = G^{-1}(U_i)$, $\hat I_{IS} = \frac1n\sum_{i=1}^n f(T_i)/g(T_i)$. With $n=10000$: $\hat I_{IS} = 0.7977$.

**g)** 1000 replications of each estimator (each based on $n=10000$ function evaluations):

| Estimator | Mean | SD | Variance reduction vs CMC |
|---|---|---|---|
| CMC | 0.795487 | 0.005666 | — |
| Antithetic | 0.795568 | 0.002097 | 86.3 % |
| Importance sampling | 0.795327 | 0.003204 | 68.0 % |

All three means agree with $I=0.79547$ to about 4 decimals, consistent with all three estimators being unbiased. Both methods reduce the variance a lot. The antithetic estimator does best (variance about 7 times smaller than crude MC): $f$ is monotone and fairly smooth on $[0,2]$, so $f(U)$ and $f(2-U)$ are strongly negatively correlated. Importance sampling cuts the variance about 3 times. It helps because $g$ follows the decreasing shape of $f$, but $f/g$ still varies by a factor of about 5 over $[0,2]$ (see d), so some variance remains.

## Problem 2

**a)** For a Weibull($k$, $\lambda$), $\mathrm{E}(T)=\lambda\,\Gamma(1+1/k)$ and $\mathrm{Var}(T)=\lambda^2\big[\Gamma(1+2/k)-\Gamma(1+1/k)^2\big]$. In the tables parametrisation, $\alpha=1/\lambda^k=1/36$ and $\beta=k=2$, so $\alpha^{-1/\beta}=6$, which gives the same result. With $k=2$, $\lambda=6$ and $\Gamma(3/2)=\sqrt\pi/2$, $\Gamma(2)=1$:

$$
\mathrm{E}(T) = 6\cdot\frac{\sqrt\pi}{2} = 3\sqrt\pi \approx 5.317 \text{ months},\qquad
\mathrm{SD}(T) = 6\sqrt{1-\pi/4} \approx 2.780 \text{ months}.
$$

**b)** Simulate inter-failure times $T_i\sim$ Weibull(2, 6) and add them up until the sum passes 60 months. See Figure 3. In this realisation **9 failures** occurred in $[0,60]$.

**c)** Based on $B=5000$ realisations:

- i) $\widehat{\mathrm{E}}[N(60)] = 10.94$. For comparison, $t/\mathrm{E}(T)=11.28$ (elementary renewal theorem). The simulated value is a little lower. This matches the second-order approximation $m(t)\approx t/\mu + (\sigma^2-\mu^2)/(2\mu^2) = 11.28-0.36 = 10.92$, which is below $t/\mu$ here because $\sigma<\mu$.
- ii) $\widehat P(N(60)\ge 15) = 0.030$.

**d)**

$$
\Lambda(t) = \int_0^t \Big(3+2\cos\frac{\pi u}{6}\Big)du = \Big[3u + 2\cdot\frac{6}{\pi}\sin\frac{\pi u}{6}\Big]_0^t = 3t + \frac{12}{\pi}\sin\Big(\frac{\pi t}{6}\Big).
$$

Expected number of failures over 3 years: $\Lambda(36) = 108 + \frac{12}{\pi}\sin(6\pi) = 108$.
For an NHPP, $N(36)\sim\text{Poisson}(\Lambda(36))$, i.e. $N(36)\sim\text{Poisson}(108)$.

**e)** See Figure 4. $\lambda(t)$ has period 12 months. It is highest, $\lambda=5$, at $t=0,12,24,36$, i.e. around the turn of the year (December/January, winter). It is lowest, $\lambda=1$, at $t=6,18,30$, i.e. around the start of July (June/July, summer).

*Thinning:* $\lambda(t)\le\lambda_{\max}=5$. Simulate a homogeneous Poisson process with rate 5 on $[0,36]$: $N\sim\text{Poisson}(5\cdot36)$ points, uniformly placed. Keep each point $t_j$ with probability $\lambda(t_j)/5$. The kept points form an NHPP with intensity $\lambda(t)$. Figure 5 shows one realisation with 133 failures, together with $\Lambda(t)$. Failures cluster in the winter months, and $N(t)$ is nearly flat in the summers. This realisation happens to lie above its expectation ($108\pm\sqrt{108}\approx 108\pm10.4$).

**f)** Based on $B=5000$ realisations:

- i) $\widehat{\mathrm{E}}[N(36)] = 108.08$, close to the theoretical $\Lambda(36)=108$.
- ii) $\widehat P(N(36)>120) = 0.119$. The exact Poisson(108) value is $1-F(120) = 0.116$.
- iii) $\widehat{\mathrm{E}}[N(3)] = 12.89$. Theory: $\Lambda(3) = 9+12/\pi \approx 12.82$. This is well above $3\cdot 3=9$ because the first quarter is the high-failure season.

## Problem 3

$n=40$, $\bar x = 7.845$, sample median $=7.40$, $s=3.085$. $B=2000$ resamples of size $n$ drawn with replacement. Histograms of the bootstrap statistics are in Figure 6.

**a)** Mean: bootstrap SD of $\bar X$ $=0.479$, close to $s/\sqrt n = 0.488$.
95% percentile bootstrap CI for $\mu$: **(6.947, 8.780)**.
The CLT interval from M1 2d was (5.414, 10.276), roughly 2.7 times wider. M1 used the exponential model, where $\mathrm{SD}(X)=\beta$, so the standard error was $\hat\beta/\sqrt n = 1.24$. The data are much less spread out than an exponential model implies ($s=3.09$ vs $\hat\beta=7.85$; M1 3d also found a gamma with shape around 6 fits much better). So the model-based interval is too wide. The bootstrap interval assumes no distribution and uses the actual spread in the data. It is close to the normal interval $\bar x\pm1.96\,s/\sqrt n=(6.89, 8.80)$.

**b)** Median: bootstrap SD of the sample median $=0.695$.
95% percentile bootstrap CI for the population median: **(6.250, 8.950)**.
This interval is wider than the one for the mean (length 2.70 vs 1.83) and is slightly asymmetric around 7.40. For roughly symmetric, light-tailed data like these, the median is a less efficient estimator than the mean, so it varies more. The bootstrap distribution of the median is also lumpy and discrete (Figure 6): a resample median can only be a data value, or the average of two adjacent ones. So the percentile interval for the median is cruder, and less reliable with $n=40$, than the one for the mean. Also note the two intervals target different parameters (population mean vs median), which differ because the data are slightly right-skewed.

## References

- STA510 lecture notes (Monte Carlo integration and variance reduction; Poisson and renewal processes; bootstrap) and the tables/formulas sheet.
- Rizzo, M. L., *Statistical Computing with R*, ch. 6 (antithetic variables, importance sampling).
