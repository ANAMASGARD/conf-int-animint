# confidence interval with animint2 (ran local first to see it )
# Port of conf.int() from the animation package to animint2
# Original: https://yihui.org/animation/example/conf-int/

library(animint2)

set.seed(42)

nmax <- 50
n    <- 30
conf <- 0.90

z <- qnorm(1 - (1 - conf) / 2)

samp <- replicate(nmax, rnorm(n, mean = 0, sd = 1))
xbar <- colMeans(samp)
se   <- 1 / sqrt(n)
lo   <- xbar - z * se
hi   <- xbar + z * se
hit  <- lo <= 0 & hi >= 0

## intervals: sample k is visible from frame k to nmax
ci_list <- vector("list", nmax)
for (k in seq_len(nmax)) {
  ci_list[[k]] <- data.frame(
    experiment = k,
    frame      = k:nmax,
    lower      = lo[k],
    upper      = hi[k],
    xmean      = xbar[k],
    result     = ifelse(hit[k], "covers 0", "misses 0"),
    stringsAsFactors = FALSE
  )
}
ci_data <- do.call(rbind, ci_list)

## running coverage: coverage at step k = hits so far / k
nhits    <- cumsum(as.integer(hit))
cov_list <- vector("list", nmax)
for (k in seq_len(nmax)) {
  cov_list[[k]] <- data.frame(
    experiment    = k,
    frame         = k:nmax,
    coverage_rate = nhits[k] / k,
    stringsAsFactors = FALSE
  )
}
cov_data <- do.call(rbind, cov_list)

## plot 1: confidence intervals
plot_intervals <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_segment(
    data = ci_data,
    aes(
      x    = lower,
      xend = upper,
      y    = experiment,
      yend = experiment,
      color = result,
      key   = experiment
    ),
    showSelected = "frame",
    linewidth = 1
  ) +
  geom_point(
    data = ci_data,
    aes(
      x    = xmean,
      y    = experiment,
      color = result,
      key   = experiment
    ),
    showSelected = "frame",
    size = 2
  ) +
  scale_color_manual(values = c("covers 0" = "steelblue", "misses 0" = "tomato")) +
  labs(
    title = "90% Confidence Intervals",
    x     = "Interval range",
    y     = "Sample"
  ) +
  theme_bw()

## plot 2: running coverage
frame_ticks <- data.frame(frame = 1:nmax)

plot_coverage <- ggplot() +
  geom_hline(yintercept = conf, linetype = "dashed", color = "gray50") +
  geom_line(
    data = cov_data,
    aes(x = experiment, y = coverage_rate),
    showSelected = "frame",
    color = "steelblue",
    linewidth = 0.9
  ) +
  geom_point(
    data = cov_data,
    aes(x = experiment, y = coverage_rate),
    showSelected = "frame",
    color = "steelblue",
    size  = 2
  ) +
  geom_tallrect(
    data = frame_ticks,
    aes(xmin = frame - 0.5, xmax = frame + 0.5),
    clickSelects = "frame",
    alpha = 0.2
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = paste0("Running coverage (target = ", conf, ")"),
    x     = "Sample",
    y     = "Coverage"
  ) +
  theme_bw()

## Deployed on Github Pages 
viz <- animint(
  title     = "Demonstration of Confidence Intervals",
  intervals = plot_intervals,
  coverage  = plot_coverage,
  time      = list(variable = "frame", ms = 400),
  source    = "https://github.com/ANAMASGARD/conf-int-animint/blob/main/conf_int_viz.R"
)

## build and view locally
#viz <- animint(
#  title     = "Demonstration of Confidence Intervals",
#  intervals = plot_intervals,
# coverage  = plot_coverage,
#  time      = list(variable = "frame", ms = 400),
# source    = "conf_int_viz.R"
# )
# animint2dir(viz, out.dir = "local_output", open.browser = TRUE) *\
