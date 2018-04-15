#' ---
#' title: "Visualizing Likelihood"
#' author: Pavan Gurazada
#' output: github_document
#' ---

#' last update: Sun Apr 15 11:59:09 2018

library(tidyverse)

set.seed(20130810)
theme_set(theme_minimal())

#' Generate some data to play with

x <- rnorm(200, 0, 3)
y <- 1.5 * x + rnorm(200, 0, 1)

sim_df <- data.frame(id = 1:200, x, y)
glimpse(sim_df)

#' In this case we have a clear closed form solution for the likelihood of
#' observing the data, since the `y`'s are drawn from a normal distribution with
#' mean `bx`, where `b = 1.5`.

log_likl <- function(b, v, data_df) {
  return(sum(dnorm(data_df$y, mean = b*data_df$x, sd = sqrt(v), log = TRUE)))
}

test <- data.frame(x = c(1, 1, 4), y = c(2.0, 1.8, 6.3))
log_likl(b = 1.8, v = 1, test)
log_likl(b = 0.5, v = 1, test)

#' Now, we can vary the coefficient `b`

b <- seq(0, 3, length.out = 500) # consider 500 values for b between 0 and 3
ll <- map_dbl(b, log_likl, 1, sim_df)

coefs <- data.frame(beta = b, 
                    log_likelihood = map_dbl(b, log_likl, 1, sim_df))
glimpse(coefs)

ggplot(coefs) +
  geom_line(aes(x = beta, y = log_likelihood), size = 1) +
  scale_x_continuous(breaks = seq(0, 3, 0.5), 
                     name = expression(beta)) +
  scale_y_continuous(name = "Log Likelihood")

#' Essentially we do a grid search on the likelihood along the range 0 - 3 and
#' choose the maximum likelihood estimate. In this case the maximum likelihood
#' estimate can be seen as the highest point in the above plot (since log
#' likelihood values are negative).
#'
#' In the analysis above the true error variance was fixed at 1. We can allow
#' the error variances to vary too. So let us vary the coef `b` as before and
#' also vary the variance as drawn from a normal distribution from a mean 1.5
#' and sd 0.1. Let us put together a parameter grid comprising of 500 values of
#' `b` and 50 values of `v`. This gives us 25,000 points in the parameter space.
#' The best choice will be one for which the likelihood is maximized.

params <- expand.grid(b = seq(0, 3, length.out = 500),
                      v = rnorm(n = 50, mean = 1.5, sd = 0.1))

glimpse(params)

ll <- pmap_dbl(as.list(params), log_likl, sim_df)

results <- bind_cols(params, data.frame(ll))
glimpse(results)

dev.new()
ggplot(results) +
  geom_line(aes(x = b, y = ll, group = v, color = v)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5), 
                     name = expression(beta)) +
  scale_y_continuous(name = "Log Likelihood") +
  scale_color_continuous(low = "grey", high = "black") +
  labs(color = "Error Variance")
