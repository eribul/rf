library(tidyverse)
library(ranger)
library(furrr)

set.seed(123)
plan(multiprocess)

# Help functions ----------------------------------------------------------


#' tibble with random data
#' @param rows number of rows
#' @param cols number of columns
rdata <- function(cols = 1e3, rows = 1e3) {

  stopifnot(cols > 3)

  # Random functions chosen randomly to generate random data
  funs  <- sample(list(rnorm, rexp, runif), cols, replace = TRUE)

  X <- sapply(funs, exec, rows)
  Y <- X %*% rnorm(cols) # linear comb with random coefs

  cbind(Y, X[, seq_len(cols - 3)]) %>%
    as_tibble(.name_repair = "unique")
}

#' Fit a random forrest model
#' @param dat data frame
#' @param trees number of trees in the forrest
rf <- function(dat, trees = 1e2) {
  ranger::ranger(
    ...1 ~ ., dat, trees,
    importance = "impurity_corrected"
  )
}


# Data management and analysis --------------------------------------------


# Data, predictions and metrics for increasingly large datasets
rf_all <-
  tibble(
    p = seq(10, 1e3, 1e2),
    n = seq(10, 1e3, 1e2)
  ) %>%
  expand(n, p) %>%
  mutate(
    data  = suppressMessages(future_map(p, rdata)),
    rf    = map(data, rf),

    # Overall out of bag prediction error (MSE)
    MSE = map_dbl(rf, "prediction.error"),
    r.squared = map_dbl(rf, "r.squared"),

    # variable importance
    imp   = map2(rf, data, ~ importance_pvalues(
      .x, "altman", formula = ...1 ~ .,data = .y, num.permutations = 10)),

    # Which variables should be included (according to p-values)
    keep = map(imp, ~ rownames(.)[.[, "pvalue"] < 0.2]),
    n_keep = map_int(keep, length), # no of included vars
    n_prop = map2_dbl(n_keep, p,  ~ .x / (.y - 3)) # proportion of included
  )

# Keep only summary data for presentation
rf_summary <-
  rf_all %>%
  gather("key", "value", MSE, n_prop, r.squared) %>%
  select(n, p, key, value)

save(rf_summary, file = "data.RData")

# Results -----------------------------------------------------------------


# Make plot
rf_summary %>%
  ggplot(aes(p, value, group = n, color = n)) +
  geom_line() +
  facet_wrap(~ key, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = c(0.1, .9))

ggsave("performance.png")
