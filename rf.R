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
  # First name of functions (for presentation), then functions themselves
  funs_s  <- sample(c("rnorm", "rexp", "runif"), cols, replace = TRUE)
  funs    <- lapply(funs_s, get)

  X <- sapply(funs, exec, rows)
  b <- rnorm(cols)
  Y <- X %*% b # linear comb with random coefs

  structure(
    cbind(Y, X[, seq_len(cols - 3)]) %>%
    as_tibble(.name_repair = "unique"),
    funs = funs_s,
    b = b
  )
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
    keep_i = map(imp, ~ .[, "pvalue"] < 0.2),
    n_keep = map_int(keep_i, sum), # no of included vars
    n_prop = map2_dbl(n_keep, p,  ~ .x / (.y - 3)) # proportion of included
  )

# Identify kept/dropped columns
col_types <-
  rf_all %>%
  transmute(
    n, p,
    # Which variables got identified?
    keep_type = map2(data, keep_i, ~ attr(.x, "funs")[.y]),
    drop_type = map2(data, keep_i, ~ attr(.x, "funs")[!.y]),
    keep_b    = map2(data, keep_i, ~ attr(.x, "b")[.y]),
    drop_b    = map2(data, keep_i, ~ attr(.x, "b")[!.y]),

    keep_b_mean = map_dbl(keep_b, mean),
    drop_b_mean = map_dbl(drop_b, mean),
    prop_b_kept = keep_b_mean / drop_b_mean
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


# Kept coefficients vs dropped
col_types %>%
  ggplot(aes(prop_b_kept)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 1), col = "red") +
  theme_minimal()

ggsave("prop_b_kept.png")

# No differnece (but shouls also consider type of variable or scale them!)
t.test(col_types$prop_b_kept)
