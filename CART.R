library(tidyverse)
library(rpart)
library(validate)
library(furrr)

rdata <- function(cols = 1e3, rows = 1e3) {
  
  stopifnot(cols > 3)
  
  # Random functions chosen randomly to generate random data
  funs  <- sample(list(rnorm, rexp, runif), cols, replace = TRUE)
  
  X <- sapply(funs, exec, rows)
  Y <- X %*% rnorm(cols) # linear comb with random coefs
  
  cbind(Y, X[, seq_len(cols - 3)]) %>%
    as_tibble(.name_repair = "unique")
}

# Grow the tree

cart <- function(dat) {
  rpart(...1 ~ .,  
        method="anova", data=dat)
}

cart_all <-
  tibble(p = seq(5, 500, 10)) %>%
  mutate(
    data  = suppressMessages(future_map(p, rdata)),
    cart  = map(data, cart),
    imp = map(cart, function(x) x$variable.importance)
  )

for (i in 1:nrow(cart_all)) {
  plotcp(cart_all[i,]$cart[[1]])
  title(paste(cart_all[[1]][i], "parameters"), line = -2)
}
