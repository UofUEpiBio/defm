
<!-- README.md is generated from README.Rmd. Please edit that file -->

# defm

<!-- badges: start -->
<!-- badges: end -->

The goal of defm is to …

## Installation

You can install the development version of defm like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(defm)
## basic example code

# Simulating core data
n   <- 10000
n_y <- 4
n_x <- 4
n_t <- 5

set.seed(1231)
Y <- matrix(as.integer(runif(n * n_y * n_t) < .1), ncol = n_y)
X <- matrix(rnorm(n * n_x * n_t), ncol = n_x)
# X[,2] <- as.integer(X[,2] > 0)
id <- sort(rep(1:n, n_t))

# Creating the model and adding a couple of terms
build_model <- function(id., Y., X., order. = 1, par. = par.) {
  
  d_model. <- new_defm(id., Y., X., order = order.)

  term_defm_ones(d_model.)
  term_defm_ones(d_model., 1)
  term_defm_transition(d_model., c(0,1,3))
  
  init_defm(d_model.)
  
  d_model.
  
}

sim_par <- c(-2, 2, 5)

d_model <- build_model(id, Y, X, order = 1, par. = sim_par)

simulated_Y <- sim_defm(d_model, sim_par)
simulated_Y <- matrix(as.vector(simulated_Y), byrow = TRUE, ncol = n_y)

# Filling the gaps
locations <- which(simulated_Y == -1, arr.ind = TRUE)
simulated_Y[locations] <- Y[locations]

View(cbind(id, simulated_Y))


d_model_sim <- build_model(id, simulated_Y, X, order = 1, par. = sim_par)
print_stats(d_model_sim, 0)
print_stats(d_model_sim, 1)

f <- function(p, as_log=TRUE) loglike_defm(d_model_sim, p, as_log = as_log)
ans <- optim(
  c(0,0,0), f, control = list(fnscale = -1),
  hessian = TRUE,
  method = "L-BFGS-B", lower = -10, upper = 10)

ans
#> $par
#> [1] -2.014700  2.003489  4.990579
#> 
#> $value
#> [1] -54330.32
#> 
#> $counts
#> function gradient 
#>       14       14 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $hessian
#>            [,1]        [,2]       [,3]
#> [1,] -19272.963  -8744.1127 -1800.8370
#> [2,]  -8744.113 -13080.9119   278.2839
#> [3,]  -1800.837    278.2839 -1002.4297
f(ans$par)
#> [1] -54330.32
```

## Code of Conduct

Please note that the defm project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
