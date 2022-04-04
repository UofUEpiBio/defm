
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
n   <- 500
n_y <- 4
n_x <- 4

set.seed(1231)
Y <- matrix(as.integer(runif(n * n_y) < .05), ncol = n_y)
X <- matrix(rnorm(n * n_x), ncol = n_x)
id <- sort(rep(1:100, 5))

# Creating the model and adding a couple of terms
d_model <- new_defm(id, Y, X, order = 1)

term_defm_ones(d_model)
term_defm_fe(d_model, 0)

init_defm(d_model)

print(d_model)

f <- function(p) loglike_defm(d_model, p)
ans <- optim(c(0,0), f, control = list(fnscale = -1), hessian = TRUE)

ans
#> $par
#> [1] -1411.046  1303.204
#> 
#> $value
#> [1] 27876.72
#> 
#> $counts
#> function gradient 
#>       67       NA 
#> 
#> $convergence
#> [1] 10
#> 
#> $message
#> NULL
#> 
#> $hessian
#>              [,1]         [,2]
#> [1,] 4.365575e-05 0.000000e+00
#> [2,] 0.000000e+00 2.910383e-05
```
