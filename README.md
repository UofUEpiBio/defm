
<!-- README.md is generated from README.Rmd. Please edit that file -->

# defm: Fit and simulate discrete binary exponential family models

<!-- badges: start -->
<!-- badges: end -->

Discrete exponential family models (DEFM) have a long tradition with a
extensive development rooted in exponential random graph models (ERGMs.)
Applicable to any form o data that can be represented as binary arrays,
DEFMs provide a way to model jointly distributed binary variables.

This package, built on top of the C++ library
[`barry`](https://github.com/USCbiostats/barry), provides a
computationally efficient implementation of this family of models.

## Installation

You can install the development version of defm like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

In this example, we will simulate a dataset that contains 1,000
individuals with four different outcomes. The outcomes, which are 0/1
vectors, will be modeled as Markov processes of order one, in which
future states of this 0/1 vector can be predicted as a function of the
previous point in time. The following lines of simulate the baseline
data:

``` r
library(defm)
#> Loading required package: stats4

# Simulation parameters
set.seed(1231)
n   <- 1000L # Count of individuals
n_y <- 4L    # Number of dependent variables
n_x <- 2L    # Number of independent variables

# Simulating how many observations we will have per individuals
n_reps <- sample(3:10, 1000, replace = TRUE)

# Final number of rows in the data
n_t <- sum(n_reps)

# Simulating the data
Y <- matrix(as.integer(runif(n_y * n_t) < .1), ncol = n_y)
X <- matrix(rnorm(n_x * n_t), ncol = n_x)
id <- rep(1:n, n_reps)
```

For this example, we will simulate a model with the following features:

-   **Ones**: Baseline density (prevalence of ones).
-   **Ones x Attr 2**: Same as before, but weighted by one of the
    covariates. (simil to fixed effect)
-   **Transition** : And a transition structure, in particular
    `y0 -> (y0, y1)`, which will be represented as a matrix in the form:

<!-- -->

    1 0 0 0
    1 1 0 0

Here is the factory function:

``` r
# Creating the model and adding a couple of terms
build_model <- function(id., Y., X., order. = 1, par. = par.) {
  
  d_model. <- new_defm(id., Y., X., order = order.)

  term_defm_ones(d_model.)
  term_defm_ones(d_model., 1)
  
  transition <- matrix(0L, nrow = order. + 1, ncol = ncol(Y.))
  transition[c(1,2,4)] <- 1
  
  term_defm_transition(d_model., transition)
  
  init_defm(d_model.)
  
  d_model.
  
}
```

With this factory function, we will use it to simulate some data with
the same dimmensions of the original dataset. In this case, the
parameters used for the simulation will be:

-   **Ones**: -2, i.e., low density,
-   **Ones x Attr 2**: 2, yet correlated with covariate # 2,
-   **Transition** : 5, And a high chance of observing the transition
    `y0 -> (y0, y1)`

``` r
sim_par <- c(-2, 2, 5)
d_model <- build_model(id, Y, X, order = 1, par. = sim_par)
simulated_Y <- sim_defm(d_model, sim_par)
head(cbind(id, simulated_Y))
#>      id        
#> [1,]  1 0 0 0 0
#> [2,]  1 0 0 0 0
#> [3,]  1 0 0 1 0
#> [4,]  1 1 0 0 0
#> [5,]  2 0 0 1 0
#> [6,]  2 0 1 1 0
```

Now, let’s see if we can recover the parameters using MLE:

``` r
d_model_sim <- build_model(id, simulated_Y, X, order = 1, par. = sim_par)
ans <- defm_mle(d_model_sim)

summary(ans)
#> Maximum likelihood estimation
#> 
#> Call:
#> stats4::mle(minuslogl = minuslog, start = start, method = "L-BFGS-B", 
#>     nobs = nrow_defm(object), lower = lower, upper = upper)
#> 
#> Coefficients:
#>                    Estimate Std. Error
#> # of ones         -1.956948 0.02668729
#> # of ones x attr1  1.981110 0.03013901
#> Motif 0 4          4.643450 0.14981689
#> 
#> -2 log L: 16029.98
```

## Code of Conduct

Please note that the defm project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
