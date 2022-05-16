
<!-- README.md is generated from README.Rmd. Please edit that file -->

# defm: Fit and simulate discrete binary exponential family models

<!-- badges: start -->
<!-- badges: end -->

Discrete exponential family models (DEFM) have a long tradition with
extensive development rooted in exponential random graph models (ERGMs.)
Applicable to any form o data that can be represented as binary arrays,
DEFMs provide a way to model jointly distributed binary variables.

This package, built on top of the C++ library
[`barry`](https://github.com/USCbiostats/barry), provides a
computationally efficient implementation of this family of models.

# Installation

You can install `defm` using devtools:

``` r
devtools::install_github("UofUEpi/defm")
```

# Examples

## Example 1: Four outcomes

In this example, we will simulate a dataset that contains 1,000
individuals with four different outcomes. The outcomes, which are 0/1
vectors, will be modeled as Markov processes of order one. Future states
of this 0/1 vector are a function of the previous point in time. The
following lines simulate the baseline data:

``` r
library(defm)
#> Loading required package: stats4

# Simulation parameters
set.seed(1231)
n   <- 5000L # Count of individuals
n_y <- 3L    # Number of dependent variables
n_x <- 2L    # Number of independent variables

# Simulating how many observations we will have per individuals
n_reps <- sample(3:10, n, replace = TRUE)

# Final number of rows in the data
n_t <- sum(n_reps)

# Simulating the data
Y <- matrix(as.integer(runif(n_y * n_t) < .1), ncol = n_y)
colnames(Y) <- paste0("y", 1:n_y - 1)
X <- matrix(rnorm(n_x * n_t), ncol = n_x)
colnames(X) <- paste0("x", 1:n_x - 1L)
id <- rep(1L:n, n_reps)
time <- unlist(sapply(n_reps, \(x) 1:x))
```

Here is a brief look at the data structure. Remember, we still have not
actually simulated data **WITH THE MODEL**.

|  id | time |  y0 |  y1 |  y2 |    x0 |    x1 |
|----:|-----:|----:|----:|----:|------:|------:|
|   1 |    1 |   0 |   0 |   0 |  0.51 |  0.95 |
|   1 |    2 |   0 |   0 |   0 |  0.16 |  0.25 |
|   1 |    3 |   0 |   0 |   0 |  1.20 | -1.72 |
|   1 |    4 |   0 |   1 |   0 | -0.20 |  1.55 |
|   2 |    1 |   0 |   1 |   0 | -0.15 | -0.68 |
|   2 |    2 |   1 |   0 |   0 |  1.19 |  0.92 |
|   2 |    3 |   0 |   0 |   1 | -0.65 |  1.16 |
|   2 |    4 |   0 |   0 |   0 | -0.99 |  0.21 |
|   2 |    5 |   0 |   0 |   0 |  0.76 |  1.45 |
|   2 |    6 |   0 |   0 |   0 | -0.68 | -1.34 |

For this example, we will simulate a model with the following features:

-   **Ones**: Baseline density (prevalence of ones),
    ![\\sum\_{itk}y\_{itk}](https://latex.codecogs.com/gif.image?%5Csum_%7Bitk%7Dy_%7Bitk%7D "\sum_{itk}y_{itk}")

-   **Ones x Attr 2**: Same as before, but weighted by one of the
    covariates. (simil to fixed effect)
    ![\\sum\_{itk}y\_{itk}x\_{it}](https://latex.codecogs.com/gif.image?%5Csum_%7Bitk%7Dy_%7Bitk%7Dx_%7Bit%7D "\sum_{itk}y_{itk}x_{it}")

-   **Transition** : And a transition structure, in particular
    `y0 -> (y0, y1)`,
    ![y\_{i0}^0y\_{i0}^{t+1} y\_{i1}^{t+1}](https://latex.codecogs.com/gif.image?y_%7Bi0%7D%5E0y_%7Bi0%7D%5E%7Bt%2B1%7D%20y_%7Bi1%7D%5E%7Bt%2B1%7D "y_{i0}^0y_{i0}^{t+1} y_{i1}^{t+1}").

In `defm`, transition statistics can be represented using matrices. In
this case, the transition can be written as:

![
\\begin{array}{c}t\\\\t+1\\end{array}\\left\[\\begin{array}{ccc}1 & \\cdot & \\cdot \\\\ 1 & 1 & \\cdot\\end{array}\\right\]
](https://latex.codecogs.com/gif.image?%0A%5Cbegin%7Barray%7D%7Bc%7Dt%5C%5Ct%2B1%5Cend%7Barray%7D%5Cleft%5B%5Cbegin%7Barray%7D%7Bccc%7D1%20%26%20%5Ccdot%20%26%20%5Ccdot%20%5C%5C%201%20%26%201%20%26%20%5Ccdot%5Cend%7Barray%7D%5Cright%5D%0A "
\begin{array}{c}t\\t+1\end{array}\left[\begin{array}{ccc}1 & \cdot & \cdot \\ 1 & 1 & \cdot\end{array}\right]
")

which in `R` is

``` r
> matrix(c(1,NA_integer_,NA_integer_,1,1,NA_integer_), nrow = 2, byrow = TRUE)
#      [,1] [,2] [,3]
# [1,]    1   NA   NA
# [2,]    1    1   NA
```

The `NA` entries in the matrix mean that those can be either zero or
one. In other words, only values different from `NA` will be taken into
account for specifying the terms. Here is the factory function:

``` r
# Creating the model and adding a couple of terms
build_model <- function(id., Y., X., order. = 1, par. = par.) {
  
  # Mapping the data to the C++ wrapper
  d_model. <- new_defm(id., Y., X., order = order.)

  # Adding the model terms
  term_defm_ones(d_model.)
  term_defm_ones(d_model., 1, "Variable 1")
  
  transition <- matrix(NA_integer_, nrow = order. + 1, ncol = ncol(Y.))
  transition[c(1,2,4)] <- 1
  
  term_defm_transition(d_model., transition)
  
  # Initializing the model
  init_defm(d_model.)
  
  # Returning
  d_model.
  
}
```

With this factory function, we will use it to simulate some data with
the same dimensions of the original dataset. In this case, the
parameters used for the simulation will be:

-   **Ones**: -2, i.e., low density,
-   **Ones x Attr 2**: 2, yet correlated with covariate \# 2,
-   **Transition** : 5, And a high chance of observing the transition
    `y0 -> (y0, y1)`

``` r
sim_par <- c(-2, 2, 5)
d_model <- build_model(id, Y, X, order = 1, par. = sim_par)
simulated_Y <- sim_defm(d_model, sim_par)
head(cbind(id, simulated_Y))
#>      id      
#> [1,]  1 0 0 0
#> [2,]  1 0 0 0
#> [3,]  1 0 0 0
#> [4,]  1 1 1 1
#> [5,]  2 0 0 0
#> [6,]  2 0 0 1
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
#>                           Estimate Std. Error
#> # of ones                -2.011721 0.01432388
#> # of ones x Variable 1    2.002699 0.01583181
#> Motif {y⁺₀} ⇨ {y⁺₀, y⁺₁}  4.997817 0.04617595
#> 
#> -2 log L: 55019.12
```

Or better, we can use `texreg` to generate a pretty output:

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>
Statistical models
</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">
 
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 1
</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
\# of ones
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.01
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.01)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
\# of ones x Variable 1
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.00
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.02)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Motif {y⁺₀} ⇨ {y⁺₀, y⁺₁}
</td>
<td style="padding-left: 5px;padding-right: 5px;">
5.00
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.05)
</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
AIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
55025.12
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
BIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
55050.31
</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
N
</td>
<td style="padding-left: 5px;padding-right: 5px;">
32777
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2">
<sup>\*\*\*</sup>p \< 0.001; <sup>\*\*</sup>p \< 0.01; <sup>\*</sup>p \<
0.05
</td>
</tr>
</tfoot>
</table>

We can also see the counts

|  id |  y0 |  y1 |  y2 |    x0 |    x1 | \# of ones | \# of ones x Variable 1 | Motif {y⁺₀} ⇨ {y⁺₀, y⁺₁} |
|----:|----:|----:|----:|------:|------:|-----------:|------------------------:|-------------------------:|
|   1 |   0 |   0 |   0 |  0.51 |  0.95 |         NA |                      NA |                       NA |
|   1 |   0 |   0 |   0 |  0.16 |  0.25 |          0 |                       0 |                        0 |
|   1 |   0 |   0 |   0 |  1.20 | -1.72 |          0 |                       0 |                        0 |
|   1 |   1 |   1 |   1 | -0.20 |  1.55 |          0 |                       0 |                        0 |
|   2 |   0 |   0 |   0 | -0.15 | -0.68 |          0 |                       0 |                        0 |
|   2 |   0 |   0 |   1 |  1.19 |  0.92 |          0 |                       0 |                        0 |
|   2 |   0 |   1 |   1 | -0.65 |  1.16 |          0 |                       0 |                        0 |
|   2 |   0 |   0 |   0 | -0.99 |  0.21 |          0 |                       0 |                        0 |
|   2 |   0 |   1 |   1 |  0.76 |  1.45 |          0 |                       0 |                        0 |
|   2 |   1 |   0 |   0 | -0.68 | -1.34 |          0 |                       0 |                        0 |

## Example 2: A fun model

For fun, imagine that we want to describe a process in which an
individual moves sequentially through a set of states. In this example,
there are ten different `y` variables, but the person can only have one
of them as active (equal to one.) We can simulate such data using DEFM.

We first need to generate the baseline data we will use for the
simulation. This involves creating a matrix of size 20x10 (so we have 20
time-points,) filled with zeros in all but the first entry:

``` r
n   <- 20L
n_y <- 10L
id <- rep(1L, n)
Y <- matrix(0L, nrow = n, ncol = n_y)
Y[1] <- 1L
X <- matrix(0.0, nrow = n, ncol = 1)
```

With the data in hand, we can now simulate the process. First, we need
to build the model. The key component of the model will be the
transition matrices:

<!-- $$
\left[\begin{array}{cccc}
1 & 0 & NA & \dots \\
0 & 1 & NA & \dots
\end{array}\right] \dots 
\left[\begin{array}{cccccc}
\dots & NA & 1 & 0 & NA & \dots  \\
\dots & NA & 0 & 1 & NA & \dots  
\end{array}\right] \dots 
\left[\begin{array}{cccc}
\dots & NA & 1 & 0 \\
\dots & NA & 0 & 1 
\end{array}\right]
$$ -->

<div align="center">

<img style="background: white;" src="https://render.githubusercontent.com/render/math?math=%5Cleft%5B%5Cbegin%7Barray%7D%7Bcccc%7D%0A1%20%26%200%20%26%20NA%20%26%20%5Cdots%20%5C%5C%0A0%20%26%201%20%26%20NA%20%26%20%5Cdots%0A%5Cend%7Barray%7D%5Cright%5D%20%5Cdots%20%0A%5Cleft%5B%5Cbegin%7Barray%7D%7Bcccccc%7D%0A%5Cdots%20%26%20NA%20%26%201%20%26%200%20%26%20NA%20%26%20%5Cdots%20%20%5C%5C%0A%5Cdots%20%26%20NA%20%26%200%20%26%201%20%26%20NA%20%26%20%5Cdots%20%20%0A%5Cend%7Barray%7D%5Cright%5D%20%5Cdots%20%0A%5Cleft%5B%5Cbegin%7Barray%7D%7Bcccc%7D%0A%5Cdots%20%26%20NA%20%26%201%20%26%200%20%5C%5C%0A%5Cdots%20%26%20NA%20%26%200%20%26%201%20%0A%5Cend%7Barray%7D%5Cright%5D">

</div>

Let’s take a look at the process:

``` r
# Creating a new instance of a DEFM object
d_model <- new_defm(id = id, Y = Y, X = X, order = 1)

# Creating the transition terms, these
for (i in (1:(n_y - 1) - 1)) {
  transition <- matrix(NA_integer_, nrow = 2, ncol = n_y)
  transition[c(1:4) + 2 * i] <- c(1,0,0,1)
  term_defm_transition(d_model, transition)
}

# Here is the last transition term
transition <- matrix(NA_integer_, nrow = 2, ncol = n_y)
transition[c(n_y * 2 - 1, n_y * 2, 1, 2)] <- c(1,0,0,1)
term_defm_transition(d_model, transition)

# Adding a term of ones
term_defm_ones(d_model)

# Initializing and simulating
init_defm(d_model)
Y_sim <-sim_defm(d_model, par = c(rep(100, n_y), -10))
```

The simulation should produce a nice looking figure:

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

# Acknowledgement

This work was supported by the Assistant Secretary of Defense for Health
Affairs endorsed by the Department of Defense, through the Psychological
Health/Traumatic Brain Injury Research Program Long-Term Impact of
Military-Relevant Brain Injury Consortium (LIMBIC)
Award/W81XWH-18-PH/TBIRP-LIMBIC under Award No. I01 RX003443. The U.S.
Army Medical Research Acquisition Activity, 839 Chandler Street, Fort
Detrick MD 21702-5014 is the awarding and administering acquisition
office. Opinions, interpretations, conclusions and recommendations are
those of the author and are not necessarily endorsed by the Department
of Defense. Any opinions, findings, conclusions recommendations
expressed in this publication are those of the author(s) and do not
necessarily reflect the views of the U.S. Government, the U.S.
Department of Veterans Affairs or the Department of Defense and no
official endorsement should be inferred.

# Code of Conduct

Please note that the defm project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
