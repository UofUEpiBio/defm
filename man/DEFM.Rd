% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R, R/defm-package.R
\name{DEFM}
\alias{DEFM}
\alias{new_defm_cpp}
\alias{new_defm}
\alias{defm}
\alias{init_defm}
\alias{print_stats}
\alias{nterms_defm}
\alias{nrow_defm}
\alias{ncol_defm_y}
\alias{ncol_defm_x}
\alias{nobs_defm}
\alias{morder_defm}
\title{Discrete Exponential Family Model (DEFM)}
\usage{
new_defm_cpp(id, Y, X, order = 1L, copy_data = TRUE)

init_defm(m, force_new = TRUE)

print_stats(m, i = 0L)

nterms_defm(m)

nrow_defm(m)

ncol_defm_y(m)

ncol_defm_x(m)

nobs_defm(m)

morder_defm(m)

new_defm(id, Y, X, order = 1)
}
\arguments{
\item{id}{Integer vector of length \code{n}. Observation ids, for example,
person id.}

\item{Y}{0/1 matrix of responses of \code{n_y} columns and \code{n} rows.}

\item{X}{Numeric matrix of covariates of size \code{n_x} by \code{n}.}

\item{order}{Integer. Order of the markov process, by default, 1.}

\item{copy_data}{Logical scalar. When \code{TRUE} (default) will copy the data
into the model, otherwise it will use the data as a pointer (see details).}

\item{m}{An object of class \code{DEFM}.}

\item{force_new}{Logical scalar. When \code{TRUE} (default) no cache is used
to add new arrays (see details).}

\item{i}{An integer scalar indicating which set of statistics to print (see details.)}
}
\value{
An external pointer of class \code{DEFM.}

\itemize{
\item \code{nterms_defm} returns the number of terms in the model.
}

\itemize{
\item \code{nrow_defm} returns the number of rows in the model.
}

\itemize{
\item \code{ncol_defm_y} returns the number of output variables in
the model.
}

\itemize{
\item \code{ncol_defm_x} returns the number of covariates in the model.
}

\itemize{
\item \code{nobs_defm} returns the number of observations (events) in the
model.
}

\itemize{
\item \code{morder_defm} returns the order of the Markov process.
}

An external pointer of class \code{DEFM.}
}
\description{
Discrete Exponential Family Models (DEFMs) are models from the exponential
family that deal with discrete data. Here, we deal with binary arrays which
can be used to represent, among other things, networks and multinomial binary
Markov processes.

Discrete Exponential Family Models (DEFMs) are models from the exponential
family that deal with discrete data. Here, we deal with binary arrays which
can be used to represent, among other things, networks and multinomial
binary Markov processes.
}
\details{
The \code{id} vector is used to group the observations. For example, if you have
a dataset with multiple individuals, the \code{id} vector should contain the
individual ids. The \code{Y} matrix contains the binary responses, where each
column represents a different response variable. The \code{X} matrix contains
the covariates, which can be used to model the relationship between the
responses and the covariates. The \code{order} parameter specifies the order of
the Markov process, which determines how many previous observations are
used to predict the current observation.

The \code{copy_data} parameter specifies
whether the data should be copied into the model or used as a pointer. If
\code{copy_data} is \code{TRUE}, the data will be copied into the model, which can
be useful if you want to avoid duplicating the data in memory. If
\code{copy_data} is \code{FALSE}, the model will use the data as a pointer, which can
be more efficient (but dangerous if the data is removed).

The \code{init_defm} function initializes the model, which means it computes
the sufficient statistics and prepares the model for fitting. The
\code{force_new} parameter specifies whether to force the model to be
consider each array added as completely unique, even if it has the
same support set as an existing array. This is an experimental feature
and should be used with caution.

The \code{print_stats} function prints the supportset of the ith type
of array in the model.
}
\examples{
# Loading Valente's SNS data
data(valentesnsList)

mymodel <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 1
)

# Adding the intercept terms and a motif from tobacco to mj
term_defm_logit_intercept(mymodel)
term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")

# Initialize the model
init_defm(mymodel)

# Fitting the MLE
defm_mle(mymodel)

}
\references{
Vega Yon, G. G., Pugh, M. J., & Valente, T. W. (2022). Discrete Exponential-Family Models for Multivariate Binary Outcomes (arXiv:2211.00627). arXiv. \url{https://arxiv.org/abs/2211.00627}
}
\seealso{
\code{\link[=defm_mle]{defm_mle()}} for maximum likelihood estimation and \code{\link[=loglike_defm]{loglike_defm()}}
for the log-likelihood function.
}
