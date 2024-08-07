% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ao.R
\name{ao}
\alias{ao}
\title{Alternating Optimization}
\usage{
ao(
  f,
  initial,
  target = NULL,
  npar = NULL,
  gradient = NULL,
  ...,
  partition = "sequential",
  new_block_probability = 0.3,
  minimum_block_number = 2,
  minimize = TRUE,
  lower = -Inf,
  upper = Inf,
  iteration_limit = Inf,
  seconds_limit = Inf,
  tolerance_value = 1e-06,
  tolerance_parameter = 1e-06,
  tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
  tolerance_history = 1,
  base_optimizer = Optimizer$new("stats::optim", method = "L-BFGS-B"),
  verbose = FALSE,
  hide_warnings = TRUE
)
}
\arguments{
\item{f}{(\code{function})\cr
A \code{function} to be optimized, returning a single \code{numeric} value.

The first argument of \code{f} should be a \code{numeric} of the same length
as \code{initial}, optionally followed by any other arguments specified by
the \code{...} argument.

If \code{f} is to be optimized over an argument other than the first, or more
than one argument, this has to be specified via the \code{target} argument.}

\item{initial}{(\code{numeric()} or \code{list()})\cr
The starting parameter values for the target argument(s).

This can also be a \code{list} of multiple starting parameter values, see details.}

\item{target}{(\code{character()} or \code{NULL})\cr
The name(s) of the argument(s) over which \code{f} gets optimized.

This can only be \code{numeric} arguments.

Can be \code{NULL} (default), then it is the first argument of \code{f}.}

\item{npar}{(\code{integer()})\cr
The length of the target argument(s).

Must be specified if more than two target arguments are specified via
the \code{target} argument.

Can be \code{NULL} if there is only one target argument, in which case \code{npar} is
set to be \code{length(initial)}.}

\item{gradient}{(\code{function} or \code{NULL})\cr
A \code{function} that returns the gradient of \code{f}.

The function call of \code{gradient} must be identical to \code{f}.

Can be \code{NULL}, in which case a finite-difference approximation will be used.}

\item{...}{Additional arguments to be passed to \code{f} (and \code{gradient}).}

\item{partition}{(\code{character(1)} or \code{list()})\cr
Defines the parameter partition, and can be either
\itemize{
\item \code{"sequential"} for treating each parameter separately,
\item \code{"random"} for a random partition in each iteration,
\item \code{"none"} for no partition (which is equivalent to joint optimization),
\item or a \code{list} of vectors of parameter indices, specifying a custom
partition for the alternating optimization process.
}

This can also be a \code{list} of multiple partition definitions, see details.}

\item{new_block_probability}{(\code{numeric(1)})\cr
Only relevant if \code{partition = "random"}.

The probability for a new parameter block when creating a random
partitions.

Values close to 0 result in larger parameter blocks, values close to 1
result in smaller parameter blocks.}

\item{minimum_block_number}{(\code{integer(1)})\cr
Only relevant if \code{partition = "random"}.

The minimum number of blocks in random partitions.}

\item{minimize}{(\code{logical(1)})\cr
Whether to minimize during the alternating optimization process.

If \code{FALSE}, maximization is performed.}

\item{lower, upper}{(\code{numeric()})\cr
Optionally lower and upper parameter bounds.}

\item{iteration_limit}{(\code{integer(1)} or \code{Inf})\cr
The maximum number of iterations through the parameter partition before
the alternating optimization process is terminated.

Can also be \code{Inf} for no iteration limit.}

\item{seconds_limit}{(\code{numeric(1)})\cr
The time limit in seconds before the alternating optimization process is
terminated.

Can also be \code{Inf} for no time limit.

Note that this stopping criteria is only checked \emph{after} a sub-problem is
solved and not \emph{within} solving a sub-problem, so the actual process time can
exceed this limit.}

\item{tolerance_value}{(\code{numeric(1)})\cr
A non-negative tolerance value. The alternating optimization terminates
if the absolute difference between the current function value and the one
before \code{tolerance_history} iterations is smaller than
\code{tolerance_value}.

Can be \code{0} for no value threshold.}

\item{tolerance_parameter}{(\code{numeric(1)})\cr
A non-negative tolerance value. The alternating optimization terminates if
the distance between the current estimate and the before
\code{tolerance_history} iterations is smaller than
\code{tolerance_parameter}.

Can be \code{0} for no parameter threshold.

By default, the distance is measured using the euclidean norm, but another
norm can be specified via the \code{tolerance_parameter_norm} argument.}

\item{tolerance_parameter_norm}{(\code{function})\cr
The norm that measures the distance between the current estimate and the
one from the last iteration. If the distance is smaller than
\code{tolerance_parameter}, the procedure is terminated.

It must be of the form \code{function(x, y)} for two vector inputs
\code{x} and \code{y}, and return a single \code{numeric} value.
By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
is used.}

\item{tolerance_history}{(\code{integer(1)})\cr
The number of iterations to look back to determine whether
\code{tolerance_value} or \code{tolerance_parameter} has been reached.}

\item{base_optimizer}{(\code{Optimizer} or \code{list()})\cr
An \code{Optimizer} object, which can be created via
\code{\link[optimizeR]{Optimizer}}. It numerically solves the sub-problems.

By default, the \code{\link[stats]{optim}} optimizer is used. If another
optimizer is specified, the arguments \code{gradient}, \code{lower}, and
\code{upper} are ignored.

This can also be a \code{list} of multiple base optimizers, see details.}

\item{verbose}{(\code{logical(1)})\cr
Whether to print tracing details during the alternating optimization
process.}

\item{hide_warnings}{(\code{logical(1)})\cr
Whether to hide warnings during the alternating optimization process.}
}
\value{
A \code{list} with the following elements:
\itemize{
\item \code{estimate} is the parameter vector at termination.
\item \code{value} is the function value at termination.
\item \code{details} is a \code{data.frame} with full information about the procedure:
For each iteration (column \code{iteration}) it contains the function value
(column \code{value}), parameter values (columns starting with \code{p} followed by
the parameter index), the active parameter block (columns starting with \code{b}
followed by the parameter index, where \code{1} stands for a parameter contained
in the active parameter block and \code{0} if not), and computation times in
seconds (column \code{seconds})
\item \code{seconds} is the overall computation time in seconds.
\item \code{stopping_reason} is a message why the procedure has terminated.
}

In the case of multiple threads, the output changes slightly, see details.
}
\description{
Alternating optimization is an iterative procedure for optimizing a
real-valued function jointly over all its parameters by alternating
restricted optimization over parameter partitions.
}
\details{
\subsection{Multiple threads}{

Alternating optimization can suffer from local optima. To increase the
likelihood of reaching the global optimum, you can specify:
\itemize{
\item multiple starting parameters
\item multiple parameter partitions
\item multiple base optimizers
}

Use the \code{initial}, \code{partition}, and/or \code{base_optimizer} arguments to provide
a \code{list} of possible values for each parameter. Each combination of initial
values, parameter partitions, and base optimizers will create a separate
alternating optimization thread.
\subsection{Output value}{

In the case of multiple threads, the output changes slightly in comparison
to the standard case. It is a \code{list} with the following elements:
\itemize{
\item \code{estimate} is the optimal parameter vector over all threads.
\item \code{estimates} is a \code{list} of optimal parameters in each thread.
\item \code{value} is the optimal function value over all threads.
\item \code{values} is a \code{list} of optimal function values in each thread.
\item \code{details} combines details of the single threads and has an additional
column \code{thread} with an index for the different threads.
\item \code{seconds} gives the computation time in seconds for each thread.
\item \code{stopping_reason} gives the termination message for each thread.
\item \code{threads} give details how the different threads were specified.
}
}

\subsection{Parallel computation}{

By default, threads run sequentially. However, since they are independent,
they can be parallelized. To enable parallel computation, use the
\href{https://future.futureverse.org/}{\code{{future}} framework}. For example, run the
following \emph{before} the \code{ao()} call:
\preformatted{
future::plan(future::multisession, workers = 4)
}
}

\subsection{Progress updates}{

When using multiple threads, setting \code{verbose = TRUE} to print tracing
details during alternating optimization is not supported. However, you can
still track the progress of threads using the
\href{https://progressr.futureverse.org/}{\code{{progressr}} framework}. For example,
run the following \emph{before} the \code{ao()} call:
\preformatted{
progressr::handlers(global = TRUE)
progressr::handlers(
  progressr::handler_progress(":percent :eta :message")
)
}
}

}
}
\examples{
# Example 1: Minimization of Himmelblau's function --------------------------

himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
ao(f = himmelblau, initial = c(0, 0))

# Example 2: Maximization of 2-class Gaussian mixture log-likelihood --------

# target arguments:
# - class means mu (2, unrestricted)
# - class standard deviations sd (2, must be non-negative)
# - class proportion lambda (only 1 for identification, must be in [0, 1])

normal_mixture_llk <- function(mu, sd, lambda, data) {
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}

set.seed(123)

ao(
  f = normal_mixture_llk,
  initial = runif(5),
  target = c("mu", "sd", "lambda"),
  npar = c(2, 2, 1),
  data = datasets::faithful$eruptions,
  partition = list("sequential", "random", "none"),
  minimize = FALSE,
  lower = c(-Inf, -Inf, 0, 0, 0),
  upper = c(Inf, Inf, Inf, Inf, 1)
)

}
