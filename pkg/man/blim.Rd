\name{blim}
\alias{blim}
\alias{print.blim}

\title{Basic Local Independence Models (BLIMs)}
\description{Fits a basic local independence model for probabilistic knowledge
  structures by minimum discrepancy maximum likelihood estimation.
}
\usage{
blim(K, N.R, method = c("MD", "ML", "MDML"),
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  P.K = rep(1/nstat, nstat), beta = rep(0.1, nitems), eta = rep(0.1, nitems),
  errtype = c("both", "error", "guessing"), errequal = FALSE, incradius = 0,
  tol=1e-7, maxiter = 10000)
}

\arguments{
  \item{K}{knowledge structure, a (named) matrix of zeroes and ones.}
  \item{N.R}{(named) vector of absolute frequencies of response patterns.}
  \item{method}{\code{MD} for minimum discrepancy estimation, \code{ML} for
    maximum likelihood estimation, \code{MDML} for minimum discrepancy
    maximum likelihood estimation}
  \item{R}{response patterns, a matrix of zeros and ones.  Per default
    inferred from the names of \code{N.R}.}
  \item{P.K}{vector of initial parameter values for probabilities of
    knowledge states.}
  \item{beta}{vector of initial parameter values for probabilities of a
    careless error.}
  \item{eta}{vector of initial parameter values for probabilities of a
    lucky guess.}
  \item{errtype}{type of response errors that can occur: \code{error} for
    careless errors only, \code{guessing} for lucky guesses only, and
    \code{both} for both error types.}
  \item{errequal}{logical, if \code{TRUE} then items have identical error
    rates.}
  \item{incradius}{include knowledge states of distance from the minimum
    discrepant states less than or equal to \code{incradius}.}
  \item{tol}{tolerance, stopping criterion for iteration.}
  \item{maxiter}{maximum number of iterations.}
}

\details{
  See Doignon and Falmagne (1999) for details on the basic local independence
  model (BLIM) for probabilistic knowledge structures.  See Heller and
  Wickelmaier (2004) for details on the parameter estimation methods.
}

\value{
  An object of class \code{blim} having the following components:
  \item{discrepancy}{mean discrepancy.}
  \item{P.K}{vector of estimated parameter values for probabilities of
    knowledge states.}
  \item{beta}{vector of estimated parameter values for probabilities of
    a careless error.}
  \item{eta}{vector of estimated parameter values for probabilities of a
    lucky guess.}
  \item{disc.tab}{matrix of symmetric set distances.}
  \item{K}{the knowledge structure.}
  \item{N.R}{the vector of frequencies of response patterns.}
  \item{nitems}{the number of items.}
  \item{nstates}{number of knowledge states.}
  \item{npatterns}{number of response patterns.}
  \item{ntotal}{number of respondents.}
  \item{nerror}{number of response errors.}
  \item{errtype}{type of response errors.}
  \item{method}{estimation method.}
  \item{iter}{number of iterations needed.}
  \item{loglike}{log-likelihood.}
}

\references{
  Doignon, J.-P. & Falmagne, J.-C. (1999). Knowledge spaces. Berlin:
  Springer.

  Heller, J., & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the
  \emph{35th European Mathematical Psychology Group (EMPG) Meeting}, September
  2-4, Ghent, Belgium. 
}

\seealso{
  \code{\link{md}}, \code{\link{simulate.blim}}.
}

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

blim(K, N.R, method="MD")    # minimum discrepancy estimation
blim(K, N.R, method="ML")    # maximum likelihood estimation by EM algorithm
blim(K, N.R, method="MDML")  # MDML estimation
}
\keyword{models}
