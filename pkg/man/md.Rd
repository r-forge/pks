\name{md}
\alias{md}
\alias{print.md}

\title{Minimum Discrepancy Estimation}
\description{
  Minimum discrepancy estimation in probabilistic knowledge structures.
}
\usage{
md(K, N.R, R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  errtype = c("both", "error", "guessing"),
  incrule = c("minimum", "hypblc1", "hypblc2"), m = 1)
}

\arguments{
  \item{K}{knowledge structure, a (named) matrix of zeroes and ones.}
  \item{N.R}{(named) vector of absolute frequencies of response patterns.}
  \item{R}{response patterns, a matrix of zeros and ones.  Per default
    inferred from the names of \code{N.R}.}
  \item{errtype}{type of response errors that can occur: \code{error} for
    careless errors only, \code{guessing} for lucky guesses only, and
    \code{both} for both error types.}
  \item{incrule}{inclusion rule for knowledge states.  (See Details.)}
  \item{m}{exponent for hyperbolic inclusion rules.}
}

\details{
  Most functionality of \code{md} is also provided by \code{blim}, apart from
  the hyperbolic inclusion rules.  So, \code{md} may be removed in the future.

  FIX ME: explain hypblc inclusion rules.
}

\value{
  An object of class \code{"md"} having the following components:
  \item{discrepancy}{mean discrepancy between \code{K} and \code{N.R}.}
  \item{P.K}{vector of estimated parameter values for probabilities of
    knowledge states.}
  \item{beta}{vector of estimated parameter values for probabilities of a
    careless error.}
  \item{eta}{vector of estimated parameter values for probabilities of a
    lucky guess.}
  \item{disc.tab}{matrix of symmetric set distances.}
  \item{nstates}{number of knowledge states.}
  \item{npatterns}{number of response patterns.}
  \item{ntotal}{number of respondents.}
}

\references{
  Heller, J., & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the
  \emph{35th European Mathematical Psychology Group (EMPG) Meeting}, September
  2-4, Ghent, Belgium. 
}

\seealso{\code{\link{blim}}}

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

md(K, N.R)                   # minimum discrepancy estimation
md(K, N.R, errtype="error")  # no guessing parameters
}
\keyword{models}
