\name{mdml}
\alias{mdml}
\alias{print.mdml}

\title{Minimum Discrepancy Maximum Likelihood Estimation}
\description{
  Minimum discrepancy maximum likelihood estimation in probabilistic knowledge
  structures.
}
\usage{
mdml(K, N.R, R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  pi = NULL, beta = NULL, eta = NULL, 
  errtype = c("both", "error", "guessing"), errequal = FALSE, radius.inc = 0,
  method = c("MD", "ML", "MDML"), tol=1e-7, maxiter = 10000)
}

\arguments{
  \item{K}{knowledge structure, a (named) matrix of zeroes and ones}
  \item{N.R}{(named) vector of absolute frequencies of response patterns}
  \item{R}{response patterns, a matrix of zeros and ones.  Per default
    inferred from the names of \code{N.R}}
  \item{pi}{vector of initial parameter values for probabilities pi of
    knowledge states}
  \item{beta}{vector of initial parameter values for probabilities beta of a
    careless error}
  \item{eta}{vector of initial parameter values for probabilities eta of a
    lucky guess}
  \item{errtype}{type of response errors that can occur: \code{error} for
    careless errors only, \code{guessing} for lucky guesses only, and
    \code{both} for both error types}
  \item{errequal}{logical, if \code{TRUE} then items have identical error
    rates}
  \item{radius.inc}{include knowledge states of distance from the minimum
    discrepant states less than or equal to \code{radius.inc}}
  \item{method}{\code{MD} for minimum discrepancy estimation, \code{ML} for
    maximum likelihood estimation, \code{MDML} for minimum discrepancy
    maximum likelihood estimation}
  \item{tol}{tolerance, stopping criterion for iteration}
  \item{maxiter}{maximum number of iterations}
}

\details{}

\value{
  An object of class \code{mdml} having the following components:
  \item{discrepancy}{mean discrepancy}
  \item{pi}{vector of estimated parameter values for probabilities pi of
    knowledge states}
  \item{beta}{vector of estimated parameter values for probabilities beta of
    a careless error}
  \item{eta}{vector of estimated parameter values for probabilities eta of a
    lucky guess}
  \item{disc.tab}{matrix of symmetric set distances}
  \item{R}{matrix of response patterns}
  \item{nstates}{number of knowledge states}
  \item{npatterns}{number of response patterns}
  \item{ntotal}{number of respondents}
  \item{nerror}{number of response errors}
  \item{iter}{number of iterations needed}
  \item{loglike}{log-likelihood}
}

\references{
  Heller, J., & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the
  \emph{35th European Mathematical Psychology Group (EMPG) Meeting}, September
  2-4, Ghent, Belgium. 
}

\seealso{
  \code{\link{md}}.
}

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

mdml(K, N.R, method="MD")    # minimum discrepancy estimation
mdml(K, N.R, method="ML")    # maximum likelihood estimation by EM algorithm
mdml(K, N.R, method="MDML")  # MDML estimation
}
\keyword{models}
