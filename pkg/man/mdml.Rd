\name{mdml}
\alias{mdml}
\alias{print.mdml}

\title{Minimum Discrepancy Maximum Likelihood Estimation}
\description{
  Minimum Discrepancy Maximum Likelihood estimation in probabilistic knowledge structures.
}
\usage{
PKS((K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)), pi = NULL, beta = NULL, eta = NULL, 
  type = c("both", "error", "guessing"), equal = FALSE, radius = 0, md = 1, em = 1, 
  tol=0.0000001, maxiter = 5000)
}

\arguments{
  \item{K}{knowledge structure}
  \item{N.R}{ ~~Describe \code{x} here~~ }
  \item{R}{response patterns}
  \item{pi}{vector of initial parameter values for probabilities pi of knowledge states}
  \item{beta}{vector of initial parameter values for probabilities beta of a careless error}
  \item{eta}{vector of initial parameter values for probabilities eta of a lucky guess}
  \item{type}{ ~~Describe \code{x} here~~ }
  \item{equal}{ ~~Describe \code{x} here~~ }
  \item{radius}{ ~~Describe \code{x} here~~ }
  \item{method}{ ~~Describe \code{x} here~~ }
  \item{tol}{ ~~Describe \code{x} here~~ }
  \item{maxiter}{ ~~Describe \code{x} here~~ }
}

\details{
  ~~ If necessary, more details than the description above ~~
}

\value{
  An object of class \code{"PKS"} having the following components:
  \item{discrepancy}{Description of 'comp1'}
  \item{pi}{vector of estimated parameter values for probabilities pi of knowledge states}
  \item{beta}{vector of estimated parameter values for probabilities beta of a careless error}
  \item{eta}{vector of estimated parameter values for probabilities eta of a lucky guess}
  \item{nerror}{Description of 'comp1'}
  \item{disc.tab}{Description of 'comp1'}
  \item{P.Kq}{Description of 'comp1'}
  \item{mat.RK}{Description of 'comp2'}
  \item{d.RK}{Description of 'comp2'}
  \item{R}{Description of 'comp2'}
  \item{nstates}{number of knowledge states}
  \item{npatterns}{number of response patterns}
  \item{ntotal}{number of respondents}
  \item{nerror}{number of response errors}
  \item{iter}{number of iterations needed}
  \item{loglike}{log-likelihood}
}

\references{
  Heller, J. & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the 35th
  European Mathematical Psychology Group (EMPG) Meeting, September 2-4, Ghent,
  Belgium. 
}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

mdml(K, N.R, method="ML")               # Maximum likelihood estimation by EM algorithm
mdml(K, N.R, method="MD")               # Minimum discrepancy estimation
mdml(K, N.R)			          # MDML estimation
}
\keyword{models}
