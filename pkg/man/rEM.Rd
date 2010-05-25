\name{rEM}
\alias{rEM}
\alias{print.rEM}

\title{Minimum Discrepancy Estimation}
\description{
  Minimum discrepancy estimation in probabilistic knowledge structures.
}
\usage{
rEM((K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  pi = NULL, beta = NULL, eta = NULL, 
  type = c("both", "error", "guessing"), equal = FALSE, radius = 0,
  md = 1, em = 1, tol=0.0000001, maxiter = 5000)
}

\arguments{
  \item{K}{ ~~Describe \code{x} here~~ }
  \item{N.R}{ ~~Describe \code{x} here~~ }
  \item{R}{ ~~Describe \code{x} here~~ }
  \item{pi}{ ~~Describe \code{x} here~~ }
  \item{beta}{ ~~Describe \code{x} here~~ }
  \item{eta}{ ~~Describe \code{x} here~~ }
  \item{type}{ ~~Describe \code{x} here~~ }
  \item{equal}{ ~~Describe \code{x} here~~ }
  \item{radius}{ ~~Describe \code{x} here~~ }
  \item{md}{ ~~Describe \code{x} here~~ }
  \item{em}{ ~~Describe \code{x} here~~ }
  \item{tol}{ ~~Describe \code{x} here~~ }
  \item{maxiter}{ ~~Describe \code{x} here~~ }
}

\details{
  ~~ If necessary, more details than the description above ~~
}

\value{
  An object of class \code{"rEM"} having the following components:
  \item{discrepancy}{Description of 'comp1'}
  \item{pi}{Description of 'comp1'}
  \item{beta}{Description of 'comp1'}
  \item{eta}{Description of 'comp1'}
  \item{nerror}{Description of 'comp1'}
  \item{disc.tab}{Description of 'comp1'}
  \item{P.Kq}{Description of 'comp1'}
  \item{mat.RK}{Description of 'comp2'}
  \item{d.RK}{Description of 'comp2'}
  \item{R}{Description of 'comp2'}
  \item{nstates}{Description of 'comp2'}
  \item{npatterns}{Description of 'comp2'}
  \item{ntotal}{Description of 'comp2'}
  \item{iter}{Description of 'comp1'}
  \item{loglike}{Description of 'comp1'}
}

\references{
  Heller, J. & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the 35th
  European Mathematical Psychology Group (EMPG) Meeting, September 2-4, Ghent,,
  Belgium. 
}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

rEM(K, N.R)			               # restricted EM estimation
rEM(K, N.R, md=1, em=0)                # minimum discrepancy estimation
rEM(K, N.R, md=0, em=1)                # EM estimation
}
\keyword{models}
