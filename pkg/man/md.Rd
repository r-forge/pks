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
  \item{K}{ ~~Describe \code{x} here~~ }
  \item{N.R}{ ~~Describe \code{x} here~~ }
  \item{R}{ ~~Describe \code{x} here~~ }
  \item{errtype}{ ~~Describe \code{x} here~~ }
  \item{incrule}{ ~~Describe \code{x} here~~ }
  \item{m}{ ~~Describe \code{x} here~~ }
}

\details{
  Most functionality of \code{md} is also provided by \code{mdml} (apart from
  the \code{hypblc1} and \code{hypblc2} inclusion rules. So, \code{md} may be
  removed in the long run.
}

\value{
  An object of class \code{"md"} having the following components:
  \item{discrepancy}{Description of 'comp1'}
  \item{P.K}{Description of 'comp2'}
  \item{beta}{Description of 'comp2'}
  \item{eta}{Description of 'comp2'}
  \item{disc.tab}{Description of 'comp2'}
  \item{nstates}{Description of 'comp2'}
  \item{npatterns}{Description of 'comp2'}
  \item{ntotal}{Description of 'comp2'}
}

\references{
  Heller, J. & Wickelmaier, F. (2004). Parameter estimation in probabilistic
  knowledge structures: the minimum-discrepancy method. Presented at the 35th
  European Mathematical Psychology Group (EMPG) Meeting, September 2-4, Ghent,
  Belgium. 
}

\seealso{\code{\link{mdml}}}

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

md(K, N.R)                   # minimum discrepancy estimation
md(K, N.R, errtype="error")  # no guessing parameters
}
\keyword{models}
