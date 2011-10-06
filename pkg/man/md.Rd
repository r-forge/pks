\name{mdisc}
\alias{mdisc}
\alias{print.mdisc}

\title{Minimum Discrepancy Estimation}
\description{
  Minimum discrepancy estimation in probabilistic knowledge structures.
}
\usage{
mdisc(K, N.R, R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  type = c("both", "error", "guessing"),
  method = c("minimum", "hypblc1", "hypblc2"), m = 1)
}

\arguments{
  \item{K}{ ~~Describe \code{x} here~~ }
  \item{N.R}{ ~~Describe \code{x} here~~ }
  \item{R}{ ~~Describe \code{x} here~~ }
  \item{type}{ ~~Describe \code{x} here~~ }
  \item{method}{ ~~Describe \code{x} here~~ }
  \item{m}{ ~~Describe \code{x} here~~ }
}

\details{
  ~~ If necessary, more details than the description above ~~
}

\value{
  An object of class \code{"mdisc"} having the following components:
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

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{
data(DoignonFalmagne7)
K   <- DoignonFalmagne7$K    # knowledge structure
N.R <- DoignonFalmagne7$N.R  # frequencies of response patterns

mdisc(K, N.R)                # minimum discrepancy estimation
mdisc(K, N.R, type="error")  # no guessing parameters
}
\keyword{models}
