\name{getKOfringe}
\alias{getKOfringe}
\title{Outer Fringes of a Knowledge Space}
\description{
  Returns the outer fringe for each state in a well-graded knowledge space.
}
\usage{
getKOfringe(K, nstates = nrow(K), nitems = ncol(K))
}
\arguments{
  \item{K}{a state-by-problem indicator matrix representing the knowledge
    space.  An element is one if the problem is contained in the state,
    and else zero.}
  \item{nstates}{the number of knowledge states in \code{K}.}
  \item{nitems}{the number of items in \code{K}.}
}
\details{
  The outer fringe of a knowledge state is the set of all items that can be
  learned from that state, such that adding an outer-fringe item to the state
  results in another state in \code{K}.
}
\value{
  A state-by-problem indicator matrix representing the outer fringe for each
  knowledge state in \code{K}.
}
\seealso{
  \code{\link{slm}}, \code{\link{simulate.blim}}.
}
\examples{
data(DoignonFalmagne7)

## Which items can be learned from each state?
getKOfringe(DoignonFalmagne7$K)
}
\keyword{models}
