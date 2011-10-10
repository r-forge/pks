\name{chess}
\alias{chess}
\title{Artificial Responses from Doignon and Falmagne (1999)}
\description{
  Fictitious data set from Doignon and Falmagne (1999, chap. 7).
  Response patterns of 1000 respondents to five problems. Each
  respondent is assumed to be in one of nine possible states of
  the knowledge structure \code{K}.
}
\usage{data(chess)}
\format{
  A list consisting of two components:
  \describe{
    \item{\code{K}}{A state-by-problem indicator matrix representing the
      hypothetical knowledge structure. An element is one if the problem
      is contained in the state, and else zero.}
    \item{\code{N.R}}{A named vector of type integer. The names denote
      responses patterns, the values denote their frequencies.}
  }
}
\source{
  Doignon, J.-P. & Falmagne, J.-C. (1999). Knowledge spaces. Berlin:
  Springer.
}
\examples{
data(chess)
chess$dst1  # knowledge structure DST1
chess$dst3  # knowledge structure DST3
chess$dst4  # knowledge structure DST4
chess$N.R   # response patterns
}
\keyword{datasets}
