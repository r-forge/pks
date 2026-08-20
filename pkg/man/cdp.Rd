\name{cdp}
\alias{cdp}
\alias{print.cdp}

\title{Competency Deletion Procedure (CDP)}
\description{The competency deletion procedure (CDP) reduces
  a set of competencies defined on a set of skills down to one of
  its possible reducts.
}
\usage{
cdp(Competencies, CS, prefRel = c("NSP", "MBP", "MSP", "FSP"), 
    DesignMat = NULL, verbose = FALSE)
}

\arguments{
  \item{Competencies}{a character vector representing the set of competencies
    which is to be reduced.}
  \item{CS}{a state-by-skill indicator matrix representing the competence
    structure. An element equals 1 if the skill is present in the state;
    otherwise, it equals 0.}
  \item{prefRel}{the deletion rule that determines the order in which
    competencies are deleted. It can either be a string defining one of the
    preference relations defined in Anselmi et al. (2024) -- except for
    "LNP" --, or an object of the class \code{DeletionRule}, which is a
    character vector containing all collections of equally preferred
    competencies as elements, ordered from the most to the least preferred
    collection.}
  \item{DesignMat}{an indicator matrix with the dimensions \code{nrow(CS)}
    \eqn{\times}{x} \code{nrow(CS)}. An element equals 1 if the corresponding
    pair of two competence states shall be discerned during the procedure;
    otherwise, it equals 0. Alternatively \code{NULL} can be chosen instead.}
  \item{verbose}{an argument deciding if the values \code{delta}, 
    \code{Competencies}, \code{CS} and \code{DesingMat} are printed in the 
    output object.}
}

\details{
  See Anselmi et al. (2022) for details on the competency deletion procedure 
  (CDP) for competence structures.

  See Anselmi et al. (2024) for details on the preference relations for the 
  competency deletion procedure (CDP) for competence structures.

  Using a manually defined preference relation for the argument \code{prefRel}
  requires a character vector with its class attribute set to
  \code{DeletionRule}. See examples.

  If \code{NULL} is used for the argument \code{DesignMat}, all possible pairs 
  of competence states (with respect to \code{CS}) are discerned.
}

\value{
  An object of class \code{cdp} having the following components:
  \item{reduct}{the vector of the final set of competencies which is a reduct.}
  \item{niter}{the number of deletions/iterations needed.}
  \item{ordered_deletions}{the competencies which were deleted, ordered by the 
    iteration in which they were deleted.}
  \item{delta}{a matrix of the discerned pairs of competence states in rows
    and the competencies from the reduct in columns. An entry in a row means
    that this pair of competence states can be discerned by the competency in
    which column is the entry.}
  \item{pref_rel}{the preference relation used as \code{DeletionRule} object.}
  \item{pref_rel_method}{the method used to create the preference relation.}
  \item{Competencies, CS, DesignMat}{See Arguments.}
}

\references{
  Anselmi, P., Heller, J., Stefanutti, L. & Robusto, E. (2022). 
  Constructing, improving, and shortening tests for skill assessment. 
  \emph{Journal of Mathematical Psychology, 106,}
  102621. https://doi.org/10.1016/j.jmp.2021.102621 

  Anselmi, P., Heller, J., Stefanutti, L., & Robusto, E. (2024). 
  Constructing tests for skill assessment with competence-based test 
  development. \emph{British Journal of Mathematical and Statistical 
  Psychology, 77}(3), 429-458. https://doi.org/10.1111/bmsp.12335
}

\seealso{
  \code{\link{delineate}}.
}

\examples{
## Create competence structure
CStr <- as.binmat(c("0000", "0100", "1100", "1010", "1110", "0111", "1111"))
rownames(CStr) <- as.pattern(CStr)

## Create competency set
T_ <- c("a", "b", "c", "ab", "ac", "ad", "bc", "bd", "cd", "abc", "abd",
        "acd", "bcd", "abcd")

## Create design matrices
DesignMat <- upper.tri(matrix(0, nrow(CStr), nrow(CStr))) + 0
colnames(DesignMat) <- rownames(DesignMat) <-
  as.pattern(CStr, useNames = TRUE)

DesignMat2 <- DesignMat
DesignMat2[1, ] <- c(0, 0, 0, 1, 1, 1, 1)
DesignMat2[3, ] <- c(0, 0, 0, 1, 1, 0, 0)

## Create manually defined preference relation
ManDef_PR <- c("a, ac, ad, abc, abd, abcd",
               "b, bc, cd, acd", 
               "ab, bd, bcd",
               "c") 
class(ManDef_PR) <- "DeletionRule"

## Classic examples from Anselmi et al. (2024)

## Basic version using preference relation "NSP"
cdp(Competencies = T_, CS = CStr, prefRel = "NSP")

## Larger output option and preference relation "FSP"
cdp(Competencies = T_, CS = CStr, prefRel = "FSP", verbose = TRUE)

## Classic complete design matrix and preference relation "MBP"
cdp(Competencies = T_, CS = CStr, prefRel = "MBP", DesignMat = DesignMat)

## Reduced design matrix and preference relation "MSP" 
cdp(Competencies = T_, CS = CStr, prefRel = "MSP", DesignMat = DesignMat2)

## Manually defined preference relation 
cdp(Competencies = T_, CS = CStr, prefRel = ManDef_PR)
}
\keyword{models}

