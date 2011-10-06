## Minimum discrepancy estimation
mdisc <- function(K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  type = c("both", "error", "guessing"),
  method = c("minimum", "hypblc1", "hypblc2"), m = 1){

  N      <- sum(N.R)
  nitems <- ncol(K)
  npat   <- nrow(R)
  nstat  <- nrow(K)

  ## Assigning state K given response R
  d.RK  <- switch(match.arg(type),
             both = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      sum(xor(q, r))))),
            error = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(q - r < 0)) NA else sum(q - r)))),
         guessing = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(r - q < 0)) NA else sum(r - q)))))
  d.min <- apply(d.RK, 1, min, na.rm=TRUE)            # minimum discrepancy

  i.RK  <- switch(match.arg(method),
             minimum = (d.RK == d.min) &              !is.na(d.RK),
             hypblc1 = replace(1/(1 + d.RK - d.min)^m, is.na(d.RK), 0),
             hypblc2 = replace(1/(1 + d.RK)^m,         is.na(d.RK), 0))
  f.KR  <- i.RK/rowSums(i.RK) * as.integer(N.R)       # P(K|R) * N(R)

  ## Minimum discrepancy distribution 
  disc.tab <- xtabs(N.R ~ d.min)
  disc     <- as.numeric(names(disc.tab)) %*% disc.tab / N

  ## Distribution of knowledge states
  P.K <- colSums(f.KR)/N
  names(P.K) <-
    if(is.null(rownames(K))) apply(K, 1, paste, collapse="") else rownames(K)

  ## Careless error and guessing parameters
  beta <- eta <- numeric(nitems)
  names(beta) <- names(eta) <-
    if(is.null(colnames(K))){
      make.unique(c("a", letters[(1:nitems %% 26) + 1])[-(nitems + 1)], sep="")
    }else colnames(K)
  for(j in 1:nitems){
    beta[j] <- sum(f.KR[which(R[,j] == 0), which(K[,j] == 1)]) /
               sum(f.KR[,which(K[,j] == 1)])

    eta[j]  <- sum(f.KR[which(R[,j] == 1), which(K[,j] == 0)]) /
               sum(f.KR[,which(K[,j] == 0)])
  }
  z = list(discrepancy=c(disc), P.K=P.K, beta=beta, eta=eta,
    disc.tab=disc.tab, nstates=nstat, npatterns=npat, ntotal=N)
  class(z) <- "mdisc"
  z
}

print.mdisc <- function(x, digits=max(3, getOption("digits") - 2), ...){
  cat("\n")
  cat("Minimum discrepancy estimation in probabilistic knowledge structures")
  cat("\n\nNumber of knowledge states:", x$nstates)
  cat("\nNumber of response patterns:", x$npatterns)
  cat("\nNumber of respondents:", x$ntotal)
  cat("\n\n")
  cat("Minimum discrepancy distribution (Mean = ",
    round(x$discrepancy, digits=digits), ")\n", sep="")
  disc.tab <- x$disc.tab
  names(dimnames(disc.tab)) <- NULL
  print(disc.tab)
  cat("\n")
  cat("Distribution of knowledge states\n")
  printCoefmat(cbind("Pr(K)"=x$P.K), digits=digits, cs.ind=1, tst.ind=NULL)
  cat("\n")
  cat("Error and guessing parameters\n")
  printCoefmat(cbind(beta=x$beta, eta=x$eta), digits=digits, cs.ind=1:2,
    tst.ind=NULL)
  cat("\n")
  invisible(x)
}

## TO DO
# summary.mdisc()
# print.summary.mdisc()
# plot.mdisc()
# simulate.mdisc()
