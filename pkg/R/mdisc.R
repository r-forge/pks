mdisc <- function(K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  guessing = TRUE,
  method = c("minimum", "hypblc1", "hypblc2"), m = 1){
  # Minimum discrepancy estimation
  # Last mod: Mar/10/2010, FW

  N      <- sum(N.R)
  nitems <- ncol(K)
  npat   <- nrow(R)
  nstat  <- nrow(K)

  ## Assigning state K given response R (change here for no guessing?)
  d.RK  <- t(apply(R, 1, function(r) apply(K, 1, function(q) sum(xor(q, r)))))
  d.min <- apply(d.RK, 1, min)                         # minimum discrepancy

  method <- match.arg(method)
  i.RK   <- switch(method,
              minimum = d.RK == d.min,
              hypblc1 = 1/(1 + d.RK - d.min)^m,
              hypblc2 = 1/(1 + d.RK)^m)
  f.KR   <- i.RK/rowSums(i.RK) * as.integer(N.R)       # P(K|R) * N(R)

  ## Minimum discrepancy distribution 
  disc.tab <- xtabs(N.R ~ d.min)
  disc     <- as.numeric(names(disc.tab)) %*% disc.tab / N

  ## Distribution of knowledge states
  P.K <- colSums(f.KR)/N
  names(P.K) <-
    if(is.null(rownames(K))) apply(K, 1, paste, collapse="") else rownames(K)

  ## Careless error and guessing parameters
  beta <- eta <- numeric(nitems)
  names(beta) <- names(eta) <-         # FIX ME: names for nitems > 26
    if(is.null(colnames(K))) letters[1:nitems] else colnames(K)
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

print.mdisc <- function(obj, digits=max(3, getOption("digits") - 2), ...){
  cat("\n")
  cat("Minimum discrepancy estimation in probabilistic knowledge structures")
  cat("\n\nNumber of knowledge states:", obj$nstates)
  cat("\nNumber of response patterns:", obj$npatterns)
  cat("\nNumber of respondents:", obj$ntotal)
  cat("\n\n")
  cat("Minimum discrepancy distribution (Mean = ",
    round(obj$discrepancy, digits=digits), ")\n", sep="")
  disc.tab <- obj$disc.tab
  names(dimnames(disc.tab)) <- NULL
  print(disc.tab)
  cat("\n")
  cat("Distribution of knowledge states\n")
  printCoefmat(cbind("Pr(K)"=obj$P.K), digits=digits, cs.ind=1, tst.ind=NULL)
  cat("\n")
  cat("Error and guessing parameters\n")
  printCoefmat(cbind(beta=obj$beta, eta=obj$eta), digits=digits, cs.ind=1:2,
    tst.ind=NULL)
  cat("\n")
  invisible(obj)
}

## TO DO
# summary.mdisc()
# print.summary.mdisc()
# plot.mdisc()
# simulate.mdisc()

