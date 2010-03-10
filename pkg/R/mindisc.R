mindisc <- function(K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  method = c("minimum", "hypblc1", "hypblc2"), m = 1){
  # Minimum discrepancy estimation
  # Last mod: Mar/10/2010, FW

  N      <- sum(N.R)
  nitems <- ncol(K)
  npat   <- nrow(R)
  nstat  <- nrow(K)

  ## Assignment of state to response (change here for no guessing?)
  d.RK  <- t(apply(R, 1, function(r) apply(K, 1, function(q) sum(xor(q, r)))))
  min.d <- apply(d.RK, 1, min)

  method <- match.arg(method)
  i.RK   <- switch(method,
              minimum = d.RK == min.d,
              hypblc1 = 1/(1 + d.RK - min.d)^m,
              hypblc2 = 1/(1 + d.RK)^m)
  m.R    <- rowSums(i.RK)                             # sum_K i(R, K)
  f.KR   <- i.RK/m.R * as.integer(N.R)                # P(K|R) * N(R)

  ## Discrepancy index
  disc.tab <- xtabs(N.R ~ min.d)
  discrep  <- as.numeric(names(disc.tab)) %*% disc.tab / N

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
  z = list(discrepancy=c(discrep), P.K=P.K, beta=beta, eta=eta,
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
  cat("Distribution of discrepancies\n")
  print(obj$disc.tab)
  cat("Minimum discrepancy:", round(obj$discrepancy, digits=digits))
  cat("\n\n")
  cat("Distribution of knowledge states\n")
  printCoefmat(cbind("Pr(K)"=obj$P.K), digits=digits, cs.ind=1, tst.ind=NULL)
  cat("\n")
  cat("Careless error and lucky guess parameters\n")
  printCoefmat(cbind(beta=obj$beta, eta=obj$eta), digits=digits, cs.ind=1:2,
    tst.ind=NULL)
  cat("\n")
  invisible(obj)
}

## TO DO
# summary.mdisc()
# print.summary.mdisc()
# plot.mdisc()

