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

## restricted EM-Algorithm

rEM <- function(K, N.R,
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  pi = NULL, beta = NULL, eta = NULL, 
  type = c("both", "error", "guessing"), equal = FALSE, radius = 0,
  md = 1, em = 1, tol=0.0000001, maxiter = 5000){

  N      <- sum(N.R)
  nitems <- ncol(K)
  npat   <- nrow(R)
  nstat  <- nrow(K)
  if (is.null(beta)) beta=rep(0.1, nitems)
  if (is.null(eta)) eta=rep(0.1, nitems)
  names(beta) <- names(eta) <-
  if(is.null(colnames(K))) {
      make.unique(c("a", letters[(1:nitems %% 26) + 1])[-(nitems + 1)], sep="")
  } else 
   colnames(K)
  if (is.null(pi)) pi=rep(1/nstat, nstat)
  names(pi) <-
    if(is.null(rownames(K))) apply(K, 1, paste, collapse="") else rownames(K)

  ## Assigning state K given response R
  d.RK  <- switch(match.arg(type),
             both = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      sum(xor(q, r))))),
            error = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(q - r < 0)) NA else sum(q - r)))),
         guessing = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(r - q < 0)) NA else sum(r - q)))))
  d.min <- apply(d.RK, 1, min, na.rm=TRUE)            # minimum discrepancy

  i.RK  <- (d.RK <= (d.min + radius)) & !is.na(d.RK)

  ## Minimum discrepancy distribution 
  disc.tab <- xtabs(N.R ~ d.min)
  disc     <- as.numeric(names(disc.tab)) %*% disc.tab / N
  
  iter <- 0
  maxdiff <- 2 * tol
  while ((maxdiff > tol) && (iter < maxiter) && ((md*(1-em) != 1) || (iter == 0))) {
   pi.old <- pi
   beta.old <- beta
   eta.old <- eta
   
   P.R.K  <- t(apply(R, 1, function(r) apply(K, 1, function(q)
      prod(beta^((1-r)*q) * (1-beta)^(r*q) * eta^(r*(1-q)) * (1-eta)^((1-r)*(1-q))))))
   P.R <- as.numeric(P.R.K %*% pi)
   P.K.R <- P.R.K * outer(1/P.R,pi)      # prediction of P(K|R)
   mat.RK <- i.RK^md * P.K.R^em
   m.RK  <- (mat.RK / rowSums(mat.RK)) * as.integer(N.R)       # m.RK = E(M.RK) = P(K|R) * N(R)
   loglike <- sum(log(P.R) * as.integer(N.R))
   ## Distribution of knowledge states
   pi <- colSums(m.RK) / N

   ## Careless error and guessing parameters
   ce <- lg <- 0
   P.Kq <- numeric(nitems)
   for(j in 1:nitems) {
     beta[j] <- sum(m.RK[which(R[,j] == 0), which(K[,j] == 1)]) /
               sum(m.RK[,which(K[,j] == 1)])
     eta[j]  <- sum(m.RK[which(R[,j] == 1), which(K[,j] == 0)]) /
               sum(m.RK[,which(K[,j] == 0)])
     P.Kq[j] <- sum(as.numeric(pi[which(K[,j] == 1)]))
     ce <- ce + as.numeric(beta[j]) * P.Kq[j]
     lg <- lg + as.numeric(eta[j]) * (1 - P.Kq[j])
   }
   maxdiff <- max(abs(c(pi, beta, eta) - c(pi.old, beta.old, eta.old)))
   iter <- iter + 1
  }
  nerror <- c(ce, lg)
  names(nerror) <- c("careless error", "lucky guess")
  if (equal) {
    beta <- rep(sum(beta * P.Kq) / sum(P.Kq), nitems)
    eta <- rep(sum(eta * (1 - P.Kq)) / (nitems - sum(P.Kq)), nitems)
  } 
  z = list(discrepancy=c(disc), pi=pi, beta=beta, eta=eta, nerror=nerror,
    disc.tab=disc.tab, P.Kq=P.Kq, mat.RK=mat.RK, d.RK=d.RK, R=R, nstates=nstat, npatterns=npat, ntotal=N,
    iter=iter, loglike=loglike)
  class(z) <- "rEM"
  z
}

print.rEM <- function(x, digits=max(3, getOption("digits") - 2), ...){
  cat("\n")
  cat("Modified ML estimation in probabilistic knowledge structures")
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
  cat("Number of iterations:", x$iter)
  cat("\n")
  cat("\nMean number or errors (total = ",
    round(sum(x$nerror), digits=digits), ")\n", sep="")
  print(x$nerror)
  cat("\nlog-Likelikood:", x$loglike)
  cat("\n\n")
  cat("Distribution of knowledge states\n")
  printCoefmat(cbind("pi"=x$pi), digits=digits, cs.ind=1, tst.ind=NULL)
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
