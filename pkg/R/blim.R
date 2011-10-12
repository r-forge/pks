## MDML estimation of the basic local independence model (BLIM)
blim <- function(K, N.R, method = c("MD", "ML", "MDML"),
  R = t(sapply(strsplit(names(N.R), ""), as.numeric)),
  P.K = rep(1/nstat, nstat), beta = rep(0.1, nitems), eta = rep(0.1, nitems),
  errtype = c("both", "error", "guessing"), errequal = FALSE, incradius = 0,
  tol=0.0000001, maxiter = 10000) {

  N.R    <- setNames(as.integer(N.R), names(N.R))  # convert to named int
  N      <- sum(N.R)
  nitems <- ncol(K)
  npat   <- nrow(R)
  nstat  <- nrow(K)

  names(P.K) <-
    if(is.null(rownames(K))) apply(K, 1, paste, collapse="") else rownames(K)

  ## Assigning state K given response R
  em    <- switch(method <- match.arg(method), MD = 0, ML = 1, MDML = 1)
  md    <- switch(method, MD = 1, ML = 0, MDML = 1)
  d.RK  <- switch(errtype <- match.arg(errtype),
             both = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      sum(xor(q, r))))),
            error = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(q - r < 0)) NA else sum(q - r)))),
         guessing = t(apply(R, 1, function(r) apply(K, 1, function(q)
                      if(any(r - q < 0)) NA else sum(r - q)))))
  d.min <- apply(d.RK, 1, min, na.rm=TRUE)            # minimum discrepancy

  i.RK  <- (d.RK <= (d.min + incradius)) & !is.na(d.RK)

  ## Minimum discrepancy distribution 
  disc.tab <- xtabs(N.R ~ d.min)
  disc     <- as.numeric(names(disc.tab)) %*% disc.tab / N
  
  iter     <- 0
  maxdiff  <- 2 * tol

  while ((maxdiff > tol) && (iter < maxiter) &&
         ((md*(1 - em) != 1) || (iter == 0))) {
    pi.old   <- P.K
    beta.old <- beta
    eta.old  <- eta
    
    P.R.K  <- switch(errtype,
           both = t(apply(R, 1, function(r) apply(K, 1, function(q)
              prod(beta^((1-r)*q) * (1-beta)^(r*q) * eta^(r*(1-q)) * (1-eta)^((1-r)*(1-q)))))),
          error = t(apply(R, 1, function(r) apply(K, 1, function(q)
              prod(beta^((1-r)*q) * (1-beta)^(r*q) * 0^(r*(1-q)) * 1^((1-r)*(1-q)))))),
       guessing = t(apply(R, 1, function(r) apply(K, 1, function(q)
              prod(0^((1-r)*q) * 1^(r*q) * eta^(r*(1-q)) * (1-eta)^((1-r)*(1-q)))))))
    P.R    <- as.numeric(P.R.K %*% P.K)
    P.K.R  <- P.R.K * outer(1/P.R, P.K)         # prediction of P(K|R)
    mat.RK <- i.RK^md * P.K.R^em
    m.RK   <- (mat.RK / rowSums(mat.RK)) * N.R  # m.RK = E(M.RK) = P(K|R) * N(R)
    loglik <- sum(log(P.R) * N.R)

    ## Distribution of knowledge states
    P.K <- colSums(m.RK) / N

    ## Careless error and guessing parameters
    for(j in seq_len(nitems)) {
      beta[j] <- sum(m.RK[which(R[,j] == 0), which(K[,j] == 1)]) /
                 sum(m.RK[,which(K[,j] == 1)])

      eta[j]  <- sum(m.RK[which(R[,j] == 1), which(K[,j] == 0)]) /
                 sum(m.RK[,which(K[,j] == 0)])
    }

    maxdiff <- max(abs(c(P.K, beta, eta) - c(pi.old, beta.old, eta.old)))
    iter <- iter + 1
  }

  ## Mean number of errors
  P.Kq <- numeric(nitems)
  for(j in seq_len(nitems))
    P.Kq[j] <- sum(P.K[which(K[,j] == 1)])
  nerror <- c("careless error" = sum(beta * P.Kq),
                 "lucky guess" = sum( eta * (1 - P.Kq)))

  ## Global beta and eta parameters
  if (errequal) {
    beta <- rep(nerror[1] / sum(P.Kq),            nitems)
     eta <- rep(nerror[2] / (nitems - sum(P.Kq)), nitems)
  }

  names(beta) <- names(eta) <-
    if(is.null(colnames(K))) {
      make.unique(c("a", letters[(seq_len(nitems) %% 26) + 1])[-(nitems + 1)],
        sep="")
    } else
      colnames(K)

  ## Number of parameters
  npar <- length(P.K) - 1 +
    (if(errtype == "both") 2 else 1) * (if(errequal) 1 else nitems)

  z <- list(discrepancy=c(disc), P.K=P.K, beta=beta, eta=eta,
    disc.tab=disc.tab, K=K, N.R=N.R, nitems=nitems, nstates=nstat,
    npatterns=npat, ntotal=N, nerror=nerror, npar=npar, errtype=errtype,
    method=method, iter=iter, loglik=loglik)
  class(z) <- "blim"
  z
}


print.blim <- function(x, digits=max(3, getOption("digits") - 2), ...){
  cat("\n")
  cat("Basic local independence models (BLIMs)")
  method <- switch(x$method,
            MD = "Minimum discrepancy",
            ML = "Maximum likelihood",
          MDML = "Minimum discrepancy maximum likelihood")
  cat("\nMethod:", method)
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
  cat("\nlog-likelikood:", x$loglik)
  cat("\n\n")
  cat("Distribution of knowledge states\n")
  printCoefmat(cbind("Pr(K)"=x$P.K), digits=digits, cs.ind=1, tst.ind=NULL)
  cat("\n")
  cat("Error and guessing parameters\n")
  printCoefmat(cbind(beta=x$beta, eta=x$eta), digits=digits, cs.ind=1:2,
    tst.ind=NULL)
  cat("\n")
  invisible(x)
}


## Log-likelihood for blim objects
logLik.blim <- function(object, ...){
  if(length(list(...)))
      warning("extra arguments discarded")
  p <- object$npar
  val <- object$loglik
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}


## Simulate responses from BLIM
simulate.blim <- function(object, nsim = 1, seed = NULL, ...){
     P.K <- object$P.K
    beta <- object$beta
     eta <- object$eta
       K <- object$K
  nitems <- object$nitems
       N <- object$ntotal

  states <- sample(rownames(K), N, replace=TRUE, prob=P.K)  # draw states

  P.1.K <- K*(1 - beta) + (1 - K)*eta              # Pr(resp = 1 | K)
  R     <- matrix(numeric(N * nitems), N, nitems)  # response matrix
  for(i in seq_along(states))
    R[i,] <- rbinom(nitems, 1, P.1.K[states[i],])  # draw a response

  N.R <- table(apply(R, 1, paste, collapse=""))
  setNames(as.integer(N.R), names(N.R))            # convert to named int
}

