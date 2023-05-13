## Check for forward- and backward-gradedness

is.forward.graded <- function(K) {
  sapply(colnames(K), function(q) {
           K.plus <- K
           K.plus[, q] <- 1
           all(as.pattern(K.plus) %in% as.pattern(K))
         })
}


is.backward.graded <- function(K) {
  sapply(colnames(K), function(q) {
           K.minus <- K
           K.minus[, q] <- 0
           all(as.pattern(K.minus) %in% as.pattern(K))
         })
}


## Check for well-gradedness FIXME: this is not enough!

is.wellgraded <- function(K) {
  # - the states in K allow for at least one stepwise learning path
  # - the outer fringe is empty only for a single state (the domain)
  nitems <- ncol(K)
  uniqItemsPerK <- unique(rowSums(K))  # must be 0, 1, ..., nitems

  length(uniqItemsPerK) == nitems + 1 &&
  all(sort(uniqItemsPerK) == c(0, seq_len(nitems))) &&
  sum(rowSums(getKOfringe(K)) == 0) == 1
}

