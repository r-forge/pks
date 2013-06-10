## Examples in Heller and Wickelmaier (2013) Elect. Notes Disc. Math. 42

## True structure
cK <- scan(what="", quiet=TRUE, text="
  0000
  0110
  0101
  1110
  1101
  1011
  1111
")
K <- t(sapply(strsplit(cK, ""), as.integer))
colnames(K) <- letters[seq_len(ncol(K))]
rownames(K) <- cK

## Misspecified structure
cK <- scan(what="", quiet=TRUE, comment.char="#", text="
  0000
  0110
# 0101
  1110
  1101
  1011
  1111
")
K2 <- t(sapply(strsplit(cK, ""), as.integer))
colnames(K2) <- letters[seq_len(ncol(K2))]
rownames(K2) <- cK

## Response patterns generated from BLIM based on K
N.R <- t(read.table(text="
     freq
  0000 15
  1000  4
  0100  6
  0010  4
  0001  5
  1100  2
  1010  3
  1001  4
  0110 18
  0101 22
  0011  2
  1110 39
  1101 37
  1011 12
  0111  7
  1111 20
"))[1,]

endm <- list(K=K, K2=K2, N.R=N.R)
rm(cK, K, K2, N.R)

