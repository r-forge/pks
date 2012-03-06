## Example in Doignon & Falmagne (1999, chap. 7)

cK <- scan(what="", quiet=TRUE, text="
  00000
  10000
  01000
  11000
  11100
  11010
  11110
  11101
  11111
")
K <- t(sapply(strsplit(cK, ""), as.integer))
colnames(K) <- letters[seq_len(ncol(K))]
rownames(K) <- cK

N.R <- t(read.table(text="
        freq
  00000   80
  10000   92
  01000   89
  00100    3
  00010    2
  00001    1
  11000   89
  10100   16
  10010   18
  10001   10
  01100   18
  01010   20
  01001    4
  00110    2
  00101    2
  00011    3
  11100   89
  11010   89
  11001   19
  10110   16
  10101   16
  10011    3
  01110   18
  01101   16
  01011    2
  00111    2
  11110   73
  11101   82
  11011   19
  10111   15
  01111   15
  11111   77
"))[1,]

DoignonFalmagne7 <- list(K=K, N.R=N.R)
rm(cK, K, N.R)

