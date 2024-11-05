A <- matrix(c(28,32,8,9,49,
              7, 21, 35, 28, 10,
              47, 43, 15, 34, 2,
              48, 42, 19, 32, 26,
              45, 44, 39, 50, 26),
            nrow = 5,
            byrow = TRUE)

B <- matrix(c(0, 26, 3, 8, 30,
              35, 12, 19, 27, 27,
              27, 24, 12, 17, 29,
              31, 36, 40, 35, 8,
              24, 43, 31, 21, 39),
            nrow = 5,
            byrow = TRUE)

autovetor_A <- eigen(A)$vectors
autovetor_B <- eigen(B)$vectors

soma_autovetores <- autovetor_A + autovetor_B

P <- B %*% (t(B) %*% B) %*% t(B)

autovetor_P <- eigen(P)$vectors

soma_autovetor_P <- colSums(autovetor_P)

soma_autovetor_P

soma_autovetor_P_arredondada <- round(sum(soma_autovetor_P), 4)
soma_autovetor_P_arredondada




C <- solve(A %*% t(B))

soma_diagonal_C <- sum(abs(diag(C)))
soma_diagonal_C



A <- matrix(c(28,32,8,9,49,
              7, 21, 35, 28, 10,
              47, 43, 15, 34, 2,
              48, 42, 19, 32, 26,
              45, 44, 39, 50, 26),
            nrow = 5,
            byrow = TRUE)

soma_trian_inferior <- sum(A[lower.tri(A)])
soma_trian_inferior


log_determinante_A <- log10(abs(det(A)))
log_determinante_B <- log10(abs(det(B)))
log_determinanteAxB <- log10(abs(det(A%*%B)))

log_determinante_A
log_determinante_B
log_determinanteAxB



ABt <- A %*% t(B)

inv_ABt <- solve(ABt)

diagonal_ABt <- diag(inv_ABt)

maior_elemento_ABt <- max(diagonal_ABt)

maior_elemento_ABt