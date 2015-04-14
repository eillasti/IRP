library(gmm)
library(data.table)
library(car)
library(miscTools)

# testReturns = as.matrix(dt[, 2:10, with = FALSE])
# factors = as.matrix(dt[, 11:12, with = FALSE])

#' Returns moment conditions functions for gmm::gmm
#' 
#' testReturns : matrix T x N - returns of test assets with column names
#' factors : matrix T x K - factors with column names
#' 
#' Returns : f(theta : vector(Q), x(T x (N + K))) => T  
createMoments = function(testReturns, factors){
  factorNames = colnames(factors)
  
  b = rep(0.01, length(factorNames))
  #   b = c(0.3, -0.1)  
  names(b) = sapply(factorNames, function(factorName) paste0("b", factorName))
  
  mu = colMeans(factors)
  names(mu) = sapply(factorNames, function(factorName) paste0("mu", factorName))
  
  mCovF = cov(factors)
#   covF = as.vector(mCovF)
#   names(covF) = as.vector(outer(rownames(mCovF), colnames(mCovF), paste0))
  covF_triag = mCovF[upper.tri(mCovF, TRUE)]
  
  theta = c(b, mu, covF_triag)
  # theta = c(b, mu)#, covF)
  x = cbind(testReturns, factors)
  
  T_ == nrow(testReturns)
  K = ncol(factors)
  N = ncol(testReturns)
  # Q = K * 2 #+ K * K 
  Q = K * 2 + K * K - 1
  
#   browser()
  
  g = function(theta, x) {
    #     browser()
    assert_that(T_ == nrow(x))
    assert_that((N + K) == ncol(x))
    assert_that(Q == length(theta))
    
    T_ = nrow(x)
    N = ncol(x) - K
    Q = length(theta)
    
    rx = x[, 1:N]
    h = x[, (N + 1) : ncol(x)]
    
    b = theta[1:K]
    mu = theta[(K + 1) : (K + K)]
    hCov_v = theta[(2 * K + 1) : (2 * K + K ^ 2 - 1)]
    hCov = diag(K)
    hCov[upper.tri(hCov, diag = TRUE)] = hCov_v
#     hCov = matrix(theta[(2 * K + 1) : (2 * K + K ^ 2)], nrow = 2, ncol = 2)
    
    zzz2 = t(apply(h, 1, function(r) r - mu))
    #         zzz2 = matrix(h - mu, nrow = T_, ncol = K)
    zzz1 = rx * as.vector(1 - zzz2 %*% b)
    
#     browser()
      h_mu = zzz2[1,]
    hCov_t = function(h_mu) {
#       as.vector(matrix(h_mu, K, 1) %*% matrix(h_mu, 1, K) - hCov)
      estCov = matrix(h_mu, K, 1) %*% matrix(h_mu, 1, K) - hCov
      estCov[upper.tri(estCov, TRUE)]
    }
    zzz3 = t(apply(zzz2, 1, hCov_t))
    
    zzz = cbind(zzz1, zzz2, zzz3)
    #       zzz = matrix(cbind(zzz1, zzz2), T_, Q)
    #     zzz = cbind(zzz1, zzz2)
    #     if (nrow(zzz) != T_ | ncol(zzz) != Q) 
    #       stop(paste("Wrong moment conditions size:", nrow(zzz), "x", ncol(zzz), "while", 
    #                  T_, "x", Q, "expected."))
    
    #   t(zzz) %*% (zzz)
    zzz
    #   unique(is.finite(zzz))
  }
  
  list("g" = g, "theta" = theta, "x" = x)
}
