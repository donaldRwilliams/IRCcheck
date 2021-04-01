irc_met <- rbind(
  cbind(matrix(.7, 10,10), matrix(0, 10,10)),
  cbind(matrix(0, 10,10), matrix(0.7, 10,10))
)
diag(irc_met) <- 1

Gamma_SS <- function(adj, k, cl){
  diag(adj ) <- 1
  
  G1 <- which(adj == 1, arr.ind = F)
  G2 <- which(adj == 1, arr.ind = F)
  
  grid <- expand.grid(G1, G2)
  
  Gamma_SS <- matrix(
    parSapply(X = 1:nrow(grid), cl = cl, FUN = function(x){
      k[grid[x,1], grid[x,2]]
    }), nrow = length(G1), ncol = length(G2), byrow = F)
  
  return(Gamma_SS)
}

Gamma_ScS <- function(adj, k, cl){
  diag(adj) <- 1
  
  G1 <- which(adj == 0, arr.ind = F)
  G2 <- which(adj == 1, arr.ind = F)
  
  grid <- expand.grid(G1, G2)
  
  Gamma_ScS <- matrix(
    parSapply(X = 1:nrow(grid), cl = cl, FUN = function(x){
      k[grid[x,1], grid[x,2]]
    }), nrow = length(G1), ncol = length(G2), byrow = F)
  
  return(Gamma_ScS)
}

symm_mat <- function (x) {
  x[lower.tri(x)] <- t(x)[lower.tri(x)]
  x
}