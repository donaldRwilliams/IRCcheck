#' Irrepresentable Condition: Regression
#'
#' @param X A matrix of dimensions \emph{n} (observations) by \emph{p} (variables)
#' 
#' @param which_nonzero Numeric vector with the location of the nonzero relations 
#' (a.k.a., the active set)
#'
#' @return 1 - infinity norm (negative the IRC is violated)
#' 
#' @export
#' @importFrom MASS mvrnorm
#' 
#'
#' @examples
#' \donttest{
#' # data
#' # note: irc_met (block diagonal; 1st 10 active)
#' cors <- IRCcheck:::irc_met
#' X <- MASS::mvrnorm(2500, rep(0, 20), Sigma = cors, empirical = TRUE)
#'  
#' # check IRC
#' irc_regression(X, which_nonzero = 1:10)  
#' 
#' # generate data
#' y <- X %*% c(rep(1,10), rep(0, 10)) + rnorm(2500)
#'
#' fit <- glmnet::glmnet(X, y, lambda = seq(10, 0.01, length.out = 400))
#' 
#' # plot
#' plot(fit, xvar = "lambda")
#' 
#' }
irc_regression <- function(X, which_nonzero){ 
  
  # 'active'
  X_1 = X[, which_nonzero, drop=FALSE] 
  
  # 'irrelevant'
  X_2 = X[, -which_nonzero, drop=FALSE] 
  
  betas <- rep(1, length(which_nonzero))
  
  # see page 2551 
  irc <- t(t(X_1) %*% X_2) %*% solve(t(X_1) %*% X_1) %*% sign(betas) 
  
  # infinity norm 
  infinity_norm <-  norm(irc, type = "i" ) 
  
  # negative fails
  return(1 - infinity_norm)
}

