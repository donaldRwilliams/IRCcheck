% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irc_regression.R
\name{irc_regression}
\alias{irc_regression}
\title{Irrepresentable Condition: Regression}
\usage{
irc_regression(X, which_nonzero)
}
\arguments{
\item{X}{A matrix of dimensions \emph{n} (observations) by \emph{p} (variables).}

\item{which_nonzero}{Numeric vector with the location of the nonzero relations
(a.k.a., the active set).}
}
\value{
infinity norm (greater than 1 the IRC is violated)
}
\description{
Check the IRC in multiple regression, following Equation (2)
in \insertCite{Zhao2006}{IRCcheck}.
}
\note{
It is common to take 1 - the infinity norm, thereby indicating the IRC
is violated when the value is negative.
}
\examples{
\donttest{
# data
# note: irc_met (block diagonal; 1st 10 active)
cors  <- rbind(
cbind(matrix(.7, 10,10), matrix(0, 10,10)),
cbind(matrix(0, 10,10), matrix(0.7, 10,10))
)
diag(cors) <- 1


X <- MASS::mvrnorm(2500, rep(0, 20), Sigma = cors, empirical = TRUE)
 
# check IRC
irc_regression(X, which_nonzero = 1:10)  

# generate data
y <- X \%*\% c(rep(1,10), rep(0, 10)) + rnorm(2500)

fit <- glmnet::glmnet(X, y, lambda = seq(10, 0.01, length.out = 400))

# plot
plot(fit, xvar = "lambda")


# Example (more or less) from Zhao and Yu (2006)
# section 3.3

# number of predictors
p <- 2^4

# number active (q in Zhao and Yu 2006)
n_beta <- 4/8 * p

# betas
beta <- c(rep(1, n_beta), rep(0, p - n_beta))

check <- NA
for(i in 1:100){
  cors <- cov2cor(
    solve(
      rWishart(1, p , diag(p))[,,1]
    ))
  
  # predictors
  X <- MASS::mvrnorm(500, rep(0, p), Sigma = cors, empirical = TRUE)
  
  check[i] <- irc_regression(X, which_nonzero = which(beta != 0))
}

# less than 1
mean(check  < 1)

# or greater than 0
mean(1 - check > 0)
}
}
\references{
\insertAllCited{}
}
