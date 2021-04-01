
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IRCcheck: Irrepresentable Condition Check

[![CircleCI build
status](https://circleci.com/gh/donaldRwilliams/IRCcheck.svg?style=svg)](https://circleci.com/gh/donaldRwilliams/IRCcheck)

The goal of IRCcheck is to check the irrepresentable condition in both
L1-regularized regression (Equation 2 in Zhao and Yu 2006) and Gaussian
graphical models (Equation 8 in Ravikumar et al. 2008). At it crux, the
IRC states that the important and unimportant variables cannot be
correlated, at least not all that much (total irrelevant covariance
below 1).

L1-regularization requires the IRC for consistent model selection, that
is, with more data, the true model is recovered.

The IRC cannot be checked in real data. The primary use for this package
is to explore the IRC in a true model that may be used in a simulation
study. Alternatively, it is very informative to simply look at the IRC
as a function of sparsity and the number of variables, including the
regularization path and model selection.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("donaldRwilliams/IRCcheck")
```

## Example: IRC is met exactly

Here it is assumed that there is *no* covariance between the important
(active) and unimportant variables.

``` r
library(IRCcheck)
library(corrplot)
library(corpcor)
library(glmnet)
library(MASS)
library(psych)

# block diagonal
cors <- IRCcheck:::irc_met

# visualize
# note: first 10 are 'active'
corrplot(cors)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Let’s inspect the regularization path

``` r
# beta
beta <- c(rep(1, 10), rep(0, 10))

set.seed(2)
X <- MASS::mvrnorm(n = 500, mu = rep(0, 20), Sigma = cors)

# SNR = 5
sigma <- sqrt(as.numeric(crossprod(beta, cors %*% beta) / 5))

set.seed(2)
# note: first 10 are 'active'
y <- X %*% c(rep(1, 10), rep(0, 10)) + rnorm(500, 0, sigma)

# fit model
fit <- glmnet(X, y)

# visualize
plot(fit, xvar = "lambda")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Example: IRC is not met

``` r
# random correlation matrix
set.seed(2)
cors <- cov2cor(
  solve(
    rWishart(1, 20 , diag(20))[,,1]
  ))

# visualize
corrplot(cors)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Here is how to check the IRC in regression

``` r
# SNR = 5
sigma <- sqrt(as.numeric(crossprod(beta, cors %*% beta) / 5))

set.seed(2)
X <- MASS::mvrnorm(n = 500, mu = rep(0, 20), Sigma = cors)

# if negative it is not met
1 - irc_regression(X, 1:10)
#> [1] -0.9509886
```

Let’s inspect the regularization path

``` r
set.seed(2)
y <- X %*% c(rep(1, 10), rep(0, 10)) + rnorm(500, 0, sigma)

# fit model
fit <- glmnet(X, y)

# visualize
plot(fit, xvar = "lambda")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Quite the difference (e.g., all true coefficients are positive). Note
that the goal is then to select lambda, which will be quite the
difficult task when the IRC is not satisfied.

## Gaussian Graphical Models

For GGMs, I find it easier to work with a partial correlation matrix and
then randomly take subsets. The following looks at partial correlations
estimated from items assessing personality.

``` r
# partials from big 5 data
pcors <- corpcor::cor2pcor(cor(na.omit(psych::bfi[,1:25])))

# collect
irc <- NA 

for(i in 1:10){
  
   # randomly select 10
  id <- sample(1:25, size = 20, replace = F)
  
  # submatrix
  pcor_sub <- pcors[id, id]
  
  # true network
  true_net <- ifelse(abs(pcor_sub) < 0.05, 0, pcor_sub)
  
  irc[i] <- irc_ggm(true_net)
}

hist(1- irc, breaks = 100, 
     xlab = "1 - infinity norm", 
     main = "", xlim  = c(min(1 - irc), 1))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r

# failed
mean(1 - irc < 0)
#> [1] 1
```

Note that negative fails, as the irrelevant covariance exceeded 1. The
IRC will fail less often with fewer variables. Also, if `0.05` is
changed to a larger value this will result in more sparsity. As a
result, the IRC will be satisfied more often.

## References

<div id="refs" class="references">

<div id="ref-ravikumar2008model">

Ravikumar, Pradeep, Garvesh Raskutti, Martin J Wainwright, and Bin Yu.
2008. “Model Selection in Gaussian Graphical Models: High-Dimensional
Consistency of L1-Regularized Mle.” In *NIPS*, 1329–36.

</div>

<div id="ref-Zhao2006">

Zhao, Peng, and Bin Yu. 2006. “On Model Selection Consistency of Lasso.”
*The Journal of Machine Learning Research* 7: 2541–63.
<https://doi.org/10.1109/TIT.2006.883611>.

</div>

</div>
