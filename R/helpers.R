irc_met <- rbind(
  cbind(matrix(.7, 10,10), matrix(0, 10,10)),
  cbind(matrix(0, 10,10), matrix(0.7, 10,10))
)
diag(irc_met) <- 1
