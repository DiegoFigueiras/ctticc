## replicating Baker (2001) item informtaion function formula (p. 109)

data <- data.frame(
  theta = c(seq(-3,3,1))
)

data$a2 <- 1.5^2                  ## a squared

data$ptheta <- 1/(1 + exp(-1.5*(data$theta - 1)))    ## `exp` is important here

data$q <- 1-data$ptheta

data$information <- data$a2 * data$ptheta * data$q
