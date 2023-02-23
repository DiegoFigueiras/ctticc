data <- read.csv("C:\\Users\\ergadmin\\Downloads\\sample.csv")

data2 <- as.data.frame(ctticc::ctticc(data,4,plot=FALSE))

c <-0

eq <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(data2$pseudoa*(x-data2$pseudob))))))}

library(ggplot2)
base <- ggplot() + xlim(-4,4)
base + geom_function(fun=eq, data=data2)
