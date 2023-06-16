## problem-solving pseudob - 6/16/23

data <- read.csv("testdata.csv")

pseudoB.temp<-data.frame(qnorm(colMeans(data, na.rm=TRUE)))

pvals <- as.data.frame(psych::describe(data)$mean)

together <- cbind(pseudoB.temp,pvals)

colnames(together)[1] <- "pseudoB"
colnames(together)[2] <- "pvals"

together$g <- abs(together$pvals - .5)
