
data<-read.csv("testdata.csv")
  library(psych)
  library(ggplot2)
  library(Rmisc)
  library(cowplot)
  pseudob<-0 #JUST CREATING A PLACEHOLDER FOR pseudob SO THE FUNCTION BELOW CAN RUN
  ahat<-function(x){
    r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)

    ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)+((0.57-(0.009*pseudob)+(0.19*pseudob^2))*r)

  }#FUNCTION TO ESTIMATE THE CTT-A STATISTIC, WHICH IS THE EQUIVALENT TO THE DISCRIMINATION STATISTIC IN IRT


  alphas<-psych::alpha(data, check.keys = TRUE)                #COMPUTING ALPHAS FOR ALL 100 ITEMS. WE NEED THIS IN ORDER TO GET THE CORRECTED ITEM-TOTAL CORRELATIONS, WHICH WE THEN USE FOR COMPUTING THE CTT-A STATISTIC.
  citcs<-data.frame(alphas$item.stats$r.drop)                  #ACCESSING THE CORRECTED ITEM-TOTAL CORRELATIONS INSIDE alphas.
  pseudoA<-data.frame(ahat(citcs))                             #USING THE ahat FUNCTION TO CALCULATE THE CTT-A PARAMETER FOR ALL 100 ITEMS. CORRECTED ITEM-TOTAL CORRELATION ARE ENTERED AS AN ARGUMENT.
  pseudoB.temp<-data.frame(qnorm(colMeans(data, na.rm=TRUE)))  #CALCULATING THE CTT-B PARAMETER, WHICH IS JUST THE PROBABILITIES OF ANSWERING RIGHT FOR EACH ITEM.
  pseudoB<- 0.000006957584+(-1.52731*pseudoB.temp)                                   ## from simulations (b ~ z_g; normal ability distribution)
  df<-as.data.frame(cbind(citcs, pseudoA, pseudoB))            #PUTTING ALL RELEVANT STATISTIC TOGETHER

  colnames(df)<-c("CITC", "PseudoA", "PseudoB")                #RENAMING COLUMN HEADERS
  c<-0
  pseudob<-df$PseudoB[1:5]
  pseudoa<-df$PseudoA[1:5]
  df$inum<-1:25

  eq <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}          #FUNCTION THAT CREATES ICC BASED ON pseudob AND pseudoa
  output<-cbind(pseudob, pseudoa)

  # if(plot==TRUE & item<2){
  #   p<- ggplot() + xlim(-4,4) + geom_function(fun=eq, data=df)  #PLOTTING CTT-ICC AND IRT-ICC SIDE BY SIDE.
  #   p

  # }
    p<-list()
    #eq<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[1]*(x-df$PseudoB[1]))))))}
    #p<-curve(eq, col="white", xlim=c(-4,4),ylim=c(0,1), xlab="Level of Trait", ylab="p(1.0)")
    colors<-rainbow(n = 25)
    for(i in 1:5){
      eq<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[i]*(x-df$PseudoB[i]))))))}
      p[[i]]<-ggplot(df)+geom_function(fun=eq)+scale_x_continuous(limits=c(-4,4),breaks=waiver(), labels=c("0.9","0.7","0.5","0.3","0.1"))+
        theme(legend.position=c(0.95,0.95), legend.justification=c("right","top"))+facet_grid(rows=vars(inum))# come back and make sure that those 0.9-0.1 are right

    }
    library(gridExtra)
    grid.arrange(grobs=c(p[1], p[2], p[3], p[4], p[5]), ncol=2, rnow=5)

    #par(mfrow=c(i, 1))


p<-ggplot(df)+
  for(i in 1:5){
    eq<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(PseudoA[i]*(x-PseudoB[i]))))))}
    geom_function(fun=eq)
  }

p+scale_x_continuous(limits=c(-4,4),breaks=waiver(), labels=c("0.9","0.7","0.5","0.3","0.1"))+
      theme(legend.position=c(0.95,0.95), legend.justification=c("right","top"))





eq<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[1]*(x-df$PseudoB[1]))))))}
#df<-df[1:2,]
p<-ggplot(df[1:2,])+
geom_function(fun=eq)
p+scale_x_continuous(limits=c(-4,4),breaks=waiver(), labels=c("0.9","0.7","0.5","0.3","0.1"))+
  theme(legend.position=c(0.95,0.95), legend.justification=c("right","top"))+facet_grid(rows=vars(df$inum[1]))
