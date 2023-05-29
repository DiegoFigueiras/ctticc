ctticc<-function(data, item, plot, ncol=2, nrow=3){
  library(psych)

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
  pseudob<-df$PseudoB[item]
  pseudoa<-df$PseudoA[item]
  df$inum<-row.names(df)

  eq <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}          #FUNCTION THAT CREATES ICC BASED ON pseudob AND pseudoa
  output<-cbind(pseudob, pseudoa)

  # if(plot==TRUE & item<2){
  #   p<- ggplot() + xlim(-4,4) + geom_function(fun=eq, data=df)  #PLOTTING CTT-ICC AND IRT-ICC SIDE BY SIDE.
  #   p

  # }


  if(plot=="separate"){ #type of plot1: all ICCs are plotted separately on different figures. Working as intended 5/26/2023
    p <- Map(function(A, B, item_lab) {
      ggplot(df, aes(color=item_lab)) +
        ylim(0,1)+
        geom_function(fun = function(x) {
          c + ((1 - c) * (1 / (1 + 2.71828^(-1.7*(A * (x-B))))))
        }) +
        scale_x_continuous(limits=c(-4,4), labels=c("Low Test Score","","Average Test Score","","High Test Score"))+
        labs(
             y = "p(1.0)",
             color="Item =")+
        theme(legend.position="top")
    }, df$PseudoA, df$PseudoB, df$inum)

    print(p[item])

  }




  if(plot=="together"){ #type of plot2: all ICCs are plotted in the same figure
    p<-0
    eq<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[1]*(x-df$PseudoB[1]))))))}
    p<-curve(eq, col="white", xlim=c(-4,4),ylim=c(0,1), xlab="Level of Trait", ylab="p(1.0)")
    colors<-rainbow(n = length(item))
    for(i in item){
      eq<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[i]*(x-df$PseudoB[i]))))))}
      p[i]<-curve(eq, col=colors[i], xlim=c(-4,4), ylim=c(0,1), main="Item Characteristic Curve", add=TRUE)
      p
      legend(x=-4, y=1, legend=colnames(data[item]), fill=colors[length(item)], pt.cex=0.5)
    }

  }

  if(plot=="grid"){ #type of plot2: all ICCs are plotted in the same figure but on a grid. Working as intended 5/26/2023
    p <- Map(function(A, B, item_lab) {
      ggplot(df, aes(color=item_lab)) +
        ylim(0,1)+
        geom_function(fun = function(x) {
          c + ((1 - c) * (1 / (1 + 2.71828^(-1.7*(A * (x-B))))))
        }) +
        scale_x_continuous(limits=c(-4,4), labels=c("Low Test Score","","Average Test Score","","High Test Score"))+
        labs(y = "p(1.0)",
           color="Item =")+
        theme(legend.justification=c(0,1), legend.position=c(0,1))
        ##theme(legend.position="top")
    }, df$PseudoA, df$PseudoB, df$inum)


    grid.arrange(grobs=c(p[item]), ncol=ncol, nrow=nrow)

  }



  return(output)
}

data<-read.csv("testdata.csv")
ctticc(data, 7:10, plot="grid", ncol=2,nrow=2)

