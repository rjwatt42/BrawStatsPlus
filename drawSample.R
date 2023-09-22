drawSample<-function(IV,DV,effect,result){
  
  # the scattered points
  result$Heteroscedasticity<-effect$Heteroscedasticity
  g<-drawPopulation(IV,DV,result,alpha=0.75,theme=plotTheme)

  dotSize<-dotSize<-(plotTheme$axis.title$size)/3
  if (result$nval>100) {
    dotSize<-dotSize*sqrt(100/length(x))
  }
  x<-result$ivplot
  y<-result$dvplot
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour = "black", fill = plotcolours$sampleC, size = dotSize)
  if (showMedians) {
    if (DV$type=="Categorical") {yuse<-0.5} else {yuse<-median(y)}
      g<-g+geom_hline(yintercept=yuse,col="red")
      if (IV$type=="Categorical") {xuse<-0.5} else {xuse<-median(x)}
      g<-g+geom_vline(xintercept=xuse,col="red")
  }
  g<-g+labs(x=IV$name,y=DV$name)+plotTheme
  g
  
}
