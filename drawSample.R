drawSample<-function(IV,DV,effect,result){
  
  # the scattered points
  x<-result$ivplot
  y<-result$dvplot
  result$Heteroscedasticity<-effect$Heteroscedasticity
  g<-drawPopulation(IV,DV,result,alpha=0.75)

  dotSize<-dotSize<-(plotTheme$axis.title$size)/3

  if (length(x)>100) {
    dotSize<-dotSize*sqrt(100/length(x))
  }
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour = "black", fill = plotcolours$sampleC, size = dotSize)+
  labs(x=IV$name,y=DV$name)+plotTheme
  g
  
}
