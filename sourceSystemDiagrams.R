##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram

output$HypothesisPlot<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("HypothesisPlot")
  
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (variablesHeld=="Data") {
    if (IV$deploy=="Within" && !grepl(paste0(",",DV$name,","),IV$targetDeploys)) {
      hmm(paste0("Warning: ", IV$name," requires matched DV (",substr(IV$targetDeploys,2,nchar(IV$targetDeploys)-1),")"))
    }
    if (DV$deploy=="Within") {
      hmm(paste0("Warning: ", DV$name," is a within-participants IV and cannot be used as a DV"))
    }
  }
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  effect<-updateEffect()
  
  PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  xmin<-2
  xmax<-8
  switch (no_ivs,
          {
            g<-PlotNULL+
              annotation_custom(grob=ggplotGrob(drawVariable(IV)),xmin=xmin,xmax=xmax,ymin=6,ymax=10)+
              annotation_custom(grob=ggplotGrob(drawVariable(DV)),xmin=xmin,xmax=xmax,ymin=0,ymax=4)
            # arrow
            g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,1)),xmin=xmin,xmax=xmax,ymin=3.5,ymax=6)
          },
          {
            g<-PlotNULL+
              annotation_custom(grob=ggplotGrob(drawVariable(IV)), xmin=0,  xmax=4,  ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(drawVariable(IV2)),xmin=6,  xmax=10, ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(drawVariable(DV)), xmin=3,  xmax=7,  ymin=0.5, ymax=3.5)
            # arrows
            g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,2)),xmin=1.5,xmax=5.5,ymin=3, ymax=7)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV2,3)),xmin=4.5,xmax=8.5,ymin=3, ymax=7)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2,4)),xmin=3,  xmax=7,  ymin=6, ymax=9)+
              annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2DV,5)),xmin=3,  xmax=7,  ymin=3, ymax=7)
          }
  )
  if (debug) debugPrint("HypothesisPlot - exit")

  g
}
)

# world diagram
output$WorldPlot<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("WorldPlot")
  
  effect<-updateEffect()

  PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  switch(RZ,
         "r"={range<-r_range},
         "z"={range<-tanh(z_range)}
  )
  if (effect$world$worldAbs) {
    rx<-seq(0,1,length.out=worldNPoints)*range
  } else {
    rx<-seq(-1,1,length.out=worldNPoints)*range
  }
  
  rdens<-fullRPopulationDist(rx,effect$world)
  
  if (RZ=="z") {
    rdens<-rdens2zdens(rdens,rx)
    rx<-atanh(rx)
  }
  rx<-c(rx[1],rx,rx[length(rx)])
  rdens<-c(0,rdens,0)
  pts=data.frame(x=rx,y=rdens)
  g1<-ggplot(pts,aes(x=x,y=y))
  g1<-g1+geom_polygon(data=pts,aes(x=x,y=y),fill=plotcolours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g1<-g1+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  switch(RZ,
         "r"={ g1<-g1+labs(x=rpLabel,y="Density")+diagramTheme },
         "z"={ g1<-g1+labs(x=zpLabel,y="Density")+diagramTheme }
         )
  
  if (debug) debugPrint("WorldPlot - exit")

  g<-g1

  g
}
)


output$WorldPlot2<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs)
  design<-updateDesign()
  if (debug) debugPrint("WorldPlot2")
  
  if (design$sNRand) {
    nbin<-seq(minN,maxRandN*design$sN,length.out=worldNPoints)
    # nbin<-minN+seq(0,qgamma(0.99,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK),length.out=101)
    ndens<-dgamma(nbin-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
    ndens<-ndens/max(ndens)
  } else {
    nbin<-seq(1,250,length.out=worldNPoints)
    ndens<-nbin*0+0.01
    use=which.min(abs(nbin-input$sN))
    ndens[use]<-1
  }
  x<-c(min(nbin),nbin,max(nbin))
  y<-c(0,ndens,0)
  
  pts=data.frame(x=x,y=y)
  g2<-ggplot(pts,aes(x=x,y=y))
  g2<-g2+geom_polygon(data=pts,aes(x=x,y=y),fill=plotcolours$descriptionC)+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
  g2<-g2+geom_line(data=pts,aes(x=x,y=y),color="black",lwd=0.25)
  g2<-g2+labs(x="n",y="Density")+diagramTheme
  
  g<-g2
  
    if (debug) debugPrint("WorldPlot2 - exit")
  
  g
  }
)

# population diagram
output$PopulationPlot <- renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("PopulationPlot")

  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  effect<-updateEffect()
  
  switch (no_ivs,
          {
            g<-drawPopulation(IV,DV,effect,alpha=1)
          },
          { 
            effect1<-effect
            effect2<-effect1
            effect2$rIV<-effect2$rIV2
            effect3<-effect1
            effect3$rIV<-effect3$rIVIV2
            
            g<-joinPlots(
              drawPopulation(IV,DV,effect1,alpha=1),
              drawPopulation(IV2,DV,effect2,alpha=1),
              drawPopulation(IV,IV2,effect3,alpha=1)
            )
          }
  )
  if (debug) debugPrint("PopulationPlot - exit")
  g
})  

# prediction diagram
output$PredictionPlot <- renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("PredictionPlot")

  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  design<-updateDesign()
  effect<-updateEffect()
  evidence<-updateEvidence()
  
  switch (no_ivs,
          { if(effect$world$worldOn) {
            g<-drawWorldSampling(effect,design,sigOnly=evidence$sigOnly)
          } else {
            g<-drawPrediction(IV,IV2,DV,effect,design)
            }
          },
          {
            if (evidence$rInteractionOn==FALSE){
              effect1<-effect
              effect2<-effect
              effect2$rIV<-effect2$rIV2
              
              g<-joinPlots(
                drawPrediction(IV,NULL,DV,effect1,design),
                drawPrediction(IV2,NULL,DV,effect2,design)
              )
            } else{
              if (showInteractionOnly){
                g<-drawPrediction(IV,IV2,DV,effect,design)
              } else{
                effect1<-effect
                effect2<-effect
                effect2$rIV<-effect2$rIV2
                
                g<-joinPlots(
                  drawPrediction(IV,NULL,DV,effect1,design),
                  drawPrediction(IV2,NULL,DV,effect2,design),
                  drawPrediction(IV,IV2,DV,effect,design)
                )
              }
            }
          }
  )
  if (debug) debugPrint("PredictionPlot - exit")
  g
})  
##################################################################################    
