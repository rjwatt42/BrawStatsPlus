graphInference<-function(IV,IV2,DV,effect,design,evidence,result,graphType="EffectSize") {
  

  switch (graphType,
          "r"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"r")
            return(joinPlots(g1))
          },
          "p"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
            return(joinPlots(g1))
          },
          "EffectSize"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"r")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "Power"= {
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"w")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"nw")
          },
          "2D"= {
            g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,result,"r","p")
            return(joinPlots(g1))
          },
          "log(lrs)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrs)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "log(lrd)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrd)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          }
  )
  g<-joinPlots(g1,g2)
  return(g)
}