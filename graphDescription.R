graphDescription<-function(IV,IV2,DV,effect,design,evidence,result) {

  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  switch (no_ivs, 
          {
            g<-joinPlots(drawDescription(IV,NULL,DV,effect,design,result))
          },
          { 
            if (evidence$rInteractionOn==FALSE){
              effect2<-effect
              effect2$rIV<-effect2$rIV2
              
              result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
              
              g1<-drawDescription(IV,NULL,DV,effect,design,result)
              g2<-drawDescription(IV2,NULL,DV,effect2,design,result2)
              g<-joinPlots(g1,g2)
            }
            else{
              if (showInteractionOnly){
                if (DV$type=="Categorical") {
                  if (IV2$type=="Interval") {
                    effect1<-effect
                    result1<-result
                    use<-result1$iv2<median(result$iv2)
                    result1$iv<-result$iv[use]
                    result1$dv<-result$dv[use]
                    result1$IVs$vals<-result$iv[use]
                    result1$DVs$vals<-result$dv[use]
                    
                    effect2<-effect
                    result2<-result
                    result2$iv<-result$iv[!use]
                    result2$dv<-result$dv[!use]
                    result2$IVs$vals<-result$iv[!use]
                    result2$DVs$vals<-result$dv[!use]
                    
                    g1<-drawDescription(IV,NULL,DV,effect1,design,result1)
                    g2<-drawDescription(IV2,NULL,DV,effect2,design,result2)
                    g<-joinPlots(g1,g2)
                  } else {
                    switch (IV2$ncats,
                            {},
                            {xmin<-c(0.5,5.5)
                            xmax<-c(4.5,9.5)
                            ymin<-c(0,0)
                            ymax<-c(5,5)},
                            {xmin<-c(0.5,5.5,3)
                            xmax<-c(4.5,9.5,7)
                            ymin<-c(0,0,5)
                            ymax<-c(4.25,4.25,9.25)},
                            {xmin<-c(0.5,5.5,0.5,5.5)
                            xmax<-c(4.5,9.5,4.5,9.5)
                            ymin<-c(0,0,5,5)
                            ymax<-c(4.25,4.25,9.25,9.25)},
                            {}
                    )
                    g1<-c()
                    for (i in 1:IV2$ncats) {
                      effect1<-effect
                      result1<-result
                      use<-result1$iv2<-as.numeric(result$iv2)==i
                      result1$iv<-result$iv[use]
                      result1$dv<-result$dv[use]
                      result1$IVs$vals<-result$iv[use]
                      result1$DVs$vals<-result$dv[use]
                      g1<-c(g1,drawDescription(IV,NULL,DV,effect1,design,result1))
                    }
                    g<-joinPlots(g1)
                  }
                  # effect2<-effect
                  # result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                } else {
                  g<-joinPlots(drawDescription(IV,IV2,DV,effect,design,result))
                }
              } else{
                effect2<-effect
                effect2$rIV<-effect2$rIV2
                
                result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, iv=result$iv, dv=result$dv, ivplot=result$iv2plot,dvplot=result$dvplot)
                g1<-drawDescription(IV,NULL,DV,effect,design,result)
                g2<-drawDescription(IV2,NULL,DV,effect2,design,result2)
                g3<-drawDescription(IV,IV2,DV,effect,design,result)
                g<-joinPlots(g1,g2,g3)
              }
            }
          }
  )
  return(g)
}