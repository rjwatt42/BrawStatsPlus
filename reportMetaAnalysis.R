
reportMetaAnalysis<-function(result){
  nc<-6

    # header
  outputText<-c("\bMeta Analysis",paste("nstudies=",format(result$metaAnalysis$nstudies)),paste("nsims=",format(length(result$bestDist)),sep=""),rep("",nc-3))
  outputText<-c(outputText,rep("",nc))
  
  outputText<-c(outputText," ","\bDistr","","\b\u03bb","\bp(0)","\bS")
  outputText<-c(outputText,"\bActual",result$effect$world$populationPDF,"",format(result$effect$world$populationPDFk,digits=3),format(result$effect$world$populationNullp,digits=3),"")
  
  n1<-sum(result$bestDist=="Single")
  n2<-sum(result$bestDist=="Gauss")
  n3<-sum(result$bestDist=="Exp")
  use<-which.max(c(n1,n2,n3))
  bestD<-c("Single","Gauss","Exp")[use]
  outputText<-c(outputText,"\bBest",bestD,paste0(sum(result$bestDist==bestD),"/",length(result$bestDist)),format(mean(result$bestK),digits=3),format(mean(result$bestNull),digits=3),format(mean(result$bestS),digits=3))
  outputText<-c(outputText,rep(" ",nc))
  
  if (result$count==1) {
    if (result$metaAnalysis$meta_pdf=="Single" || (result$metaAnalysis$meta_pdf=="All" && includeSingle)) {
      outputText<-c(outputText,"\bEstimated","Single",format(n1),
                  paste0(format(mean(result$single$Kmax),digits=3)),
                  paste0(format(mean(result$single$Nullmax),digits=3)),
                  paste0(format(mean(result$single$Smax),digits=3))
                  )
    }
    if (result$metaAnalysis$meta_pdf=="Gauss" || result$metaAnalysis$meta_pdf=="All") {
        outputText<-c(outputText," ","Gauss",format(n2),
                  paste0(format(mean(result$gauss$Kmax),digits=3)),
                  paste0(format(mean(result$gauss$Nullmax),digits=3)),
                  paste0(format(mean(result$gauss$Smax),digits=3))
                  )
      }
    if (result$metaAnalysis$meta_pdf=="Exp" || result$metaAnalysis$meta_pdf=="All") {
          outputText<-c(outputText," ","Exp",format(n3),
                  paste0(format(mean(result$exp$Kmax),digits=3)),
                  paste0(format(mean(result$exp$Nullmax),digits=3)),
                  paste0(format(mean(result$exp$Smax),digits=3))
                  )
        }
  } else {
    if (result$metaAnalysis$meta_pdf=="Single" || (result$metaAnalysis$meta_pdf=="All" && includeSingle)) {
      outputText<-c(outputText,"\bEstimated","Single",format(n1),
                  paste0(format(mean(result$single$Kmax),digits=3),"\u00B1",format(std(result$single$Kmax),digits=2)),
                  paste0(format(mean(result$single$Nullmax),digits=3),"\u00B1",format(std(result$single$Nullmax),digits=2)),
                  paste0(format(mean(result$single$Smax),digits=3),"\u00B1",format(std(result$single$Smax),digits=2))
                  )
    }
    if (result$metaAnalysis$meta_pdf=="Gauss" || result$metaAnalysis$meta_pdf=="All") {
        outputText<-c(outputText," ","Gauss",format(n2),
                  paste0(format(mean(result$gauss$Kmax),digits=3),"\u00B1",format(std(result$gauss$Kmax),digits=2)),
                  paste0(format(mean(result$gauss$Nullmax),digits=3),"\u00B1",format(std(result$gauss$Nullmax),digits=2)),
                  paste0(format(mean(result$gauss$Smax),digits=3),"\u00B1",format(std(result$gauss$Smax),digits=2))
                  )
      }
    if (result$metaAnalysis$meta_pdf=="Exp" || result$metaAnalysis$meta_pdf=="All") {
      outputText<-c(outputText," ","Exp",format(n3),
                  paste0(format(mean(result$exp$Kmax),digits=3),"\u00B1",format(std(result$exp$Kmax),digits=2)),
                  paste0(format(mean(result$exp$Nullmax),digits=3),"\u00B1",format(std(result$exp$Nullmax),digits=2)),
                  paste0(format(mean(result$exp$Smax),digits=3),"\u00B1",format(std(result$exp$Smax),digits=2))
                  )
    }
  }

  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
