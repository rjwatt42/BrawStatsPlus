runDebug<-function(IV,IV2,DV,effect,design,evidence,expected,result,expResult=NULL){
  
  
  rn2se<-function(r,n) {
    sqrt((1-r^2)/(n-2))
  }
  
  r2p<-function(r,n,ng=2){
    t_vals<-r/rn2se(r,n)
    if (ng>2) {
      (1-pt(t_vals^2,ng-1,n-ng))*2
    } else {
    (1-pt(abs(t_vals),n-2))*2
    }
  }
  
  formatR<-function(x,digits=2,t=FALSE,r=FALSE,g=FALSE) {
    # do green first
    if (g) {
      return(paste0("!g",format(round(x*(10^digits))/10^digits)))
    }
    if ((r) || (t && abs(x)>0.05)){
      return(paste0("\r",format(round(x*(10^digits))/10^digits)))
    }
    return(format(round(x*(10^digits))/10^digits))
  }
  
  op<-c()
  if (is.null(IV2)) {
    nc<-7
    op<-c(op,paste("\bMake sample:","n=",design$sN),"\bExpected","\bActual","","","","")
    op<-c(op,"r",formatR(effect$rIV),formatR(result$rIV),"","","","")
    
    op<-c(op,rep(" ",nc))
    op<-c(op,paste("\bMake ",expected$nSims ," samples:"),"\bExpected","\bActual","","","","")
    op<-c(op,"r",formatR(effect$rIV),formatR(mean(expResult$result$rIV)),"","","","")
    op<-c(op,"+/-",formatR(rn2se(effect$rIV,design$sN)),formatR(std(expResult$result$rIV)),"","","","")
    return(op)
  } else {
    nc<-12
  }
  
  e0<-0.1
  
  e1<-effect$rIV
  e2<-effect$rIV2
  ei<-effect$rIVIV2DV
  ec<-effect$rIVIV2
  efull<-sqrt(e1^2+e2^2+2*ec*e1*e2)
  Edirects<-c(e1,e2,ei)
  Etotals<-c(e1+ec*e2,e2+ec*e1,ei)
  Euniques<-c(e1*sqrt(1-ec^2),e2*sqrt(1-ec^2),ei)
  Rdirects<-c(mean(expResult$result$r$direct[,1]),mean(expResult$result$r$direct[,2]),mean(expResult$result$r$direct[,3]))
  Runiques<-c(mean(expResult$result$r$unique[,1]),mean(expResult$result$r$unique[,2]),mean(expResult$result$r$unique[,3]))
  Rtotals <-c(mean(expResult$result$r$total[,1]),mean(expResult$result$r$total[,2]),mean(expResult$result$r$total[,3]))
  RAdirects<-c(mean(abs(expResult$result$r$direct[,1])),mean(abs(expResult$result$r$direct[,2])),mean(abs(expResult$result$r$direct[,3])))
  RAuniques<-c(mean(abs(expResult$result$r$unique[,1])),mean(abs(expResult$result$r$unique[,2])),mean(abs(expResult$result$r$unique[,3])))
  RAtotals <-c(mean(abs(expResult$result$r$total[,1])),mean(abs(expResult$result$r$total[,2])),mean(abs(expResult$result$r$total[,3])))
  Ts<-c("rIV1","rIV2","rIVx")
  
  op<-c(op,paste("\bMake ",expected$nSims ," samples:"),"","\bExpected","","","","\bActual","","","","\bError","")
  op<-c(op,paste0("rIVIV2=",formatR(effect$rIVIV2)),"direct","unique","total","","direct","unique","total","","direct","unique","total")
  for (i in 1:3) {
    diffs<-c(abs((Edirects[i])-Rdirects[i]), abs((Euniques[i])-Runiques[i]), abs((Etotals[i])-Rtotals[i]))
    ndiffs<-c(abs(abs(Edirects[i])-abs(RAdirects[i])), abs(abs(Euniques[i])-abs(RAuniques[i])), abs(abs(Etotals[i])-abs(RAtotals[i])))
    re<-ndiffs>e0
    ge<-diffs>e0
    op<-c(op,Ts[i],
          formatR(Edirects[i],r=re[1],g=ge[1]),formatR(Euniques[i],r=re[2],g=ge[2]),formatR(Etotals[i],r=re[3],g=ge[3]),"",
          formatR(Rdirects[i],r=re[1],g=ge[1]),formatR(Runiques[i],r=re[2],g=ge[2]),formatR(Rtotals[i],r=re[3],g=ge[3]),"",
          formatR(diffs[1],r=re[1],g=ge[1]),formatR(diffs[2],r=re[2],g=ge[2]),formatR(diffs[3],r=re[3],g=ge[3])
    )
  }
  op<-c(op," ","","","","","","","","","","","")
  
  return(op)
}

