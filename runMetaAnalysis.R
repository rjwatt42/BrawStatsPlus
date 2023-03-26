source("runLikelihood.R")

getMaxS<-function(S,kvals) {
  np<-length(kvals)
  if (S[1]==max(S)) {
    return(kvals[1])
  }
  if (S[np]==max(S)) {
    return(kvals[np])
  }
  return(approx(diff(S),(kvals[1:(np-1)]+kvals[2:np])/2,0)$y)
}

findPmax<-function(zs,ns,distr,kvals,psigAnal,S) {
  nzp<-11
  pzvals<-seq(0,1,length.out=nzp)
  Nullmax<-0
  for (i in 1:nzp) {
    newS<-getLikelihood(zs,ns,distr,kvals,psigAnal,pzvals[i])
    if (max(newS,na.rm=TRUE)>max(S)) {
      S<-newS
      Nullmax<-pzvals[i]
    }
  }
  pzvals<-seq(-0.1,0.1,length.out=nzp)+Nullmax
  pzvals<-pzvals[pzvals>=0]
  pzvals<-pzvals[pzvals<=1]
  for (i in 1:length(pzvals)) {
    newS<-getLikelihood(zs,ns,distr,kvals,psigAnal,pzvals[i])
    if (max(newS)>max(S)) {
      S<-newS
      Nullmax<-pzvals[i]
    }
  }
  
  list(S=S,Nullmax=Nullmax)
}

runMetaAnalysis<-function(metaAnalysis,metaResult){
  rs<-metaResult$result$rIV
  zs<-atanh(rs)
  ns<-metaResult$result$nval
  np<-51
  
  
  if (metaAnalysis$meta_fixedAnal=="fixed") {
    kvals<-seq(-1,1,length.out=np)*0.95
    singleS<-getLikelihood(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,0)
    singleKmax<-approx(diff(singleS),(kvals[1:(np-1)]+kvals[2:np])/2,0)$y
    singleSmax<-max(singleS,na.rm=TRUE)
    singleNullmax<-0
    
    bestDist<-"Single"
    bestK<-singleKmax
    bestNull<-0
    bestS<-singleSmax
    
    gaussKmax<-NA
    gaussSmax<-NA
    gaussNullmax<-NA
    expKmax<-NA
    expSmax<-NA
    expNullmax<-NA
    
  } else {
    
    # doing random effects analysis
    singleSmax<-NA
    gaussSmax<-NA
    expSmax<-NA
    singleKmax<-NA
    gaussKmax<-NA
    expKmax<-NA
    singleNullmax<-NA
    gaussNullmax<-NA
    expNullmax<-NA
    
    # find best Single
    if (metaAnalysis$meta_pdf=="Single" || metaAnalysis$meta_pdf=="All") {
    kvals<-seq(-1,1,length.out=np)*0.95
    singleS<-getLikelihood(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,0)
    singleNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,singleS)
      singleS<-S$S
      singleNullmax<-S$Nullmax
    } 
    singleKmax<-getMaxS(singleS,kvals)
    singleSmax<-max(singleS,na.rm=TRUE)
    }
    
    # find best Gauss
    if (metaAnalysis$meta_pdf=="Gauss" || metaAnalysis$meta_pdf=="All") {
      kvals<-seq(0.01,1,length.out=np)
    gaussS<-getLikelihood(zs,ns,"Gauss",kvals,metaAnalysis$meta_psigAnal,0)
    gaussNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Gauss",kvals,metaAnalysis$meta_psigAnal,gaussS)
      gaussS<-S$S
      gaussNullmax<-S$Nullmax
    } 
    gaussKmax<-getMaxS(gaussS,kvals)
    gaussSmax<-max(gaussS,na.rm=TRUE)
    }
    
    # find best Exp
    if (metaAnalysis$meta_pdf=="Exp" || metaAnalysis$meta_pdf=="All") {
      kvals<-seq(0.01,1,length.out=np)
    expS<-getLikelihood(zs,ns,"Exp",kvals,metaAnalysis$meta_psigAnal,0)
    expNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Exp",kvals,metaAnalysis$meta_psigAnal,expS)
      expS<-S$S
      expNullmax<-S$Nullmax
    } 
    expKmax<-getMaxS(expS,kvals)
    expSmax<-max(expS,na.rm=TRUE)
    }
    
    use<-which.max(c(singleSmax,gaussSmax,expSmax))
    bestDist<-c("Single","Gauss","Exp")[use]
    bestK<-c(singleKmax,gaussKmax,expKmax)[use]
    bestNull<-c(singleNullmax,gaussNullmax,expNullmax)[use]
    bestS<-c(singleSmax,gaussSmax,expSmax)[use]
    
  }
  
  
  if (metaAnalysis$append) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestK<-c(metaResult$bestK,bestK)
    bestNull<-c(metaResult$bestNull,bestNull)
    bestS<-c(metaResult$bestS,bestS)
    singleKmax<-c(metaResult$single$kmax,singleKmax)
    singleSmax<-c(metaResult$single$Smax,singleSmax)
    singleNullmax<-c(metaResult$single$nullMax,singleNullmax)
    gaussKmax<-c(metaResult$gauss$kmax,gaussKmax)
    gaussSmax<-c(metaResult$gauss$Smax,gaussSmax)
    gaussNullmax<-c(metaResult$gauss$nullMax,gaussNullmax)
    expKmax<-c(metaResult$exp$kmax,expKmax)
    expSmax<-c(metaResult$exp$Smax,expSmax)
    expNullmax<-c(metaResult$exp$nullMax,expNullmax)
  }
  
  metaResult<-list(single=list(kmax=singleKmax,Smax=singleSmax,nullMax=singleNullmax),
                   gauss=list(kmax=gaussKmax,Smax=gaussSmax,nullMax=gaussNullmax),
                   exp=list(kmax=expKmax,Smax=expSmax,nullMax=expNullmax),
                   bestDist=bestDist,
                   bestK=bestK,
                   bestNull=bestNull,
                   bestS=bestS,
                   count=length(metaResult$bestDist)+1,
                   nsims=metaResult$nsims,
                   effect=effect,
                   design=design,
                   metaAnalysis=metaAnalysis,
                   result=metaResult$result
  )
  return(metaResult)
}



