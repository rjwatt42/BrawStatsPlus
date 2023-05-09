source("runLikelihood.R")

getMaxS<-function(S,kvals) {
  np<-length(kvals)
  if (S[1]==max(S)) {
    return(kvals[1])
  }
  if (S[np]==max(S)) {
    return(kvals[np])
  }
  closeMax<-which.max(S)
  use<-closeMax+seq(-1,1)
  closerMax<-approx(diff(S[use]),(kvals[use[1:2]]+kvals[use[2:3]])/2,0)$y
  return(closerMax)
}

findPmax<-function(zs,ns,distr,kvals,psigAnal,S) {
  nzp<-11
  pzvals<-seq(0,1,length.out=nzp)
  Nullmax<-0
  for (i in 1:nzp) {
    newS<-getLogLikelihood(zs,ns,distr,kvals,pzvals[i],psigAnal)
    if (max(newS,na.rm=TRUE)>max(S)) {
      S<-newS
      Nullmax<-pzvals[i]
    }
  }
  pzvals<-seq(-0.1,0.1,length.out=nzp)+Nullmax
  pzvals<-pzvals[pzvals>=0]
  pzvals<-pzvals[pzvals<=1]
  for (i in 1:length(pzvals)) {
    newS<-getLogLikelihood(zs,ns,distr,kvals,pzvals[i],psigAnal)
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
  nkpoints<-13
  nnullpoints<-13
  niterations<-2
  reInc<-(nkpoints-1)/2/2
  
  if (metaAnalysis$meta_fixedAnal=="fixed") {
    kvals<-seq(-1,1,length.out=nkpoints)*0.95
    singleS<-getLogLikelihood(zs,ns,"Single",kvals,0,metaAnalysis$meta_psigAnal)
    singleKmax<-approx(diff(singleS),(kvals[1:(nkpoints-1)]+kvals[2:nkpoints])/2,0)$y
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
    
    if (metaAnalysis$meta_nullAnal) {
      startNullvals<-seq(0,1,length.out=nnullpoints)
    } else {
      startNullvals<-0
    }
    
    # find best Single
    if (metaAnalysis$meta_pdf=="Single" || metaAnalysis$meta_pdf=="All") {
      nullvals<-startNullvals
      kvals<-seq(-1,1,length.out=nkpoints)*0.95
      for (re in 1:niterations) {
        singleS<-getLogLikelihood(zs,ns,"Single",kvals,nullvals,metaAnalysis$meta_psigAnal)
        singleSmax<-max(singleS,na.rm=TRUE)
        use<-which(singleS==singleSmax, arr.ind = TRUE)
        singleNullmax<-nullvals[use[1,2]]
        singleKmax<-kvals[use[1,1]]
        kvals<-seq(kvals[max(1,use[1,1]-reInc)],kvals[min(use[1,1]+reInc,nkpoints)],length.out=nkpoints)
        nullvals<-seq(nullvals[max(1,use[1,2]-reInc)],nullvals[min(use[1,2]+reInc,nnullpoints)],length.out=nnullpoints)
      }
    }
    
    # find best Gauss
    if (metaAnalysis$meta_pdf=="Gauss" || metaAnalysis$meta_pdf=="All") {
      nullvals<-startNullvals
      kvals<-seq(0.01,1,length.out=nkpoints)
      for (re in 1:niterations) {
        gaussS<-getLogLikelihood(zs,ns,"Gauss",kvals,nullvals,metaAnalysis$meta_psigAnal)
        gaussSmax<-max(gaussS,na.rm=TRUE)
        use<-which(gaussS==gaussSmax, arr.ind = TRUE)
        gaussNullmax<-nullvals[use[1,2]]
        gaussKmax<-kvals[use[1,1]]
        kvals<-seq(kvals[max(1,use[1,1]-reInc)],kvals[min(use[1,1]+reInc,nkpoints)],length.out=nkpoints)
        nullvals<-seq(nullvals[max(1,use[1,2]-reInc)],nullvals[min(use[1,2]+reInc,nnullpoints)],length.out=nnullpoints)
      }
    }
    
    # find best Exp
    if (metaAnalysis$meta_pdf=="Exp" || metaAnalysis$meta_pdf=="All") {
      nullvals<-startNullvals
      kvals<-seq(0.01,1,length.out=nkpoints)
      for (re in 1:niterations) {
        expS<-getLogLikelihood(zs,ns,"Exp",kvals,nullvals,metaAnalysis$meta_psigAnal)
        expSmax<-max(expS,na.rm=TRUE)
        use<-which(expS==expSmax, arr.ind = TRUE)
        expNullmax<-nullvals[use[1,2]]
        expKmax<-kvals[use[1,1]]
        kvals<-seq(kvals[max(1,use[1,1]-reInc)],kvals[min(use[1,1]+reInc,nkpoints)],length.out=nkpoints)
        nullvals<-seq(nullvals[max(1,use[1,2]-reInc)],nullvals[min(use[1,2]+reInc,nnullpoints)],length.out=nnullpoints)
      }
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



