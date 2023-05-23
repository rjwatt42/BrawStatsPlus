
getMaxLikelihood<-function(zs,ns,dist,metaAnalysis) {
  nkpoints<-13
  nnullpoints<-13
  
  niterations<-1
  reInc<-(nkpoints-1)/2/3

  if (metaAnalysis$meta_nullAnal) {
    startNullvals<-seq(0,1,length.out=nnullpoints)
  } else {
    startNullvals<-0
  }
  
  nullvals<-startNullvals
  if (dist=="Single") {
    kvals<-seq(-1,1,length.out=nkpoints)*0.95
  } else {
    kvals<-seq(0.01,1,length.out=nkpoints)
  }
  
  for (re in 1:niterations) {
    S<-getLogLikelihood(zs,ns,dist,kvals,nullvals,metaAnalysis$meta_psigAnal)
    Smax<-max(S,na.rm=TRUE)
    
    use<-which(S==Smax, arr.ind = TRUE)
    Nullmax<-nullvals[use[1,2]]
    lb2<-nullvals[max(1,use[1,2]-reInc)]
    ub2<-nullvals[min(length(nullvals),use[1,2]+reInc)]
    Kmax<-kvals[use[1,1]]
    lb1<-kvals[max(1,use[1,1]-reInc)]
    ub1<-kvals[min(length(kvals),use[1,1]+reInc)]
    
    kvals<-seq(lb1,ub1,length.out=nkpoints)
    nullvals<-seq(lb2,ub2,length.out=nnullpoints)
  }
  
  if (niterations==1) {      
    fun<-function(x) { -getLogLikelihood(zs,ns,dist,x[1],x[2],metaAnalysis$meta_psigAnal)}
    result<-fmincon(c(Kmax,Nullmax),fun,ub=c(ub1,ub2),lb=c(lb1,lb2))
    Kmax<-result$par[1]
    Nullmax<-result$par[2]
    Smax<- -result$value
  }
  return(list(Kmax=Kmax,Nullmax=Nullmax,Smax=Smax))
}


runMetaAnalysis<-function(metaAnalysis,metaResult){
  rs<-metaResult$result$rIV
  zs<-atanh(rs)
  ns<-metaResult$result$nval
  
  if (metaAnalysis$meta_fixedAnal=="fixed") {
    single<-getMaxLikelihood(zs,ns,"Single",metaAnalysis)
    gauss<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    exp<-list(Kmax=NA,Nullmax=NA,Smax=NA)

  } else {
    
    # doing random effects analysis

    # find best Single 
    if (metaAnalysis$meta_pdf=="Single" || (metaAnalysis$meta_pdf=="All" && includeSingle)) {
      single<-getMaxLikelihood(zs,ns,"Single",metaAnalysis)
    } else {
      single<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }

    # find best Gauss
    if (metaAnalysis$meta_pdf=="Gauss" || metaAnalysis$meta_pdf=="All") {
      gauss<-getMaxLikelihood(zs,ns,"Gauss",metaAnalysis)
    } else {
      gauss<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }

    # find best Exp
    if (metaAnalysis$meta_pdf=="Exp" || metaAnalysis$meta_pdf=="All") {
      exp<-getMaxLikelihood(zs,ns,"Exp",metaAnalysis)
    } else {
      exp<-list(Kmax=NA,Nullmax=NA,Smax=NA)
    }
    
  }
  
  
  use<-which.max(c(single$Smax,gauss$Smax,exp$Smax))
  bestDist<-c("Single","Gauss","Exp")[use]
  bestK<-c(single$Kmax,gauss$Kmax,exp$Kmax)[use]
  bestNull<-c(single$Nullmax,gauss$Nullmax,exp$Nullmax)[use]
  bestS<-c(single$Smax,gauss$Smax,exp$Smax)[use]

  if (metaAnalysis$append) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestK<-c(metaResult$bestK,bestK)
    bestNull<-c(metaResult$bestNull,bestNull)
    bestS<-c(metaResult$bestS,bestS)
    single$Kmax<-c(metaResult$single$Kmax,single$Kmax)
    single$Smax<-c(metaResult$single$Smax,single$Smax)
    single$Nullmax<-c(metaResult$single$Nullmax,single$Nullmax)
    gauss$Kmax<-c(metaResult$gauss$Kmax,gauss$Kmax)
    gauss$Smax<-c(metaResult$gauss$Smax,gauss$Smax)
    gauss$Nullmax<-c(metaResult$gauss$Nullmax,gauss$Nullmax)
    exp$Kmax<-c(metaResult$exp$Kmax,exp$Kmax)
    exp$Smax<-c(metaResult$exp$Smax,exp$Smax)
    exp$Nullmax<-c(metaResult$exp$Nullmax,exp$Nullmax)
  }
  
  metaResult<-list(single=single,
                   gauss=gauss,
                   exp=exp,
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



