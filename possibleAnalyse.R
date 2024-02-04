
densityFunctionStats<-function(dens_r,rp){
  use<-!is.na(dens_r)
  cum_dens_r<-cumsum(dens_r[use])/sum(dens_r[use])
  cum_rp<-rp[use]
  if (length(unique(cum_dens_r))<5) {
    ci<-c(-1,1)
  } else {
    if (any(cum_dens_r==0)) {
      use1<-max(which(cum_dens_r==0))
    } else {use1<-1}
    if (any(cum_dens_r==1)) {
      use2<-min(which(cum_dens_r==1))
    } else {use2<-length(cum_rp)}
    keep<-use1:use2
    use<-match(unique(cum_dens_r[keep]),cum_dens_r[keep])
    if (length(keep[use])>=2) {
      ci<-approx(cum_dens_r[keep[use]],cum_rp[keep[use]]+(rp[2]-rp[1])/2,c(0.025,0.975))$y
    } else {
      ci<-c(-1,1)
    }
  }
  
  peak<-rp[which.max(dens_r)]
  dens_at_peak<-max(dens_r)
  list(
    peak=peak,
    mean=sum(rp*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE),
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=ci,
    dens_at_peak=dens_at_peak
  )
  
}

describePossibleSamples<-function(possibleResult) {
  
  pRho<-possibleResult$pRho
  pRhogain<-possibleResult$pRhogain
  
  sr_effectR<-possibleResult$Sims$sSims
  sSimDens<-c()
  if (!isempty(sr_effectR)) {
    if (RZ=="z") {
      use_effects<-atanh(sr_effectR)
      hist_range<-z_range
    } else {
      use_effects<-sr_effectR
      hist_range<-r_range
    }
    binWidth<-2*IQR(use_effects)/length(use_effects)^(1/3)
    nbins=round(2/binWidth)
    if (possible$show!="Power") {
      sSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
    } else {
      use_effects<-zn2w(atanh(sr_effectR),42)
      hist_range<-w_range
      sSimBins<-seq(w_range[1],w_range[2],length.out=nbins+1)
    }
    for (i in 1:nrow(use_effects)) {
      use_data<-abs(use_effects[i,])<=hist_range
      h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
      sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
    }
    
    rsSim_ci=quantile(use_effects,c(0.025,0.975))
    rsSim_peak=sSimBins[which.max(sSimDens)]+sSimBins[2]-sSimBins[1]
    rsSim_sd<-sd(use_effects,na.rm=TRUE)
    
    result<-list(sr_effectR=sr_effectR,
                 sSimBins=sSimBins,sSimDens=sSimDens,
                 rsSim_peak=rsSim_peak,rsSim_sd=rsSim_sd,rsSim_ci=rsSim_ci)
  } else {
    result<-NULL
  }
  
}


describePossiblePopulations<-function(possibleResult) {
  
  sRho<-possibleResult$sRho
  
  pr_effectR<-possibleResult$Sims$pSims
  pr_effectRP<-possibleResult$Sims$pSimsP
  pr_effectN<-possibleResult$Sims$pSimsN
  
  if (!isempty(pr_effectRP)) {
    pr_effectW<-rn2w(pr_effectRP,pr_effectN)
    
    # do this in z - for symmetry
    keep<-abs(atanh(pr_effectR)-sRho[1])<possible$possibleSimSlice
    pr_effectRP_slice<-pr_effectRP[keep]
    pr_effectW_slice<-pr_effectW[keep]
    
    if (RZ=="z") {
      use_effectRP_slice<-atanh(pr_effectRP_slice)
      use_effectR<-atanh(pr_effectR)
      use_effectRP<-atanh(pr_effectRP)
      hist_range<-z_range
    } else {
      use_effectRP_slice<-pr_effectRP_slice
      use_effectR<-pr_effectR
      use_effectRP<-pr_effectRP
      hist_range<-1
    }
    
    if (possible$prior$populationPDF=="Single" || possible$prior$populationPDF=="Double") {
      binWidth<-0.05
    } else {
      binWidth<-max(0.05,2*IQR(use_effectRP_slice,na.rm=TRUE)/length(use_effectRP_slice)^(1/3))
    }
    nbins=max(10,round(2/binWidth))
    pSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
    pSimBinsW<-seq(w_range[1],w_range[2],length.out=nbins+1)
    
    keep<-abs(use_effectRP_slice)<hist_range
    pSimDens_slice<-hist(use_effectRP_slice[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectRP)<hist_range
    pSimDensRP<-hist(use_effectRP[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectR)<hist_range
    pSimDensR<-hist(use_effectR[keep],pSimBins,plot=FALSE)$counts
    
    keep<-pr_effectW_slice>=w_range[1] & pr_effectW_slice<=w_range[2]
    pSimDensW<-hist(pr_effectW_slice[keep],pSimBinsW,plot=FALSE)$counts
    
    rpSim_ci=quantile(use_effectRP_slice,c(0.025,0.975))
    rpSim_peak=pSimBins[which.max(pSimDens_slice)]+pSimBins[2]-pSimBins[1]
    rpSim_sd<-sd(use_effectRP_slice,na.rm=TRUE)
    rpSimWaste<-sum(!keep)
    wpSim_peak<-pSimBinsW[which.max(pSimDensW)]+pSimBinsW[2]-pSimBinsW[1]
    wpSim_mean<-mean(pr_effectW_slice,na.rm=TRUE)
    wpSimWaste<-sum(!keep)
    
    result<-list(pr_effectR=pr_effectR,pr_effectRP=pr_effectRP,pr_effectN=pr_effectN,pr_effectW=pr_effectW,
                 pSimBins=pSimBins,
                 pSimDens_slice=pSimDens_slice,pSimDensRP=pSimDensRP,pSimDensR=pSimDensR,pSimDensW=pSimDensW,
                 rpSim_ci=rpSim_ci,rpSim_peak=rpSim_peak,rpSim_sd=rpSim_sd,rpSimWaste=rpSimWaste,
                 wpSim_peak=wpSim_peak,wpSim_mean=wpSim_mean,wpSimWaste=wpSimWaste
    )
  } else {
    result<-NULL
  }
  
}
