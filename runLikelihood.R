
npops=2*3*4*50+1 # 2*3*4*n+1
npoints=501

uniformGain=1


zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

rdens2zdens<-function(rdens,rvals){
  rdens*(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

zpriorDistr<-function(zvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
          "Single_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1
            zdens*(1-rvals^2)
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1
            zdens
          },
          "Uniform_r"={
            rvals<-tanh(zvals)
            rvals*0+1*(1-rvals^2)
          },
          "Uniform_z"={
            zdens<-zvals*0+1
            zdens*uniformGain
          },
          "Exp_r"={
            rvals<-tanh(zvals)
            exp(-abs(rvals)/k)*(1-rvals^2)
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            rvals<-tanh(zvals)
            exp(-0.5*(abs(rvals)/k)^2)*(1-rvals^2)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
}

rSamplingDistr<-function(rvals,rmu,n){
  # map to Fisher-z
  zvals<-atanh(rvals)
  zmu<-atanh(rmu)
  zdens<-zSamplingDistr(zvals,zmu,n)
  zdens2rdens(zdens,rvals)
}


zpopDistr<-function(zvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(atanh(k)-zvals))]<-1
            zdens
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]=1
            zdens
          },
          "Uniform_r"={
            zdens<-atanh(zvals)*0+1*uniformGain
            rdens2zdens(zdens,zvals)
          },
          "Uniform_z"={
            zdens<-zvals*0+1
            zdens*uniformGain
          },
          "Exp_r"={
            exp(-abs(tanh(zvals)/k))
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            exp(-0.5*(abs(atanh(zvals))/k)^2)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
}
rpopDistr<-function(rvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
          "Single_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1
            rdens
          },
          "Single_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]=1
            zdens<-zdens2rdens(zdens,rvals)
            zdens
          },
          "Uniform_r"={
            rvals*0+1*uniformGain
          },
          "Uniform_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0+1
            zdens<-zdens2rdens(zdens,rvals)
            zdens*uniformGain
          },
          "Exp_r"={
            exp(-abs(rvals)/k)
          },
          "Exp_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-abs(zvals)/k)
            zdens2rdens(zdens,rvals)
          },
          "Gauss_r"={
            exp(-0.5*(abs(rvals)/k)^2)
          },
          "Gauss_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
            zdens2rdens(zdens,rvals)
          }
  )
}

rSamp2Pop<-function(r_s,n,world=NULL) {
  if (is.null(world)) {world<-list(populationPDF="Uniform",populationRZ="r",populationPDFk<-0)}
  k<-world$populationPDFk
  z_s<-atanh(r_s)
  if (is.na(z_s)) {return(NA)}
  switch(world$populationPDF,
         "Uniform"={mlEst<-z_s},
         "Single"={mlEst<-z_s},
         "Gauss"={mlEst<-z_s*k^2*(n-3)/(k^2*(n-3) + 1)},
         "Exp"={
           overEst<-1/k/(n-3)
           if (z_s<0) {
             mlEst<-min(z_s+overEst,0)
           } else {
             mlEst<-max(z_s-overEst,0)
           }
           }
         )
  tanh(mlEst)
}
  
populationDensityFunction<-function(rpw,likelihood){
  if (likelihood$type=="Populations") {
    switch (likelihood$UsePrior,
            "none" ={Prior<-list(populationPDF="Uniform",populationRZ=likelihood$viewRZ,populationPDFk=0,populationNullp=0)},
            "world"={Prior<-likelihood$world},
            "prior"={Prior<-likelihood$prior}
            )
  } else {
    Prior<-likelihood$world
  }
  rpw_dens<-rpopDistr(rpw,Prior$populationPDF,Prior$populationRZ,Prior$populationPDFk)
  rpw_dens<-rpw_dens*(1-Prior$populationNullp)
  if ((Prior$populationPDF=="Single") && Prior$populationNullp>0) {
    use<-which.min(abs(rpw-0))
    rpw_dens[use]<-Prior$populationNullp
  }
  rpw_dens    
}


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
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=ci,
    dens_at_peak=dens_at_peak
  )


}

get_pRho<-function(world,by="r",viewRZ="r") {
  
  if (by=="z") {
    if (world$populationPDF=="Single") {
      if (world$populationRZ=="r") {
        pRho<-atanh(world$populationPDFk)
      } else {
        pRho<-world$populationPDFk
      }
      pRhogain<-1
      if (world$populationNullp>0) {
        pRho<-c(0,pRho)
        pRhogain<-c(world$populationNullp,1-world$populationNullp)
      }
    } else {
      switch (viewRZ,
              "r" ={
                pRho<-atanh(seq(-1,1,length=npops)*r_range*0.9)
                pRhogain<-zpriorDistr(pRho,world$populationPDF,world$populationRZ,world$populationPDFk)
              },
              "z" ={
                pRho<-seq(-1,1,length=npops)*z_range*1.5
                pRhogain<-zpriorDistr(pRho,world$populationPDF,world$populationRZ,world$populationPDFk)
              }
      )
    }

  } else {
    if (!world$worldOn) {
      world$populationPDF="Single"
      world$populationRZ="r"
    }
    
    if (world$populationPDF=="Single") {
      if (world$populationRZ=="r") {
        pRho<-world$populationPDFk
      } else {
        pRho<-tanh(world$populationPDFk)
      }
      pRhogain<-1
      if (world$populationNullp) {
        pRho<-c(0,pRho)
        pRhogain<-c(world$populationNullp,1-world$populationNullp)
      }
    } else {
      pRho<-seq(-1,1,length.out=npops)*r_range
      pRhogain<-zdens2rdens(zpriorDistr(atanh(pRho),world$populationPDF,world$populationRZ,world$populationPDFk),pRho)
    }
  }
  list(pRho=pRho,pRhogain=pRhogain)  
}

getZDist<-function(rs,pRho,pRhogain,source,design,likelihood) {
  # sampling distributions from specified populations (pRho)
  n<-design$sN
  sDens_z<-matrix(nrow=length(pRho),ncol=length(rs))
  for (ei in 1:length(pRho)){
    if (design$sNRand) {
      d<-0
      for (ni in seq(minN,maxRandN*design$sN,length.out=nNpoints)) {
        g<-dgamma(ni-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
        d1<-zSamplingDistr(rs,pRho[ei],ni)*g
        if (likelihood$sigOnly) {
          crit_z<-qnorm(0.975,0,1/sqrt(ni-3))
          d1[abs(rs)<crit_z]<-0
        }
        d<-d+d1
      }
    } else {
      d<-zSamplingDistr(rs,pRho[ei],n)
      if (likelihood$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(n-3))
        d[abs(rs)<crit_z]<-0
      }
    }
    # d<-d/sum(d)
    sDens_z[ei,]<-d*pRhogain[ei]
  }
  # and sum of sampling distributions
  if (source$populationPDF=="Single" && source$populationNull>0) {
    sDens_z_plus<-sDens_z[2,]
  } else {
    sDens_z_plus<-colMeans(sDens_z)
  }
  sDens_z_plus<-sDens_z_plus/sum(sDens_z_plus)
  if (design$sNRand) {
    d<-0
    for (ni in seq(minN,maxRandN*design$sN,length.out=nNpoints)) {
      g<-dgamma(ni-minN,shape=design$sNRandK,scale=(n-minN)/design$sNRandK)
      d1<-zSamplingDistr(rs,0,ni)*g
      if (likelihood$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(ni-3))
        d1[abs(rs)<crit_z]<-0
      }
      d<-d+d1
    }
  } else {
    d<-zSamplingDistr(rs,0,n)
    if (likelihood$sigOnly) {
      crit_z<-qnorm(0.975,0,1/sqrt(n-3))
      d[abs(rs)<crit_z]<-0
    }
  }
  sDens_z_null<-d #/sum(d)
  
  list(sDens_z=sDens_z,sDens_z_plus=sDens_z_plus,sDens_z_null=sDens_z_null)
}

getNDist<-function(nvals,design,logScale) {
  n<-design$sN
  if (design$sNRand) {
    ng<-dgamma(nvals-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  } else {
    ng<-nvals*0
    use<-which.min(abs(nvals-design$sN))
    ng[use]<-1
  }
  if (logScale) ng<-ng*nvals
  ng
}

fullRPopulationDist<-function(rvals,world) {
  rpopDistr(rvals,world$populationPDF,world$populationRZ,world$populationPDFk)
}

fullRSamplingDist<-function(vals,world,design,doStat="r",logScale=FALSE,sigOnly=FALSE) {
  # sampling distribution from specified populations (pRho)
  if (is.null(vals)) {
    vals<-seq(-1,1,length=npoints)*r_range
  }
  if (is.null(world)) {
   pR<-list(pRho=0,pRhogain=1) 
  } else {
  pR<-get_pRho(world)
  }
  # distribution of sample sizes
  n<-design$sN
  ng<-1
  if (design$sNRand) {
    n<-5+seq(0,5*n,length.out=nNpoints)
    ng<-dgamma(n-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  }
  
  sDens_r<-c()
  for (ei in 1:length(pR$pRho)){
      d<-0
      for (ni in 1:length(n)) {
        switch (doStat,
                "r"={
                  addition<-rSamplingDistr(vals,pR$pRho[ei],n[ni])
                  if (sigOnly) {
                    critR<-tanh(qnorm(1-alpha/2,0,1/sqrt(n[ni]-3)))
                    addition[abs(vals)<critR]<-0
                  }
                  if (logScale) addition<-addition*vals
                },
                "p"={
                  rp<-tanh(qnorm(1-vals/2)/sqrt(n[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dzp<-exp(-erfcinv(vals)^2)
                  a<-addition[1]
                  addition<-addition/dzp*(1-rp^2)
                  addition[1]<-a
                  if (logScale) addition<-addition*vals
                },
                "log(lrs)"={
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dzs<-vals*(n[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                  if (logScale) addition<-addition*vals
                },
                "log(lrd)"={ #XXXXXXXX
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dzs<-vals*(n[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                  if (logScale) addition<-addition*vals
                },
                "w"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alpha/2)+zp*sqrt(n[ni]-3)) + pnorm(qnorm(alpha/2)-zp*sqrt(n[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dwz<-dnorm(zp,qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3)) -
                    dnorm(zp,-qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                  if (logScale) addition<-addition*vals
                },
                "nw"={ 
                  zp<-(qnorm(0.8)-qnorm(alpha))/sqrt(vals-3)
                  rp<-tanh(zp)
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dznw<- -zp/(vals-3)/2
                  addition<-addition*dznw*(1-rp^2)
                  if (logScale) addition<-addition*vals
                },
                "wp"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alpha/2)+zp*sqrt(n[ni]-3)) + pnorm(qnorm(alpha/2)-zp*sqrt(n[ni]-3))
                  addition<-fullRPopulationDist(rp,world)
                  dwz<-dnorm(zp,qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3)) -
                    dnorm(zp,-qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                  if (logScale) addition<-addition*vals
                }
        )
        d<-d+addition*ng[ni]
      }
    d<-d/sum(d,na.rm=TRUE)
    sDens_r<-rbind(sDens_r,d*pR$pRhogain[ei])
  }
  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain
  sDens_r_plus<-colMeans(sDens_r)
  sDens_r_plus
}

likelihood_run <- function(IV,DV,effect,design,evidence,likelihood,doSample=TRUE){
  n<-likelihood$design$sampleN
  design$sN<-n
  
  # note that we do everything in z and then, if required transform to r at the end
  switch (likelihood$viewRZ,
          "r" ={
            rs<-atanh(seq(-1,1,length=npoints)*r_range)
            rp<-atanh(seq(-1,1,length=npoints)*r_range)
          },
          "z" ={
            rs<-seq(-1,1,length=npoints)*z_range
            rp<-seq(-1,1,length=npoints)*z_range
          }
  )
  
  if (is.null(likelihood$ResultHistory)) {
    sRho<-likelihood$targetSample
    n<-likelihood$design$sampleN
  } else {
    sRho<-likelihood$ResultHistory$r
    n<-likelihood$ResultHistory$n
  }
  sRho<-atanh(sRho)
  
  # get the source population distribution
  switch(likelihood$UseSource,
         "null"={source<-list(worldOn=FALSE,
                              populationPDF="Single",
                              populationPDFk=0,
                              populationRZ="r",
                              populationNullp=0
         )},
         "hypothesis"={source<-likelihood$world},
         "world"={source<-likelihood$world},
         "prior"={source<-likelihood$prior}
  )
  asDens_z<-zpriorDistr(rs,source$populationPDF,source$populationRZ,source$populationPDFk)
  if (source$populationNullp>0 && source$populationPDF=="Single") {
    asDens_z<-asDens_z*(1-source$populationNullp)
    asDens_z[rp==0]<-asDens_z[rp==0]+source$populationNullp
  }
  # get the prior population distribution
  switch(likelihood$UsePrior,
         "none"={ prior<-list(worldOn=TRUE,
                              populationPDF="Uniform",
                              populationPDFk=rp,
                              populationRZ=likelihood$viewRZ,
                              populationNullp=0.0) },
         "world"={ prior<-likelihood$world },
         "prior"={ prior<-likelihood$prior }
  )
  doNullsSingle<-(prior$populationNullp>0 && prior$populationPDF=="Single")
  doNulls<-TRUE
  
  apDens_z<-zpriorDistr(rp,prior$populationPDF,prior$populationRZ,prior$populationPDFk)
  if (doNulls) {
    apDens_z_null<-apDens_z*(1-prior$populationNullp)
    apDens_z_null[rp==0]<-apDens_z_null[rp==0]+prior$populationNullp
    apDens_z_null<-apDens_z_null/max(apDens_z_null)
  } else {
    apDens_z_null<-apDens_z
  }
  if (doNullsSingle) {
    apDens_z<-apDens_z_null
  }
  
  # enumerate the source populations
  #  as r and gain 
  pR<-get_pRho(source,"z",likelihood$viewRZ)
  pRho<-pR$pRho
  pRhogain<-pR$pRhogain
  sD<-getZDist(rs,pRho,pRhogain,source,design,likelihood)
  sDens_z<-sD$sDens_z
  sDens_z_plus<-sD$sDens_z_plus
  sDens_z_null<-sD$sDens_z_null
  
  pR<-get_pRho(prior,"z",likelihood$viewRZ)
  pRhoP<-pR$pRho
  pRhogainP<-pR$pRhogain
  sD<-getZDist(rs,pRhoP,pRhogainP,prior,design,likelihood)
  pDens_z<-sD$sDens_z
  pDens_z_plus<-sD$sDens_z_plus
  pDens_z_null<-sD$sDens_z_null

  if (length(pRho)>25) {
    l<-length(pRho)
    use<-seq(1,l,length.out=(l-1)/24)
    if (likelihood$viewRZ=="z") {
      keep<-abs(pRho[use])<z_range
      use<-use[keep]
    }
    sDens_z<-sDens_z[use,]
    pRho<-pRho[use]
    pRhogain<-pRhogain[use]
  }
  if (likelihood$likelihoodCorrection) {
    nout<-ceil(likelihood$likelihoodSimSlice*sqrt(likelihood$design$sampleN-3))*20+1
    correction<-seq(-1,1,length.out=nout)*likelihood$likelihoodSimSlice
  }  else {
    correction<-0
  }
  
  # likelihood function for each sample (there's usually only 1)
  pDens_z<-1
  spDens_z<-c()
  for (ei in 1:length(sRho)){
    zDens<-0
    for (ci in 1:length(correction)) {
      if (design$sNRand) {
        d<-0
        for (ni in seq(minN,maxRandN*design$sN,length.out=nNpoints)) {
          # for (ni in 5+seq(0,maxRandN,1/n[ei])*n[ei]) {
          g<-dgamma(ni-minN,shape=design$sNRandK,scale=(n[ei]-minN)/design$sNRandK)
          d<-d+zSamplingDistr(rp,sRho[ei]+correction[ci],ni)*g
        }
        d<-d/sum(d)
        zDens<-zDens+d
      } else {
        zDens<-zDens+zSamplingDistr(rp,sRho[ei]+correction[ci],n[ei])
      }
    }
    spDens_z<-rbind(spDens_z,zDens/length(correction))
    pDens_z <- pDens_z * zDens/length(correction)
  }
  # times the a-priori distribution
    pDens_z<-pDens_z*apDens_z_null
    for (ei in 1:length(sRho)){
      spDens_z[ei,]<-spDens_z[ei,]*apDens_z_null
    }
  
  # simulations
  sr_effects<-NULL
  sSimBins<-NULL
  sSimDens<-NULL
  rsSim_sd<-NULL
  rsSim_ci=NULL
  rsSim_peak=NULL
  
  pr_effectS<-NULL
  pr_effectP<-NULL
  pSimBins<-NULL
  pSimDens<-NULL
  pSimDensS<-NULL
  pSimDensP<-NULL
  rpSim_sd<-NULL
  rpSim_ci=NULL
  rpSim_peak=NULL
  
  # make the samples
  nsims=likelihood$Likelihood_length
  
  s=1/sqrt(n-3)
  switch (likelihood$type,
          "Samples"={
            if ((doSample || (length(likelihood_S_ResultHold$sSims)>1)) && length(pRho)>10) {
              # use<-seq(1,length(pRho),3)
              # pRho<-pRho[use]
              # pRhogain<-pRhogain[use]
              # sDens_z<-sDens_z[use,]
            }
            if (doSample) {
              r_effects<-c()
              for (i in 1:length(pRho)) {
                if (likelihood$likelihoodLongHand){
                  effect1<-effect
                  effect1$rIV<-tanh(pRho[i])
                  effect1$world$worldOn<-FALSE
                  effect1$world$populationPDF<-"Single"
                  res<-multipleAnalysis(IV,NULL,DV,effect1,design,evidence,nsims,appendData=FALSE, earlierResult=c(),sigOnly=FALSE,
                                        showProgress=TRUE,progressPrefix=paste0("Possible Samples ",format(i),"/",format(length(pRho)),":"))
                  r_effects<-rbind(r_effects,t(res$rIV))
                } else {
                  if (design$sNRand) {
                      ns<-minN+rgamma(nsims,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
                      ns<-floor(ns)
                      s1<-1/sqrt(ns-3)
                    r_effects<-rbind(r_effects,tanh(rnorm(nsims,mean=pRho[i],sd=s1)))
                  } else {
                    r_effects<-rbind(r_effects,tanh(rnorm(nsims,mean=pRho[i],sd=s)))
                  }
                }
              }
              if (likelihood$appendSim){
                sr_effects<-cbind(likelihood_S_ResultHold$sSims,r_effects)
              } else {
                sr_effects<-r_effects
              }
            } else {
              sr_effects<-likelihood_S_ResultHold$sSims
              sSimBins<-likelihood_S_ResultHold$sSimBins
              sSimDens<-likelihood_S_ResultHold$sSimDens
            }

            if (!isempty(sr_effects)) {
              if (likelihood$viewRZ=="z") {
                use_effects<-atanh(sr_effects)
                hist_range<-z_range
              } else {
                use_effects<-sr_effects
                hist_range<-1
              }
              binWidth<-2*IQR(use_effects)/length(use_effects)^(1/3)
              nbins=round(2/binWidth)
              sSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
              sSimDens<-c()
              for (i in 1:length(pRho)) {
                use_data<-abs(use_effects[i,])<=hist_range
                h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
                sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
              }
              likelihood_S_ResultHold<<-list(sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens)
              rsSim_ci=quantile(sr_effects,c(0.025,0.975))
              rsSim_peak=sSimBins[which.max(sSimDens)]+sSimBins[2]-sSimBins[1]
              rsSim_sd<-sd(sr_effects,na.rm=TRUE)
            }
          },
            
          "Populations"={
            if (doSample) {
              effect$world<-prior
              if (likelihood$likelihoodLongHand){
                sample_increase=10
                res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims*sample_increase,appendData=FALSE, earlierResult=c(),sigOnly=FALSE,
                                      showProgress=TRUE,progressPrefix=paste0("Possible Populations :"))
                r_effects<-res$rIV
                pops<-res$rpIV
              } else {
                sample_increase=1000
                # make some population values according to the specified a priori distribution
                switch (paste0(prior$populationPDF,"_",prior$populationRZ),
                        "Single_r"={
                          pops<-rep(prior$populationPDFk,nsims*sample_increase)
                        },
                        "Single_z"={
                          pops<-rep(prior$populationPDFk,nsims*sample_increase)
                          pops<-tanh(pops)
                        },
                        "Exp_z"={
                          pops<-rexp(nsims*sample_increase,rate=1/prior$populationPDFk)
                          pops<-tanh(pops)
                          pops<-pops*sign(rnorm(length(pops)))
                        },
                        "Exp_r"={
                          pops<-rexp(nsims*1.5*sample_increase,rate=1/prior$populationPDFk)
                          pops<-pops[pops<1]
                          if (length(pops)>nsims*sample_increase) {
                            pops<-pops[1:(nsims*sample_increase)]
                          }
                          pops<-pops*sign(rnorm(length(pops)))
                        },
                        "Gauss_z"={
                          pops<-rnorm(nsims*sample_increase,sd=prior$populationPDFk)
                          pops<-tanh(pops)
                        },
                        "Gauss_r"={
                          pops<-rnorm(nsims*sample_increase,sd=prior$populationPDFk)
                          pops<-pops[abs(pops)<1]
                        },
                        "Uniform_r"={
                          pops<-runif(nsims*sample_increase,min=-1,max=1)
                        },
                        "Uniform_z"={
                          pops<-runif(nsims*sample_increase,min=-10,max=10)
                          pops<-tanh(pops)
                        }
                )
                if (likelihood$prior$populationNullp>0) {
                  change<-round(likelihood$prior$populationNullp*length(pops))
                  pops[1:change]<-0
                }
                # make some sample sizes
                if (design$sNRand) {
                  ns<-minN+rgamma(nsims*sample_increase,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
                  ns<-round(ns)
                  s1<-1/sqrt(ns-3)
                  r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s1))
                } else {
                  r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s))
                }
              }
              if (likelihood$appendSim){
                pr_effectP<-c(likelihood_P_ResultHold$pSims,pops)
                pr_effectS<-c(likelihood_P_ResultHold$sSims,r_effects)
              } else {
                pr_effectP<-pops
                pr_effectS<-r_effects
              }
              likelihood_P_ResultHold$pSims<<-pr_effectP
              likelihood_P_ResultHold$sSims<<-pr_effectS
            } else {
              pr_effectP<-likelihood_P_ResultHold$pSims
              pr_effectS<-likelihood_P_ResultHold$sSims
            }
            
            if (!isempty(pr_effectS)) {
              # do this in z - for symmetry
              keep<-abs(atanh(pr_effectS)-sRho[1])<likelihood$likelihoodSimSlice
              pr_effectP_use<-pr_effectP[keep]

              if (likelihood$viewRZ=="z") {
                use_effects<-atanh(pr_effectP_use)
                use_effectP<-atanh(pr_effectP)
                use_effectS<-atanh(pr_effectS)
                hist_range<-z_range
              } else {
                use_effects<-pr_effectP_use
                use_effectP<-pr_effectP
                use_effectS<-pr_effectS
                hist_range<-1
              }

              if (prior$populationPDF=="Single") {
                binWidth<-0.05
              } else {
                binWidth<-max(0.05,2*IQR(use_effects)/length(use_effects)^(1/3))
              }
              hist_use<-abs(use_effects)<hist_range
              nbins=max(10,round(2/binWidth))
              pSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
              pSimDens<-hist(use_effects[hist_use],pSimBins,plot=FALSE)
              rpSim_ci=quantile(use_effects,c(0.025,0.975))
              rpSim_peak=pSimBins[which.max(pSimDens$counts)]+pSimBins[2]-pSimBins[1]
              rpSim_sd<-sd(use_effects,na.rm=TRUE)
              
              pSimDensP<-hist(use_effectP[abs(use_effectP)<hist_range],pSimBins,plot=FALSE)
              pSimDensS<-hist(use_effectS[abs(use_effectS)<hist_range],pSimBins,plot=FALSE)
            }
          }
  )

  # convert from z to r
  sDens_r<-sDens_z
  sDens_r_plus<-sDens_z_plus
  sDens_r_null<-sDens_z_null
  pDens_r<-pDens_z
  spDens_r<-spDens_z
  apDens_r<-apDens_z
  pDens_r_plus<-pDens_z_plus
  pDens_r_null<-pDens_z_null
  asDens_r<-asDens_z
  if (likelihood$viewRZ=="r") {
    pRho<-tanh(pRho)
    sRho<-tanh(sRho)
    rp<-tanh(rp)
    rs<-tanh(rs)
    for (ei in 1:length(sRho)){
      spDens_r[ei,]<-zdens2rdens(spDens_z[ei,],rp)
    }
    for (ei in 1:length(pRho)){
      sDens_r[ei,]<-zdens2rdens(sDens_z[ei,],rs)
    }
    sDens_r_plus<-zdens2rdens(sDens_z_plus,rs)
    sDens_r_null<-zdens2rdens(sDens_z_null,rs)
    pDens_r<-zdens2rdens(pDens_z,rp)
    apDens_r<-zdens2rdens(apDens_z,rp)
    pDens_r_plus<-zdens2rdens(pDens_z_plus,rs)
    pDens_r_null<-zdens2rdens(pDens_z_null,rs)
    asDens_r<-zdens2rdens(asDens_z,rs)
  }
  if (any(!is.na(spDens_r))) {
    dr_gain<-max(spDens_r,na.rm=TRUE)
    spDens_r<-spDens_r/dr_gain
  }
  pDens_r<-pDens_r/max(pDens_r)
  
  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain # *(1-likelihood$world$populationNullp)
  sDens_r_plus<-sDens_r_plus/sum(sDens_r_plus)*(1-source$populationNullp)
  sDens_r_null<-sDens_r_null/sum(sDens_r_null)*(source$populationNullp)
  sDens_r_total<-sDens_r_plus+sDens_r_null
  dr_gain<-max(sDens_r_total)
  sDens_r_total<-sDens_r_total/dr_gain
  sDens_r_null<-sDens_r_null/dr_gain
  sDens_r_plus<-sDens_r_plus/dr_gain
  rs_stats<-densityFunctionStats(sDens_r_total,rs)
  
  if (any(!is.na(pDens_r))) {
    dr_gain<-max(pDens_r,na.rm=TRUE)
    pDens_r<-pDens_r/dr_gain
  }
  
  # pDens_r<-pDens_r*(1-prior$populationNullp)
  if (!doNullsSingle) {
    spDens_r<-spDens_r*(1-prior$populationNullp)*dnorm(atanh(sRho),0,1/sqrt(n-3))
    apDens_r<-apDens_r*(1-prior$populationNullp)
    asDens_r<-asDens_r*(1-source$populationNullp)
    pDens_r_plus<-pDens_r_plus/sum(pDens_r_plus)*(1-prior$populationNullp)
    pDens_r_null<-pDens_r_null/sum(pDens_r_null)*(prior$populationNullp)
  }
  rp_stats<-densityFunctionStats(pDens_r,rp) 
  
  dens_at_peak=1
  if (is.na(sRho[1])) {
    dens_at_sample<-NA
    dens_at_population<-NA
    dens_at_zero<-NA
  } else {
    dens_at_sample<-approx(rp,pDens_r,sRho[1])$y
    dens_at_population<-approx(rp,pDens_r,ResultHistory$rp[1])$y
    dens_at_zero<-approx(rp,pDens_r,0)$y
  }

    switch (likelihood$type,
          "Samples"={
            likelihoodResult<-list(likelihood=likelihood,
                                   pRho=pRho,
                                   sRho=likelihood$targetSample,
                                   n=n,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_plus=sDens_r_plus,sDens_r_null=sDens_r_null,sDens_r_total=sDens_r_total,
                                     rp=rp,pDens_r=sDens_r,spDens_r=sDens_r,apDens_r=apDens_r,asDens_r=asDens_r,
                                     rs_peak=rs_stats$peak,
                                     rs_sd=rs_stats$sd,
                                     rs_ci=rs_stats$ci
                                   ),
                                   Sims=list(
                                     sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                                     rsSim_sd=rsSim_sd,
                                     rsSim_ci=rsSim_ci,
                                     rsSim_peak=rsSim_peak
                                   )
            )
          },
          "Populations"={
            likelihoodResult<-list(likelihood=likelihood,
                                   pRho=likelihood$targetPopulation,
                                   sRho=sRho,
                                   n=n,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_plus=sDens_r_plus,sDens_r_null=sDens_r_null,sDens_r_total=sDens_r_total,
                                     rp=rp,pDens_r=pDens_r,spDens_r=spDens_r,apDens_r=apDens_r,asDens_r=asDens_r,pDens_r_null=pDens_r_null,pDens_r_plus=pDens_r_plus,
                                     rp_peak=rp_stats$peak,
                                     rp_sd=rp_stats$sd,
                                     rp_ci=rp_stats$ci,
                                     dens_at_peak=dens_at_peak,dens_at_sample=dens_at_sample,
                                     dens_at_population=dens_at_population,dens_at_zero=dens_at_zero
                                   ),
                                   Sims=list(
                                     pSims=pr_effectS,pSimsP=pr_effectP,pSimBins=pSimBins,pSimDens=pSimDens,
                                     pSimDensS=pSimDensS,pSimDensP=pSimDensP,
                                     rpSim_sd=rpSim_sd,
                                     rpSim_ci=rpSim_ci,
                                     rpSim_peak=rpSim_peak
                                   )
            )
          }
  )
}
