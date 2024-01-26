
# below we use (npops-1)/24 as an integer
npops=2*3*4*10+1 # 2*3*4*n+1
npoints=501
wDensMethod=2
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

zPopulationDist<-function(zvals,world){
  k<-world$populationPDFk
  switch (paste0(world$populationPDF,"_",world$populationRZ),
          "Single_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1
          },
          "Double_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1/2
            zdens[which.min(abs(k+rvals))]<-1/2
          },
          "Double_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1/2
            zdens[which.min(abs(k+zvals))]<-1/2
          },
          "Uniform_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(rvals*0+1,rvals)
          },
          "Uniform_z"={
            zdens<-zvals*0+uniformGain
          },
          "Exp_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(exp(-abs(rvals)/k),rvals)
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(exp(-0.5*(abs(rvals)/k)^2),rvals)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
  zdens
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
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1
            rdens
          },
          "Double_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1/2
            rdens[which.min(abs(k+rvals))]<-1/2
            rdens
          },
          "Double_z"={
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1/2
            rdens[which.min(abs(tanh(k)+rvals))]<-1/2
            rdens
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
  
populationDensityFunction<-function(rpw,possible){
  if (possible$type=="Populations") {
    switch (possible$UsePrior,
            "none" ={Prior<-list(populationPDF="Uniform",populationRZ=RZ,populationPDFk=0,populationNullp=0)},
            "world"={Prior<-possible$world},
            "prior"={Prior<-possible$prior}
            )
  } else {
    Prior<-possible$world
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
    mean=sum(rp*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE),
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=ci,
    dens_at_peak=dens_at_peak
  )


}

get_pRho<-function(world,by="r",viewRZ="r") {
  if (!world$worldOn) {
    world$populationPDF="Single"
    world$populationRZ="r"
  }
  if (by=="z") {
    if (world$populationPDF=="Single") {
      if (world$populationRZ=="r") {
        pRho<-world$populationPDFk
      } else {
        pRho<-tanh(world$populationPDFk)
      }
      pRhogain<-1
    } else {
      pRho<-seq(-1,1,length=npops)*z_range
      pRhogain<-zPopulationDist(pRho,world)
      switch (viewRZ,
              "r" ={
                pRho<-tanh(pRho)
                pRhogain<-zdens2rdens(pRhogain,pRho)
              },
              "z" ={
              }
      )
    }

  } else {
    if (world$populationPDF=="Single") {
      if (world$populationRZ=="r") {
        pRho<-world$populationPDFk
      } else {
        pRho<-tanh(world$populationPDFk)
      }
      pRhogain<-1
    } else {
      pRho<-seq(-1,1,length=npops)*r_range
      pRhogain<-zPopulationDist(atanh(pRho),world)
    }
    switch (viewRZ,
            "r" ={
            },
            "z" ={
              pRho<-atanh(pRho)
              pRhogain<-rdens2zdens(pRhogain,pRho)
            }
    )
  }
  if (world$populationNullp>0) {
    pRho<-c(0,pRho)
    pRhogain<-c(world$populationNullp,pRhogain/sum(pRhogain)*(1-world$populationNullp))
  }
  
  list(pRho=pRho,pRhogain=pRhogain)  
}

zSampleDist<-function(rs,pRho,pRhogain,source,design,possible) {
  # sampling distributions from specified populations (pRho)
  n<-design$sN
  ndist<-getNDist(design,source,sigOnly=FALSE)
  nis<-ndist$nvals
  ndens<-ndist$ndens
  if (length(nis)>1) ndens<-ndens*c(diff(nis),0)
  
  Dens_z<-matrix(nrow=length(pRho),ncol=length(rs))
  
  for (ei in 1:length(pRho)){
      dplus<-0
      g<-0
      for (ni in 1:(length(nis)-1)) {
        d1<-zSamplingDistr(rs,pRho[ei],nis[ni])
        if (possible$sigOnly) {
          crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
          d1[abs(rs)<crit_z]<-0
        }
        dplus<-dplus+d1*ndens[ni]
        g<-g+ndens[ni]
      }
      dplus<-dplus/g
    Dens_z[ei,]<-dplus*pRhogain[ei]
  }

  # and sum of sampling distributions
  if (source$populationPDF=="Single" && source$populationNull>0) {
    Dens_z_null<-Dens_z[1,]
    Dens_z_plus<-Dens_z[2,]
  } else {
    Dens_z_plus<-colSums(Dens_z)/sum(pRhogain)

    dnull<-0
    g<-0
    for (ni in 1:(length(nis)-1)) {
      d0<-zSamplingDistr(rs,0,nis[ni])
      if (possible$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
        d0[abs(rs)<crit_z]<-0
      }
      dnull<-dnull+d0*ndens[ni]
      g<-g+ndens[ni]
    }
    dnull<-dnull/g
    Dens_z_null<-dnull
  }
  list(Dens_z=Dens_z,Dens_z_plus=Dens_z_plus,Dens_z_null=Dens_z_null)
}

getNDist<-function(design,world=NULL,logScale=FALSE,sigOnly=FALSE,HQ=FALSE,asList=FALSE) {
  if (asList && !design$sNRand) {
    return(list(nvals=design$sN,ndens=1,ndensSig=1))
  }
    if (HQ) nmult<-5 else nmult=1
    nmax<-5
    if (logScale) {
      nvals<-10^seq(log10(minN),log10(nmax*design$sN),length.out=(nNpoints-1)*nmult+1)
    }else{
      nvals<-minN+seq(0,nmax*design$sN,length.out=(nNpoints-1)*nmult+1)
    }

    n<-design$sN
    if (design$sNRand) {
      ng<-dgamma(nvals-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
    } else {
      ng<-nvals*0
      use<-which.min(abs(nvals-design$sN))
      ng[use]<-1
    }
    if (sigOnly) {
      nsig<-ng
      if (is.null(world)) {
        pR<-list(pRho=0,pRhogain=1) 
      } else {
        pR<-get_pRho(world,by="r",viewRZ="r")
      }
      for (ni in 1:length(nvals)) {
        psig<-sum(rn2w(pR$pRho,nvals[ni])*pR$pRhogain)
        nsig[ni]<-nsig[ni]*psig
      }
    } else {
      nsig<-NA
    }
    if (logScale) {
      ng<-ng*nvals
      nsig<-nsig*nvals
    }
    list(nvals=nvals,ndens=ng,ndensSig=nsig)
}

fullRPopulationDist<-function(rvals,world) {
  rpopDistr(rvals,world$populationPDF,world$populationRZ,world$populationPDFk)
}

fullRSamplingDist<-function(vals,world,design,doStat="r",logScale=FALSE,sigOnly=FALSE,HQ=FALSE,separate=FALSE) {
  # sampling distribution from specified populations (pRho)
  if (is.null(vals)) {
    vals<-seq(-1,1,length=npoints)*r_range
  }
  if (is.null(world)) {
   pR<-list(pRho=0,pRhogain=1) 
  } else {
    if (doStat=="r")   pR<-get_pRho(world,by="r",viewRZ="r")
    else pR<-get_pRho(world,by="r",viewRZ="r")
  }

  # distribution of sample sizes
  ndist<-getNDist(design,HQ=HQ,asList=TRUE)
  nvals<-ndist$nvals
  ndens<-ndist$ndens
  
  sourceSampDens_r<-c()
  for (ei in 1:length(pR$pRho)){
      d<-0
      d1<-0
      for (ni in 1:length(nvals)) {
        switch (doStat,
                "r"={
                  rp<-vals
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])
                },
                "p"={
                  rp<-tanh(qnorm(1-vals/2)/sqrt(nvals[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])+
                            rSamplingDistr(-rp,pR$pRho[ei],nvals[ni])
                  dzp<-exp(-erfcinv(vals)^2)
                  a<-addition[1]
                  addition<-addition/dzp*(1-rp^2)
                  addition[1]<-a
                },
                "log(lrs)"={
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])+rSamplingDistr(-rp,pR$pRho[ei],nvals[ni])
                  dzs<-vals*(nvals[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                },
                "log(lrd)"={ #XXXXXXXX
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])+rSamplingDistr(-rp,pR$pRho[ei],nvals[ni])
                  dzs<-vals*(nvals[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                },
                "w"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])+rSamplingDistr(-rp,pR$pRho[ei],nvals[ni])
                  dwz<-dnorm(zp,qnorm(alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3)) -
                    dnorm(zp,-qnorm(alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                },
                "nw"={ 
                  zp<-(qnorm(0.8)-qnorm(alphaSig))/sqrt(vals-3)
                  rp<-tanh(zp)
                  addition<-rSamplingDistr(rp,pR$pRho[ei],nvals[ni])+rSamplingDistr(-rp,pR$pRho[ei],nvals[ni])
                  dznw<- -zp/(vals-3)/2
                  addition<-addition*dznw*(1-rp^2)
                },
                "wp"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-fullRPopulationDist(rp,world)
                  dwz<-dnorm(zp,qnorm(alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3)) -
                    dnorm(zp,-qnorm(alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                }
        )
        if (logScale) addition<-addition*vals
        d1<-d1+addition*ndens[ni]
        if (sigOnly) {
          critR<-tanh(qnorm(1-alphaSig/2,0,1/sqrt(nvals[ni]-3)))
          addition[abs(rp)<critR]<-0
        }
        d<-d+addition*ndens[ni]
      }
    d<-d/sum(d1*c(0,diff(rp)),na.rm=TRUE)
    sourceSampDens_r<-rbind(sourceSampDens_r,d*pR$pRhogain[ei])
  }
  # dr_gain<-max(sourceSampDens_r,na.rm=TRUE)
  # sourceSampDens_r<-sourceSampDens_r/dr_gain
  if (separate) {
    return(sourceSampDens_r)
  } else {
    return(colMeans(sourceSampDens_r))
  }
}

possibleRun <- function(IV,DV,effect,design,evidence,possible,metaResult,doSample=TRUE){
  n<-possible$design$sampleN
  design$sN<-n
  
  # note that we do everything in z and then, if required transform to r at the end
  switch (RZ,
          "r" ={
            zs<-atanh(seq(-1,1,length=npoints)*r_range)
            zp<-atanh(seq(-1,1,length=npoints)*r_range)
          },
          "z" ={
            zs<-seq(-1,1,length=npoints)*z_range
            zp<-seq(-1,1,length=npoints)*z_range
          }
  )

  # get the sample effect size of interest and its corresponding sample size
  sRho<-possible$targetSample
  if (RZ=="z") sRho<-tanh(sRho)
  n<-possible$design$sampleN
  if (!is.null(possible$ResultHistory)) {
    sRho<-possible$ResultHistory$r
    n<-possible$ResultHistory$n
  }
  if (!is.null(metaResult$result$rIV)) {
    sRho<-metaResult$result$rIV
    n<-metaResult$result$nval
  }
  sRho<-atanh(sRho)
  
  # get the source population distribution
  switch(possible$UseSource,
         "null"={source<-list(worldOn=FALSE,
                              populationPDF="Single",
                              populationPDFk=0,
                              populationRZ="r",
                              populationNullp=0
         )},
         "hypothesis"={source<-list(worldOn=FALSE,
                                    populationPDF="Single",
                                    populationPDFk=effect$rIV,
                                    populationRZ="r",
                                    populationNullp=0
         )},
         "world"={source<-possible$world},
         "prior"={source<-possible$prior}
  )
  sourcePopDens_z<-zPopulationDist(zs,source)
  # we add in the nulls for display, but only when displaying them makes sense
  if (source$populationPDF=="Single") {
    sourcePopDens_z<-sourcePopDens_z*(1-source$populationNullp)
    sourcePopDens_z[zp==0]<-sourcePopDens_z[zp==0]+source$populationNullp
  }
  
  # get the prior population distribution
  switch(possible$UsePrior,
         "none"={ prior<-list(worldOn=TRUE,
                              populationPDF="Uniform",
                              populationPDFk=zp,
                              populationRZ=RZ,
                              populationNullp=0.0) },
         "world"={ prior<-possible$world },
         "prior"={ prior<-possible$prior }
  )
  if (possible$type=="Populations") source<-prior
  
  priorPopDens_z<-zPopulationDist(zp,prior)
  priorPopDens_z_full<-priorPopDens_z*(1-prior$populationNullp)
  priorPopDens_z_full[zp==0]<-priorPopDens_z_full[zp==0]+prior$populationNullp
  priorPopDens_z_full<-priorPopDens_z_full/max(priorPopDens_z_full)
  if (prior$populationPDF=="Single") {
    priorPopDens_z_show<-priorPopDens_z_full
  } else {
    priorPopDens_z_show<-priorPopDens_z
  }
  
  # enumerate the source populations
  #  as r and gain 
  pR<-get_pRho(source,RZ,RZ)
  pRho<-pR$pRho
  pRhogain<-pR$pRhogain
  # if (RZ=="r")    pRhogain<-pRhogain/(1-tanh(pRho)^2)
  sD<-zSampleDist(zs,pRho,pRhogain,source,design,possible)
  sourceSampDens_z<-sD$Dens_z
  sourceSampDens_z_plus<-sD$Dens_z_plus
  sourceSampDens_z_null<-sD$Dens_z_null
  
  pR<-get_pRho(prior,RZ,RZ)
  pRhoP<-pR$pRho
  pRhogainP<-pR$pRhogain
  # if (RZ=="r")    pRhogainP<-pRhogainP/(1-tanh(pRhoP)^2)
  pD<-zSampleDist(zs,pRhoP,pRhogainP,prior,design,possible)
  priorSampDens_z<-pD$Dens_z
  priorSampDens_z_plus<-pD$Dens_z_plus
  priorSampDens_z_null<-pD$Dens_z_null

  if (length(pRho)>25) {
    l<-length(pRho)
    use<-seq(1,l,length.out=(l-1)/24)
    if (RZ=="z") {
      keep<-abs(pRho[use])<z_range
      use<-use[keep]
    }
    sourceSampDens_z<-sourceSampDens_z[use,]
    pRho<-pRho[use]
    pRhogain<-pRhogain[use]
  }
  if (possible$possibleCorrection) {
    nout<-ceil(possible$possibleSimSlice*sqrt(possible$design$sampleN-3))*20+1
    correction<-seq(-1,1,length.out=nout)*possible$possibleSimSlice
  }  else {
    correction<-0
  }
  
  # likelihood function for each sample (there's usually only 1)
  priorSampDens_z<-1
  priorLikelihood_z<-c()
  for (ei in 1:length(sRho)){
    zDens<-0
    for (ci in 1:length(correction)) {
      if (design$sNRand) {
        d<-0
        for (ni in seq(minN,maxRandN*design$sN,length.out=nNpoints)) {
          # for (ni in 5+seq(0,maxRandN,1/n[ei])*n[ei]) {
          g<-dgamma(ni-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
          d<-d+zSamplingDistr(zp,sRho[ei]+correction[ci],ni)*g
        }
        d<-d/sum(d)
        zDens<-zDens+d
      } else {
        zDens<-zDens+zSamplingDistr(zp,sRho[ei]+correction[ci],n[ei])
      }
    }
    priorLikelihood_z<-rbind(priorLikelihood_z,zDens/length(correction))
    priorSampDens_z <- priorSampDens_z * zDens/length(correction)
  }
  # times the a-priori distribution
    priorSampDens_z<-priorSampDens_z*priorPopDens_z_full
    for (ei in 1:length(sRho)){
      priorLikelihood_z[ei,]<-priorLikelihood_z[ei,]*priorPopDens_z_full
    }
  
  # simulations
  sr_effects<-NULL
  sSimBins<-NULL
  sSimDens<-NULL
  sSimBinsW<-NULL
  sSimDensW<-NULL
  rsSim_sd<-NULL
  rsSim_ci=NULL
  rsSim_peak=NULL
  
  pr_effectR<-NULL
  pr_effectRP<-NULL
  pr_effectN<-NULL
  pSimBins<-NULL
  pSimDens<-NULL
  pSimDensR<-NULL
  pSimDensRP<-NULL
  pSimBinsW<-NULL
  pSimDensW<-NULL
  rpSim_sd<-NULL
  rpSim_ci=NULL
  rpSim_peak=NULL
  rpSimWaste<-NULL
  wpSim_peak=NULL
  wpSim_mean<-NULL
  wpSimWaste<-NULL
  
  # make the samples
  nsims=possible$possibleLength
  
  s=1/sqrt(n-3)
  switch (possible$type,
          "Samples"={
            if (doSample) {
              r_effects<-c()
              for (i in 1:length(pRho)) {
                if (!shortHand){
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
              if (possible$appendSim){
                sr_effects<-cbind(possibleSResultHold$sSims,r_effects)
              } else {
                sr_effects<-r_effects
              }
            } else {
              sr_effects<-possibleSResultHold$sSims
              sSimBins<-possibleSResultHold$sSimBins
              sSimDens<-possibleSResultHold$sSimDens
            }

            if (!isempty(sr_effects)) {
              if (RZ=="z") {
                use_effects<-atanh(sr_effects)
                hist_range<-z_range
              } else {
                use_effects<-sr_effects
                hist_range<-r_range
              }
              binWidth<-2*IQR(use_effects)/length(use_effects)^(1/3)
              nbins=round(2/binWidth)
              sSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
              sSimDens<-c()
              sSimBinsW<-seq(w_range[1],w_range[2],length.out=nbins+1)
              sSimDensW<-c()
              for (i in 1:length(pRho)) {
                use_data<-abs(use_effects[i,])<=hist_range
                h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
                sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
                h<-hist(zn2w(atanh(sr_effects[i,]),42),sSimBinsW,plot=FALSE)$counts
                sSimDensW<-rbind(sSimDensW,h*pRhogain[i]/(1-tanh(pRho[i])^2))
              }
              possibleSResultHold<<-list(sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens)
              rsSim_ci=quantile(sr_effects,c(0.025,0.975))
              rsSim_peak=sSimBins[which.max(sSimDens)]+sSimBins[2]-sSimBins[1]
              rsSim_sd<-sd(sr_effects,na.rm=TRUE)
            }
          },
            
          "Populations"={
            if (doSample) {
              effect$world<-prior
                sample_increase=10
                res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims*sample_increase,appendData=FALSE, earlierResult=c(),sigOnly=FALSE,
                                      showProgress=TRUE,progressPrefix=paste0("Possible Populations :"))
                r_effects<-res$rIV
                n_effects<-res$nval
                pops<-res$rpIV
              if (possible$appendSim){
                pr_effectRP<-c(possiblePResultHold$RP,pops)
                pr_effectR<-c(possiblePResultHold$R,r_effects)
                pr_effectN<-c(possiblePResultHold$N,n_effects)
              } else {
                pr_effectRP<-pops
                pr_effectR<-r_effects
                pr_effectN<-n_effects
              }
                possiblePResultHold$RP<<-pr_effectRP
                possiblePResultHold$R<<-pr_effectR
                possiblePResultHold$N<<-pr_effectN
            } else {
              pr_effectRP<-possiblePResultHold$RP
              pr_effectR<-possiblePResultHold$R
              pr_effectN<-possiblePResultHold$N
            }
            
            if (!isempty(pr_effectRP)) {
              pr_effectW<-rn2w(pr_effectRP,pr_effectN)
              
              # do this in z - for symmetry
              keep<-abs(atanh(pr_effectR)-sRho[1])<possible$possibleSimSlice
              pr_effectRP_use<-pr_effectRP[keep]
              pr_effectW_use<-pr_effectW[keep]
              
              if (RZ=="z") {
                use_effectR_use<-atanh(pr_effectR_use)
                use_effectR<-atanh(pr_effectR)
                use_effectRP<-atanh(pr_effectRP)
                hist_range<-z_range
              } else {
                use_effectRP_use<-pr_effectRP_use
                use_effectR<-pr_effectR
                use_effectRP<-pr_effectRP
                hist_range<-1
              }

              if (prior$populationPDF=="Single" || prior$populationPDF=="Double") {
                binWidth<-0.05
              } else {
                binWidth<-max(0.05,2*IQR(use_effectRP_use,na.rm=TRUE)/length(use_effectRP_use)^(1/3))
              }
              keep<-abs(use_effectRP_use)<hist_range
              nbins=max(10,round(2/binWidth))
              pSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
              pSimDens<-hist(use_effectRP_use[keep],pSimBins,plot=FALSE)
              rpSim_ci=quantile(use_effectRP_use,c(0.025,0.975))
              rpSim_peak=pSimBins[which.max(pSimDens$counts)]+pSimBins[2]-pSimBins[1]
              rpSim_sd<-sd(use_effectRP_use,na.rm=TRUE)
              rpSimWaste<-sum(!keep)
              
              pSimDensRP<-hist(use_effectRP[abs(use_effectRP)<hist_range],pSimBins,plot=FALSE)
              pSimDensR<-hist(use_effectR[abs(use_effectR)<hist_range],pSimBins,plot=FALSE)
              
              pSimBinsW<-seq(w_range[1],w_range[2],length.out=nbins+1)
              keep<-pr_effectW_use>=w_range[1] & pr_effectW_use<=w_range[2]
              pSimDensW<-hist(pr_effectW_use[keep],pSimBinsW,plot=FALSE)
              wpSim_peak<-pSimBinsW[which.max(pSimDensW$counts)]+pSimBinsW[2]-pSimBinsW[1]
              wpSim_mean<-mean(pr_effectW_use,na.rm=TRUE)
              wpSimWaste<-sum(!keep)
            }
          }
  )
  
  # convert from z to r
  rs<-zs
  rp<-zp
  sourcePopDens_r<-sourcePopDens_z
  sourceSampDens_r<-sourceSampDens_z
  sourceSampDens_r_plus<-sourceSampDens_z_plus
  sourceSampDens_r_null<-sourceSampDens_z_null
  
  priorPopDens_r<-priorPopDens_z_show
  priorSampDens_r<-priorSampDens_z
  priorLikelihood_r<-priorLikelihood_z
  priorSampDens_r_plus<-priorSampDens_z_plus
  priorSampDens_r_null<-priorSampDens_z_null
  if (RZ=="r") {
    pRho<-tanh(pRho)
    sRho<-tanh(sRho)
    rp<-tanh(zp)
    rs<-tanh(zs)
    for (ei in 1:length(sRho)){
      priorLikelihood_r[ei,]<-zdens2rdens(priorLikelihood_z[ei,],rp)
    }
    for (ei in 1:length(pRho)){
      sourceSampDens_r[ei,]<-zdens2rdens(sourceSampDens_z[ei,],rs)
    }
    sourceSampDens_r_plus<-zdens2rdens(sourceSampDens_z_plus,rs)
    sourceSampDens_r_null<-zdens2rdens(sourceSampDens_z_null,rs)
    priorSampDens_r<-zdens2rdens(priorSampDens_z,rp)
    priorPopDens_r<-zdens2rdens(priorPopDens_z_show,rp)
    priorSampDens_r_plus<-zdens2rdens(priorSampDens_z_plus,rs)
    priorSampDens_r_null<-zdens2rdens(priorSampDens_z_null,rs)
    sourcePopDens_r<-zdens2rdens(sourcePopDens_z,rs)
  }
  if (any(!is.na(priorLikelihood_r))) {
    dr_gain<-max(priorLikelihood_r,na.rm=TRUE)
    priorLikelihood_r<-priorLikelihood_r/dr_gain
  }

  # power calculations
  if (design$sReplicationOn && design$sReplPowerOn) {
    if (RZ=="r") {
      nUse<-rw2n(sRho[1],design$sReplPower)
    } else {
      nUse<-rw2n(tanh(sRho[1]),design$sReplPower)
    }
  }
  else {
    nUse<-design$sN
  }
  if (wDensMethod==1) {
    wp<-seq(w_range[1],w_range[2],length.out=npoints+1)
    # wp<-c(wp,wp[npoints]+diff(wp[1:2]))
    z_use<-wn2z(wp,nUse)
  } else {
    wp<-seq(w_range[1],w_range[2],length.out=npoints)
    w<-zn2w(zp,nUse)
  }
  switch (possible$type,
          "Samples"={
            spDens_w<-sourceSampDens_z
          },
          "Populations"={
            spDens_w<-priorLikelihood_z
          }
  )
  if (any(!is.na(spDens_w))) {
    for (i1 in 1:nrow(spDens_w)) {
      if (wDensMethod==1) {
        # this way round avoids awkward infinities
        # by using numerical differentiation
        z_use_dens<-approx(zp,spDens_w,z_use)$y
        wd<-(z_use_dens[1:(length(z_use)-1)]+z_use_dens[2:(length(z_use))])/2 * diff(z_use)
        z_use_dens<-approx(zp,spDens_w,-z_use)$y
        wd<-wd+(z_use_dens[1:(length(z_use)-1)]+z_use_dens[2:(length(z_use))])/2 * diff(z_use)
      } else {
        zd<-spDens_w[i1,]
        useBreak<-which(w==min(w))
        
        use<-zp<=0
        use1<-use & !duplicated(w*use)
        wd1<-approx(w[use1],zd[use1],wp)$y
        wd1[wp==1]<-0
        
        use<-zp>=0
        use2<-use & !duplicated(w*use)
        wd2<-approx(w[use2],zd[use2],wp)$y
        wd2[wp==1]<-0

        z2<-approx(w[use2],zp[use2],wp)$y
        wd<-(wd1+wd2)/abs(dwdz(z2,nUse))
        wd[wp==1]<-0
      }
      spDens_w[i1,]<-wd
    }
    spDens_w<-spDens_w/max(spDens_w,na.rm=TRUE)
  }
  if (wDensMethod==1) {
    wp<-wp[1:npoints]+diff(wp)/2
  }
  
  dr_gain<-max(sourceSampDens_r,na.rm=TRUE)
  sourceSampDens_r<-sourceSampDens_r/dr_gain # *(1-possible$world$populationNullp)
  
  sourceSampDens_r_plus<-sourceSampDens_r_plus*(1-source$populationNullp)
  sourceSampDens_r_null<-sourceSampDens_r_null*(source$populationNullp)
  sourceSampDens_r_total<-sourceSampDens_r_plus+sourceSampDens_r_null
  dr_gain<-max(sourceSampDens_r_total)
  sourceSampDens_r_total<-sourceSampDens_r_total/dr_gain
  sourceSampDens_r_null<-sourceSampDens_r_null/dr_gain
  sourceSampDens_r_plus<-sourceSampDens_r_plus/dr_gain
  rs_stats<-densityFunctionStats(sourceSampDens_r_total,zs)
  
  if (any(!is.na(priorSampDens_r))) {
    dr_gain<-max(priorSampDens_r,na.rm=TRUE)
    priorSampDens_r<-priorSampDens_r/dr_gain
  }
  
  if (prior$worldOn && prior$populationNullp>0) {
    priorLikelihood_r<-priorLikelihood_r*(1-prior$populationNullp)
    priorPopDens_r<-priorPopDens_r*(1-prior$populationNullp)
    sourcePopDens_r<-sourcePopDens_r*(1-source$populationNullp)
    for (i in 1:length(sRho)) {
      priorLikelihood_r<-priorLikelihood_r*dnorm(atanh(sRho[i]),0,1/sqrt(n[i]-3))
    }
    priorSampDens_r_plus<-priorSampDens_r_plus/sum(priorSampDens_r_plus)*(1-prior$populationNullp)
    priorSampDens_r_null<-priorSampDens_r_null/sum(priorSampDens_r_null)*(prior$populationNullp)
  }
  rp_stats<-densityFunctionStats(priorSampDens_r,zp) 
  wp_stats<-densityFunctionStats(spDens_w,wp) 
  
  dens_at_peak=1
  if (is.na(sRho[1])) {
    dens_at_sample<-NA
    dens_at_population<-NA
    dens_at_zero<-NA
  } else {
    dens_at_sample<-approx(zp,priorSampDens_r,sRho[1])$y
    dens_at_population<-approx(zp,priorSampDens_r,ResultHistory$rp[1])$y
    dens_at_zero<-approx(zp,priorSampDens_r,0)$y
  }
  
    switch (possible$type,
          "Samples"={
            possibleResult<-list(possible=possible,
                                   pRho=pRho,
                                   sRho=sRho,
                                   n=n,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sourceSampDens_r=sourceSampDens_r,sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,sourceSampDens_r_total=sourceSampDens_r_total,
                                     rp=rp,priorSampDens_r=sourceSampDens_r,priorLikelihood_r=sourceSampDens_r,priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r,
                                     rs_peak=rs_stats$peak,
                                     rs_sd=rs_stats$sd,
                                     rs_ci=rs_stats$ci,
                                     wp=wp,
                                     spDens_w=spDens_w
                                   ),
                                   Sims=list(
                                     sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                                     sSimBinsW=sSimBinsW,sSimDensW=sSimDensW,
                                     rsSim_sd=rsSim_sd,
                                     rsSim_ci=rsSim_ci,
                                     rsSim_peak=rsSim_peak
                                   )
            )
          },
          "Populations"={
            possibleResult<-list(possible=possible,
                                   pRho=possible$targetPopulation,
                                   sRho=sRho,
                                   n=n,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sourceSampDens_r=sourceSampDens_r,sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,sourceSampDens_r_total=sourceSampDens_r_total,
                                     rp=rp,priorSampDens_r=priorSampDens_r,priorLikelihood_r=priorLikelihood_r,priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r,priorSampDens_r_null=priorSampDens_r_null,priorSampDens_r_plus=priorSampDens_r_plus,
                                     rp_peak=rp_stats$peak,
                                     rp_sd=rp_stats$sd,
                                     rp_ci=rp_stats$ci,
                                     wp=wp,
                                     wp_peak=wp_stats$peak,
                                     wp_mean=wp_stats$mean,
                                     spDens_w=spDens_w,
                                     dens_at_peak=dens_at_peak,dens_at_sample=dens_at_sample,
                                     dens_at_population=dens_at_population,dens_at_zero=dens_at_zero
                                   ),
                                   Sims=list(
                                     pSims=pr_effectR,pSimsP=pr_effectRP,pSimsN=pr_effectN,
                                     pSimBins=pSimBins,pSimDens=pSimDens,
                                     pSimDensR=pSimDensR,pSimDensRP=pSimDensRP,
                                     pSimBinsW=pSimBinsW,pSimDensW=pSimDensW,
                                     rpSim_sd=rpSim_sd,
                                     rpSim_ci=rpSim_ci,
                                     rpSim_peak=rpSim_peak,
                                     rpSimWaste=rpSimWaste,
                                     wpSim_peak=wpSim_peak,
                                     wpSim_mean=wpSim_mean,
                                     wpSimWaste=wpSimWaste
                                   )
            )
          }
  )
}
