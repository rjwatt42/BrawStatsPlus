
npops=2*3*4*50+1 # 2*3*4*n+1
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

getZDist<-function(rs,pRho,pRhogain,source,design,possible) {
  # sampling distributions from specified populations (pRho)
  n<-design$sN
  sDens_z<-matrix(nrow=length(pRho),ncol=length(rs))
  if (possible$possibleHQ) {
    nis<-2.^seq(3,8,length.out=nNpoints*8)
  } else {
    nis<-2.^seq(3,8,length.out=nNpoints)
  }
  for (ei in 1:length(pRho)){
    if (design$sNRand) {
      dplus<-0
      g<-0
      for (ni in 1:(length(nis)-1)) {
        g1<-dgamma(nis[ni]-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
        d1<-zSamplingDistr(rs,pRho[ei],nis[ni])
        if (possible$sigOnly) {
          crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
          d1[abs(rs)<crit_z]<-0
        }
        dplus<-dplus+d1*g1*(nis[ni+1]-nis[ni])
        g<-g+g1*(nis[ni+1]-nis[ni])
      }
      dplus<-dplus/g
    } else {
      dplus<-zSamplingDistr(rs,pRho[ei],n)
      if (possible$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(n-3))
        dplus[abs(rs)<crit_z]<-0
      }
    }
    sDens_z[ei,]<-dplus*pRhogain[ei]
  }

  # and sum of sampling distributions
  if (source$populationPDF=="Single" && source$populationNull>0) {
    sDens_z_plus<-sDens_z[2,]
  } else {
    sDens_z_plus<-colSums(sDens_z)/sum(pRhogain)
  }

  if (design$sNRand) {
    dnull<-0
    g<-0
    for (ni in 1:(length(nis)-1)) {
      g0<-dgamma(nis[ni]-minN,shape=design$sNRandK,scale=(n-minN)/design$sNRandK)
      d0<-zSamplingDistr(rs,0,nis[ni])
      if (possible$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
        d0[abs(rs)<crit_z]<-0
      }
      dnull<-dnull+d0*g0*(nis[ni+1]-nis[ni])
      g<-g+g0*(nis[ni+1]-nis[ni])
    }
    dnull<-dnull/g
  } else {
    dnull<-zSamplingDistr(rs,0,n)
    if (possible$sigOnly) {
      crit_z<-qnorm(0.975,0,1/sqrt(n-3))
      dnull[abs(rs)<crit_z]<-0
    }
  }
  sDens_z_null<-dnull
  list(sDens_z=sDens_z,sDens_z_plus=sDens_z_plus,sDens_z_null=sDens_z_null)
}

getNDist<-function(nvals,design,logScale) {
  n<-design$sN
  if (design$sNRand) {
    ng<-dgamma(nvals-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
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
    ng<-dgamma(n-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
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
         "hypothesis"={source<-possible$world},
         "world"={source<-possible$world},
         "prior"={source<-possible$prior}
  )

  asDens_z<-zpriorDistr(zs,source$populationPDF,source$populationRZ,source$populationPDFk)
  if (source$populationNullp>0 && source$populationPDF=="Single") {
    asDens_z<-asDens_z*(1-source$populationNullp)
    asDens_z[zp==0]<-asDens_z[zp==0]+source$populationNullp
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
  
  doNullsSingle<-(prior$populationNullp>0 && prior$populationPDF=="Single")
  doNulls<-TRUE
  
  apDens_z<-zpriorDistr(zp,prior$populationPDF,prior$populationRZ,prior$populationPDFk)
  if (doNulls) {
    apDens_z_null<-apDens_z*(1-prior$populationNullp)
    apDens_z_null[zp==0]<-apDens_z_null[zp==0]+prior$populationNullp
    apDens_z_null<-apDens_z_null/max(apDens_z_null)
  } else {
    apDens_z_null<-apDens_z
  }
  if (doNullsSingle) {
    apDens_z<-apDens_z_null
  }
  
  # enumerate the source populations
  #  as r and gain 
  pR<-get_pRho(source,"z",RZ)
  pRho<-pR$pRho
  pRhogain<-pR$pRhogain
  if (RZ=="r")    pRhogain<-pRhogain/(1-tanh(pRho)^2)
  sD<-getZDist(zs,pRho,pRhogain,source,design,possible)
  sDens_z<-sD$sDens_z
  sDens_z_plus<-sD$sDens_z_plus
  sDens_z_null<-sD$sDens_z_null
  
  pR<-get_pRho(prior,"z",RZ)
  pRhoP<-pR$pRho
  pRhogainP<-pR$pRhogain
  if (RZ=="r")    pRhogainP<-pRhogainP/(1-tanh(pRhoP)^2)
  sD<-getZDist(zs,pRhoP,pRhogainP,prior,design,possible)
  pDens_z<-sD$sDens_z
  pDens_z_plus<-sD$sDens_z_plus
  pDens_z_null<-sD$sDens_z_null

  if (length(pRho)>25) {
    l<-length(pRho)
    use<-seq(1,l,length.out=(l-1)/24)
    if (RZ=="z") {
      keep<-abs(pRho[use])<z_range
      use<-use[keep]
    }
    sDens_z<-sDens_z[use,]
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
          d<-d+zSamplingDistr(zp,sRho[ei]+correction[ci],ni)*g
        }
        d<-d/sum(d)
        zDens<-zDens+d
      } else {
        zDens<-zDens+zSamplingDistr(zp,sRho[ei]+correction[ci],n[ei])
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
              for (i in 1:length(pRho)) {
                use_data<-abs(use_effects[i,])<=hist_range
                h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
                sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
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

              if (prior$populationPDF=="Single") {
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
  sDens_r<-sDens_z
  sDens_r_plus<-sDens_z_plus
  sDens_r_null<-sDens_z_null
  pDens_r<-pDens_z
  spDens_r<-spDens_z
  apDens_r<-apDens_z
  pDens_r_plus<-pDens_z_plus
  pDens_r_null<-pDens_z_null
  asDens_r<-asDens_z
  if (RZ=="r") {
    pRho<-tanh(pRho)
    sRho<-tanh(sRho)
    rp<-tanh(zp)
    rs<-tanh(zs)
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
    wp<-seq(w_range[1],w_range[2],length.out=npoints)
    wp<-c(wp,wp[npoints]+diff(wp[1:2]))
    z_use<-wn2z(wp,nUse)
  } else {
    wp<-seq(w_range[1],w_range[2],length.out=npoints)
    w<-zn2w(zp,nUse)
  }
  spDens_w<-spDens_z
  if (any(!is.na(spDens_z))) {
    for (i1 in 1:nrow(spDens_z)) {
      if (wDensMethod==1) {
        # this way round avoids awkward infinities
        z_use_dens<-approx(zp,spDens_z,z_use)$y
        wd<-(z_use_dens[1:(length(z_use)-1)]+z_use_dens[2:(length(z_use))])/2 * diff(z_use)
        z_use_dens<-approx(zp,spDens_z,-z_use)$y
        wd<-wd+(z_use_dens[1:(length(z_use)-1)]+z_use_dens[2:(length(z_use))])/2 * diff(z_use)
      } else {
        zd<-spDens_z[i1,]
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

  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain # *(1-possible$world$populationNullp)
  
  sDens_r_plus<-sDens_r_plus*(1-source$populationNullp)
  sDens_r_null<-sDens_r_null*(source$populationNullp)
  sDens_r_total<-sDens_r_plus+sDens_r_null
  dr_gain<-max(sDens_r_total)
  sDens_r_total<-sDens_r_total/dr_gain
  sDens_r_null<-sDens_r_null/dr_gain
  sDens_r_plus<-sDens_r_plus/dr_gain
  rs_stats<-densityFunctionStats(sDens_r_total,zs)
  
  if (any(!is.na(pDens_r))) {
    dr_gain<-max(pDens_r,na.rm=TRUE)
    pDens_r<-pDens_r/dr_gain
  }
  
  if (prior$worldOn && prior$populationNullp>0) {
    spDens_r<-spDens_r*(1-prior$populationNullp)
    apDens_r<-apDens_r*(1-prior$populationNullp)
    asDens_r<-asDens_r*(1-source$populationNullp)
    for (i in 1:length(sRho)) {
      spDens_r<-spDens_r*dnorm(atanh(sRho[i]),0,1/sqrt(n[i]-3))
    }
    pDens_r_plus<-pDens_r_plus/sum(pDens_r_plus)*(1-prior$populationNullp)
    pDens_r_null<-pDens_r_null/sum(pDens_r_null)*(prior$populationNullp)
  }
  rp_stats<-densityFunctionStats(pDens_r,zp) 
  wp_stats<-densityFunctionStats(spDens_w,wp) 
  
  dens_at_peak=1
  if (is.na(sRho[1])) {
    dens_at_sample<-NA
    dens_at_population<-NA
    dens_at_zero<-NA
  } else {
    dens_at_sample<-approx(zp,pDens_r,sRho[1])$y
    dens_at_population<-approx(zp,pDens_r,ResultHistory$rp[1])$y
    dens_at_zero<-approx(zp,pDens_r,0)$y
  }
  
    switch (possible$type,
          "Samples"={
            possibleResult<-list(possible=possible,
                                   pRho=pRho,
                                   sRho=sRho,
                                   n=n,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_plus=sDens_r_plus,sDens_r_null=sDens_r_null,sDens_r_total=sDens_r_total,
                                     rp=rp,pDens_r=sDens_r,spDens_r=sDens_r,apDens_r=apDens_r,asDens_r=asDens_r,
                                     rs_peak=rs_stats$peak,
                                     rs_sd=rs_stats$sd,
                                     rs_ci=rs_stats$ci,
                                     wp=wp,
                                     spDens_w=spDens_w
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
            possibleResult<-list(possible=possible,
                                   pRho=possible$targetPopulation,
                                   sRho=sRho,
                                   n=n,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_plus=sDens_r_plus,sDens_r_null=sDens_r_null,sDens_r_total=sDens_r_total,
                                     rp=rp,pDens_r=pDens_r,spDens_r=spDens_r,apDens_r=apDens_r,asDens_r=asDens_r,pDens_r_null=pDens_r_null,pDens_r_plus=pDens_r_plus,
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
