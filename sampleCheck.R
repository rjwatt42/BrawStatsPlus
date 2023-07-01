
cheatSample<-function(IV,IV2,DV,effect,design,evidence,sample,result) {
  
  if (design$sCheating=="None") return(result)
  if (isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence)) return(result)
  
  if (design$sCheating=="Retry") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      sample<-makeSample(IV,IV2,DV,effect,design)
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  if (design$sCheating=="Prune") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]
        design$sN<-length(sample1$iv)
        result1<-analyseSample(IV,IV2,DV,effect,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      keep<-ps>min(ps)
      sample$participant<-sample$participant[keep]
      sample$iv<-sample$iv[keep]
      sample$dv<-sample$dv[keep]
      sample$ivplot<-sample$ivplot[keep]
      sample$dvplot<-sample$dvplot[keep]
      
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  design2<-design
  design2$sNRand<-FALSE
  
  effect2<-effect
  effect2$populationRZ<-NA
  effect2$rIV<-result$rpIV
  
  sample2<-makeSample(IV,IV2,DV,effect2,design2)
  
  if (design$sCheating=="Grow") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      sample$participant<-c(sample$participant,length(sample$participant)+1)
      sample$iv<-c(sample$iv,sample2$iv[ntrials+1])
      sample$dv<-c(sample$dv,sample2$dv[ntrials+1])
      sample$ivplot<-c(sample$ivplot,sample2$ivplot[ntrials+1])
      sample$dvplot<-c(sample$dvplot,sample2$dvplot[ntrials+1])
      design$sN<-design$sN+1
      
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  if (design$sCheating=="Replace") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]

        result1<-analyseSample(IV,IV2,DV,effect,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      change<-which.min(ps)
      sample$iv[change]<-sample2$iv[ntrials+1]
      sample$dv[change]<-sample2$dv[ntrials+1]
      sample$ivplot[change]<-sample2$ivplot[ntrials+1]
      sample$dvplot[change]<-sample2$dvplot[ntrials+1]
      
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
}

replicateSample<-function(IV,IV2,DV,effect,design,evidence,sample,res) {
  oldalpha<-alpha
  res1<-res
  ResultHistory<-list(n=res$nval,df1=res$df1,r=res$rIV,rp=res$rpIV,p=res$pIV)
  
  if (!isempty(design$sReplicationOn) && !is.na(design$sReplicationOn) && design$sReplicationOn) {
    if (design$sReplAlpha=="half") alpha<<-oldalpha*2
    while (design$sReplSigOnly && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
      if (evidence$longHand) {
        sample<-makeSample(IV,IV2,DV,effect,design)
        res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      } else {
        res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
      }
      ResultHistory<-list(n=res$nval,df1=res$df1,r=res$rIV,rp=res$rpIV,p=res$pIV)
    }
    if (design$sReplAlpha=="half") alpha<<-oldalpha/2
    
    res1<-res
    resHold<-res
    # now we freeze the population effect size
    effect$rIV<-res$rpIV
    effect$world$worldOn<-FALSE
    # if we are doing one-tailed we need the sign
    rSign<-sign(res$rIV)
    
    design1<-design
    design1$sNRand<-FALSE
    design1$sN<-res$nval
    if (design$sReplRepeats>0) {
    for (i in 1:design$sReplRepeats) {
      if (design$sReplKeep=="cautious" && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
        break
      }
      # get the relevant sample effect size for the power calc
      if (!(isempty(design$sReplPower)) && (design$sReplPower>0)) {
        if (design$sReplCorrection) {r<-rSamp2Pop(res$rIV,design1$sN,effect$world)} else {r<-res$rIV}
      # get the new sample size
      design1$sN<-rw2n(r,design$sReplPower,design$sReplTails)
      }
      
      if (evidence$longHand) {
        sample<-makeSample(IV,IV2,DV,effect,design1)
        res<-analyseSample(IV,IV2,DV,effect,design1,evidence,sample)
      } else {
        res<-sampleShortCut(IV,IV2,DV,effect,design1,evidence,1,FALSE)
      }
      if (design$sReplTails==1) {
        if (sign(res$rIV)!=sign(ResultHistory$r[1])) {
          res$pIV<-1
        }
      }
        
      if ((design$sReplKeep=="largest" && res$nval>resHold$nval) || design$sReplKeep=="last")
        { resHold<-res }
      ResultHistory$n<-c(ResultHistory$n,res$nval)
      ResultHistory$df1<-c(ResultHistory$df1,res$df1)
      ResultHistory$r<-c(ResultHistory$r,res$rIV)
      ResultHistory$rp<-c(ResultHistory$rp,res$rpIV)
      ResultHistory$p<-c(ResultHistory$p,res$pIV)
    }
    }
    res<-resHold
    
    if (design$sReplKeep=="cautious") {
      use<-!isSignificant(STMethod,ResultHistory$p,ResultHistory$r,ResultHistory$n,ResultHistory$df1,evidence)
      use[1]<-FALSE
      if (any(use)) {
        use<-which(use)[1]
        res$rIV<-ResultHistory$r[use]
        res$nval<-ResultHistory$n[use]
        res$df1<-ResultHistory$df1[use]
        res$pIV<-ResultHistory$p[use]
      }
    }
    
    if (design$sReplKeep=="median" && design$sReplRepeats>0) {
      use<-which(ResultHistory$p==sort(ResultHistory$p)[floor(length(ResultHistory$p)/2)])
      use<-use[1]
        res$rIV<-ResultHistory$r[use]
        res$nval<-ResultHistory$n[use]
        res$df1<-ResultHistory$df1[use]
        res$pIV<-ResultHistory$p[use]
    }
    
  }
  alpha<<-oldalpha
  
  res$ResultHistory<-ResultHistory
  res$roIV<-res1$rIV
  res$no<-res1$nval
  res$df1o<-res1$df1
  res$poIV<-res1$pIV
  
  res
}
