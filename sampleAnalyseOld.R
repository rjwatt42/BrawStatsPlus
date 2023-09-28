debugHere<-FALSE


wsd<-function(x,w=1,na.rm=TRUE) {
  if (length(w)==1) {
    sqrt(mean((x-mean(x,na.rm=na.rm))^2))
  } else {
    sqrt(sum((x-mean(x,na.rm=na.rm))^2 *w)/sum(w))
  }
}


p2r<-function(p,n,df1=1) {
  if (any(abs(n)<3)) {
    print("p2r n-exception")
    n[n<3]<-4
  }
  df2<-n-(df1+1)
  
  Fvals <- qf(1-p,df1,df2)
  r <- sqrt(Fvals*df1)/sqrt(Fvals*df1+df2)
  return(r)
  
  t_vals <- qt(p/2,n-2)
  r_vals <- t_vals/sqrt(t_vals^2+(n-2))
  r_vals
}


r2p<-function(r,n,df1=1){
  if (!is.numeric(r) || !is.numeric(n)) {return(1)}
  if (any(abs(r)>1)) {
    print(paste("r2p r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2p n-exception")
    n[n<3]<-4
  }
  df2<-n-(df1+1)
  
  Fvals<-r^2/(1-r^2)*df2/df1
  p<-(1-pf(Fvals,df1,df2))
  return(p)
  
  if (any(df1>1)) {
    Fvals<-r^2/(1-r^2)*df2/df1
    (1-pf(Fvals,df1,df2))
  } else {
    t_vals<-r/r2se(r,n)
    (1-pt(abs(t_vals),df2))*2
  }
  
}

r2se<-function(r,n){
  if (any(abs(r)>1)) {
    print(paste("r2se r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2se n-exception")
    n[n<3]<-4
  }
  sqrt((1-r^2)/(as.vector(n)-2))
}

r2ci<-function(r,n,s=0){
  if (any(abs(r)>1)) {
    print(paste("r2ci r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2ci n-exception")
    n[n<3]<-4
  }
  z<-atanh(r)
  zci<-qnorm(1-0.05/2)*sqrt(1/(n-3))
  if (s==0){
    tanh(z+c(-1,1)*zci)
  } else {
    tanh(z+s*zci)
  }
}

res2llr<-function(result,method=STMethod) {
  r2llr(result$rIV,result$nval,result$df1,method,result$evidence$llr,result$evidence$prior)
}

r2llr<-function(r,n,df1,method=STMethod,llr=list(e1=c(),e2=0),world=NULL) {
  if (any(abs(r)>1)) {
    print(paste("r2llr r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2llr n-exception")
    n[n<3]<-4
  }
  z<-atanh(r)
  if (method=="dLLR") {
    z<-abs(z)
    if (!is.matrix(z)) {
      z<-matrix(z,ncol=1)
    }
    if (!is.matrix(n)) {
      n<-matrix(n,ncol=1)
    }
    if (is.null(world) || is.null(world$worldOn) || !world$worldOn) {
      world$populationPDF<-"Single"
      world$populationNullp<-0.5
    }
    lk<-getLogLikelihood(z,n,df1,world$populationPDF,world$populationPDFk,worldDistNullP=c(0,1),remove_nonsig=FALSE,doSeparate=TRUE)
    lk1<-lk[,,1]+log(1-world$populationNullp)
    lk2<-lk[,,2]+log(world$populationNullp)
    llk<-lk1-lk2
  } else {
    if (isempty(llr$e1) || is.na(llr$e1)) { llr1=z }
    else {llr1=llr$e1}
    lk1<-dnorm(llr1,mean=z,sd=1/sqrt(n-3))
    lk2<-dnorm(llr$e2,mean=z,sd=1/sqrt(n-3))
    llk<-log(lk1/lk2)
  }
  llk
  # for llr1=0 this can be simplified to just this: z^2*(n-3)/2
}



model2directeffect<-function(mF){
  if (any(class(mF)[1]==c("lmerMod","glmerMod")))
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
    expected<-fitted(mF)
    residuals<-residuals(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
    expected<-mF$fitted.values
    residuals<-mF$residuals
  }
  dv<-data[,1]
  n<-length(dv)
  
  if (is.numeric(dv)) {
    DVgain<-wsd(dv,na.rm=TRUE)
  } else {
    DVgain<-wsd(expected+residuals)
  }
  
  # single variable model
  if (length(mTerms)==1 && !grepl(":",mTerms)) {
    v1<-data[[mTerms[1]]]
    
    if (is.numeric(v1)) {
      m1<-mean(v1,na.rm=TRUE)
      h1<-c(1,1)
      data1<-data.frame(iv1=m1+c(-1,1)*wsd(v1))
    } else {
      h1<-as.numeric(prop.table(table(v1)))
      data1<-data.frame(iv1=levels(v1))
    }
    names(data1)<-mTerms[1]
    data1<-as.list(data1)
    # names(data1)<-colnames(mF$model)[2]
    if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
      nc1<-predict(mF,cbind(data1,data.frame(participant=mF@frame$participant[1])))
    } else {
      nc1<-predict.lm(mF,data1)    
    }
    ncoeff<-wsd(nc1,h1) * sign(sum(diff(nc1)))
    return(ncoeff/DVgain)
  }
  
  if (length(mTerms)==1 && grepl(":",mTerms)) {
    expl<-wsd(expected)/DVgain * sign(sum(diff(mF$coefficients),na.rm=TRUE))
    return(expl)
  }
  
  # multiple variable model
  v1<-data[,2]
  v2<-data[,3]

  if (any(mTerms=="iv1")) {
    if (is.numeric(v1)){
      DV1var<-wsd(v1*mF$coefficients[["iv1"]])*sign(mF$coefficients[["iv1"]])
    } else {
      nlev1<-length(levels(v1))
      v1a<-0
      c1<-0
      for (i in 2:nlev1) {
        coeff<-mF$coefficients[[paste0("iv1",levels(v1)[i])]]
        c1<-c(c1,coeff)
        v1a<-v1a+(v1==levels(v1)[i])*coeff
      }
      DV1var<-wsd(v1a)*sign(sum(diff(c1)))
    }
  } else DV1var<-0
  
  if (any(mTerms=="iv2")) {
    if (is.numeric(v2)){
      DV2var<-wsd(v2*mF$coefficients[["iv2"]])*sign(mF$coefficients[["iv2"]])
    } else {
      nlev2<-length(levels(v2))
      v2a<-0
      c2<-0
      for (i in 2:nlev2) {
        coeff<-mF$coefficients[[paste0("iv2",levels(v2)[i])]]
        c2<-c(c2,coeff)
        v2a<-v2a+(v2==levels(v2)[i])*coeff
      }
      DV2var<-wsd(v2a)*sign(sum(diff(c2)))
    }
  } else DV2var<-0
  
  if (any(mTerms=="iv1:iv2")) {
    if (is.numeric(v1) && is.numeric(v2)) {
      DV12var<-wsd(v1*v2*mF$coefficients[["iv1:iv2"]])*sign(mF$coefficients[["iv1:iv2"]])
    } 
    if (!is.numeric(v1) && is.numeric(v2)) {
      nlev1<-length(levels(v1))
      v12a<-0
      c12<-0
      for (i in 2:nlev1) {
        coeff<-mF$coefficients[[paste0("iv1",levels(v1)[i],":","iv2")]]
        if (is.na(coeff)) coeff<-0
        c12<-c(c12,coeff)
        v12a<-v12a+v2*(v1==levels(v1)[i])*coeff
      }
      DV12var<-wsd(v12a)*sign(sum(diff(c12)))
    } 
    if (is.numeric(v1) && !is.numeric(v2)) {
      nlev2<-length(levels(v2))
      v12a<-0
      c12<-0
      for (i in 2:nlev2) {
        coeff<-mF$coefficients[[paste0("iv1",":","iv2",levels(v2)[i])]]
        if (is.na(coeff)) coeff<-0
        c12<-c(c12,coeff)
        v12a<-v12a+v1*(v2==levels(v2)[i])*coeff
      }
      DV12var<-wsd(v12a)*sign(sum(diff(c12)))
    } 
    if (!is.numeric(v1) && !is.numeric(v2)) {
      nlev1<-length(levels(v1))
      nlev2<-length(levels(v2))
      v12a<-0
      c12<-0
      for (i1 in 2:nlev1) {
        for (i2 in 2:nlev2) {
          coeff<-mF$coefficients[[paste0("iv1",levels(v1)[i1],":","iv2",levels(v2)[i2])]]
          if (is.na(coeff)) coeff<-0
          c12<-c(c12,coeff)
          v12a<-v12a+(v1==levels(v1)[i1])*(v2==levels(v2)[i2])*coeff
        }
      }
      DV12var<-wsd(v12a)*sign(sum(diff(c12)))
      # if (is.na(DV12var)) browser()
    }
  } else DV12var<-0

if (1==2)
  switch (length(mTerms), 
          {return(DV12var/DVgain)},
          {return(c(DV1var, DV2var)/DVgain)},
          {return(c(DV1var, DV2var, DV12var)/DVgain)}
  )
  
  
  if (is.numeric(v1)){
    h1<-c(1,1)
    m1<-mean(v1,na.rm=TRUE)+c(-1,1)*wsd(v1,na.rm=TRUE)
  } else {
    h1<-as.numeric(prop.table(table(v1)))
    m1<-levels(v1)
  }
  if (is.numeric(v2)){
    h2<-c(1,1)
    m2<-mean(v2,na.rm=TRUE)+c(-1,1)*wsd(v2,na.rm=TRUE)
  } else {
    h2<-as.numeric(prop.table(table(v2)))
    m2<-levels(v2)
  }
  n1<-length(m1)
  n2<-length(m2)
  igrid<-meshgrid(1:n1,1:n2)
  data1<-data.frame(iv1=as.vector(m1[igrid$X]),iv2=as.vector(m2[igrid$Y]))

  # we find effect-size by 
  # looking at the change in prediction around the centre of an interval predictor
  # looking at the sd of predictions for all possible cases of categorical predictors
  
  if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
    p1<-data$participant
    p1<-levels(p1)[1]
    v<-predict(mF,cbind(data1,data.frame(participant=p1)))
  } else {
    v<-predict.lm(mF,data1)
  }
  dim(v)<-c(n2,n1)
  
  nc1<-colMeans(v)
  nc2<-rowMeans(v)
  
  ncoeff1<-wsd(nc1,h1) * sign(sum(diff(nc1)))
  ncoeff2<-wsd(nc2,h2) * sign(sum(diff(nc2)))
  
  if (any(mTerms=="iv1:iv2")) {
    ie<-t(t(v)-nc1)-nc2
    ncoeff3<-wsd(ie,1)*sign(sum(diff(-ie[1,])))
  }

    switch (length(mTerms), 
          {return(ncoeff3/DVgain)},
          {return(c(ncoeff1, ncoeff2)/DVgain)},
          {return(c(ncoeff1, ncoeff2, ncoeff3)/DVgain)}
  )
  
}

appendList <- function (x1, x2) 
{
  xnames <- names(x1)
  for (v in names(x2)) {
    x2[[v]]<-if (is.null(x2[[v]])) NA else x2[[v]]
    x1[[v]] <- if (v %in% xnames && is.list(x1[[v]]) && is.list(x2[[v]])) 
      appendList(x1[[v]], x2[[v]])
    else c(x1[[v]], x2[[v]])
  }
  x1
}


multipleAnalysis<-function(IV,IV2,DV,effect,design,evidence,n_sims=1,appendData=FALSE, earlierResult=c(),sigOnly=FALSE, showProgress=TRUE,progressPrefix=""){
  if (n_sims==1) {showProgress<-FALSE}
  
  rho<-effect$rIV
  rho2<-effect$rIV2
  
  pvals=c()
  rvals=c()
  nvals=c()
  df1vals<-c()
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }

  nterms<-0
  if (IV$type=="Categorical") {nterms<-nterms+IV$ncats-1} else {nterms<-nterms+1}
  if (!is.null(IV2)) {
    if (IV2$type=="Categorical") {nterms<-nterms+IV2$ncats-1} else {nterms<-nterms+1}
  }

    if (appendData) {
    mainResult<-earlierResult
  } else {
    mainResult<-list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),
                   rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),
                   nval=c(),df1=c(),
                   r=list(direct=c(),unique=c(),total=c(),coefficients=c()),
                   p=list(direct=c(),unique=c(),total=c()),
                   showType=design$showType)
  }
  
  for (i in 1:n_sims){
    if (showProgress && (n_sims<=50 || (n_sims>50 && i==round(i/25)*25))) {
      off<-length(mainResult$rIV)+1
      off<-0
      showNotification(paste(progressPrefix,format(i+off),"/",format(n_sims+off)),id="counting",duration=Inf,closeButton=FALSE,type="message")
    } 
    effect$rIV<-rho[i]
    if (!is.null(IV2)) {effect$rIV2<-rho2[i]}
    
    res<-runSimulation(IV,IV2,DV,effect,design,evidence,sigOnly)
      
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0
    }
    newResult<-list(rpIV=res$rpIV,roIV=res$roIV,rIV=res$rIV,pIV=res$pIV,poIV=res$poIV,
                    rIV2=res$rIV2,pIV2=res$pIV2,rIVIV2DV=res$rIVIV2DV,pIVIV2DV=res$pIVIV2DV,
                    nval=res$nval,df1=res$df1,
                    r=list(direct=res$r$direct,unique=res$r$unique,total=res$r$total,coefficients=res$r$coefficients),
                    p=list(direct=res$p$direct,unique=res$p$unique,total=res$p$total),
                    showType=design$showType)
    

   if (any(class(res$rawModel)[1]==c("lmerMod","glmerMod"))) {
      coeffs<-colMeans(coef(res$rawModel)$participant)
    } else {
      coeffs<-res$rawModel$coefficients
    }
    newResult$r$coefficients<-as.double(coeffs[2:length(coeffs)])
    # mainResult<-appendList(mainResult,newResult)
    
    mainResult$rpIV<-rbind(mainResult$rpIV,newResult$rpIV)
    mainResult$rIV<-rbind(mainResult$rIV,newResult$rIV)
    mainResult$roIV<-rbind(mainResult$roIV,newResult$roIV)
    mainResult$pIV<-rbind(mainResult$pIV,newResult$pIV)
    mainResult$poIV<-rbind(mainResult$poIV,newResult$poIV)
    mainResult$nval<-rbind(mainResult$nval,newResult$nval)
    mainResult$df1<-rbind(mainResult$df1,newResult$df1)
    if (!is.null(IV2)){
      mainResult$rIV2<-rbind(mainResult$rIV2,newResult$rIV2)
      mainResult$pIV2<-rbind(mainResult$pIV2,newResult$pIV2)
      mainResult$rIVIV2DV<-rbind(mainResult$rIVIV2DV,newResult$rIVIV2DV)
      mainResult$pIVIV2DV<-rbind(mainResult$pIVIV2DV,newResult$pIVIV2DV)

      mainResult$r$direct<-rbind(mainResult$r$direct,newResult$r$direct)
      mainResult$r$unique<-rbind(mainResult$r$unique,newResult$r$unique)
      mainResult$r$total<-rbind(mainResult$r$total,newResult$r$total)
      mainResult$r$coefficients<-rbind(mainResult$r$coefficients,newResult$r$coefficients)

      mainResult$p$direct<-rbind(mainResult$p$direct,newResult$p$direct)
      mainResult$p$unique<-rbind(mainResult$p$unique,newResult$p$unique)
      mainResult$p$total<-rbind(mainResult$p$total,newResult$p$total)

    } else {
      mainResult$rIV2<-rbind(mainResult$rIV2,NA)
      mainResult$pIV2<-rbind(mainResult$pIV2,NA)
      mainResult$rIVIV2DV<-rbind(mainResult$rIVIV2DV,NA)
      mainResult$pIVIV2DV<-rbind(mainResult$pIVIV2DV,NA)

      mainResult$r$direct<-rbind(mainResult$r$direct,newResult$rIV)
      mainResult$r$unique<-rbind(mainResult$r$unique,newResult$rIV)
      mainResult$r$total<-rbind(mainResult$r$total,newResult$rIV)
      mainResult$r$coefficients<-rbind(mainResult$r$coefficients,newResult$r$coefficients)

      mainResult$p$direct<-rbind(mainResult$p$direct,newResult$pIV)
      mainResult$p$unique<-rbind(mainResult$p$unique,newResult$pIV)
      mainResult$p$total<-rbind(mainResult$p$total,newResult$pIV)

    }
  }
  # if (showProgress) removeNotification(id="counting")
  mainResult$showType<-evidence$showType
  mainResult$effect<-effect
  mainResult$design<-design
  mainResult$evidence<-evidence
  
  mainResult
}

convert2Interval<-function(var) {
  var$type<-"Interval"
  var$mu<-var$median
  var$sd<-var$iqr*qnorm(0.75)
}

analyseSample<-function(IV,IV2,DV,effect,design,evidence,result){

  if (is.null(IV2)) {no_ivs<-1} else {no_ivs<-2}
  # CONVERT ALL ORDINAL VARIABLES TO INTERVAL  
  if (IV$type=="Ordinal") {
    IV$type<-"Interval"
    }
  if (!is.null(IV2) && IV2$type=="Ordinal") {
    IV2$type<-"Interval"
    }
  if (!is.null(IV2) && DV$type=="Ordinal") {
    DV$type<-"Interval"
    }
  if (IV$type=="Categorical") {
    df1<-IV$ncats-1
  } else {
    df1<-1
  }
  if (!is.null(IV2) && IV2$type=="Categorical") {
    df2<-IV2$ncats-1
  } else {
    df2<-1
  }
  if (!is.null(IV2)) {
    if (IV$type=="Categorical" && IV2$type=="Categorical") {
    df12<-df1*df2
    } else {
      df12<-df1*df2
    }
  } else {
    df2<-0
    df12<-0
  }
    
  # remove duplicated rows (from covariates of within designs)
  # if (is.null(IV2)){
  #   waste<-duplicated(data.frame(pt=result$participant,iv=result$iv,dv=result$dvplot))
  #   iv1<-result$iv[!waste]
  #   iv2<-result$iv2
  #   dv<-result$dv[!waste]
  # } else {
  #   waste<-duplicated(data.frame(pt=result$participant,iv=result$iv,iv2=result$iv2,dv=result$dvplot))
  #   iv1<-result$iv[!waste]
  #   iv2<-result$iv2[!waste]
  #   dv<-result$dv[!waste]
  # }
  
  iv1<-result$iv
  iv2<-result$iv2
  dv<-result$dv
  
  if (is.factor(iv1) && all(iv1==iv1[1])) {
    result$rIV<-NA
    result$pIV<-NA
    result$rpIV<-NA
    return(result)
    }
  # if (is.factor(iv2) && all(iv2==iv2[1])) {return(NA)}
  
  n<-length(dv)
  
  # MAKE MAIN DATA STORAGE
  resultRawData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  #MAKE NORM DATA STORAGE
    # centre variables on zero
    # this helps with the interaction term
    if (IV$type=="Interval")  iv1=(iv1-mean(iv1,na.rm=TRUE))#/sd(iv1,na.rm=TRUE)
    if (!is.null(IV2) && IV2$type=="Interval") iv2=(iv2-mean(iv2,na.rm=TRUE))#/sd(iv2,na.rm=TRUE)
    if (DV$type=="Interval")  dv=(dv-mean(dv,na.rm=TRUE))#/sd(dv,na.rm=TRUE)
    # make data frame
  resultNormData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  # get Categorical cases sorted
  if (IV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv1[1])},
            "Frequency"={ref=which.max(tabulate(match(iv1, IV$cases)))}
    )
    resultNormData$iv1<-relevel(resultNormData$iv1,ref=ref)
  }
  if (!is.null(IV2) && IV2$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv2[1])},
            "Frequency"={ref=which.max(tabulate(match(iv2, IV2$cases)))}
    )
    resultNormData$iv2<-relevel(resultNormData$iv2,ref=ref)
  }
  if (DV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(dv[1])},
            "Frequency"={ref=which.max(tabulate(match(dv, DV$cases)))}
    )
    resultNormData$dv<-relevel(resultNormData$dv,ref=ref)
  }
  
# CREATE FORMULA
  formula<-"dv~iv1"
  if (!is.null(IV2)) {
    formula<-paste(formula,"+iv2",sep="")
    if (evidence$rInteractionOn==1) formula<-paste(formula,"+iv1:iv2",sep="")
  }
  if ((IV$type=="Categorical" && design$sIV1Use=="Within") || (!is.null(IV2) && IV2$type=="Categorical" && design$sIV2Use=="Within")){
    doingWithin<-TRUE
    formula<-paste(formula,"+(1|participant)",sep="")
    if ((IV$type=="Categorical" && design$sIV1Use=="Within") && (!is.null(IV2) && IV2$type=="Categorical" && design$sIV2Use=="Within")){
      formula<-paste(formula,"+(1|iv1:participant)+(1|iv2:participant)",sep="")
    }
  } else {
    doingWithin<-FALSE
  }

# SET UP CONTRASTS
  # these are needed to make the anova type 3 work properly
  contrasts<-c()
  if (IV$type=="Categorical")                   contrasts<-c(contrasts,list(iv1=contr.sum))
  if (!is.null(IV2) && IV2$type=="Categorical") contrasts<-c(contrasts,list(iv2=contr.sum))

  # get linear model and anova
  if (DV$type=="Categorical") {
    if (doingWithin) {
      lmRaw<-glmer(formula=as.formula(formula),data=resultRawData,family="binomial")
      lmRawC<-glmer(formula=as.formula(formula),data=resultRawData,family="binomial",contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-glmer(formula=as.formula(formula),data=resultNormData,family="binomial")
      lmNormC<-glmer(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
      testMethod<-"Chisq"
    } else {
      lmRaw<-glm(formula=as.formula(formula),data=resultRawData,family="binomial")
      lmRawC<-glm(formula=as.formula(formula),data=resultRawData,family="binomial",contrasts=contrasts)
      lmNorm<-glm(formula=as.formula(formula),data=resultNormData,family="binomial")
      lmNormC<-glm(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
      testMethod<-"F"
    }
    pcol=3;prow=2
    
  } else { # Interval DV
    # lmRaw to report model
    if (doingWithin) {
      # print(cor(resultRawData[1:42,4],resultRawData[43:84,4]))
      lmRaw<-lmer(formula=as.formula(formula),data=resultRawData)
      lmRawC<-lmer(formula=as.formula(formula),data=resultRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lmer(formula=as.formula(formula),data=resultNormData)
      lmNormC<-lmer(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    } else {
      lmRaw<-lm(formula=as.formula(formula),data=resultRawData)
      lmRawC<-lm(formula=as.formula(formula),data=resultRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lm(formula=as.formula(formula),data=resultNormData)
      lmNormC<-lm(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    }
    testMethod<-"F"
    pcol=4;prow=2;
  }

  switch (evidence$ssqType,
          "Type1"={
            anRaw<-Anova(lmRaw,test=testMethod)
            anRawC<-Anova(lmRawC,test=testMethod)
            anNorm<-Anova(lmNorm,test=testMethod)
            anNormC<-Anova(lmNormC,test=testMethod)
          },
          "Type2"={
            anRaw<-Anova(lmRaw,test=testMethod,type=2)
            anRawC<-Anova(lmRawC,test=testMethod,type=2)
            anNorm<-Anova(lmNorm,test=testMethod,type=2)
            anNormC<-Anova(lmNormC,test=testMethod,type=2)
          },
          "Type3"={
            anRaw<-Anova(lmRaw,test=testMethod,type=3,singular.ok=TRUE)
            anRawC<-Anova(lmRawC,test=testMethod,type=3,singular.ok=TRUE)
            anNorm<-Anova(lmNorm,test=testMethod,type=3,singular.ok=TRUE)
            anNormC<-Anova(lmNormC,test=testMethod,type=3,singular.ok=TRUE)
          }
          )

  anU<-Anova(lmNormC,test=testMethod,type=3,singular.ok=TRUE)
  if (any(class(lmRaw)[1]==c("lmerMod","glmerMod"))) {
    # we need to sort this to match Jamovi etc
    # we use anNormC as the starting point, but replace any Interval predictors
    anRaw<-anNormC
    if (IV$type!="Categorical") {
      anRaw["iv1",]<-anNormC["iv1",]
    }
    if (!is.null(IV2) && IV2$type!="Categorical") {
      anRaw["iv2",]<-anNormC["iv2",]
    }
    # now we move cells around
    newRow<-nrow(anRaw)+1
    anRaw[newRow,]<-c(NA,anRaw$Df.res[2],0,NA)
    rownames(anRaw)[newRow]<-"Error"
    anRaw["Df.res"]<-NA
    anRaw<-anRaw[,c(3,2,1,4)]
    # ssqE<-sum(result$rawModel@resp$wtres^2)
    # ssqIV<-sum((result$rawModel@resp$mu)^2)
    # anova[,1]<-c(0,ssqIV,ssqE)
    colnames(anRaw)[1]<-""
    anRaw[,1]<-NA
    anRaw[1,2]<-0 # intercept df
    if ((IV$deploy=="Within") && (!is.null(IV2) && IV2$deploy=="Within")) {
      z1<-summary(aov(dv~iv1*iv2+Error(participant/(iv1*iv2)),resultRawData))
      F<-c(0,
           z1$'Error: participant:iv1'[[1]]$'F value'[1],
           z1$'Error: participant:iv2'[[1]]$'F value'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'F value'[1],
           NA
           )
      p<-c(0,
           z1$'Error: participant:iv1'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv2'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'Pr(>F)'[1],
           NA
      )
      anRaw[,3]<-F
      anRaw[,4]<-p
    }
    
  }

  switch (no_ivs,
          { result$rpIV<-result$effectRho
            result$rIV<-model2directeffect(lmNormC)
            result$pIV<-r2p(result$rIV,n,df1)
            result$rIVCI<-r2ci(result$rIV,n)
            result$pIVCI<-r2p(result$rCI,n,df1)
            if (result$rIV>0 && result$rIVCI[1]<0 && result$rIVCI[2]>0) result$pIVCI[1]<-1
            if (result$rIV<0 && result$rIVCI[1]<0 && result$rIVCI[2]>0) result$pIVCI[2]<-1
            # overall model effect-size
            result$rFull<-result$rIV
            result$rFullse<-r2se(result$rFull,n)
            result$rFullCI<-r2ci(result$rFull,n)
            result$wFull<-rn2w(result$rFull,n)
            result$wFulln80<-rw2n(result$rFull,0.8)
          },
          # 2 ivs
          { if (doingWithin) {
            # overall model effect-size
            if (class(lmNormC)=="glmerMod") {
              result$rFull<-sqrt((sum(anNormC$Chisq)-anNormC$Chisq[1])/nlevels(result$participant))
            } else {
              result$rFull<-r.squaredGLMM(lmNormC)[[1]]
            }
            result$rFullse<-r2se(result$rFull,n)
            result$rFullCI<-r2ci(result$rFull,n)
            result$wFull<-rn2w(result$rFull,n)
            result$wFulln80<-rw2n(result$rFull,0.8)
            
            directEffects<-model2directeffect(lmNormC)
            result$rIV<-directEffects[1]
            result$pIV<-r2p(result$rIV,n,df1)
            #  IV2 next    
            result$rIV2<-directEffects[2]
            result$pIV2<-r2p(result$rIV2,n,df2)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-directEffects[3]
              result$pIVIV2DV<-r2p(result$rIVIV2DV,n,df12)
            } else {
              result$rIVIV2DV<-NA
              result$pIVIV2DV<-NA
            }
            result$rIVIV2<-0
            
            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-r.direct
            r.total<-r.direct
            
          } else {
            # overall model effect-size
            result$rFull<-sqrt(sum(anU$`Sum Sq`[is.element(rownames(anU),c("iv1","iv2","iv1:iv2"))])/sum(anU$`Sum Sq`))
            result$rFullse<-r2se(result$rFull,n)
            result$rFullCI<-r2ci(result$rFull,n)
            result$wFull<-rn2w(result$rFull,n)
            result$wFulln80<-rw2n(result$rFull,0.8)
            
            # 1. direct effect sizes for individual IVs
            #  IV first
            directEffects<-model2directeffect(lmNorm)
            if (any(abs(directEffects)>1)) {print("direct")}
            result$rIV<-directEffects[1]
            result$pIV<-r2p(result$rIV,n,df1)
            #  IV2 next    
            result$rIV2<-directEffects[2]
            result$pIV2<-r2p(result$rIV2,n,df2)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-directEffects[3]
              result$pIVIV2DV<-r2p(result$rIVIV2DV,n,df12)
            } else {
              result$rIVIV2DV<-NA
              result$pIVIV2DV<-NA
            }
            #  find the covariation
            r12<-result
            r12$dv<-result$iv2
            # r12<-analyseSample(IV,NULL,IV2,effect,design,evidence,r12)
            # result$rIVIV2<-r12$rIV
            
            # 2. find the unique and total effects
            # total model: with the single term
            # plain model: no interaction
            totalSD<-sd(lmNormC$fitted.values+lmNormC$residuals)
            switch (DV$type,
                    "Interval"={
                      # total effect sizes
                      lm1total<-lm(formula=dv~iv1,data=resultNormData)
                      lm2total<-lm(formula=dv~iv2,data=resultNormData)
                      lm12total<-lm(formula=dv~iv1:iv2,data=resultNormData,contrasts=contrasts)
                    },
                    "Categorical"={
                      lm1total<-glm(formula=dv~iv1,data=resultNormData,family="binomial")
                      lm2total<-glm(formula=dv~iv2,data=resultNormData,family="binomial")
                      lm12total<-glm(formula=dv~iv1:iv2,data=resultNormData,family="binomial")
                    }
            )
            rIV1total<-model2directeffect(lm1total)
            rIV2total<-model2directeffect(lm2total)
            rIVIV2DVtotal<-model2directeffect(lm12total)
            totalEffects<-c(rIV1total,rIV2total,rIVIV2DVtotal)
            if (any(abs(totalEffects)>1,na.rm=TRUE)) {print("total")}
            
            # get the unique effects
            if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
            n2<-nrow(anU)
            uniqueEffects<-sqrt(anU$`Sum Sq`[n1:(n2-1)]/sum(anU$`Sum Sq`[n1:n2]))
            uniqueEffects<-uniqueEffects*sign(directEffects)
            if (any(abs(uniqueEffects)>1,na.rm=TRUE)) {print("unique")}
            
            r.direct<-directEffects
            r.unique<-uniqueEffects
            r.total<-totalEffects
          }
            r.direct[is.na(r.direct)]=0
            r.unique[is.na(r.unique)]=0
            r.total[is.na(r.total)]=0
          }
  )
  
  if (is.null(IV2)) {
    hypothesisType=paste(IV$type,DV$type,sep=" ")
    switch (hypothesisType,
            "Interval Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Ordinal Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Categorical Interval"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"t-test: Independent Samples"
                  if (any(c(sum(iv1==levels(iv1)[1]),sum(iv1==levels(iv1)[2]))<3))
                  {             
                    tval<-0
                    result$pIV<-1
                    df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                  } else {
                  tv<-t.test(dv~iv1,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                  df<-paste("(",format(tv$parameter),")",sep="")
                  }
                }
                t_name<-"t"
                # tval<-sqrt(anRaw$`F value`[2])*sign(result$rIV)
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"One-Way ANOVA: Repeated Measures"
                } else {
                  an_name<-"One-Way ANOVA: Independent Measures"
                }
                t_name<-"F"
                if (evidence$Welch) {
                  tv<-oneway.test(dv~iv1, data = resultRawData, var.equal = FALSE)
                  tval<-tv$statistic
                  df<-paste("(",format(tv$parameter[1]),",",format(tv$parameter[2],digits=3),")",sep="")
                  result$pIV<-tv$p.value
                } else {
                  tval<-anRaw$`F value`[2]
                  if (is.null(tval)) {tval<-anRaw$F[2]}
                  df<-paste("(",format(anRaw$Df[2]),",",format(anRaw$Df[3]),")",sep="")
                  result$pIV<-anRaw$"Pr(>F)"[2]
                }
              }
            },
            "Interval Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Ordinal Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Categorical Ordinal"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Wilcoxon signed-rank Test: Paired Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"T"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,paired=TRUE,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Mann Whitney U test: Independent Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"U"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Friedman Test: Repeated Measures"
                  op <- options(warn = (-1))
                  tv<-friedman(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Kruskal Wallis Test: Independent Measures"
                  op <- options(warn = (-1))
                  tv<-kruskal.test(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
                df<-paste("(",format(anRaw$Df[2]),",n=",format(length(dv)),")",sep="")
              }
            },
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmRaw$null.deviance-lmRaw$deviance
              result$pIV<-1-pchisq(tval,1) # noCases-1
              
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmRaw$null.deviance-lmRaw$deviance
              result$pIV<-1-pchisq(tval,1) # noCases-1
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"

              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              df<-paste("(",format(chiResult$parameter),",","n=",format(length(result$participant)),")",sep="")
              nhold<-c()
              for (ini in 1:DV$ncats) {
                nhold<-c(nhold,sum(as.numeric(result$dv)==ini))
              }
              ncorrection<-(max(nhold)/min(nhold))
              # ncorrection<-1
              result$rIV<-sqrt(unname(chiResult$statistic/n/ncorrection))*sign(result$rIV)
              result$pIV<-chiResult$p.value
              result$rFull<-result$rIV
              result$rFullse<-r2se(result$rFull,n)
              tval<-chiResult$statistic
            }
    )
    if (is.na(result$pIV)) {result$pIV<-1}
  } else {
    switch (DV$type,
            "Interval"={
              an_name<-"General Linear Model"
              t_name<-"F"
              df<-anRaw$Df
              tval<-anRaw$`F value`
            },
            "Categorical"={
              an_name<-"Generalized Linear Model"
              t_name<-"chi2"
              df<-anRaw$Df
              tval<-anRaw$Deviance
            }
    )
    
    switch (length(r.direct),
            {df<-df1},
            {df<-c(df1,df2)},
            {df<-c(df1,df2,df12)}
    )
    
    p.direct<-r2p(r.direct,n,df)
    p.unique<-r2p(r.unique,n,df)
    p.total<-r2p(r.total,n,df)
    result$rIV<-r.direct[1]
    result$rIV2<-r.direct[2]
    result$rIVIV2DV<-r.direct[3]
    result$pIV<-p.direct[1]
    result$pIV2<-p.direct[2]
    result$pIVIV2DV<-p.direct[3]
    
    if (!evidence$rInteractionOn) {
      r.direct<-r.direct[1:2]
      r.unique<-r.unique[1:2]
      r.total<-r.total[1:2]
      
      p.direct<-p.direct[1:2]
      p.unique<-p.unique[1:2]
      p.total<-p.total[1:2]
    }
    
    result$r=list(direct=r.direct,unique=r.unique,total=r.total)
    result$rse=list(direct=r2se(r.direct,n),unique=r2se(r.unique,n),total=r2se(r.total,n))
    result$p=list(direct=p.direct,unique=p.unique,total=p.total)
  }
  
  # adding fields to existing result
  result$rawModel<-lmRaw
  result$normModel<-lmNorm
  result$rawModelC<-lmRawC
  result$normModelC<-lmNormC
  result$rawAnova<-anRaw
  result$normAnova<-anNorm
  result$rawAnovaC<-anRawC
  result$normAnovaC<-anNormC
  result$nval<-n
  if (IV$type=="Categorical") {
    result$df1<-IV$ncats-1
  } else {
    result$df1<-1
  }
  
  result$model<-result$rawModel
  result$anova<-result$rawAnovaC
  
  result$an_name<-an_name
  result$test_name<-t_name
  result$df<-df
  result$test_val<-tval
  
  result$effect<-effect
  result$design<-design
  result$evidence<-evidence
  
  result$ResultHistory<-ResultHistory
  
  result$showType<-evidence$showType
  result$Heteroscedasticity<-0
  # result$wval<-rn2w(result$rIV,result$nval)
  # result$nwval<-rw2n(result$rIV,0.8)
  result
  
}

runSimulation<-function(IV,IV2,DV,effect,design,evidence,sig_only=FALSE,onlyAnalysis=FALSE,oldResult=NULL) {
  if (onlyAnalysis && !is.null(oldResult)) {
    res<-analyseSample(IV,IV2,DV,effect,design,evidence,oldResult)
    return(res)
  }
  
  ntrials<-0
  p_min<-1
  while (1==1) {
    if (!shortHand) {
      sample<-makeSample(IV,IV2,DV,effect,design)
      res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
    } else {
      res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
    }
    if (design$sBudgetOn) {
      if (res$pIV<p_min) {
        p_min<-res$pIV
        res1<-res
      } else {
        res<-res1
      }
      ntrials<-ntrials+res$nval
      if (ntrials>=design$sNBudget) {
        break
      }
    } else {
      break
    }
  }
  
  # sig only
  while (sig_only && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
    if (!shortHand) {
      sample<-makeSample(IV,IV2,DV,effect,design)
      res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
    } else {
      res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
    }
  }
  # Cheating ?
  res<-cheatSample(IV,IV2,DV,effect,design,evidence,sample,res)
  # Replication?
  res<-replicateSample(IV,IV2,DV,effect,design,evidence,sample,res)
  
  res
  
}

