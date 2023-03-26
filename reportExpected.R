
reportExpected<-function(IV,IV2,DV,effect,evidence,expected,result,nullresult){
  
  if (is.null(IV2) | is.null(result$rIVIV2DV)){nc=3}
  else{
    if (is.na(result$rIVIV2DV[1])) {nc=6} else {nc=9}
  }
  if (expected$type=="NHSTErrors" || expected$type=="FDR"){nc=4}
  
  # header
  if (length(nullresult$rIV)>0) {
    outputText<-c("\bExpected",paste("nsims=",format(length(result$rIV)),"+",format(length(nullresult$rIV)),sep=""),rep("",nc-2))
  } else {
    outputText<-c("\bExpected",paste("nsims=",format(length(result$rIV)),sep=""),rep("",nc-2))
  }
  outputText<-c(outputText,rep("",nc))
  if (!(is.null(IV2) | is.null(result$rIVIV2DV))){
    outputText<-c(outputText,"","\b Main Effect 1","","","\b Main Effect 2","")
    if (!is.na(result$rIVIV2DV[1])) outputText<-c(outputText,"","\bInteraction","")
    }

  if (is.null(IV2)) {
    rs<-result$rIV
    ps<-result$pIV
  } else {
    switch (result$showType,
            "direct"={rs<-result$r$direct
                      ps<-result$p$direct},
            "unique"={rs<-result$r$unique
                      ps<-result$p$unique},
            "total"={rs<-result$r$total
                     ps<-result$p$total},
            "all"={
                  rs<-c()
                  ps<-c()
                  xoff<-c()
                  for (jk in 1:ncol(result$r$direct)) {
                    rs<-cbind(rs,result$r$direct[,jk],result$r$unique[,jk],result$r$total[,jk])
                    ps<-cbind(ps,result$p$direct[,jk],result$p$unique[,jk],result$p$total[,jk])
                    xoff<-cbind(xoff,c(0,0.2,0.4)+(jk-1))
                  }
                },
            "coefficients"={rs<-result$r$coefficients
                      ps<-result$p$direct}
    )
  }
  
  # column labels
  if (expected$type=="NHSTErrors") {outputText1<-c("!j\bErrors:","\bI","\bII"," ")}
  else {
    if(expected$type=="CILimits") {outputText1<-c("   ","lower","upper")}
    else {
      outputText1<-c("   ",paste0("\b",expected$Expected_par1),paste0("\b",expected$Expected_par2))
    }
  }
  outputText<-c(outputText,rep(outputText1,nc/3))

  if (expected$type=="NHSTErrors"){
    nullSig<-isSignificant(STMethod,nullresult$pIV,nullresult$rIV,nullresult$nval,nullresult$evidence)
    resSig<-isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$evidence)
      e1=paste0(format(mean(nullSig)*100,digits=report_precision),"%")
      e2=paste0(format(mean(!resSig)*100,digits=report_precision),"%")
      if (result$effect$world$worldOn) {
        nr<-(length(nullresult$pIV)+length(result$pIV))
        # ea=paste0("Combined: ",format((sum(nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%")
        e1a<-paste0(format(sum(nullSig)/nr*100,digits=report_precision),"%")
        e2a<-paste0(format(sum(!resSig)/nr*100,digits=report_precision),"%")
        outputText<-c(outputText,"!jAll",e1a,e2a," ")
        outputText<-c(outputText,"!jr=0",e1," "," ")
        outputText<-c(outputText,"!jr>0"," ",e2," ")
        outputText<-c(outputText," ","","","")
        outputText<-c(outputText,"!j\bOutcomes:","\bFalse","\bValid","")
        
        e1a=paste0("\b",format((sum(nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%")
        e2a=paste0(format((sum(!nullSig)+sum(resSig))/nr*100,digits=report_precision),"%")
        outputText<-c(outputText,"!jAll:",e1a,e2a,"")
        
        e1a=paste0("(",format((sum(nullSig)+sum(resSig))/nr*100,digits=report_precision),"%)")
        e2a=paste0("(",format((sum(!nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%)")
        
        
        e1n=paste0("\b",format(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
        e1=paste0(format(sum(resSig)/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
        outputText<-c(outputText,paste0("!jSig ",e1a,":"),e1n,e1," ")
        
        e2n=paste0(format(sum(!nullSig)/(sum(!nullSig)+sum(!resSig))*100,digits=report_precision),"%")
        e2=paste0("\b",format(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=report_precision),"%")
        outputText<-c(outputText,paste0("!jNot Sig ",e2a,":"),e2,e2n," ")
      } else {
        outputText<-c(outputText," ",e1,e2," ")
    }
      
  }else{
    
    ot1<-c()
    ot2<-c()
    ot4<-c()
    ot5<-c()
    ot6<-c()
    
    for (i in 1:(nc/3)) {
      r<-rs[,i]
      p<-ps[,i]

      if (expected$type=="CILimits"){
        a<-r2ci(r,result$nval[1],-1)
        b<-r2ci(r,result$nval[1],+1)
      } else {
        switch (expected$Expected_par1,
                "r"={a<-r},
                "p"={a<-p},
                "log(lrs)"={a<-res2llr(result,"sLLR")},
                "log(lrd)"={a<-res2llr(result,"dLLR")},
                "n"={a<-result$nval},
                "w"={a<-rn2w(r,result$nval)},
                "nw"={a<-rw2n(r,0.8,result$design$sReplTails)},
                "rp"={a<-result$rpIV},
                "r1"={a<-result$roIV},
                "p1"={a<-result$poIV},
                "wp"={a<-rn2w(result$rpIV,result$nval)}
        )
        switch (expected$Expected_par2,
                "r"={b<-r},
                "p"={b<-p},
                "log(lrs)"={b<-res2llr(result,"sLLR")},
                "log(lrd)"={b<-res2llr(result,"dLLR")},
                "n"={b<-result$nval},
                "w"={b<-rn2w(r,result$nval)},
                "nw"={b<-rw2n(r,0.8,result$design$sReplTails)},
                "rp"={b<-result$rpIV},
                "r1"={b<-result$roIV},
                "p1"={b<-result$poIV},
                "wp"={b<-rn2w(result$rpIV,result$nval)}
        )
      }
      ot1<-c(ot1,
             "mean",
             format(mean(a,na.rm=TRUE),digits=report_precision),
             format(mean(b,na.rm=TRUE),digits=report_precision)
      )
      ot2<-c(ot2,
             "sd",
             format(sd(a,na.rm=TRUE),digits=report_precision),
             format(sd(b,na.rm=TRUE),digits=report_precision)
      )
      ot4<-c(ot4,
             "quant75",
             format(quantile(a,0.75,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.75,na.rm=TRUE),digits=report_precision)
      )
      ot5<-c(ot5,
             "median",
             format(quantile(a,0.5,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.5,na.rm=TRUE),digits=report_precision)
      )
      ot6<-c(ot6,
             "quant25",
             format(quantile(a,0.25,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.25,na.rm=TRUE),digits=report_precision)
      )
      if (i>1){
        ot1[length(ot1)-2]<-""
        ot2[length(ot1)-2]<-""
        ot4[length(ot1)-2]<-""
        ot5[length(ot1)-2]<-""
        ot6[length(ot1)-2]<-""
      }
    }
    outputText<-c(outputText,ot1,ot2,rep("  ",nc),ot4,ot5,ot6)
    if (expected$Expected_par1=="p") {
      outputText<-c(outputText,rep("  ",nc),"p(sig)",paste0(format(mean(p<alpha)*100,digits=report_precision),"%"),rep(" ",nc-2))
    }
    if (expected$Expected_par2=="p") {
      outputText<-c(outputText,rep("  ",nc),"p(sig)"," ",paste0(format(mean(p<alpha)*100,digits=report_precision),"%"),rep(" ",nc-3))
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
