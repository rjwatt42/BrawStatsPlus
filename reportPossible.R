reportPossible<-function(Iv,DV,effect,design,possible,possibleResult){

  switch (possible$type,
          "Samples"={
            possibleResult<-possibleResult$samples
            sr_effects<-possibleResult$Sims$sSims
          },
          "Populations"={
            possibleResult<-possibleResult$populations
            }
  )
  
  nc<-3
  outputText<-rep("",nc)
  outputText[1]<-paste("\bPossible:",possible$type)
  if (possible$show=="Power") outputText[1]<-paste(outputText[1],"Power(w))")
  
  if (!is.na(possible$targetSample)) {
    switch (possible$type,
          "Samples"={
            progress<-paste0("(no sims = ",format(length(sr_effects)),")")
          },
          "Populations"={
            if (possible$show=="Power") {waste<-possibleResult$Sims$wpSimWaste} 
            else {waste<-possibleResult$Sims$rpSimWaste}
              progress<-paste("(no sims=",format(length(possibleResult$Sims$pSims)),
                                   "; no at target=",format(sum(possibleResult$Sims$pSimDens)),
                                   "; out of bounds at target=",format(waste),
                                   ")",sep="")
          }
  )
    outputText<-c(outputText,progress,rep("",nc-1))
  }
  outputText<-c(outputText,rep("",nc))
  
  switch(possible$UseSource,
         "null"={
           text0<-paste0("Hypothesis","(",format(0,digits=3),")")
         },
         "hypothesis"={
           text0<-paste0("Hypothesis","(",format(effect$rIV,digits=3),")")
         },
         "world"={
           if (effect$world$worldOn) {
             text0<-paste0(effect$world$populationPDF,"(",format(effect$world$populationPDFk,digits=3),")","+Null(",format(effect$world$populationNullp),")")
           } else {
             text0<-paste0("Single","(",format(effect$rIV,digits=3),")")
           }
         },
         "prior"={
           text0<-paste0(possible$prior$populationPDF,"(",format(possible$prior$populationPDFk,digits=3),")","+Null(",format(possible$prior$populationNullp),")")
         }
  )
  outputText<-c(outputText,"Source:",paste0(possible$UseSource,"=",text0)," ")

  if (possible$type=="Populations") {
    switch(possible$UsePrior,
           "world"={
             if (effect$world$worldOn) {
               text0<-paste0(effect$world$populationPDF,"(",format(effect$world$populationPDFk,digits=3),")","+Null(",format(effect$world$populationNullp),")")
             } else {
               text0<-paste0("Single","(",format(effect$rIV,digits=3),")")
             }
           },
           "prior"={
             text0<-paste0(possible$prior$populationPDF,"(",format(possible$prior$populationPDFk,digits=3),")","+Null(",format(possible$prior$populationNullp),")")
           },
           "none"={text0<-""}
    )
    outputText<-c(outputText,"Prior:",paste0(possible$UsePrior,"=",text0)," ")
  }
  
  switch (possible$type,
          "Samples"={

            outputText<-c(outputText,paste("Population ","effect-size=", format(possible$targetPopulation,digits=report_precision),sep=""),"","")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(samples)  ",
                          format(possibleResult$Theory$rs_peak,digits=report_precision),
                          format(possibleResult$Sims$rsSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(samples)",
                          format(possibleResult$Theory$rs_sd,digits=report_precision),
                          format(possibleResult$Sims$rsSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("<", format(possibleResult$Theory$rs_ci[1],digits=report_precision), ",", format(possibleResult$Theory$rs_ci[2],digits=report_precision), ">"),
                          paste("<", format(possibleResult$Sims$rsSim_ci[1],digits=report_precision), ",", format(possibleResult$Sims$rsSim_ci[2],digits=report_precision), ">")
            )
            outputText<-c(outputText,rep(" ",nc))
            if (length(possibleResult$Sims$sSims)==0){
              outputText[seq(9,length(outputText),3)]<-" "
            }
            if (!is.na(possible$targetSample)) {
              xi<-possibleResult$Theory$rs
              yi<-cumsum(possibleResult$Theory$sourceSampDens_r_plus)
              yi<-yi/sum(possibleResult$Theory$sourceSampDens_r_plus)
              theory<-1-approx(xi,yi,abs(possible$targetSample))$y+approx(xi,yi,-abs(possible$targetSample))$y
              if (length(possibleResult$Sims$sSims)>0) {
                sims<-mean(abs(possibleResult$Sims$sSims)>abs(possible$targetSample))
              } else {sims<-0}
              outputText<-c(outputText,"Sample Probability:",
                            format(theory,digits=report_precision),
                            format(sims,digits=report_precision))
              if (length(possibleResult$Sims$sSims)==0) {
                outputText[seq(3,27,3)]<-" "
              }
            }
          },
          "Populations"={
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
              } else {
                use_effectRP_slice<-pr_effectRP_slice
                use_effectR<-pr_effectR
                use_effectRP<-pr_effectRP
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
            }

            if (!is.na(possible$targetSample)) {
              outputText<-c(outputText,"Sample ",paste("r=", format(mean(possibleResult$sRho[1]),digits=report_precision)," (n=",format(possibleResult$n[1]),")",sep=""),"")
              if (length(possibleResult$sRho)>1) {
                for (ei in 2:length(possibleResult$sRho)) {
                  outputText<-c(outputText," ",paste("r=", format(mean(possibleResult$sRho[ei]),digits=report_precision)," (n=",format(possibleResult$n[ei]),")",sep=""),"")
                }
              }
              outputText<-c(outputText,rep("",nc))
              
              outputText<-c(outputText," ","Theory","Simulation")
              if (possible$show!="Power") {
              outputText<-c(outputText,"max(populations)",format(possibleResult$Theory$rp_peak,digits=report_precision),
                            format(possibleResult$Sims$rpSim_peak,digits=report_precision))
              outputText<-c(outputText,"sd(populations)",format(possibleResult$Theory$rp_sd,digits=report_precision),format(possibleResult$Sims$rpSim_sd,digits=report_precision))
              outputText<-c(outputText,"CI(samples)",
                            paste("<", format(possibleResult$Theory$rp_ci[1],digits=report_precision), ",", format(possibleResult$Theory$rp_ci[2],digits=report_precision), ">"),
                            paste("<", format(possibleResult$Sims$rpSim_ci[1],digits=report_precision), ",", format(possibleResult$Sims$rpSim_ci[2],digits=report_precision), ">")
              )
              } else {
                outputText<-c(outputText,"max(w)",format(possibleResult$Theory$wp_peak,digits=report_precision),
                              format(possibleResult$Sims$wpSim_peak,digits=report_precision))
                outputText<-c(outputText,"mean(w)",format(possibleResult$Theory$wp_mean,digits=report_precision),
                              format(possibleResult$Sims$wpSim_mean,digits=report_precision))
              }
              outputText<-c(outputText,rep("",nc))
            if (length(possibleResult$Sims$pSims)==0){
              outputText[seq(12,length(outputText),3)]<-" "
            }
            S<-log(possibleResult$Theory$dens_at_sample)
            S1<-log(possibleResult$Theory$dens_at_zero)
            S2<-log(possibleResult$Theory$dens_at_population)
            if (identical(a,numeric(0))) {S2<-NA}
            
            text1="loglikelihood(rp=rs,0";
            text2<-paste0("S = ",format(S,digits=report_precision),", ",format(S1,digits=report_precision))
            if (!isempty(S2) && !is.na(S2)) {
              text1<-paste0(text1,",rp")
              text2<-paste0(text2,", ",format(S2,digits=report_precision))
            }
            text1<-paste0(text1,"): ")
            outputText<-c(outputText,text1,text2," ")
            }
          }
          )

  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)        

}
