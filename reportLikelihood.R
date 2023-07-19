reportLikelihood<-function(Iv,DV,effect,design,likelihood,likelihoodResult){

  switch (likelihood$type,
          "Samples"={likelihoodResult<-likelihoodResult$samples},
          "Populations"={likelihoodResult<-likelihoodResult$populations}
  )
  
  nc<-3
  outputText<-rep("",nc)
  outputText[1]<-paste("\bPossible:",likelihood$type)
  if (!is.na(likelihood$targetSample)) {
    switch (likelihood$type,
          "Samples"={
            outputText[3]<-paste0("(no sims = ",format(length(likelihoodResult$Sims$sSims)),")")
          },
          "Populations"={
              outputText[3]<-paste("(no sims=",format(length(likelihoodResult$Sims$pSims)),"; no at target=",format(sum(likelihoodResult$Sims$pSimDens$counts)),")",sep="")
          }
  )
  }

  switch(likelihood$UseSource,
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
           text0<-paste0(likelihood$prior$populationPDF,"(",format(likelihood$prior$populationPDFk,digits=3),")","+Null(",format(likelihood$prior$populationNullp),")")
         }
  )
  outputText<-c(outputText,"Source:",paste0(likelihood$UseSource,"=",text0)," ")

  if (likelihood$type=="Populations") {
    switch(likelihood$UsePrior,
           "world"={
             if (effect$world$worldOn) {
               text0<-paste0(effect$world$populationPDF,"(",format(effect$world$populationPDFk,digits=3),")","+Null(",format(effect$world$populationNullp),")")
             } else {
               text0<-paste0("Single","(",format(effect$rIV,digits=3),")")
             }
           },
           "prior"={
             text0<-paste0(likelihood$prior$populationPDF,"(",format(likelihood$prior$populationPDFk,digits=3),")","+Null(",format(likelihood$prior$populationNullp),")")
           },
           "none"={text0<-""}
    )
    outputText<-c(outputText,"Prior:",paste0(likelihood$UsePrior,"=",text0)," ")
  }
  switch (likelihood$type,
          "Samples"={
            outputText<-c(outputText,paste("Population ","effect-size=", format(likelihood$targetPopulation,digits=report_precision),sep=""),"","")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(samples)  ",format(likelihoodResult$Theory$rs_peak,digits=report_precision),format(likelihoodResult$Sims$rsSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(samples)",format(likelihoodResult$Theory$rs_sd,digits=report_precision),format(likelihoodResult$Sims$rsSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("<", format(likelihoodResult$Theory$rs_ci[1],digits=report_precision), ",", format(likelihoodResult$Theory$rs_ci[2],digits=report_precision), ">"),
                          paste("<", format(likelihoodResult$Sims$rsSim_ci[1],digits=report_precision), ",", format(likelihoodResult$Sims$rsSim_ci[2],digits=report_precision), ">")
            )
            outputText<-c(outputText,rep(" ",nc))
            if (length(likelihoodResult$Sims$sSims)==0){
              outputText[seq(9,length(outputText),3)]<-" "
            }
            if (!is.na(likelihood$targetSample)) {
              xi<-likelihoodResult$Theory$rs
              yi<-cumsum(likelihoodResult$Theory$sDens_r_plus)
              yi<-yi/sum(likelihoodResult$Theory$sDens_r_plus)
              theory<-1-approx(xi,yi,abs(likelihood$targetSample))$y+approx(xi,yi,-abs(likelihood$targetSample))$y
              if (length(likelihoodResult$Sims$sSims)>0) {
                sims<-mean(abs(likelihoodResult$Sims$sSims)>abs(likelihood$targetSample))
              } else {sims<-0}
              outputText<-c(outputText,"Sample Probability:",
                            format(theory,digits=report_precision),
                            format(sims,digits=report_precision))
              if (length(likelihoodResult$Sims$sSims)==0) {
                outputText[seq(3,27,3)]<-" "
              }
            }
          },
          "Populations"={
            if (!is.na(likelihood$targetSample)) {
            outputText<-c(outputText,"Sample ",paste("r=", format(mean(likelihoodResult$sRho[1]),digits=report_precision)," (n=",format(likelihoodResult$n[1]),")",sep=""),"")
            if (length(likelihoodResult$sRho)>1) {
            for (ei in 2:length(likelihoodResult$sRho)) {
              outputText<-c(outputText," ",paste("r=", format(mean(likelihoodResult$sRho[ei]),digits=report_precision)," (n=",format(likelihoodResult$n[ei]),")",sep=""),"")
            }
            }
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(populations)",format(likelihoodResult$Theory$rp_peak,digits=report_precision),
                          format(likelihoodResult$Sims$rpSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(populations)",format(likelihoodResult$Theory$rp_sd,digits=report_precision),format(likelihoodResult$Sims$rpSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("[", format(likelihoodResult$Theory$rp_ci[1],digits=report_precision), ",", format(likelihoodResult$Theory$rp_ci[2],digits=report_precision), "]"),
                          paste("[", format(likelihoodResult$Sims$rpSim_ci[1],digits=report_precision), ",", format(likelihoodResult$Sims$rpSim_ci[2],digits=report_precision), "]")
            )
            outputText<-c(outputText,rep("",nc))
            if (length(likelihoodResult$Sims$pSims)==0){
              outputText[seq(9,length(outputText),3)]<-" "
            }
            S<-log(likelihoodResult$Theory$dens_at_sample)
            S1<-log(likelihoodResult$Theory$dens_at_zero)
            S2<-log(likelihoodResult$Theory$dens_at_population)
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
