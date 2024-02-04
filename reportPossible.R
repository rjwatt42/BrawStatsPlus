reportPossible<-function(IV,DV,effect,design,possible,possibleResult){

  switch (possible$type,
          "Samples"={
            possibleResult<-possibleResult$samples
            srSimAnalysis<-describePossibleSamples(possibleResult)
            sr_effectR<-srSimAnalysis$sr_effectR
            
            srTheoryAnalysis<-densityFunctionStats(possibleResult$Theory$sourceSampDens_r_total,possibleResult$Theory$rs)
            
          },
          "Populations"={
            possibleResult<-possibleResult$populations
            prSimAnalysis<-describePossiblePopulations(possibleResult)
            pr_effectR<-prSimAnalysis$pr_effectR
            prTheoryAnalysis<-densityFunctionStats(possibleResult$Theory$priorSampDens_r,possibleResult$Theory$rp) 
            pwTheoryAnalysis<-densityFunctionStats(possibleResult$Theory$spDens_w,possibleResult$Theory$wp) 
          }
  )
  
  nc<-3
  outputText<-rep("",nc)
  outputText[1]<-paste("\bPossible:",possible$type)
  if (possible$show=="Power") outputText[1]<-paste(outputText[1],"Power(w))")
  
  if (!is.na(possible$targetSample)) {
    switch (possible$type,
          "Samples"={
            progress<-paste0("(no sims = ",format(length(sr_effectR)),")")
          },
          "Populations"={
            if (possible$show=="Power") {
              waste<-prSimAnalysis$wpSimWaste
            } else {
              waste<-prSimAnalysis$rpSimWaste
            }
            progress<-paste("(no sims=",format(length(pr_effectR)),
                                   "; no at target=",format(sum(prSimAnalysis$pSimDens_slice)),
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
                          format(srTheoryAnalysis$peak,digits=report_precision),
                          format(srSimAnalysis$rsSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(samples)",
                          format(srTheoryAnalysis$sd,digits=report_precision),
                          format(srSimAnalysis$rsSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("<", format(srTheoryAnalysis$ci[1],digits=report_precision), ",", format(srTheoryAnalysis$ci[2],digits=report_precision), ">"),
                          paste("<", format(srSimAnalysis$rsSim_ci[1],digits=report_precision), ",", format(srSimAnalysis$rsSim_ci[2],digits=report_precision), ">")
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
              outputText<-c(outputText,"max(populations)",format(prTheoryAnalysis$peak,digits=report_precision),
                            format(prSimAnalysis$rpSim_peak,digits=report_precision))
              outputText<-c(outputText,"sd(populations)",format(prTheoryAnalysis$sd,digits=report_precision),
                            format(prSimAnalysis$rpSim_sd,digits=report_precision))
              outputText<-c(outputText,"CI(samples)",
                            paste("<", format(prTheoryAnalysis$ci[1],digits=report_precision), ",", format(prTheoryAnalysis$ci[2],digits=report_precision), ">"),
                            paste("<", format(prSimAnalysis$rpSim_ci[1],digits=report_precision), ",", format(prSimAnalysis$rpSim_ci[2],digits=report_precision), ">")
              )
              } else {
                outputText<-c(outputText,"max(w)",format(pwTheoryAnalysis$peak,digits=report_precision),
                              format(prSimAnalysis$wpSim_peak,digits=report_precision))
                outputText<-c(outputText,"mean(w)",format(pwTheoryAnalysis$mean,digits=report_precision),
                              format(prSimAnalysis$wpSim_mean,digits=report_precision))
              }
              outputText<-c(outputText,rep("",nc))
            if (length(possibleResult$Sims$pSims)==0){
              outputText[seq(12,length(outputText),3)]<-" "
            }
              
            dens_at_zero<-approx(possibleResult$Theory$rp,possibleResult$Theory$priorSampDens_r,0)$y
            dens_at_sample<-approx(possibleResult$Theory$rp,possibleResult$Theory$priorSampDens_r,possibleResult$sRho[1])$y
            dens_at_population<-approx(possibleResult$Theory$rp,possibleResult$Theory$priorSampDens_r,possible$ResultHistory$rp[1])$y
            S<-log(dens_at_sample)
            S1<-log(dens_at_zero)
            S2<-log(dens_at_population)
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
