################################################################        
# update basic functions
# set prediction, design, evidence variables from UI
#


# PREDICTION & DESIGN & EVIDENCE
updateEffect<-function(type=0){
  if (debug) debugPrint("     updateEffect")
  
  if (switches$doWorlds) {
    world<-list(worldOn=input$world_on,populationPDF=input$world_distr,
                populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                populationNullp=input$world_distr_Nullp,
                worldAbs=input$world_abs)
  } else {
    world<-list(worldOn=FALSE,populationPDF="Single",populationPDFk=NA,populationRZ=NA,populationNullp=NA,worldAbs=FALSE)
  }
  if (pPlus) world$populationNullp<-1-world$populationNullp
  
  if (is.null(world$worldOn)) {world$worldOn<-FALSE}
  
  if (is.null(type)) {
    effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,
                 Heteroscedasticity=input$Heteroscedasticity,Welch=input$Welch,ResidDistr=input$ResidDistr,
                 world=world
    )
  } else {
    effect<-list(rIV=input$rIV,rIV2=input$rIV2,rIVIV2=input$rIVIV2,rIVIV2DV=input$rIVIV2DV,
                 Heteroscedasticity=input$Heteroscedasticity,Welch=input$Welch,ResidDistr=input$ResidDistr,
                 world=world
    )
  }
  if (effect$Heteroscedasticity!=0 && shortHand) {
    hmm("Please switch to longhand calculations: heteroscedasticity")
  }
  if (effect$ResidDistr!="normal" && shortHand) {
    hmm("Please switch to longhand calculations: heteroscedasticity")
  }
  
  if (effect$world$worldOn==FALSE) {
    effect$world$populationPDF<-"Single"
    effect$world$populationRZ<-"r"
    effect$world$populationPDFk<-effect$rIV
    effect$world$populationNullp<-0
    effect$world$worldAbs<-FALSE
  }
  if (is.null(oldEffect)) {
    effect$Heteroscedasticity<-checkNumber(effect$Heteroscedasticity)
    effect$world$populationPDFk<-checkNumber(effect$world$populationPDFk)
    effect$world$populationNullp<-checkNumber(effect$world$populationNullp)
  } else {
    effect$Heteroscedasticity<-checkNumber(effect$Heteroscedasticity,oldEffect$Heteroscedasticity)
    effect$world$populationPDFk<-checkNumber(effect$world$populationPDFk,oldEffect$world$populationPDFk)
    effect$world$populationNullp<-checkNumber(effect$world$populationNullp,oldEffect$world$populationNullp)
  }
  oldEffect<<-effect
  
  if (debug) debugPrint("     updateEffect - exit")
  effect
}

updateDesign<-function(){
  if (debug) debugPrint("     updateDesign")
  design<-list(sN=input$sN, sNRand=input$sNRand,sNRandK=input$sNRandK,
               sBudgetOn=input$sBudgetOn,sNBudget=input$sNBudget,
               sMethod=input$sMethod ,sIV1Use=input$sIV1Use,sIV2Use=input$sIV2Use, 
               sRangeOn=input$sRangeOn, sIVRange=input$sIVRange, sDVRange=input$sDVRange, 
               sDependence=input$sDependence, sOutliers=input$sOutliers, sClustering=input$sClustering,
               sCheating=input$sCheating,sCheatingLimit=input$sCheatingLimit,sCheatingAmount=input$sCheatingAmount,sCheatingBudget=input$sCheatingBudget,
               sReplicationOn=input$sReplicationOn,
               sReplPowerOn=input$sReplPowerOn,sReplPower=input$sReplPower,
               sReplSigOnly=input$sReplSigOnly,
               sReplType=input$sReplType,sReplRepeats=input$sReplRepeats,sReplBudget=1000,
               sReplCorrection=input$sReplCorrection,sReplTails=input$sReplTails,
               sReplKeep=input$sReplKeep,
               sReplVarAlpha=input$sReplVarAlpha,sReplAlpha=input$sReplAlpha,
               sN_Strata=input$sN_Strata, sR_Strata=input$sR_Strata,
               sNClu_Cluster=input$sNClu_Cluster, sRClu_Cluster=input$sRClu_Cluster,
               sNClu_Convenience=input$sNClu_Convenience, sRClu_Convenience=input$sRClu_Convenience, sNCont_Convenience=input$sNCont_Convenience, sRCont_Convenience=input$sRCont_Convenience, sRSpread_Convenience=input$sRSpread_Convenience,
               sNClu_Snowball=input$sNClu_Snowball, sRClu_Snowball=input$sRClu_Snowball, sNCont_Snowball=input$sNCont_Snowball, sRCont_Snowball=input$sRCont_Snowball, sRSpread_Snowball=input$sRSpread_Snowball
  )
  
  if (is.element(design$sCheating,c("Grow","Replace")) && shortHand) {
    hmm("Please switch to longhand calculations: cheating")
  }
  
  if (design$sMethod!="Random" && shortHand) {
    hmm("Please switch to longhand calculations: sampling")
  }
  
  if (any(c(design$sDependence,design$sOutliers)!=0) && shortHand) {
    hmm("Please switch to longhand calculations: anomalies")
  }
  
  design$sN<-checkNumber(design$sN,c=10)
  if (is.null(oldDesign)) {
    design$sNRandK<-checkNumber(design$sNRandK)
    design$sReplPower<-checkNumber(design$sReplPower)
  } else {
    design$sNRandK<-checkNumber(design$sNRandK,oldDesign$sNRandK)
    design$sReplPower<-checkNumber(design$sReplPower,oldDesign$sReplPower)
  }
  oldDesign<<-design
  if (variablesHeld=="Data" && !applyingAnalysis && switches$doBootstrap) {design$sMethod<-"Resample"}
  if (debug) debugPrint("     updateDesign - exit")
  design
}

updateEvidence<-function(){
  if (debug) debugPrint("     updateEvidence")
  evidence<-list(rInteractionOn=input$rInteractionOn,
                 rInteractionOnly=input$rInteractionOnly,
                 showType=input$EvidenceEffect_type,
                 showTheory=input$evidenceTheory,
                 sigOnly=input$evidenceSigOnly,
                 allScatter=input$allScatter,
                 ssqType=input$ssqType,
                 llr=list(e1=input$llr1,e2=input$llr2),
                 evidenceCaseOrder=input$evidenceCaseOrder,Welch=input$Welch,
                 dataType=input$dataType,analysisType=input$analysisType,
                 pScale=input$pScale,wScale=input$wScale,nScale=input$nScale,
                 usePrior=input$STPrior,
                 prior=list(worldOn=FALSE,populationPDF="",
                            populationPDFk=0,populationRZ="r",
                            populationNullp=0)
  )

  if (input$Evidence=="Single") {
    evidence$showType<-input$EvidenceEffect_type1
  }
    
  switch(input$STPrior,
         "none"={
           evidence$prior=list(worldOn=FALSE,populationPDF="Uniform",
                               populationPDFk=0,populationRZ="z",
                               populationNullp=0.5)
         },
         "world"={
           evidence$prior=list(worldOn=input$world_on,populationPDF=input$world_distr,
                               populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                               populationNullp=input$world_distr_Nullp)
           if (pPlus) evidence$prior$populationNullp<-1-evidence$prior$populationNullp
         },
         "prior"={
           evidence$prior=list(worldOn=TRUE,populationPDF=input$possiblePrior_distr,
                               populationPDFk=input$possiblePrior_distr_k,populationRZ=input$possiblePrior_distr_rz,
                               populationNullp=input$possiblePrior_Nullp)
           if (pPlus) evidence$prior$populationNullp<-1-evidence$prior$populationNullp
         }
  )
  if (!switches$doWorlds) {
    evidence$prior$worldOn<-FALSE
  }
  
  if (debug) debugPrint("     updateEvidence - exit")
  evidence
}

##################################################################################  
