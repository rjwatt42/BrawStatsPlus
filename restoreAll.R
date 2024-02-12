# make this file from findControls.R

restoreAll<-function(session,newInput) {

numericInputs<- unique(c(
  "sCheatingBudget" ,"sCheatingAmount" ,"sN" ,"sNRandK" ,"sDependence" ,"sOutliers" ,"sWithinCor" ,"sN_Strata" ,"sR_Strata" ,"sNClu_Cluster" ,"sRClu_Cluster" ,"sNClu_Convenience" ,"sRClu_Convenience" ,"sNCont_Convenience" ,"sRCont_Convenience" ,"sRSpread_Convenience" ,"sNClu_Snowball" ,"sRClu_Snowball" ,"sNCont_Snowball" ,"sRCont_Snowball" ,"sRSpread_Snowball" ,"sNBudget" ,"sReplAlpha" ,"rIV" ,"rIV2" ,"rIVIV2" ,"rIVIV2DV" ,"alpha" ,"llr2" ,"llr1" ,"Explore_esRange" ,"Explore_nRange" ,"Explore_npoints" ,"Explore_esRange" ,"Explore_anomRange" ,"Explore_quants" ,"Explore_metaRange" ,"MaxOrdinal" ,"shortHandGain" ,"Heteroscedasticity" ,"meta_nStudies" ,"possibleSampRho" ,"possiblePSampRho" ,"possibleSimSlice" ,"possibleScale" ,"possibleAzimuth" ,"possibleElevation" ,"possibleRange" ,"Prior_distr_k" ,"Prior_Nullp" ,"sReplRepeats" ,"sReplBudget" ,"sReplPower" ,"sReplBudget" ,"MVmu" ,"MVsd" ,"MVskew" ,"MVkurt" ,"MVnlevs" ,"MVcentre" ,"MVspread" ,"MVncats" ,"world_distr_k" ,"world_distr_Nullp"
))

checkboxInputs<-unique(c(
  "sNRand" ,"sRangeOn" ,"sBudgetOn" ,"sReplVarAlpha" ,"EvidenceExpected_append" ,"Welch" ,"rInteractionOn" ,"evidenceInteractionOnly" ,"evidenceLogDensity" ,"evidenceSigOnly" ,"evidenceTheory" ,"evidenceHQ" ,"Explore_ylogH" ,"ExploreAppendH" ,"Explore_xlogD" ,"Explore_ylogD" ,"ExploreAppendD" ,"ExploreAny_ylim" ,"exploreTheory" ,"Explore_Mxlog" ,"ExploreAppendM" ,"ImportOrdinals" ,"ExportHeader" ,"ExportShortForm" ,"LoadExtras" ,"LargeGraphs" ,"shortHand" ,"WhiteGraphs" ,"AllowResampling" ,"showMean" ,"showSD" ,"showResiduals" ,"meta_psigStudies" ,"meta_nullAnal" ,"meta_psigAnal" ,"meta_append" ,"possible_cutaway" ,"possible_sigonly" ,"possible_append" ,"possibleP_append" ,"possibleCorrection" ,"possibleHQ" ,"possibleBoxed" ,"possibleTheory" ,"sReplicationOn" ,"sReplPowerOn" ,"sReplUseBudget" ,"world_on" ,"world_abs"
))

selectInputs<-unique(c(
  "batchFile_length" ,"batchFile_nVars" ,"sCheating" ,"sCheatingLimit" ,"sMethod" ,"sIV1Use" ,"sIV2Use" ,"EvidenceInfer_type" ,"EvidenceEffect_type1" ,"EvidenceExpected_type" ,"EvidenceExpected_par1" ,"EvidenceExpected_par2" ,"EvidenceEffect_type" ,"EvidenceExpected_length" ,"Transform" ,"STMethod" ,"STPrior" ,"ssqType" ,"dataType" ,"analysisType" ,"evidenceCaseOrder" ,"allScatter" ,"pScale" ,"wScale" ,"nScale" ,"Explore_typeH" ,"Explore_VtypeH" ,"Explore_showH" ,"Explore_whichShowH" ,"Explore_typeShowH" ,"Explore_lengthH" ,"Explore_typeD" ,"Explore_showD" ,"Explore_whichShowD" ,"Explore_typeShowD" ,"Explore_lengthD" ,"Explore_graphStyle" ,"Explore_typeM" ,"Explore_showM" ,"Explore_lengthM" ,"wsInputSheet" ,"dataInputSheet" ,"RZ" ,"Notation1" ,"Notation2" ,"Notation3" ,"Using" ,"IVchoice" ,"IV2choice" ,"DVchoice" ,"ResidDistr" ,"inspectOrder" ,"whichResiduals" ,"meta_pdf" ,"meta_showAnal" ,"meta_showParams" ,"meta_runlength" ,"metaInputSheet" ,"possibleUseSource" ,"possible_length" ,"possibleUsePrior" ,"possibleP_length" ,"possibleView" ,"possibleShow" ,"Prior_distr" ,"Prior_distr_rz" ,"Hypchoice" ,"Effectchoice" ,"sReplSigOnly" ,"sReplType" ,"sReplTails" ,"sReplCorrection" ,"sReplKeep" ,"MVtype" ,"MVdiscrete" ,"MVsource" ,"world_distr" ,"world_distr_rz"
))

for (ni in numericInputs) {
  use<-which(newInput$id==ni)
  a<-newInput[[use,2]]
  if (is.na(a)) a<-NULL
  updateNumericInput(session,ni,value=a)
}

for (ni in checkboxInputs) {
  use<-which(newInput$id==ni)
  a<-newInput[[use,2]]
  if (is.na(a)) a<-NULL
  updateCheckboxInput(session,ni,value=a==1)
}

for (ni in selectInputs) {
  use<-which(newInput$id==ni)
  a<-newInput[[use,2]]
  if (is.na(a)) a<-NULL
  updateSelectInput(session,ni,selected=a)
}


}

storeAll<-function(input) {
  
  numericInputs<- unique(c(
    "sCheatingBudget" ,"sCheatingAmount" ,"sN" ,"sNRandK" ,"sDependence" ,"sOutliers" ,"sWithinCor" ,"sN_Strata" ,"sR_Strata" ,"sNClu_Cluster" ,"sRClu_Cluster" ,"sNClu_Convenience" ,"sRClu_Convenience" ,"sNCont_Convenience" ,"sRCont_Convenience" ,"sRSpread_Convenience" ,"sNClu_Snowball" ,"sRClu_Snowball" ,"sNCont_Snowball" ,"sRCont_Snowball" ,"sRSpread_Snowball" ,"sNBudget" ,"sReplAlpha" ,"rIV" ,"rIV2" ,"rIVIV2" ,"rIVIV2DV" ,"alpha" ,"llr2" ,"llr1" ,"Explore_esRange" ,"Explore_nRange" ,"Explore_npoints" ,"Explore_esRange" ,"Explore_anomRange" ,"Explore_quants" ,"Explore_metaRange" ,"MaxOrdinal" ,"shortHandGain" ,"Heteroscedasticity" ,"meta_nStudies" ,"possibleSampRho" ,"possiblePSampRho" ,"possibleSimSlice" ,"possibleScale" ,"possibleAzimuth" ,"possibleElevation" ,"possibleRange" ,"Prior_distr_k" ,"Prior_Nullp" ,"sReplRepeats" ,"sReplBudget" ,"sReplPower" ,"sReplBudget" ,"MVmu" ,"MVsd" ,"MVskew" ,"MVkurt" ,"MVnlevs" ,"MVcentre" ,"MVspread" ,"MVncats" ,"world_distr_k" ,"world_distr_Nullp"
  ))
  
  checkboxInputs<-unique(c(
    "sNRand" ,"sRangeOn" ,"sBudgetOn" ,"sReplVarAlpha" ,"EvidenceExpected_append" ,"Welch" ,"rInteractionOn" ,"evidenceInteractionOnly" ,"evidenceLogDensity" ,"evidenceSigOnly" ,"evidenceTheory" ,"evidenceHQ" ,"Explore_ylogH" ,"ExploreAppendH" ,"Explore_xlogD" ,"Explore_ylogD" ,"ExploreAppendD" ,"ExploreAny_ylim" ,"exploreTheory" ,"Explore_Mxlog" ,"ExploreAppendM" ,"ImportOrdinals" ,"ExportHeader" ,"ExportShortForm" ,"LoadExtras" ,"LargeGraphs" ,"shortHand" ,"WhiteGraphs" ,"AllowResampling" ,"showMean" ,"showSD" ,"showResiduals" ,"meta_psigStudies" ,"meta_nullAnal" ,"meta_psigAnal" ,"meta_append" ,"possible_cutaway" ,"possible_sigonly" ,"possible_append" ,"possibleP_append" ,"possibleCorrection" ,"possibleHQ" ,"possibleBoxed" ,"possibleTheory" ,"sReplicationOn" ,"sReplPowerOn" ,"sReplUseBudget" ,"world_on" ,"world_abs"
  ))
  
  selectInputs<-unique(c(
    "batchFile_length" ,"batchFile_nVars" ,"sCheating" ,"sCheatingLimit" ,"sMethod" ,"sIV1Use" ,"sIV2Use" ,"EvidenceInfer_type" ,"EvidenceEffect_type1" ,"EvidenceExpected_type" ,"EvidenceExpected_par1" ,"EvidenceExpected_par2" ,"EvidenceEffect_type" ,"EvidenceExpected_length" ,"Transform" ,"STMethod" ,"STPrior" ,"ssqType" ,"dataType" ,"analysisType" ,"evidenceCaseOrder" ,"allScatter" ,"pScale" ,"wScale" ,"nScale" ,"Explore_typeH" ,"Explore_VtypeH" ,"Explore_showH" ,"Explore_whichShowH" ,"Explore_typeShowH" ,"Explore_lengthH" ,"Explore_typeD" ,"Explore_showD" ,"Explore_whichShowD" ,"Explore_typeShowD" ,"Explore_lengthD" ,"Explore_graphStyle" ,"Explore_typeM" ,"Explore_showM" ,"Explore_lengthM" ,"wsInputSheet" ,"dataInputSheet" ,"RZ" ,"Notation1" ,"Notation2" ,"Notation3" ,"Using" ,"IVchoice" ,"IV2choice" ,"DVchoice" ,"ResidDistr" ,"inspectOrder" ,"whichResiduals" ,"meta_pdf" ,"meta_showAnal" ,"meta_showParams" ,"meta_runlength" ,"metaInputSheet" ,"possibleUseSource" ,"possible_length" ,"possibleUsePrior" ,"possibleP_length" ,"possibleView" ,"possibleShow" ,"Prior_distr" ,"Prior_distr_rz" ,"Hypchoice" ,"Effectchoice" ,"sReplSigOnly" ,"sReplType" ,"sReplTails" ,"sReplCorrection" ,"sReplKeep" ,"MVtype" ,"MVdiscrete" ,"MVsource" ,"world_distr" ,"world_distr_rz"
  ))
  
  state<-c()
  
  for (ni in numericInputs) {
    a<-input[[ni]]
    if (is.null(a)) a<-NA
    state<-c(state,(a))
  }
  
  for (ni in checkboxInputs) {
    a<-input[[ni]]
    if (is.null(a)) a<-NA
    state<-c(state,(a))
  }
  
  for (ni in selectInputs) {
    a<-input[[ni]]
    if (is.null(a)) a<-NA
    state<-c(state,(a))
  }

  s1<-c(numericInputs,checkboxInputs,selectInputs)
  s2<-matrix((state),ncol=1)
  state<-data.frame(id=s1,val=s2)
  # names(state)<-s1
  return(state)
}
