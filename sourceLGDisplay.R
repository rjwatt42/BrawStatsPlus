######################################################
## large display output functions

designFields<-list(select=c("sMethod","sCheating"),
                   number=c("sN","sNRandK","sCheatingAmount"),
                   check=c("sNRand"))
hypothesisFields<-list(select=c("world_distr","world_distr_rz"),
                       number=c("rIV","rIV2","rIVIV2","rIVIV2DV","world_distr_k","world_distr_Nullp"),
                       check=c("world_on"))
worldFields<-list(select=c("world_distr","world_distr_rz"),
                  number=c("world_distr_k","world_distr_Nullp"),
                  check=c("world_on"))
evidenceFields<-list(select=c("EvidenceInfer_type","EvidenceExpected_type","EvidenceExpected_par1","EvidenceExpected_par2","EvidenceEffect_type","EvidenceExpected_length"),
                     number=c(),
                     check=c("EvidenceExpected_append"))
metaFields<-list(select=c("meta_fixedAnal","meta_runlength","meta_pdf"),
                 number=c("meta_nStudies"),
                 check=c("meta_psigStudies","meta_nullAnal","meta_psigAnal","meta_append"))
exploreFields<-list(select=c("Explore_typeH","Explore_VtypeH","Explore_showH","Explore_whichShowH","Explore_typeShowH","Explore_lengthH",
                             "Explore_typeD","Explore_showD","Explore_whichShowD","Explore_typeShowD","Explore_lengthD",
                             "Explore_typeM","Explore_showM","Explore_lengthM"),
                    number=c("Explore_nRange","Explore_npoints","Explore_esRange","Explore_quants","ExploreFull_ylim","Explore_anomRange","Explore_metaRange"),
                    check=c("ExploreAppendH","ExploreAppendD","ExploreAppendM","Explore_xlog")
)
possibleFields<-list(select=c("likelihoodP_length","likelihood_length","likelihoodPrior_distr","likelihoodPrior_distr_rz","likelihoodUsePrior","likelihoodUseSource"),
                     number=c("likelihoodSampRho","likelihoodSimSlice","likelihoodPSampRho","likelihoodPrior_distr_k","likelihoodPrior_distr_Nullp",
                              "LikelihoodAzimuth","LikelihoodElevation","likelihoodRange"),
                     check=c("likelihood_cutaway","likelihood_sigonly","likelihoodTheory","likelihoodCorrection","likelihoodP_append","likelihood_append")
)

updateHypothesisFields<-function(prefix) {
  if (input$IV2choice=="none") {
    shinyjs::hideElement(paste0(prefix,"IV2-DV"))
    shinyjs::hideElement(paste0(prefix,"rIV2"))
    shinyjs::hideElement(paste0(prefix,"IV1-IV2"))
    shinyjs::hideElement(paste0(prefix,"rIVIV2"))
    shinyjs::hideElement(paste0(prefix,"IV1-IV2-DV"))
    shinyjs::hideElement(paste0(prefix,"rIVIV2DV"))
  } else {
    shinyjs::showElement(paste0(prefix,"IV2-DV"))
    shinyjs::showElement(paste0(prefix,"rIV2"))
    shinyjs::showElement(paste0(prefix,"IV1-IV2"))
    shinyjs::showElement(paste0(prefix,"rIVIV2"))
    shinyjs::showElement(paste0(prefix,"IV1-IV2-DV"))
    shinyjs::showElement(paste0(prefix,"rIVIV2DV"))
  }
}
updateDesignFields<-function(prefix) {
  if (!input$sNRand) {
    shinyjs::hideElement(paste0(prefix,"sNRandK"))
    shinyjs::hideElement(paste0(prefix,"gamma"))
  } else {
    shinyjs::showElement(paste0(prefix,"snRandK"))
    shinyjs::showElement(paste0(prefix,"gamma"))
  }
}
observeEvent(c(input$LGEvidencesNRand,input$LGMetasNRand,input$LGExploreNRand),{
  updateDesignFields("LGEvidence")
  updateDesignFields("LGMeta")
  updateDesignFields("LGExplore")
})
updateEvidenceFields<-function(prefix) {
  if (input$IV2choice=="none") {
    shinyjs::hideElement(paste0(prefix,"Effect_type"))
  } else {
    shinyjs::showElement(paste0(prefix,"Effect_type"))
  }
}
updateMetaFields<-function(prefix) {
  if (input$IV2choice=="none") {
    shinyjs::hideElement(paste0(prefix,"Effect_type"))
  } else {
    shinyjs::showElement(paste0(prefix,"Effect_type"))
  }
}
updateExploreFields<-function(prefix) {
  if (input$IV2choice=="none") {
    shinyjs::hideElement(paste0(prefix,"_whichShowH"))
    shinyjs::hideElement(paste0(prefix,"_typeShowH"))
    shinyjs::hideElement(paste0(prefix,"_whichShowD"))
    shinyjs::hideElement(paste0(prefix,"_typeShowD"))
  } else {
    shinyjs::showElement(paste0(prefix,"_whichShowH"))
    shinyjs::showElement(paste0(prefix,"_typeShowH"))
    shinyjs::showElement(paste0(prefix,"_whichShowD"))
    shinyjs::showElement(paste0(prefix,"_typeShowD"))
  }
  if (input$LGExplore_typeD!="SampleSize") {
    shinyjs::hideElement(paste0(prefix,"_max"))
    shinyjs::hideElement(paste0(prefix,"_nRange"))
    shinyjs::hideElement(paste0(prefix,"_log"))
    shinyjs::hideElement(paste0(prefix,"_xlog"))
  } else {
    shinyjs::showElement(paste0(prefix,"_max"))
    shinyjs::showElement(paste0(prefix,"_nRange"))
    shinyjs::showElement(paste0(prefix,"_log"))
    shinyjs::showElement(paste0(prefix,"_xlog"))
  }
  if (!is.element(input$LGExplore_typeH,c("IV","IV2","DV"))) {
    shinyjs::hideElement(paste0(prefix,"_VtypeH"))
  } else {
    shinyjs::showElement(paste0(prefix,"_VtypeH"))
  }
}
observeEvent(c(input$LGExplore_typeD,input$LGExplore_typeH),{
  updateExploreFields("LGExplore")
})

saveFields <-function(fields,suffix="") {
  if (length(fields$select)>0) {
    for (i in 1:length(fields$select)) {
      updateSelectInput(session,fields$select[i],selected=input[[paste0("LG",suffix,fields$select[i])]])
    }
  }
  if (length(fields$number)>0) {
    for (i in 1:length(fields$number)) {
      updateNumericInput(session,fields$number[i],value=input[[paste0("LG",suffix,fields$number[i])]])
    }
  }
  if (length(fields$check)>0) {
    for (i in 1:length(fields$check)) {
      updateCheckboxInput(session,fields$check[i],value=input[[paste0("LG",suffix,fields$check[i])]])
    }
  }
} 

loadFields <-function(fields,suffix="") {
  if (length(fields$select)>0) {
    for (i in 1:length(fields$select)) {
      updateSelectInput(session,paste0("LG",suffix,fields$select[i]),selected=input[[fields$select[i]]])
    }
  }
  if (length(fields$number)>0) {
    for (i in 1:length(fields$number)) {
      updateNumericInput(session,paste0("LG",suffix,fields$number[i]),value=input[[fields$number[i]]])
    }
  }
  if (length(fields$check)>0) {
    for (i in 1:length(fields$check)) {
      updateCheckboxInput(session,paste0("LG",suffix,fields$check[i]),value=input[[fields$check[i]]])
    }
  }
} 

saveLGEvidence <- function (){
  saveFields(designFields,"Evidence")
  saveFields(hypothesisFields,"Evidence")
  saveFields(evidenceFields)
}

observeEvent(c(input$LGEvidenceStart,input$LGEvidenceStart1,input$LGEvidenceStart2,input$LGEvidenceStart3),{
  req(input$changed)
  
  loadFields(designFields,"Evidence")
  loadFields(hypothesisFields,"Evidence")
  loadFields(worldFields,"Evidence")
  loadFields(evidenceFields)
  
  toggleModal(session, "LGmodalEvidence", toggle = "open")
  updateTabsetPanel(session,"LGEvidenceShow",selected=input$Graphs)
  
  updateHypothesisFields("LGEvidence")
  updateDesignFields("LGEvidence")
  updateEvidenceFields("LGEvidence")
  
  validExpected<<-FALSE
}
)
observeEvent(input$LGEvidenceClose,{
  saveLGEvidence()
  toggleModal(session, "LGmodalEvidence", toggle = "close")
}
)
output$LGshowSampleOutput<-renderPlot( {
  doIt<-c(input$LGEvidencenewSample)
  saveLGEvidence()
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeSampleGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
}
)
output$LGshowDescribeOutput<-renderPlot( {
  doIt<-c(input$LGEvidencenewSample)
  saveLGEvidence()
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeDescriptiveGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
}
)
output$LGshowInferOutput<-renderPlot( {
  doIt<-c(input$LGEvidencenewSample)
  saveLGEvidence()
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeInferentialGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
}
)
output$LGshowExpectOutput<-renderPlot( {
  doIt<-c(input$LGEvidencenewSample)
  saveLGEvidence()
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeExpectedGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
}
)


saveLGMeta <- function() {
  saveFields(designFields,"Meta")
  saveFields(hypothesisFields,"Meta")
  saveFields(metaFields)
}

observeEvent(input$LGMetaStart,{
  req(input$changed)
  
  toggleModal(session, "LGmodalMeta", toggle = "open")
  
  loadFields(designFields,"Meta")
  loadFields(hypothesisFields,"Meta")
  loadFields(metaFields)
  
  updateHypothesisFields("LGMeta")
  updateDesignFields("LGMeta")
}
)
observeEvent(input$LGMetaClose,{
  saveLGMeta()
  toggleModal(session, "LGmodalMeta", toggle = "close")
}
)
output$LGMetaShowOutput<-renderPlot( {
  doIt<-c(input$LGmetaRun)
  saveLGMeta()
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeMetaGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
})


saveLGExplore <- function() {
  saveFields(designFields,"Explore")
  saveFields(hypothesisFields,"Explore")
  saveFields(metaFields,"Explore")
  saveFields(exploreFields)
  updateTabsetPanel(session,"ExploreTab",input$LGExploreShow)
}

observeEvent(input$LGExploreStart,{
  req(input$changed)
  
  toggleModal(session, "LGmodalExplore", toggle = "open")
  
  loadFields(designFields,"Explore")
  loadFields(hypothesisFields,"Explore")
  loadFields(metaFields,"Explore")
  loadFields(exploreFields)
  if (is.element(input$ExploreTab,c("Hypothesis","Design"))) {
    updateSelectInput(session,"LGExploreShow",selected = input$ExploreTab)
  }
  
  updateHypothesisFields("LGExplore")
  updateDesignFields("LGExplore")
  updateMetaFields("LGExplore")
  updateExploreFields("LGExplore")
  
}
)
observeEvent(input$LGExploreClose,{
  saveLGExplore()
  toggleModal(session, "LGmodalExplore", toggle = "close")
}
)
output$LGExploreShowOutput<-renderPlot( {
  doIt<-c(input$LGexploreRunH,input$LGexploreRunD,input$LGexploreRunM)
  saveLGExplore()
  
  plotTheme<<-mainTheme+LGplotTheme
  g<-makeExploreGraph()
  plotTheme<<-mainTheme+SMplotTheme
  g
})


saveLGPossible <- function () {
  saveFields(hypothesisFields,"likelihood")
  saveFields(designFields,"likelihood")
  saveFields(worldFields,"likelihood")
  saveFields(possibleFields)
}

observeEvent(input$LGPossibleStart,{
  req(input$changed)
  graphicSource<<-"None"
  toggleModal(session, "LGmodalPossible", toggle = "open")
  loadFields(hypothesisFields,"likelihood")
  loadFields(designFields,"likelihood")
  loadFields(worldFields,"likelihood")
  loadFields(possibleFields)
  if (is.element(input$Likelihood,c("Samples","Populations"))) {
    updateSelectInput(session,"LGshowPossible",selected = input$Likelihood)
  }
  if (input$IV2choice=="none") {
    shinyjs::hideElement(id="LGlikelihoodrIV2")
    shinyjs::hideElement(id="LGlikelihoodrIVIV2DV")
    shinyjs::hideElement(id="LGlikelihoodrIVIV2")
  }
  graphicSource<<-"Modal"
}
)
observeEvent(input$LGlikelihoodClose, {
  saveLGPossible()
  toggleModal(session, "LGmodalPossible", toggle = "close")
})

output$LGshowPossibleOutput<-renderPlot( {
  saveLGPossible()
  updateTabsetPanel(session,"Likelihood",input$LGshowPossible)
  
  par(cex=1.8)
  makeLikelihoodGraph()
})
