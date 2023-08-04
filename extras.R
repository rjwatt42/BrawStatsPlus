

loadExtras<-function(session,addingExtras=TRUE){
  
  if (!addingExtras) {
    switches$doReplications<<-FALSE
    removeTab("Design","Replicate",session)
    
    switches$doWorlds<<-FALSE
    removeTab("Hypothesis","World",session)
    removeTab("HypothesisDiagram","World",session)
    updateSelectInput(session,"possibleUseSource",choices=c("null","prior"))
    updateSelectInput(session,"possibleUsePrior",choices=c("none","prior"))
    
    switches$doCheating<<-FALSE
    switches$doLikelihoodInfer<<-FALSE
    
    updateSelectInput(session,"Explore_typeH",choices=hypothesisChoicesV2)
    updateSelectInput(session,"Explore_typeD",choices=designChoices)

    updateSelectInput(session,"Explore_showH",choices=showChoices)
    updateSelectInput(session,"Explore_showD",choices=showChoices)

    updateSelectInput(session,"EvidenceInfer_type",choices=singleTypeChoices)
    updateSelectInput(session,"EvidenceExpected_par1",choices=inferTypeChoices,selected="r")
    updateSelectInput(session,"EvidenceExpected_par2",choices=inferTypeChoices,selected="p")
  } else {
    
    # replications
    if (!switches$doReplications) {
      switches$doReplications<<-TRUE
      insertTab("Design",replicationTab(),"Anomalies","after",select=FALSE,session)
    }
    
    # worlds
    if (!switches$doWorlds) {
      switches$doWorlds<<-TRUE
      insertTab("Hypothesis",worldPanel(),"Effects","after",select=FALSE,session)
      insertTab("HypothesisDiagram",worldDiagram(),"Hypothesis","after",select=FALSE,session)
      updateSelectInput(session,"possibleUseSource",choices=c("null","world","prior"))
      updateSelectInput(session,"possibleUsePrior",choices=c("none","world","prior"))
    }
    
    # cheating
    if (!switches$doCheating) {
      switches$doCheating<<-TRUE
    }
    # likelihood inferences
    if (!switches$doLikelihoodInfer) {
      switches$doLikelihoodInfer<<-TRUE
    }
    
    # explore
    updateSelectInput(session,"Explore_typeH",choices=hypothesisChoicesV2Extra)
    updateSelectInput(session,"Explore_typeD",choices=designChoicesExtra)

    updateSelectInput(session,"Explore_showH",choices=showChoicesExtra)
    updateSelectInput(session,"Explore_showD",choices=showChoicesExtra)

    updateSelectInput(session,"EvidenceInfer_type",choices=singleTypeChoicesExtra)
    updateSelectInput(session,"EvidenceExpected_par1",choices=inferTypeChoicesExtra,selected="r")
    updateSelectInput(session,"EvidenceExpected_par2",choices=inferTypeChoicesExtra,selected="p")
  }
}


