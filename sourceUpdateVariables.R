######################################################  
## update variables functions

updateIV<-function(){
  if (debug) debugPrint("     updateIV")
  use<-match(input$IVchoice,variables$name)
  if (is.na(use)) return(NULL)

  IV<-as.list(variables[use,])
  
  if (IV$type!="Interval" && shortHand) {
    hmm("Please switch to longhand calculations: IV not Interval")
  }
  if (any(c(IV$skew,IV$kurtosis-3)!=0) && shortHand) {
    hmm("Please switch to longhand calculations: IV skew/kurtosis")
  }
  
  if (IV$type=="Categorical") {
    cs<-IV$cases
    cs<-strsplit(cs,",")
    cs<-cs[[1]]
    if (length(cs)<IV$ncats){
      cs<-c(cs,paste("C",(length(cs)+1):IV$ncats,sep=""))
    }
    IV$cases<-cs
    if (is.character(IV$proportions)) {
      IV$proportions<-as.numeric(unlist(strsplit(IV$proportions,",")))
    }
  }
  if (simData) {
    IV$deploy<-input$sIV1Use
  }
  if (debug) debugPrint("     updateIV - exit")
  return(IV)
}

updateIV2<-function(){
  if (debug) debugPrint("     updateIV2")
  if (input$IV2choice=="none"){
    no_ivs<<-1
    if (debug) debugPrint("     updateIV2 - exit unused")
    return(NULL)
  } else {
    no_ivs<<-2
  }
  
  use<-match(input$IV2choice,variables$name)
  if (is.na(use)) return(NULL)
  
  IV2<-as.list(variables[use,])
  
  if (IV2$type=="Categorical") {
    cs<-IV2$cases
    cs<-strsplit(cs,",")
    cs<-cs[[1]]
    if (length(cs)<IV$ncats){
      cs<-c(cs,paste("C",(length(cs)+1):IV$ncats,sep=""))
    }
    IV2$cases<-cs
    if (is.character(IV2$proportions)) {
      IV2$proportions<-as.numeric(unlist(strsplit(IV2$proportions,",")))
    }
    #             IV$proportions<-MV$prop
  }
  if (simData) {
    IV2$deploy<-input$sIV2Use
  }
  if (debug) debugPrint("     updateIV2 - exit")
  return(IV2)
}

updateDV<-function(){
  if (debug) debugPrint("     updateDV")
  use<-match(input$DVchoice,variables$name)
  if (is.na(use)) return(NULL)
  
  DV<-as.list(variables[use,])
  if (DV$type=="Ordinal" && input$IV2choice!="none") {
    if (warn3Ord==FALSE) {
      hmm("Ordinal DV with more than 1 IV. It will be treated as Interval.")
      warn3Ord<<-TRUE
    }
  }
  
  if (DV$type!="Interval" && shortHand) {
    hmm("Please switch to longhand calculations: DV not Interval")
  }
  if (any(c(DV$skew,DV$kurtosis-3)!=0) && shortHand) {
    hmm("Please switch to longhand calculations: DV skew/kurtosis")
  }
  
  if (DV$type=="Categorical") {
    cs<-DV$cases
    cs<-strsplit(cs,",")
    cs<-cs[[1]]
    if (length(cs)<IV$ncats){
      cs<-c(cs,paste("C",(length(cs)+1):DV$ncats,sep=""))
    }
    DV$cases<-cs
    if (is.character(DV$proportions)) {
      DV$proportions<-as.numeric(unlist(strsplit(DV$proportions,",")))
    }
    #             IV$proportions<-MV$prop
  }
  if (debug) debugPrint("     updateDV - exit")
  return(DV)
}

# UI changes    
observeEvent(c(input$rIV,input$rIV2,input$rIVIV2,input$rIVIV2DV,
               input$sN,input$sMethod,input$sIV1Use,input$sIV2Use),{
                 if (debug) debugPrint("     effectChanged")
                 
                 # remove out of date sample and other 
                 validSample<<-FALSE
                 validExpected<<-FALSE
                 validExplore<<-FALSE
                 
                 # expectedResult<-c()
                 exploreResultHold<-list(Hypothesis=c(),Design=c(),MetaAnalysis=c())
                 possiblePResultHold<-c()
                 possibleSResultHold<-c()
                 
                 updateCheckboxInput(session,"EvidenceExpected_append",value=FALSE)
                 updateCheckboxInput(session,"ExploreAppendH",value=FALSE)
                 updateCheckboxInput(session,"ExploreAppendD",value=FALSE)
                 updateCheckboxInput(session,"ExploreAppendM",value=FALSE)
                 
                 if (debug) debugPrint("     effectChanged - exit")
               },priority=100)

observeEvent(c(input$IVchoice,input$IV2choice,input$DVchoice),
             {
               validSample<<-FALSE
               validExpected<<-FALSE
               validExplore<<-FALSE
             })


