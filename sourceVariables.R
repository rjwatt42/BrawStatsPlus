observeEvent(input$IVchoice,{
  if (debug) debugPrint("IVChoice")
  use<-match(input$IVchoice,variables$name)
  if (!is.na(use)){
    newMV<-variables[use,]
  }
  else return(NULL)
  updateSelectInput(session,"sIV1Use", selected=newMV$deploy)
  if (switches$rigidWithin && variablesHeld=="Data") {
    if(newMV$deploy=="Within" && nchar(newMV$targetDeploys)>0) {
      DVchoices<-strsplit(substring(newMV$targetDeploys,2,nchar(newMV$targetDeploys)-1),",")[[1]]
      use<-!(variables$name %in% DVchoices) & (sapply(variables$targetDeploys,nchar)==0)
      DVchoices<-list("applicable"=c(DVchoices," "),"not applicable"=variables$name[use])
      updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices$available[1])
    } else {
      DVchoices<-variables$name[sapply(variables$targetDeploys,nchar)==0]
      updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices[length(DVchoices)])
    }
  }
  
  switch (newMV$type,
          "Interval"={
            shinyjs::disable(id= "sIV1Use")
          },
          "Categorical"={
            shinyjs::enable(id= "sIV1Use")
          }
  )
  
})

observeEvent(input$IV2choice,{
  use<-match(input$IV2choice,variables$name)
  if (!is.na(use)){
    newMV<-variables[use,]
  }
  else return(NULL)
  updateSelectInput(session,"sIV2Use", selected=newMV$deploy)
})

# modalDialog to edit each variable
# all of this code only gets used if the modalDialog mechanism is set up in ui.R
# if we are using the older tabs mechanism, then this code never gets called
source("uiVariable.R")

modalVar<-c()
editVar<-reactiveValues(data=0)
oldName<-""

updateMVType<-function(MV) {
  if (debug) debugPrint("updateMVType")
  
  switch (MV$type,
          "Interval"={
            shinyjs::hideElement(id= "MVOrdVal")
            shinyjs::hideElement(id= "MVCatVala")
            shinyjs::hideElement(id= "MVCatValb")
            shinyjs::showElement(id= "MVIntVal")
          },
          "Ordinal"={
            shinyjs::hideElement(id= "MVIntVal")
            shinyjs::hideElement(id= "MVCatVala")
            shinyjs::hideElement(id= "MVCatValb")
            shinyjs::showElement(id= "MVOrdVal")
          },
          "Categorical"={
            shinyjs::hideElement(id= "MVIntVal")
            shinyjs::hideElement(id= "MVOrdVal")
            shinyjs::showElement(id= "MVCatVala")
            shinyjs::showElement(id= "MVCatValb")
          },
  )
  
  # if we are editing imported variables, there is less we can change
  if (MV$process=="data") {
    shinyjs::hideElement(id= "MVIntVal")
    shinyjs::hideElement(id= "MVOrdVal")
    shinyjs::hideElement(id= "MVCatVala")
    shinyjs::hideElement(id= "MVCatValb")
  }
  if (debug) debugPrint("updateMVType - exit")
  
}
observeEvent(input$MVtype, {
  updateMVType (list(type=input$MVtype,process=MV$process))
})
observeEvent(input$MVnlevs, {
  updateNumericInput(session,"MVcentre",value=(input$MVnlevs+1)/2)
  updateNumericInput(session,"MVspread",value=(input$MVnlevs-1)/2)
})
#Press "OK": make the new variable
observeEvent(input$MVok, {
  if (debug) debugPrint("MVOK")
  MV<<-makeVar(name=input$MVname, type=input$MVtype,
               mu=checkNumber(input$MVmu), sd=checkNumber(input$MVsd),
               skew=checkNumber(input$MVskew), kurtosis=checkNumber(input$MVkurt),
               nlevs=input$MVnlevs,median=input$MVcentre,iqr=checkNumber(input$MVspread),discrete=input$MVdiscrete,
               ncats=input$MVncats,cases=input$MVcases,proportions=checkNumber(input$MVprop),source=input$MVsource,
               deploy=MV$deploy,process=MV$process)

  switch (modalVar,
          "IV" ={
            MV$deploy<<-input$sIV1Use
            setIVanyway(MV)
          },
          "IV2"={
            MV$deploy<<-input$sIV2Use
            setIV2anyway(MV)
          },
          "DV" ={setDVanyway(MV)}
  )
  removeModal()
  # a change of name looks after itself
  # when the uiHypothesis is updated
  # no other types of change are registered
  # so we trigger a reactiveValue events
  if (oldName==MV$name) {
    editVar$data<<-editVar$data+1
  }
})


# create the modalDialog for each variable 
observeEvent(input$editIV,{
  modalVar<<-"IV"
  IV<-updateIV()
  MV<<-IV
  
  # now set up the controls in the dialogue with up to date values
  updateTextInput(session,"MVname",value=MV$name)
  updateSelectInput(session,"MVtype",selected=MV$type)
  updateNumericInput(session,"MVmu",value=MV$mu)
  updateNumericInput(session,"MVsd",value=MV$sd)
  updateNumericInput(session,"MVskew",value=MV$skew)
  updateNumericInput(session,"MVkurt",value=MV$kurtosis)
  updateNumericInput(session,"MVnlevs",value=MV$nlevs)
  updateNumericInput(session,"MVcentre",value=MV$median)
  updateNumericInput(session,"MVspread",value=MV$iqr)
  updateNumericInput(session,"MVncats",value=MV$ncats)
  updateTextInput(session,"MVcases",value=MV$cases)
  updateTextInput(session,"MVprop",value=MV$proportions)
  updateSelectInput(session,"source",selected=MV$source)    
  
  # now show the dialog
  showModal(
    modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                              "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                title=modalVar,
                size="s",
                variableDialog,
                
                footer = tagList( 
                  modalButton("Cancel"),
                  actionButton("MVok", "OK")
                )
    )
  )
  updateMVType(MV)
  
  oldName<<-MV$name
  # make sure we get the current values
})

observeEvent(input$editIV2,{
  modalVar<<-"IV2"
  IV2<-updateIV2()
  MV<<-IV2
  
  # now set up the controls in the dialogue with up to date values
  updateTextInput(session,"MVname",value=MV$name)
  updateSelectInput(session,"MVtype",selected=MV$type)
  updateNumericInput(session,"MVmu",value=MV$mu)
  updateNumericInput(session,"MVsd",value=MV$sd)
  updateNumericInput(session,"MVskew",value=MV$skew)
  updateNumericInput(session,"MVkurt",value=MV$kurtosis)
  updateNumericInput(session,"MVnlevs",value=MV$nlevs)
  updateNumericInput(session,"MVcentre",value=MV$median)
  updateNumericInput(session,"MVspread",value=MV$iqr)
  updateNumericInput(session,"MVncats",value=MV$ncats)
  updateTextInput(session,"MVcases",value=MV$cases)
  updateTextInput(session,"MVprop",value=MV$proportions)
  updateSelectInput(session,"source",selected=MV$source)    
  
  # now show the dialog
  showModal(
    modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                              "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                title=modalVar,
                size="s",
                variableDialog,
                
                footer = tagList( 
                  modalButton("Cancel"),
                  actionButton("MVok", "OK")
                )
    )
  )
  updateMVType(MV)
  
  oldName<<-MV$name
  # make sure we get the current values
})

observeEvent(input$editDV,{
  modalVar<<-"DV"
  DV<-updateDV()
  MV<<-DV
  
  # now set up the controls in the dialogue with up to date values
  updateTextInput(session,"MVname",value=MV$name)
  updateSelectInput(session,"MVtype",selected=MV$type)
  updateNumericInput(session,"MVmu",value=MV$mu)
  updateNumericInput(session,"MVsd",value=MV$sd)
  updateNumericInput(session,"MVskew",value=MV$skew)
  updateNumericInput(session,"MVkurt",value=MV$kurtosis)
  updateNumericInput(session,"MVnlevs",value=MV$nlevs)
  updateNumericInput(session,"MVcentre",value=MV$median)
  updateNumericInput(session,"MVspread",value=MV$iqr)
  updateNumericInput(session,"MVncats",value=MV$ncats)
  updateTextInput(session,"MVcases",value=MV$cases)
  updateTextInput(session,"MVprop",value=MV$proportions)
  updateSelectInput(session,"source",selected=MV$source)    
  
  # now show the dialog
  showModal(
    modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                              "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                title=modalVar,
                size="s",
                variableDialog,
                
                footer = tagList( 
                  modalButton("Cancel"),
                  actionButton("MVok", "OK")
                )
    )
  )
  updateMVType(MV)
  
  oldName<<-MV$name
  # make sure we get the current values
})


setIVanyway<-function(newMV=NULL){
  if (debug) {debugPrint("setIVanyway")}
  newName<-FALSE    
  if (is.null(newMV)) {
    use<-match(input$IVchoice,variables$name)
    if (!is.na(use)){
      newMV<-variables[use,]
    }
    else return(NULL)
  } else {
    if (any(newMV$name==variables$name)) {
      use<-which(newMV$name==variables$name)
      variables[use,]<<-newMV
    } else {
      variables<<-rbind(variables,newMV)
      newName<-TRUE
    }
    
    if (newName) {
      if (debug) print(paste("IV new name detected",newMV$name))
      updateSelectInput(session, "IVchoice", choices=variables$name)
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name))
      if (input$IV2choice!="none") {updateSelectInput(session, "IV2choice", selected=input$IV2choice)}
      updateSelectInput(session, "DVchoice", choices = variables$name, selected=input$DVchoice)
    }
    if (newMV$name!=input$IVchoice) {
      if (debug) print(paste("IV changed name detected",newMV$name))
      updateSelectInput(session, "IVchoice", selected=newMV$name)
    }
  }
  
  updateSelectInput(session,"sIV1Use", selected=newMV$deploy)
  switch (newMV$type,
          "Interval"={
            shinyjs::disable(id= "sIV1Use")
          },
          "Categorical"={
            shinyjs::enable(id= "sIV1Use")
          }
  )
  
  validSample<<-FALSE
  validExpected<<-FALSE
  validExplore<<-FALSE
}

setIV2anyway<-function(newMV=NULL){
  if (debug) {debugPrint("setIV2anyway")}
  newName<-FALSE
  if (is.null(newMV)) {
    if (input$IV2choice=="none") {
      shinyjs::disable("editIV2")
      return(NULL)
    }
    else {
      shinyjs::enable("editIV2")
    }
    
    use<-match(input$IV2choice,variables$name)
    if (!is.na(use)){
      newMV<-variables[use,]
    } else {return(NULL)}
  } else {
    if (newMV$name !="none") {
      use<-match(newMV$name,variables$name)
      if (is.na(use)){
        use<-nrow(variables)+1
        newName<-TRUE
      }
      variables[use,]<<-newMV
    }
  }
  updateSelectInput(session,"sI21Use", selected=newMV$deploy)
  switch (newMV$type,
          "Interval"={
            shinyjs::disable(id= "sIV2Use")
          },
          "Categorical"={
            shinyjs::enable(id= "sIV2Use")
          }
  )
  
  if (newName) {
    if (debug) print(paste("IV2 new name detected",newMV$name))
    updateSelectInput(session, "IVchoice", choices=variables$name, selected=input$IVchoice)
    updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected=newMV$name)
    updateSelectInput(session, "DVchoice", choices = variables$name, selected=input$DVchoice)
  } else {
    if (newMV$name!=input$IV2choice) {
      if (debug)   print(paste("IV2 changed name detected",newMV$name))
      updateSelectInput(session, "IV2choice", selected=newMV$name)
    }
  }
  
  validSample<<-FALSE
  validExpected<<-FALSE
  validExplore<<-FALSE
}

setDVanyway<-function(newMV=NULL){
  if (debug) {debugPrint("setDVanyway")}
  newName<-FALSE
  if (is.null(newMV)) {
    use<-match(input$DVchoice,variables$name)
    if (!is.na(use)){
      newMV<-variables[use,]
    }
    else return(NULL)
  } else {
    use<-match(newMV$name,variables$name)
    if (is.na(use)){
      use<-nrow(variables)+1
      newName<-TRUE
    }
    variables[use,]<<-newMV
  }
  
  validSample<<-FALSE
  validExpected<<-FALSE
  validExplore<<-FALSE
  
  if (newName) {
    if (debug)       print(paste("DV new name detected",newMV$name))
    updateSelectInput(session, "IVchoice", choices=variables$name, selected=input$IVchoice)
    updateSelectInput(session, "IV2choice", choices = c("none",variables$name))
    if (input$IV2choice!="none") {updateSelectInput(session, "IV2choice", selected=input$IV2choice)}
    updateSelectInput(session, "DVchoice", choices = variables$name)
  }
  if (newMV$name!=input$DVchoice) {
    if (debug)       print(paste("DV changed name detected",newMV$name))
    updateSelectInput(session, "DVchoice", selected=newMV$name)
  }
  
}
