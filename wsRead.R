readWS<-function(session,filename,sheetname){

  if (filename=="clip") {
    raw_data<-read_clip_tbl()
  } else {
    if (isempty(sheetname)) {
      raw_data<-read_excel(filename)
    } else {
      raw_data<-read_excel(filename,sheet=sheetname)
    }
  }
  if (ncol(raw_data)<3) {return(null)}
  
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  IV2changed<-FALSE
  
  for (i in 1:nrow(raw_data)){
    val<-raw_data[[i,4]]
    if (!is.na(val)){
      if (!grepl("[^0-9.]",val)) {val<-as.numeric(val)}
      if (is.element(raw_data[[i,1]],c("IV","IV2","DV")))
      { variable<-raw_data[[i,1]]
        field<-raw_data[[i,2]]
        if (variable=="IV2") IV2changed<-TRUE
        if (is.element(variable,variables$name)){
          use<-match(variable,variables$name)
          variables[use,field]<<-val
        }
        switch (variable,
                "IV"={IV[field]<-val},
                "IV2"={IV2[field]<-val},
                "DV"={DV[field]<-val}
        )
        
      }
      else {
        objectID<-raw_data[[i,2]] 
        if (!is.na(raw_data[[i,3]])) objectID<-raw_data[[i,3]]
        done<-FALSE
        if (is.element(objectID,c("sMethod","sIV1Use","sIV2Use","ResidDistr","sCheating","sCheatingLimit",
                                  "sReplSigOnly","sReplType","sReplCorrection","sReplKeep","populationRZ","populationPDF"))){
          updateSelectInput(session,objectID,selected=val)
          done<-TRUE
        }
        if (is.element(objectID,c("sRangeOn","rInteractionOn","worldOn","worldAbs","Welch","sNRand","sBudgetOn","sReplicationOn","sReplPowerOn","sReplVarAlpha"))){
          switch(val,
                 "FALSE"={val<-FALSE},
                 "TRUE"={val<-TRUE}
          )
          updateCheckboxInput(session,objectID,value=val)
          done<-TRUE
        }
        if (is.element(objectID,c("sDVRange","sIVRange"))){
          val<-as.numeric(strsplit(val,",")[[1]])
          updateSliderInput(session,objectID,value=val)
          done<-TRUE
        }
        if (!done){
          if (!grepl("[^0-9.]",val)) {
            val<-as.numeric(val)
            updateNumericInput(session,objectID,value=val)
          }
        }
      }
    }
  }

  useIV<-match(IV$name,variables$name)
  if (is.na(useIV)){
    useIV<-nrow(variables)+1
    variables<<-rbind(variables,IV)
  }
  variables[useIV,]<<-IV
  
  if (IV2changed) {
    useIV2<-match(IV2$name,variables$name)
    if (is.na(useIV2)){
      useIV2<-nrow(variables)+1
      variables<<-rbind(variables,IV2)
    }
    variables[useIV2,]<<-IV2
  }
  
  useDV<-match(DV$name,variables$name)
  if (is.na(useDV)){
    useDV<-nrow(variables)+1
    variables<<-rbind(variables,DV)
  }
  variables[useDV,]<<-DV

  updateSelectInput(session,"IVchoice",choices=variables$name,selected=IV$name)
  updateSelectInput(session,"DVchoice",choices=variables$name,selected=DV$name)
  if (IV2changed) {
    updateSelectInput(session,"IV2choice",choices=c("none",variables$name),selected=IV2$name)
  } else {
    updateSelectInput(session,"IV2choice",choices=c("none",variables$name),selected="none")
  }
  
}
