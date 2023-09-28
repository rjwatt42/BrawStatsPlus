##################################################################################    
#  IMPORT/EXPORT data and workspace

getNewVariables<-function(raw_data,header=c()){
  keep<-!apply(is.na(raw_data),2,all)
  raw_data<-raw_data[,keep]
  
  keep<-!apply(is.na(raw_data),1,all)
  raw_data<-raw_data[keep,]
  
  newVariables<-readSample(raw_data,input$ImportOrdinals,input$MaxOrdinal,header)
  
  # save the current set of variables
  defaultVariables<<-variables
  # store the variables in global workspace
  if (mergeVariables){
    variables<<-rbind(newVariables,variables)
  } else{
    variables<<-newVariables
    simData<<-FALSE
  }
  changeUI2Data()
}    

# respond to file selection by getting sheet names
inspectDataFile<-observeEvent(input$dataInputFile, {
  sheet_names<-excel_sheets(input$dataInputFile$datapath)
  updateSelectInput(session, "dataInputSheet", choices = sheet_names)
  # get the raw data
  if (length(sheet_names)<=1){
    mergeVariables<<-FALSE
    raw_data<-read_excel(input$dataInputFile$datapath)
    if (nrow(raw_data)>0 && ncol(raw_data)>0)
      getNewVariables(raw_data)
  }
})

# data input    
importDataFile<-observeEvent(input$dataInputFileLoad, {
  mergeVariables<<-FALSE
  # get the raw data
  if (is.character(input$dataInputFile$datapath)) {
    if (isempty(input$dataInputSheet)) {
      raw_data<-read_excel(input$dataInputFile$datapath)
    } else {
      raw_data<-read_excel(input$dataInputFile$datapath,sheet = input$dataInputSheet)
    }
    if (nrow(raw_data)>0 && ncol(raw_data)>0)
      getNewVariables(raw_data)
  }
})

readCLipDataFile<-observeEvent(input$dPaste, {
  mergeVariables<<-FALSE
  # get the raw data
  raw_h1<-read_clip()
  header<-strsplit(raw_h1[1],"\t")[[1]]
  raw_data<-read_clip_tbl()
  # read_clip_tbl doesn't like some characters like | and =
  colnames(raw_data)<-header
  if (nrow(raw_data)>0 && ncol(raw_data)>0)
    getNewVariables(raw_data)
})

exportData<-function() {
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  result<-sampleAnalysis()
  if (is.null(result)) return(NULL)
  
  id<-result$participant
  iv<-result$iv
  dv<-result$dv
  if (input$ExportShortForm && (IV$deploy=="Within" && (!is.null(IV2) && IV2$deploy=="Within"))) {
    iv2<-result$iv2
    id.d<-unique(id)
    data<-data.frame(Participant=id.d)
    
    for (j1 in 1:IV$ncats) {
      for (j2 in (1:IV2$ncats)) {
        iv.j<-c()
        for (i in 1:length(id.d)) {
          use<-(id==id.d[i]) & (iv==IV$cases[j1]) & (iv2==IV2$cases[j2])
          iv.j<-c(iv.j,mean(dv[use],na.rm=TRUE))
        }
        data[[paste0(DV$name,"|",IV$name,"=",IV$cases[j1],";",IV2$name,"=",IV2$cases[j2])]]<-iv.j
      }
    }
  } else {
    if (input$ExportShortForm && (IV$deploy=="Within" || (!is.null(IV2) && IV2$deploy=="Within"))) {
      id.d<-unique(id)
      data<-data.frame(Participant=id.d)
      
      if (IV$deploy=="Between") {
        use.d<-c()
        for (i in 1:length(id.d)) {
          use<-which(id==id.d[i])
          use.d<-c(use.d,use[1])
        }
        data[IV$name]<-iv[use.d]
      } else {
        for (j in 1:IV$ncats) {
          iv.j<-c()
          for (i in 1:length(id.d)) {
            use<-(id==id.d[i]) & (iv==IV$cases[j])
            iv.j<-c(iv.j,mean(dv[use],na.rm=TRUE))
          }
          data[[paste0(DV$name,"|",IV$name,"=",IV$cases[j])]]<-iv.j
        }
      }
      
      if (!is.null(IV2)) {
        iv2<-result$iv2
        if (IV2$deploy=="Between") {
          use.d<-c()
          for (i in 1:length(id.d)) {
            use<-which(id==id.d[i])
            use.d<-c(use.d,use[1])
          }
          data[IV2$name]<-iv2[use.d]
        } else {
          for (j in 1:IV2$ncats) {
            iv2.j<-c()
            for (i in 1:length(id.d)) {
              use<-(id==id.d[i]) & (iv2==IV2$cases[j])
              iv2.j<-c(iv2.j,mean(dv[use],na.rm=TRUE))
            }
            data[[paste0(DV$name,"|",IV2$name,"=",IV2$cases[j])]]<-iv2.j
          }
        }
      }
    } else {
      if (is.null(IV2)){
        data<-data.frame(participant=id,iv=iv,dv=dv)
        colnames(data)<-c("Participant",IV$name,DV$name)
      } else {
        iv2<-result$iv2
        data<-data.frame(participant=id,iv=iv,iv=iv2,dv=dv)
        colnames(data)<-c("Participant",IV$name,IV2$name,DV$name)
      }
    }
  }
  data      
}

exportDataClip<-observeEvent(input$dCopy, {
  data<-exportData()      
  if (!is.null(data)) write_clip(data,allow_non_interactive = TRUE,col.names=input$ExportHeader)
})

exportDataFile<-observeEvent(input$dataOutputFileSave, {
  data<-exportData()      
  if (!is.null(data)) 
  {filename<-input$dataOutputFile
  ext<-file_ext(filename)
  if (ext!="xlsx" && ext!="xls") {filename=paste(filename,".xlsx",sep="")}
  
  write_xlsx(data, path = filename)
  }
})


##########################################

exportMetaFile<-observeEvent(input$metaOutputFileSave, {
  meta<-data.frame(rs=metaResult$result$rIV,n=metaResult$result$nval)
  if (!is.null(meta)) 
  {filename<-input$metaOutputFile
  ext<-file_ext(filename)
  if (ext!="xlsx" && ext!="xls") {filename=paste(filename,".xlsx",sep="")}
  
  write_xlsx(meta, path = filename)
  }
})


##########################################

addList<-function(L,name) {
  addFields<-names(L)
  header<-name
  fields<-""
  subfields<-""
  vals<-""
  
  for (i in 1:length(addFields)){
    v<-L[[addFields[i]]]
    if (class(v)!="list") {
      if (length(v)>1) v<-paste(v,collapse=",")
      if (!is.na(v) && !is.null(v) && length(v)==1){
        if (is.logical(v)){
          if (v){v<-"TRUE"} else {v<-"FALSE"}
        }
        header<-c(header,name)
        fields<-c(fields,addFields[i])
        subfields<-c(subfields,"")
        vals<-c(vals,v)
      }
    } else {
      newList<-addList(v,addFields[i])
      newList<-data.frame(top=name,newList)
      header<-c(header,newList$top)
      fields<-c(fields,newList$header)
      subfields<-c(subfields,newList$field)
      vals<-c(vals,newList$value)
    }
  }
  data.frame(header=header,field=fields,subfield=subfields,value=vals)
}

exportWSFile<-observeEvent(input$WSOutputFileSave, {
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  
  data1<-addList(IV,"IV")
  if (!is.null(IV2))  data2<-addList(IV2,"IV2")
  else data2<-c()
  data3<-addList(DV,"DV")
  data4<-addList(effect,"effect")
  data5<-addList(design,"design")
  data<-rbind(data1,data2,data3,data4,data5)

  filename<-input$wsOutputFile
  ext<-file_ext(filename)
  if (ext!="xlsx" && ext!="xls") {filename=paste(filename,".xlsx",sep="")}
  
  write_xlsx(data, path = filename)
})

exportWSCLipboard<-observeEvent(input$wsCopy, {
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  
  data1<-addList(IV,"IV")
  data2<-addList(IV2,"IV2")
  data3<-addList(DV,"DV")
  data4<-addList(effect,"effect")
  data5<-addList(design,"design")
  data<-rbind(data1,data2,data3,data4,data5)
  
  if (!is.null(data)) write_clip(data,allow_non_interactive = TRUE)
  
})


# respond to file selection by getting sheet names
inspectWSFile<-observeEvent(input$wsInputFile, {
  sheet_names<-excel_sheets(input$wsInputFile$datapath)
  updateSelectInput(session, "wsInputSheet", choices = sheet_names, selected=sheet_names[1])
  
  if (length(sheet_names)==1){
    readWS(session,input$wsInputFile$datapath,sheet_names[1])
  }
})

importWSFile<-observeEvent(input$wsInputFileLoad, {
  if (is.character(input$dataInputFile$datapath)) {
    readWS(session,input$wsInputFile$datapath,input$wsInputSheet)
    editVar$data<<-editVar$data+1
  }
})

importWSClip<-observeEvent(input$wsPaste, {
  readWS(session,"clip")
  editVar$data<<-editVar$data+1
})

batchfiles<-observeEvent(input$batchFileRun,{
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  runBatchFiles(IV,IV2,DV,effect,design,evidence,input)
  
})
##################################################################################    
