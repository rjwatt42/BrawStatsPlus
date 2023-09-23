
runBatchFiles<-function(IV,IV2,DV,effect,design,evidence,input,useWorld=TRUE){

  subDir<-"DataFiles"
  if (!file.exists(subDir)){
    dir.create(file.path(pwd(), subDir))
  }

vvals<-c()
rvals<-c()    
pvals<-c()
anvals<-c()
fvals<-c()    
wvals<-c()

files<-c()

nfiles<-input$batchFile_length
for (i in 1:nfiles) {
  IVtype<-ceil(runif(1)*4)
  switch (IVtype,
          {IV$type<-"Interval"},
          {IV$type<-"Ordinal"},
          {IV$type<-"Categorical"
          IV$ncats<-2
          IV$cases<-c("C1","C2")
          IV$proportions<-c(1,1)},
          {IV$type<-"Categorical"
          IV$ncats<-3
          IV$cases<-c("C1","C2","C3")
          IV$proportions<-c(1,1,1)},
  )

  DVtype<-ceil(runif(1)*3)
  switch (DVtype,
          {DV$type<-"Interval"},
          {DV$type<-"Ordinal"},
          {DV$type<-"Categorical"
          DV$ncats<-2
          DV$cases<-c("E1","E2")
          DV$proportions<-c(1,1)},
  )
  switch (input$batchFile_nVars,
          "2"={IV2<-c()},
          "3"={
            IV2<-IV
            IV2type<-ceil(runif(1)*4)
            IV2$name<-"IV2"
            switch (IV2type,
                    {IV2$type<-"Interval"},
                    {IV2$type<-"Ordinal"},
                    {IV2$type<-"Categorical"
                    IV2$ncats<-2
                    IV2$cases<-c("D1","D2")
                    IV2$proportions<-c(1,1)},
                    {IV2$type<-"Categorical"
                    IV2$ncats<-3
                    IV2$cases<-c("D1","D2","D3")
                    IV2$proportions<-c(1,1,1)},
            )
            
          },
          "either"={
            if (runif(1)<0.5) {
              IV2<-c()
            } else {
              IV2<-IV
              IV2type<-ceil(runif(1)*4)
              IV2$name<-"IV2"
              switch (IV2type,
                      {IV2$type<-"Interval"},
                      {IV2$type<-"Ordinal"},
                      {IV2$type<-"Categorical"
                      IV2$ncats<-2
                      IV2$cases<-c("D1","D2")
                      IV2$proportions<-c(1,1)},
                      {IV2$type<-"Categorical"
                      IV2$ncats<-3
                      IV2$cases<-c("D1","D2","D3")
                      IV2$proportions<-c(1,1,1)},
              )
            }
          }
  )

  exponent<-0.1
  if (!useWorld) {
  effect$rIV<-tanh(rexp(1,1/exponent))
  if (!is.null(IV2)) {
    effect$rIV2<-tanh(rexp(1,1/exponent))
    effect$rIVIV2<-tanh(rexp(1,1/exponent))
    effect$rIVIV2DV<-tanh(rexp(1,1/exponent))
    while(effect$rIV^2+effect$rIV2^2+effect$rIVIV2DV^2>1) {
      effect$rIV2<-tanh(rexp(1,1/exponent))
      effect$rIVIV2<-tanh(rexp(1,1/exponent))
      effect$rIVIV2DV<-tanh(rexp(1,1/exponent))
    }
  }
  design$sN<-round(runif(1,50,200))
  }
# print("OK1")

  sample<-makeSample(IV,IV2,DV,effect,design)
  if (is.null(sample)) return(NULL)
  result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
  if (is.null(result)) return(NULL)
# print("OK2")

  iv<-sample$iv
  dv<-sample$dv
  if (is.null(IV2)){
    vvals<-rbind(vvals,c(IV$type," ",DV$type))
    data<-data.frame(participant=result$participant,iv=iv,dv=dv)
    colnames(data)<-c("Participant",IV$name,DV$name)
    rvals<-rbind(rvals,c(result$rIV,0,0,0,0,0,0,0,0))
    pvals<-rbind(pvals,c(result$pIV,0,0,0,0,0,0,0,0))
  } else {
    iv2<-sample$iv2
    vvals<-rbind(vvals,c(iv$type,iv2$type,dv$type))
    data<-data.frame(participant=result$participant,iv=iv,iv=iv2,dv=dv)
    colnames(data)<-c("Participant",IV$name,IV2$name,DV$name)
    rvals<-rbind(rvals,c(result$rIV,result$rIV2,result$rIVIV2DV,result$r$unique,result$r$total))
    pvals<-rbind(pvals,c(result$pIV,result$pIV2,result$pIVIV2DV,result$p$unique,result$p$total))
  }
  fvals<-rbind(fvals,c(result$rFull,result$rFullse,result$rFullCI))
  wvals<-rbind(wvals,c(result$wFull,result$wFulln80))
  anvals<-rbind(anvals,c(result$an_name,result$test_name,result$test_val,result$df))
  # print("OK3")

  if (!is.null(data)) 
  {filename<-paste0(subDir,"/","Data",format(i),".xlsx")
  write_xlsx(data, path = filename)
  files<-rbind(files,filename)
  }
# print("OK4")
  showNotification(paste0("Batch files: ",i,"/",nfiles),id="counting",duration=Inf,closeButton=FALSE,type="message")
}
showNotification(paste0("Batch files: Done"),id="counting",duration=Inf,closeButton=FALSE,type="message")

filename<-paste0(subDir,"/","Results.xlsx")
data<-data.frame(file=files,v=vvals,r=rvals,p=pvals,f=fvals,w=wvals,an=anvals)
colnames(data)<-c("files",
                  "IVtype","IV2type","DVtype",
                  "rIV","rIV2","rIVxIV2","rUniqueIV","rUniqueIV2","rUniqueIVxIV2","rTotalIV","rTotalIV2","rTotalIVxIV2",
                  "pIV","pIV2","pIVxIV2","pUniqueIV","pUniqueIV2","pUniqueIVxIV2","pTotalIV","pTotalIV2","pTotalIVxIV2",
                  "rFull","rFullse","rFullCI(1)","rFullCI(2)",
                  "wFull","wFullN80",
                  "analysis","test statistic","test value","df")

write_xlsx(data, path = filename)

Sys.sleep(2)
removeNotification(id = "counting")

}


