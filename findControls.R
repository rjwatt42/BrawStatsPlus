#########################################
##

files<-list.files('.',pattern = 'ui')


controls1<-c()
controls2<-c()
controls3<-c()

for (fi in 1:length(files)) {
  print(files[fi])
  lines<-readLines(files[fi])
  for (li in 1:length(lines)) {
    a<-str_match(lines[li],'numericInput[(]*\"[a-zA-Z0-9_]*\"')
    if (!is.na(a)) {
      a<-substr(a,nchar('numericInput(\"'),nchar(a))
      controls1<-c(controls1,a)
    }
    a<-str_match(lines[li],'checkboxInput[(]*\"[a-zA-Z0-9_]*\"')
    if (!is.na(a)) {
      a<-substr(a,nchar('checkboxInput(\"'),nchar(a))
      controls2<-c(controls2,a)
    }
    a<-str_match(lines[li],'selectInput[(]*\"[a-zA-Z0-9_]*\"')
    if (!is.na(a)) {
      a<-substr(a,nchar('selectInput(\"'),nchar(a))
      controls3<-c(controls3,a)
    }
  }
}

#########################################
##
controlArray1<-c()
controlArray2<-c()
controlArray3<-c()

for (ci in 1:length(controls1)) {
  if (ci>1) controlArray1<-paste(controlArray1,",")
  controlArray1<-paste0(controlArray1,controls1[ci])
}

for (ci in 1:length(controls2)) {
  if (ci>1) controlArray2<-paste(controlArray2,",")
  controlArray2<-paste0(controlArray2,controls2[ci])
}

for (ci in 1:length(controls3)) {
  if (ci>1) controlArray3<-paste(controlArray3,",")
  controlArray3<-paste0(controlArray3,controls3[ci])
}

