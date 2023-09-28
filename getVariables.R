###########################
# variables

makeVar<-function(name,type="Interval",
                  mu=0,sd=1,skew=0,kurtosis=3,
                  nlevs=7,iqr=3,median=4,discrete="discrete",ordProportions=NA,
                  ncats=2,cases="C1,C2",proportions="1,1",source="discrete",
                  deploy="Between",targetDeploys="",process="sim"){
  
  var<-list(name=name,type=type,
       mu=mu,sd=sd,skew=skew,kurtosis=kurtosis,
       nlevs=nlevs,iqr=iqr,median=median,discrete=discrete,ordProportions=ordProportions,
       ncats=ncats,cases=cases,proportions=proportions,source=source,
       deploy=deploy,targetDeploys=targetDeploys,
       process=process)
  # do ordinal mean and sd (for graphs)
  if (var$type=="Ordinal") {
    var$mu<-var$median
    var$sd<-var$iqr/2
    if (!is.na(ordProportions)) {
    pp<-strsplit(var$ordProportions,",")
    pp<-pp[[1]]
    if (length(pp)<var$nlevs) {
      pp<-c(pp,rep("1",var$nlevs-length(pp)))
    }
    if (length(pp)>var$nlevs){
      pp<-pp[1:var$nlevs]
    }
    var$ordProportions<-paste(pp,sep='',collapse=',')
    } else {
      var$ordProportions<-NA
    }
  }
  # check for cases
  if (var$type=="Categorical") {
  cs<-strsplit(var$cases,",")
  cs<-cs[[1]]
  if (length(cs)<var$ncats){
    cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
  }
  if (length(cs)>var$ncats){
    cs<-cs[1:var$ncats]
  }
  var$cases<-paste(cs,sep='',collapse=',')
  # check for proportions
  pp<-strsplit(var$proportions,",")
  pp<-pp[[1]]
  if (length(pp)<var$ncats) {
    pp<-c(pp,rep("1",var$ncats-length(pp)))
  }
  if (length(pp)>var$ncats){
    pp<-pp[1:var$ncats]
  }
  var$proportions<-paste(pp,sep='',collapse=',')
  }
  # return var
  var
}

getVariables<-function() {
  
defaultVars<-list(
  makeVar(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2"),
  makeVar(name="IV2",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2"),
  makeVar(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2"),

  makeVar(name="Treatment",type="Categorical",ncats=2,cases="before,after",proportions="1,1"),
  makeVar(name="Treatment?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
  makeVar(name="IQ",type="Interval",mu=100,sd=15),
  makeVar(name="Diligence",type="Interval",mu=0,sd=2),
  makeVar(name="Perfectionism",type="Interval",mu=0,sd=2),
  makeVar(name="Happiness",type="Interval",mu=50,sd=12),
  makeVar(name="Grade",type="Interval",mu=65,sd=10),
  makeVar(name="RiskTaking",type="Interval",mu=30,sd=6),
  makeVar(name="Interesting",type="Interval",mu=10,sd=2),
  
  makeVar(name="Coffee?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
  makeVar(name="Smoker?",type="Categorical",ncats=2,cases="no,yes",proportions="2,1"),
  makeVar(name="RiskTaker?",type="Categorical",ncats=2,cases="no,yes"),
  makeVar(name="Musician?",type="Categorical",ncats=2,cases="no,yes"),
  
  makeVar(name="StudySubject",type="Categorical",ncats=3,cases="psych,phil,sports",proportions="1.5,1,2"),
  makeVar(name="BirthOrder",type="Categorical",ncats=4,cases="first,middle,last,only",proportions="1,0.4,0.6,0.2")
)

variablesLocal<-data.frame(defaultVars[[1]])
for (i in 2:length(defaultVars)){
  variablesLocal<-rbind(variablesLocal,defaultVars[[i]])
}
if (switches$startBlank) {
  variablesLocal[1,]$type="empty"
  variablesLocal[3,]$type="empty"
}
variables<<-variablesLocal

defaultVariables<<-variables
variablesHeld<<-"Simulations"


emptyVariable<<-makeVar(name="none")

# make basic variables    
IV<<-variables[1,]
IV2<<-emptyVariable
DV<<-variables[3,]
MV<<-IV
}

