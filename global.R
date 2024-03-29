source("packages.R")

################################

switches<-list(doKeys=TRUE,doClipboard=FALSE,
               doCheating=TRUE,doReplications=TRUE,doPossible=TRUE,doBootstrap=TRUE,
               doWorlds=FALSE,doMetaAnalysis=FALSE,doLikelihoodInfer=FALSE,doPossiblePower=FALSE,
               doVariablesExplore=FALSE,
               doBatchFiles=FALSE,
               startBlank=FALSE,
               showProgress=TRUE,showAnimation=TRUE,doStop=FALSE,
               importOrdinals=TRUE,
               rigidWithin=TRUE) # only applies to imported data

################################
# ui design

fontScale<-1

mainHue=0.55

hypHue=0.986667
desHue=0.1
eviHue=0.33333
expHue=0.65
posHue=0.583
metHue=0.05
filHue=0.0833

mainSat=0.25
mainBright=0.95

subSat=0.16
subBright=1

darkSat=0.5
darkBright=0.85

fileBright=0.8
likeBright=0.8
exploreSat=0.8

panelcolours<-list(hypothesisC=hsv(hypHue,mainSat,mainBright),
                   designC=hsv(desHue,mainSat,mainBright),
                   simulateC=hsv(eviHue,mainSat,mainBright),
                   exploreC=hsv(expHue,mainSat*exploreSat,mainBright),
                   possibleC=hsv(posHue,mainSat,mainBright*likeBright),
                   metaAnalC=hsv(metHue,mainSat,mainBright*likeBright),
                   filesC=hsv(filHue,mainSat,mainBright*fileBright),
                   helpC=hsv(mainHue,mainSat,mainBright)
)
subpanelcolours<-list(hypothesisC=hsv(hypHue,subSat,subBright),
                      designC=hsv(desHue,subSat,subBright),
                      simulateC=hsv(eviHue,subSat,subBright),
                      exploreC=hsv(expHue,subSat*exploreSat,subBright),
                      possibleC=hsv(posHue,subSat,subBright),
                      filesC=hsv(filHue,subSat,subBright*fileBright)
)
darkpanelcolours<-list(hypothesisC=hsv(hypHue,darkSat,darkBright),
                      designC=hsv(desHue,darkSat,darkBright),
                      simulateC=hsv(eviHue,darkSat,darkBright),
                      exploreC=hsv(expHue,darkSat*exploreSat,darkBright),
                      possibleC=hsv(posHue,darkSat,darkBright*likeBright),
                      filesC=hsv(filHue,darkSat,darkBright*fileBright)
)


maincoloursBL<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF")
maincoloursBW<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")
maincolours<-maincoloursBL

updateThemes<-function() {
  mainTheme<<-theme(panel.background = element_rect(fill=graphcolours$graphBack, colour="black"),
                  panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                  plot.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC))
  SMplotTheme<<-theme(plot.title=element_text(size=14,face="bold"),axis.title=element_text(size=16,face="bold"),
                      axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
  LGplotTheme<<-theme(plot.title=element_text(size=21,face="bold"),axis.title=element_text(size=24,face="bold"),
                      axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))
  
  plotTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
  reportTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
  diagramTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
  
  plotBlankTheme<<-theme(panel.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC),
                         panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                         plot.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC),
                         axis.title=element_text(size=16,face="bold")
  )
  
  gridTheme<<-theme(plot.margin=margin(0,0,0,0,"cm"))
}


#useful character codes
helpChar=HTML("<span style=\"color:#005E86;\"><b>?</b></span>")

# ui styles
labelStyle=paste0("font-size:",format(0.8*fontScale) ,"vw;font-weight:bold;text-align: left;")
localStyle=paste0("font-size:",format(0.8*fontScale) ,"vw;font-weight:bold;text-align: right;")
localPlainStyle=paste0("font-size:",format(0.8*fontScale) ,"vw;font-weight:normal;text-align: right;")
helpStyle=paste("font-size:",format(0.7*fontScale) ,"vw;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")

################################
# some global values for general running

mergeVariables<-FALSE # when reading data - add to existing data?

shiftKeyOn<-FALSE
controlKeyOn<-FALSE
altKeyOn<-FALSE

validSample<-FALSE
validExpected<-FALSE
validExplore<-FALSE
validPossible<-0
show<-0

shortHand<-FALSE      # instead of making full samples
shortHandGain=10

uniformZrange=10

#####################
# warnings to generate

warn3Cat2<-FALSE
warnOrd<-FALSE
warn3Ord<-FALSE

##############################
# the stop running mechanism is complex...
#
# at the heart of the simulations is a repeating cycle through the simulation
# this is kept going by call to invalidate(ms) to trigger the next cycle
#
# if the stop button is pressed, the calls to invalidate() should stop
# But these invalidate() calls queue up, if one is called before the previous one has finished.
# Any press of the stop button joins that queue.
# For the stop to be (nearly) immediate, we have to make sure there is never a queue.
# For this, it is important not to invalidate() before the current one is done.
#
# To achieve this, the duration ms is set slightly longer than the time it takes to 
# do a cycle. There is.a significant bit oft he cycle that we cannot see - this is the
# part where graphs/reports are drawn. 
# So the cycle time is established by timing the first few runs (cycles2observe)
# and using that as a good guess for how long subsequent cycles will last.
# 
# The call to invalidate() for the next cycle takes that cycle time and adds 
# an additional time of pauseWait ms
# this (nearly) guarantees a gap between cycles for the stop button to be registered
#
stopLabel<-"Stop"
startLabel<-"Run"
silentTime<-0
pauseWait<-500
cycles2observe<-5
cycleCount<-0

startButton<-function(session,whichButton) {
  if (switches$doStop) {
    updateActionButton(session,whichButton,label=startLabel)
  } else {
    showElement(whichButton)
  }
}
stopButton<-function(session,whichButton) {
  if (switches$doStop) {
    updateActionButton(session,whichButton,label=stopLabel)
  } else {
    hideElement(whichButton)
  }
}


source("getGlobals.R")
getGlobals()
source("getVariables.R")
getVariables()

###########################################
# fine tuning
is_local <- (Sys.getenv('SHINY_PORT') == "") && (Sys.getenv("USERNAME")=="rjwatt42" || Sys.info()["user"]=="rogerwatt")
if (is_local) {
  switches$doPossiblePower<-TRUE
  switches$doBatchFiles<-TRUE
  switches$doVariablesExplore<-TRUE
  
  # switches$doWorlds<-TRUE
}

