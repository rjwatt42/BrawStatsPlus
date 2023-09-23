source("packages.R")

################################

switches<-list(doKeys=TRUE,doClipboard=FALSE,
               doCheating=TRUE,doReplications=TRUE,doPossible=TRUE,doBootstrap=TRUE,
               doWorlds=FALSE,doMetaAnalysis=FALSE,doLikelihoodInfer=FALSE,doPossiblePower=FALSE,
               doVariablesExplore=FALSE,
               doBatchFiles=FALSE,
               loadExtrasValue=FALSE,
               startBlank=FALSE,
               showAnimation=TRUE,
               importOrdinals=TRUE,
               rigidWithin=TRUE) # only applies to imported data

################################
# ui design

fontScale<-1.0

mainHue=0.55

hypHue=0.986667
desHue=0.1
eviHue=0.33333
posHue=0.583
expHue=0.65
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

#useful character codes
helpChar=HTML("<span style=\"color:#005E86;\"><b>?</b></span>")

# ui styles
labelStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:bold;text-align: left;")
localStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:bold;text-align: right;")
localPlainStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:normal;text-align: right;")
helpStyle=paste("font-size:",format(7*fontScale) ,"pt;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")

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
doStop<-TRUE
silentTime<-0
stopLabel<-"Stop"
pauseWait<-300
cycles2observe<-5
cycleCount<-0

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
  # switches$doWorlds<-TRUE
    # switches$loadExtrasValue<-TRUE
}

