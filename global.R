library(ggplot2)

switches<-list(doKeys=TRUE,doClipboard=FALSE,doBatchFiles=FALSE,doLarge=TRUE,
               doReplications=TRUE,doPossible=TRUE,doBootstrap=TRUE,
               doWorlds=FALSE,doMetaAnalysis=FALSE,doLikelihoodInfer=FALSE,
               doCheating=TRUE,
               doVariablesExplore=FALSE,
               loadExtrasValue=FALSE,
               startBlank=FALSE,
               showAnimation=TRUE,
               importOrdinals=TRUE,
               rigidWithin=TRUE)

################################
# ui design

fontScale=0.85 # use with 400% zoom in Google Chrome
fontScale=1.0

# these are minimum widths
LGGraphHeight="14cm"
LGPanelHeight="15.3cm"
LGModalHeight="16.4cm"

# not sure which of these is more useful
LGModalWidth="30cm"
LGModalWidth="95%"

maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF",graphBack="#888888")
# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")

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
                   likelihoodC=hsv(posHue,mainSat,mainBright*likeBright),
                   filesC=hsv(filHue,mainSat,mainBright*fileBright)
)
subpanelcolours<-list(hypothesisC=hsv(hypHue,subSat,subBright),
                      designC=hsv(desHue,subSat,subBright),
                      simulateC=hsv(eviHue,subSat,subBright),
                      exploreC=hsv(expHue,subSat*exploreSat,subBright),
                      likelihoodC=hsv(posHue,subSat,subBright),
                      filesC=hsv(filHue,subSat,subBright*fileBright)
)
darkpanelcolours<-list(hypothesisC=hsv(hypHue,darkSat,darkBright),
                      designC=hsv(desHue,darkSat,darkBright),
                      simulateC=hsv(eviHue,darkSat,darkBright),
                      exploreC=hsv(expHue,darkSat*exploreSat,darkBright),
                      likelihoodC=hsv(posHue,darkSat,darkBright*likeBright),
                      filesC=hsv(filHue,darkSat,darkBright*fileBright)
)

plotcolours<-list(sampleC="#FFCC00",descriptionC="#FF8833",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#11CC00",infer_nsigC="#FF4400",
                  infer_hiterr="#118800",infer_misserr="#881100",infer_none="#AAAAAA",
                  psig="#FFAA00",fdr="#227700",fmr="#BB5555")

shapes<-list(data=21,study=22,parameter=21,meta=24)

#useful character codes
expandLabel<-HTML("&#9974")
emdash="\u2014"
helpChar=HTML("<span style=\"color:#005E86;\"><b>?</b></span>")

# ui styles
labelStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:bold;text-align: left;")
localStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:bold;text-align: right;")
localPlainStyle=paste0("font-size:",format(8*fontScale) ,"pt;font-weight:normal;text-align: right;")
helpStyle=paste("font-size:",format(7*fontScale) ,"pt;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")

report_precision<-3
graph_precision<-2
fullShowHelp<-FALSE

# graph themes
popplotMarginsTheme<-theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))

mainTheme=theme(panel.background = element_rect(fill=maincolours$graphBack, colour="black"),
                panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC))
SMplotTheme=theme(plot.title=element_text(size=16,face="bold"),axis.title=element_text(size=16,face="bold"),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
LGplotTheme=theme(plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=32,face="bold"),axis.text.x=element_text(size=16),axis.text.y=element_text(size=16))

plotTheme=mainTheme+SMplotTheme

plotBlankTheme=theme(panel.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                     panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                     plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                     axis.title=element_text(size=16,face="bold")
)

gridTheme=theme(plot.margin=margin(0,0,0,0,"cm"))


################################
# starting values for important variables

IV<-list(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2",proportions="1,1")
IV2<-list(name="none",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2",proportions="1,1")
DV<-list(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2",proportions="1,1")

effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
             world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.2,populationRZ="r",populationNullp=0,worldAbs=FALSE)
)

nulleffect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
                 world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.0,populationRZ="r",populationNullp=0)
)

design<-list(sN=42, sNRand=FALSE,sNRandK=2, 
             sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sDependence=0, sOutliers=0, sClustering=0,
             sCheating=FALSE,sCheatingAmount=5,
             sReplicationOn=FALSE,sReplPowerOn=TRUE,sReplPower=0.8,sReplTails=2,
             sReplSigOnly="No",sReplUseBudget=FALSE,sReplRepeats=1,sReplKeep="last",sReplBudget=1000,
             sReplCorrection="None",
             sReplVarAlpha=FALSE,sReplAlpha=2,
             sN_Strata=5, sR_Strata=2,
             sNClu_Cluster=5,     sRClu_Cluster=0.7,
             sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
             sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
)    

evidence<-list(rInteractionOn=TRUE,
               rInteractionOnly=TRUE,
               showType="EffectSize",
               showTheory=FALSE,
               sigOnly=FALSE,
               shortHand=FALSE,
               ssqType="Type3",
               llr=list(e1=c(),e2=0),
               evidenceCaseOrder="Alphabetic",
               allScatter="all",
               Welch=FALSE,
               dataType="Raw",
               analysisType="Anova",
               pScale="log10",wScale="linear",nScale="log10",
               usePrior="world",
               prior=list(worldOn=FALSE,populationPDF="",
                          populationPDFk=0,populationRZ="r",
                          populationNullp=0)
)

result<-c()

metaAnalysis<-list(nstudies=100,
                   meta_fixedAnal="random",
                   meta_pdf="All",
                   sig_only=FALSE,
                   meta_psigAnal=FALSE,
                   meta_nullAnal=TRUE,
                   nsims=1,
                   meta_showAnal="All",
                   meta_showParams="n-k",
                   showTheory=TRUE,
                   append=FALSE
)

explore<-list(Explore_type="IV",
              Explore_show="EffectSize", 
              Explore_typeShow="direct", 
              Explore_whichShow="Main 1", 
              Explore_length=10,
              Append=FALSE,
              Explore_npoints=13,Explore_xlog = FALSE,
              Explore_quants=0.95,
              Explore_esRange=0.8,Explore_nRange=250,
              Explore_metaRange=10000,Explore_Mxlog = TRUE,Explore_nrRange=250,
              ExploreFull_ylim=FALSE,
              ExploreTheory=TRUE,
              Explore_family="Hypothesis"  
)

likelihood<-
  list(type=c(),
       UsePrior="none", UseSource="world",
       prior=list(worldOn=TRUE,populationPDF="Uniform",populationPDFk=0.2,populationRZ="r",populationNullp=0),
       world=effect$world,
       design=list(sampleN=design$sN,sampleNRand=design$sNRand,sampleNRandK=design$sNRandK),
       targetSample=c(),targetPopulation=0,
       cutaway=FALSE,
       sigOnly=FALSE,
       ResultHistory=c(),
       likelihoodTheory=TRUE,
       likelihoodSimSlice=0.1,likelihoodCorrection=TRUE,
       likelihoodHQ=FALSE,
       appendSim=FALSE,Likelihood_length="10",
       view="3D",azimuth=50,elevation=5,range=2,
       textResult=FALSE
  )


exploreResultHold<-list(Hypothesis=c(),Design=c(),MetaAnalysis=c())
likelihood_P_ResultHold<-c()
likelihood_S_ResultHold<-c()

oldEffect<-effect
oldDesign<-design
oldEvidence<-evidence
oldMetaAnalysis<-metaAnalysis
oldLikelihood<-likelihood

importedData<-c()
lastSample<-c()
ResultHistory<-c()
oldWorld_distr_k<-0.2

##########################
# NHST constants
alpha<-0.05
alphaLLR<-0.5*qnorm(1-alpha/2)^2
STMethod<-"NHST"
lrRange<-20

#########################
# display choices

RZ<-"r"
showPossible<-"Samples"
shortHand<-FALSE      # instead of making full samples
shortHandGain=10

z_range<-1.5
r_range<-0.975
fullRange<-3
nNpoints<-51
worldNPoints<-51
varNPoints<-101

allScatter<-"all"
minN<-10
maxRandN<-5 # times mean sample size

points_threshold=50 # for displaying expected results
wPlotScale="log10"
pPlotScale="log10"
nPlotScale="log10"

useSignificanceCols<-TRUE
showInteractionOnly<-TRUE

includeSingle<-FALSE  # in "All" meta-analysis

# pSigLabel<-"p(p<.05)"
alphaChar<-'\u03B1'

# Pchar<-'\u03A9'
Pchar<-"P" 
Zchar<-"Z"
Lchar<-'\u03BB'

pPlus<-FALSE

source("Notation.R")
useLabels<-list(psig="psig",UD="D",P="0")
setNotation(useLabels)
if (pPlus) effect$world$populationNullp<<-1-effect$world$populationNullp

worldsList<-list("pdf"="PDF","k"="k","pNull"="pNull")
names(worldsList)[3]<-pPlusLabel
names(worldsList)[2]<-Lchar

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

###########################################
# fine tuning
is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  switches$doClipboard<-TRUE
  
  if (Sys.getenv("USERNAME")=="rjwatt42" || Sys.info()["user"]=="rogerwatt") {
    switches$loadExtrasValue<-TRUE
  } 
}

