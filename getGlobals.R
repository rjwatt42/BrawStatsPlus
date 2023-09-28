getGlobals<-function() {

  usingShiny<<-TRUE

################################
# graph design

char3D<<-1.3
labelSize<<-4

graphcoloursBL<<-list(graphC="#BFECFF",graphBack="#999999")
graphcoloursBW<<-list(graphC="#FFFFFF",graphBack="#999999")
graphcolours<<-graphcoloursBL

plotcolours<<-list(maineffectES="#FFCC00",covariationES="#FF1100",interactionES="#0011FF",
                  sampleC="#FFCC00",descriptionC="#FF9955",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#11CC00",infer_nsigC="#FF4400",infer_none="#AAAAAA",
                  infer_sigNonNull="#11CC00",infer_isigNonNull="#881100",infer_nsNonNull="#881100",infer_nsdNonNull="#CCCCCC",
                  infer_sigNull="#118800",infer_isigNull="#FF4400",infer_nsNull="#FF4400",infer_nsdNull="#CCCCCC",
                  psig="#FFAA00",alpha="#44FF22",one="#FF4422",
                  fdr="#227700",fmr="#BB5555")

shapes<<-list(data=21,study=22,parameter=21,meta=24)

report_precision<<-3
graph_precision<<-2

# graph themes
mainTheme=theme(panel.background = element_rect(fill=graphcolours$graphBack, colour="black"),
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


################################
# starting values for important variables

effect<<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
             world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.2,populationRZ="r",populationNullp=0,worldAbs=FALSE)
)

nulleffect<<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
                 world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.0,populationRZ="r",populationNullp=0)
)

design<<-list(sN=42, sNRand=FALSE,sNRandK=2, 
             sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sDependence=0, sOutliers=0, sClustering=0,
             sCheating=FALSE,sCheatingLimit="Budget",sCheatingAmount=5,sCheatingBudget=1000,
             sBudgetOn=FALSE,sNBudget=1000,
             sReplicationOn=FALSE,sReplPowerOn=TRUE,sReplPower=0.8,sReplTails=2,sReplType="Fixed",
             sReplSigOnly="No",sReplRepeats=1,sReplKeep="last",sReplBudget=1000,
             sReplCorrection="None",
             sReplVarAlpha=FALSE,sReplAlpha=2,
             sN_Strata=5, sR_Strata=2,
             sNClu_Cluster=5,     sRClu_Cluster=0.7,
             sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
             sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
)    

evidence<<-list(rInteractionOn=TRUE,
               rInteractionOnly=TRUE,
               showType="direct",
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

result<<-c()

metaAnalysis<<-list(nstudies=100,
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

explore<<-list(Explore_type="IV",
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
              ExploreAny_ylim=FALSE,
              ExploreTheory=TRUE,
              Explore_family="Hypothesis"  
)

possible<<-
  list(type=c(),
       UsePrior="none", UseSource="world",
       prior=list(worldOn=TRUE,populationPDF="Uniform",populationPDFk=0.2,populationRZ="r",populationNullp=0),
       world=effect$world,
       design=list(sampleN=design$sN,sampleNRand=design$sNRand,sampleNRandK=design$sNRandK),
       targetSample=c(),targetPopulation=0,
       cutaway=FALSE,
       sigOnly=FALSE,
       ResultHistory=c(),
       possibleTheory=TRUE,
       possibleSimSlice=0.1,possibleCorrection=TRUE,
       possibleHQ=FALSE,
       appendSim=FALSE,possibleLength="10",
       view="3D",show="Normal",azimuth=50,elevation=5,range=2,boxed=FALSE,
       textResult=FALSE
  )


exploreResultHold<<-list(Hypothesis=c(),Design=c(),MetaAnalysis=c())
possiblePResultHold<<-c()
possibleSResultHold<<-c()

oldEffect<<-effect
oldDesign<<-design
oldEvidence<<-evidence
oldMetaAnalysis<<-metaAnalysis
oldPossible<<-possible

importedData<<-c()
lastSample<<-c()
ResultHistory<<-c()
oldWorld_distr_k<<-0.2

##########################
# NHST constants
alphaSig<<-0.05
alphaLLR<<-0.5*qnorm(1-alphaSig/2)^2
STMethod<<-"NHST"
lrRange<<-10

#########################
# display choices

RZ<<-"r"

z_range<<-1.5
r_range<<-0.975
w_range<<-c(0.05,0.99)
w_range<<-c(0.05,1)
fullRange<<-3
nNpoints<<-51
worldNPoints<<-51
varNPoints<<-101

allScatter<<-"all"
showMedians<<-FALSE
minN<<-10
maxRandN<<-5 # times mean sample size
reportGroupMeans<<-TRUE
plotDescriptionCols<<-c()
CatCatcols<<-c()
doLegendBars<<-TRUE
simData<<-TRUE

points_threshold<<-50 # for displaying expected results
wPlotScale<<-"log10"
pPlotScale<<-"log10"
nPlotScale<<-"log10"

useSignificanceCols<<-TRUE
showInteractionOnly<<-TRUE

includeSingle<<-FALSE  # in "All" meta-analysis

alphaChar<<-'\u03B1'

##################################
# notation for worlds

source("Notation.R")

# Pchar<-'\u03A9'
Pchar<<-"P" 
Zchar<<-"Z"
Lchar<<-'\u03BB'

useLabels<-list(psig="psig",UD="D",P="0")

switch(useLabels$psig,
       "psig"={pSigLabel<<-bquote(bold(p[.('sig')]))},
       "w"={pSigLabel<<-bquote(bold(w))}
)

LabelUD<<-useLabels$UD

posChar<<-"+"
nullChar<<-"0"
switch(useLabels$P,
       "+"={
         pPlus<<-TRUE
       },
       "0"={
         pPlus<<-FALSE
       },
       "-"={
         pPlus<<-FALSE
         nullChar<<-'\u2013'
       }
)


if (pPlus) {
  Ptypechar<-posChar 
} else {
  Ptypechar<-nullChar
}
Ltypechar<-posChar

pPlusLabel<<-paste0("P(",Ptypechar,")")

switch (LabelUD, 
        "U"={
          Plabel<<-bquote(bold(.(Pchar)^.(Ptypechar)))
          Llabel<<-bquote(bold(.(Lchar)^.(Ltypechar)))
          
          nonNullPositive<<-bquote(.(Zchar)^.(posChar)~'+sig')  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)^.(posChar) ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)^.(posChar) ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)^.(nullChar) ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)^.(nullChar) ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)^.(nullChar) ~"-sig")  # "Z0 -ve"
        },
        "D"={
          Plabel<<-bquote(bold(.(Pchar)[.(Ptypechar)]))
          Llabel<<-bquote(bold(.(Lchar)[.(Ltypechar)]))
          
          nonNullPositive<<-bquote(.(Zchar)[.(posChar)] ~"+sig")  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)[.(posChar)] ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)[.(posChar)] ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)[.(nullChar)] ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)[.(nullChar)] ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)[.(nullChar)] ~"-sig")  # "Z0 -ve"
        }
)
allPositive<<-bquote(.(Zchar) ~"+ve")
allNegative<<-bquote(.(Zchar) ~"ns")

if (pPlus) effect$world$populationNullp<<-1-effect$world$populationNullp

}
