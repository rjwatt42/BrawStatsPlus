##################################################################################    
# LIKELIHOOD
# UI changes
# set ui variable from UI
# calculations
# outputs
#    

likelihoodResult<-list(samples=c(),populations=c())
# UI changes
# if the tabs are selected
likelihoodUpdateTabs<-observeEvent(input$Likelihood,{
  if (input$Likelihood=="Samples" || input$Likelihood=="Populations")
  {
    updateTabsetPanel(session, "Graphs",
                      selected = "Possible")
    updateTabsetPanel(session, "Reports",
                      selected = "Possible")
    showPossible<<-input$Likelihood
  }
},priority=100)
# if the tabs are selected
likelihoodUpdateTabs<-observeEvent(input$LGshowPossible,{
  if (input$LGshowPossible=="Samples" || input$LGshowPossible=="Populations")
  {
    showPossible<<-input$LGshowPossible
  }
},priority=100)

# update samples from populations and vice versa
# we check to make sure that we are only copying from the current tab to the hidden one
observeEvent(c(input$likelihoodPSampRho),
             {
               if (input$Likelihood=="Populations") {
                 updateNumericInput(session,"likelihoodSampRho",value=input$likelihoodPSampRho)
               }
             })
observeEvent(c(input$likelihoodSampRho),
             {
               if (input$Likelihood=="Samples") {
                 updateNumericInput(session,"likelihoodPSampRho",value=input$likelihoodSampRho)
               }
             })



# set likelihood variable from UI 
updateLikelihood<-function(){
  IV<-updateIV()
  DV<-updateDV()
  effect<-updateEffect()
  
  if (switches$doWorlds) {
    world<-list(worldOn=input$world_on,populationPDF=input$world_distr,populationRZ=input$world_distr_rz, 
                populationPDFk=input$world_distr_k,
                populationNullp=input$world_distr_Nullp
    )
  } else {
    world<-list(worldOn=FALSE,populationPDF="Single",populationRZ="R", 
                populationPDFk=effect$rIV,
                populationNullp=0
    )     
  }
  
  # if (graphicSource=="Modal") {
  #           switch (showPossible,
  #                   "Populations"={
  #                     likelihood<-
  #                       list(type=showPossible,
  #                            Use=input$LGlikelihoodUsePrior,
  #                            prior=list(populationPDF=input$LGlikelihoodPrior_distr,populationRZ=input$LGlikelihoodPrior_distr_rz, 
  #                                       populationPDFk=input$LGlikelihoodPrior_distr_k,
  #                                       populationNullp=input$LGlikelihoodPrior_Nullp
  #                            ),
  #                            world=list(populationPDF=input$LGlikelihoodworld_distr,populationRZ=input$LGlikelihoodworld_distr_rz, 
  #                                       populationPDFk=input$LGlikelihoodworld_distr_k,
  #                                       populationNullp=input$LGlikelihoodworld_distr_Nullp
  #                            ),
  #                            design=list(sampleN=input$LGlikelihoodsN,sampleNRand=input$LGlikelihoodsNRand,sampleNRandK=input$LGlikelihoodsNRandK),
  #                          targetSample=input$LGlikelihoodPSampRho,targetPopulation=effect$rIV,
  #                          ResultHistory=ResultHistory,
  #                          likelihoodTheory=input$LGlikelihoodTheory,likelihoodLongHand=input$LGlikelihoodLongHand,likelihoodSimSlice=input$LGlikelihoodSimSlice,likelihoodCorrection=input$LGlikelihoodCorrection,
  #                          appendSim=input$LGlikelihoodP_append,Likelihood_length=as.numeric(input$LGlikelihoodP_length),
  #                          view=input$LGlikelihoodView,azimuth=input$LGlikelihoodAzimuth,elevation=input$LGlikelihoodElevation,range=input$LGlikelihoodRange,
  #                          textResult=TRUE
  #                     )
  #                   },
  #                   "Samples"={
  #                     likelihood<-
  #                       list(type=showPossible,
  #                            Use=input$LGlikelihoodUsePrior,
  #                            prior=list(populationPDF=input$LGlikelihoodPrior_distr,populationRZ=input$LGlikelihoodPrior_distr_rz, 
  #                                       populationPDFk=input$LGlikelihoodPrior_distr_k,
  #                                       populationNullp=input$LGlikelihoodPrior_Nullp
  #                            ),
  #                            world=list(populationPDF=input$LGlikelihoodworld_distr,populationRZ=input$LGlikelihoodworld_distr_rz, 
  #                                       populationPDFk=input$LGlikelihoodworld_distr_k,
  #                                       populationNullp=input$LGlikelihoodworld_distr_Nullp
  #                            ),
  #                            design=list(sampleN=input$LGlikelihoodsN,sampleNRand=input$LGlikelihoodsNRand,sampleNRandK=input$LGlikelihoodsNRandK),
  #                            targetSample=input$LGlikelihoodSampRho,cutaway=input$LGlikelihood_cutaway,targetPopulation=input$LGlikelihoodPrior_distr_k,
  #                            ResultHistory=ResultHistory,
  #                            likelihoodTheory=input$LGlikelihoodTheory,likelihoodLongHand=input$LGlikelihoodLongHand,likelihoodSimSlice=input$LGlikelihoodSimSlice,likelihoodCorrection=input$LGlikelihoodCorrection,
  #                          appendSim=input$LGlikelihood_append,Likelihood_length=as.numeric(input$LGlikelihood_length),
  #                          view=input$LGlikelihoodView,azimuth=input$LGlikelihoodAzimuth,elevation=input$LGlikelihoodElevation,range=input$LGlikelihoodRange,
  #                          textResult=TRUE
  #                     )
  #                   }
  #           )
  #   
  #         } else {
  switch (showPossible,
          "Populations"={
            likelihood<-
              list(type=showPossible,
                   UsePrior=input$likelihoodUsePrior,
                   UseSource=input$likelihoodUseSource,
                   prior=list(worldOn=TRUE,populationPDF=input$likelihoodPrior_distr,populationRZ=input$likelihoodPrior_distr_rz, 
                              populationPDFk=input$likelihoodPrior_distr_k,
                              populationNullp=input$likelihoodPrior_Nullp
                   ),
                   world=world,
                   design=list(sampleN=input$sN,sampleNRand=input$sNRand,sampleNRandK=input$sNRandK),
                   targetSample=input$likelihoodPSampRho,targetPopulation=effect$rIV,
                   ResultHistory=ResultHistory,
                   sigOnly=input$likelihood_sigonly,
                   likelihoodTheory=input$likelihoodTheory,likelihoodLongHand=input$likelihoodLongHand,
                   likelihoodSimSlice=input$likelihoodSimSlice,likelihoodCorrection=input$likelihoodCorrection,
                   appendSim=input$likelihoodP_append,Likelihood_length=as.numeric(input$likelihoodP_length),
                   view=input$LikelihoodView,viewRZ=input$likelihoodViewRZ,azimuth=input$LikelihoodAzimuth,elevation=input$LikelihoodElevation,range=input$LikelihoodRange,
                   textResult=FALSE
              )
          },
          "Samples"={
            likelihood<-
              list(type=showPossible,
                   UsePrior=input$likelihoodUsePrior,
                   UseSource=input$likelihoodUseSource,
                   prior=list(worldOn=TRUE,populationPDF=input$likelihoodPrior_distr,populationRZ=input$likelihoodPrior_distr_rz, populationPDFk=input$likelihoodPrior_distr_k,
                              populationNullp=input$likelihoodPrior_Nullp),
                   world=world,
                   design=list(sampleN=input$sN,sampleNRand=input$sNRand,sampleNRandK=input$sNRandK),
                   targetSample=input$likelihoodSampRho,targetPopulation=effect$world$populationPDFk,
                   cutaway=input$likelihood_cutaway,
                   sigOnly=input$likelihood_sigonly,
                   ResultHistory=ResultHistory,
                   likelihoodTheory=input$likelihoodTheory,likelihoodLongHand=input$likelihoodLongHand,likelihoodSimSlice=input$likelihoodSimSlice,likelihoodCorrection=input$likelihoodCorrection,
                   appendSim=input$likelihood_append,Likelihood_length=as.numeric(input$likelihood_length),
                   view=input$LikelihoodView,viewRZ=input$likelihoodViewRZ,azimuth=input$LikelihoodAzimuth,elevation=input$LikelihoodElevation,range=input$LikelihoodRange,
                   textResult=FALSE
              )
          }
  )
  # }
  if (likelihood$world$worldOn==FALSE) {
    likelihood$world$populationPDF<-"Single"
    likelihood$world$populationRZ<-"r"
    likelihood$world$populationPDFk<-effect$rIV
    likelihood$world$populationNullp<-0
  }
  if (is.null(oldLikelihood)) {
    likelihood$world$populationPDFk<-checkNumber(likelihood$world$populationPDFk)
    likelihood$prior$populationPDFk<-checkNumber(likelihood$prior$populationPDFk)
    likelihood$world$populationNullp<-checkNumber(likelihood$world$populationNullp)
    likelihood$prior$populationNullp<-checkNumber(likelihood$prior$populationNullp)
    
    likelihood$design$sampleNRandK<-checkNumber(likelihood$design$sampleNRandK)
  } else {
    likelihood$world$populationPDFk<-checkNumber(likelihood$world$populationPDFk,oldLikelihood$world$populationPDFk)
    likelihood$prior$populationPDFk<-checkNumber(likelihood$prior$populationPDFk,oldLikelihood$prior$populationPDFk)
    likelihood$world$populationNullp<-checkNumber(likelihood$world$populationNullp,oldLikelihood$world$populationNullp)
    likelihood$prior$populationNullp<-checkNumber(likelihood$prior$populationNullp,oldLikelihood$prior$populationNullp)
    
    likelihood$design$sampleNRandK<-checkNumber(oldLikelihood$design$sampleNRandK)
  }
  oldLikelihood<<-likelihood
  likelihood
}

# main likelihood calcuations    
likelihoodReset<-observeEvent(c(input$likelihoodPrior_Nullp,
                                input$likelihoodPrior_distr,input$likelihoodPrior_distr_rz,input$likelihoodPrior_distr_k,
                                input$likelihoodUsePrior,
                                input$sN,
                                input$likelihoodLongHand,
                                input$LGlikelihoodPrior_Nullp,
                                input$LGlikelihoodPrior_distr,input$LGlikelihoodPrior_distr_rz,input$LGlikelihoodPrior_distr_k,input$LGlikelihoodUsePrior,
                                input$LGlikelihoodsN,
                                input$LGlikelihoodLongHand
),{
  likelihood_P_ResultHold<<-c()
  likelihood_S_ResultHold<<-c()
})

likelihoodAnalysis<-eventReactive(c(input$Likelihood,
                                    input$likelihood_run,input$likelihoodP_run,
                                    input$likelihoodPSampRho,
                                    input$likelihoodUsePrior,input$likelihoodUseSource,
                                    input$likelihoodPrior_distr,input$likelihoodPrior_distr_rz,input$likelihoodPrior_distr_k,input$likelihoodPrior_Nullp,
                                    input$rIV,
                                    input$world_on,input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
                                    input$sN,input$sNRand,input$sNRandK,
                                    input$EvidencenewSample,
                                    input$likelihoodTheory,input$likelihood_sigonly,
                                    input$likelihoodLongHand,input$likelihoodSimSlice,input$likelihoodCorrection,
                                    input$likelihoodViewRZ,
                                    input$LGshowPossible,
                                    input$LGlikelihood_run,input$LGlikelihoodP_run,
                                    input$LGlikelihoodSampRho,input$LGlikelihoodPSampRho,
                                    input$LGlikelihoodUsePrior,input$LGlikelihoodUseSource,
                                    input$LGlikelihoodPrior_distr,input$LGlikelihoodPrior_distr_rz,input$LGlikelihoodPrior_distr_k,input$LGlikelihoodPrior_Nullp,
                                    input$LGlikelihoodworld_on,input$LGlikelihoodworld_distr,input$LGlikelihoodworld_distr_rz,input$LGlikelihoodworld_distr_k,input$LGlikelihoodworld_distr_Nullp,
                                    input$LGlikelihoodsN,input$LGlikelihoodsNRand,input$LGlikelihoodsNRandK,
                                    input$LGlikelihoodTheory,input$LGlikelihood_sigonly,
                                    input$LGlikelihoodLongHand,input$LGlikelihoodSimSlice,input$LGlikelihoodCorrection
),{
  if (graphicSource=="None") {return(likelihoodResult)}
  
  req(input$changed)
  
  if (is.element(input$changed,c("Likelihood","likelihood_run","likelihoodP_run","likelihoodPSampRho",
                                 "world_on","world_distr","world_distr_rz","world_distr_k","world_distr_Nullp",
                                 "Prior_distr","Prior_distr_rz","Prior_distr_k","Prior_Nullp",
                                 "likelihoodTheory","LGlikelihoodSimSlice","LGlikelihoodCorrection",
                                 "sN","sNRand","sNRandK")))
  {
    graphicSource<<-"Main"
    showPossible<-input$Likelihood
  }
  if (is.element(input$changed,c("LGshowPossible",
                                 "LGlikelihood_run","LGlikelihood_run","LGlikelihoodSampRho",
                                 "LGlikelihoodworld_on","LGlikelihoodworld_distr","LGlikelihoodworld_distr_rz","LGlikelihoodworld_distr_k","LGlikelihoodworld_distr_Nullp",
                                 "LGlikelihoodPrior_distr","LGlikelihoodPrior_distr_rz","LGlikelihoodPrior_distr_k","LGlikelihoodPrior_Nullp",
                                 "LGlikelihoodTheory","LGlikelihoodSimSlice","LGlikelihoodCorrection",
                                 "LGlikelihoodsN","LGlikelihoodsNRand","LGlikelihoodsNRandK")))
  {
    graphicSource<<-"Modal"
    showPossible<-input$LGshowPossible
  }
  IV<-updateIV()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  result<-sampleAnalysis()
  likelihood<-updateLikelihood()
  
  if ((input$likelihood_run+input$likelihoodP_run+input$LGlikelihood_run+input$LGlikelihoodP_run>validLikelihood)){
    showNotification(paste0("Possible ",likelihood$type," : starting"),id="counting",duration=Inf,closeButton=FALSE,type="message")
    validLikelihood<<-validLikelihood+1
    likelihoodRes<-likelihood_run(IV,DV,effect,design,evidence,likelihood,doSample = TRUE)
    removeNotification(id="counting")
    keepSamples<-FALSE
  } else {
    likelihoodRes<-likelihood_run(IV,DV,effect,design,evidence,likelihood,doSample = FALSE)
    keepSamples<-all(unlist(lapply(seq(3),function(i)likelihoodRes$likelihood$world[[i]]==likelihoodResult$samples$likelihood$world[[i]])))
    keepSamples<-keepSamples && all(unlist(lapply(seq(3),function(i)likelihoodRes$likelihood$design[[i]]==likelihoodResult$populations$likelihood$design[[i]])))
  }
  
  switch (showPossible,
          "Samples"={          
            # if (keepSamples && !is.null(likelihoodResult$samples$Sims$sSims)) {
            # likelihoodRes$Sims<-likelihoodResult$samples$Sims
            # }
            likelihoodResult$samples<<-likelihoodRes
            likelihood_S_ResultHold<<-list(sSims=likelihoodResult$samples$Sims$sSims,sSimBins=likelihoodResult$samples$Sims$sSimBins,sSimDens=likelihoodResult$samples$Sims$sSimDens)
          },
          "Populations"={
            # if (keepSamples && !is.null(likelihoodResult$populations$Sims$pSims)) {
            #   likelihoodRes$Sims<-likelihoodResult$populations$Sims
            # }
            likelihoodResult$populations<<-likelihoodRes
            # likelihood_P_ResultHold<<-list(pSims=likelihoodResult$populations$Sims$pSimsP,sSims=likelihoodResult$populations$Sims$pSims)
          }
  )
  likelihoodResult
}
)

makeLikelihoodGraph<-function(){
  if (!is.element(showPossible,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
  IV<-updateIV()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  
  # this guarantees that we update without recalculating if possible
  likelihood<-updateLikelihood()
  doIt<-c(ResultHistory)
  
  likelihoodResult<-likelihoodAnalysis()
  
  drawLikelihood(IV,DV,effect,design,likelihood,likelihoodResult)
}

# likelihood outputs    
# show likelihood analysis        
output$LikelihoodPlot <- renderPlot( {
  LKtype<-c(input$Likelihood,input$EvidencenewSample)
  par(cex=1.2)
  makeLikelihoodGraph()
})

# report likelihood analysis        
output$LikelihoodReport <- renderPlot({
  if (!is.element(showPossible,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
  IV<-updateIV()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  
  likelihood<-updateLikelihood()
  likelihoodResult<-likelihoodAnalysis()
  
  reportLikelihood(Iv,DV,effect,design,likelihood,likelihoodResult)
  
})

##################################################################################    

