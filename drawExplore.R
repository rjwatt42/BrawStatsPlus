no_se_multiple<-TRUE
multiOverlap<-FALSE
valsGap<-1.4
ErrorsWorld<-"1scale"
# ErrorsWorld<-"2scale"
all_cols<-c()

drawExplore<-function(IV,IV2,DV,effect,design,explore,exploreResult){
  oldAlpha<-alpha
  rho<-effect$rIV
  
  vals<-exploreResult$result$vals
  if (is.character(vals[1])){
    vals<-((1:length(vals))-1)/(length(vals)-1)
    doLine=FALSE
  } else {doLine=TRUE}
  
  g<-ggplot()
  ybreaks=c()
  ylabels=c()
  switch (explore$Explore_show,
          "EffectSize"={
            ylim<-c(-1,1)
            g<-g+scale_y_continuous(limits=ylim)
            ylabel<-bquote(r[sample])
          },
          "p"={
            ylim<-c(-4,0)
            ylabel<-bquote(log[10](p))
            ybreaks<-c(-4,-3,-2,-1,0)
            ylabels<-c(0.0001,0.001,0.01,0.1,1)
            g<-g+scale_y_continuous(limits=ylim,breaks=ybreaks,labels=ylabels)
          },
          "w"={
            if (wPlotScale=="log10"){
              ylim<-c(-2,0)
              ylabel<-bquote(log[10](w[est]))
              ybreaks=c(-2,-1,0)
              ylabels=c(0.01,0.1,1)
              g<-g+scale_y_continuous(limits=ylim,breaks=ybreaks,labels=ylabels)
            } else {
              ylim<-c(0,1)
              ylabel<-bquote(w[est])
              g<-g+scale_y_continuous(limits=ylim)
            }
          },
          "p(sig)"={
            ylabel<-"p(sig)"
            ylim<-c(0,1)
            g<-g+scale_y_continuous(limits=ylim)
          },
          "NHSTErrors"={
            ylim<-c(0,1)
            if (ErrorsWorld=="1scale") {
              ylabel<-"Results"
            } else {
            ylabel<-"Type I"
            g<-g+scale_y_continuous(limits=ylim,sec.axis=sec_axis(~ 1-.,name="Type II"))
            g<-g+theme(axis.title.y.left = element_text(color="darkgreen"),axis.title.y.right = element_text(color="darkred"))
            }
          },
          "p(samp)"={
            ylabel<-paste0("p(r_s=",format(0.123,digits=3),")")
          },
          "FDR"={
            ylim<-c(0,1)
            ylabel<-"False Discovery"
            g<-g+scale_y_continuous(limits=ylim,sec.axis=sec_axis(~ 1-.,name="False Miss"))
            g<-g+theme(axis.title.y.left = element_text(color="darkgreen"),axis.title.y.right = element_text(color="darkred"))
          },
          "log(lrs)"={
            ylim<-c(-0.1,10)
            ylabel<-bquote(log[e](lr[s]))
            g<-g+scale_y_continuous(limits=ylim)
          },
          "log(lrd)"={
            ylim<-c(-1,1)*lrRange
            ylabel<-bquote(log[e](lr[d]))
            g<-g+scale_y_continuous(limits=ylim)
          },
          "likelihood"={
            ylim<-c(-10,10)
            ylabel<-bquote(log[e](lr[d]))
            g<-g+scale_y_continuous(limits=ylim)
          },
          "k"={
            ylim<-c(-0.01,1.01)
            ylabel<-"k"
            g<-g+scale_y_continuous(limits=ylim)
          },
          "SampleSize"={
            ylim<-c(minN,maxRandN*design$sN)
            ylabel<-"n"
            g<-g+scale_y_continuous(limits=ylim)
          },
          "pNull"={
            ylim<-c(-0.01,1.01)
            ylabel<-"pNull"
            g<-g+scale_y_continuous(limits=ylim)
          },
          "PDF"={
            ylim<-c(0,1)
            ylabel<-"p(PDF)"
            g<-g+scale_y_continuous(limits=ylim)
          },
          "S"={
            ylim<-c(min(exploreResult$result$Ss),max(exploreResult$result$Ss))
            ylabel<-"S"
            g<-g+scale_y_continuous(limits=ylim)
          },
          "mean(IV)"={
            ylabel<-"mean(IV)"
            ylim<-c(-1,1)
            },
          "sd(IV)"={ylabel<-"sd(IV)"},
          "skew(IV)"={ylabel<-"skew(IV)"},
          "kurtosis(IV)"={ylabel<-"kurtosis(IV)"},
          "mean(DV)"={ylabel<-"mean(DV)"},
          "sd(DV)"={ylabel<-"sd(DV)"},
          "skew(DV)"={ylabel<-"skew(DV)"},
          "kurtosis(DV)"={ylabel<-"kurtosis(DV)"}
  )

  if (!is.null(IV2) && is.element(explore$Explore_show,c("EffectSize","p","w","p(sig)"))) {
    switch (explore$Explore_show,
            "EffectSize"={use_cols<<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "p"=         {use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))},
            "w"=         {use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))},
            "p(sig)"=    {use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")},
    )
    names(use_cols)<-c("direct","unique","total")
    all_cols<<-use_cols
    g<-g+scale_fill_manual(name=explore$Explore_whichShow,values=all_cols)
    use_col_names<-TRUE
  } else {
    all_cols<-c()
    use_col_names<-FALSE
  }
  
  markersize<-7
  ni_max1<-1
  ni_max2<-1
  multi="none"
  if (explore$Explore_typeShow=="all") { # all of direct/unique/total
    markersize<-4
    ni_max1<-3
    multi<-"allTypes"
  } 
  if (explore$Explore_whichShow=="All") { # all of main1 main2 interaction
    markersize<-4
    ni_max2<-3
    multi<-"allEffects"
  } else {
    if (explore$Explore_whichShow=="Mains") { 
      markersize<-6
      ni_max2<-2
      multi<-"mainEffects"
    }
  }
  
  for (ni1 in 1:ni_max1){
    for (ni2 in 1:ni_max2){
      if (ni_max1>1) {
      switch (ni1,
            {explore$Explore_typeShow<-"direct"},
            {explore$Explore_typeShow<-"unique"},
            {explore$Explore_typeShow<-"total"})
      } 
      if (ni_max2>1) {
      switch (ni2,
              {explore$Explore_whichShow<-"Main 1"},
              {explore$Explore_whichShow<-"Main 2"},
              {explore$Explore_whichShow<-"Interaction"})
      }

    extra_y_label<-""
    if (is.null(IV2)){
      rVals<-exploreResult$result$rIVs
      pVals<-exploreResult$result$pIVs
    } else {
      switch (explore$Explore_whichShow,
              "Main 1"={
                rVals<-exploreResult$result$r1[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p1[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 1:",explore$Explore_typeShow)
              },
              "Main 2"={
                rVals<-exploreResult$result$r2[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p2[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 2:",explore$Explore_typeShow)
              },
              "Interaction"={
                rVals<-exploreResult$result$r3[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p3[[explore$Explore_typeShow]]
                extra_y_label<-paste("Interaction:",explore$Explore_typeShow)
              }
      )
    }
    nVals<-exploreResult$result$nvals
    
    extra_x_label<-""
    switch (explore$Explore_show,
            "EffectSize"={
              showVals<-rVals
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                        }
                )
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p"={
              showVals<-pVals
              lines<-c(0.05)
              if (pPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"red"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "w"={
              showVals<-rn2w(rVals,exploreResult$result$nvals)
              lines<-c(0.05,0.8)
              if (wPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "SampleSize"={
              showVals<-exploreResult$result$nvals
              lines<-c(design$sN)
              # if (nPlotScale=="log10"){
              #   showVals<-log10(showVals)
              #   lines<-log10(lines)
              # }
              if (is.null(IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p(sig)"={
              y50<-c()
              y25<-c()
              y75<-c()
              y62<-c()
              y38<-c()
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  alpha<<-exploreResult$result$vals[i]
                  alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                }
                p<-mean(isSignificant(STMethod,pVals[,i],rVals[,i],nVals[,i],exploreResult$evidence),na.rm=TRUE)
                p_se<-sqrt(p*(1-p)/length(pVals[,i]))
                y50[i]<-p
                y75[i]<-p+p_se*qnorm(0.75)
                y25[i]<-p+p_se*qnorm(0.25)
                y62[i]<-p+p_se*qnorm(0.625)
                y38[i]<-p+p_se*qnorm(0.375)
              }
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "NHSTErrors"={
              yall<-c()
              y50<-c()
              y25<-c()
              y75<-c()
              y25a<-c()
              y50a<-c()
              y75a<-c()
              y25b<-c()
              y50b<-c()
              y75b<-c()
              yalle<-c()
              y50e<-c()
              y25e<-c()
              y75e<-c()
              y25ea<-c()
              y50ea<-c()
              y75ea<-c()
              y25eb<-c()
              y50eb<-c()
              y75eb<-c()
              if (effect$world$worldOn) {
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  sigs<-isSignificant(STMethod,pVals[,i],rVals[,i],nVals[,i],exploreResult$evidence)
                  nulls<-exploreResult$result$rpIVs[,i]==0
                  yall[i]<-sum(!nulls,na.rm=TRUE)/length(sigs)
                  yalle[i]<-sum(nulls,na.rm=TRUE)/length(sigs)
                  # NB because we plot this upside down 
                  if (STMethod=="NHST") {
                    # type II errors
                    p<-sum(!sigs & !nulls,na.rm=TRUE)/length(sigs) 
                    y50[i]<-p
                    y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type I errors
                    p<-sum(sigs[nulls],na.rm=TRUE)/length(sigs)
                    y50e[i]<-p
                    y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  } else {
                    d<-r2llr(rVals[,i],nVals[,i],STMethod,world=effect$world)
                    # type II errors: not-nulls & sig in wrong direction
                    p<-sum(sigs & d<0 & !nulls,na.rm=TRUE)/length(d) 
                    y50[i]<-p
                    y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type II errors: not-nulls & not sig
                    p<-sum(!sigs & !nulls,na.rm=TRUE)/length(d) 
                    y50a[i]<-p
                    y75a[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25a[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type II errors: not-nulls & not sig
                    p<-sum(sigs & d>0 & !nulls,na.rm=TRUE)/length(d) 
                    y50b[i]<-p
                    y75b[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25b[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type I errors: nulls & sig in wrong direction
                    p<-sum(sigs & d>0 & nulls,na.rm=TRUE)/length(d)
                    y50e[i]<-p
                    y75e[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25e[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type I errors: nulls & not sig
                    p<-sum(!sigs & nulls,na.rm=TRUE)/length(d)
                    y50ea[i]<-p
                    y75ea[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25ea[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                    # type I errors: nulls & not sig
                    p<-sum(sigs & d<0 & nulls,na.rm=TRUE)/length(d)
                    y50eb[i]<-p
                    y75eb[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                    y25eb[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  }
                }
              } else {
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  p<-mean(!isSignificant(STMethod,pVals[,i],rVals[,i],nVals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50[i]<-p
                  y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                  yall[i]<-0.5
                  yalle[i]<-0.5
                }
                
                peVals<-exploreResult$nullresult$pIVs
                reVals<-exploreResult$nullresult$rIVs
                neVals<-exploreResult$nullresult$nvals
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  p<-mean(isSignificant(STMethod,peVals[,i],reVals[,i],neVals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50e[i]<-p
                  y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                  y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
                }
              }
              col<-plotcolours$infer_misserr
              cole<-plotcolours$infer_hiterr
              colFill<-col
              lines<-c(0.05)
            },
            "FDR"={
              y50<-c()
              y25<-c()
              y75<-c()
              y50e<-c()
              y25e<-c()
              y75e<-c()
              if (effect$world$worldOn) {
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  sigs<-isSignificant(STMethod,pVals[,i],rVals[,i],nVals[,i],exploreResult$evidence)
                  nulls<-exploreResult$result$rpIVs[,i]==0
                  if (STMethod=="NHST") {
                  if (!all(nulls)) {
                    p1<-sum(!sigs & !nulls,na.rm=TRUE)/sum(!sigs)
                  } else {
                    p1<-0
                  }
                    p2<-sum(sigs & nulls,na.rm=TRUE)/sum(sigs)
                  } else {
                    d<-r2llr(rVals[,i],nVals[,i],STMethod,world=effect$world)
                    p1<-mean(!sigs,na.rm=TRUE)
                    p2<-(sum(sigs & nulls & d>0)+sum(sigs & !nulls & d<0))/sum(sigs)
                  }
                  y50[i]<-p1
                  y75[i]<-p1+sqrt(p1*(1-p1)/length(pVals[,i]))
                  y25[i]<-p1-sqrt(p1*(1-p1)/length(pVals[,i]))
                  y50e[i]<-p2
                  y75e[i]<-p2+sqrt(p2*(1-p2)/length(pVals[,i]))
                  y25e[i]<-p2-sqrt(p2*(1-p2)/length(pVals[,i]))
                }
              } else {
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  p<-mean(isSignificant(STMethod,pVals[,i],rVals[,i],nVals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50[i]<-p
                  y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                  y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
                }
                
                peVals<-exploreResult$nullresult$pIVs
                reVals<-exploreResult$nullresult$rIVs
                neVals<-exploreResult$nullresult$nvals
                for (i in 1:length(exploreResult$result$vals)){
                  if (explore$Explore_type=="Alpha") {
                    alpha<<-exploreResult$result$vals[i]
                    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                  }
                  p<-mean(isSignificant(STMethod,peVals[,i],reVals[,i],neVals[,i],exploreResult$evidence),na.rm=TRUE)
                  y50e[i]<-p
                  y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                  y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
                }
              }
              col<-"#FF8600"
              cole<-"#33FF99"
              colFill<-col
              lines<-c(0.05)
            },            "log(lrs)"={
              ns<-exploreResult$result$nvals
              showVals<-r2llr(rVals,ns,"sLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                        }
                )
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "log(lrd)"={
              ns<-exploreResult$result$nvals
              showVals<-r2llr(rVals,ns,"dLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                        }
                )
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p(llrs)"={
              y50<-c()
              y25<-c()
              y75<-c()
              y62<-c()
              y38<-c()
              ns<-exploreResult$result$nvals
              showVals<-r2llr(rVals,ns,"sLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  alpha<<-exploreResult$result$vals[i]
                  alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                }
                p<-mean(isSignificant(STMethod,pvals[,i],rvals[,i],nvals[,i],exploreResult$evidence),na.rm=TRUE)
                p_se<-sqrt(p*(1-p)/length(pVals[,i]))
                y50[i]<-p
                y75[i]<-p+p_se*qnorm(0.75)
                y25[i]<-p+p_se*qnorm(0.25)
                y62[i]<-p+p_se*qnorm(0.625)
                y38[i]<-p+p_se*qnorm(0.375)
              }
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p(llrd)"={
              y50<-c()
              y25<-c()
              y75<-c()
              y62<-c()
              y38<-c()
              ns<-exploreResult$result$nvals
              showVals<-r2llr(rVals,ns,"dLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              for (i in 1:length(exploreResult$result$vals)){
                if (explore$Explore_type=="Alpha") {
                  alpha<<-exploreResult$result$vals[i]
                  alphaLLR<<-0.5*qnorm(1-alpha/2)^2
                }
                p<-mean(isSignificant(STMethod,pvals[,i],rvals[,i],nvals[,i],exploreResult$evidence),na.rm=TRUE)
                p_se<-sqrt(p*(1-p)/length(pVals[,i]))
                y50[i]<-p
                y75[i]<-p+p_se*qnorm(0.75)
                y25[i]<-p+p_se*qnorm(0.25)
                y62[i]<-p+p_se*qnorm(0.625)
                y38[i]<-p+p_se*qnorm(0.375)
              }
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "likelihood"={
              showVals<-exploreResult$result$likes
              y50<-showVals
              y75<-showVals
              y25<-showVals
              y62<-showVals
              y38<-showVals
            },
            "k"={
              showVals<-exploreResult$result$ks
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "pNull"={
              showVals<-exploreResult$result$pnulls
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "PDF"={
              showVals<-exploreResult$result$dists
              y50<-c()
              y25<-c()
              y75<-c()
              y62<-c()
              y38<-c()
              ySingle<-c()
              yGauss<-c()
              yExp<-c()
              for (i in 1:length(exploreResult$result$vals)){
                p<-mean(showVals[,i]==effect$world$populationPDF,na.rm=TRUE)
                p_se<-sqrt(p*(1-p)/length(showVals[,i]))
                y50[i]<-p
                y75[i]<-p+p_se*qnorm(0.75)
                y25[i]<-p+p_se*qnorm(0.25)
                y62[i]<-p+p_se*qnorm(0.625)
                y38[i]<-p+p_se*qnorm(0.375)
                
                ySingle<-c(ySingle,mean(showVals[,i]=="Single"))
                yGauss<-c(yGauss,mean(showVals[,i]=="Gauss"))
                yExp<-c(yExp,mean(showVals[,i]=="Exp"))
              }
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "S"={
              showVals<-exploreResult$result$Ss
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "mean(IV)"={
              showVals<-rVals
              col<-"yellow"
                colFill<-col
                lines<-c()
            }
    )
    alpha<<-oldAlpha
    
    if (is.element(explore$Explore_show,c("EffectSize","p","w","SampleSize","log(lrs)","log(lrd)","k","S","pNull","mean(IV)"))) {
      y75<-c()
      y62<-c()
      y50<-c()
      y38<-c()
      y25<-c()
      quants<-explore$Explore_quants/2
      for (i in 1:length(exploreResult$result$vals)) {
        y75[i]<-quantile(showVals[,i],0.50+quants,na.rm=TRUE)
        y62[i]<-quantile(showVals[,i],0.50+quants/2,na.rm=TRUE)
        y50[i]<-quantile(showVals[,i],0.50,na.rm=TRUE)
        y38[i]<-quantile(showVals[,i],0.50-quants/2,na.rm=TRUE)
        y25[i]<-quantile(showVals[,i],0.50-quants,na.rm=TRUE)
      }
    y75[y75>ylim[2]]<-ylim[2]
    y62[y62>ylim[2]]<-ylim[2]
    y38[y38>ylim[2]]<-ylim[2]
    y25[y25>ylim[2]]<-ylim[2]
    
    y75[y75<ylim[1]]<-ylim[1]
    y62[y62<ylim[1]]<-ylim[1]
    y38[y38<ylim[1]]<-ylim[1]
    y25[y25<ylim[1]]<-ylim[1]
    y50[y50<ylim[1]]<-NA
    }

    vals_offset<-0
    valsRange<-1
    if (vals[1]<0) valsRange<-2
    # if (multi=="allTypes") {
    #   vals_offset<-(ni1-1)*(valsRange*valsGap)
    # } 
    if (multi=="allEffects" || multi=="mainEffects") {
      vals_offset<-(ni2-1)*(valsRange*valsGap)
    }

    if (explore$Explore_show=="FDR") {
      pts1<-data.frame(vals=vals+vals_offset,y25=y25,y50=y50,y75=y75)
      pts2<-data.frame(vals=vals+vals_offset,y25e=y25e,y50e=y50e,y75e=y75e)
      lb1<-"!-ve"
      lb2<-"!+ve"
      if (doLine) {
        # shaded fills
        areaVals<-c(vals[1],vals,vals[length(vals)])
        # type 2 errors
        areaData<-1-c(0,pts1$y50,0)
        ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData)
        g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=col)
        g<-g+geom_line(data=pts1,aes(x=vals,y=1-y50),color=col)
        # g<-g+geom_point(data=pts1,aes(x=vals,y=1-y50),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        lb=data=data.frame(x=max(vals),y=mean(c(1,1-pts1$y50[length(vals)])),lb=lb1)
        g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="white",fill=col)
        
        # type 1 errors
        if (!is.null(y50e)) {
          areaData<-c(0,pts2$y50e,0)
          ptsNHSTe<-data.frame(x=areaVals+vals_offset,y=areaData)
          g<-g+geom_polygon(data=ptsNHSTe,aes(x=x,y=y),fill=cole)
          g<-g+geom_line(data=pts2,aes(x=vals,y=y50e),color=cole)
          # g<-g+geom_point(data=pts2,aes(x=vals,y=y50e),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
          lb=data=data.frame(x=max(vals),y=mean(c(0,pts2$y50e[length(vals)])),lb=lb2)
          g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="white",fill=cole)
        }
      } else {
        # shaded fills
        outline<-c(-1,-1,1,1)*0.1
        areaVals<-rep(vals,each=4)+rep(outline,length(vals))
        ids<-rep(1:length(vals),each=4)
        areaData<-1-rep(c(0,1,1,0),length(vals))*rep(1-y50,each=4)
        ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData,ids=ids)
        g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y,group=ids),colour = "black",fill=col,alpha=0.5)
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        
        if (!is.null(y50e)) {
          areaData<-rep(c(0,1,1,0),length(vals))*rep(y50e,each=4)
          ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData,ids=ids)
          g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y,group=ids),colour = "black",fill=cole,alpha=0.5)
          g<-g+geom_point(data=pts2,aes(x=vals,y=y50e),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        }
      }
      }
    if (explore$Explore_show=="NHSTErrors") {
      pts1<-data.frame(vals=vals+vals_offset,y25=y25,y50=y50,y75=y75)
      pts2<-data.frame(vals=vals+vals_offset,y25e=y25e,y50e=y50e,y75e=y75e)
      lb1<-"!-ve"
      lb2<-"!+ve"
      
      # if (STMethod=="dLLR" && explore$Explore_show=="NHSTErrors") {
      #   pts1<-data.frame(vals=vals+vals_offset,y50=y50+y50a,y25=y25,y75=y75)
      #   pts2<-data.frame(vals=vals+vals_offset,y50e=y50e+y50ea,y25e=y25e,y75e=y75e)
      #   
      #   areaVals<-c(vals[1],vals,vals[length(vals)])
      #   areaData<-c(y50b+yalle,rev(yalle))
      #   ptsNHST<-data.frame(x=c(vals,rev(vals))+vals_offset,y=areaData)
      #   g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_sigC)
      #   areaData<-c(yalle,rev(yalle-y50eb))
      #   ptsNHST<-data.frame(x=c(vals,rev(vals))+vals_offset,y=areaData)
      #   g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_nsigC)
      #   
      #   if (ErrorsWorld=="1scale") {
      #     # non-null effects
      #     nAreaVals<-c(areaVals[2:(length(areaVals)-1)],rev(areaVals[2:(length(areaVals)-1)]))
      #     areaData<-c(y50b+y50+yalle,rev(y50b+yalle))
      #     ptsNHST<-data.frame(x=nAreaVals+vals_offset,y=areaData)
      #     g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_hiterr)
      #     # null effects
      #     areaData<-c(yalle-y50eb+y50e,rev(yalle-y50eb))
      #     ptsNHSTe<-data.frame(x=nAreaVals+vals_offset,y=areaData)
      #     g<-g+geom_polygon(data=ptsNHSTe,aes(x=x,y=y),fill=plotcolours$infer_hiterr)
      #     
      #     col<-plotcolours$infer_misserr
      #     cole<-plotcolours$infer_misserr
      #     lb1<-"!+ve"
      #     lb2<-"!-ve"
      #     pts1<-data.frame(vals=vals+vals_offset,y50=y50a,y25=y25,y75=y75)
      #     pts2<-data.frame(vals=vals+vals_offset,y50e=y50ea,y25e=y25e,y75e=y75e)
      #   } else {
      #     # non-null effects
      #     areaData<-1-c(0,y50+y50a,0)
      #     ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData)
      #     g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_misserr)
      #     # null effects
      #     areaData<-c(0,y50e+y50ea,0)
      #     ptsNHSTe<-data.frame(x=areaVals+vals_offset,y=areaData)
      #     g<-g+geom_polygon(data=ptsNHSTe,aes(x=x,y=y),fill=plotcolours$infer_misserr)
      #     
      #     col<-plotcolours$infer_hiterr
      #     cole<-plotcolours$infer_hiterr
      #     lb1<-" +ve"
      #     lb2<-"!+ve"
      #     pts1<-data.frame(vals=vals+vals_offset,y50=y50,y25=y25,y75=y75)
      #     pts2<-data.frame(vals=vals+vals_offset,y50e=y50e,y25e=y25e,y75e=y75e)
      #   }
      # } 
      
          areaVals<-c(vals[1],vals,vals[length(vals)])
          areaData<-c(yalle,rev(1-y50))
          ptsNHST<-data.frame(x=c(vals,rev(vals))+vals_offset,y=areaData)
          g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_sigC)
          lb=data=data.frame(x=max(vals),y=mean(ptsNHST$y[length(vals)+c(0,1)]),lb=" +ve")
          g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="white",fill=plotcolours$infer_sigC)
          
          if (!is.null(y50e)) {
          areaVals<-c(vals[1],vals,vals[length(vals)])
          areaData<-c(yalle-y50e,rev(yalle))
          ptsNHST<-data.frame(x=c(vals,rev(vals))+vals_offset,y=areaData)
          g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=plotcolours$infer_hiterr)
          lb=data=data.frame(x=max(vals),y=mean(ptsNHST$y[length(vals)+c(0,1)]),lb="!+ve")
          g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="red",fill=plotcolours$infer_hiterr)
          }
          
          col<-plotcolours$infer_misserr
          cole<-plotcolours$infer_nsigC
          lb1<-"!-ve"
          lb2<-" -ve"
          pts1<-data.frame(vals=vals+vals_offset,y50=y50,y25=y25,y75=y75)
          pts2<-data.frame(vals=vals+vals_offset,y50e=yalle-y50e,y25e=y25e,y75e=y75e)
      if (doLine) {
        # shaded fills
        areaVals<-c(vals[1],vals,vals[length(vals)])
        # type 2 errors
        areaData<-1-c(0,pts1$y50,0)
        ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData)
        g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=col)
        g<-g+geom_line(data=pts1,aes(x=vals,y=1-y50),color=col)
        # g<-g+geom_point(data=pts1,aes(x=vals,y=1-y50),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        lb=data=data.frame(x=max(vals),y=mean(c(1,1-pts1$y50[length(vals)])),lb=lb1)
        g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="white",fill=col)
        
        # type 1 errors
        if (!is.null(y50e)) {
          areaData<-c(0,pts2$y50e,0)
        ptsNHSTe<-data.frame(x=areaVals+vals_offset,y=areaData)
        g<-g+geom_polygon(data=ptsNHSTe,aes(x=x,y=y),fill=cole)
        g<-g+geom_line(data=pts2,aes(x=vals,y=y50e),color=cole)
        # g<-g+geom_point(data=pts2,aes(x=vals,y=y50e),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        lb=data=data.frame(x=max(vals),y=mean(c(0,pts2$y50e[length(vals)])),lb=lb2)
        g<-g+geom_label(data=lb,aes(x=x,y=y,label=lb),hjust=-0.2,vjust=0.5,size=4,colour="white",fill=cole)
        }
      } else {
        # shaded fills
        outline<-c(-1,-1,1,1)*0.1
        areaVals<-rep(vals,each=4)+rep(outline,length(vals))
        ids<-rep(1:length(vals),each=4)
        areaData<-1-rep(c(0,1,1,0),length(vals))*rep(1-y50,each=4)
        ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData,ids=ids)
        g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y,group=ids),colour = "black",fill=col,alpha=0.5)
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=shapes$parameter, colour = "black", fill = "white", size = 4)

        if (!is.null(y50e)) {
          areaData<-rep(c(0,1,1,0),length(vals))*rep(y50e,each=4)
          ptsNHST<-data.frame(x=areaVals+vals_offset,y=areaData,ids=ids)
          g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y,group=ids),colour = "black",fill=cole,alpha=0.5)
          g<-g+geom_point(data=pts2,aes(x=vals,y=y50e),shape=shapes$parameter, colour = "black", fill = "white", size = 4)
        }
      }
    } else {
      if (explore$Explore_show=="PDF") {
        y=rep(0,length(ySingle))
        switch(effect$world$populationPDF,
               "Single"={
                 pts<-data.frame(x=vals+vals_offset,y=ySingle)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+ySingle)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+ySingle,rev(y+ySingle+yExp)))
               },
               "Gauss"={
                 pts<-data.frame(x=vals+vals_offset,y=yGauss)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+yGauss)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+yGauss,rev(y+yGauss+yExp)))
               },
               "Exp"={
                 pts<-data.frame(x=vals+vals_offset,y=yExp)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+yExp)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+yExp,rev(y+yExp+yGauss)))
               })
        g<-g+geom_line(data=pts,aes(x=x,y=y),color="black")
        g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$parameter,fill=plotcolours$sampleC,size = markersize)
        # g<-g+geom_polygon(data=pts1,aes(x=x,y=y),fill=plotcolours$sampleC)
        # g<-g+geom_polygon(data=pts2,aes(x=x,y=y),fill=plotcolours$sampleC,alpha=0.7)
      } else {
        pts1<-data.frame(vals=vals+vals_offset,y25=y25,y38=y38,y50=y50,y62=y62,y75=y75)
        if (doLine) {
        pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25,rev(y75)))
        pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38,rev(y62)))
        if (ni_max2==1 || !no_se_multiple) {
          g<-g+geom_polygon(data=pts1f,aes(x=x,y=y),fill=col,alpha=0.2)
          g<-g+geom_polygon(data=pts2f,aes(x=x,y=y),fill=col,alpha=0.4)
        }
        g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
      } else{
        if (ni_max2==1 || !no_se_multiple){
          g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.7/length(vals)))
        }
      }
      if (use_col_names){
        pts1<-data.frame(x=vals+vals_offset,y=y50,fill=explore$Explore_typeShow)
        g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
      } else {
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=shapes$parameter, colour = "black",fill=col, size = markersize)
      }
      }
    }
    
    if (is.element(explore$Explore_show,c("EffectSize","Interaction")) && is.element(exploreResult$Explore_type,c("EffectSize","EffectSize1","EffectSize2","Interaction"))){
      pts3<-data.frame(x=c(-1,1),y=c(-1,1))
      g<-g+geom_line(data=pts3,aes(x=x,y=y),colour="yellow", linetype="dotted")
    }
    
    if (explore$Explore_show=="p(sig)" && exploreResult$Explore_type=="SampleSize" && effect$world$populationPDF=="Single"){
      w<-y50
      n<-exploreResult$result$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
      r_est<-r_est$minimum
      nvals<-seq(min(n),max(n),length.out=101)
      yvals<-rn2w(r_est,nvals)
      ptsn<-data.frame(x=nvals+vals_offset,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(10,explore$Explore_nRange),w=0.8,r=r_est)
      
      if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2))
        # label<-paste("n80 =",format(n80$minimum,digits=2),"  r_est =", format(r_est,digits=3))
      } else {
        if (sum(n<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
        if (sum(n>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        # label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max2>1){label<-paste(explore$Explore_typeShow,": ",label,sep="")}
      lpts<-data.frame(x=min(n)+vals_offset,y=0.8+(ni_max2-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3.5)
    }
    if (explore$Explore_show=="p(sig)" && exploreResult$Explore_type=="EffectSize"){
      w<-y50
      r<-exploreResult$result$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n_est<-optimize(minrw,c(0,100),w=w,r=r)
      n_est<-n_est$minimum
      rvals<-seq(min(r),max(r),length.out=101)
      yvals<-rn2w(rvals,n_est)
      ptsn<-data.frame(x=rvals+vals_offset,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
      
      if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2))
        # label<-paste("n80 =",format(n80$minimum,digits=2),"  n_est =", format(n_est,digits=3))
      } else {
        if (sum(r<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
        if (sum(r>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        # label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max2>1){label<-paste(explore$Explore_typeShow,": ",label,sep="")}
      lpts<-data.frame(x=0+vals_offset,y=0.8+(ni-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3.5)
    }
  }
  }

  if (multi=="allEffects" || multi=="mainEffects") {
    for (ni2 in 1:ni_max2) {
      switch (ni2,
              {explore$Explore_whichShow<-"Main 1"},
              {explore$Explore_whichShow<-"Main 2"},
              {explore$Explore_whichShow<-"Interaction"}
              )
      if (is.character(exploreResult$result$vals[1])) {
        vals_offset<-(ni2-1)*valsGap+0.5
      } else {
        vals_offset<-(ni2-1)*valsGap*2 
      }
      td<-data.frame(x=vals_offset,y=ylim[2]-diff(ylim)/6,label=explore$Explore_whichShow)
      g<-g+geom_label(data=td,aes(x=x, y=y, label=label,hjust=0.5))
    }
    if (is.character(exploreResult$result$vals[1])) {
      g<-g+geom_vline(aes(xintercept=valsGap*c(1,ni_max2-1)-0.5*(valsGap-1)))
    } else {
      g<-g+geom_vline(aes(xintercept=valsGap*c(1,ni_max2)))
    }
    if (min(vals)<0) {
      tk<-(-2:2)/2
      jk<-2*valsGap
    } else {
      tk<-(0:4)/4
      jk<-valsGap
    }
    if (is.character(exploreResult$result$vals[1])) {
      tk<-seq(0,1,length.out=length(vals))
      g<-g+scale_x_continuous(breaks=c(vals,vals+jk,vals+jk*2),labels=c(exploreResult$result$vals,exploreResult$result$vals,exploreResult$result$vals),limits=c(0,1+jk*2)+c(-1,1)*0.25) +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    } else {
    g<-g+scale_x_continuous(breaks=c(tk,tk+jk,tk+jk*2),labels=c(tk,tk,tk),limits=c(tk[1],1+jk*2)+c(-1,1)*0.25)
    }
  } else {
    if (is.character(exploreResult$result$vals[1]))
      g<-g+scale_x_continuous(breaks=vals,labels=exploreResult$result$vals)
  }
  if ((is.element(exploreResult$Explore_type,c("SampleSize","Repeats","CheatingAmount","Alpha")) &&
                 explore$Explore_xlog) 
      || ((exploreResult$Explore_type=="NoStudies") && explore$Explore_Mxlog)) {
    g<-g+scale_x_log10(limits=c(min(vals)/1.05,max(vals)*1.1))
  } else {
    g<-g+scale_x_continuous(breaks=vals,labels=exploreResult$result$vals,limits=c(min(vals)/1.05,max(vals)*1.1))
  }
  
  if (explore$ExploreFull_ylim){
  g<-g+coord_cartesian(ylim = ylim*1.05)
  }
    g<-g+ylab(ylabel)
    switch (exploreResult$Explore_type,
            "EffectSize"={g<-g+xlab(bquote(r[population]))},
            "EffectSize1"={
              g<-g+xlab(bquote(MainEffect1:r[population]))
              # g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=-1, angle=0, label="Main Effect 1",color="white")
            },
            "EffectSize2"={
              g<-g+xlab(bquote(MainEffect2:r[population]))
              # g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=-1, angle=0, label="Main Effect 2",color="white")
            },
            "Covariation"={
              g<-g+xlab(bquote(covariation:r[population]))
              # g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=-1, angle=0, label="Covariation",color="white")
            },
            "Interaction"={
              g<-g+xlab(bquote(interaction:r[population]))
              # g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=-1, angle=0, label="Interaction",color="white")
            },
            g<-g+xlab(exploreResult$Explore_type)
    )
    
    # if (explore$Explore_show=="p(sig)") {
    #   top<-max(y75,na.rm=TRUE)
    #   top<-ceil(top*10)/10
    #   ylim<-c(0,top)
    #   g<-g+scale_y_continuous(limits=ylim)
    # }
    
  g+plotTheme
}

