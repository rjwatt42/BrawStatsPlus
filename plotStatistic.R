min_p=0.0001
truncate_p=FALSE
min_nw=10
max_nw=10000
horiz_scatter=0.5

se_size=0.75
se_arrow=0.3
CI=0.95

histGain<-NA

collectData<-function(result) {
  ns<-cbind(result$nval)
  df1<-cbind(result$df1)
  rp<-cbind(result$rpIV)
  ro<-cbind(result$roIV)
  po<-cbind(result$poIV)

  if (all(is.na(result$rIV2))){
    rs<-cbind(result$rIV)
    ps<-cbind(result$pIV)
  } else {
    switch (result$showType,
            "direct"={
              rs<-rbind(result$r$direct)
              ps<-rbind(result$p$direct)
            },
            "unique"={
              rs<-rbind(result$r$unique)
              ps<-rbind(result$p$unique)
            },
            "total"={
              rs<-rbind(result$r$total)
              ps<-rbind(result$p$total)
            },
            "all"={
              rs<-c()
              ps<-c()
              ysc=1/3
              xoff=c(0,0,0,2,2,2,4,4,4)
              for (jk in 1:ncol(result$r$direct)) {
                rs<-cbind(rs,result$r$direct[,jk],result$r$unique[,jk],result$r$total[,jk])
                ps<-cbind(ps,result$p$direct[,jk],result$p$unique[,jk],result$p$total[,jk])
              }
            },
            "coefficients"={
              rs<-rbind(result$r$coefficients)
              ps<-rbind(result$p$direct)
            }
    )
  }
  if (truncate_p) {
    ps[ps<min_p]<-min_p
    po[po<min_p]<-min_p
  }
  out<-list(rs=rs,ps=ps,ns=ns,df1=df1,rp=rp,ro=ro,po=po)
}

makeFiddle<-function(y,yd){
  yz<-c()
  xz<-c()
  xd<-0.15
  
  for (i in 1:length(y)){
    found<-(abs(yz-y[i])<yd)
    if (any(found,na.rm=TRUE)) {
      x_max<-max(xz[found])
      x_which<-which.max(xz[found])
      y_at_max<-yz[found][x_which]
      x_min<-min(xz[found])
      x_which<-which.min(xz[found])
      y_at_min<-yz[found][x_which]
      if (abs(x_min)<x_max) {
        x_inc<-sqrt(1-((y[i]-y_at_min)/yd)^2)
        xz<-c(xz,x_min-x_inc*xd)
        yz<-c(yz,y[i])
      } else {
      x_inc<-sqrt(1-((y[i]-y_at_max)/yd)^2)
      xz<-c(xz,x_max+x_inc*xd)
      yz<-c(yz,y[i])
      }
    } else {
      xz<-c(xz,0)
      yz<-c(yz,y[i])
    }
  }
  
  return(xz)
}

get_upperEdge<-function(allvals,svals){
  target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1,na.rm=TRUE)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    target<-(target1+target2)/2
  } else target<-target1+0.001
}
get_lowerEdge<-function(allvals,svals) {
  target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    if (target2==-Inf) target2=target1-0.5
    target<-(target1+target2)/2
  } else {target<-target1-0.5}
}

getBins<-function(vals,nsvals,target,minVal,maxVal,fixed=FALSE) {
  nv=max(length(nsvals),length(vals))
  nb<-round(sqrt(nv)*0.75)
  
  if (min(vals,na.rm=TRUE)==max(vals,na.rm=TRUE)) {nb<-3}

  high_p<-max(vals,na.rm=TRUE)+0.2
  low_p<-min(vals,na.rm=TRUE)-0.2
  if (!is.null(minVal)) {
    low_p<-max(minVal,low_p,na.rm=TRUE)
  }
  if (!is.null(maxVal)) {
    high_p<-min(maxVal,high_p,na.rm=TRUE)
  }

  if ((length(nsvals)==0) || (length(nsvals)==length(vals))){
    bins<-seq(low_p,high_p,length.out=nb)
    return(bins)
  }

  if (fixed) {
    target_low<-max(-target,low_p)
    target_high<-min(target,high_p)
    targetRange<-target_high-target_low
    nbs<-ceiling(nb*targetRange/(high_p-low_p))
    binStep<-targetRange/nbs
    bins<-seq(target_low,target_high,binStep)
    if (target<high_p) {
      bins<-c(bins,seq(target+binStep,high_p+binStep,binStep))
    }
    if (-target>low_p) {                                
      bins<-c(rev(seq(-target-binStep,low_p-binStep,-binStep)),bins)
    }
    return(bins)
  } 
  
  # make sure it goes through target
  if (length(target)>1) {
    if (high_p>target[2] && low_p< target[1]) {
      nbs<-ceiling(nb*(target[2]-0)/(high_p-low_p))
      binStep<-target[2]/nbs
      bins<-c(rev(seq(0,low_p-binStep,-binStep)),seq(binStep,high_p,binStep))
      return(bins)
    }
    if (high_p>target[2]) {
      nbs<-ceiling(nb*(high_p-target[2])/(high_p-low_p))
      binStep<-(high_p-target[2])/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target[1]) {
      nbs<-ceiling(nb*(target[1]-low_p)/(high_p-low_p))
      binStep<-(target[1]-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  } else {
    if (high_p>target) {
      nbs<-ceiling(nb*(high_p-target)/(high_p-low_p))
      binStep<-(high_p-target)/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target) {
      nbs<-ceiling(nb*(target-low_p)/(high_p-low_p))
      binStep<-(target-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  }
  # if all else fails
  binStep<-(high_p-low_p)/nb
  bins<-seq(low_p-binStep,high_p,binStep)
  return(bins)
}

expected_hist<-function(vals,svals,valType){

  if (is.null(valType)) valType<-"r"
  if (is.element(valType,c("r1","rp","ci1","ci2"))) valType<-"r"
  if (is.element(valType,c("e1","e2","p1"))) valType<-"p"
  if (is.element(valType,c("wp"))) valType<-"w"
  
  switch (valType,
          "r"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "p"=  { # ns is large
            if (pPlotScale=="log10") {
              target<-log10(alphaSig)
              bins<-getBins(vals,svals,target,log10(min_p),log10(1))
            } else {
              target<-alphaSig
              bins<-getBins(vals,svals,target,0,1)
              bins<-c(0,bins[bins>0])
            }
          },
            
          "log(lrs)"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),0,lrRange)
          },
          
          "e1d"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "log(lrd)"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "e2d"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "w"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,log10(min_p),NULL)
          },
          
          "n"= { # ns is small
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,10000)
          },
          
          "nw"= { # ns is large
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,max_nw)
          }
  )
  use<-vals>=bins[1] & vals<bins[length(bins)]
  dens<-hist(vals[use],breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens<-dens$counts

  use<-svals>=bins[1] & svals<bins[length(bins)]
  sdens<-hist(svals[use],breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  sdens<-sdens$counts

  if (is.na(histGain)) {
    sdens<-sdens/max(dens,na.rm=TRUE)/2
    dens<-dens/max(dens,na.rm=TRUE)/2
  } else {
    sdens<-sdens/(sum(dens)*(bins[2]-bins[1]))*histGain
    dens<-dens/(sum(dens)*(bins[2]-bins[1]))*histGain
  }
  # browser()
  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  y2<-c(0,as.vector(matrix(c(sdens,sdens),2,byrow=TRUE)),0)
  data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

start_plot<-function() {
  g<-ggplot()
  g<-g+theme(legend.position = "none")+plotTheme
  g<-g+scale_x_continuous(breaks=NULL)
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}

expected_plot<-function(g,pts,expType=NULL,result=NULL,IV=NULL,DV=NULL,i=1,scale=1,col="white"){
  dotSize<-(plotTheme$axis.title$size)/3*scale
  
  if (!is.null(expType)) {
    if (useSignificanceCols){
      c1=plotcolours$infer_sigC
      c2=plotcolours$infer_nsigC
    } else {
      c1=plotcolours$descriptionC
      c2=plotcolours$descriptionC
    }
    if (expType=="e1") {
      c1=plotcolours$infer_sigNull
      c2=plotcolours$infer_nsNull
    }
    if (expType=="e2") {
      c1=plotcolours$infer_sigNonNull
      c2=plotcolours$infer_nsNonNull
    }
    if (expType=="e1d") {
      c1=plotcolours$infer_sigNull
      c2=plotcolours$infer_nsdNull
      c3<-plotcolours$infer_isigNull
    }
    if (expType=="e2d") {
      c1=plotcolours$infer_sigNonNull
      c2=plotcolours$infer_nsdNonNull
      c3<-plotcolours$infer_isigNonNull
    }
  } else {
    c1=col
    c2=col
  }
  
  if (length(pts$y1)<=points_threshold) {
    if (!is.null(result) && is.element(expType,c("r","p")) && length(pts$y1)==1) {
      switch(i,
             {rCI<-result$rIVCI
             pCI<-result$pIVCI
             if (isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,result$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-result$rIV2CI
             pCI<-result$pIV2CI
             if (isSignificant(STMethod,result$pIV2,result$rIV2,result$nval,result$df2,result$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-result$rIVIV2CI
             pCI<-result$pIVIV2CI
             if (isSignificant(STMethod,result$pIVIV2DV,result$rIVIV2DV,result$nval,result$df12,result$evidence)) {c<-c1} else (c<-c2)
             }
             )
      if (expType=="r" && !is.null(rCI)){
        pts1se<-data.frame(x=pts$x,y=rCI)
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size)
      }
      if (expType=="p" && !is.null(result$pIVCI)){
        pts1se<-data.frame(x=pts$x,y=log10(pCI))
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size)
      }
    }
      
    xr<-makeFiddle(pts$y1,2/40)*scale*scale
    pts$x<-pts$x+xr
    
    if (scale<1) {
      co1<-c1
      co2<-c2
    } else {
      co1<-"black"
      co2<-"black"
    }
    pts1=pts[!pts$y2,]
    g<-g+geom_point(data=pts1,aes(x=x, y=y1),shape=shapes$study, colour = co2, fill = c2, size = dotSize)
    pts2=pts[pts$y2,]
    g<-g+geom_point(data=pts2,aes(x=x, y=y1),shape=shapes$study, colour = co1, fill = c1, size = dotSize)
    if (!is.null(expType))
      if (is.element(expType,c("e1d","e2d"))) {
        pts3=pts[pts$y3,]
        g<-g+geom_point(data=pts3,aes(x=x, y=y1),shape=shapes$study, colour = co1, fill = c3, size = dotSize)
      }
    
  } else {
    if (is.logical(pts$y2)) {
      pts1<-expected_hist(pts$y1,pts$y1[pts$y2],expType)
    } else {
      pts1<-expected_hist(pts$y1,pts$y2,expType)
    }
    xoff<-pts$x[1]
    g<-g+
      geom_polygon(data=pts1,aes(y=x,x=y1*scale*scale+xoff),colour=NA, fill = c2)+
      geom_polygon(data=pts1,aes(y=x,x=y2*scale*scale+xoff),colour=NA, fill = c1)
    if (!is.null(expType))
      if (is.element(expType,c("e1d","e2d"))) {
        if (is.logical(pts$y3)) {
          pts1<-expected_hist(pts$y1,pts$y1[pts$y3],expType)
        }
        g<-g+
          geom_polygon(data=pts1,aes(y=x,x=y2+xoff),colour=NA, fill = c3)
      }
  }
  g
}


r_plot<-function(result,IV,IV2=NULL,DV,effect,expType="r",logScale=FALSE,otherresult=NULL){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }
  rlims<-c(-1,1)
  rlab<-"r"
  
  if (RZ=="z") {
    r<-atanh(r)
    rlims<-c(-1,1)*z_range
    rlab<-"z"
  }
    rActual<-r
  rActual[is.na(r)]<-0

  if (all(is.na(result$rIVIV2DV)) && is.null(IV2)){
    xoff=0
  } else {
    if (is.na(result$rIVIV2DV[1])){
      xoff=c(0,2)
    } else {
      xoff=c(0,2,4)
    }
  }
  
  switch (expType,
          "r"={
            ylim<-rlims
            ylabel<-bquote(.(rlab))
            },
          "p"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          },
          "p1"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p[1])
          },
          "log(lrs)"={
            ylim<-c(0, lrRange)
            ylabel<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e1d"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e2d"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "w"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
          },
          "nw"={
            ylim<-c(1, max_nw)
            ylabel<-bquote(n[w=80])
            },
          "n"={
            ylim<-c(1, result$design$sN*5*1.1)
            ylabel<-"n"
          },
          "rp"={
            ylim<-rlims
            ylabel<-"R"
          },
          "r1"={
            ylim<-rlims
            ylabel<-bquote(r[1])
          },
          "wp"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
          },
          "ci1"={
            ylim<-rlims
            ylabel<-rlab
          },
          "ci2"={
            ylim<-rlims
            ylabel<-rlab
          },
          "e1"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          },
          "e2"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          }
  )
  if (logScale) {
    ylim<-log10(ylim)
    ylabel<-bquote(bold(log['10'](.(ylabel))))
  }  else {
    ylabel<-bquote(bold(.(ylabel)))
  }
  
  if (!all(is.na(result$rIV))) {
    data<-collectData(result)
    if (RZ=="z") {
      data$rs<-atanh(data$rs)
      data$rp<-atanh(data$rp)
      data$ro<-atanh(data$ro)
    }
    switch (expType,
            "r"={data$sh<-data$rs},
            "rp"={data$sh<-data$rp},
            "r1"={data$sh<-data$ro},
            "p"={data$sh<-data$ps},
            "p1"={data$sh<-data$po},
            "log(lrs)"={data$sh<-cbind(res2llr(result,"sLLR"))},
            "log(lrd)"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "e1d"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "e2d"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "n"={data$sh<-data$ns},
            "w"={data$sh<-rn2w(data$rs,data$ns)},
            "wp"={data$sh<-rn2w(data$rp,data$ns)},
            "nw"={data$sh<-rw2n(data$rs,0.8,result$design$sReplTails)},
            "ci1"={data$sh<-r2ci(data$rs,data$ns,-1)},
            "ci2"={data$sh<-r2ci(data$rs,data$ns,+1)},
            "e1"={data$sh<-data$ps},
            "e2"={data$sh<-data$ps}
    )
    if (logScale) {
      data$sh<-log10(data$sh)
    }  
  }    
  g<-start_plot()
  
  # make theory
  for (i in 1:length(xoff)){
    if (result$evidence$showTheory) {
      if (is.element(expType,c("p","e1","e2","p1"))) {
        if (logScale) {
          yv<-seq(log10(min_p),0,length.out=51)
          yvUse<-10^yv
        }else{
          yv<-seq(0,1,length.out=51)
          yvUse<-yv
        }
        xd<-fullRSamplingDist(yvUse,result$effect$world,result$design,"p",logScale=logScale)
      } else {
        npt<-101
      switch(expType,
             "r"={
               if (RZ=="z") {
                 yv<-seq(-1,1,length.out=npt)*z_range
                 xd<-fullRSamplingDist(tanh(yv),result$effect$world,result$design,"r",logScale=logScale)
                 xd<-rdens2zdens(xd,tanh(yv))
               } else {
                 yv<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale)
               }
             },
             "ci1"={
               yv<-seq(-1,1,length.out=npt)*0.99
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale)
             },
             "ci2"={
               yv<-seq(-1,1,length.out=npt)*0.99
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale)
             },
             "w"={
               yv<-seq(alphaSig*1.01,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"w",logScale=logScale)
             },
             "log(lrs)"={
               yv<-seq(0,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrs)",logScale=logScale)
             },
             "log(lrd)"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale)
             },
             "e1d"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale)
             },
             "e2d"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale)
             },
             "nw"={
               if (logScale) {
                 yv<-seq(log10(5),log10(max_nw),length.out=npt)
                 yvUse<-10^yv
               }else{
                 yv<-5+seq(0,max_nw,length.out=npt)
                 yvUse<-yv
               }
               xd<-fullRSamplingDist(yvUse,result$effect$world,result$design,"nw",logScale=logScale)
             },
             "rp"={
               yv<-seq(-1,1,length.out=npt)*0.99
               xd<-fullRPopulationDist(yv,result$effect$world)
             },
             "n"={
               if (logScale) {
                 yv<-seq(log10(5),log10(5*result$design$sN),length.out=npt)
                 yvUse<-yv^10
               }else{
                 yv<-5+seq(0,5*result$design$sN,length.out=npt)
                 yvUse<-yv
               }
               xd<-getNDist(yv,result$design,logScale=logScale)
             },
             "wp"={
               yv<-seq(alphaSig*1.01,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"wp",logScale=logScale)
             }
      )
      }
      xd[is.na(xd)]<-0
        xd<-xd/max(xd)/2
      histGain<<-sum(xd)*(yv[2]-yv[1])
      ptsp<-data.frame(y=c(yv,rev(yv)),x=c(xd,-rev(xd))+xoff[i])
      g<-g+geom_polygon(data=ptsp,aes(x=x,y=y),colour="black",fill="white")
    } else {
      histGain<-NA
    }

    # then the samples
  if (!all(is.na(result$rIV))) {
      shvals<-data$sh[,i]
      rvals<-data$rs[,i]
      pvals<-data$ps[,i]
      resSig<-isSignificant(STMethod,pvals,rvals,data$ns,data$df1,result$evidence)
      if (result$showType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(expType,c("e1d","e2d"))) {
        d<-res2llr(result,STMethod)
        err<-(d<0 & data$rp[,i]!=0) | (d>0 & data$rp[,i]==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,n<-data$ns)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,n<-data$ns)
      }
      g<-expected_plot(g,pts,expType,result,IV,DV,i)
    
    if (is.element(expType,c("p","e1","e2","e1d","e2d"))) {
      if (effect$world$worldOn && is.element(expType,c("e1","e2","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otherresult)) n<-n+length(otherresult$pIV)
        switch (expType,
                "e1"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    sstr<-paste0("(",format(s),"/",format(n),")")
                  } else {
                    nstr<-""
                    sstr<-""
                  }
                  labelPt1<-paste0("p(True ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(False sig) = ",format(s/n*100,digits=2),"% ",sstr)
                },
                "e2"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    sstr<-paste0("(",format(s),"/",format(n),")")
                  } else {
                    nstr<-""
                    sstr<-""
                  }
                  labelPt1<-paste0("p(False ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(True sig) = ",format(s/n*100,digits=2),"% ",sstr)
                },
                "e1d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    s2str<-paste0("(",format(s2),"/",format(n),")")
                    s1str<-paste0("(",format(s1),"/",format(n),")")
                  } else {
                    nstr<-""
                    s2str<-""
                    s1str<-""
                  }
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(True sig) = ",format(s2/n*100,digits=2),"% ",s1str)
                  labelPt1<-paste0("p(False sig) = ",format(s1/n*100,digits=2),"% ",s1str)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"%")
                  labelPt1a<-paste0("p(True sig) = ",format(s2/n*100,digits=2),"%")
                  labelPt1<-paste0("p(False sig) = ",format(s1/n*100,digits=2),"%")
                },
                "e2d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    s2str<-paste0("(",format(s2),"/",format(n),")")
                    s1str<-paste0("(",format(s1),"/",format(n),")")
                  } else {
                    nstr<-""
                    s2str<-""
                    s1str<-""
                  }
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(False sig) = ",format(s2/n*100,digits=2),"% ",s2str)
                  labelPt1<-paste0("p(True sig) = ",format(s1/n*100,digits=2),"% ",s1str)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"%")
                  labelPt1a<-paste0("p(False sig) = ",format(s2/n*100,digits=2),"%")
                  labelPt1<-paste0("p(True sig) = ",format(s1/n*100,digits=2),"%")
                }
        )
        lpts1<-data.frame(x = xoff[i]-0.98, y = ylim[2]+diff(ylim)/25,label = labelPt1)
        g<-g+geom_label(data=lpts1,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
        lpts1a<-data.frame(x = xoff[i]-0.98, y = ylim[1]-diff(ylim)/25,label = labelPt1a)
        g<-g+geom_label(data=lpts1a,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
        if (is.element(expType,c("e1d","e2d"))) {
          lpts1<-data.frame(x = xoff[i]-0.98, y = sum(ylim)/2,label = labelPt1b)
          g<-g+geom_label(data=lpts1,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
        }
      } else {
        switch (expType,
                "p"={labelPt1<-paste0("p(sig)"," = ")},
                "e1"={labelPt1<-"p(Type I) = "},
                "e2"={labelPt1<-"p(Type II) = "}
        )
        if (expType=="e2") {
        labelPt2<-paste0(labelPt1,format(mean(!resSig,na.rm=TRUE)*100,digits=graph_precision),"%")
        labelPt3<-paste0(labelPt2,"  (",format(sum(!resSig,na.rm=TRUE)),"/",format(length(pvals)),")")
      } else {
        labelPt2<-paste0(labelPt1,format(mean(resSig,na.rm=TRUE)*100,digits=graph_precision),"%")
        labelPt3<-paste0(labelPt2,"  (",format(sum(resSig,na.rm=TRUE)),"/",format(length(pvals)),")")
      }
      if (length(xoff)>1) {
        lpts1<-data.frame(x = xoff[i]-0.95, y = ylim[2]+diff(ylim)/25,label = labelPt2)
        # lpts2<-data.frame(x = xoff[i]-0.95, y = ylim[1]-diff(ylim)/25,label = labelPt2)
      } else {
        lpts1<-data.frame(x = xoff[i]-0.95, y = ylim[2]+diff(ylim)/25,label = labelPt3)
        # lpts2<-data.frame(x = xoff[i]-0.95, y = ylim[1]-diff(ylim)/25,label = labelPt3)
      }
      g<-g+geom_label(data=lpts1,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
      # if (!is.null(lpts2)) {
      # g<-g+geom_label(data=lpts2,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
      # }
      }
    }
    
    if (is.element(expType,c("r","ci1","ci2"))) {
      lpts<-data.frame(x = xoff[i]-0.95, y = ylim[2],label=paste("actual =",format(rActual[i],digits=graph_precision)))
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill="white",size=labelSize)
    }
  }
  }
  
  if (length(xoff)>1)
    if (rem(i,3)==1)
      switch (xoff[i]/2+1,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )

  if (result$showType=="all") {
    for (i in 1:3) {
      g<-g+geom_hline(yintercept=(-1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", linewidth=1)
      g<-g+geom_hline(yintercept=(0.0+1)*ysc*0.9+(i-1)*ysc*2-1, linetype="dotted", color="black", linewidth=0.5)
      g<-g+geom_hline(yintercept=(1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", linewidth=1)
    }
    g<-g+coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))+
      scale_y_continuous(breaks=(c(-1,0,1,-1,0,1,-1,0,1)+1)*ysc*0.9+(c(1,1,1,2,2,2,3,3,3)-1)*ysc*2-1,labels=c(-1,0,1,-1,0,1,-1,0,1))
  } else {
    g<-g+geom_hline(yintercept=0.0, linetype="dotted", color="black", linewidth=0.5)+
      coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))
  }
  g<-g+ylab(ylabel)
  g
}


r1_plot<-function(result,IV,IV2=NULL,DV,effect){
  r_plot(result,IV,IV2,DV,effect,"r1")
}

rp_plot<-function(result,IV,IV2=NULL,DV,effect){
  r_plot(result,IV,IV2,DV,effect,"rp")
}

llrs_plot<-function(result,IV,IV2=NULL,DV,effect){
  g<-r_plot(result,IV,IV2,DV,effect,"log(lrs)")
  sAlpha<-log(dnorm(0)/dnorm(qnorm(1-alphaSig/2)))
  g<-g+geom_hline(yintercept=sAlpha, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  g
}

llrd_plot<-function(result,IV,IV2=NULL,DV,effect,ptype=NULL,otherresult=NULL){
  g<-r_plot(result,IV,IV2,DV,effect,"log(lrd)")
  sAlpha<-log(dnorm(0)/dnorm(qnorm(1-alphaSig/2)))
  g<-g+geom_hline(yintercept=sAlpha, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  g<-g+geom_hline(yintercept= -sAlpha, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  g
}

p_plot<-function(result,IV,IV2=NULL,DV,effect,ptype="p",otherresult=NULL,PlotScale=pPlotScale){

  g<-r_plot(result,IV,IV2,DV,effect,ptype,PlotScale=="log10",otherresult)
  
  if (ptype=="p") {
  if (PlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(1), linetype="dotted", color=plotcolours$one, linewidth=0.5)+
      geom_hline(yintercept=log10(0.005), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.01), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      scale_y_continuous(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  } else
  {
    g<-g+geom_hline(yintercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
    # g<-g+  scale_y_continuous(breaks=seq(0,1,0.1),labels=seq(0,1,0.1))
  }
  }
  g
}

p1_plot<-function(result,IV,IV2=NULL,DV,effect,ptype="p1"){
  g<-r_plot(result,IV,IV2,DV,effect,ptype,pPlotScale=="log10")
  
  if (pPlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(1), linetype="dotted", color=plotcolours$one, linewidth=0.5)+
      geom_hline(yintercept=log10(0.005), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.01), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(alpha), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      scale_y_continuous(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  } else
  {
    g<-g+geom_hline(yintercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      scale_y_continuous(breaks=seq(0,1,0.1),labels=seq(0,1,0.1))
  }
  g
}


w_plot<-function(result,IV,IV2=NULL,DV,effect){
  g<-r_plot(result,IV,IV2,DV,effect,"w",wPlotScale=="log10")
  
  if (wPlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.5), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.8), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  } else {
    g<-g+geom_hline(yintercept=alphaSig, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=0.5, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=0.8, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  }
  g
}

wp_plot<-function(result,IV,IV2=NULL,DV,effect){
  g<-r_plot(result,IV,IV2,DV,effect,"wp",wPlotScale=="log10")
  
  if (wPlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.5), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=log10(0.8), linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  } else {
    g<-g+geom_hline(yintercept=alphaSig, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=0.5, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
      geom_hline(yintercept=0.8, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)
  }
  g
}

n_plot<-function(result,IV,IV2=NULL,DV,effect){
  r_plot(result,IV,IV2,DV,effect,"n",nPlotScale=="log10")
}

nw_plot<-function(result,IV,IV2=NULL,DV,effect){
  r_plot(result,IV,IV2,DV,effect,"nw",nPlotScale=="log10")
}

e2_plot<-function(result,IV,IV2=NULL,DV,effect,nullresult=NULL){
  distr<-tolower(effect$world$populationPDF)
  lambda<-format(effect$world$populationPDFk,digits=3)
  switch (RZ,
          "r"={
            lab<-bquote(bold("Non-null:  " ~ r["p"] ~ "~" ~ .(distr) (r/.(lambda))))
          },
          "z"={
            lab<-bquote(bold("Non-null:  " ~ z["p"] ~ "~" ~ .(distr) (z/.(lambda))))
          }
  )
  switch (STMethod,
          "NHST"={
            p_plot(result,IV,IV2,DV,effect,ptype="e2",otherresult=nullresult)+
              ggtitle(lab)
          },
          "sLLR"={
            p_plot(result,IV,IV2,DV,effect,ptype="e2",otherresult=nullresult)+
              ggtitle(lab)
          },
          "dLLR"={
            p_plot(result,IV,IV2,DV,effect,ptype="e2d",otherresult=nullresult,PlotScale="linear")+
              geom_hline(yintercept=alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
              geom_hline(yintercept=-alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
              ggtitle(lab)
          }
  )
}

e1_plot<-function(nullresult,IV,IV2=NULL,DV,effect,result=NULL){
  switch (RZ,
          "r"={
            lab<-bquote(bold("Null:  " ~ r["p"] == 0))
          },
          "z"={
            lab<-bquote(bold("Null:  " ~ z["p"] == 0))
          }
  )
  switch (STMethod,
          "NHST"={
            p_plot(nullresult,IV,IV2,DV,effect,ptype="e1",otherresult=result)+
              ggtitle(lab)
          },
          "sLLR"={
            p_plot(nullresult,IV,IV2,DV,effect,ptype="e1",otherresult=result)+
              ggtitle(lab)
          },
          "dLLR"={
            p_plot(nullresult,IV,IV2,DV,effect,ptype="e1d",otherresult=result,PlotScale="linear")+
              geom_hline(yintercept=alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
              geom_hline(yintercept=-alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5)+
              ggtitle(lab)
          }
  )
}

ci1_plot<-function(result,IV=NULL,IV2=NULL,DV=NULL,effect){
  r_plot(result,IV,IV2,DV,effect,"ci1")
}

ci2_plot<-function(result,IV,IV2=NULL,DV,effect){
  r_plot(result,IV,IV2,DV,effect,"ci2")
}


