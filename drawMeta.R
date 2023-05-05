source("runMetaAnalysis.R")

showMeta<-"S"
nscaleLog=FALSE
maxnPlot=3
absPlot<-TRUE

makeMetaHist<-function(vals,use,xlim) {
  nbins<-10
  bins<-seq(xlim[1],xlim[2],length.out=nbins+1)
  dens<-hist(vals[use],bins,plot=FALSE)$counts/length(vals)
  h<-list(bins=bins,dens=dens)
}

drawMeta<-function(metaAnalysis,metaResult,metaWhich) {
  
  yall<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax)
  displayType<-"histograms"

  n1<-sum(metaResult$bestDist=="Single")
  n2<-sum(metaResult$bestDist=="Gauss")
  n3<-sum(metaResult$bestDist=="Exp")
  use<-which.max(c(n1,n2,n3))
  bestD<-c("Single","Gauss","Exp")[use]
  fullText<-paste0(bestD,"(",format(mean(metaResult$bestK),digits=3),")","\n",format(sum(metaResult$bestDist==bestD)),"/",length(metaResult$bestDist))
  if (metaAnalysis$meta_nullAnal) {
    fullText<-paste0(fullText,"\nnull=",format(mean(metaResult$bestNull),digits=3))
  }
  
  if (metaWhich=="Plain") {
    d1<-metaResult$result$rIV
    d1n<-(metaResult$result$rpIV==0)
    xlim<-c(-1, 1)
    if (absPlot) xlim<-c(0,1)
    d2<-metaResult$result$nval
    disp2<-"n"
    ylim<-c(5, maxnPlot*metaResult$design$sN*1.1)
    if (nscaleLog) {
      d2<-log10(d2)
      disp2<-bquote(log[10] (.(disp2)))
      ylim<-log10(ylim)
    }
    if (absPlot) d1<-abs(d1)
    use<-(d2>ylim[1]) & (d2<ylim[2])
    pts<-data.frame(x=d1[use],y=d2[use])
    usen<-(d2>ylim[1]) & (d2<ylim[2] & d1n)
    ptsn<-data.frame(x=d1[usen],y=d2[usen])
    
    g<-ggplot(pts,aes(x=x, y=y))
    if (length(d1)>=1200) {
      nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))*2
      nbins<-min(nbins,101)
      g<-g+stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c("#666666",plotcolours$descriptionC))
    }
    
    g<-drawWorld(metaResult$design,metaResult$effect,metaResult,g,plotcolours$descriptionC,metaAnalysis$showTheory)
    
    dotSize<-(plotTheme$axis.title$size)/3
    
    # show individual studies
    if (length(d1)<1200) {
      dotSize<-dotSize/(ceil(length(d1)/400))
        cl<-"black"
        g<-g+geom_point(data=pts,aes(x=x, y=y),shape=shapes$study, colour = cl, fill = plotcolours$descriptionC, size = dotSize)
        g<-g+geom_point(data=ptsn,aes(x=x, y=y),shape=shapes$study, colour = cl, fill = plotcolours$infer_nsigC, size = dotSize)
    }
    g<-g+theme(legend.position = "none")+plotTheme
    g<-g+scale_x_continuous(limits = xlim)
    if (nscaleLog) {
      g<-g+scale_y_continuous(limits = ylim)
    } else {
      g<-g+scale_y_continuous(limits = ylim)
    }
    
    g<-g+xlab("r")+ylab(disp2)

    return(g)
  } else {
    sAll<-c(metaResult$single$Smax,metaResult$gauss$Smax,metaResult$exp$Smax)
    switch (metaWhich,
            "Single"={
              x<-metaResult$single$kmax
              y<-metaResult$single$Smax
              y1<-metaResult$single$nullMax
              xlim<-c(-1,1)
            },
            "Gauss"={
              x<-metaResult$gauss$kmax
              y<-metaResult$gauss$Smax
              y1<-metaResult$gauss$nullMax
              xlim<-c(0,1)
            },
            "Exp"={
              x<-metaResult$exp$kmax
              y<-metaResult$exp$Smax
              y1<-metaResult$exp$nullMax
              xlim<-c(0,1)
            }
    )
    keep<- !is.na(x) & !is.na(y)
    best<-metaResult$bestS[keep]
    y<-y[keep]
    y1<-y1[keep]
    x<-x[keep]
    
    if (isempty(x)) {return(ggplot()+plotBlankTheme)}
    
    if (metaAnalysis$meta_nullAnal) {
      useBest<-y==best
      # ylim<-c(-0.5,0.5)
      # ylabel<-"S"
      switch (showMeta,
              "S"={
                y<-y
                ylim<-c(min(sAll),max(sAll))+c(-1,1)*(max(sAll)-min(sAll))/10
                ylabel<-"S"
              },
              "n"={
                y<-y1
                ylim<-c(-0.02,1.1)
                ylabel<-"p(0)"
              })
      
      pts<-data.frame(x=x,y=y)
      g<-ggplot(pts,aes(x=x, y=y))
      
      dotSize=min(8,max(3.5,sqrt(400/length(x))))
      # dotSize<-(plotTheme$axis.title$size)/3
      
      g<-g+geom_point(data=pts,aes(x=x, y=y),shape=shapes$meta, colour = "black", fill = "grey", size = dotSize)
      pts<-data.frame(x=x[useBest],y=y[useBest])
      g<-g+geom_point(data=pts,aes(x=x, y=y),shape=shapes$meta, colour = "black", fill = "yellow", size = dotSize)
      
      g<-g+theme(legend.position = "none")+plotTheme
      g<-g+scale_x_continuous(limits = c(min(x),max(x))+c(-1,1)*(max(x)-min(x))*0.2)
      
      if (!is.null(ylim)) {
      }
      if (metaWhich=="Single") {
        g<-g+scale_y_continuous(limits = ylim)+ylab(ylabel)
      } else {
        g<-g+scale_y_continuous(limits = ylim,breaks=c())+ylab("")
      }
      
    } else {
      switch (displayType,
              "histograms"={
                use<-y==best
                if (length(x)<=100) {
                  xb<-seq(xlim[1],xlim[2],length.out=31)
                  yb<-xb*0
                  y<-x*0
                  for (i in 1:length(x)) {
                    use.x<-sum(xb<x[i])
                    yb[use.x]<-yb[use.x]+1
                    y[i]<-yb[use.x]
                  }
                  dotSize<-(plotTheme$axis.title$size)/4
                  
                  g<-ggplot()
                  if (!all(use)) {
                    pts<-data.frame(x=x[!use],y=y[!use])
                    g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$study,fill="white",size=dotSize)
                  }
                  if (any(use)) {
                    pts<-data.frame(x=x[use],y=y[use])
                    g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$study,fill="yellow",size=dotSize)
                  }
                  g<-g+theme(legend.position = "none")+plotTheme
                  ylim<-c(0,max(50,max(y,na.rm=TRUE)*1.1))
                  g<-g+scale_x_continuous(limits=xlim)+xlab("k")
                  g<-g+scale_y_continuous(limits=ylim,breaks=c())+ylab("")
                } else {
                  h1<-makeMetaHist(x,use,xlim)
                  x<-as.vector(matrix(c(h1$bins,h1$bins),2,byrow=TRUE))
                  y<-c(0,as.vector(matrix(c(h1$dens,h1$dens),2,byrow=TRUE)),0)
                  pts<-data.frame(x=x,y=y)
                  g<-ggplot(pts,aes(x=x, y=y))
                  if (metaWhich==bestD) {
                    g<-g+geom_polygon(data=pts,aes(x=x,y=y),fill="yellow",colour="black")
                  } else {
                    g<-g+geom_polygon(data=pts,aes(x=x,y=y),fill="white",colour="black")
                  }
                  g<-g+theme(legend.position = "none")+plotTheme
                  h2a<-makeMetaHist(metaResult$single$kmax,metaResult$single$Smax==metaResult$bestS,c(-1,1))
                  h2b<-makeMetaHist(metaResult$gauss$kmax,metaResult$gauss$Smax==metaResult$bestS,c(0,1))
                  h2c<-makeMetaHist(metaResult$exp$kmax,metaResult$exp$Smax==metaResult$bestS,c(0,1))
                  ylim<-c(0,max(c(h2a$dens,h2b$dens,h2c$dens)*1.2))
                  g<-g+scale_y_continuous(limits=ylim,breaks=c())+ylab("")
                }
                # g<-g+scale_y_continuous(limits=c(0,1.2),breaks=c())+ylab("")
              },
              "S"= {
                pts<-data.frame(x=x,y=y)
                g<-ggplot(pts,aes(x=x, y=y))
                
                dotSize=min(8,max(3.5,sqrt(400/length(x))))
                # dotSize<-(plotTheme$axis.title$size)/3
                
                g<-g+geom_point(data=pts,aes(x=x, y=y),shape=shapes$meta, colour = "black", fill = "grey", size = dotSize)
                use<-y==metaResult$bestS
                pts<-data.frame(x=x[use],y=y[use])
                g<-g+geom_point(data=pts,aes(x=x, y=y),shape=shapes$meta, colour = "black", fill = "yellow", size = dotSize)
                
                g<-g+theme(legend.position = "none")+plotTheme
                g<-g+scale_x_continuous(limits = c(min(x),max(x))+c(-1,1)*(max(x)-min(x))*0.2)
                
                ylim<-c(min(yall)*1.2,max(0,max(yall)*1.2))
                if (metaWhich=="Single") {
                  g<-g+scale_y_continuous(limits = ylim)+ylab("log(lk)")
                } else {
                  g<-g+scale_y_continuous(limits = ylim,breaks=c())+ylab("")
                }
              }
      )
    }
    g<-g+xlab("k")
    if (metaWhich==bestD) {
      pts_lb<-data.frame(x=mean(x), y=ylim[1], lb=fullText)
      g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),hjust=0.5,vjust=0,size=3,fill="yellow")
    }
    g+ggtitle(metaWhich)
    return(g)
  }
  
}


drawWorld<-function(design,effect,metaResult,g,colour="white",showTheory=FALSE) {
  x<-seq(-1,1,length.out=101)
  y<-seq(5,maxnPlot*design$sN,length.out=101)
  sigma<-1/sqrt(y-3)
  gain<-dgamma(y-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  
  nbd<-31
  xbins<-seq(-1,1,length.out=nbd+1)
  ybins<-seq(5,1+maxnPlot*design$sN,length.out=nbd+1)
  zd<-c()
  for (i in 2:length(ybins)) {
    use<-metaResult$result$nval>=ybins[i-1] & metaResult$result$nval<ybins[i]
    z1<-hist(metaResult$result$rIV[use],breaks=xbins)$counts
    zd<-rbind(zd,z1)
  }
  xd<-(xbins[1:nbd]+xbins[2:(nbd+1)])/2
  yd<-(ybins[1:nbd]+ybins[2:(nbd+1)])/2
  xd<-as.vector(matrix(rep(xd,nbd),nbd,byrow=TRUE))
  yd<-as.vector(matrix(rep(yd,nbd),nbd,byrow=FALSE))
  zd<-as.vector(zd)
  zd<-zd/max(zd,na.rm=TRUE)

  lambda<-effect$world$populationPDFk
  nullP<-effect$world$populationNullp
  z<-c()
  switch (effect$world$populationPDF,
          "Single"={
            for (i in 1:length(y)) {
              z1<-SingleSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
             if (metaResult$metaAnalysis$sig_only) {
               zcrit<-qnorm(1-alpha/2,0,sigma[i])
               z1[atanh(abs(x))<zcrit]<-0
             }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Gauss"={
            for (i in 1:length(y)) {
              z1<-GaussSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
              if (metaResult$metaAnalysis$sig_only) {
                zcrit<-qnorm(1-alpha/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Exp"={
            for (i in 1:length(y)) {
              z1<-ExpSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
              if (metaResult$metaAnalysis$sig_only) {
                zcrit<-qnorm(1-alpha/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          }
  )
  x1<-as.vector(matrix(rep(x,101),101,byrow=TRUE))
  y1<-as.vector(matrix(rep(y,101),101,byrow=FALSE))
  za<-as.vector(z)

  lambda<-metaResult$bestK
  nullP<-metaResult$bestNull
  z<-c()
  switch (metaResult$bestDist,
          "Single"={
            for (i in 1:length(y)) {
              z1<-SingleSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
              if (metaResult$metaAnalysis$meta_psigAnal) {
                zcrit<-qnorm(1-alpha/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Gauss"={
            for (i in 1:length(y)) {
              z1<-GaussSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
              if (metaResult$metaAnalysis$meta_psigAnal) {
                zcrit<-qnorm(1-alpha/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          },
          "Exp"={
            for (i in 1:length(y)) {
              z1<-ExpSamplingPDF(atanh(x),lambda,sigma[i])*(1-nullP)+
                ZeroSamplingPDF(atanh(x),sigma[i])*nullP
              if (metaResult$metaAnalysis$meta_psigAnal) {
                zcrit<-qnorm(1-alpha/2,0,sigma[i])
                z1[atanh(abs(x))<zcrit]<-0
              }
              z<-rbind(z,zdens2rdens(z1,x)*gain[i])
            }
          }
  )
  if (nscaleLog) {y<-log10(y)}
  
  # x1<-as.vector(matrix(rep(x,101),101,byrow=TRUE))
  # y1<-as.vector(matrix(rep(y,101),101,byrow=FALSE))
  zb<-as.vector(z)

  use<-is.finite(za) & y1<maxnPlot*design$sN
  za<-za/max(za,na.rm=TRUE)
  ptsa<-data.frame(x=x1[use],y=y1[use],z=za[use])
  
  use<-is.finite(zb) & y1<maxnPlot*design$sN
  zb<-zb/max(zb,na.rm=TRUE)
  ptsb<-data.frame(x=x1[use],y=y1[use],z=zb[use])
  
  use<-is.finite(zd) & yd<maxnPlot*design$sN
  zd<-zd/max(zd,na.rm=TRUE)
  ptsd<-data.frame(x=xd[use],y=yd[use],z=zd[use])

  # white is the actual world
  # colour is the best fit world
  # black is the actual data
  if (showTheory) {
  g<-g+geom_contour(data=ptsa,aes(x=x,y=y,z=z),colour="white",breaks=c(0.1,0.3,0.5,0.7,0.9)) 
  }
  # g<-g+geom_contour(data=ptsd,aes(x=x,y=y,z=z),colour="black",breaks=c(0.1,0.3,0.5,0.7,0.9),lwd=1) 
  g<-g+geom_contour(data=ptsb,aes(x=x,y=y,z=z),colour="black",breaks=c(0.1,0.3,0.5,0.7,0.9),lwd=0.1)
  
  lb<-paste0("\u2014",metaResult$bestDist,"(",format(metaResult$bestK,digits=3),")")
  if (metaResult$metaAnalysis$meta_nullAnal) {
    lb<-paste0(lb," null=",format(metaResult$bestNull,digits=3))
  }
  pts_lb<-data.frame(x=0,y=max(y)*1.1,lb=lb)
  if (absPlot) pts_lb$x[1]<-0.5
  g<-g+geom_label(data=pts_lb,aes(x=x,y=y,label=lb),colour=colour,fill="white",fontface="bold")
  return(g)
}
