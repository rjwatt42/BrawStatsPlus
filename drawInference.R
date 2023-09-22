nscaleLog=FALSE
maxnPlot=200

drawInference<-function(IV,IV2,DV,effect,design,evidence,result,disp){
  
  switch (disp,
          "p"={g<-p_plot(result,IV,IV2,DV,effect)},
          "r"={g<-r_plot(result,IV,IV2,DV,effect)},
          "r1"={g<-r1_plot(result,IV,IV2,DV,effect)},
          "p1"={g<-p1_plot(result,IV,IV2,DV,effect)},
          "log(lrs)"={g<-llrs_plot(result,IV,IV2,DV,effect)},
          "log(lrd)"={g<-llrd_plot(result,IV,IV2,DV,effect)},
          "w"={g<-w_plot(result,IV,IV2,DV,effect)},
          "wp"={g<-wp_plot(result,IV,IV2,DV,effect)},
          "nw"={g<-nw_plot(result,IV,IV2,DV,effect)},
          "rp"={g<-rp_plot(result,IV,IV2,DV,effect)},
          "n"={g<-n_plot(result,IV,IV2,DV,effect)},
          "e1"={g<-e1_plot(result,IV,IV2,DV,effect)},
          "e2"={g<-e2_plot(result,IV,IV2,DV,effect)},
          "ci1"={g<-ci1_plot(result,IV,IV2,DV,effect)},
          "ci2"={g<-ci2_plot(result,IV,IV2,DV,effect)}
  )
  g+ggtitle(result$an_name)
}


draw2Inference<-function(IV,IV2,DV,effect,design,evidence,result,disp1,disp2,metaPlot=FALSE){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }
  
  pvals<-result$pIV
  rvals<-result$rIV
  nvals<-result$nval
  df1vals<-result$df1

  rlim<-c(-1,1)
  if (RZ=="z")  {
    r<-atanh(r)
    rvals<-atanh(rvals)
    rlim<-c(-1,1)*z_range
  }
  xsc<-0
  switch (disp1,
          "p"={
            d1<-result$pIV
            if (pPlotScale=="log10"){xsc<-1}
            xlim<-c(0,1)
          },
          "r"={
            d1<-result$rIV
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1<-"z"
            }
            xlim<-rlim
          },
          "log(lrs)"={
            d1<-res2llr(result,"sLLR")
            xlim<-c(-0.1, lrRange)
            disp1<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d1<-res2llr(result,"dLLR")
            if (any(d1<0)) {
              ylim<-c(-lrRange, lrRange)
            } else {
              ylim<-c(-0.1, lrRange)
            }
            disp1<-bquote(log[e](lr[d]))
          },
          "w"={
            d1<-result$rIV
            d1<-rn2w(d1,result$nval)
            if (wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "wp"={
            d1<-result$rp
            d1<-rn2w(d1,result$nval)
            if (wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "nw"={
            d1<-rw2n(result$rIV,0.8,result$design$sReplTails)
            xlim<-c(1, max(d1)*1.1)
          },
          "rp"={
            d1<-result$rpIV
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1<-"zp"
            }
            xlim<-rlim
          },
          "r1"={
            d1<-result$roIV
            disp1<-bquote(r[1])
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1<-bquote(z[1])
            }
            xlim<-rlim
          },
          "p1"={
            d1<-result$poIV
            if (pPlotScale=="log10"){xsc<-1}
            xlim<-c(0, 1)
            disp1<-bquote(p[1])
          },
          "n"={
            d1<-result$nval
            xlim<-c(1, 200*1.1)
          }
  )
  
  ysc<-0
  switch (disp2,
          "p"={
            d2<-result$pIV
            if (pPlotScale=="log10"){
              ysc<-1
              }
            ylim<-c(0,1)
          },
          "r"={
            d2<-result$rIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2<-"z"
            }
            ylim<-rlim
          },
          "r1"={
            d2<-result$roIV
            disp2<-bquote(r[1])
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2<-bquote(z[1])
            }
            ylim<-rlim
          },
          "p1"={
            d2<-result$poIV
            if (pPlotScale=="log10"){
              ysc<-1
            }
            ylim<-c(0, 1)
            disp2<-bquote(p[1])
          },
          "rp"={
            d2<-result$rpIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2<-"zp"
            }
            ylim<-rlim
          },
          "n"={
            d2<-result$nval
            ylim<-c(1, 200*1.1)
            if (nscaleLog) {
              ysc<-2
              ylim<-c(log10(6), log10(200))
            }
          },
          "w"={
            d2<-rn2w(result$rIV,result$nval)
            if (wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "wp"={
            d2<-rn2w(result$rp,result$nval)
            if (wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "log(lrs)"={
            ylim<-c(-0.1, lrRange)
            disp2<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d2<-res2llr(result,"dLLR")
            if (any(d2<0)) {
              ylim<-c(-lrRange, lrRange)
            } else {
              ylim<-c(-0.1, lrRange)
            }
            disp2<-bquote(log[e](lr[d]))
          },
          "nw"={
            d2<-rw2n(result$rIV,0.8,result$design$sReplTails)
            ylim<-c(1, max(d1)*1.1)
          },
          "R"={
            d2<-result$rpIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2<-"Z"
            }
            ylim<-rlim
          }
  )
  if (disp1=="r" && disp2=="p") {
    show_line<-TRUE
  } else {
    show_line<-FALSE
  }
  
  if (xsc==1) {
    d1<-log10(d1)
    xlim<-c(log10(min_p), 0)
    disp1<-bquote(bold(log['10'] (.(disp1))))
  }
  if (ysc==1) {
    d2<-log10(d2)
    ylim<-c(log10(min_p), 0)
    disp2<-bquote(bold(log['10'] (.(disp2))))
  }
  if (ysc==2) {
    d2<-log10(d2)
    disp2<-bquote(bold(log['10'] (.(disp2))))
  }
  pts<-data.frame(x=d1,y=d2)
  
  g<-ggplot(pts,aes(x=x, y=y))
  
  if (show_line) {
    rs<-seq(-r_range,r_range,length.out=51)
    ps<-r2p(rs,result$nval[1])
    if (pPlotScale=="log10")  ps<-log10(ps)
    g<-g+geom_line(data=data.frame(x=rs,y=ps),aes(x=x,y=y),col="white")
  }
  
  dotSize=min(8,max(3.5,sqrt(400/length(d1))))
  dotSize<-dotSize<-(plotTheme$axis.title$size)/3

  if (!metaPlot && useSignificanceCols){
    c1=plotcolours$infer_sigC
    c2=plotcolours$infer_nsigC
  } else {
    c1=plotcolours$descriptionC
    c2=plotcolours$descriptionC
  }
  if (length(d1)<200) {
    use<-!isSignificant(STMethod,pvals,rvals,nvals,df1vals,result$evidence)
    pts1=pts[use,]
    g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=shapes$study, colour = "black", fill = c2, size = dotSize)
    pts2=pts[!use,]
    g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=shapes$study, colour = "black", fill = c1, size = dotSize)
  } else {
    if (length(d1)<=10000) {
      use<-!isSignificant(STMethod,pvals,rvals,nvals,df1vals,result$evidence)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=shapes$study, colour = c2, fill = c2, size = dotSize/4)
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=shapes$study, colour = c1, fill = c1, size = dotSize/4)
    } else {
      use<-d2<=maxnPlot
      pts<-data.frame(x=d1[use],y=d2[use])
      nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))
      g<-g+stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c(maincolours$graphBack,plotcolours$descriptionC))
    }
  }
  g<-g+theme(legend.position = "none")+plotTheme
  if (xsc==0) {
    g<-g+scale_x_continuous(limits = xlim)
  } else {
    g<-g+scale_x_continuous(limits = xlim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  if (ysc==0) {
    g<-g+scale_y_continuous(limits = ylim)
  }
  if (ysc==1) {
    g<-g+scale_y_continuous(limits = ylim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  if (ysc==2) {
    g<-g+scale_y_continuous(limits = ylim)
  }
  
  g<-g+xlab(disp1)+ylab(disp2)
  g+ggtitle(result$an_name)
}
