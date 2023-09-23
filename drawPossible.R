addTransparency <- function(col,alpha) {
  col<-col2rgb(col)/255
  rgb(col[1],col[2],col[3],alpha)
}

darken <- function(col,gain=1,off=0) {
  col<-col2rgb(col)/255*gain+off
  col[col<0]<-0
  col[col>1]<-1
  rgb(col[1],col[2],col[3])
}

BoxCol<-"#666666"

colS="#FFCC88"
colSdark=darken(colS,off=-0.67)
colSsim=darken(colS,off=0.0)
  
colP="#AABBFF"
colPdark=darken(colP,off=-0.67)
colPsim=darken(colP,off=-0.33)

colVline="#FF5500"
colVline="#EEDD00"
colVline="#000000"
# colVline=darken(colP,off=-0.2)

colS1="#FFAA77"
colS2="#FFDD88"
colNullS=plotcolours$infer_nsigC
colDistS=darken(plotcolours$infer_sigC,off=-0.4)
colDistS=colPdark
highTransparency=0.25

scale3D<-1.1
wallHeight<-1
logZ<-FALSE

doConnecting<-TRUE
doSampleLine<-FALSE
doPeakLine<-TRUE
doFloorLines<-FALSE 
doFloorCILines<-TRUE 
doCILines<-FALSE
doTextResult<-TRUE
showJointLk<-FALSE
showNull<-TRUE
normSampDist<-FALSE

drawPossible <- function(IV,DV,effect,design,possible,possibleResult){
  switch(possible$type,
         "Samples"={
           switch (possible$UseSource,
                   "world"={possible$source<-possible$world},
                   "prior"={possible$source<-possible$prior},
                   "null"={possible$source<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=0,populationRZ="r",populationNullp=0)}
           )
         },
         "Populations"={
           switch (possible$UsePrior,
                   "world"={possible$source<-possible$world},
                   "prior"={possible$source<-possible$prior},
                   "none"={possible$source<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=0,populationRZ="r",populationNullp=0)}
           )
         }
      )
  # make the distribution        

  switch (possible$type,
          "Samples"={
            possibleResult<-possibleResult$samples
            switch(possible$show,
                   "Normal"={label.z<-"Probability Density"},
                   "Inverse"={label.z<-"Likelihood"},
                   "Power"={label.z<-"Probability Density"},
            )
            col=colP
            col2=colS
          },
          "Populations"={
            possibleResult<-possibleResult$populations
            switch(possible$show,
                   "Inverse"={label.z<-"Probability Density"},
                   "Normal"={label.z<-"Likelihood"},
                   "Power"={label.z<-"Likelihood"},
            )
            col=colS
            col2=colP
          }
  )

  pRho<-sort(possibleResult$pRho)
  sRho<-sort(possibleResult$sRho)

  rs<-possibleResult$Theory$rs
  sDens_r<-possibleResult$Theory$sDens_r
  sDens_r_total<-possibleResult$Theory$sDens_r_total
  sDens_r_null<-possibleResult$Theory$sDens_r_null
  sDens_r_plus<-possibleResult$Theory$sDens_r_plus
  rp<-possibleResult$Theory$rp
  pDens_r<-possibleResult$Theory$pDens_r
  spDens_r<-possibleResult$Theory$spDens_r
  pDens_r_null<-possibleResult$Theory$pDens_r_null
  pDens_r_plus<-possibleResult$Theory$pDens_r_plus
  if (possible$show=="Power") {
    spDens_r<-possibleResult$Theory$spDens_w
    rp<-possibleResult$Theory$wp
  }

  # make the back wall population distributions
  rpw<-rp
  if (possible$type=="Samples") {
    rpw_dens<-possibleResult$Theory$asDens_r
  } else {
    rpw_dens<-possibleResult$Theory$apDens_r
  }

  # make the back wall sample distributions
  rsw<-possibleResult$Theory$rs
  rsw_dens_plus<-possibleResult$Theory$sDens_r_plus
  rsw_dens_null<-possibleResult$Theory$sDens_r_null
  rsw_dens<-rsw_dens_plus+rsw_dens_null
  
  view_lims<-c(-1,1)
  if (RZ=="z") {
    view_lims<-c(-1,1)*z_range
  }
  xlim<-view_lims
  if (possible$show=="Power") {
    if (w_range[1]<0.5) {
      xlim<-c(0,1)
    } else{
      xlim<-w_range
    }
  }
  ylim<-view_lims
  zlim<-c(0,1)
  draw_lower_limit=0.01
  if (logZ) {
    if (possible$type=="Samples") {
      zlim<-c(-2,0)
    } else {
      zlim<-c(-5,0)
    }
    draw_lower_limit<-zlim[1]
  }
  zlim[2]<-zlim[2]+diff(zlim)*0.2
                       

  rp_stats<-densityFunctionStats(spDens_r,rp)
  rp_peak<-rp_stats$peak
  rp_ci<-rp_stats$ci
  dens_at_peak<-rp_stats$dens_at_peak

  rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
  if (!(possible$type=="Populations" && possible$UsePrior=="none")) {
    rsw_dens_plus<-rsw_dens_plus/max(rsw_dens_plus+rsw_dens_null,na.rm=TRUE)
    rsw_dens_null<-rsw_dens_null/max(rsw_dens_plus+rsw_dens_null,na.rm=TRUE)
    }
  populationBackwall<-list(rpw=rpw,rpw_dens=rpw_dens,pDens_r=pDens_r,rp=rp)
  sampleBackwall<-list(rsw=rsw,rsw_dens_plus=rsw_dens_plus,rsw_dens_null=rsw_dens_null,rs=rs)
  
  n<-possibleResult$n[1]

  # graph frame
  switch (possible$view,
          "3D"= {
            
            # make the floor
            longt<-1
            if (possible$show=="Power") {
              longt<-2.5
            }
            
            f <- function(x, y) { x*0+y*0 }
            z <- outer(xlim, xlim, f)+zlim[1]
            z[is.na(z)] <- zlim[1]
            par(bg=graphcolours$graphC,mar=c(0,5,0,0),font.lab=2)
            mapping<-persp(xlim,ylim,z, 
                           xlim=xlim+c(-1,1)*diff(xlim)*(scale3D-1),
                           ylim=ylim+c(-1,1)*diff(ylim)*(scale3D-1),
                           zlim = zlim, 
                           theta = possible$azimuth, phi = possible$elevation, r=possible$range, 
                           ticktype = "simple", 
                           box = FALSE,
                           axes = FALSE,
                           expand = 0.5, col = graphcolours$graphBack,
                           cex.axis=0.6,
                           xlab = "Populations", ylab = "Samples", zlab = label.z
            )
            # outside box            
            if (possible$boxed){
              polygon(trans3d(x=c(xlim[1], xlim[1], xlim[1],xlim[1]),
                              y=c(ylim[1], ylim[1], ylim[2],ylim[2]),
                              z=zlim[c(1, 2, 2,1)],mapping),
                      col=graphcolours$graphBack,border=NA
              )
              polygon(trans3d(x=c(xlim[1], xlim[1], xlim[2],xlim[2]),
                              y=c(ylim[2], ylim[2], ylim[2],ylim[2]),
                              z=zlim[c(1, 2, 2,1)],mapping),
                      col=graphcolours$graphBack,border=NA
              )
              lines(trans3d(x=c(xlim[1], xlim[1], xlim[2]),
                            y=c(ylim[1],ylim[2],ylim[2]),
                            z=c(zlim[2],zlim[2],zlim[2]),pmat=mapping), col=BoxCol)        
              lines(trans3d(x=c(xlim[1],xlim[1]),
                            y=c(ylim[1],ylim[1]),
                            z=zlim,pmat=mapping),col=BoxCol)
              lines(trans3d(x=c(xlim[1],xlim[1]),
                            y=c(ylim[2],ylim[2]),
                            z=zlim,pmat=mapping),col=BoxCol)
              lines(trans3d(x=c(xlim[2],xlim[2]),
                            y=c(ylim[2],ylim[2]),
                            z=zlim,pmat=mapping),col=BoxCol)
            }
            
            tick_grow<-2
            xtick_length<-0.02*diff(xlim)
            ytick_length<-0.02*diff(ylim)
            # z-axis
            lines(trans3d(x=c(xlim[1],xlim[1]),
                          y=c(ylim[1],ylim[1]),
                          z=zlim*wallHeight,pmat=mapping),col="black")
            # short ticks
            if (possible$boxed) {
              plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)*wallHeight
            } else {
              plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)*wallHeight
            }
            tick.z.start <- trans3d(xlim[1],ylim[1],plot_ticks, mapping)
            tick.z.end <- trans3d(xlim[1],ylim[1]-ytick_length,plot_ticks, mapping)
            segments(tick.z.start$x, tick.z.start$y, tick.z.end$x, tick.z.end$y)
            # long ticks
            long_ticks<-seq(zlim[1],zlim[2],diff(zlim)/2)*wallHeight
            tick.z.start <- trans3d(xlim[1],ylim[1],long_ticks, mapping)
            tick.z.end <- trans3d(xlim[1],ylim[1]-ytick_length*tick_grow,long_ticks, mapping)
            segments(tick.z.start$x, tick.z.start$y, tick.z.end$x, tick.z.end$y)
            # label
            pos.z<-trans3d(xlim[1],1.2*ylim[1],mean(zlim)*wallHeight,mapping)
            rotate.z=trans3d(x=c(xlim[1],xlim[1]),
                             y=c(ylim[1],ylim[1]),
                             z=zlim,pmat=mapping)
            rotate.z<-180+atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
            text(pos.z$x,pos.z$y,label.z,srt=rotate.z,font=2,cex=char3D*0.65)
            
            # x and y ticks
              
              plot_ticks<-seq(floor(xlim[1]*10)/10,xlim[2],0.1)
              long_ticks<-seq(floor(xlim[1]*2)/2,xlim[2],0.5/longt)
              if (possible$show=="Power") {
                plot_ticks<-c(0,plot_ticks)
                long_ticks<-c(0,long_ticks)
              } else {
                plot_ticks<-c(-rev(plot_ticks),0,plot_ticks)
                long_ticks<-c(-rev(long_ticks),0,long_ticks)
              }
            # short ticks  
            tick.x.start <- trans3d(plot_ticks, ylim[1], zlim[1], mapping)
            tick.x.end <- trans3d(plot_ticks , ylim[1]-ytick_length, zlim[1], mapping)
            segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
            # long ticks
            tick.x.start <- trans3d(long_ticks, ylim[1], zlim[1], mapping)
            tick.x.end <- trans3d(long_ticks , ylim[1]-ytick_length*tick_grow, zlim[1], mapping)
            segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
            # tick labels
            ticks.x<-trans3d(long_ticks+xtick_length,ylim[1]- ytick_length*tick_grow*char3D*1.2,zlim[1],mapping)
            text(ticks.x$x,ticks.x$y,long_ticks,cex=0.6*char3D,adj=c(1,NA))
            
              plot_ticks<-seq(0.1,xlim[2],0.1)
              long_ticks<-seq(0.5,xlim[2],0.5)
              plot_ticks<-c(-rev(plot_ticks),0,plot_ticks)
              long_ticks<-c(-rev(long_ticks),0,long_ticks)
            tick.y.start <- trans3d(xlim[2], plot_ticks, zlim[1], mapping)
            tick.y.end <- trans3d(xlim[2]+xtick_length, plot_ticks , zlim[1], mapping)
            segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
            # long ticks
            tick.y.start <- trans3d(xlim[2], long_ticks, zlim[1], mapping)
            tick.y.end <- trans3d(xlim[2]+xtick_length*tick_grow, long_ticks , zlim[1], mapping)
            segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
            # tick labels
            ticks.y<-trans3d(xlim[2]+xtick_length*tick_grow*char3D*1.5,long_ticks-0.02,zlim[1],mapping)
            text(ticks.y$x,ticks.y$y,long_ticks,cex=0.6*char3D,adj=c(0,NA))
            
            if (RZ=="r") {
              label.x<-bquote(bold(r['p']))
              label.y<-bquote(bold(r['s']))
            } else {
              label.x<-bquote(bold(z['p']))
              label.y<-bquote(bold(z['s']))
            }
            if (possible$show=="Power") label.x<-bquote(bold(w['s']))
            
            pos.x<-trans3d(sum(xlim)/2,ylim[1]-ytick_length*tick_grow*char3D*2.5,zlim[1],mapping)
            text(pos.x$x,pos.x$y,label.x,adj=c(1,1),font=2,cex=char3D*0.75)
            
            pos.y<-trans3d(xlim[2]+xtick_length*tick_grow*char3D*2.5,sum(ylim)/2,zlim[1],mapping)
            text(pos.y$x,pos.y$y,label.y,adj=c(0,1),font=2,cex=char3D*0.75)
            
            
            if (possible$show!="Power") {
              
# lines on the floor
              # general 
              if (doFloorLines) {
                lines(trans3d(x=xlim,y=c(0,0),z=c(0,0)+zlim[1],pmat=mapping),col="black",lty=3)
                lines(trans3d(x=c(0,0),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col="black",lty=3)
              }
              # populations 
              if (possible$type=="Populations" && !is.na(possible$targetSample)) {
                # show peak and CIs on floor
                if (doFloorCILines) {
                  lines(trans3d(x=c(rp_peak,rp_peak),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=2)
                  if (doCILines) {
                    lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lty=3,lwd=2)
                    lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lty=3,lwd=2)
                  }
                }
                # show rp==rs on floor
                if (possible$world$populationPDF!="Single"){
                  lines(trans3d(x=c(sRho[1],sRho[1]),y=ylim,z=c(0,0),pmat=mapping),col=colPdark)
                }
              }
                    

# population wall
              x<-populationBackwall$rpw
              y<-x*0+ylim[2]
              z<-populationBackwall$rpw_dens
              if (logZ) z<-log10(z)
              polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(zlim[1],z,zlim[1])*wallHeight,pmat=mapping),col=addTransparency(colP,0.95))
              
              if (showJointLk && !any(is.na(populationBackwall$pDens_r))) {
                # show the joint likelihood function
                x<-populationBackwall$rp
                y<-x*0+ylim[2]
                z<-populationBackwall$pDens_r
                if (logZ) z<-log10(z)
                polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(zlim[1],z,zlim[1])*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.5),border=NA)
              }
              if (possible$type=="Populations" && !is.na(possible$targetSample)) {
                # show peak likelihood on population back wall
                dens_rp_peak<-approx(populationBackwall$rpw,populationBackwall$rpw_dens,rp_peak)$y
                if (logZ) {
                  dens_rp_peak<-log10(dens_rp_peak)
                }
                lines(trans3d(x=c(0,0)+rp_peak,y=c(0,0)+view_lims[2],z=c(zlim[1],dens_rp_peak)*wallHeight,pmat=mapping),col=colVline,lwd=2)
              }
              
# sample wall
            # sampling distribution
              y<-sampleBackwall$rsw
              x<-y*0+view_lims[1]
              ztotal<-sampleBackwall$rsw_dens_plus+sampleBackwall$rsw_dens_null
              if (normSampDist) {
                ztotal<-ztotal/sum(ztotal,na.rm=TRUE)
                zgain<-1/max(ztotal,na.rm=TRUE)
                ztotal<-ztotal*zgain
              }
              if (logZ) {
                ztotal<-log10(ztotal)
                ztotal[ztotal<zlim[1]]<-zlim[1]
              }
              if (!(possible$type=="Populations" && possible$UsePrior=="none")) {
                polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(zlim[1],ztotal,zlim[1])*wallHeight,pmat=mapping),col=addTransparency(colS,0.95))
                # split into 2 parts  
                if (possible$source$worldOn && possible$source$populationNullp>0){
                  if (!any(is.na(sampleBackwall$rsw_dens_null))) {
                    znull <- sampleBackwall$rsw_dens_null
                  } else {
                    znull<-0
                  }
                  zplus<-sampleBackwall$rsw_dens_plus
                  if (normSampDist) {
                    znull<-znull/sum(znull,na.rm=TRUE)
                    zplus<-zplus/sum(zplus,na.rm=TRUE)
                    znull<-znull*zgain
                    zplus<-zplus*zgain
                  }
                  if (logZ) {
                    znull<-log10(znull)
                    znull[znull<zlim[1]]<-zlim[1]
                    zplus<-log10(zplus)
                    zplus[zplus<zlim[1]]<-zlim[1]
                  }
                  if (possible$source$populationNullp>0 ) {
                    lines(trans3d(x=x,y=y,z=znull*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                  }
                  lines(trans3d(x=x,y=y,z=zplus*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                }
              }
              
            # vertical lines
              if (possible$possibleTheory) {
                if (possible$type=="Samples") {
                  # show probability density on sample back wall
                  if (!isempty(sRho)){
                    for (si in 1:length(sRho)) {
                      z<-approx(sampleBackwall$rsw,ztotal,sRho[si])$y
                      if (logZ) z<-log10(z)
                      lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],z)*wallHeight,pmat=mapping),col=colVline,lwd=2)
                    }
                  }
                }
                if (possible$type=="Populations" &&!is.na(possible$targetSample)) {
                  # show likelihood on sample back wall
                  si=1;
                  if (possible$UsePrior!="none") {
                    za<-approx(y,sampleBackwall$rsw_dens_null,sRho[si])$y
                    zb<-approx(y,sampleBackwall$rsw_dens_plus,sRho[si])$y
                    llrNull<-log(za/zb)
                    if (logZ) {
                      za<-log10(za)
                      zb<-log10(zb)
                      za[za<zlim[1]]<-zlim[1]
                      zb[zb<zlim[1]]<-zlim[1]
                    }
                    if (za>=zb) {
                      lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                      lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                    } else {
                      lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                      lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                    }
                  # } else  {
                      # zb<-approx(y,ztotal,sRho[si])$y
                      # lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                  }
                }
              }
            }
            
# main distributions            
            theoryAlpha=1
            simAlpha<-1
            switch (possible$type,
                    "Samples"={
                      # draw simulations
                      if (length(pRho)>1) {
                        theoryAlpha<-0.85
                        simAlpha<-0.95
                      }
                      pgain<-(1-possible$source$populationNullp)
                      if (length(pRho)==2) {
                        pgain<-max(c(1-possible$source$populationNullp,possible$source$populationNullp))
                      } 
                      # prepare simulations first
                      if (!is.null(possibleResult$Sims$sSimDens)) {
                        theoryAlpha<-0.25
                        
                        bins<-possibleResult$Sims$sSimBins
                        dens<-possibleResult$Sims$sSimDens
                        simgain<-mean(sDens_r)/mean(dens)
                        dens<-dens*simgain*pgain
                          if (possible$cutaway) {
                            waste<-sum(bins<=min(sRho))
                            use_s<-(waste):length(bins)
                            bins<-bins[use_s]
                            bins[1]<-min(sRho)
                            use_s<-use_s[1:(length(use_s)-1)]
                          } else {
                            if (!is.matrix(dens)) {
                              dens<-t(dens)
                              sDens_r<-t(sDens_r)
                            } 
                            use_s<-(1:ncol(dens))
                        }
                      } 

                      # we interleave simulations and theory (because no hidden line removal)
                      for (i in 1:length(pRho)) {
                        # draw simulations
                        if (!is.null(possibleResult$Sims$sSimDens)){
                          x1<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          z1<-as.vector(matrix(c(dens[i,use_s],dens[i,use_s]),2,byrow=TRUE))
                          if (logZ) z1<-log10(z1)
                          z1[z1<zlim[1]]<-zlim[1]
                          polygon(trans3d(x=x1*0+pRho[i],y=x1,z=c(zlim[1],z1,zlim[1]),pmat=mapping),col=addTransparency(colSsim,simAlpha))
                        }
                        
                        # draw theory
                        if (possible$possibleTheory){
                          col<-addTransparency(colS,theoryAlpha)

                          z_use<-sDens_r[i,]*pgain
                          if (possible$cutaway) {
                            z_use[rs<min(sRho)]<-0
                          }
                          r_use<-rs
                          while (length(z_use)>0 && any(z_use>0)) {
                            use1<-which(z_use>0)[1]
                            z_use<-z_use[use1:length(z_use)]
                            r_use<-r_use[use1:length(r_use)]
                            use2<-which(c(z_use,0)==0)[1]-1
                            rs_draw<-r_use[c(1,1:use2,use2)]
                            z_draw<-z_use[1:use2]
                            if (logZ) z_draw<-log10(z_draw)
                            z_draw[z_draw<=draw_lower_limit]<-NA
                            polygon (trans3d(x = rs_draw*0+pRho[i], y = rs_draw, z = c(zlim[1],z_draw,zlim[1]), pmat = mapping), col = col, lwd=1)
                            if (use2==length(z_use)) break
                            z_use<-z_use[(use2+1):length(z_use)]
                            r_use<-r_use[(use2+1):length(r_use)]
                          }
                        }
                        # vertical lines on main distribution
                        if (!isempty(sRho)){
                          for (si in 1:length(sRho)) {
                            z<-approx(rs,sDens_r[i,],sRho[si])$y
                            z<-z*pgain
                            if (logZ) {
                              z<-log10(z)
                              z[z<zlim[1]]<-zlim[1]
                            }
                            lines(trans3d(x=c(pRho[i],pRho[i]),y=c(sRho[si],sRho[si]),z=c(zlim[1],z),pmat=mapping),col=colVline, lwd=1)
                            # connecting lines
                            if (doConnecting && length(pRho)>5 && i<length(pRho)) {
                              z1<-approx(rs,sDens_r[i+1,],sRho[si])$y
                              z1<-z1*pgain
                              if (logZ) {
                                z1<-log10(z1)
                                z1[z1<zlim[1]]<-zlim[1]
                              }
                              lines(trans3d(x=c(pRho[i],pRho[i+1]),y=c(sRho[si],sRho[si]),z=c(z,z1),pmat=mapping),col=colVline, lwd=1)
                            }
                          }
                        }
                      }
                    },
                    "Populations"={
                      if (!is.null(possibleResult$Sims$pSims)) {
                        if (possible$show!="Power") {
                          bins<-possibleResult$Sims$pSimBins
                          dens<-possibleResult$Sims$pSimDens$counts
                        } else {
                          bins<-possibleResult$Sims$pSimBinsW
                          dens<-possibleResult$Sims$pSimDensW$counts
                        }
                        
                        if (!is.null(dens)){
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          
                          if (possible$show!="Power") {
                          # population wall
                          densP<-possibleResult$Sims$pSimDensP$counts
                          gainSim<-sum(densP)*diff(bins[1:2])
                          gainTheory<-sum(rpw_dens)*diff(rpw[1:2])
                          densP<-densP/gainSim*gainTheory
                          if (max(densP)>1.2) {densP<-densP/max(densP)*1.2}
                          yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                          if (logZ) yP<-log10(yP)
                          polygon(trans3d(x=x,y=x*0+view_lims[2],z=yP*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.35),border=NA)
                          
                          # sample wall
                          densS<-possibleResult$Sims$pSimDensS$counts
                          gainSim<-sum(densS)*(bins[2]-bins[1])
                          gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                          densS<-densS/(gainSim/gainTheory)
                          if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                          yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                          if (logZ) yS<-log10(yS)
                          polygon(trans3d(x=x*0+view_lims[1],y=x,z=yS*wallHeight,pmat=mapping),col = addTransparency(colSdark,0.25),border=NA)
                          }
                          
                          #slice of interest
                          si=1
                          gainSim<-sum(dens)*diff(bins[1:2])
                          gainTheory<-sum(possibleResult$Theory$spDens_r)*diff(possibleResult$Theory$rp[1:2])
                          dens<-dens/(gainSim/gainTheory)
                          # dens<-dens/max(dens,na.rm=TRUE)
                          # if (max(dens)>1.2) {dens<-dens/max(dens)*1.2}
                          y1<-as.vector(matrix(c(dens,dens),2,byrow=TRUE))
                          if (logZ) {
                            y1<-log10(y1)
                            y1[y1<zlim[1]]<-zlim[1]
                          }
                          polygon(trans3d(x=x,y=x*0+sRho[si],z=c(zlim[1],y1,zlim[1]),pmat=mapping),col=colPsim,border=NA)
                        }
                      }
                      # draw theory main distribution & lines
                      if (possible$possibleTheory){
                        theoryAlpha=0.85
                        if (!is.na(possible$targetSample)) {
                          rd<-spDens_r
                          if (logZ) {
                            rd<-log10(rd)
                            rd[rd<zlim[1]]<-zlim[1]
                          }
                          if (!is.null(spDens_r)){
                            use_si<-order(-sRho)
                            # main distribution
                            for (si in use_si) {
                              if (is.null(possibleResult$Sims$pSims)) {
                                polygon (trans3d(x = c(rp[1],rp,rp[length(rp)]), y = c(0,rp*0,0)+sRho[si], z = c(zlim[1],rd[si,],zlim[1]), pmat = mapping), col = addTransparency(colP,theoryAlpha), lwd=1)
                              } else {
                                polygon (trans3d(x = c(rp[1],rp,rp[length(rp)]), y = c(0,rp*0,0)+sRho[si], z = c(zlim[1],rd[si,],zlim[1]), pmat = mapping), col = addTransparency(colP,highTransparency), lwd=1)
                              }
                            }
                          }
                          
                          # vertical lines on main distribution
                          if (possible$show!="Power") {
                          if (doPeakLine && length(sRho)==1) {
                            if (showNull) {
                              lines(trans3d(x=c(0,0),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],0)$y-0.01),pmat=mapping),col=colVline, lwd=2)
                            }
                            si<-1
                            if (rp_peak==0) colHere<-colNullS else colHere<-colVline
                            lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_peak)$y-0.01),pmat=mapping),col=colHere, lwd=2)
                            if (doCILines) {
                            lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[1])$y-0.01),pmat=mapping),col=colVline, lwd=2,lty=3)
                            lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[2])$y-0.01),pmat=mapping),col=colVline, lwd=2,lty=3)
                            }
                            if (doSampleLine && possible$world$populationPDF!="Single"){
                              lines(trans3d(x=c(sRho[si],sRho[si]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],sRho[si])$y-0.01),pmat=mapping),col=colVline,lty=3,lwd=2)
                            }
                          }
                          # text annotations
                          if (possible$textResult || doTextResult) {
                            param<-RZ
                            # llr 
                            if (possible$UsePrior!="none") {
                              text(trans3d(x=view_lims[1],y=mean(view_lims),z=zlim[2]*1.05,pmat=mapping),labels=bquote(
                                llr(italic(.(param))["+"]/italic(.(param))[0])==bold(.(format(-llrNull,digits=3)))),
                                col=colSdark,adj=c(0.5,-0.5),cex=0.9)
                            }
                            # mle population
                            text(trans3d(x=rp_peak,y=view_lims[2],z=dens_rp_peak*wallHeight+0.05,pmat=mapping),labels=bquote(
                              italic(.(param))[mle]== bold(.(format(rp_peak,digits=3)))
                            ),col=colPdark,adj=-0.02,cex=0.9)
                            text(trans3d(x=mean(view_lims),y=view_lims[2],z=zlim[2]*1.05,pmat=mapping),labels=bquote(
                              # llr(italic(r)[s]/italic(r)[0])==bold(.(format(log(dens_at_sample/approx(rp,pDens_r,0)$y),digits=3)))~";"~
                                llr(italic(.(param))[mle]/italic(.(param))[0])==bold(.(format(log(1/approx(rp,pDens_r,0)$y),digits=3)))
                            ),col=colPdark,adj=c(0.5,-0.5),cex=0.9)
                          }
                          }
                        }
                      }
                    }
            )

            # finish off plot box
            if (possible$boxed){
            lines(trans3d(x=c(view_lims[1], view_lims[2], view_lims[2]),
                          y=c(view_lims[1],view_lims[1],view_lims[2]),
                          z=c(1,1,1)*zlim[2],pmat=mapping), col=BoxCol, lty=3)        
            lines(trans3d(x=c(view_lims[2],view_lims[2]),y=c(view_lims[1],view_lims[1]),z=zlim,pmat=mapping),col=BoxCol,lty=3)
            }
          },
  "2D"={
    par(bg=graphcolours$graphC,pin=c(1.33,1)*3,mar=c(5,5,1,1))
    
    # show the back wall
    switch (possible$type,
            "Populations"={
              rw<-rpw
              rw_dens<-rpw_dens
              if (RZ=="z") {
                xlabel<-bquote(bold(z['p']))
              } else {
                xlabel<-bquote(bold(r['p']))
              }
              col<-colP
              },
            "Samples"={
              rw<-rsw
              rw_dens<-rsw_dens
              if (RZ=="z") {
                xlabel<-bquote(bold(z['s']))
              } else {
                xlabel<-bquote(bold(r['s']))
              }
              col<-colS
            }
    )
    
    rwd<-c(rw[1],rw,rw[length(rw)])
    rwd_dens<-c(0,rw_dens,0)
    plot(x=rwd,y=rwd_dens,xlab=xlabel,ylab=label.z,type="n",yaxt="n",font.lab=2, cex.lab=char3D,
         xlim=xlim,ylim=zlim)
    axis(side = 2,  at=0, labels = FALSE, tck=-0.05)

    # gray background
    u <- par("usr") # The coordinates of the plot area
    rect(u[1], u[3], u[2], u[4], col=graphcolours$graphBack, border=NA)
    lines(u[c(1,2)],c(0,0),col="black")
    
    # make the back wall
    polygon(x=rwd,y=rwd_dens,col=addTransparency(col,0.2))
    lines(x=rw,y=rw_dens,col=colDistS,lwd=2)
    if (possible$type=="Populations" && possible$source$populationNullp>0) {
      lines(x=c(-1,-1,1,1)*0.02,y=c(0,1,1,0),col=addTransparency(colNullS,0.2),lwd=2)
    }
    
    theoryAlpha=1
    # simulations
    switch (possible$type,
            "Populations"={
              if (!is.null(possibleResult$Sims$pSims)) {
                bins<-possibleResult$Sims$pSimBins
                dens<-possibleResult$Sims$pSimDens
                dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colP)
                  theoryAlpha=0.25
                }
              }
            },
            "Samples"={
              if (!is.null(possibleResult$Sims$sSims)) {
                bins<-possibleResult$Sims$sSimBins
                dens<-possibleResult$Sims$sSimDens
                # dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colS)
                  theoryAlpha=0.25

                }
              }
            }
    )
    zpSample<-approx(rw,rw_dens,sRho[1])$y
    lines(x=c(0,0)+sRho[1],y=c(0,zpSample),col="red", lwd=3)
    
    if (possible$possibleTheory){
      switch (possible$type,
              "Samples"={
                if (!all(is.na(sDens_r_total))){
                  # main distributions
                  # total
                  polygon (x = rs[c(1,1:length(rs),length(rs))], y = c(0,sDens_r_total,0), col = addTransparency(colS,theoryAlpha), lwd=1)
                  # null
                  if (possible$world$worldOn) {
                    if (possible$world$populationNullp>0)
                      lines (x = rs, y = sDens_r_null, col = colNullS, lwd=2)
                      # plus
                      lines (x = rs, y = sDens_r_plus, col = colDistS, lwd=2)
                  }
                  
                  if (!all(is.na(sRho))) {
                    for (i in 1:length(sRho)) {
                    s<-sRho[i]
                    gain<-sum(sDens_r_total)*diff(rs[1:2])
                    p_at_sample<-(sum(sDens_r_total[rs>=s])+sum(sDens_r_total[rs< -s]))/sum(sDens_r_total)
                    pn_at_sample<-(sum(sDens_r_null[rs>=s])+sum(sDens_r_null[rs< -s]))/sum(sDens_r_total)
                    pd_at_sample<-(sum(sDens_r_plus[rs>=s])+sum(sDens_r_plus[rs< -s]))/sum(sDens_r_total)
                    
                    l_at_sample<-approx(rs,sDens_r_total,s)$y#/mean(sDens_r_total)
                    ln_at_sample<-approx(rs,sDens_r_null,s)$y#/mean(sDens_r_total)
                    ld_at_sample<-approx(rs,sDens_r_plus,s)$y#/mean(sDens_r_total)
                    
                    lines(x=c(sRho[i],sRho[i]),y=c(0,l_at_sample),col=colVline,lwd=1)
                    points(x=sRho[i],y=l_at_sample,col=colVline,pch=16,cex=1.5)
                    if (possible$world$worldOn) {
                      if (length(sRho)<=10) {
                        ptext<-bquote(
                          bolditalic(p)[.(RZ)]== bold(.(format(p_at_sample,digits=3))) ~" "~ atop(phantom(bold(.(format(pd_at_sample,digits=3)))),phantom(bold(.(format(pn_at_sample,digits=3)))))
                        )
                        ltext<-bquote(
                          bold(pd(.(RZ)[s]))==.(format(l_at_sample/gain,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                        )
                        ltext<-format(log(l_at_sample),digits=3)
                        if (s>0)   {
                          # text(s,0.95,labels=ptext,col=colPdark,adj=0,cex=0.9)
                          text(s+0.05,l_at_sample+0.05,labels=ltext,col="black",adj=0,cex=0.9)
                        } else  {
                          # text(s,0.95,labels=ptext,col=colPdark,adj=1,cex=0.9)
                          text(s-0.05,l_at_sample+0.05,labels=ltext,col="black",adj=0.6,cex=0.9)
                        } 
                      }
                    } else {
                      s<-abs(sRho[i])
                      p_at_sample<-(sum(sDens_r_total[rs>=s])+sum(sDens_r_total[rs< -s]))/sum(sDens_r_total)
                      l_at_sample<-approx(rs,sDens_r_total,s)$y
                      
                      lines(x=c(sRho[1],sRho[1]),y=c(0,l_at_sample-0.01),col=colSdark,lwd=2)
                      
                      text(0,1.05,labels=bquote(
                        bolditalic(p)[.(RZ)]== bold(.(format(p_at_sample,digits=3)))
                      ),col=colPdark,adj=-0.1,cex=0.9)
                      text(0,1.05,labels=bquote(
                        bolditalic(l)[.(RZ)]==bold(.(format(l_at_sample/gain,digits=3)))
                      ),col=colPdark,adj=1.1,cex=0.9)
                    }
                    }
                    if (length(sRho)>1) {
                      l_at_sample<-sum(log(approx(rs,sDens_r_total,sRho)$y),na.rm=TRUE)#/mean(sDens_r_total)
                      ltext<-bquote(
                        bold(log(lk(.(RZ)[s])))==.(format(l_at_sample,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                      )
                      ltext<-bquote(
                        bold(log(lk(Z)))==.(format(l_at_sample,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                      )
                      text(0+0.05,1+0.15,labels=ltext,col="black",adj=0.5,cex=0.9)
                    }
                  }
                }
              },
              "Populations"={
                if (!all(is.na(pDens_r))){
                  # main distribution
                  polygon (x = rp, y = pDens_r, col = addTransparency(colP,theoryAlpha), lwd=1)
                  # vertical lines
                  dens_at_peak<-max(pDens_r)
                  dens_at_sample<-approx(rp,pDens_r,sRho[1])$y
                  dens_at_ci<-approx(rp,pDens_r,rp_ci)$y
                  lines(x=c(sRho[1],sRho[1]),y=c(0,dens_at_sample-0.01),col="black",lwd=1.5)
                  if (possible$world$populationPDF!="Uniform_r" && !is.null(rp_peak)){
                    lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="black",lwd=2.5)
                    lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="white",lwd=2)
                    lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="red",lty=3,lwd=2)
                    
                    lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="black",lwd=2.5)
                    lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="white",lwd=2)
                    lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="red",lty=3,lwd=2)
                    lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="black",lwd=2.5)
                    lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="white",lwd=2)
                    lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="red",lty=3,lwd=2)
                  }
                  text(rp_peak,1.05,labels=bquote(
                    bolditalic(.(RZ))[mle]== bold(.(format(rp_peak,digits=3)))
                  ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                  text(x=rp_peak,1.15,labels=bquote(
                    bold(llr)(bolditalic(.(RZ))[mle]/bolditalic(.(RZ))[0])==bold(.(format(log(1/approx(rp,pDens_r,0)$y),digits=3)))
                  ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                  
                  if (effect$world$worldOn && possible$prior$populationNullp>0) {
                    ln_at_sample<-approx(rs,pDens_r_null,sRho[1])$y
                    ld_at_sample<-approx(rs,pDens_r_plus,sRho[1])$y
                    llrNull<-log(ln_at_sample/ld_at_sample)
                    text(view_lims[1],1.15,labels=bquote(
                      bold(llr)(bolditalic(.(RZ))["+"]/bolditalic(.(RZ))[0])==bold(.(format(-llrNull,digits=3)))),
                      col=colPdark,adj=c(0),cex=0.9)
                  }
                  
                }
              }
      )
      
    }
  }
)
}
