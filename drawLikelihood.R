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

colS="#FFCC88"
colSdark=darken(colS,off=-0.67)
colSsim=darken(colS,off=0.0)
  
colP="#8899DD"
colPdark=darken(colP,off=-0.67)
colPsim=darken(colP,off=-0.33)

colVline="#FF5500"
# colVline="#FFFFFF"
# colVline=darken(colP,off=-0.2)

colS1="#FFAA77"
colS2="#FFDD88"
colNullS=plotcolours$infer_nsigC
colDistS=darken(plotcolours$infer_sigC,off=-0.4)
highTransparency=0.25

wallHeight<-0.75

doConnecting<-TRUE
draw_lower_limit=0.01
doSampleLine<-FALSE
doPeakLine<-TRUE
doFloorLines<-FALSE 
doFloorCILines<-TRUE 
doCILines<-FALSE
doTextResult<-TRUE
showJointLk<-FALSE
showNull<-TRUE

drawLikelihood <- function(IV,DV,effect,design,likelihood,likelihoodResult){
  switch (likelihood$UseSource,
          "world"={likelihood$source<-likelihood$world},
          "prior"={likelihood$source<-likelihood$prior},
          "null"={likelihood$source<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=0,populationRZ="r",populationNullp=0)})
  # make the distribution        

  switch (likelihood$type,
          "Samples"={
            likelihoodResult<-likelihoodResult$samples
            ylab<-"Probability Density"
            col=colP
            col2=colS
          },
          "Populations"={
            likelihoodResult<-likelihoodResult$populations
            ylab<-"Likelihood"
            col=colS
            col2=colP
          }
  )

  pRho<-likelihoodResult$pRho
  pRho<-sort(pRho)
  sRho<-likelihoodResult$sRho

  rs<-likelihoodResult$Theory$rs
  sDens_r<-likelihoodResult$Theory$sDens_r
  sDens_r_total<-likelihoodResult$Theory$sDens_r_total
  sDens_r_null<-likelihoodResult$Theory$sDens_r_null
  sDens_r_plus<-likelihoodResult$Theory$sDens_r_plus
  rp<-likelihoodResult$Theory$rp
  pDens_r<-likelihoodResult$Theory$pDens_r
  spDens_r<-likelihoodResult$Theory$spDens_r
  pDens_r_null<-likelihoodResult$Theory$pDens_r_null
  pDens_r_plus<-likelihoodResult$Theory$pDens_r_plus

  # make the back wall population distributions
  rpw<-rp
  if (likelihood$type=="Samples") {
    rpw_dens<-likelihoodResult$Theory$asDens_r
  } else {
    rpw_dens<-likelihoodResult$Theory$apDens_r
  }

  # make the back wall sample distributions
  rsw<-likelihoodResult$Theory$rs
  rsw_dens_plus<-likelihoodResult$Theory$sDens_r_plus
  rsw_dens_null<-likelihoodResult$Theory$sDens_r_null
  rsw_dens<-rsw_dens_plus+rsw_dens_null

  view_lims<-c(-1,1)
  if (likelihood$viewRZ=="z") {
    view_lims<-c(-1,1)*z_range
  }
  
  rp_stats<-densityFunctionStats(spDens_r,rp)
  rp_peak<-rp_stats$peak
  rp_ci<-rp_stats$ci
  dens_at_peak<-rp_stats$dens_at_peak

  rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
  rsw_dens_plus<-rsw_dens_plus/max(rsw_dens_plus+rsw_dens_null,na.rm=TRUE)
  rsw_dens_null<-rsw_dens_null/max(rsw_dens_plus+rsw_dens_null,na.rm=TRUE)
  populationBackwall<-list(rpw=rpw,rpw_dens=rpw_dens,pDens_r=pDens_r,rp=rp)
  sampleBackwall<-list(rsw=rsw,rsw_dens_plus=rsw_dens_plus,rsw_dens_null=rsw_dens_null,rs=rs)
  
  n<-likelihoodResult$n[1]
  si<-1

  # graph frame
  switch (likelihood$view,
          "3D"= {
            tick_length<-0.05*view_lims[2]
            charExp<-1.3
            
            # make the floor
            x<-view_lims    
            y<-view_lims
            f <- function(x, y) { x*0+y*0 }
            z <- outer(x, y, f)
            z[is.na(z)] <- 0
            par(bg=maincolours$graphC,mar=c(0,5,0,0),font.lab=2)
            persp(x, y, z, zlim = range(c(0,1), na.rm = TRUE),
                  theta = likelihood$azimuth, phi = likelihood$elevation, r=likelihood$range, 
                  ticktype = "simple", 
                  axes = FALSE,
                  expand = 0.5, col = "#aaaaaa",
                  cex.axis=0.6,
                  xlab = "Populations", ylab = "Samples", zlab = ylab
            )->mapping
            
            if (view_lims[2]<=1) {
            plot_ticks=seq(view_lims[1],view_lims[2],0.25)
            } else {
              plot_ticks=seq(view_lims[1],view_lims[2],0.5)
            }
            tick.x.start <- trans3d(plot_ticks, view_lims[1], 0.0, mapping)
            tick.x.end <- trans3d(plot_ticks , view_lims[1]-tick_length, 0.0, mapping)
            tick.y.start <- trans3d(view_lims[2], plot_ticks, 0.0, mapping)
            tick.y.end <- trans3d(view_lims[2]+tick_length, plot_ticks , 0.0, mapping)
            
            ticks.x<-trans3d(plot_ticks+tick_length,view_lims[1]- tick_length*2*charExp,0,mapping)
            pos.x<-trans3d(view_lims[2]/2,1.3*view_lims[1],0,mapping)
            
            ticks.y<-trans3d(view_lims[2]+tick_length*charExp+0.02,plot_ticks-0.02,0,mapping)
            pos.y<-trans3d(1.3*view_lims[2],view_lims[1]/2,0,mapping)
            
            pos.z<-trans3d(-1*view_lims[2],-1.05*view_lims[2],0.5,mapping)
            
            segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
            segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
            
            text(ticks.x$x,ticks.x$y,plot_ticks,cex=0.6*charExp,adj=c(1,NA))
            text(ticks.y$x,ticks.y$y,plot_ticks,cex=0.6*charExp,adj=c(0,NA))
            if (likelihood$viewRZ=="r") {
              text(pos.x$x,pos.x$y,bquote(bold(r[population])),font=2,adj=c(1,1),cex=0.8*charExp)
              text(pos.y$x,pos.y$y,bquote(bold(r[sample])),font=2,adj=c(0,1),cex=0.8*charExp)
            } else {
              text(pos.x$x,pos.x$y,bquote(bold(z[population])),font=2,adj=c(1,1),cex=0.8*charExp)
              text(pos.y$x,pos.y$y,bquote(bold(z[sample])),font=2,adj=c(0,1),cex=0.8*charExp)
            }
            text(pos.z$x,pos.z$y,ylab,font=2,srt=90,cex=0.65*charExp)
            
            # general lines on the floor
            if (doFloorLines) {
            lines(trans3d(x=view_lims,y=c(0,0),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(0,0),y=view_lims,z=c(0,0),pmat=mapping),col="black",lty=3)
            }

            # make the back walls
            # population wall
            x<-populationBackwall$rpw
            y<-x*0+view_lims[2]
            z<-populationBackwall$rpw_dens
            polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(0,z,0)*wallHeight,pmat=mapping),col=addTransparency(colP,0.95))
            
            if (showJointLk && !any(is.na(populationBackwall$pDens_r))) {
              # show the joint likelihood function
              x<-populationBackwall$rp
              y<-x*0+view_lims[2]
              z<-populationBackwall$pDens_r
              polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(0,z,0)*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.5),border=NA)
            }
            
            # sample wall
              y<-sampleBackwall$rsw
              x<-y*0+view_lims[1]
              ztotal<-sampleBackwall$rsw_dens_plus+sampleBackwall$rsw_dens_null
              polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(0,ztotal,0)*wallHeight,pmat=mapping),col=addTransparency(colS,0.95))
              
              if (likelihood$source$worldOn && likelihood$source$populationNullp>0){
                if (!any(is.na(sampleBackwall$rsw_dens_null))) {
                  znull <- sampleBackwall$rsw_dens_null
                } else {
                  znull<-0
                }
                zplus<-sampleBackwall$rsw_dens_plus
                if (likelihood$source$populationNullp>0 ) {
                  lines(trans3d(x=x,y=y,z=znull*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                }
                lines(trans3d(x=x,y=y,z=zplus*wallHeight,pmat=mapping),col=colDistS,lwd=2)
              }
              
              if (likelihood$likelihoodTheory){
                # horizontal lines
                switch (likelihood$type,
                        "Samples"={
                          z<-approx(sampleBackwall$rsw,ztotal,sRho[si])$y
                          lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,z)*wallHeight,pmat=mapping),col=colSdark,lwd=2)
                          if (doFloorCILines) {
                            lines(trans3d(x=c(sRho[si],sRho[si]),y=view_lims,z=c(0,0),pmat=mapping),col=colSdark)
                            lines(trans3d(x=view_lims,y=c(sRho[si],sRho[si]),z=c(0,0),pmat=mapping),col=colSdark)
                          }
                          },
                        "Populations"={
                          if (showNull) {
                            lines(trans3d(x=c(0,0),y=view_lims,z=c(0,0),pmat=mapping),col=colVline,lwd=2)
                          }
                          if (!is.na(likelihood$targetSample)) {
                            # show peak and CIs on floor
                            if (doFloorCILines) {
                              y_ci<-view_lims
                              lines(trans3d(x=c(rp_peak,rp_peak),y=y_ci,z=c(0,0),pmat=mapping),col=colVline,lwd=2)
                              if (doCILines) {
                              lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=y_ci,z=c(0,0),pmat=mapping),col=colVline,lty=3,lwd=2)
                              lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=y_ci,z=c(0,0),pmat=mapping),col=colVline,lty=3,lwd=2)
                              }
                              
                              # show likelihood on sample back wall
                              zb<-approx(y,pDens_r_plus,sRho[si])$y
                              if (length(pDens_r_null)==length(pDens_r_plus)) {
                                za<-approx(y,pDens_r_null,sRho[si])$y
                                llrNull<-log(za/zb)
                                if (za>=zb) {
                                  lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                                  lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                                } else {
                                  lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                                  lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                                }
                              } else  {
                                lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(0,zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                              }
                              # show prob-dens on population back wall
                              dens_rp_peak<-approx(populationBackwall$rpw,populationBackwall$rpw_dens,rp_peak)$y
                              lines(trans3d(x=c(0,0)+rp_peak,y=c(0,0)+view_lims[2],z=c(0,dens_rp_peak)*wallHeight,pmat=mapping),col=colVline,lwd=2)

                              # show rp==rs on floor
                              if (likelihood$world$populationPDF!="Single"){
                                lines(trans3d(x=c(sRho[si],sRho[si]),y=y_ci,z=c(0,0),pmat=mapping),col=colPdark)
                              }
                            }
                          }
                        }
                )
              }
              
            theoryAlpha=1
            switch (likelihood$type,
                    "Samples"={
                      simAlpha<-1
                      if (length(pRho)>1) {
                        theoryAlpha<-0.85
                        simAlpha<-0.95
                      }
                      if (length(pRho)>11) {
                        if (!is.na(likelihood$targetSample) && !likelihood$cutaway) {
                          theoryAlpha<-theoryAlpha/2
                        } else {
                          theoryAlpha<-1
                        }
                      }
                      if (!is.null(likelihoodResult$Sims$sSimDens)) {
                        bins<-likelihoodResult$Sims$sSimBins
                        dens<-likelihoodResult$Sims$sSimDens
                        hgain<-sum(sDens_r)*diff(rs[c(1,2)])/sum(dens)/diff(bins[c(1,2)])
                          hgain<-hgain*(1-likelihood$world$populationNullp)
                        dens<-dens*hgain
                        theoryAlpha<-0.25
                        if (likelihood$cutaway) {
                          waste<-sum(bins<=sRho[si])
                          use_s<-(waste):length(bins)
                          bins<-bins[use_s]
                          bins[1]<-sRho[si]
                          use_s<-use_s[1:(length(use_s)-1)]
                        } else {
                          if (!is.matrix(dens)) {
                            dens<-t(dens)
                            sDens_r<-t(sDens_r)
                          } 
                          use_s<-(1:ncol(dens))
                        }
                        x1<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                      } else {dens<-NULL}

                      # simulations
                      for (i in 1:length(pRho)) {
                        # draw simulations
                        if (!is.null(dens)){
                          y1<-c(0,as.vector(matrix(c(dens[i,use_s],dens[i,use_s]),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x1*0+pRho[i],y=x1,z=y1,pmat=mapping),col=addTransparency(colSsim,simAlpha))
                        }
                        
                        # now draw theory
                        gain<-(1-likelihood$source$populationNullp)
                        if (length(pRho)==2) {
                          gain<-max(c(1-likelihood$source$populationNullp,likelihood$source$populationNullp))
                        } 
                        gain<-gain*0.75
                        if (likelihood$likelihoodTheory){
                          if (is.null(likelihoodResult$Sims$sSims)) {
                            if (length(pRho)==2) {
                              col<-addTransparency(colS,theoryAlpha)
                            } else {
                              col<-addTransparency(colS,theoryAlpha)
                            }
                          } else {
                            col<-addTransparency(colS,theoryAlpha)
                          }

                          z_use<-sDens_r[i,]
                          if (likelihood$cutaway) {
                            z_use[rs<sRho[si]]<-0
                          }
                          z_use[z_use<=draw_lower_limit]<-0
                          r_use<-rs
                          while (length(z_use)>0 && any(z_use>0)) {
                            use1<-which(z_use>0)[1]
                            z_use<-z_use[use1:length(z_use)]
                            r_use<-r_use[use1:length(r_use)]
                            use2<-which(c(z_use,0)==0)[1]-1
                            rs_draw<-r_use[c(1,1:use2,use2)]
                            z_draw<-c(0,z_use[1:use2],0)
                            polygon (trans3d(x = rs_draw*0+pRho[i], y = rs_draw, z = z_draw*gain, pmat = mapping), col = col, lwd=1)
                            if (use2==length(z_use)) break
                            z_use<-z_use[(use2+1):length(z_use)]
                            r_use<-r_use[(use2+1):length(r_use)]
                          }
                        }
                        # vertical lines
                        z<-approx(rs,sDens_r[i,],sRho[si])$y
                        # if (length(pRho)==1) {z<-1}
                        lines(trans3d(x=c(pRho[i],pRho[i]),y=c(sRho[si],sRho[si]),z=c(0,z)*gain,pmat=mapping),col=colVline, lwd=3)
                        # connecting lines
                        if (doConnecting && length(pRho)>5 && i<length(pRho)) {
                          z1<-approx(rs,sDens_r[i+1,],sRho[si])$y
                          lines(trans3d(x=c(pRho[i],pRho[i+1]),y=c(sRho[si],sRho[si]),z=c(z,z1)*gain,pmat=mapping),col=colVline, lwd=3)
                        }
                      }
                    },
                    "Populations"={
                      if (!is.null(likelihoodResult$Sims$pSims)) {
                        bins<-likelihoodResult$Sims$pSimBins
                        dens<-likelihoodResult$Sims$pSimDens$counts
                        
                        if (!is.null(dens)){
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          
                          # population wall
                          densP<-likelihoodResult$Sims$pSimDensP$counts
                          gainSim<-sum(densP)*(bins[2]-bins[1])
                          gainTheory<-sum(rpw_dens)*(rpw[2]-rpw[1])
                          densP<-densP/(gainSim/gainTheory)
                          if (max(densP)>1.2) {densP<-densP/max(densP)*1.2}
                          yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+view_lims[2],z=yP*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.35),border=NA)
                          
                          # sample wall
                          densS<-likelihoodResult$Sims$pSimDensS$counts
                          gainSim<-sum(densS)*(bins[2]-bins[1])
                          gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                          densS<-densS/(gainSim/gainTheory)
                          if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                          yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x*0+view_lims[1],y=x,z=yS*wallHeight,pmat=mapping),col = addTransparency(colSdark,0.25),border=NA)
                          
                          #slice of interest
                          gainSim<-sum(dens)*(bins[2]-bins[1])
                          gainTheory<-sum(likelihoodResult$Theory$spDens_r)*(likelihoodResult$Theory$rp[2]-likelihoodResult$Theory$rp[1])
                          dens<-dens/(gainSim/gainTheory)
                          dens<-dens/max(dens,na.rm=TRUE)
                          if (max(dens)>1.2) {dens<-dens/max(dens)*1.2}
                          y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+sRho[si],z=y1*wallHeight,pmat=mapping),col=colPsim,border=NA)
                        }
                      }
                      # draw theory main distribution & lines
                      if (likelihood$likelihoodTheory){
                        theoryAlpha=0.85
                        if (!is.na(likelihood$targetSample)) {
                          rd<-spDens_r
                          if (!is.null(spDens_r)){
                            use_si<-order(-sRho)
                            # main distribution
                            for (si in use_si) {
                              if (is.null(likelihoodResult$Sims$pSims)) {
                                polygon (trans3d(x = c(rp[1],rp,rp[length(rp)]), y = c(0,rp*0,0)+sRho[si], z = c(0,rd[si,],0)*wallHeight, pmat = mapping), col = addTransparency(colP,theoryAlpha), lwd=1)
                              } else {
                                polygon (trans3d(x = c(rp[1],rp,rp[length(rp)]), y = c(0,rp*0,0)+sRho[si], z = c(0,rd[si,],0)*wallHeight, pmat = mapping), col = addTransparency(colP,highTransparency), lwd=1)
                              }
                            }
                          }
                          # vertical lines on main distribution
                          if (doPeakLine && length(sRho)==1) {
                            if (showNull) {
                              lines(trans3d(x=c(0,0),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,rd[si,],0)$y-0.01)*wallHeight,pmat=mapping),col=colVline, lwd=2)
                            }
                            si<-1
                            lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,rd[si,],rp_peak)$y-0.01)*wallHeight,pmat=mapping),col=colVline, lwd=2)
                            if (doCILines) {
                            lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,rd[si,],rp_ci[1])$y-0.01)*wallHeight,pmat=mapping),col=colVline, lwd=2,lty=3)
                            lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,rd[si,],rp_ci[2])$y-0.01)*wallHeight,pmat=mapping),col=colVline, lwd=2,lty=3)
                            }
                            if (doSampleLine && likelihood$world$populationPDF!="Single"){
                              lines(trans3d(x=c(sRho[si],sRho[si]),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,rd[si,],sRho[si])$y-0.01)*wallHeight,pmat=mapping),col=colVline,lty=3,lwd=2)
                            }
                          }
                          # text annotations
                          if (likelihood$textResult || doTextResult) {
                            param<-likelihood$viewRZ
                            # llr 
                            if (effect$world$worldOn && likelihood$prior$populationNullp>0) {
                              text(trans3d(x=view_lims[1],y=mean(view_lims),z=1.05,pmat=mapping),labels=bquote(
                                llr(italic(.(param))["+"]/italic(.(param))[0])==bold(.(format(-llrNull,digits=3)))),
                                col=colSdark,adj=c(0.5,-0.5),cex=0.9)
                            }
                            # mle population
                            text(trans3d(x=rp_peak,y=view_lims[2],z=dens_rp_peak*wallHeight+0.05,pmat=mapping),labels=bquote(
                              italic(.(param))[mle]== bold(.(format(rp_peak,digits=3)))
                            ),col=colPdark,adj=-0.02,cex=0.9)
                            text(trans3d(x=mean(view_lims),y=view_lims[2],z=1.05,pmat=mapping),labels=bquote(
                              # llr(italic(r)[s]/italic(r)[0])==bold(.(format(log(dens_at_sample/approx(rp,pDens_r,0)$y),digits=3)))~";"~
                                llr(italic(.(param))[mle]/italic(.(param))[0])==bold(.(format(log(1/approx(rp,pDens_r,0)$y),digits=3)))
                            ),col=colPdark,adj=c(0.5,-0.5),cex=0.9)
                          }
                        }
                      }
                    }
            )

            # finish off plot box
            lines(trans3d(x=c(view_lims[1], view_lims[2], view_lims[2]),
                          y=c(view_lims[1],view_lims[1],view_lims[2]),
                          z=c(1,1,1),pmat=mapping), col="#888888", lty=3)        
            lines(trans3d(x=c(view_lims[2],view_lims[2]),y=c(view_lims[1],view_lims[1]),z=c(0,1),pmat=mapping),col="#888888",lty=3)
          },
  "Samples"={
    par(bg=maincolours$graphC)
    
  },
  "2D"={
    par(bg=maincolours$graphC,pin=c(1.33,1)*3,mar=c(5,5,1,0))
    
    # show the back wall
    switch (likelihood$type,
            "Populations"={
              rw<-rpw
              rw_dens<-rpw_dens
              if (likelihood$viewRZ=="z") {
                xlabel<-bquote(bold(z[p]))
              } else {
                xlabel<-bquote(bold(r[p]))
              }
              },
            "Samples"={
              rw<-rsw
              rw_dens<-rsw_dens
              if (likelihood$viewRZ=="z") {
                xlabel<-bquote(bold(z[s]))
              } else {
                xlabel<-bquote(bold(r[s]))
              }
            }
    )
    yh<-approx(rw,rw_dens,sRho[1])$y
    
    rw<-c(rw[1],rw,rw[length(rw)])
    rw_dens<-c(0,rw_dens,0)
    switch 
    plot(x=rw,y=rw_dens,xlab=xlabel,ylab=ylab,type="n",yaxt="n",font.lab=2,xlim=view_lims,ylim=c(0,1.25))
    axis(side = 2,  at=0, labels = FALSE, tck=-0.05)
    
        u <- par("usr") # The coordinates of the plot area
    rect(u[1], u[3], u[2], u[4], col="#AAAAAA", border=NA)
    lines(u[c(1,2)],c(0,0),col="black")
    
    # make the back wall
    polygon(x=rw,y=rw_dens,col="lightgrey")
    
    theoryAlpha=0.75
    # simulations
    switch (likelihood$type,
            "Populations"={
              if (!is.null(likelihoodResult$Sims$pSims)) {
                bins<-likelihoodResult$Sims$pSimBins
                dens<-likelihoodResult$Sims$pSimDens
                dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colP)
                  theoryAlpha=0.5
                }
              }
            },
            "Samples"={
              if (!is.null(likelihoodResult$Sims$sSims)) {
                bins<-likelihoodResult$Sims$sSimBins
                dens<-likelihoodResult$Sims$sSimDens
                # dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colS)
                  theoryAlpha=0.5

                }
              }
            }
    )
    lines(x=c(0,0)+sRho[1],y=c(0,yh),col="red", lwd=3)
    
    if (likelihood$likelihoodTheory){
      switch (likelihood$type,
              "Samples"={
                if (!all(is.na(sDens_r_total))){
                  # main distributions
                  # total
                  polygon (x = rs, y = sDens_r_total, col = addTransparency(colS,theoryAlpha), lwd=1)
                  # null
                  if (likelihood$world$worldOn) {
                    if (likelihood$world$populationNullp>0)
                      lines (x = rs, y = sDens_r_null, col = colNullS, lwd=1)
                  # plus
                  lines (x = rs, y = sDens_r_plus, col = colDistS, lwd=1)
                  }
                  
                  if ((!is.na(sRho[1]))) {
                    s<-sRho[1]
                    p_at_sample<-(sum(sDens_r_total[rs>=s])+sum(sDens_r_total[rs< -s]))/sum(sDens_r_total)
                    pn_at_sample<-(sum(sDens_r_null[rs>=s])+sum(sDens_r_null[rs< -s]))/sum(sDens_r_total)
                    pd_at_sample<-(sum(sDens_r_plus[rs>=s])+sum(sDens_r_plus[rs< -s]))/sum(sDens_r_total)
                    l_at_sample<-approx(rs,sDens_r_total,s)$y/mean(sDens_r_total)
                    ln_at_sample<-approx(rs,sDens_r_null,s)$y/mean(sDens_r_total)
                    ld_at_sample<-approx(rs,sDens_r_plus,s)$y/mean(sDens_r_total)
                    
                    if (likelihood$world$worldOn) {
                    if (ln_at_sample>ld_at_sample) {
                      lines(x=c(sRho[1],sRho[1]),y=c(ld_at_sample,ln_at_sample-0.01),col=colNullS,lwd=2)
                      lines(x=c(sRho[1],sRho[1]),y=c(0,ld_at_sample-0.01),col=colDistS,lwd=2)
                    } else {
                      if (likelihood$world$populationNullp>0)
                        lines(x=c(sRho[1],sRho[1]),y=c(ln_at_sample,ld_at_sample-0.01),col=colDistS,lwd=2)
                      lines(x=c(sRho[1],sRho[1]),y=c(0,ln_at_sample-0.01),col=colNullS,lwd=2)
                    }
                    
                      ptext<-bquote(
                        bolditalic(p)[.(likelihood$viewRZ)]== bold(.(format(p_at_sample,digits=3))) ~" "~ atop(phantom(bold(.(format(pd_at_sample,digits=3)))),phantom(bold(.(format(pn_at_sample,digits=3)))))
                      )
                      ltext<-bquote(
                        bolditalic(l)[.(likelihood$viewRZ)]==bold(.(format(l_at_sample,digits=3))) ~" "~ atop(phantom(bold(.(format(ld_at_sample,digits=3)))),phantom(bold(.(format(ln_at_sample,digits=3)))))
                      )
                    if (s>0)   {
                      text(s,1.05,labels=ptext,col=colPdark,adj=0,cex=0.9)
                      text(s,0.95,labels=ltext,col=colPdark,adj=0,cex=0.9)
                    } else  {
                      text(s,1.05,labels=ptext,col=colPdark,adj=1,cex=0.9)
                      text(s,0.95,labels=ltext,col=colPdark,adj=1,cex=0.9)
                    } 
                    if (likelihood$world$populationNullp>0) {
                      text(0,1.05,labels=bquote(
                        phantom(bolditalic(p)[.(likelihood$viewRZ)]== bold(.(format(p_at_sample,digits=3)))) ~" "~ atop(bold(.(format(pd_at_sample,digits=3))),phantom(bold(.(format(pn_at_sample,digits=3)))))
                      ),col=colDistS,adj=-0.1,cex=0.9)
                      text(0,1.05,labels=bquote(
                        phantom(bolditalic(p)[.(likelihood$viewRZ)]== bold(.(format(p_at_sample,digits=3)))) ~" "~ atop(phantom(bold(.(format(pd_at_sample,digits=3)))),bold(.(format(pn_at_sample,digits=3))))
                      ),col=colNullS,adj=-0.1,cex=0.9)
                    }

                    # text(0,1.05,labels=bquote(
                    #   phantom(bolditalic(l)[.(likelihood$viewRZ)]==bold(.(format(l_at_sample,digits=3)))) ~" "~ atop(bold(.(format(ld_at_sample,digits=3))),phantom(bold(.(format(ln_at_sample,digits=3)))))
                    # ),col=colDistS,adj=1.1,cex=0.9)
                    if (likelihood$world$populationNullp>0) {
                      text(0,1.05,labels=bquote(
                      phantom(bolditalic(l)[.(likelihood$viewRZ)]==bold(.(format(l_at_sample,digits=3)))) ~" "~ atop(phantom(bold(.(format(ld_at_sample,digits=3)))),bold(.(format(ln_at_sample,digits=3))))
                    ),col=colNullS,adj=1.1,cex=0.9)
                    }
                  } else {
                    s<-abs(sRho[1])
                    p_at_sample<-(sum(sDens_r_total[rs>=s])+sum(sDens_r_total[rs< -s]))/sum(sDens_r_total)
                    l_at_sample<-approx(rs,sDens_r_total,s)$y
                    
                    lines(x=c(sRho[1],sRho[1]),y=c(0,l_at_sample-0.01),col=colSdark,lwd=2)
                    
                    text(0,1.05,labels=bquote(
                      bolditalic(p)[.(likelihood$viewRZ)]== bold(.(format(p_at_sample,digits=3)))
                    ),col=colPdark,adj=-0.1,cex=0.9)
                    text(0,1.05,labels=bquote(
                      bolditalic(l)[.(likelihood$viewRZ)]==bold(.(format(l_at_sample,digits=3)))
                    ),col=colPdark,adj=1.1,cex=0.9)
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
                  if (likelihood$world$populationPDF!="Uniform_r" && !is.null(rp_peak)){
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
                    bolditalic(.(likelihood$viewRZ))[mle]== bold(.(format(rp_peak,digits=3)))
                  ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                  text(x=rp_peak,1.15,labels=bquote(
                    bold(llr)(bolditalic(.(likelihood$viewRZ))[mle]/bolditalic(.(likelihood$viewRZ))[0])==bold(.(format(log(1/approx(rp,pDens_r,0)$y),digits=3)))
                  ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                  
                  if (effect$world$worldOn && likelihood$prior$populationNullp>0) {
                    ln_at_sample<-approx(rs,pDens_r_null,sRho[1])$y
                    ld_at_sample<-approx(rs,pDens_r_plus,sRho[1])$y
                    llrNull<-log(ln_at_sample/ld_at_sample)
                    text(view_lims[1],1.15,labels=bquote(
                      bold(llr)(bolditalic(.(likelihood$viewRZ))["+"]/bolditalic(.(likelihood$viewRZ))[0])==bold(.(format(-llrNull,digits=3)))),
                      col=colPdark,adj=c(0),cex=0.9)
                  }
                  
                }
              }
      )
      
    }
  }
)
}
