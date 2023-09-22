inspectMainGraph<-function(inspect) {
  
  var<-inspect$var
  data<-inspect$data
  data<-data[!is.na(data)]
  
  # start
  g<-ggplot()
  
  if (!is.null(data)) {
    
    g<-showMean(g,inspect)
    g<-showSD(g,inspect)    
    
    if (var$type=="Categorical") {data<-as.numeric(data)+runif(length(data),-1,1)*0.1}
    # data points
    switch(inspect$inspectOrder,
           "unsorted"={y<-1:length(data)},
           "sorted"={y<-rank(data,ties.method="first")},
           "piled"={y<-pile(data)}
           )
    
    ptSize<-max(3,min(8,sqrt(42)*8/sqrt(length(data))))
    pts<-data.frame(x=data,y=y)
    g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$data, colour="black", fill=plotcolours$sampleC,size=ptSize)
    mn<-mean(data)
    
    # showing the residuals
    if (inspect$showResiduals) {
      # vertical line
      g<-g+geom_vline(xintercept=inspect$ResidVal, colour = "black", lwd=1.5)
      # horizontal lines      
      switch (var$type,
              "Categorical"={
              },
              "Ordinal"={
                for (i in 1:inspect$n) {
                  if (!is.na(data[i])) {
                    col<-sign(inspect$ResidVal-data[i])
                  col<-col/2+0.5
                  col<-rgb(col,col,col)
                  g<-g+geom_segment(data=data.frame(x=data[i],xend=inspect$ResidVal,y=y[i],yend=y[i]),aes(x=x,y=y,xend=xend,yend=yend), colour=col, lwd=1  )
                  }
                }
              },
              "Interval"={
                for (i in 1:inspect$n) {
                  if (!is.na(data[i])) {
                    col=(inspect$ResidVal-data[i])/var$sd/2
                    col=max(min(col/2+0.5,1),0)
                    col=rgb(col,col,col)
                    g<-g+geom_segment(data=data.frame(x=data[i],xend=inspect$ResidVal,y=y[i],yend=y[i]),aes(x=x,y=y,xend=xend,yend=yend), colour=col, lwd=1  )
                  }
                }
              }
      )
    }
  }
  
  # wind up
  g<-g+scale_y_continuous(breaks=NULL)
  switch (var$type,
          "Categorical"+{
            g<-g+scale_x_continuous(breaks=1:var$ncats,labels=var$cases)
            g<-g+coord_cartesian(xlim=c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          },
          "Ordinal"={
            g<-g+scale_x_continuous(breaks=1:var$nlevs)
            g<-g+coord_cartesian(xlim=c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          },
          "Interval"={
            g<-g+scale_x_continuous()
            g<-g+coord_cartesian(xlim=c(-1,1)*3*var$sd+var$mu,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          }
          )
  g+labs(x=var$name,y=NULL)+diagramTheme
}


inspectPenaltyGraph<-function(inspect) {

  var<-inspect$var
  data<-inspect$data
  data<-data[!is.na(data)]
  
  if (!is.null(data) && inspect$showResiduals) {
    g<-ggplot()
    g<-showMean(g,inspect)
    g<-showSD(g,inspect)    
    
    # vertical line
    if (inspect$inspectHistory[length(inspect$inspectHistory)]!=inspect$ResidVal)
    {inspect$inspectHistory<-c(inspect$inspectHistory,inspect$ResidVal)}
    
    y=c()
    x<-sort(inspect$inspectHistory)
    
    for (i in 1:length(x)) {
      if (inspect$whichResiduals=="1") {
        switch(var$type,
               "Categorical"={
                 val<-mean(as.numeric(data)!=round(x[i]))
               },
               "Ordinal"={
                 if (var$discrete) {mval<-round(x[i])} else {mval<-x[i]}
                 val<-mean(sign(data-mval))
               },
               "Interval"={
                 val<-mean(data-x[i])/2/var$sd
               }
        )
      } else {
        switch(var$type,
               "Categorical"={
                 val<-(sum(data!=round(x[i]))+(length(data)-sum(data==round(x[i]))))/length(data)/var$ncats
               },
               "Ordinal"={
                 # sum(outside iqr)-sum(inside iqr)
                 if (var$discrete) {mval<-round(x[i])} else {mval<-x[i]}
                 q1<-median(data[data<mval],na.rm=TRUE)
                 q2<-median(data[data>mval],na.rm=TRUE)
                 r1=sum(data<q1,na.rm=TRUE)+sum(data>q2,na.rm=TRUE)
                 r2<-sum(data>q1 & data<q2,na.rm=TRUE)
                 val<-r1-r2
               },
               "Interval"={
                 # squared residual
                 val<-sqrt(mean((data-x[i])^2,na.rm=TRUE))/2/var$sd
               }
        )
      }
    if (val<0) {
      g<-g+geom_polygon(data=data.frame(x=c(-1,-1,1,1)/50+x[i],y=c(0,val,val,0)),aes(x=x,y=y),colour="white",fill="white")
      g<-g+geom_hline(yintercept=0,colour="white",lwd=1)
    } else {
      g<-g+geom_polygon(data=data.frame(x=c(-1,-1,1,1)/50+x[i],y=c(0,val,val,0)),aes(x=x,y=y),colour="black",fill="black")
      g<-g+geom_hline(yintercept=0,colour="black",lwd=1)
    }
      y<-c(y,val)
      if (i>1) {
        pts<-data.frame(x=x[i-1],y=y[i-1],xend=x[i],yend=y[i])
        g<-g+geom_segment(data=pts,aes(x=x,y=y,xend=xend,yend=yend),colour="yellow",lwd=0.5)
      }
    }
  } else {
    return(ggplot()+plotBlankTheme)
  }
  
  # wind up
  g<-g+scale_y_continuous(breaks=0)
  switch (var$type,
          "Categorical"+{
            g<-g+scale_x_continuous(breaks=1:var$ncats,labels=var$cases)
            g<-g+coord_cartesian(xlim=c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10,ylim=c(-1,1))
          },
          "Ordinal"={
            g<-g+scale_x_continuous(breaks=1:var$nlevs)
            g<-g+coord_cartesian(xlim=c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10,ylim = c(-1,1))
          },
          "Interval"={
            g<-g+scale_x_continuous()
            g<-g+coord_cartesian(xlim=c(-1,1)*3*var$sd+var$mu,ylim = c(-1,1))
          }
  )
  # g+labs(x=var$name,y=paste("Residuals^(",inspect$whichResiduals,")",sep=""))+diagramTheme
  switch (inspect$whichResidual,
          "1"={g<-g+labs(x=var$name,y=bquote(Residuals^1))},
          "2"={g<-g+labs(x=var$name,y=bquote(Residuals^2))}
          )
  g+diagramTheme

  
}


pile<-function(data) {
  x<-c()
  y<-c()
  space<-5/length(data)
  data<-data-min(data)
  data<-data/max(data)
  
  for (i in 1:length(data)){
    for (iy in seq(0,10,by=space)) {
      distances=sqrt((x-data[i])^2+(y-iy)^2)
      found<-(distances<space)
      if (!any(found)) {
        x<-c(x,data[i])
        y<-c(y,iy)
        break
      }
    }
  }
  return(y/max(y)*length(data))
}


showMean<-function(g,inspect) {
  if (inspect$showMean) {
    var<-inspect$var
    data<-inspect$data
    data<-data[!is.na(data)]
    if (var$type=="Categorical") {data<-as.numeric(data)}
    # show mean
    # vertical line
    switch (var$type,
            "Categorical"={
              g<-g+geom_vline(xintercept=Mode(data), colour = "red", lwd=2)
            },
            "Ordinal"={
              g<-g+geom_vline(xintercept=median(data), colour = "red", lwd=2)
            },
            "Interval"={
              g<-g+geom_vline(xintercept=mean(data), colour = "red", lwd=2)
            }
    )
  }
  return(g)
}

showSD<-function(g,inspect) {
  if (inspect$showSd) {
    var<-inspect$var
    data<-inspect$data
    data<-data[!is.na(data)]
    if (var$type=="Categorical") {data<-as.numeric(data)}
    # show sd
    # vertical lines
    switch (var$type,
            "Categorical"={
            },
            "Ordinal"={
              g<-g+geom_vline(xintercept=quantile(data,0.25), colour = "red", lwd=1)
              g<-g+geom_vline(xintercept=quantile(data,0.75), colour = "red", lwd=1)
            },
            "Interval"={
              g<-g+geom_vline(xintercept=mean(data)+std(data,1), colour = "red", lwd=1)
              g<-g+geom_vline(xintercept=mean(data)-std(data,1), colour = "red", lwd=1)
            }
    )
  }
  return(g)
  
}
