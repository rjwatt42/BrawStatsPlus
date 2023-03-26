
n<-21
x<-seq(-3,3,length.out=n)


xp<-c(0,1,1,0)*(x[2]-x[1])
yp<-c(0,0,1,1)

v<-exp(-0.5*(x^2))


pts<-data.frame(x=rep(x,each=4)+rep(xp,n),y=rep(yp,n),value=rep(v,each=4),id=rep(1:n,each=4))

g<-ggplot(pts,aes(x=x,y=y))+geom_polygon(aes(x=x,y=y,alpha=value,group=id),fill="Yellow",show.legend = FALSE)
g

