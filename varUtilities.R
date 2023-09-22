
CatProportions<-function(var) {
  if (is.numeric(var$proportions)) {
    pp<-var$proportions
  } else {
    pp<-as.numeric(unlist(strsplit(var$proportions,",")))
  }
  pp<-pp/max(pp)
}


OrdProportions<-function(var) {
  if (isempty(var$ordProportions) || is.na(var$ordProportions)) {
    ng<-var$nlevs
    centre<-((var$median-(ng+1)/2)/ng+0.5)
    concentration<-1/(var$iqr/2)*10
    alphaK<-1+centre*(concentration-2)
    betaK<-1+(1-centre)*(concentration-2)
    pp<-dbeta(seq(0,1,1/(ng+1)),alphaK,betaK)
    pp<-pp[2:(ng+1)]
    # pp<-exp(-0.5*(((1:ng)-(ng+1)/2)/(var$iqr/2)^2)
  } else {
    pp<-as.numeric(unlist(strsplit(var$ordProportions,",")))
  }
  pp<-pp/max(pp)
}

r2CatProportions<-function(rho,ncats1,ncats2) {
  
  # find proportions in each cell
  sigma<-matrix(c(1,rho,rho,1),nrow=2)
  mu<-c(0,0)
  
  xbreaks<-qnorm(seq(0,1,1/ncats1))
  ybreaks<-qnorm(seq(0,1,1/ncats2))
  division<-matrix(ncol=ncats1+1,nrow=ncats2)
  for (ix in 1:ncats1+1){
    whole<-pmnorm(c(xbreaks[ix],Inf),mu,sigma)
    divis<-pmnorm(matrix(c(rep(xbreaks[ix],ncats2+1),ybreaks),ncol=2,byrow=FALSE),mu,sigma)
    division[,ix]<-diff(divis)
  }
  division[,1]<-0
  division<-t(diff(t(division)))
  division
}


r2OrdProportions<-function(rho,ng) {
  
}
