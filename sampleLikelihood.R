
ZeroSamplingPDF<-function(z,sigma) {
  exp(-0.5*(z^2/sigma^2))/sqrt(2*pi*sigma^2)
}
ZeroSamplingCDF<-function(zcrit,sigma) {
  1-(pnorm(zcrit,0,sigma)-pnorm(-zcrit,0,sigma))
}


SingleSamplingPDF<-function(z,lambda,sigma) {
  exp(-0.5*((z-lambda)^2/sigma^2))/sqrt(2*pi*sigma^2)
}
SingleSamplingCDF<-function(zcrit,lambda,sigma) {
  1-(pnorm(zcrit,lambda,sigma)-pnorm(-zcrit,lambda,sigma))
}


UniformSamplingPDF<-function(z,lambda,sigma) {
  (1-tanh(z)^2)
}
UniformSamplingCDF<-function(zcrit,lambda,sigma) {
  1-(tanh(zcrit)-tanh(-zcrit))/2
}


GaussSamplingPDF<-function(z,lambda,sigma) {
  exp(-0.5*z^2/(sigma^2+lambda^2))/sqrt(2*pi*(sigma^2+lambda^2))
}
GaussSamplingCDF<-function(zcrit,lambda,sigma) {
  sigma<-sqrt(lambda^2+sigma^2)
  1-(pnorm(zcrit,0,sigma)-pnorm(-zcrit,0,sigma))
}


ExpSamplingPDF<-function(z,lambda,sigma) {
  lambda<-1/lambda
  0.25*(lambda*exp(-lambda*(z-sigma^2*lambda/2))*(1+erf((z-sigma^2*lambda)/sqrt(2)/sigma)) +
          lambda*exp(-lambda*(-z-sigma^2*lambda/2))*(1+erf((-z-sigma^2*lambda)/sqrt(2)/sigma)))
}
ExpSamplingCDF<-function(zcrit,lambda,sigma) {
  lambda<-1/lambda
  z<-zcrit
  p1<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  z<--zcrit
  p2<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  1-(p1-p2)
}

getLogLikelihood<-function(z,n,worldDistr,worldDistK,p_sig,nullP=0) {
  sigma<-1/sqrt(n-3)
  
  # get nulls ready first
  if (nullP>0) {
    nullLikelihoods<-SingleSamplingPDF(z,0,sigma)
    if (p_sig) {
      zcrit<-qnorm(1-alpha/2,0,sigma)
      gainNull<-SingleSamplingCDF(zcrit,0,sigma)
    } else {
      gainNull<-1
    }
  } else {
    nullLikelihoods<-0
    gainNull<-0
  } 
  gainMain<-1
  switch(worldDistr,
         "Single"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-SingleSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-SingleSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,sum(log(likelihoods),na.rm=TRUE))
           }
         },
         "Uniform"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-UniformSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-UniformSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,sum(log(likelihoods),na.rm=TRUE))
           }
         },
         "Gauss"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-GaussSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-GaussSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,sum(log(likelihoods),na.rm=TRUE))
           }
         },
         "Exp"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             z<-abs(z)
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-ExpSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-ExpSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,sum(log(likelihoods),na.rm=TRUE))
           }
         }
  )
  res
}

