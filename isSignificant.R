isSignificant<-function(method="NHST",p,r,n,evidence) {
  
  switch (method,
          "NHST"={
            sig<-p<alpha
          },
          "sLLR"={
            s<-r2llr(r,n,"sLLR",evidence$llr,evidence$prior)
            sig<-s>alphaLLR
          },
          "dLLR"={
            r[abs(r)>1]<-1
            d<-r2llr(abs(r),n,"dLLR",evidence$llr,evidence$prior)
            sig<-d>alphaLLR
          }
  )
  return(sig)
}
