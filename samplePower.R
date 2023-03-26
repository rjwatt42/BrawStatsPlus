

rn2w<-function(r,n,t=2){
  if (any(abs(r)>1)) {
    print(paste0("rn2w exception: ",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  r<-abs(r)
  z<-atanh(r)

  w<-(r+n)*0
  # one-tailed
  if (t==1) {
    w<-pnorm(qnorm(alpha)+z*sqrt(n-3))
  } else {
    # two-tailed
    pw1<-pnorm(qnorm(alpha/2)+z*sqrt(n-3))
    pw2<-pnorm(qnorm(alpha/2)-z*sqrt(n-3))
    w<-pw1+pw2
  }
  w[n<3]<-0
  w  
}

rw2n<-function(r,w,t=1){
  if (any(abs(r)>1)) {
    print("rw2n exception")
    r[r>1]<-1
    r[r < -1]<- -1
  }
  r<-abs(r)
  z<-atanh(r)
  if (t==1) {
    # one-tailed
    nnear<-((qnorm(w)-qnorm(alpha))/z)^2+3
  } else {
    # two tailed
    nnear<-(qnorm(w)-(qnorm(alpha/2))/z)^2+3
  }
  nnear<-round(nnear)  
  nnear[nnear>1000000]<-1000000
  nnear[nnear<5]<-5
  nnear
}
