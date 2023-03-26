f_johnson_M <- function(mu,sd,skew,kurt){
  # - use moments to estimate parameters of a Johnson distribution
  #
  # USAGE: result = f_johnson_M(mu,sd,skew,kurt)
  #
  # mu   = mean
  # sd   = standard deviation
  # skew = skewness  MAX = sqrt(2)
  # kurt = kurtosis
  #
  # result = structure of results with the following fields:
  #  .coef = parameters as: coef = [gamma delta xi lambda];
  #  .type = type of Johnson distribution as: SL, SU, SB, SN, or ST
  #
  # -----Author:-----
  # by David L. Jones, Mar-2014
  #
  # This file is part of the 'JOHNSON CURVE TOOLBOX FOR MATLAB'
  # and is released under the BSD 2-clause license.
  
  # -----Set defaults & check input:-----

  # Check for negative SD:
  if (sd<0) {
    print('Cannot have a negative SD!')
    return(NULL)
  }
  
  # Call subfunction ported from Hill et al.'s (1976) FORTRAN code:
  output <- sub_jnsn(mu,sd,skew,kurt)
  if (output$itype==1 && output$lambda<0) {output$delta=-output$delta}
  
  while ((output$ifault==3) || (output$itype==1 && output$lambda == -1)) {
    if (abs(skew)>0.1) {skew <- skew-sign(skew)*skew*0.25}
    else {kurt <- kurt-sign(kurt)*kurt*0.25}
    output <- sub_jnsn(mu,sd,skew,kurt)
  }
  
  # Recode Johnson curve types:
  switch (output$itype,
          {type <- 'SL'},
          {type <- 'SU'},
          {type <- 'SB'},
          {type <- 'SN'},
          {type <- 'ST'},
          error('Unknown ITYPE!')
  )          
  
  # Wrap results up into a structure:
  list(gamma=output$gamma, delta=output$delta, xi=output$xi, lambda=output$lambda, type=type)
}



################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_jnsn <- function(xbar,sd,rb1,bb2){
  # - finds type and parameters of a Johnson curve with given first four moments
  
  # Define constants:
  tol <- 0.01

  # Check for negative SD:
  if (sd < 0){
    return(list(itype  = NaN, gamma = NaN, delta = NaN, lambda = NaN, xi = NaN, ifault = 1))
  } 
  # and zero SD:
  if (sd ==0) {
    return(list(itype = 5, delta = 0, gamma = 0, lambda  = 0 ,xi=xbar, ifault=0))
  }
  
  ifault <- 0
  xi     <- 0
  xlam   <- 0
  gamma  <- 0
  delta  <- 0
  
  b1    <- rb1*rb1
  b2    <- bb2
  fault <- 0
  # Test whether Lognormal (or Normal) required:
  if (b2 >= 0){
    # Test for position relative to boundary line:
    if (b2 > b1+tol+1){
      if ((abs(rb1) <= tol) && (abs(b2-3) <= tol)){
        return(list(itype = 4, delta = 1/sd, gamma = -xbar/sd, lambda  = 1 ,xi=0, ifault=0))
      } 
      stopWhile <- 0 # proceed to WHILE loop
      skip      <- 1 # skip first line of WHILE loop:
    } else {
      if (b2 >= b1+1){
        y<-0.5 + 0.5*sqrt(1-4/(b1+4))
        if (rb1 > 0) {y<-1 - y}
        x<-sd/sqrt(y*(1-y))
        return(list(itype = 5, delta = y, gamma = 0, lambda  = xbar-y*x+x ,xi=xbar-y*x, ifault=0))
      }
      return(list(itype = 5, delta = 0, gamma = 0, lambda  = 0 ,xi=0, ifault=2))
    }
  } else {
    stopWhile <- 0# proceed to WHILE loop
    skip      <- 0# don't skip 1st line of WHILE loop
  }
  
  while (stopWhile == 0){
    # -----Skip this block on 1st run:-----
    if (skip==1){
      skip <- 0 # don't skip anymore
    } else {
      if (!abs(rb1) > tol){return(list(itype = 4, delta = 1/sd, gamma = -xbar/sd, lambda  = 1 ,xi=0, ifault=0))}
    }
    # Test for position relative to Lognormal line:
    x <- 0.5*b1 + 1
    y <- abs(rb1)*sqrt(1/4*b1+1)
    u <- (x+y)^(1/3)
    w <- u + 1/u - 1
    u <- w*w*(3+w*(2+w)) - 3
    if ((b2 < 0) || (fault)) {b2 <- u}
    x <- u - b2
    if (abs(x) <= tol){
      # Lognormal (SL) distribution:
      u     <- xlam*xbar
      x     <- 1/sqrt(log(w))
      y     <- 1/2*1/sqrt(log(w))*log(w*(w-1)/(sd*sd))
      return(list(itype = 1, delta = x, gamma = y, lambda  = sub_sign(1,rb1), xi=sub_sign(1,rb1)*(u-exp((0.5/x-y)/x)), ifault=0))
    } 
    # SB or SU distribution:
    if (x > 0){
      itype <- 3
      sb_out <- sub_sbfit(xbar,sd,rb1,b2)
      gamma<-sb_out$gamma
      delta<-sb_out$delta
      xlam<-sb_out$lambda
      xi<-sb_out$xi
      fault<-sb_out$fault
      if (fault==0) { return(list(itype=3, delta=delta,gamma=gamma,lambda=xlam,xi=xi,ifault=0)) }
      # Failure - try to fit approximate result:
      ifault <- 3
      if (b2 <= b1+2){
        y<-0.5 + 0.5*sqrt(1-4/(b1+4))
        if (rb1 > 0) {y<-1 - y}
        x<-sd/sqrt(y*(1-y))
        return(list(itype = 5, delta = y, gamma = 0, lambda  = xbar-y*x+x ,xi=xbar-y*x, ifault=0))
      } 
      skip <- 0# don't skip 1st line of WHILE loop
      next # next iteration of WHILE loop
    } else {
      itype <- 2
      su_out <- sub_sufit(xbar,sd,rb1,b2)
      gamma<-su_out$gamma
      delta<-su_out$delta
      xlam<-su_out$lambda
      xi<-su_out$xi
      fault<-0
      return(list(itype=2, delta=delta,gamma=gamma,lambda=xlam,xi=xi,ifault=0))
    }
  }
  return(list(itype=itype, delta=delta,gamma=gamma,lambda=xlam,xi=xi,ifault=fault))
}



################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_sign<-function(A,B) {
  # If B\ge 0 then the result is ABS(A), else it is -ABS(A).
  A      <- abs(A)
  A[B<0] <- A[B<0] * -1
  A
}



################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_sufit <- function(xbar,sd,rb1,b2) {
  # function [gamma,delta,xlam,xi] <- sub_sufit(xbar,sd,rb1,b2)
  # - finds parameters of Johnson SU curve with given first four moments

# Define constants:
tol <- 0.01

b1 <- rb1 * rb1; b3 <- b2 - 3

# w is first estimate of exp(delta^(-2)):
w <- sqrt(2*b2-2.8*b1-2)
w <- sqrt(w-1)

# Initialize:
stopWhile <- 0

if (abs(rb1)>tol){
  while (stopWhile == 0) { # Johnson iteration:
    w1  <- w + 1
    wm1 <- w - 1
    z   <- w1*b3
    v   <- w*(6+w*(3+w))
    a   <- 8*(wm1*(3+w*(7+v))-z)
    b   <- 16*(wm1*(6+v)-b3)
    y   <- (sqrt(a*a-2*b*(wm1*(3+w*(9+w*(10+v)))-2*w1*z))-a)/b
    z   <- y*wm1*(4*(w+2)*y+3*w1*w1)^2/(2*(2*y+w1)^3)
    v   <- w*w
    w   <- sqrt(1-2*(1.5-b2+(b1*(b2-1.5-v*(1+0.5*v)))/z))
    w   <- sqrt(w-1)
    if (abs(b1-z) <= tol){
      y <- y/w
      y <- log(sqrt(y)+sqrt(y+1))
      if (rb1 > 0) {y <- -y}
      break # terminate WHILE loop
    } 
  }
} else {
  # Symmetrical case - results are known
  y <- 0
}
x     <- sqrt(1/log(w))
delta <- x
gamma <- y*x
y     <- exp(y)
z     <- y*y
x     <- sd/sqrt(1/2*(w-1)*(1/2*w*(z+1/z)+1))
xlam  <- x
xi    <- (1/2*sqrt(w)*(y-1/y))*x + xbar
return(list(gamma=gamma,delta=delta,lambda=xlam,xi=xi))
}



################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_sbfit <- function (xbar,sigma,rtb1,b2) {
  # function [gamma,delta,xlam,xi,fault] <- sub_sbfit(xbar,sigma,rtb1,b2)
  # - finds parameters of Johnson SB curve with given first four moments
  
  # Preallocate:
  deriv <- c(NaN,NaN,NaN,NaN)
  dd <- deriv
  
  # Define constants:
  tt <- 1.0e-4; tol <- 0.01; limit <- 50
  
  a1 <- 0.0124; a2 <- 0.0623; a3 <- 0.4043; a4 <- 0.408; a5 <- 0.479; a6 <- 0.485
  a7 <- 0.5291; a8 <- 0.5955; a9 <- 0.626; a10 <- 0.64; a11 <- 0.7077; a12 <- 0.7466
  a13 <- 0.8; a14 <- 0.9281; a15 <- 1.0614; a16 <- 1.25; a17 <- 1.7973; a18 <- 1.8
  a19 <- 2.163; a20 <- 2.5; a21 <- 8.5245; a22 <- 11.346; rb1 <- abs(rtb1)
  b1 <- rb1 * rb1; neg <- (rtb1 < 0)
  
  # Get d as first estimate of delta:
  e <- b1 + 1
  x <- 0.5*b1 + 1
  y <- abs(rb1)*sqrt(0.25*b1+1)
  u <- (x+y)^(1/3)
  w <- u + 1/u - 1
  f <- w*w*(3+w*(2+w)) - 3
  e <- (b2-e)/(f-e)
  if (abs(rb1)>tol){
    d <- 1/sqrt(log(w))
    if (d < a10){
      f <- a16*d
    } else {
      f <- 2 - a21/(d*(d*(d-a19)+a22))
    }
  } else {
    f <- 2
  }
  f <- e*f + 1
  if (f < a18){
    d <- a13*(f-1)
  } else {
    d <- (a9*f-a4)*(3-f)^(-a5)
  }
  
  # Get g as first estimate of gamma:
  g <- 0
  if (b1 >= tt){
    if (d > 1){
      if (d <= a20){
        u <- a2
        y <- a3
      } else {
        u <- a1
        y <- a7
      }
      g <- b1^(u*d+y)*(a14+d*(a15*d-a11))
    } else {
      g <- (a12*d^a17+a8)*b1^a6
    }
  }
  
  # -----Main iteration starts here:-----
  stopWhile <- 0# initialize
  m    <- 0
  while (stopWhile == 0){
    m     <- m + 1
    fault <- (m > limit)
    if (fault) {return(list(gamma=0,delta=0,lambda=0,xi=0,fault=fault))}
    
    # Get first six moments for latest g and d values:
    mom_out <- sub_mom(g,d)
    hmu<-mom_out$a
    fault<-mom_out$fault
    if (fault) {return(list(gamma=0,delta=0,lambda=0,xi=0,fault=fault))}
    s     <- hmu[1]*hmu[1]
    h2    <- hmu[2] - s
    fault <- (h2 <= 0)
    if (fault) {return(list(gamma=0,delta=0,lambda=0,xi=0,fault=fault))}
    
    t    <- sqrt(h2)
    h2a  <- t*h2
    h2b  <- h2*h2
    h3   <- hmu[3] - hmu[1]*(3*hmu[2]-2*s)
    rbet <- h3/h2a
    h4   <- hmu[4] - hmu[1]*(4*hmu[3]-hmu[1]*(6*hmu[2]-3*s))
    bet2 <- h4/h2b
    w    <- g*d
    u    <- d*d
    
    # Get derivatives:
    for (j in 1:2){
      for (k in 1:4){
        t <- k
        if (j==1){
          s <- hmu[k+1] - hmu[k]
        } else {
          s <- ((w-t)*(hmu[k]-hmu[k+1])+(t+1)*(hmu[k+1]-hmu[k+2]))/u
        }
        dd[k] <- t*s/d
      }
      t          <- 2*hmu[1]*dd[1]
      s          <- hmu[1]*dd[2]
      y          <- dd[2] - t
      deriv[j]   <- (dd[3]-3*(s+hmu[2]*dd[1]-t*hmu[1])-1.5*h3*y/h2)/h2a
      deriv[j+2] <- (dd[4]-4*(dd[3]*hmu[1]+dd[1]*hmu[3])+6*(hmu[2]*t+hmu[1]*(s-t*hmu[1]))-2*h4*y/h2)/h2b
    }
    t <- 1/(deriv[1]*deriv[4]-deriv[2]*deriv[3])
    u <- (deriv[4]*(rbet-rb1)-deriv[2]*(bet2-b2))*t
    y <- (deriv[1]*(bet2-b2)-deriv[3]*(rbet-rb1))*t
    
    # Form new estimates of g and d:
    g <- g - u
    if ((b1 == 0) || (g < 0)) {g <- 0}
    d <- d - y
    
    # Assess WHILE loop:
    if ((abs(u) <= tt) && (abs(y) <= tt)){
      delta <- d
      xlam  <- sigma/sqrt(h2)
      if (neg){
        gamma  <- -g
        hmu[1] <- 1 - hmu[1]
      } else {
        gamma  <- g
      }
      xi <- xbar - xlam*hmu[1]
      break # terminate WHILE loop
    }
  }
  return(list(gamma=gamma,delta=delta,lambda=xlam,xi=xi,fault=0))
}


################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_mom <- function(g,d) {
  # - evaluates 1st six moments of a johnson SB distribution, using Goodwin method
  
  # -----Notes:-----
  # rttwo : sqrt(2.0)
  # rrtpi : reciprocal of sqrt(pi)
  # expa  : a value such that exp(expa) does not quite cause overflow
  # expb  : a value such that 1.0 + exp(-expb) may be taken to be 1.0
  
  # Define constants:
  zz <- 1.0e-5; vv <- 1.0e-8; limit <- 500; rttwo <- sqrt(2); rrtpi <- 1/sqrt(pi)
  expa <- 80.0; expb <- 23.7
  
  w <- g/d
  
  # Preallocate or initialize:
  a     <- c(NaN,NaN,NaN,NaN,NaN,NaN)
  b     <- a
  fault <- 0
  c     <- c(0,0,0,0,0,0)
  
  # Trial value of h:
  if (w > expa){ 
    fault <- 1
    return(list(a=a,fault=fault))
  }
  
  e <- exp(w) + 1
  r <- rttwo/d
  h <- 0.75
  if (d < 3) {h <- d/4}
  k <- 1
  
  # -----OUTER WHILE loop:-----
  skip     <- 1 # skip 1st block of outer WHILE loop on 1st run
  stop_out <- 0 # initiialize
  while ((stop_out == 0)){
    # -----Skip this block on 1st run:-----
    if (skip==1){
      skip <- 0 # don't skip anymore:
    } else {
      k <- k + 1
      if ( k > limit ) {
        fault <- 1
        return(list(a=a,fault=fault))
      }
      c<-a    
      #  No convergence yet - try smaller h:
      h <- 0.5*h
    }
    # -------------------------------------
    t <- w
    u <- t
    y <- h*h
    x <- 2*y
    a[1] <- 1/e
    for (i in 2:6){
      a[i] <- a[i-1]/e
    }
    v <- y
    f <- r*h
    m <- 0
    
    # -----INNER WHILE loop evaluates infinite series:-----
    stop_inn <- 0
    break_out <- 0
    while (stop_inn == 0){
      m <- m + 1
      if (m > limit){ break } # terminate INNER WHILE loop
      b<-a
      u <- u - f
      z <- 1
      if (u > -expb) {z <- exp(u) + z}
      t <- t + f
      l <- (t > expb)
      if (l==0) {s <- exp(t) + 1}
      p <- exp(-v)
      q <- p
      for (i in 1:6){
        aa <- a[i]
        p  <- p/z
        ab <- aa
        aa <- aa + p
        if (aa == ab) {break} # terminate this FOR loop
        if (l==0){
          q  <- q/s
          ab <- aa
          aa <- aa + q
          l <- (aa==ab)
        }
        a[i] <- aa
      }
      y <- y + x
      v <- v + y
      for (i in 1:6){
        if (a[i]==0) {
          fault <- 1 
          return(list(a=a,fault=fault))
        }
        if (abs((a[i]-b[i])/a[i]) > vv){
          cont_inn <- 1 # continue next iteration of INNER WHILE loop
          break # terminate this FOR loop
        } else {
          cont_inn <- 0# don't continue
        }
      }
      if (cont_inn==1) {next} # skip to start of INNER WHILE loop
      v <- rrtpi*h
      a <- v*a
      for (i in 1:6){
        if (a[i] == 0) {
          fault <- 1
          return(list(a=a,fault=fault))
        }
        if (abs((a[i]-c[i])/a[i]) > zz){
          break_out <- 1# signal OUTER WHILE loop should be terminated
          break  # terminate this FOR loop
        } else {
          break_out <- 0# don't terminate OUTER WHILE loop
        }
      }
      if (break_out==1) {break} # terminate INNER WHILE loop
    }
    # -----------------------------------------------------
    if (break_out==1) {break} # terminate OUTER WHILE loop
  }
  # ---------------------------
  return(list(a=a,fault=fault))
}
