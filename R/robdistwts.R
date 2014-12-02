robdistwts = function(xmat, y,percent=0.95) {
#
#     Original source is ww.1.7.r
#
#  robdis2=mycov.rob(as.matrix(xmat),method="mcd")$robdis2
  robdis2 <- robdist.hbrfit(xmat)
  fit <- ltsreg(xmat,y)
  intest=fit$coef
  xmat=as.matrix(xmat)
  y=as.matrix(y)
  n=dim(xmat)[1]
  p=dim(xmat)[2]
  cut=qchisq(percent,p)
  resids=y-intest[1]-xmat%*%as.matrix(intest[2:(p+1)])
  sigma=mad(resids)
  m=psi(cut/robdis2)
  a=resids/(sigma*m)
  c=(median(a)+3*mad(a))^2
  h=sqrt(c)/a
#  tmp=pairup(h)
#  ans=psi(abs(tmp[,1]*tmp[,2]))
   wts=psi(abs(h))
   sresids = resids/sigma
   wdsr = cbind(wts,robdis2,sresids)
   list(wdsr=wdsr,wts=wts,robdis2=robdis2,resids=resids,sresids=sresids)
}


