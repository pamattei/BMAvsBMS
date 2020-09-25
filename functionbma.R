bmavsbms = function(X,Y){
  nrep=500
  
  n=nrow(X)
  errMS=numeric(nrep)
  errMA=numeric(nrep)
  gain=numeric(nrep)
  
  bound=numeric(nrep)
  
  
  for (i in 1:nrep){
    
    train=sample(n,floor(n/2))
    
    out<-   bas.lm((Y[train]) ~ ., data=data.frame(X[train,]),prior="hyper-g-n",modelprior=uniform(), initprobs="eplogp")
    
    BMA  = predict(out,data.frame(X), estimator="BMA")
    
    HPM = predict(out,data.frame(X), estimator="HPM")
    
    errMA[i]=sum((BMA$fit[-train]-Y[-train])^2)/length(Y[-train])
    
    errMS[i]=sum((HPM$fit[-train]-Y[-train])^2)/length(Y[-train])
    
    #gain0[i]=sum((HPM$fit[-train]-Y[-train])^2)/(2*sd(HPM$fit[train]-Y[train])^2)  -  sum((BMA$fit[-train]-Y[-train])^2)/(2*sd(BMA$fit[train]-Y[train])^2) + log(sd(HPM$fit[train]-Y[train])/sd(BMA$fit[train]-Y[train]))
    
    sigmahat=sd(BMA$fit[train]-Y[train])
    #sigmahat=sd(HPM$fit[train]-Y[train])
    
    gain[i]=(errMS[i]-errMA[i])
    
    bound[i]=-log(max(out$postprobs))/length(train)*(2*sigmahat^2)
    #cat(i)
  }
  

  
  return(list(bound=mean(bound),gain=mean(gain)))
}