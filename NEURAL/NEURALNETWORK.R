BPTrain <- function(trnData, trnLabel, nHiddenUnits, alpha)
{
  trnData = as.matrix(trnData)
  trnLabel = as.matrix(trnLabel)
  trnDataDim = dim(trnData)[2]
  trnLabelDim = dim(trnLabel)[2]
  V = matrix(rep(1,nHiddenUnits*(trnDataDim+1)), nrow=trnDataDim+1, ncol=nHiddenUnits)
  W = matrix(rep(1,(nHiddenUnits + 1)*trnLabelDim), ncol=trnLabelDim, nrow=nHiddenUnits+1)
  V = as.matrix(apply(V,1:2,runif))
  W = as.matrix(apply(W,1:2,runif))
  bindedData = cbind(matrix(rep(1,dim(trnData)[1]), dim(trnData)[1], 1), trnData)
  bindedData = as.matrix(bindedData)
  for (i in 1:1000)
  {
    index = sample (1:dim(trnData)[1], dim(trnData)[1], replace=FALSE)
    for (j in index)
    {
      z_j = bindedData[j,]%*%V
      zj = as.matrix(apply(z_j, 1:2, f1))
      z2 = cbind(matrix(rep(1, dim(zj)[1]),dim(zj)[1],1), zj)
      y_k = as.matrix(z2%*%(W))
      yk = as.matrix(apply(y_k, 1:2, f1))
      
      deltak = (trnLabel[j]-yk) * apply(y_k,1:2,f2)
      deltaW = alpha * deltak %*% z2
      deltaW = t(as.matrix(deltaW))
      
      P = dim(V)[2]
      M = dim(W)[2]
      var1 = matrix(rep(1, P), nrow=P, ncol=1)
      onesMx1 = matrix(rep(1,M), nrow = M, ncol=1)
      altdW = as.matrix(var1 %o% (deltak))
      deltaj = (as.matrix(W[-1,]) %*% deltak) * t(f2(z_j))
      deltaV = alpha *  (as.matrix(bindedData[j,])) %*% t(deltaj)
      deltaV = as.matrix(deltaV)
      
      W = W + deltaW
      V = V + deltaV
    }
  }
  
  return(list(V,W))
  
}

BPClassify <- function(V,W,tstData)
{
  tstData = as.matrix(tstData) 
  counter = dim(tstData)[1]
  HiddenLayerUnitCount = dim(W)[1]-1
  bindedTestData = cbind(matrix(rep(1,counter), counter,1), tstData)
  z_j = as.matrix(bindedTestData%*%V)
  zj = as.matrix(apply(z_j,1:2, f1))
  zj = cbind(matrix(rep(1, dim(zj)[1]),dim(zj)[1],1), zj)
  y_k = as.matrix(zj%*%(W))
  yk = as.matrix(apply(y_k, 1:2, f1))
  
  return(yk)
}

f1 <- function(x)
{
  ans= 2/(1+exp(-x)) - 1
  return(ans)
}

f2 <- function(x)
{
  ans = 0.5 * (1+f1(x)) * (1-f1(x))
  return(ans)
}