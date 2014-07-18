BPTrain<-function(trnData, trnLabel, nHiddenUnits, alpha)
{
  x=as.matrix(trnData)
  y=as.matrix(trnLabel)
  x1=matrix(1,200)
  x=cbind(x1,x)
  v=matrix(runif(9,-1,1),3,3)
  w=matrix(runif(4,-1,1),4,1)
  tk=0
  z5=matrix()
  deltav=matrix(0,3,3)
  for(l in 1:20)
  {
  sample1=sample(1:200,200,replace=FALSE)
  for(i in 1:200)
  {
    
    index=sample1[i]        
    tk=y[index]
    for(j in 1:3)
    {
    z1=x[index,]%*%v[,j]
    z2=bs1(z1)
    if(j==1)
    {
      z=z1
      z3=z2
    }
    else
    {
      z3=cbind(z3,z2)
      z=cbind(z,z1)
    }
    }
    z3=cbind(1,z3)
    y2=z3%*%w
    y3=bs1(y2)
    y4=bs2(y2)
    delta1=(tk-y3)*y4  
    deltaw=alpha*(delta1%*%z3)
    for(j in 1:3)
    {
      z5[j]=bs2(z[j])
      delta2=delta1*(z5[j]*w[j])
      deltav[,j]=alpha*delta2*x[index,]
    } 
    deltaw=t(deltaw)
                w=w+deltaw
                print(v)
                v=v+deltav
  }
  }
  return(list(v,w))
}
BPClassify<-function(V, W, tstData)
{
  x=as.matrix(tstData)
  x1=matrix(1,1681)
  x=cbind(x1,x)
  y2=vector()
  y3=vector()
    z=matrix()
  z3=matrix()
    tk=0
    sample1=sample(1:1681,1681,replace=FALSE)
  for(l in 1:1000)
  {
    for(i in 1:1681)
    {
      index=sample1[i]
      for(j in 1:3)
      {
        z1=x[index,]%*%V[,j]
        z2=bs1(z1)
        if(j==1)
        {
          z=z1
          z3=z2
        }
        else
        {
          z=cbind(z,z1)
          z3=cbind(z3,z2)
        }
      }
      z3=cbind(1,z3)
      y2[i]=z3%*%W
      y3[i]=bs1(y2[i])
}
  }
  return(y3)
}
bs1 <- function(x)
{
  ans= 2/(1+exp(-x)) - 1
  return(ans)
}

bs2 <- function(x)
{
  ans = 0.5 * (1+bs1(x)) * (1-bs1(x))
  return(ans)
}