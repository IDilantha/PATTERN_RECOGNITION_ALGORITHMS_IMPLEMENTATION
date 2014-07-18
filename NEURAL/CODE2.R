#trn = read.table('C:\\Users\\Jyoti Mathur\\Desktop\\PR\\hw4Data.txt')
#trnData = trn[,1:2]
#trnLabel = trn[,3]

BPTrain<-function(trnData, trnLabel, nHiddenUnits, alpha)
{ 
  trnData=cbind(1,trnData)
  trnData=as.matrix(trnData)
  v <- matrix(runif(9, 0, 1), 3)
  w <- matrix(runif(4, 0, 1), 4)
  k=1
  epoch=0
  while(epoch<1000)
  {
    z=matrix()
    z1=matrix()
    sampler=sample(1:200,200)
    for(i in 1:200)
    {
      index=sampler[i]
      for(j in 1:3)
      {
        z[j]=(trnData[index,]%*%v[,j])
        z1[j]=(2/(1+exp(-z[j])))-1
        
      }
    a=matrix(c(1),1)
      z1=matrix(z1,1,3)
    z1=cbind(a,z1)
    
    
    y1=z1%*%w
    y=(2/(1+exp(y1))-1)
    
    
    l=(1+y)*(1-y)
    l=0.5*l
    #deltak=matrix()
    deltak=(trnLabel[index]-y)*l
    
    deltaw=alpha*(deltak%*%z1)
    deltaj=0
    for(j in 1:3)
    {
      n=(1+z[k])%*%(1-z[k])
      n=n*0.5
      k=1
      #deltaj=matrix()
      deltaj=deltaj+deltak%*%w[j][k]*n
    }
    
    
    
    deltav=matrix(0,3,3)
    for(j in 1:3){
      #deltav=matrix()
      deltav[,j]=alpha*trnData[index,]*deltaj
    }
      deltaw=t(deltaw)
    w=w+deltaw
    v=v+deltav
    }
    epoch=epoch+1
  }
  return(list(w,v))
}


BPClassify<-function(V, W, tstData)
{
  
  trnData=cbind(1,trnData)
  trnData=as.matrix(trnData)
  v <- matrix(runif(9, 0, 1), 3)
  w <- matrix(runif(4, 0, 1), 4)
  sampler=sample(1:200,200)
  for(i in 1:200)
  {
    index=sampler[i]
    for(j in 1:3)
    {
      z=matrix()
      z[j]=(trnData[index,]%*%v[,j])
      z1=matrix()
      z1[j]=(2/(1+exp(-z[j])))-1
      
    }
  }
  z1=cbind(1,z1)
  k=1
{
  for(j in 1:3)
    
  {   
    y=matrix()
    
    y[k]=y[k]+z1[j]*w[j,k]
    y1=matrix()
    y1[k]=(2/(1+exp(-y[k])))-1
    return(score)
    
  }
  
  
  #z[j]=z[j]+(y[i]%*%v[i][k]
}
}