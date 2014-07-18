
svmTrain<-function(trnData, trnLabel, C)
{
  a=as.matrix(trnData,nrow=200)
  b=as.matrix(trnLabel,nrow=200)
  A=a %*% t(a)
  B=b %*% t(b)
  c=A * B
  a1=t(b)
  a2=(diag(200))
  a3=(-diag(200))
  A1=rbind(a1,a2,a3)
  A1=t(A1)
  z1=0
  z2=matrix(rep(0,200))
  z3=matrix(rep(-C,200))
  Z=rbind(z1,z2,z3)
  c=c+(10 ^ (-12)*diag(200))
  d=rep(1,200)
  sol=solve.QP(c,d,A1,Z,meq=1)
  return (sol$solution)
}
svmClassify<-function(trnData, trnLabel, A, tstData)
{
  trnData=as.matrix(trnData)
  trnLabel=matrix(trnLabel,nrow=1)
  tstData=as.matrix(tstData)
  tstData=t(tstData)
  A=as.matrix(A)
  count1=0
  G=0
  for(i in 1:6461)
  {
    sum=0
    for(j in 1:200)
    {
      if(A[j]>0)
      {
        sum=sum+((A[j]*trnLabel[j])*((trnData[j,])%*%tstData[,i]))
        count1=count1+1
      }
    }
    G[i]=sum 
  }
  sum1=0
  count2=0
  for(i in 1:200)
  {
    if(A[i]>0 & A[i]<2)
    {
      sum1=sum1+trnLabel[i]
      count2=count2+1
    }
  }
  sum1=sum1/count2
  sum2=0
  for(i in 1:200)
  {
    for(j in 1:200)
    {
      if(A[i]>0 & A[j]>0 & A[i]<2) 
      {
        sum2=sum2+(A[j]%*%trnLabel[j]%*%t(trnData[j,])%*%trnData[i,])
      }
    }
  }
  sum2=sum2/count2
  G1=sum1-sum2
  G=G+G1
  return(G)
}