
LSC<-function(X,Y){
  C=as.matrix(X,nrow=200,ncol=2)
  d=matrix(1,200,1)
  e=cbind(C,d)
  w=solve(t(e)%*%e)%*%t(e)%*%Y
  return (w)
}
FLD<-function(X,Y){
  A=as.matrix(X,nrow=200,ncol=2)  
  m1=matrix(c(colMeans(X[1:100,1:2])))
  m2=matrix(c(colMeans(X[101:200,1:2])))
  
  j=1
  while(j<=100){
    f=(c((X[j,1]-m1[1,1]),(X[j,2]-m1[2,1])))
    f=f%*%t(f)
    if (j==1){
      sum=f
    }
      
    sum=sum+f
    j=j+1
  }
  
  j1=101
  while(j1<=200){
    f1=c((A[j1,1]-m2[1,1]),(A[j1,2]-m2[2,1]))
    f1=f1%*%t(f1)
    if (j1==101){
      sum1=f
    }
    sum1=sum1+f1
    j1=j1+1
  }
  sw=sum+sum1
m3=as.matrix(m1-m2)
  h=(solve(sw))%*%(m3)
  m4=colMeans(X[1:200,1:2])
  m4=as.matrix(m4)
  w3=t(h)%*%m4
  return(c(h,w3))
}