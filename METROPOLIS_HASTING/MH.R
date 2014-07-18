MH = function(N)
{  
  vec=vector("numeric",30000)
  vec[1]=1
  for(i in 1:30000) 
  {
    u=runif(1,0,1) 
    xi=vec[i]
    xstar=rnorm(1,xi,100)
    pxstar=.3*exp(-.2* (xstar+10)^2) + .7*exp(-.2*(xstar-6)^2)
    qxistar=rnorm(1,xstar,100)
    pxi=.3*exp(-.2* (xi+10)^2) + .7*exp(-.2*(xi-6)^2)
    qxstarxi=rnorm(1,xi,100)
    alpha=min(1,(pxstar*qxistar)/(pxi*qxstarxi))
    if(u<alpha) 
      
      vec[i+1]=xstar
    else
      vec[i+1]=vec[i]
    
  }
  return(vec)
  
  
}