P1 <- function()
{
  Pt = .4*.2*(.4*.95+.6*.01)+.6*.11*(.15*.95+.85*.01)
  Pf = .4*.8*(.4*.95+.6*.01)+.6*.89*(.15*.95+.85*.01)
  alpha = 1/(Pt+Pf)
  return(alpha*Pt)
}

P2 <- function(N)
{
  probabilitymatrix = matrix(0, nrow=30000, ncol=7)
  for (i in 1:N)
  {
    S = PS()
    C = PCS(S)
    H = PHS(S)
    H1 = PH1H(H)
    H2 = PH2H(H)
    C1 = PC1C(C)
    C2 = PC2C(C)
    probabilitymatrix[i,] = c(S, H, C, H1, H2, C1, C2)
  }
  A = probabilitymatrix[probabilitymatrix[,4]==1,]
  probabilityH1 = dim(A)[1]
  probabilityC = dim(A[A[,3]==1,])[1]
  return(probabilityC/probabilityH1)
}

P3 <- function(N)
{
  probabilitymatrix = matrix(0, nrow=30000, ncol=7)
  S = PS()
  C = PCS(S)
  H = PHS(S)
  H1 = PH1H(H)
  H2 = PH2H(H)
  C1 = PC1C(C)
  C2 = PC2C(C)
  probabilitymatrix[1,] = c(S, H, C, H1, H2, C1, C2)
  for (i in 2:N)
  {
    S = PS()
    C = PCS(S)
    H = PHS(S)
    H1 = PH1H(H) * H
    H2 = PH2H(H) * H
    C1 = PC1C(C) * C
    C2 = PC2C(C) * C
    probabilitymatrix[i,] = c(S, H, C, H1, H2, C1, C2)
  }
  A = probabilitymatrix[probabilitymatrix[,4]==1,]
  probabilityH1 = dim(A)[1]
  probabilityC = dim(A[A[,3]==1,])[1]
  return(probabilityC/probabilityH1)
}

PS <- function()
{
  truthness = 1
  x = runif(1,0,1)
  if (x < .4) 
  {
    truthness = 1
  }
  else 
  {
    truthness = 0
  }
  return(truthness)
}
PHS <- function(S)
{
  truthness = 1
  x = runif(1, 0,1)
  if (S == 1)
  {
    if (x < .4) 
    {
      truthness = 1
    }
    else 
      {
      truthness = 0
      }
  }
  else
  {
    
    if (x < .15)
      {
      truthness = 1
    } 
    else
    {
      truthness = 0
    }
  }
  return(truthness)
}
PCS <- function(S)
{
  truthness = 1
  x = runif(1, 0,1)
  if (S == 1)
  {
    if (x < .2) 
      {
      truthness = 1
    } 
    else 
    {
      truthness = 0
    }
  }
  else
  {
    if (x < .11)
    {
      truthness = 1
    }
    else
    {
      truthness = 0
    }
  }
  return(truthness)
}
PH1H <- function(H)
{
  truthness = 1
  x = runif(1, 0,1)
  if (H == 1)
  {
    if (x < .95) 
    {
      truthness = 1
    }
    else
    {
      truthness = 0
    }
  }
  else
  {
    if (x < .01) 
    {
      truthness = 1
    } 
    else 
    {
      truthness = 0
    }
  }
  return(truthness)
}
PH2H <- function(H)
{
  truthness = 1
  x = runif(1, 0,1)
  if (H == 1)
  {
    if (x < .98) 
    {
      truthness = 1
    } else 
    {
      truthness = 0
    }
  }
  else
  {
    if (x < .05) 
    {
      truthness = 1
    } 
    else 
    {
      truthness = 0
    }
  }
  return(truthness)
}
PC1C <- function(C)
{
  truthness = 1
  x = runif(1, 0,1)
  if (C == 1)
  {
    if (x < .99) 
    {
      truthness = 1
    } 
    else 
    {
      truthness = 0
    }
  }
  else
  {
    if (x < .1) 
    {
      truthness = 1
    }
    else 
    {
      truthness = 0
    }
  }
  return(truthness)
}
PC2C <- function(C)
{
  truthness = 1
  x = runif(1, 0,1)
  if (C == 1)
  {
    if (x < .98) 
    {
      truthness = 1
    } 
    else
    {
      truthness = 0
    }
  }
  else
  {
    if (x < .05)
    {
      truthness = 1
    } 
    else 
    {
      truthness = 0
    }
  }
  return(truthness)
}