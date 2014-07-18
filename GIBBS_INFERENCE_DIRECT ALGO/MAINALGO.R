

source('ALGO.R')
cat(sprintf("Problem 1 ....\n"))
p = P1() 
cat(sprintf("Exact inference: %.4f\n\n", p))

N = 30000;
cat(sprintf("Problem 2 ....\n"))
p = P2(N) 
cat(sprintf("Inference using direct sampling: %.4f\n\n", p))

cat(sprintf("Problem 3 ....\n"))
p = P3(N) 
cat(sprintf("Inference using Gibbs sampling: %.4f\n\n", p))