

source('MH.R')
X = seq(-20,20,.1)

# Target distribution
y = .3*exp(-.2* (X+10)^2) + .7*exp(-.2*(X-6)^2)

xRange = range(X)

par(mfrow=c(2,1))
plot(X, y/(sum(y)*.1), type='l', col='blue', xlim=xRange, xlab = 'x', ylab='P(x)')
title(main='HW5')
grid()

# sampling
N = 30000
cat(sprintf("Sampling starts ....\n"))
x = MH(N)
cat(sprintf("Sampling ends....\n"))
x = x[1000:N];t
hist(x, breaks=50, xlim=xRange, xlab = 'x', ylab='count')
grid()
cat(sprintf("The median of x = %.1f\n", median(x)))
cat(sprintf("The mean of x = %.1f\n", mean(x)))
cat(sprintf("The std of x = %.1f\n", sqrt(var(x))))
cat(sprintf("Prob(-10 <= x <= 10) = %.3f\n", length(which(x>=-10 & x<=10 ))/length(x)))
