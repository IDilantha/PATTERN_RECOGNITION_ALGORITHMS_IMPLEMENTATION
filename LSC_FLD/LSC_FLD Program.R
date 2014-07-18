source('functions.R')

data = read.table("data.txt")
X = data[,c(1, 2)]
Y = data[,3]
testX = seq(-2,6,by=0.1)
P = X[which(Y == 1),c(1,2)]
N = X[which(Y == -1),c(1,2)]

# call your FLD function
w = FLD(X, Y)
print(w)
fldpred = - testX * w[1]/w[2] + w[3]/w[2]
pdf('hw1plot.pdf')
par(mfrow=c(1,2))
plot(testX, fldpred, type='l', xlim=c(-2,6), ylim=c(-2,4.5), main='FLD', xlab='X', ylab='Y')
grid()
points(P, col="dark red")
points(N, col="blue")

# call your LSC function
w = LSC(X, Y)
print(w)
lscpred = - testX * w[1]/w[2] - w[3]/w[2]
plot(testX, lscpred, type='l', xlim=c(-2,6), ylim=c(-2,4.5), main='LSC', xlab='X', ylab='Y')
grid()
points(P, col="dark red")
points(N, col="blue")

dev.off()
