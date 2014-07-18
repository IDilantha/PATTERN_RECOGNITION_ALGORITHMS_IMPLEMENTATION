
source('NEURALNETWROK.R')

cat(sprintf("Reading the training data....\n"))
trn = read.table('Data.txt')
trnData = trn[,1:2]
trnLabel = trn[,3]

# plot the training points
pdf('plot.pdf')
P = trnLabel == 1
N = trnLabel == -1
c1 = trnData[P,]
c2 = trnData[N,]
xRange = range(trnData[,1])
yRange = range(trnData[,2])
plot(c1[,1], c1[,2], type = "p", col="red",xlim=xRange,ylim=yRange, xlab='X',ylab='Y', main='NN')
par(new=TRUE)
plot(c2[,1], c2[,2], type = "p", col="green",xlim=xRange,ylim=yRange, xlab='X',ylab='Y')
par(new=TRUE)

# train
nHiddenUnits = 3
alpha = .2
cat(sprintf("Training.... \n"))
A = BPTrain(trnData, trnLabel, nHiddenUnits, alpha)
V = A[[1]]
W = A[[2]]

# classify
x = seq(-1, 1, 0.05)
y = seq(-1, 1, 0.05)
tstData = expand.grid(x,y)
cat(sprintf("Computing the prediction scores for the test data....\n"))
z = BPClassify(V, W, tstData)
z = matrix(z,length(x), length(y))
contour(x,y,z, levels = c(0), xlim=xRange,ylim=yRange, xlab='X',ylab='Y')
dev.off()