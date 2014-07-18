source('SVM.R')
cat(sprintf("Reading the training data....\n"))
trn = read.table('Data.txt')
trnData = trn[,1:2]
trnLabel = trn[,3]

# plot the training points
P = trnLabel == 1
N = trnLabel == -1
c1 = trnData[P,]
c2 = trnData[N,]
xRange = range(trnData[,1])
yRange = range(trnData[,2])
plot(c1[,1], c1[,2], type = "p", col="red",xlim=xRange,ylim=yRange, xlab='X',ylab='Y', main='SVM')
par(new=TRUE)
plot(c2[,1], c2[,2], type = "p", col="green",xlim=xRange,ylim=yRange, xlab='X',ylab='Y')
par(new=TRUE)

# train
C = 2
cat(sprintf("Training.... \n"))
A = svmTrain(trnData, trnLabel, C)

# classify
x = seq(-3, 6, 0.1)
y = seq(-2, 5, 0.1)
tstData = expand.grid(x,y)
cat(sprintf("Computing the prediction scores for the test data....\n"))
z = svmClassify(trnData, trnLabel, A, tstData)
z = matrix(z,length(x), length(y))
contour(x,y,z, levels = c(-1, 0, 1), xlim=xRange,ylim=yRange, xlab='X',ylab='Y')