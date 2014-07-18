
source('CODE1.R')
# prepare the training data
cat(sprintf("Reading the training data....\n"))
trn = read.table('train.txt')
nCol = ncol(trn)
trnData = trn[,2:nCol]
trnLabel = trn[,1]

# train
cat(sprintf("Training.... \n"))
val = NB_learn(trnData, trnLabel)
Mean = val[[1]]
Var = val[[2]]
Prior = val[[3]];

# prepare the test data
cat(sprintf("Reading the test data....\n"))
tst = read.table('test.txt')
tstData = tst[, 2:nCol]
tstLabel = tst[,1]

# classify
cat(sprintf("Classifying the test data....\n"))
for (i in 1:nrow(tstData)) {
  pred = NB_classify(Mean, Var, Prior, tstData[i,])
  cat(sprintf("%d ", tstLabel[i]))
  for (j in 1:10) {
    cat(sprintf("%.1f ", pred[j]));
  }
  cat(sprintf("\n"));
}
