# Kevin Finity, kf2tg

# Part 2 of project - given the analysis done in part 1 (project1.R),
# and the holdout file "holdout/holdout_full.csv" (from holdout.R)

# First, re-run 10-fold CV on the initial dataset to get detect & false alarm rates...

set.seed(1)
pix <- read.csv("HaitiPixels.csv")

# create binary response variable
IsBT = rep(0,nrow(pix))
IsBT[pix$Class=="Blue Tarp"] = 1
IsBT.bin = IsBT # binary version
IsBT = factor(IsBT, levels=c(1,0)) # factor version

# for 10x - full data set
pix.p = pix[,-1] # predictors
pix.c = pix[,1] # response

# feature engineering
# blue tarp pixels have high levels of blue *relative to the other colors present*
pix.p$br = pix.p$Blue / pix.p$Red
pix.p$bg = pix.p$Blue / pix.p$Green
#pix.p$rg = pix.p$Red / pix.p$Green

# K-Nearest Neighbors, 10x on initial data
library(class)
library(caret)

idx <- createFolds(pix.c, k=10) # create 10 folds
ks <- 1:15
kdetect = rep(0,length(ks)) # vec to hold the detect rates for the different ks
kfalse = rep(0,length(ks)) # same, for the false alarm rate
kd = rep(0,10) # temp vec to hold cv detect rate
kf = rep(0,10) # temp vec to hold cv false alarm rate
for (k in ks) {
  # for each value of k...
  for (i in 1:10) {
    # loop over each of the 10 cross-validation folds
    # generate knn predictions
    knn.pred = knn(train = pix.p[ -idx[[i]], ],
                   test = pix.p[ idx[[i]], ],
                   cl = IsBT[ -idx[[i]] ],
                   k=k)
    y = IsBT[ idx[[i]] ] # test classification
    kd[i] = (sum(knn.pred == y & y ==1)/sum(y==1)) # %tarps detected correctly
    kf[i] = (sum(knn.pred != y & knn.pred==1)/sum(y!=1)) # %non-tarps detected as tarps
  }
  ##average over the 10 folds
  kdetect[k] = mean(kd)
  kfalse[k] = mean(kf)
}
plot(kdetect,main="Optimizing KNN k-value", xlab="KNN k-values",ylab="Detect Rate") # 
plot(kfalse,xlab="KNN k-values",ylab="False Alarm Rate") # 
kdetect[7] # Detect rate 0.977257 for k=7
kfalse[7] # at k=7, false alarm rate is 0.001535433
# with 5 features, slightly worse
kdetect[8] # 0.9653856
kfalse[8] # 0.001600757

# LDA, 10x on initial 
library(MASS)
nfolds = 10 # 10 folds
detect = rep(0,nfolds) 
falarm = rep(0,nfolds) 
#set.seed(2)
# commented out - reuse same folds as above for better comparison
#idx <- createFolds(pix.c, k=10) # create 10 folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  y.train = IsBT[ -idx[[i]] ]
  y.test = IsBT[ idx[[i]] ]
  lda.fit=lda(y.train~.,data=pix.p[ -idx[[i]], ])
  lda.pred=predict(lda.fit, pix.p[ idx[[i]], ]) #
  pc=lda.pred$class # predicted class
  detect[i] = (sum(pc == y.test & y.test==1)/sum(y.test==1)) # %tarps detected correctly
  falarm[i] = (sum(pc != y.test & pc==1)/sum(y.test!=1)) # %non-tarps detected as tarps
}
mean(detect) # 0.8016998 detect rate (RGB)
mean(falarm) # 0.01007868 false alarm
# 5 features:
# detect 0.8412549, falarm 0.0008167026

# QDA, 10x on initial 
#library(MASS)
#nfolds = 10 # 10 folds
#n = nrow(pix.p)
detect = rep(0,nfolds) 
falarm = rep(0,nfolds) 
#set.seed(2)
#idx <- createFolds(pix.c, k=10) # create 10 folds
#folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  #i.train = pix.p[-folds[[i]],]
  #i.test = pix.p[folds[[i]],]
  y.train = IsBT[ -idx[[i]] ]
  y.test = IsBT[ idx[[i]] ]
  qda.fit=qda(y.train~.,data=pix.p[ -idx[[i]], ])
  qda.pred=predict(qda.fit, pix.p[ idx[[i]], ]) #
  pc=qda.pred$class # predicted class
  detect[i] = (sum(pc == y.test & y.test==1)/sum(y.test==1)) # %tarps detected correctly
  falarm[i] = (sum(pc != y.test & pc==1)/sum(y.test!=1)) # %non-tarps detected as tarps
}
mean(detect) # 0.8407575
mean(falarm) # 0.0002940082
# with 5 features:
# detect 0.970336, falarm 0.00267885

# Logistic Regression, 10x on initial
#nfolds = 10 # 10 folds
#n = nrow(pix.p)
detect = rep(0,nfolds) 
falarm = rep(0,nfolds) 
#set.seed(2)
#idx <- createFolds(pix.c, k=10) # create 10 folds
#folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  #i.train = pix.p[-folds[[i]],]
  #i.test = pix.p[folds[[i]],]
  y.train = IsBT.bin[ -idx[[i]] ]
  y.test = IsBT.bin[ idx[[i]] ]
  glm.fits=glm(y.train~.,data=pix.p[ -idx[[i]], ],family=binomial)
  glm.probs=predict(glm.fits, pix.p[ idx[[i]], ], type="response")
  pc = rep(0,length(idx[[i]]))
  pc[glm.probs>.5]=1 # predicted class, threshold
  detect[i] = (sum(pc == y.test & y.test==1)/sum(y.test==1)) # %tarps detected correctly
  falarm[i] = (sum(pc != y.test & pc==1)/sum(y.test!=1)) # %non-tarps detected as tarps
}
mean(detect) # 0.885268
mean(falarm) # 0.00107809
# with 5 features
# detect 0.9040604, falarm 0.00161714

# Random Forest, 10x on initial data
library(randomForest)

# look for a good ntree value
rf.bt = randomForest(IsBT~.,data=pix.p, ntree=1501,importance=TRUE)
plot(rf.bt) # it doesn't seem to improve much past ~250, so the default of 500 is probably fine

res = tuneRF(x = pix.p, y = IsBT, ntreeTry = 501)
print(res)
# I feel like RF might do better with more features - e.g. combinations of the basic RGB values
# 5 features: mtry=2 min

rf.bt = randomForest(IsBT~.,data=pix.p, ntree=501, mtry=2, importance=TRUE)
rf.bt$err.rate[,1] 
# 3 features: 69 for mtry=3, 111 for mtry=2, also around 70 for mtry=1
# 5 feat: ntree=87 for mtry=2 0.002735567
#     also same error out around 473
plot(rf.bt$err.rate[,1], type="line", main="Random Forest test error rate", xlab="ntree values", ylab="Test error rate")

rf.bt = randomForest(IsBT~.,data=pix.p, ntree=87, mtry=2, importance=TRUE)
importance(rf.bt)
#               1         0 MeanDecreaseAccuracy MeanDecreaseGini
# Red    8.544358  3.681963             4.254527         175.1218
# Green  8.235617  2.994330             3.571787          95.9813
# Blue  10.067633  4.847576             5.532105         255.5726
# br    40.885018 14.861085            34.498803        2169.4238
# bg    13.613588  2.358216            13.963860        1172.1292

# 10x CV
detect = rep(0,nfolds) 
falarm = rep(0,nfolds) 
#set.seed(2)
#idx <- createFolds(pix.c, k=10) # create 10 folds
#folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  #i.train = pix.p[-folds[[i]],]
  #i.test = pix.p[folds[[i]],]
  y.train = IsBT[ -idx[[i]] ]
  y.test = IsBT[ idx[[i]] ]
  rf.fits=randomForest(y.train~.,data=pix.p[ -idx[[i]], ], ntree=87, mtry=2, importance=TRUE)
  pc = predict(rf.fits, newdata=pix.p[ idx[[i]], ])
  detect[i] = (sum(pc == y.test & y.test==1)/sum(y.test==1)) # %tarps detected correctly
  falarm[i] = (sum(pc != y.test & pc==1)/sum(y.test!=1)) # %non-tarps detected as tarps
}
mean(detect) # 0.9500585
mean(falarm) # 0.00133937
# 5 feat:
# detect 0.9490684, 0.001208747


# SVM, 10x on initial data
library(e1071)

svmfit=svm(IsBT~., data=pix.p, kernel="linear", cost=10,scale=FALSE)

pix2 = pix.p
pix2$IsBT = IsBT
# test some different kernels with a variety of costs on the full dataset
tune.out=tune(svm, IsBT~., data=pix2, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) 
plot(tune.out, main="Linear")
# cost=1 is best, 0.004364276 cv error
tune.out=tune(svm, IsBT~., data=pix2, kernel="polynomial", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) 
plot(tune.out, main="Polynomial")
# cost=100 is best, 0.003225751
tune.out=tune(svm, IsBT~., data=pix2, kernel="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),gamma=c(0.5,1,2,3,4)))
summary(tune.out) 
plot(tune.out, main="Radial")
# best: cost=10, gamma=4, 0.002624878

# radial seems best - let's use it in a full CV to get detect & false alarm rates

# need to use single df - add features
pix2 = pix
pix2$Class = NULL
pix2$br = pix$Blue / pix$Red
pix2$bg = pix$Blue / pix$Green
pix2$gr = pix$Red / pix$Green
pix2$IsBT = IsBT

detect = rep(0,nfolds) 
falarm = rep(0,nfolds) 
for (i in 1:10) { # train on all 10 folds for each k-value. 
  train = pix2[ -idx[[i]], ]
  test = pix2[ idx[[i]], ]
  svm.fit=svm(IsBT~., data=pix2, kernel="radial", cost=10, gamma=4)
  pc = predict(svm.fit, newdata=pix2[ idx[[i]], ])
  detect[i] = (sum(pc == test$IsBT & test$IsBT==1)/sum(test$IsBT==1)) # %tarps detected correctly
  falarm[i] = (sum(pc != test$IsBT & pc==1)/sum(test$IsBT!=1)) # %non-tarps detected as tarps
}
mean(detect) # 0.9579647
mean(falarm) # 0.001290388
# 5 features
# 0.9782446
# 0.0008983779
# 6 feat: 0.9767595 0.0008003947

# End Cross-Validation on Initial Data



# Load holdout data set (after running holdout.R)
h = read.csv("holdout/holdout_full.csv")

names(h) = c("idx", "Red", "Green", "Blue", "IsBT")
h$br = h$Blue / h$Red
h$bg = h$Blue / h$Green


# Train each algorithm on the initial data set (using any parameters calculated above)
# and then run predictions on the holdout data set and check the detection / false alarm rates.

# KNN, k=7
knn.pred = knn(train = pix.p[,1:3],
               test = h[,2:4],
               cl = IsBT,
               k=7)
y = h$IsBT # test classification
detect = (sum(knn.pred == y & y ==1)/sum(y==1)) # %tarps detected correctly
falarm = (sum(knn.pred != y & knn.pred==1)/sum(y!=1)) # %non-tarps detected as tarps
detect # 0.8299724
falarm # 0.006515565

# LDA
lda.fit=lda(IsBT~Red+Green+Blue+br+bg, data=pix.p)
lda.pred=predict(lda.fit, h) #
pc=lda.pred$class # predicted class
(sum(pc == h$IsBT & h$IsBT==1)/sum(h$IsBT==1)) # detect: 0.9833564
(sum(pc != h$IsBT & pc==1)/sum(h$IsBT!=1)) # false alarm rate: 0.01723127

# QDA
qda.fit=qda(IsBT~Red+Green+Blue+br+bg, data=pix.p)
qda.pred=predict(qda.fit, h) #
pc=qda.pred$class # predicted class
(sum(pc == h$IsBT & h$IsBT==1)/sum(h$IsBT==1)) # detect: 0.9919199
(sum(pc != h$IsBT & pc==1)/sum(h$IsBT!=1)) # false alarm rate: 0.03375589
pc = as.ordered(pc)
levels(pc) = c("Blue Tarp", "Not Blue Tarp")
confusionMatrix(pc, 
      factor(h$IsBT, levels=c(1,0), labels=c("Blue Tarp", "Not Blue Tarp")))

# Logistic regression
glm.fits=glm(IsBT.bin~Red+Green+Blue+br+bg, data=pix.p, family=binomial)
glm.probs=predict(glm.fits, h, type="response")
pc = rep(0,nrow(h))
pc[glm.probs>.5]=1 # predicted class, threshold
y.bin = as.integer(h$IsBT)
(sum(pc == y & y==1)/sum(y==1)) # 0.9901934
(sum(pc != y & pc==1)/sum(y!=1)) # 0.02464245
pc = factor(pc, levels=c(1,0), ordered=TRUE)
levels(pc) = c("Blue Tarp", "Not Blue Tarp")
confusionMatrix(pc, 
                factor(h$IsBT, levels=c(1,0), labels=c("Blue Tarp", "Not Blue Tarp")))

# Random forest (3 features)
rf.fits=randomForest(IsBT~Red+Green+Blue, data=pix.p, ntree=87, mtry=2)
pc = predict(rf.fits, newdata=h )
(sum(pc == y & y==1)/sum(y==1)) # 0.7905387
(sum(pc != y & pc==1)/sum(y!=1)) # 0.005280201

# SVM (5 features)
svm.fit=svm(IsBT~Red+Green+Blue+br+bg, data=pix2, kernel="radial", cost=10, gamma=4)
pc = predict(svm.fit, newdata=h)
(sum(pc == h$IsBT & h$IsBT==1)/sum(h$IsBT==1)) # 0.248895 - overfit?
(sum(pc != h$IsBT & pc==1)/sum(h$IsBT!=1)) # 0.005333978

# try again with linear, cost=1
svm.fit=svm(IsBT~Red+Green+Blue+br+bg, data=pix2, kernel="linear", cost=1)
pc = predict(svm.fit, newdata=h)
(sum(pc == h$IsBT & h$IsBT==1)/sum(h$IsBT==1)) # 0.9907459
(sum(pc != h$IsBT & pc==1)/sum(h$IsBT!=1)) # 0.03250696