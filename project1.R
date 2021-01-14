# Kevin Finity, kf2tg

pix <- read.csv("HaitiPixels.csv")
pix
# 63241 obs. of 4 variables
summary(pix)
str(pix)
library(ROCR)

set.seed(1)
# train/test sets not used with 10x
train = sample(1:nrow(pix), 0.9*nrow(pix)) # 90% of rows as training data
pix.train = pix[train,-1] # just the predictors
pix.test = pix[-train,-1]
class.train = pix[train,1] # pull out response values
class.test = pix[-train,1]

# for 10x - full data set
pix.p = pix[,-1] # predictors
pix.c = pix[,1] # response

# create binary response variable
IsBT = rep(0,nrow(pix))
IsBT[pix$Class=="Blue Tarp"] = 1

# K-Nearest Neighbors

library(class)
knn.pred=knn(pix.train,pix.test,class.train,k=1)
table(knn.pred,class.test)
# 189 true positives, 13 false negatives, 3 false positives

# look for best k
cv.error.10=rep(0,10)
for (i in 1:10){
  knn.pred=knn(pix.train,pix.test,class.train,k=i)
  cv.error.10[i]=mean(knn.pred!=class.test)
}
plot(cv.error.10)
# k=8 has the lowest error

knn.pred=knn(pix.train,pix.test,class.train,k=8)
table(knn.pred,class.test) # 190, 12, 7
mean(knn.pred!=class.test) # MSE = 0.0724
# try an alternate measure of success
sum(knn.pred!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp") # 0.0594 MSE for blue tarps


# takes a little while
# repeated with only counting accuracy as % of blue tarps correctly identified
cv.error.20=rep(0,20)
for (i in 1:20){
  set.seed(1)
  knn.pred=knn(pix.train,pix.test,class.train,k=i)
  cv.error.20[i]=(sum(knn.pred!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp"))
  print((sum(knn.pred!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp")))
}
plot(cv.error.20)
# a lot of variation. KNN might not be a good choice. but k=5 is relatively low.
knn.pred=knn(pix.train,pix.test,class.train,k=5)
table(knn.pred,class.test) # 
mean(knn.pred!=class.test) # MSE = 0.0754 - mixed up more of the non-blue-tarp classes
sum(knn.pred!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp") # 0.0495 MSE for blue tarps
# comments: for some reason, even with using set.seed(), I couldn't get reliable results out of this. 
# I'm guessing knn() can pull random numbers a variable number of times? so the RNG isn't always at the same state at the end.

# 10x cross validation
n.k = 20 # test 20 values of k for KNN
klist = seq(n.k) 
nfolds = 10 # 10 folds
n = nrow(pix.p)
cv.err = rep(0,nfolds)
cv.err.bt = rep(0,nfolds)
k.err = rep(0,n.k)
k.err.bt = rep(0,n.k)
set.seed(1)
folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (kv in klist) { # for each k-value
  k.val = kv
  for (i in 1:nfolds) { # train on all 10 folds for each k-value
    i.train = pix.p[-folds[[i]],]
    i.test = pix.p[folds[[i]],]
    i.train.class = pix.c[-folds[[i]]]
    i.test.class = pix.c[folds[[i]]]
    knn.pred = knn(i.train,i.test,i.train.class,k=k.val)
    cv.err[i] = mean(knn.pred!=i.test.class)
    cv.err.bt[i]=(sum(knn.pred!=i.test.class & i.test.class=="Blue Tarp")/sum(i.test.class=="Blue Tarp"))
  }
  k.err[kv] = mean(cv.err) 
  k.err.bt[kv] = mean(cv.err.bt)
}
plot(k.err,xlab="KNN k-values",ylab="Mean Error") # k=11 probably the best on this one
plot(k.err.bt,xlab="KNN k-values",ylab="Blue Tarp Mean Error") # k=8 has the lowest value
k.err[11] # Error 0.0714 for k=11
k.err.bt[8] # Error 0.0313 for k=8


# LDA - Linear Discriminant Analysis
library(MASS)
attach(pix)
lda.fit=lda(Class~Red+Green+Blue,data=pix,subset=train)
lda.fit # display summary - priors, 
#plot(lda.fit) # plot the classes
lda.pred=predict(lda.fit, pix.test)
lda.class=lda.pred$class # predicted class
table(lda.class,class.test) # confusion matrix
mean(lda.class!=class.test) # 0.143 error, 86% accuracy
sum(lda.class!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp") # 0.228 err, 77% for tarps

rates<-prediction(lda.pred, class.test)
##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for WCGS")
lines(x = c(0,1), y = c(0,1), col="red")
# AUC
auc<-performance(rates, measure = "auc")
auc@y.values



# 10x cv
nfolds = 10 # 10 folds
n = nrow(pix.p)
cv.err = rep(0,nfolds)
cv.err.bt = rep(0,nfolds) # Blue Tarp errors
set.seed(2)
folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  i.train = pix.p[-folds[[i]],]
  i.test = pix.p[folds[[i]],]
  i.train.class = pix.c[-folds[[i]]]
  i.test.class = pix.c[folds[[i]]]
  lda.fit=lda(i.train.class~Red+Green+Blue,data=i.train)
  lda.pred=predict(lda.fit, i.test) #.class
  lda.class=lda.pred$class # predicted class
  cv.err[i]=mean(lda.class!=i.test.class)
  cv.err.bt[i]=sum(lda.class!=i.test.class & i.test.class=="Blue Tarp")/sum(i.test.class=="Blue Tarp")
}
mean(cv.err) # 0.144 error - not bad, not great
mean(cv.err.bt) # 0.182 error for Blue Tarps. Notable improvement over the simple validation set approach above.


# QDA 
qda.fit=qda(Class~Red+Green+Blue,data=pix,subset=train)
qda.fit
qda.class=predict(qda.fit, class.test)$class[-train]
table(qda.class,class.test)
mean(qda.class!=class.test) # 0.100 err - better than LDA so far
sum(qda.class!=class.test & class.test=="Blue Tarp")/sum(class.test=="Blue Tarp") # 0.144 err for tarps

# 10x cv
nfolds = 10 # 10 folds
n = nrow(pix.p)
cv.err = rep(0,nfolds)
cv.err.bt = rep(0,nfolds) # Blue Tarp errors
set.seed(2)
folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  i.train = pix.p[-folds[[i]],]
  i.test = pix.p[folds[[i]],]
  i.train.class = pix.c[-folds[[i]]]
  i.test.class = pix.c[folds[[i]]]
  qda.fit=qda(i.train.class~Red+Green+Blue,data=i.train)
  qda.pred=predict(qda.fit, i.test.class)
  qda.class=qda.pred$class[folds[[i]]] # predicted class
  cv.err[i]=mean(qda.class!=i.test.class)
  cv.err.bt[i]=sum(qda.class!=i.test.class & i.test.class=="Blue Tarp")/sum(i.test.class=="Blue Tarp")
}
mean(cv.err) # 0.0988 error - not as good as KNN, but better than LDA
mean(cv.err.bt) # 0.136 error for Blue Tarps. Lower than the validation set approach, not as good as KNN

# create binary response variable
IsBT = rep(0,nrow(pix))
IsBT[pix$Class=="Blue Tarp"] = 1

# Logistic Regression
glm.fits=glm(IsBT~Red+Green+Blue,data=pix,family=binomial,subset=train)
summary(glm.fits) # as expected, Blue has a positive relationship and Red/Green have negative correlations
glm.probs=predict(glm.fits,pix.test,type="response") # predict whether it'll be a blue tarp or not
glm.pred = rep(0,nrow(pix.test))
glm.pred[glm.probs>.5]=1
table(glm.pred,IsBT[-train]) # confusion matrix - how often were the predictions correct
mean(glm.pred!=IsBT[-train]) # 0.0047 error.
mean(glm.pred!=IsBT[-train] & IsBT[-train]==1) # 0.0043 error for blue tarps alone

# testing - ROC
#rates<-prediction(glm.pred, IsBT[-train])
##store the true positive and false postive rates
#roc_result<-performance(rates,measure="tpr", x.measure="fpr")
##plot ROC curve and overlay the diagonal line for random guessing
#plot(roc_result, main="ROC Curve for Logistic Regression")
#lines(x = c(0,1), y = c(0,1), col="red")
# AUC
#auc<-performance(rates, measure = "auc")
#auc@y.values # 0.933


# 10x CV
nfolds = 10 # 10 folds
n = nrow(pix.p)
cv.err = rep(0,nfolds)
cv.err.bt = rep(0,nfolds) # Blue Tarp errors
set.seed(2)
folds = split(sample(n),rep(1:nfolds,length=n)) # split training set into 10 random folds
for (i in 1:10) { # train on all 10 folds for each k-value. 
  i.train = pix.p[-folds[[i]],]
  i.test = pix.p[folds[[i]],]
  i.train.class = IsBT[-folds[[i]]]
  i.test.class = IsBT[folds[[i]]]
  glm.fits=glm(i.train.class~Red+Green+Blue,data=i.train,family=binomial)
  glm.probs=predict(glm.fits,i.test,type="response")
  glm.pred = rep(0,nrow(i.test))
  glm.pred[glm.probs>.5]=1 # predicted class
  cv.err[i]=mean(glm.pred!=i.test.class)
  cv.err.bt[i]=mean(glm.pred!=i.test.class & i.test.class==1)
}
mean(cv.err) # 0.0047 error - very good
mean(cv.err.bt) # 0.0037 error - lower than validation set approach

# Visualizing the separation of the Blue Tarp class from the other classes.
plot(Blue[IsBT==1], Red[IsBT==1]+Green[IsBT==1],xlab="Blue",ylab="Red+Green", col="blue",pch = 3,main="Separation of Blue Tarps")
points(Blue[IsBT==0], Red[IsBT==0]+Green[IsBT==0], col="gray",pch = 4)

# looking at some class color profile histograms. 
hist(Blue[Class=="Blue Tarp"]) # very skewed
hist(Blue[Class=="Rooftop"])
hist(Blue[Class=="Soil"]) # pretty normal
hist(Red[Class=="Soil"]) # very skewed
hist(Blue[Class=="Various Non-Tarp"])
hist(Blue[Class=="Vegetation"])
hist(Green[Class=="Vegetation"])

plot(Blue/Red+Blue/Green, IsBT.bin)

# This is very interesting - great separation with dividing blue by the other two.
plot(Blue[IsBT==1]/Red[IsBT==1], Blue[IsBT==1]/Green[IsBT==1],xlab="B/R",ylab="B/G", col="blue",pch = 3,main="Separation of Blue Tarps")
points(Blue[IsBT==0]/Red[IsBT==0], Blue[IsBT==0]/Green[IsBT==0], col="gray",pch = 4)
points(Blue[pix.c=="Rooftop"]/Red[pix.c=="Rooftop"], Blue[pix.c=="Rooftop"]/Green[pix.c=="Rooftop"], col="black",pch = 4)
points(Blue[pix.c=="Soil"]/Red[pix.c=="Soil"], Blue[pix.c=="Soil"]/Green[pix.c=="Soil"], col="red",pch = 4)
points(Blue[pix.c=="Various Non-Tarp"]/Red[pix.c=="Various Non-Tarp"], Blue[pix.c=="Various Non-Tarp"]/Green[pix.c=="Various Non-Tarp"], col="purple",pch = 4)
points(Blue[pix.c=="Vegetation"]/Red[pix.c=="Vegetation"], Blue[pix.c=="Vegetation"]/Green[pix.c=="Vegetation"], col="green",pch = 4)

levels(pix.c)
names(pix.p)

# look at principal components for RGB data
pr.out=prcomp(pix.p[,1:3], scale=TRUE)
summary(pr.out) 
# Importance of components:
#                           PC1     PC2     PC3
# Standard deviation     1.7092 0.25738 0.11111
# Proportion of Variance 0.9738 0.02208 0.00411
# Cumulative Proportion  0.9738 0.99589 1.00000
pr.out$rotation
#             PC1        PC2        PC3
# Red   0.5765526 -0.6188816  0.5334536
# Green 0.5823318 -0.1467245 -0.7996009
# Blue  0.5731290  0.7716589  0.2758001

biplot(pr.out, scale=0) # plot the data on the PCs
pr.out$rotation=-pr.out$rotation # rotate it by multiplying everything by -1
pr.out$x=-pr.out$x
biplot(pr.out, scale=0) # and view from a different angle

# pairs(pix2)
plot(pix2$br, (pix2$Red / pix2$Green))
plot(pix2$br[IsBT==1], pix2$Red[IsBT==1]/pix2$Green[IsBT==1],xlab="B/R",ylab="R/G", col="blue",pch = 3,main="Separation of Blue Tarps")
points(pix2$br[IsBT==0], pix2$Red[IsBT==0]/pix2$Green[IsBT==0], col="gray",pch = 4)
