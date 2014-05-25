train = read.csv("~/Boringstuff/train.csv", header = F) #800 513
test = read.csv("~/Boringstuff/test.csv", header = F) #1888 512
names(train) = c("y", names(train)[1:512])

train2 = train
train2$y = as.numeric(train$y==6)
library(glmnet)
train.sparse = glmnet(x=as.matrix(train2[,-1]), y=train2[,1], family="binomial")
lambda = numeric()
for (i in 1:length(train.sparse$lambda)){
  lambda [i] = sum(coef(train.sparse, s = train.sparse$lambda[i])[,1]!=0)
}
which(lambda == 24) #17
portfolio = coef(train.sparse, s = train.sparse$lambda[15])[,1]
portfolio = portfolio[which(portfolio!=0)]
portfolio
sparsevectors = c(5, 9, 13, 48, 100, 108, 112, 128, 262, 281, 391, 394, 410, 415, 450, 
                  454, 503, 506)
sparsevectors2 = c(5,9,13,43,57,99,100,108,128,262,281,391,399,410,415,
                   450,454,503,506) 

train2 = train
train2$y = as.numeric(train$y==6)
train2 = train2[, c(1, sparsevectors+1)]

accuracy.lr = c()
for(i in 1:1000){
samps = sample.int(800, 500)
train.train = train2[sort(samps),]
train.test = train2[-sort(samps),]

train.lr = glm(y~., binomial, data=train.train)
train.predict = predict(train.lr, train.test[,-1])
train.predict = as.numeric(train.predict>0)
prop = sum(train.predict==train.test[,1])/length(train.test[,1])
accuracy.lr = c(accuracy.lr, prop)
}
1-mean(accuracy.lr)
#0.09039333 sparse 13 - lambda[12]
#####################################
library(cluster)
accuracy.kmeds = c()
for (j in 1:20){
comp.kmeds = pam(train[,-1], k=20) #512 500
comp.cluster = as.numeric(comp.kmeds$clustering)
classifiers = c()
for (i in 1:length(unique(comp.cluster))){
  comp.table = table(train[which(comp.cluster==i), 1])
  classifier = as.numeric(names(which(comp.table == max(comp.table))))
  classifiers = c(classifiers, classifier)
}
#1 3 2 8 4 6 8 5 7 5
comp.cluster2 = classifiers[comp.cluster]
accuracy.kmeds = c(accuracy.kmeds, sum(comp.cluster2[which(train[,1]==6)] == 
                    train[which(train[,1]==6),1])/length(train[which(train[,1]==6),1]))
}
mean(accuracy.kmeds)
#0.465 - k=8
#0.46 - k=10
#0.44 - k=15
#0.33 - k=20



