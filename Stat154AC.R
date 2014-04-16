train = read.csv("~/Boringstuff/train.csv", header = F) #800 513
test = read.csv("~/Boringstuff/test.csv", header = F) #1888 512
names(train) = c("y", names(train)[1:512])

#KMEANS
samps = sample.int(800, 500)
train.train = train[sort(samps),]
train.test = train[-sort(samps),]

accuracy.kmeans = c()
for (j in 1:20){
comp.kmeans = kmeans(train[,-1], centers = 20) #512 500
comp.cluster = as.numeric(comp.kmeans$cluster)
classifiers = c()
for (i in 1:length(unique(comp.cluster))){
  comp.table = table(train[which(comp.cluster==i), 1])
  classifier = as.numeric(names(which(comp.table == max(comp.table))))
  classifiers = c(classifiers, classifier)
}
#1 3 2 8 4 6 8 5 7 5
comp.cluster2 = classifiers[comp.cluster]
accuracy.kmeans = c(accuracy.kmeans, sum(comp.cluster2[which(train[,1]==6)] == 
                    train[which(train[,1]==6),1])/length(train[which(train[,1]==6),1]))
}
mean(accuracy.kmeans)
#0.67375 - k=15
#0.695 - k=18
#0.7225 - k=19
#0.72875 - k=20

#KMEDOIDS
library(cluster)
pam(train[,-1], centers = 10)