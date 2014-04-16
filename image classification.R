setwd("~/Dropbox/School/Statistics/Stat 154 Spring 2014/Picture Classification")
###testing from known sample
img = read.csv("train.csv", header = F)
names(img) = c("V1", as.character(1:512))
img$V1 = as.factor(img$V1)

test.rows = sample(1:dim(img)[1], 300)
test.rows = test.rows[order(test.rows)]
training.sample = img[test.rows, ]
testing.sample = img[-test.rows, -1]

library("e1071")
img.svm = svm(V1~., data = training.sample, type = "C-classification")
predicted.img.class = predict(img.svm, testing.sample)

test = predicted.img.class == img[-test.rows, 1]
names(test) = img[-test.rows, 1]
test
mean(test)

1 - mean(test[img[-test.rows, 1]==8])
#[1] 0.109375
1 - mean(test[img[-test.rows, 1]==7])
#[1] 0.08928571
1 - mean(test[img[-test.rows, 1]==6])
#[1] 0.5671642
1 - mean(test[img[-test.rows, 1]==5])
#[1] 0.09090909
1 - mean(test[img[-test.rows, 1]==4])
#[1] 0.1578947
1 - mean(test[img[-test.rows, 1]==3])
#[1] 0.2058824
1 - mean(test[img[-test.rows, 1]==2])
#[1] 0.1323529
1 - mean(test[img[-test.rows, 1]==1])
#[1] 0.1846154
#beat Matt 1 4 5 & 7, so method will be favored in this case
#[1] 0.802 this is the overall prediction accuracy based on created test data

###completing the model.
train_img = read.csv("train.csv", header = F)
names(train_img) = c("V1", as.character(1:512))
train_img$V1 = as.factor(train_img$V1)
test_img = read.csv("test.csv", header = F)
names(test_img) = as.character(1:512)
img.svm = svm(V1~., data = train_img, type = "C-classification")
predicted.img.class = predict(img.svm, test_img)

write.csv(data.frame(id = c(1:length(predicted.img.class)),Predictions =  predicted.img.class), file = "predictions.cs")
#0.76589 was the actual accuracy went tested against the Kaggle data