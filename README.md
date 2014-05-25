Picture-Classification
======================
Final project for Stat 154: Modern Statistical Prediction and Machine Learning. The objective was to classify various pictures
based on location. I worked with Matthew Iannone and Audrey Chou with this project.

The training set consists of a 800x513 dimensional data frame containing 800 labeled examples. The first column contains the labels coded as integers 1-8 (so there are 8 classes). The codes are:

1-coast, 2-forest, 3-highway, 4-inside city, 5 - mountain, 6 - open country, 7 - street, 8 - tall building.

The remaining 512 columns contains the GIST decomposition[1] for each image. GIST is a spectral decomposition of images into a low-dimensional basis which purports to capture global properties of images.

The test set consists of a 1888x512 data frame of unlabeled images. Our job is to label them.
