# load the data
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

# Splitting Data set
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

# Create train and test sets
qualityTrain = subset(quality, split == T)
qualityTest = subset(quality, split == F)

nrow(qualityTest)

# Building model 

#Model 1 using office vists and narcotics
QualityLog = glm(PoorCare~ OfficeVisits+ Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")

tapply(predictTrain, qualityTrain$PoorCare, mean)

# Model 2 using startedOnCombination and ProviderCount
QualityLog1 = glm(PoorCare~StartedOnCombination+ProviderCount, data = qualityTrain, family = "binomial")
summary(QualityLog1)

# Threshhol value at >0.5
table(qualityTrain$PoorCare, predictTrain >0.5)
#sensitivity
10/25
#Specivicity
70/74

# Generate ROC curve
library(ROCR)
ROCRPred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = T)
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,0.7))


predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
