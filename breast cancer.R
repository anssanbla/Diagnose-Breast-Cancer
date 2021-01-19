# Agus(002), Risma(006), Anissa(007)
# Decision Tree Classification on Breast cancer dataset
lokasi_kerja <- "D:/wd/TugasBesar"
setwd(lokasi_kerja)
getwd()

# Downloading the file
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")

# read the data
data <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(data)

# Remove ID column, col = 1
data <- data[,-1]

# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

names(data) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")

# Numerical values in the response variable are converted to labels
data$Class <- factor(data$Class, levels=c(2,4), labels=c("benign", "malignant"))
print(summary(data))

# Dividing the dataset into training and validation sets. There are many ways to do this.
# Alternate method is also listed here.
set.seed(123)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
validationData <- data[ind==2,]

dataFormula <- Class ~ ClumpThickness + UniformityCellSize + UniformityCellShape + MarginalAdhesion + SingleEpithelialCellSize + BareNuclei + BlandChromatin + NormalNucleoli + Mitoses
Breast_Cancer_rpart <- rpart(dataFormula, data = trainData, control = rpart.control(minsplit = 10))
rpartMod <- rpart(Class ~ ClumpThickness + UniformityCellSize + UniformityCellShape + MarginalAdhesion + SingleEpithelialCellSize + BareNuclei + BlandChromatin + NormalNucleoli + Mitoses, data = trainData, method = "class")
plot(model)
text(model, digits = 3)
printcp(rpartMod)

plotcp(rpartMod)
print(Breast_Cancer_rpart)

prop.table(table(trainData$Class))

library(caTools)

# Alternate method 
set.seed(123)
split = sample.split(data$Class, SplitRatio = 0.7)
split

# Create training and testing sets
dataTrain = subset(data, split == TRUE)
dataTest = subset(data, split == FALSE) 

install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")

library(rpart)
library(rpart.plot)
library(party)

# Creating Model
set.seed(123)
model = rpart(Class ~ ., data=trainData, method="class", minsplit = 10, minbucket = 10, cp = -1)
par(xpd = NA)
rpart.plot(model)

#Predicted
predicted.classes = predict(model, validationData, type = "class")
head(predicted.classes)
head(validationData$Class)

mean(predicted.classes == validationData$Class)

dim(trainData)
dim(validationData)
