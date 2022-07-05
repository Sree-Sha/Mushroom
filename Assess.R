#install.packages('rpart')
#install.packages('rpart.plot')

library(ggplot2)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(naniar)
library(party)
library(dplyr)
library(e1071)

options(warn=-1)

getwd()
setwd("C:\\Users\\srees\\Desktop\\Notes\\DS\\Assess")

getwd()
# Reading the csv file into mushroom and getting the header / Feature names
mushroom <- read.csv(file = 'mushrooms.csv', header = TRUE , stringsAsFactors = TRUE)
names(mushroom)
# Getting the summary of the attribute count in each of the features & Viewing the imported File
summary(mushroom)

library(naniar)
View(mushroom)
# Visual representation of the data set mushroom - 100% Present, nothing is missing
vis_miss(mushroom)
#To check the dimensions of the data
dim(mushroom)
str(mushroom)

#Taking the backup the data set Mushroom & checking the distinct value of it
mush_backup <- mushroom
mush <- mushroom %>% distinct()

### Bar Graph for plotting a few features - Cap shape, Cap surface, Cap Color
ggplot(mush, aes(x = cap.shape, fill = class)) + geom_bar() + ggtitle("CAP SHAPE")
ggplot(mush, aes(x = cap.surface, fill = class)) + geom_bar() + ggtitle("CAP SURFACE")
ggplot(mush, aes(x = cap.color, fill = class)) + geom_bar() + ggtitle("CAP COLOR")


###Bar Graph plotting of few features - Bruises & Odor
ggplot(mush, aes(x = bruises, fill = class)) + geom_bar() + ggtitle("BRUISES")
ggplot(mush, aes(x = odor, fill = class)) + geom_bar() + ggtitle("ODOR")

### Bar Graph for plotting of few features - Gill Attachment, Gill Spacing, Gill Size, Gill Color
ggplot(mush, aes(x = gill.attachment, fill = class)) + geom_bar() + ggtitle("GILL ATTACHMENT")
ggplot(mush, aes(x = gill.spacing, fill = class)) + geom_bar() + ggtitle("GILL SPACING")
ggplot(mush, aes(x = gill.size, fill = class)) + geom_bar() + ggtitle("GILL SIZE")
ggplot(mush, aes(x = gill.color, fill = class)) + geom_bar() + ggtitle("GILL COLOR")


### Bar Graph for plotting of few features - Stalk Shape & Root
ggplot(mush, aes(x = stalk.shape, fill = class)) + geom_bar() + ggtitle("STALK SHAPE")
ggplot(mush, aes(x = stalk.root, fill = class)) + geom_bar() + ggtitle("STALK ROOT")

### Bar Graph for plotting of few features - Stalk SURFACE above & below ring
s1 <- ggplot(mush, aes(x = stalk.surface.above.ring, fill = class)) + geom_bar() + ggtitle("Stalk Surface Above Ring")
s2 <- ggplot(mush, aes(x = stalk.surface.below.ring, fill = class)) + geom_bar() + ggtitle("Stalk Surface Below Ring")
# Arranging it in grid manner to compare the values and conclude values for the observations
grid.arrange(s1, s2) # ncol = 2)
grid.arrange(s1, s2, ncol = 2)

### Bar Graph for plotting of few features - Stalk COLOR above & below ring
s3 <- ggplot(mush, aes(x = stalk.color.above.ring, fill = class)) + geom_bar() + ggtitle("Stalk Color Above Ring")
s4 <- ggplot(mush, aes(x = stalk.color.below.ring, fill = class)) + geom_bar() + ggtitle("Stalk Color Below Ring")
# Arranging it in grid manner to compare the values and conclude values for the observations
grid.arrange(s3, s4) # ncol = 2)
grid.arrange(s3, s4, ncol = 2)

### Bar Graph for plotting of few features - Veil Type & Veil Color
ggplot(mush, aes(x = veil.type, fill = class)) +  geom_bar() + ggtitle("VEIL TYPE")
ggplot(mush, aes(x = veil.color, fill = class)) +  geom_bar() + ggtitle("VEIL COLOR")

### Bar Graph for plotting of few features - Ring Number & Type
ggplot(mush, aes(x = ring.number, fill = class)) +  geom_bar() + ggtitle("Ring Number")
ggplot(mush, aes(x = ring.type, fill = class)) +  geom_bar() + ggtitle("Ring Type")

### Bar Graph for plotting of few features - Spore Print Color, Population & Habitat
ggplot(mush, aes(x = spore.print.color, fill = class)) +  geom_bar() + ggtitle("Spore Print Color")
ggplot(mush, aes(x = population, fill = class)) +  geom_bar() + ggtitle("Population")
ggplot(mush, aes(x = habitat, fill = class)) +  geom_bar() + ggtitle("Habitat")

# Only feature with 1 attribute
summary(mush$veil.type)
# Completely redundant, Data present but no use in the data 1set

# Remove the features that are not require - //Mainly Veil Type// Create a new data set - ROOM
room <- mush
View(room)

room <- room[-16]
View(room)

library(naniar)
vis_miss(room)

# 22 Columns are there in the ROOM data set
vis_miss(room)
View(room)

# Training and Test Data -- Split into 80% & 20%
set.seed(40)
room[,"train"] <- ifelse(runif(nrow(room))<0.8, 1, 0)
trainset <- room[room$train == "1",]
testset <- room[room$train == "0",]
trainset <- trainset[-23]
testset <- testset[-23]

## DECISION TREE ##

library(party)
library(dplyr)
library(rpart)
library(rpart.plot)
room_tree <- rpart(class~., data = trainset, method = "class")

rpart.plot(room_tree, extra = "auto")
test_data <- testset[-23]
tree_pred <- predict(room_tree, newdata = test_data, type = "class")
table(predicted = tree_pred, actual = testset$class)
mean(tree_pred==testset$class)

View(test_data)
View(trainset)
View(testset)
library(e1071)

# Setting the class to Boolean, considering Edible as '0' and Poisonous as '1'
room2 <- room
room2$class <- ifelse(room2$class == 'e', 0 , 1)
View(room2)

# Training and Test Data -- Split into 80% & 20% - Using for SVM
trainset1 <- room2[room2$train == "1",]
testset1 <- room2[room2$train == "0",]
trainset1 <- trainset1[-23]
testset1 <- testset1[-23]

# SVM
svm_trainset <- trainset1
svm_trainset$class <- as.factor(svm_trainset$class)
svm_churn <- svm(class~., data = svm_trainset) 

summary(svm_churn) 
svm_pred <- predict(svm_churn, newdata = test_data, type = "response")
table(predicted = svm_pred, actual = test_data$class)
mean(svm_pred==testset1$class)
