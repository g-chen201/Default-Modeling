# Install packages
install.packages("stats")
install.packages("lattice")
install.packages("latticeExtra")
install.packages("ggplot2")
install.packages("caTools")
install.packages("AUC")

# Include library in R
suppressMessages(library(stats))
suppressMessages(library(lattice))
suppressMessages(library(latticeExtra))
suppressMessages(library(ggplot2))
suppressMessages(library(caTools))
suppressMessages(library(AUC))

# Read in csv file
Data <- read.csv("C:\Users\Jason\Desktop\Default Modeling/DefaultData.csv", header = T)

# View beginning and end of data sets
head(Data)
tail(Data)

# Get type of data series
class(Data)

# There are six data fields including:
# 0 or 1 which is no default and default respectively
# WC/TA which is working capital divided by tangible assets
# RE/TA whic is retained earnings divided by tangible assets
# EBIT/TA whic is earnings before interest and taxes divided by tangible assets
# ME/TL which is market value of equity divided by total liabilities
# S/TA which is sales divided by tangible assets
# These are the financial values used in Altman's Z-Score Model
# Altman's Z-Score model gives a single score but provides guidance around
# what a value means.  So, we may have to "tune" on the probability value
# from logistic regression to determine a threshold that implies severe distress 
# meaning that default is highly likely
# This could be referred to as "Tuning the Probability for Classification"

# Summary statistics
summary(Data)

# Count of no defaults and defaults
no.default.number <- sum(Data$Default == 0)
default.number <- sum(Data$Default == 1)
default.number/no.default.number

# Create data sets to look at statistics by no default and default
no.default.data <- subset(Data, Data$Default == 0)
default.data <- subset(Data, Data$Default == 1)

# Summary statistics for all data, no default data, default data

# First total data set excluding default flag column
data.stat.min <- round(apply(Data[,2:6],2,min),4)
data.stat.mean <- round(apply(Data[,2:6],2,mean),4)
data.stat.median <- round(apply(Data[,2:6],2,median),4)
data.stat.max <- round(apply(Data[,2:6],2,max),4)
data.stat.sd <- round(apply(Data[,2:6],2,sd),4)
  
# Second for no default data excluding default flag column
no.default.data.stat.min <- round(apply(no.default.data[,2:6],2,min),4)
no.default.data.stat.mean <- round(apply(no.default.data[,2:6],2,mean),4)
no.default.data.stat.median <- round(apply(no.default.data[,2:6],2,median),4)
no.default.data.stat.max <- round(apply(no.default.data[,2:6],2,max),4)
no.default.data.stat.sd <- round(apply(no.default.data[,2:6],2,sd),4)

# Third for default data excluding default flag column
default.data.stat.min <- round(apply(default.data[,2:6],2,min),4)
default.data.stat.mean <- round(apply(default.data[,2:6],2,mean),4)
default.data.stat.median <- round(apply(default.data[,2:6],2,median),4)
default.data.stat.max <- round(apply(default.data[,2:6],2,max),4)
default.data.stat.sd <- round(apply(default.data[,2:6],2,sd),4)

# Create plots
# Scatterplot matrix
splom(~Data[,2:6], data = Data, groups = Data$Default, pscales = 0,
      main = list("Independent Variables and Default or No Default", cex = 1.0),
      varname.cex = 1.0, auto.key = list(columns = 2, title = "No Default(0) or Default(1)",
      cex = 0.7))

# Box plots
box.plot.wc <- bwplot(~Data$WC.TA|factor(Data$Default, labels = c("No Default(0)",
                    "Default(1)")), data = Data, layout = c(2,1))
box.plot.re <- bwplot(~Data$RE.TA|factor(Data$Default, labels = c("No Default(0)",
                    "Default(1)")), data = Data, layout = c(2,1))
box.plot.ebit <- bwplot(~Data$EBIT.TA|factor(Data$Default, labels = c("No Default(0)",
                    "Default(1)")), data = Data, layout = c(2,1))
box.plot.me <- bwplot(~Data$ME.TL|factor(Data$Default, labels = c("No Default(0)",
                    "Default(1)")), data = Data, layout = c(2,1))
box.plot.s <- bwplot(~Data$S.TA|factor(Data$Default, labels = c("No Default(0)",
                    "Default(1)")), data = Data, layout = c(2,1))

# Create one box plot output
plot(box.plot.wc, split = c(1,1,1,5))
plot(box.plot.re, split = c(1,2,1,5), newpage = FALSE)
plot(box.plot.ebit, split = c(1,3,1,5), newpage = FALSE)
plot(box.plot.me, split = c(1,4,1,5), newpage = FALSE)
plot(box.plot.s, split = c(1,5,1,5), newpage = FALSE)

# Density plots
density.plot.wc <- densityplot(~Data$WC.TA,  data = Data, group = factor(Data$Default,
                              levels = c(0,1), labels = c("No Default(0)", "Default(1)")),
                              layout = c(1,1))

density.plot.re <- densityplot(~Data$RE.TA,  data = Data, group = factor(Data$Default,
                              levels = c(0,1), labels = c("No Default(0)", "Default(1)")),
                              layout = c(1,1))

density.plot.ebit <- densityplot(~Data$EBIT.TA,  data = Data, group = factor(Data$Default,
                              levels = c(0,1), labels = c("No Default(0)", "Default(1)")),
                              layout = c(1,1))

density.plot.me <- densityplot(~Data$ME.TL,  data = Data, group = factor(Data$Default,
                              levels = c(0,1), labels = c("No Default(0)", "Default(1)")),
                              layout = c(1,1))

density.plot.s <- densityplot(~Data$S.TA,  data = Data, group = factor(Data$Default,
                              levels = c(0,1), labels = c("No Default(0)", "Default(1)")),
                              layout = c(1,1))

# Create one density plot output
plot(density.plot.wc, split = c(1,1,1,5))
plot(density.plot.re, split = c(1,2,1,5), newpage = FALSE)
plot(density.plot.ebit, split = c(1,3,1,5), newpage = FALSE)
plot(density.plot.me, split = c(1,4,1,5), newpage = FALSE)
plot(density.plot.s, split = c(1,5,1,5), newpage = FALSE)

# Logistic regression on full data set as part of exploratory data analysis
# using stepwise approach
eda.model <- glm(as.factor(Data$Default)~., family = binomial("logit"), data = Data)
summary(eda.model)
backwards <- step(eda.model, trace = 0)
formula(backwards)
summary(backwards)


# Split data into training (in-sample) and testing (out-of-sample) sets
# random split but check for defaults to ensure they are represented in
# the test data set by creating a new column which is TRUE and FALSE
# which we can use to create the training and test sets
set.seed(123)
data.spl <- sample.split(Data$Default, SplitRatio = .70)
train.data <- subset(Data, data.spl==TRUE)
test.data <- subset(Data, data.spl==FALSE)

# Total data set had 72 defaults or 72/4000 = .018

# 50 default observations in training data (50/2800 = .0179)
head(train.data)
tail(train.data)
sum(train.data$Default)
nrow(train.data)

# 22 default obervations in test data (22/1200 = .018)
head(test.data)
tail(test.data)
sum(test.data$Default)
nrow(test.data)

# Check using tables - another way to get the counts
table(Data$Default)
table(train.data$Default)
table(test.data$Default)

# We are going to keep all independent variables in as we are more focused
# on prediction and not p-values being too low on coefficients
# Logistic regression fit on training data
train.model <- glm(as.factor(train.data$Default) ~.,
                  family = binomial("logit"), data = train.data)
summary(train.model)

# Default threshold probability such that value above this would signify
# sufficient distress and we would classify as default
default.threshold <- .035

# Confusion matrix for training fit and training data
train.model.train <- predict(train.model, train.data, type = "response")
train.model.train <- ifelse(train.model.train >=default.threshold,1,0)
table(train.model.train, train.data$Default)

# ROC curve and AUC for training fit and training data
train.model.train.roc.data <- cbind(train.model.train, train.data$Default)
train.model.train.roc <- roc(predictions = train.model.train.roc.data[,1],labels = factor(train.data$Default))
train.model.auc <- auc(train.model.train.roc)
plot(train.model.train.roc, col = "red", lty = 1, lwd = 2, main = "ROC Curve for
     Logistic Regression Fit on Training and Predictions on Training Data")
# We want to maximize AUC by making trade-offs around true positives and false
# positives
train.model.auc

# Confusion matrix for training fit and testing data
train.model.test <- predict(train.model, test.data, type = 'response')
train.model.test
train.model.test <- ifelse(train.model.test >=default.threshold,1,0)
train.model.test
table(train.model.test, test.data$Default)

# ROC curve and AUC for training fit and training data
train.model.test.roc.data <- cbind(train.model.test, test.data$Default)
train.model.test.roc <- roc(predictions = train.model.test.roc.data[,1],
                             labels = factor(test.data$Default))
test.model.auc <- auc(train.model.test.roc)
plot(train.model.test.roc, col = "red", lty = 1, lwd = 2, main = "ROC Curve for
     Logistic Regression Fit on Training and Predictions on Test Data")
test.model.auc

