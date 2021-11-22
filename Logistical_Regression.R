###############################################################################
#                                DATA PROCESSING                              #
###############################################################################
wren.df = read.csv(file="Wren.csv", head=TRUE, sep=',')
attach(wren.df)

#Declare categorical variable as factor
survive = as.factor(survive)

###############################################################################
#                         EXPLORATORY DATA ANALYSIS                           #
###############################################################################
# Pairs plots of numerical variables.
pairs(wren.df[, -c(1)] ,lower.panel = panel.smooth, upper.panel = panel.cor)
# Refined selection of variables.
pairs(wren.df[, -c(1, 2, 5, 10)] ,lower.panel = panel.smooth, upper.panel = panel.cor)

# Correlation matrix
cor.prob(wren.df[2:10])

# All response-predictor variable boxplots
par(mfrow=c(3,3))
# Predictor variable vector
predictor.vector<- 2:10
# Vector of the predictor names
name.vector <- names(wren.df[2:10,])
# Loop to iterate over the predictor variables and produce boxplots
for(i in predictor.vector){
  x<- wren.df[,i]
  if(name.vector[i] != "weight"){
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (mm)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }else{
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (g)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }
}

# Refined response-predictor variable boxplots
par(mfrow=c(2,2))
predictor.vector<- c(2,4,9,10)
# Vector of the predictor names
name.vector <- names(wren.df[2:10,])
# Loop to iterate over the predictor variables and produce boxplots
for(i in predictor.vector){
  x<- wren.df[,i]
  if(name.vector[i] != "weight"){
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (mm)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }else{
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (g)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }
}
par(mfrow=c(1,1))

# Boxplot summary statistics
aggregate(length~survive, data = wren.df, summary)
aggregate(weight~survive, data = wren.df, summary)
aggregate(wskull~survive, data = wren.df, summary)
aggregate(lkeel~survive, data = wren.df, summary)

# 70:30 Training:Testing partition.
set.seed(911)

# The data was partitioned such that the training and test subsets are a 
# disjunctive union of the wren data-set. 
# Rows were randomly selected to avoid selection bias.

# 70% (rounded down) of the rows in the wren data-set were randomly selected.
dataPartition <- sample(nrow(wren.df),  floor(0.7*nrow(wren.df)), replace = FALSE)

# Create data.frame of the training subset
train <- wren.df[dataPartition,]

# Testing set dataframe
test <- wren.df[-dataPartition,]

###############################################################################
#                       LOGISTICAL REGRESSION MODEL                          #
###############################################################################
wren.glm = glm(survive~., data=train, family = binomial)
summary(wren.glm)

# Model Fit
# Residual Deviance for Model Fit
anova(wren.glm, test="Chisq")
pchisq(32.4, df= 29)

# Assumptions
# Independent observations
par(mfrow=c(2,1))
plot(wren.glm$residuals,main = "Residuals vs Time", ylab = "Residuals")

# Outliers
plot(wren.glm, which=4)
par(mfrow=c(1,1))

# Influential observations
sort(cooks.distance(wren.glm)) 
pf(.2110591, 8, 30) # observation 10
pf(.2575168, 8, 30) # observation 34
pf(.2608256, 8, 30) # observation 53
shapiro.test(wren.glm$residuals)

# Multicolinearity
vif(wren.glm)

# Prediction Accuracy
# Logistical Regression Predictions
glm.probs = predict(wren.glm, test, type="response")
glm.pred = rep(0, 17)
glm.pred[glm.probs>0.5] = 1
table(glm.pred,test$survive)

# Accurate Predictions of Logistic Regression
mean(glm.pred==test$survive)
# Error rate of Logistic Regression
mean(glm.pred!=test$survive)