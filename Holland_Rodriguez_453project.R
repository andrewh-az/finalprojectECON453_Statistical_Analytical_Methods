library("readxl")
library("dplyr")
library("knitr")
library("openxlsx")
library("ggplot2")
library("BSDA")
library("car")
library("data.table")
library("ggpubr")
setwd("/Users/andrewholland/Documents/R/Project")
options(scipen = 999)
# Set up for R
schooldata.bf <- 
  fread("/Users/andrewholland/Documents/R/Project/Holland_Rodriguez_453project.csv")
plot(schooldata.bf$`Serial No.`,schooldata.bf$`Chance of Admit`,
     pch = 16, col = "black")
# See the distribution of the ratings based on the arbitrary Serial Number
plot(schooldata.bf$`University Rating`,schooldata.bf$`Chance of Admit`
     , pch = 16, col = "black")
cor(schooldata.bf$`University Rating`, schooldata.bf$`Chance of Admit`)
plot(schooldata.bf$CGPA,schooldata.bf$`Chance of Admit`
     , pch = 16, col = "black")
cor(schooldata.bf$CGPA, schooldata.bf$`Chance of Admit`)
plot(schooldata.bf$`University Rating`, schooldata.bf$CGPA,
     pch = 16, col = "black")
cor(schooldata.bf$`University Rating`, schooldata.bf$CGPA)
plot(schooldata.bf$SOP, schooldata.bf$CGPA,
     pch = 16, col = "black")
cor(schooldata.bf$SOP, schooldata.bf$CGPA)
plot(schooldata.bf$SOP, schooldata.bf$`Chance of Admit`,
     pch = 16, col = "black")
cor(schooldata.bf$SOP, schooldata.bf$`Chance of Admit`)
plot(schooldata.bf$SOP, schooldata.bf$`University Rating`,
     pch = 16, col = "black")
cor(schooldata.bf$SOP, schooldata.bf$`University Rating`)
# Get an understanding of the distributions based on different variables
summary(schooldata.bf$`TOEFL Score`)
summary(schooldata.bf$`GRE Score`)
summary(schooldata.bf$`Chance of Admit`)
summary(schooldata.bf$`University Rating`)
summary(schooldata.bf$SOP)
summary(schooldata.bf$LOR)
# Summarize Variables
schooldata <- subset(schooldata.bf, select = -c(`Serial No.`))
schooldata$CGPA <- schooldata$CGPA/10 *4
# Removing the arbitrary Serial # column
# Convert Indian (10.0) CGPA to US (4.0) GPA
plot(schooldata$CGPA,schooldata$`Chance of Admit`
     , pch = 16, col = "darkgreen")
num_records <- nrow(schooldata)
set.seed(828)
shuffled_frame <- sample(num_records)
schooldata <- schooldata[shuffled_frame,]
colnames(schooldata) <- c("GRE","TOEFL", "Rating",
                          "SOP", "LOR", "GPA", "Research", "Admission")
# Rearrange the order of the data points for robust testing
# The seed is arbitrarily decided
# Make naming consistent
Tschooldata <- schooldata[1:350,]
Vschooldata <- schooldata[351:500,]
# Breaking up the data for the holdout method. 
schoolmodel1 <- lm(Admission ~ Rating + GPA + GRE 
             + TOEFL + SOP + LOR + Research, data = Tschooldata) 
summary(schoolmodel1)
# Understand the relationship across all variables in a linear model
schoolmodel2 <- lm(Admission ~ Rating*SOP + GPA + GRE 
             + TOEFL + LOR + Research, data = Tschooldata)
summary(schoolmodel2)
# Try to understand an interaction between Rating and SOP
## SOP = Statement of Purpose Letter Strength {1 - 5}
linearHypothesis(schoolmodel1, "Research = 0")
linearHypothesis(schoolmodel1, "Rating = 0")
linearHypothesis(schoolmodel2, "Research = 0")
linearHypothesis(schoolmodel2, "Rating = 0")
cor(Tschooldata$Admission, Tschooldata$Rating)
cor(Tschooldata$Rating, Tschooldata$SOP)
# Trying to get a better understanding of some relationships
t.test(Tschooldata$GPA, mu = 2, alternative = "greater")
t.test(Tschooldata$GPA, mu = 2)
t.test(Tschooldata$GPA, mu = 0, alternative = "greater")
t.test(Tschooldata$GPA, mu = 0)
## Testing using the t.test command
summarymodel1 <- summary(schoolmodel1)
coefGPA1 <- summarymodel1$coefficients["GPA", "Estimate"]
seGPA1 <- summarymodel1$coefficients["GPA", "Std. Error"]
tvalmodel1 <- coefGPA1/seGPA1
pval1 <- summarymodel1$coefficients["GPA", "Pr(>|t|)"]
if (pval1 < 0.05) {
  cat("Reject the null hypothesis. There is evidence of a positive
      relationship between GPA and admission chance.")
} else {
  cat("Fail to reject the null hypothesis. There is no evidence of
      a positive relationship between GPA and admission chance.")
}
print(summarymodel1)
print(coefGPA1)
print(seGPA1)
print(tvalmodel1)
print(pval1)
## Testing using the model [model 1]
summarymodel2 <- summary(schoolmodel2)
coefGPA2 <- summarymodel2$coefficients["GPA", "Estimate"]
seGPA2 <- summarymodel2$coefficients["GPA", "Std. Error"]
tvalmodel2 <- coefGPA2/seGPA2
pval2 <- summarymodel2$coefficients["GPA", "Pr(>|t|)"]
if (pval2 < 0.05) {
  cat("Reject the null hypothesis. There is evidence of a positive
      relationship between GPA and admission chance.")
} else {
  cat("Fail to reject the null hypothesis. There is no evidence of
      a positive relationship between GPA and admission chance.")
}
print(summarymodel2)
print(coefGPA2)
print(seGPA2)
print(tvalmodel2)
print(pval2)
## [model 2]
# Hypothesis testing on the models
TPredictModel1 <- predict(schoolmodel1, Tschooldata)
TPredictModel2 <- predict(schoolmodel2, Tschooldata)
VPredictModel1 <- predict(schoolmodel1, Vschooldata)
VPredictModel2 <- predict(schoolmodel2, Vschooldata)
plot(TPredictModel1, Tschooldata$Admission, pch = 16, col = "gray") %>%
  abline(a = 0, b = 1, col = "red")
plot(TPredictModel2, Tschooldata$Admission, pch = 16, col = "gray") %>%
   abline(a = 0, b = 1, col = "red")
plot(VPredictModel1, Vschooldata$Admission, pch = 16, col = "gray") %>%
  abline(a = 0, b = 1, col = "red")
plot(VPredictModel2, Vschooldata$Admission, pch = 16, col = "gray") %>%
  abline(a = 0, b = 1, col = "red")
# Checking the relative plots when applying predictions
plot(Tschooldata$GPA,Tschooldata$Admission
     , pch = 16, col = "black")
cor(Tschooldata$GPA, Tschooldata$Admission)
plot(Tschooldata$GPA, TPredictModel2,
     pch = 16, col = "black")
cor(Tschooldata$GPA, TPredictModel2)
# Checking GPA relationship on T data
schoolmodel3 <- lm (Admission ~ Rating + GPA + GRE + 
                      TOEFL + SOP + LOR, data = Tschooldata)
schoolmodel4 <- lm (Admission ~ Rating + GPA + GRE + 
                      TOEFL + SOP, data = Tschooldata)
schoolmodel5 <- lm (Admission ~ Rating + GPA + GRE, data = Tschooldata)
summary(schoolmodel3)
summary(schoolmodel4)
summary(schoolmodel5)
schoolmodel6 <- lm (Admission ~ GPA + GRE + 
                      TOEFL + LOR, data = Tschooldata)
summary(schoolmodel6)
schoolmodel7 <- lm (Admission ~  Rating + GPA + GRE + 
                      TOEFL + LOR, data = Tschooldata)
summary(schoolmodel7)
schoolmodel8 <- lm (Admission ~ GPA*SOP + Rating + GRE + LOR + TOEFL
                    + Research, data = Tschooldata)
summary(schoolmodel8)
schoolmodel9 <- lm (Admission ~ GPA + I(GPA^2) + SOP + Rating + LOR + TOEFL
                    + Research + GRE, data = Tschooldata)
summary(schoolmodel9)
schoolmodel10 <- lm (Admission ~ GPA, data = Tschooldata)
summary(schoolmodel10)
# More Models (Quadratic and more interactions)
## Trying to figure out if there is any kind of interesting behavior
## with the other variables
### Of the models I've tested, Model2 is the best and most robust
### Highest R-square and adj R-square
summary(schooldata$GPA)
pred1 <- predict(schoolmodel2, data.frame(GPA = 3.251, Research = 0, 
                                          SOP = 2.5, Rating = 2,LOR = 3, 
                                          TOEFL = 103, GRE = 308),
                 interval = "prediction")
print(pred1)
## 1st Quartile Predictions
pred2 <- predict(schoolmodel2, data.frame(GPA = 2.72, Research = 0,
                                          SOP = 1, Rating = 1, LOR = 1,
                                          TOEFL = 92, GRE = 290),
                 interval = "prediction")
print(pred2)
## Minimum Predictions
# Running predictions on model2
## There is a lower limit to all of these variables due to the data.
## If most variables are at their realistic lowest, the predictions are negative
## However, if predicting the lower limits from the data, it becomes positive
Tschooldata.exp <- Tschooldata
Tschooldata.exp$GPA_log <- log(Tschooldata.exp$GPA)
exponentmodel1 <- lm(log(Admission) ~ GPA, data = Tschooldata.exp)
exponentmodel2 <- lm(log(Admission) ~ GPA + Research + SOP + Rating + LOR
                     + TOEFL + GRE, data = Tschooldata.exp)
summary(exponentmodel1)
summary(exponentmodel2)
# Test an Exponential model
## Seems to be no exponential relationship on GPA like originally theorized
# No Logit Model - Not testing for a binary outcome Y = {0, 1}
summarymodele1 <- summary(exponentmodel1)
coefGPAe1 <- summarymodele1$coefficients["GPA", "Estimate"]
seGPAe1 <- summarymodele1$coefficients["GPA", "Std. Error"]
tvalmodele1 <- coefGPAe1/seGPAe1
pvale1 <- summarymodele1$coefficients["GPA", "Pr(>|t|)"]
if (pvale1 < 0.05) {
  cat("Reject the null hypothesis. There is evidence of a positive
      relationship between GPA and admission chance.")
} else {
  cat("Fail to reject the null hypothesis. There is no evidence of
      a positive relationship between GPA and admission chance.")
}
print(summarymodele1)
print(coefGPAe1)
print(seGPAe1)
print(tvalmodele1)
print(pvale1)
#####
summarymodele2 <- summary(exponentmodel2)
coefGPAe2 <- summarymodele2$coefficients["GPA", "Estimate"]
seGPAe2 <- summarymodele2$coefficients["GPA", "Std. Error"]
tvalmodele2 <- coefGPAe2/seGPAe2
pvale2 <- summarymodele2$coefficients["GPA", "Pr(>|t|)"]
if (pvale2 < 0.05) {
  cat("Reject the null hypothesis. There is evidence of a positive
      relationship between GPA and admission chance.")
} else {
  cat("Fail to reject the null hypothesis. There is no evidence of
      a positive relationship between GPA and admission chance.")
}
print(summarymodele2)
print(coefGPAe2)
print(seGPAe2)
print(tvalmodele2)
print(pvale2)
# Hypothesis testing the models
Vschooldata.exp <- Vschooldata
Vschooldata.exp$GPA_log <- log(Vschooldata.exp$GPA)
VPredictEModel1 <- predict(exponentmodel1, Vschooldata.exp)
VPredictEModel2 <- predict(exponentmodel2, Vschooldata.exp)
ResidualVModel2 <- Vschooldata$Admission - VPredictModel2
ResidualVeModel1 <- Vschooldata.exp$Admission - VPredictEModel1
ResidualVeModel2 <- Vschooldata.exp$Admission - VPredictEModel2
MSEModel2 <- mean(ResidualVModel2)
MAEModel2 <- mean(abs(ResidualVModel2))
MAPEModel2 <-mean(abs(ResidualVModel2/Vschooldata$Admission)) * 100
MSEModele1 <- mean(ResidualVeModel1)
MAEModele1 <- mean(abs(ResidualVeModel1))
MAPEModele1 <-mean(abs(ResidualVeModel1/Vschooldata.exp$Admission)) * 100
MSEModele2 <- mean(ResidualVeModel2)
MAEModele2 <- mean(abs(ResidualVeModel2))
MAPEModele2 <-mean(abs(ResidualVeModel2/Vschooldata.exp$Admission)) * 100
MSE.MAE.MAPEframe <- data.frame(
  Model = c("Model 2", "Exponential Model 1", "Exponential Model 2"),
  MSE = c(MSEModel2,MSEModele1,MSEModele2),
  MAE = c(MAEModel2,MAEModele1,MAEModele2),
  MAPE = c(MAPEModel2,MAPEModele1,MAPEModele2)
)
print(MSE.MAE.MAPEframe)
# MSE, MAE, MAPE for model 2 and the two exponent models
## Based on MSE alone, Model 2 is better. 