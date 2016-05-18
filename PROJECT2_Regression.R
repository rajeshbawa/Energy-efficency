################################################################
#########PROJECT 1##############################################
#########ENERGY_EFFICENCY DATA-SET############################
setwd("~/Documents/master_app_stats/adv_regression/Project1/BAWA_PROJ")
file1 <- read.csv("energy_efficency_data.txt", header = T, sep="\t")
#########################################################
####################EXPLORATORY DATA ANALYSIS###########
pdf("Histogram_prob_density1.pdf", width = 18, height = 25)
par(lwd=1, cex=1, mfrow = c(5, 2), cex.lab=2)
hist(file1$REL.COMPACT, xlab="Relative Compactness", col="coral1", main=" ")
hist(file1$S.AREA, xlab="Surface Area", col="coral1", main=" ")
hist(file1$W.AREA, xlab="Wall Area", col="coral1", main=" ")
hist(file1$R.AREA, xlab="Roof Area", col="coral1", main=" ")
hist(file1$O.HEIGHT, xlab="Overall Height", col="coral1", main=" ")
hist(file1$GLAZING.AREA, xlab="Glaze Area", col="coral1", main=" ")
hist(file1$HEATING.LOAD, xlab="Heating Load", col="coral1", main=" ")
hist(file1$COOL.LOAD, xlab="Cooling Load", col="coral1", main=" ")
hist(file1$ORIENTATION, xlab="Orientation", col="coral1", main=" ")
hist(file1$GLAZE.AREA.DIS, xlab="Glaze Area Distribution", col="coral1", main=" ")
dev.off()
####So, definitely the variables are not normaly distributed
####looking at these independent variables being not normally distributed, it will 
####better to do spearmans on the all variables by the response variables (report p-values on it)
###############Spearmans rank correlation between the variables and response variables (y1, y2)
library(pspearman)
########with heating load
x1 <- spearman.test(file1$HEATING.LOAD, file1$REL.COMPACT, alternative = "two.sided", approximation = "t-distribution")
x2 <- spearman.test(file1$HEATING.LOAD, file1$S.AREA, alternative = "two.sided", approximation = "t-distribution")
x3 <- spearman.test(file1$HEATING.LOAD, file1$W.AREA, alternative = "two.sided", approximation = "t-distribution")
x4 <- spearman.test(file1$HEATING.LOAD, file1$R.AREA, alternative = "two.sided", approximation = "t-distribution")
x5 <- spearman.test(file1$HEATING.LOAD, file1$O.HEIGHT, alternative = "two.sided", approximation = "t-distribution")
x6 <- spearman.test(file1$HEATING.LOAD, file1$GLAZING.AREA, alternative = "two.sided", approximation = "t-distribution")
x7 <- spearman.test(file1$HEATING.LOAD, file1$ORIENTATION, alternative = "two.sided", approximation = "t-distribution")
x8 <- spearman.test(file1$HEATING.LOAD, file1$GLAZE.AREA.DIS, alternative = "two.sided", approximation = "t-distribution")
##############################################
######among the input variables themselves####
par(mfrow=c(1,1), cex.lab = 2)
input.variables <- file1[, c(1:8)]
pdf("scatterplot_matrix_energy.pdf", height = 25, width = 25)
pairs(input.variables, pch = 21, cex=0.5, col="red") #scatter plot for all input variables
pdf("scatterplot_matrix_energy1.pdf", height = 25, width = 25)
pairs(file1[, c(1:9)], pch = 21, cex = .5, col="red") #scatter plot for all variables
dev.off()
#################correlation matrix between input variables (independent variables)
library(psych)
input.variables.1 <- as.data.frame(file1[, c(1:8)])
corr.input1 <- cor(input.variables.1, use = "pairwise", method = "pearson")
#########corr.matrix#############################
########GRAPHING THE CORRELATION MATRIX FOR INPUT VARIABLES
###heatmap of the correlation matrix####
library(ggplot2)
library(reshape2)
m <-melt(corr.input1)
p <- ggplot(data=m, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
###########################
#set up a coloring scheme using colorRampPalette
red=rgb(1,0,0); green=rgb(0,1,0); blue=rgb(0,0,1); white=rgb(1,1,1)
RtoWrange<-colorRampPalette(c(red, white ) )
WtoGrange<-colorRampPalette(c(white, green) ) 
p <- p + scale_fill_gradient2(low=RtoWrange(100), mid=WtoGrange(100), high="gray")
plot(p)
######################REGRESSION############################################
########This gives us an indication of collinearity and relationship among variables before doing regression.
##############################################################
###Building linear models#########
#######Simple model without any transformation or correction for assumptions#####
energy.model1 <- lm(HEATING.LOAD~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1, qr=T)
summary(energy.model1)
anova(energy.model1)
stack.diag1 <- ls.diag(energy.model1)
names(stack.diag1)
par(mfrow=c(1,2))
plot(energy.model1$fitted.values,stack.diag1$stud.res, ylab="Externally studentized residuals", xlab = "Fitted Values", main = "Residual vs Fitted", pch = 21, cex=0.5, col = "red")
abline(h =0, untf = FALSE, col="blue")
qqnorm(stack.diag1$stud.res, col="red", ylab = "Externally studentized residuals", xlab = "Theoretical quantiles", main = "Normal probability plot", cex=0.5)
qqline(stack.diag1$stud.res, col="blue", add=T, cex=3)
############transform the response variable
log.heatingLoad <- unlist(lapply(file1$HEATING.LOAD, log))
file1.logresponse <- data.frame(log.heatingLoad, file1)
energy.model2 <- lm(log.heatingLoad~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1.logresponse, qr=T)
summary(energy.model2)
anova(energy.model2)
stack.diag2 <- ls.diag(energy.model2)
names(stack.diag2)
par(mfrow=c(1,2))
plot(energy.model2$fitted.values,stack.diag2$stud.res, ylab="Externally studentized residuals", xlab = "Fitted Values", main = "Residual vs Fitted", pch = 21, ylim = c(-4, 4), cex=0.5, col="red")
abline(h =0, untf = FALSE, col="blue")
qqnorm(stack.diag2$stud.res, col="red", ylab = "Externally studentized residuals", xlab = "Theoretical quantiles", main = "Normal probability plot", cex=0.5)
qqline(stack.diag2$stud.res, col="blue", add=T, cex=3)
#####default residual plots in R
par(mfrow=c(2,2))
plot(energy.model2)
#####################################
##################Looking at problematic data points ####################
####Using log transform the 18, 19, 20 observations look problematic
file2.logresponse[c(17, 18, 19, 20), ]
####look at the residuals
library(MASS)
d1 <- cooks.distance(energy.model2)
r <- studres(energy.model2)
################lets try weighted least squares on the log transformation to deal with this
#####looking at obs with highest residuals
rabs <- abs(r)
file2.residuals.1 <- data.frame(file1.logresponse, rabs)
sorted.residuals <- file2.residuals.1[order(-rabs), ]
sorted.residuals[1:25, ]
##########################measuring collinearity####################
####find the linear dependent variables
alias(energy.model2) ###there are none, we removed the singular variable roof area
library(car)
vif(energy.model1) ####prints variation inflation factors
vif(energy.model2)
######################################################################################
#######################LOOK at the influencial datapoints#################
###influential datapoints are:
##studentized residuals
obs <- 1:768
rabs1 <- data.frame(obs, rabs, stack.diag2$hat, stack.diag2$cooks, stack.diag2$dfits)
names(rabs1) <- c("obs", "studRes","hat", "cooks", "dfits")
par(mfrow=c(2,2))
plot(rabs1$obs, rabs1$studRes, ylab="Abs.Deleted Residuals", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=3, untf = FALSE, col="blue", cex=3)
plot(rabs1$obs, rabs1$hat, ylab="Hat (H)", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.03, untf = FALSE, col="blue", cex=3)
plot(rabs1$obs, rabs1$cooks, ylab="Cook's Distance", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs1$obs, rabs1$dfits, ylab="DFIT", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.4, untf = FALSE, col="blue", cex=3)
##################
##############################
############################################
############################################################
#################################################################################
#All possible regressions
library(leaps)
library(car)
library(qpcR)
############################
leaps.test1 <- regsubsets(HEATING.LOAD~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1, nbest=3, nvmax=8, method="seqrep", really.big=F)
leaps.test2 <- leaps(x = as.matrix(file1[, c(1:3,5:8)]), y=file1.logresponse$log.heatingLoad, nbest=3)
leaps.test3 <- leaps(x = as.matrix(file1[, c(1:3,5:8)]), y=file1.logresponse$log.heatingLoad, nbest=3, method="adjr2")
###############################
####running the best 3 regression models
####Log(HEATING.LOAD) ~ REL.COMPACT + W.AREA + O.HEIGHT + GLAZING.AREA + GLAZE.AREA.DIS 
energy.model3 <- lm(log.heatingLoad~REL.COMPACT+W.AREA+O.HEIGHT+GLAZING.AREA+GLAZE.AREA.DIS, data=file1.logresponse, qr=T)
summary(energy.model3)
anova(energy.model3)
stack.diag3 <- ls.diag(energy.model3)
names(stack.diag3)
par(mfrow=c(1,2))
plot(energy.model3$fitted.values,stack.diag3$stud.res, ylab="Externally studentized residuals", xlab = "Fitted Values", main = "Residual vs Fitted", pch = 21, cex=0.5, col="red")
abline(h =0, untf = FALSE, col="blue")
qqnorm(stack.diag3$stud.res, col="red", ylab = "Externally studentized residuals", xlab = "Theoretical quantiles", main = "Normal probability plot", cex=0.5)
qqline(stack.diag3$stud.res, col="blue", add=T, cex=3)
#############
vif(energy.model3)
PRESS(energy.model3)$stat
#############
##Influence measures
rabs3 <- data.frame(obs, abs(stack.diag3$stud.res), stack.diag3$hat, stack.diag3$cooks, stack.diag3$dfits)
names(rabs3) <- c("obs", "studRes","hat", "cooks", "dfits")
#################
par(mfrow=c(2,2))
plot(rabs3$obs, rabs3$studRes, ylab="Abs.Deleted Residuals", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=2.5, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$hat, ylab="Hat (H)", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$cooks, ylab="Cook's Distance", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$dfits, ylab="DFIT", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=c(0.2,-0.2), untf = FALSE, col="blue", cex=3)
##################
#######Log(HEATING.LOAD) ~ REL.COMPACT + S.AREA + W.AREA + O.HEIGHT +
# GLAZING.AREA + GLAZE.AREA.DIS 
energy.model4 <- lm(log.heatingLoad~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+GLAZE.AREA.DIS, data=file1.logresponse, qr=T)
summary(energy.model4)
anova(energy.model4)
stack.diag4 <- ls.diag(energy.model4)
names(stack.diag4)
par(mfrow=c(1,2))
plot(energy.model4$fitted.values,stack.diag4$stud.res, ylab="Externally studentized residuals", xlab = "Fitted Values", main = "Residual vs Fitted", pch = 21, cex=0.5, col="red")
abline(h =0, untf = FALSE, col="blue")
qqnorm(stack.diag4$stud.res, col="red", ylab = "Externally studentized residuals", xlab = "Theoretical quantiles", main = "Normal probability plot", cex=0.5)
qqline(stack.diag4$stud.res, col="blue", add=T, cex=3)
#################
vif(energy.model4)
PRESS(energy.model4)$stat
######################
par(mfrow=c(2,2))
plot(energy.model4)
###########
##Influence measures
rabs3 <- data.frame(obs, abs(stack.diag4$stud.res), stack.diag4$hat, stack.diag4$cooks, stack.diag4$dfits)
names(rabs3) <- c("obs", "studRes","hat", "cooks", "dfits")
#################
par(mfrow=c(2,2))
plot(rabs3$obs, rabs3$studRes, ylab="Abs.Deleted Residuals", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=2.5, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$hat, ylab="Hat (H)", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$cooks, ylab="Cook's Distance", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$dfits, ylab="DFIT", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=c(0.2,-0.2), untf = FALSE, col="blue", cex=3)
###########################
##################
#######
#Log(HEATING.LOAD) ~  S.AREA + W.AREA + 
#O.HEIGHT + GLAZING.AREA + ORIENTATION + GLAZE.AREA.DIS
energy.model5 <- lm(log.heatingLoad~S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1.logresponse, qr=T)
summary(energy.model5)
anova(energy.model5)
stack.diag5 <- ls.diag(energy.model5)
names(stack.diag5)
par(mfrow=c(1,2))
plot(energy.model5$fitted.values,stack.diag5$stud.res, ylab="Externally studentized residuals", xlab = "Fitted Values", main = "Residual vs Fitted", pch = 21, cex=0.5, col="red")
abline(h =0, untf = FALSE, col="blue")
qqnorm(stack.diag5$stud.res, col="red", ylab = "Externally studentized residuals", xlab = "Theoretical quantiles", main = "Normal probability plot", cex=0.5)
qqline(stack.diag5$stud.res, col="blue", add=T, cex=3)
##########
vif(energy.model5)
PRESS(energy.model5)$stat
######################
par(mfrow=c(2,2))
plot(energy.model5)
#########################
##Influence measures
rabs3 <- data.frame(obs, abs(stack.diag5$stud.res), stack.diag5$hat, stack.diag5$cooks, stack.diag5$dfits)
names(rabs3) <- c("obs", "studRes","hat", "cooks", "dfits")
#################
par(mfrow=c(2,2))
plot(rabs3$obs, rabs3$studRes, ylab="Abs.Deleted Residuals", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=2.5, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$hat, ylab="Hat (H)", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$cooks, ylab="Cook's Distance", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=0.02, untf = FALSE, col="blue", cex=3)
plot(rabs3$obs, rabs3$dfits, ylab="DFIT", xlab="Observation", col = "red", pch =21, cex=0.5)
abline(h=c(0.2,-0.2), untf = FALSE, col="blue", cex=3)
#######################################
###Stepwise Regression
#Backward
step1 <- step(lm(log.heatingLoad~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1.logresponse), direction="backward")
step1$anova # display results
step2 <- stepAIC(fit, direction="forward", trace=T)
step2$anova # display results
####
#Forward
step2 <- step(lm(log.heatingLoad~1, data=file1.logresponse), direction="forward", scope=~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS)
step2$anova
#####
#Both
step3 <- step(lm(log.heatingLoad~REL.COMPACT+S.AREA+W.AREA+O.HEIGHT+GLAZING.AREA+ORIENTATION+GLAZE.AREA.DIS, data=file1.logresponse), direction="both")
step3$anova
##########################################
################################################
###CROSSS-VALIDATION
library(DAAG)
library(boot)
par(cex =0.5)
val1 <- CVlm(data=file1.logresponse, form.lm=formula(log.heatingLoad~REL.COMPACT+W.AREA+O.HEIGHT+GLAZING.AREA+GLAZE.AREA.DIS), plotit = "Observed", m=10)
###
model.val <- glm(log.heatingLoad~REL.COMPACT+W.AREA+O.HEIGHT+GLAZING.AREA+GLAZE.AREA.DIS, data=file1.logresponse, family = gaussian)
summary(model.val)
val2 <- cv.glm(data = file1.logresponse, glmfit = model.val, K=10)
val2.1 <- cv.glm(data = file1.logresponse, glmfit = model.val, K=nrow(file1.logresponse))
#################################
########Robust regression (IRLS)
####using rlm from MASS packages for IRLS (Iterative weighted least squares method)
energy.model6 <- rlm(log(HEATING.LOAD)~REL.COMPACT+W.AREA+O.HEIGHT+GLAZING.AREA+GLAZE.AREA.DIS, data=file1.logresponse, method = "MM", maxit=100)
summary(energy.model6)
file2.1.residuals.weights <- data.frame(energy.model2.1$residuals, energy.model2.1$weights)
file2.1.residuals.weights.1 <- file2.1.residuals.weights[order(-file2.1.residuals.weights$energy.model2.1.residuals), ]
file2.1.residuals.weights.1[1:25, ]



