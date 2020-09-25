# This R code reproduces the "BMA vs BMS" experiment from the paper [1]
#
#
# [1] Mattei, P.-A., A Parsimonious Tour of Bayesian Model Uncertainty, arXiv preprint arXiv:1902.05539, 2020



source("functionBMA.R")

# We first load the packages that contain the data

library(AppliedPredictiveModeling)
library(BAS)
library(ISLR)
library(ElemStatLearn)
library(mlbench)
library(lars)
library(ggplot2)
library(ggrepel)
library(latex2exp)
#library(regsel)
library(FWDselect)
library(boot)
library(carData)
library(Ecdat)

# Then we create the data sets and compare selection and averaging



data("abalone")
Y = scale(abalone$Rings)
X = abalone[,2:7]
mse1 = bmavsbms(X,Y)

data(Auto)
Y=scale(Auto$mpg)
X=Auto[,2:7]
mse2 = bmavsbms(X,Y)

data(BostonHousing)
Y=scale(BostonHousing$medv)
X=BostonHousing[-c(4,ncol(BostonHousing))]
mse3 = bmavsbms(X,Y)


data(ozone)
Y=scale(ozone$ozone)
X=ozone[-1]
mse4 = bmavsbms(X,Y)

data("prostate")
X=as.matrix(prostate[,1:8])
Y=scale(as.vector(prostate[,9]))
mse5 = bmavsbms(X,Y)


data("vowel.train")
X=as.matrix(vowel.train[,2:10])
Y=scale(as.vector(vowel.train[,1]))
mse6 = bmavsbms(X,Y)

data("concrete")
X=as.matrix(concrete[,1:8])
Y=scale(as.vector(concrete[,9]))
mse7= bmavsbms(X,Y)

data("fgl")
X=as.matrix(fgl[,2:8])
Y=scale(as.vector(fgl[,1]))
mse8= bmavsbms(X,Y)

data(Carseats)
X=as.matrix(Carseats[,c(2:6,8:9)])
Y=scale(as.vector(Carseats[,1]))
mse9= bmavsbms(X,Y)


data(diabetes)
X=as.matrix(diabetes[,-1])
Y=scale(as.vector(diabetes[,1]))
mse10= bmavsbms(X,Y)

data(episode) # this one's long
X=as.matrix(episode[,-c(ncol(episode)-1,ncol(episode))])
Y=scale(as.vector(episode[,ncol(episode)-1]))
mse11= bmavsbms(X,Y)

data("Anscombe")
X=as.matrix(Anscombe[,2:4])
Y=scale(as.vector(Anscombe[,1]))
mse12= bmavsbms(X,Y)

data("Freedman")
Freedman_complete = Freedman[complete.cases(Freedman),] # to remove missing values
X=as.matrix(Freedman_complete[,1:3])
Y=scale(as.vector(Freedman_complete[,4]))
mse13= bmavsbms(X,Y)

data("Ginzberg")
X=as.matrix(Ginzberg[,4:5])
Y=scale(as.vector(Ginzberg[,6]))
mse14= bmavsbms(X,Y)

data("Clothing")
X=as.matrix(Clothing[,2:13])
Y=scale(as.vector(Clothing[,1]))
mse15= bmavsbms(X,Y)

data("Doctor")
X=as.matrix(Doctor[,2:4])
Y=scale(as.vector(Doctor[,1]))
mse16= bmavsbms(X,Y)

data("Electricity")
X=as.matrix(Electricity[,2:8])
Y=scale(as.vector(Electricity[,1]))
mse17= bmavsbms(X,Y)

data("Oil")
X=as.matrix(Oil[,c(2,3,5:11)])
Y=scale(as.vector(Oil[,1]))
mse18= bmavsbms(X,Y)

data("PPP")
X=as.matrix(PPP[,-3])
Y=scale(as.vector(PPP[,3]))
mse19= bmavsbms(X,Y)

data("University")
X=as.matrix(University[,-14])
Y=scale(as.vector(University[,14]))
mse20= bmavsbms(X,Y)


# We can now plot the results

mse=as.matrix(rbind(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10,mse11,mse12,mse13,mse14,mse15,mse16,mse17,mse18,mse19,mse20))

dat = data.frame(err=unlist(mse[,1]),bound=unlist(mse[,2]),names=c("Abalone","Auto","Housing","Ozone","Prostate","Vowel","Concrete","FGL","Carseats","Diabetes","Episode","Anscombe","Freedman","Ginzberg","Clothing","Doctor","Electricity","Oil","PPP","University"))


ggplot(dat) + aes(x = err, y = bound,label = names)+ geom_point(color = 4) + geom_abline(intercept = 0,slope = 1,colour=2) + xlab("Estimate of model averaging gain obtained with the PAC bounds") + ylab("Out-of-sample MSE gain for model averaging") + geom_text_repel(min.segment.length = 1,box.padding = 0.4)+ coord_fixed()   +xlim(c(-0.006,0.12)) +ylim(c(-0.006,0.049)) #+ theme_bw()  + theme(legend.title = element_blank()) 


# We now fit a robust linear regression model

mod = rlm(unlist(mse[,2])~unlist(mse[,1]),method = "MM",maxit=150) # robust linear regression with all data sets
mod$coefficients

# The "Oil" data set seems to be an outlier, let us try to ignore it and refit the robust linear regression model

mod = rlm(unlist(mse[-18,2])~unlist(mse[-18,1]),method = "MM",maxit=150) # robust linear regression without the "Oil" data set
mod$coefficients



