##Capstone Project Instrument Variable Regression Code
##Date: 11/28/2016
##William George, Sonny Hydes, Steven Lavell, Ingrid Martinez, Jayesh Rai, David Casale.

#creates and instrument variable for Foreign Born values based on census data, and performs an IV Regression to
#Identify the impact of immigration on housing prices at a neighborhood level.


#Load Libraries
if (!("AER" %in% installed.packages()))
	install.packages("AER")
if (!("lmtest" %in% installed.packages()))
	install.packages("lmtest")
if (!("arm" %in% installed.packages()))
	install.packages("arm")
if (!("corrplot" %in% installed.packages()))
	install.packages("corrplot")
	
library(AER) 
library(lmtest)
library(arm)
library(corrplot)

#Load the data
setwd("C/set path here")
foreign <- read.csv("ForeignBornCensusData.csv", stringsAsFactors = FALSE)

#### Clean the data ####
#Check for NAs/nulls
colsums(is.na(foreign))

#Create columns with the lagged variables
foreign$lagfb <- shift(foreign$foreignborn, 1)
foreign$lagmhv  <- shift(foreign$Median_Home_Value ,1)
foreign$lagmha  <- shift(foreign$Median_Home_Age ,1)
foreign$lagvc  <- shift(foreign$Total_Vacancy ,1)
foreign$lagsfd  <- shift(foreign$Single_Family_Detached ,1)

#Plot the histograms
for (i in 1:ncol(foreign)) {
	x <- foreign[ , i]
	
	#Create a histogram with a normal curve overlay
	h <- hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
    main = PASTE("Histogram of ", colnames(x), " with Normal Curve") 
	xfit<-seq(min(x),max(x),length=40) 
	yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
	yfit <- yfit*diff(h$mids[1:2])*length(x) 
	lines(xfit, yfit, col="blue", lwd=2)

	#time delay to cycle through the histograms
	sys.sleep(7)
}

#Plot the kernel densities
for (o in 1:ncol(foreign)) {
	d <- density(foreign[ , o])
	plot(d, main = paste("Kernel Density of ", colnames(d))
	polygon(d, col="red", border="blue") 
	
	sys.sleep(7)
}

#Variance, Covariance, & Correlation
#Summary of Data
summary(foreign)

#Box Plot of the data frame
boxplot(foreign)

#Variance
fbvar <- var(foreign)
fbvar

#Covariance
fbcov <- cov(foreign)
fbcov

#Check the Corr Plot
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for(u in 1:(n-1)){
        for(j in (u+1):n){
            tmp <- cor.test(mat[,u], mat[,j], conf.level = conf.level)
            p.mat[u,j] <- p.mat[j,u] <- tmp$p.value
            lowCI.mat[u,j] <- lowCI.mat[j,u] <- tmp$conf.int[1]
            uppCI.mat[u,j] <- uppCI.mat[j,u] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

cor.fb <- cor.mtest(foreign,0.95)

## specialized the insignificant value according to the significant level (Marks correlation above .8)
corrplot.mixed(foreign, lower = "number", upper = "ellipse", p.mat = cor.fb[[1]], sig.level=0.2, )
###################################################################################################################




#### Instrument Variable Regression using Two-Stage Least Squares Method ####
fpred <- lm(foreignborn ~ homevalue + lagfb + lagmhv + lagvc + lavsfd + lagmha, data=foreign) 
foreign$Foreign_Pred <- predict(fpred) 
ivr.fb <- lm(homevalue ~ fpred + lagmhv + lagvc + lavsdh + lagmha, data = foreign) 

###Research what this code is doing ####################
simple.comp <- encomptest(homevalue ~ fpred + lagmhv + lagvc + lavsdh + lagmha, homevalue ~ lagfb + lagmhv + lagvc + lavsdh + lagmha, data = foreign) 
ls.ftest <- encomptest(foreignborn ~ homevalue + lagfb + lagmhv + lagvc + lavsdh + lagmha, foreignborn ~ homevalue, data = foreign) 

#Should the vars in this be lagged?  Also fix the coeff names to our names
coefplot(lm(homevalue ~ lagfb + lagmhv + lagvc + lavsdh + lagmha, data = foreign),vertical=FALSE,var.las=1,varnames=c("Education","Unemp","Hispanic","Af-am","Female","Urban","Education")) 
coefplot(ivr.fb , vertical=FALSE,var.las=1,varnames=c("Education","Unemp","Hispanic","Af-am","Female","Urban","Education"))
#####################

#We will need to rewrite the IV regression above for each year and will be repeated five times for 1970, 1980, 1990, 2000, and 2010


###################################################################################################################