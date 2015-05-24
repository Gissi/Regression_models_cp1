# start by analyzing the data 

setwd("C:/Users/gissurj/R/Coursera/Regression models")

rm(list=ls())

summary(mtcars)
str(mtcars)
?mtcars
mtcars$am<-factor(mtcars$am,levels=c(0,1), labels = c("Automatic","Manual"))
str(mtcars)
cor(mtcars)
summary(lm(mpg ~ . , data = mtcars))
#lets only fit 

#graphical comparison of the  

library(car)
scatterplotMatrix(~mpg+cyl+hp+wt|am, data=mtcars, main ="Automatic vs Manual")
pairs(mtcars, panel = panel.smooth, main = "Mt Cars data", col = heat.colors(17, alpha = 1))

#The data set seems to be skewed in that sense that every car with manual shift tends to be with more hp and more weight 

xyplot(mpg~hp| am, data = mtcars,
       # col = c("blue", "red"),
       main = "Difference in MPG by transmission",
       xlab = "Transmission",
       ylab = "Miles per Gallon",
       type = 'l'
)

library(ggplot2)

qplot(hp, mpg, color=factor(am), data=mtcars, geom=c("point", "smooth"))
qplot(wt, mpg, color=factor(am), data=mtcars, geom=c("point", "smooth"))
qplot(disp, mpg, color=factor(am), data=mtcars, geom=c("point", "smooth"))
qplot(cyl, mpg, color=factor(am), data=mtcars, geom=c("point"))

install.packages("Hmisc")
library("Hmisc")
correlation<-rcorr(as.matrix(mtcars))

cor(mtcars)

# the correlactio matri shows that the factors that are mainly correlated with mpg seems to be cyl, disp, hp, and wt 
# we therefore will examine model with that in mind 
# as cyl is in my opinion factor variable i will change it to such and run linear fit based on that. 

#convert variable in the data set to ther 
mtcars$am     <- as.factor(mtcars$am)
mtcars$cyl    <- as.factor(mtcars$cyl)


fitallclean <- lm(mpg ~ ., data = mtcars_clean)
summary(fitallclean)

fitall <- lm(mpg ~ ., data = mtcars)
summary(fitall)
fit_am_wt <- lm(mpg ~ wt + am, data = mtcars)
summary(fit_am_wt)
anova(fitall,fit_am_wt)
fit_wt <- lm(mpg ~ wt, data = mtcars)
summary(wit)
anova(fit_wt,fit_am_wt)

fit_wt_hp <- lm(mpg ~ wt + hp, data = mtcars)
summary(fit_wt_hp)
anova(fit_wt,fit_wt_hp)

fit_wt_hp_am <- lm(mpg ~ wt + hp + am, data = mtcars)
summary(fit_wt_hp_am)
anova(fit_wt_hp,fit_wt_hp_am)


fit1 <- lm(mpg ~ hp + wt + disp + cyl, data = mtcars)
summary(fit1)
fit2 <- lm(mpg ~ hp + wt + disp + cyl + am, data = mtcars)
summary(fit2)
fit3 <- lm(mpg ~ hp + wt, data = mtcars)
summary(fit3)


anova(fit3,fit2,fit1,fitall)
#lets plot a boxplot comparing differences in better view for the mpg depend on automatic vs manual 

install.packages("car")
library("car")

#analyze the differenc in data based on auto vs manual
attach(mtcars)
am_mean <-aggregate(mtcars, by=list(am), 
                    FUN=mean, na.rm=TRUE)
am_mean

am_std <-aggregate(mtcars, by=list(am), 
                    FUN=sd, na.rm=TRUE)
am_std

mtcars_clean$am<-factor(mtcars$am,levels=c(0,1), labels = c("Automatic","Manual"))

boxplot(mpg~am, data = mtcars_clean,
                col = c("blue", "red"),
                main = "Difference in MPG by transmission",
                xlab = "Transmission",
                ylab = "Miles per Gallon"
                )

xyplot(mpg~hp| am, data = mtcars_clean,
       # col = c("blue", "red"),
        main = "Difference in MPG by transmission",
        xlab = "Transmission",
        ylab = "Miles per Gallon"
)





# first impression does seem to give an indictation that manual 

# let use model to 

fit1 <- (lm(mpg ~ . , data = mtcars))
sqrt(vif(fit1))

# disp seems related to weight 
fit2 <- (lm(mpg ~ . , data = mtcars))

plot(fit1)
