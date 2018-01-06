#import libraries 
install.packages("statsr")
library(statsr)
library(MASS)
library(dplyr)
library(ggplot2)
install.packages("BAS")
library(BAS)

#import dataset
venice_house<-read.csv(file.choose(),header=TRUE)
head(venice)
str(venice)
ggplot(data=venice_house,aes(x=number_of_reviews,y=price))+geom_point() 
venice_house$price<-as.numeric(venice_house$price)
# glm model 
price_point=lm(price~number_of_reviews+host_listings_count+accommodates+bedrooms+bathrooms+
                 square_feet+minimum_nights+maximum_nights,data=venice_house)
price_point$coefficients

#Bayesian model averaging 
price_point1=bas.lm(price~number_of_reviews+host_listings_count+accommodates+bedrooms+bathrooms+
                      square_feet+minimum_nights+maximum_nights,data=venice_house)
summary(price_point1)
plot(price_point1,ask=FALSE)

#coefficients
coef_price_point1=coefficients(price_point1)
coef_price_point1
plot(coef_price_point1,subset=c(9,7,3),ask=FALSE)
confint(coef_price_point1)

#predictions
BPM_pred_price=predict(price_point1,estimator="BPM",se.fit=TRUE)
summary(BPM_pred_price) 
MPM_pred_price=predict(price_point1,estimator="MPM")
opt=which.max(BPM_pred_price$fit)
#what characteristics lead to the highest price per night
opt = which.max(BPM_pred_price$fit)
t(venice_house[opt,])
#95% credible interval 
ci_price = confint(BPM_pred_price, parm="pred")
ci_price
ci_price[opt,]
exp(ci_price[opt,])

