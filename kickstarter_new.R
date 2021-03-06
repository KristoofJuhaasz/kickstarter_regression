#install.packages("lattice")
#install.packages("lmtest")
#install.packages("car")
#install.packages("sandwich")
#install.packages("corrplot")
#install.packages("lavaan")
#install.packages("caret")

library(lattice)
library(lmtest)
library(car)
library(sandwich)
library(corrplot)
library(lavaan)
library(caret)



#minta kiv�laszt�sa!!!! 
kickstarter <- read.csv("train_80.csv",header=TRUE,sep=";", dec=",")
summary(kickstarter)

#1 - folytonosok plotol�sa + dummy-k t�bl�zatba hogy mi h�ny sz�zal�k (nincs itt adat)
hist(kickstarter$SUCCESS_RATE, breaks = 200)
hist(kickstarter$SUCCESS_RATE, xlim = c(0, 1.5), breaks = 2000)

hist(kickstarter$NAME_LEN, breaks = 50)
hist(scale(kickstarter$NAME_LEN), breaks = 50)

hist(kickstarter$BLURB_LEN, breaks = 50)
hist(scale(kickstarter$BLURB_LEN), breaks = 50)

hist(kickstarter$GOAL, breaks = 50)
hist(log(kickstarter$GOAL), breaks = 50)
hist(scale(log(kickstarter$GOAL)), breaks = 50)

hist(kickstarter$BACKERS, breaks = 100)
hist(log(kickstarter$BACKERS), breaks = 100)
hist(scale(kickstarter$BACKERS), breaks = 100)

hist(kickstarter$DAYS, breaks = 50)
hist(scale(kickstarter$DAYS), breaks = 50)



#2 - folytonos v�ltoz�k deklar�l�sa fentiek alapj�n

kickstarter$NAME_LEN_N <- scale(kickstarter$NAME_LEN)
kickstarter$BLURB_LEN_N <- scale(kickstarter$BLURB_LEN)
kickstarter$BACKERS_N <- scale(kickstarter$BACKERS) #indokolt lenne logolni, de 0 miatt +1-el probl�m�s �s cs�kken R2!!!!
kickstarter$GOAL_N <- scale(log(kickstarter$GOAL))
kickstarter$DAYS_N <- scale(kickstarter$DAYS)


#3 - line�ris forma tesztel�se

fit<-lm(SUCCESS_RATE~BACKERS_N+DAYS_N+GOAL_N+NAME_LEN_N+BLURB_LEN_N+USD+CATEGORY+SPOTLIGHT+STAFF_PICKED+CURR_TRAILING,data=kickstarter)
summary(fit)
step_fit<-step(fit, direction = "backward", trace=FALSE ) 
summary(step_fit)    #52%

resettest(step_fit)  #kellenek kvadratikus alakok -- kis p H0-t elvetj�k - H0: j� a line�ris forma


#4 - kvadratikus alakok bevon�sa - m�r a sztenderdiz�ltak� mert nem k�ne nagyon elrontania a n�gyzetes kapcsolatot

kickstarter$NAME_LEN_Q <- kickstarter$NAME_LEN_N^2
kickstarter$BLURB_LEN_Q <- kickstarter$BLURB_LEN_N^2
kickstarter$BACKERS_Q <- kickstarter$BACKERS_N^2
kickstarter$GOAL_Q <- kickstarter$GOAL_N^2
kickstarter$DAYS_Q <- kickstarter$DAYS_N^2

fit<-lm(SUCCESS_RATE~BACKERS_N+DAYS_N+GOAL_N+NAME_LEN_N+BLURB_LEN_N+USD+CATEGORY+SPOTLIGHT+STAFF_PICKED+CURR_TRAILING+NAME_LEN_Q+BLURB_LEN_Q+BACKERS_Q+GOAL_Q+DAYS_Q,data=kickstarter)
step_fit<-step(fit, direction = "backward", trace=F ) 
summary(step_fit)    #beker�lt a BACKERS_Q --- javult az R2!!!! - 58%


#5 - homoszkedaszticitas-Breusch-Pagan test (H0=homoszkedaszticitas)

bptest(step_fit)
coeftest(step_fit,vcovHC(step_fit))
#ki kell z�rni a nem szignifik�nsakat!!!!!
step_fit <- lm(SUCCESS_RATE~BACKERS_N+GOAL_N+NAME_LEN_N,data=kickstarter)


#6 - multikollinearit�s

M <- kickstarter[,c(7,20,21,18)]
cor(M)
corrplot(cor(M))
corrplot(cor(M), method="color", type="lower", outline=TRUE, tl.srt=60, tl.col="black")

vif(step_fit)





#7 - k�sz modell bemutat�sa

summary(step_fit)


#8 - TESZT - becsl�s vs eredeti

fitted_success<-(step_fit$fitted.values>=1)
sum(fitted_success==(kickstarter$SUCCESS_RATE>=1))
xtabs(~ (kickstarter$SUCCESS_RATE>=1)+fitted_success)


#9 - TESZT - tan�tott vs teszt
#EXCELBEN!!!!!!!!!!!!!!!!!!!



