# PS7 Yarberry 

library(readr)
wages <- read_csv("ModelingOptimization/wages.csv")
View(wages)

# problem 5 
wages <- wages[!is.na(wages$hgc), ]
wages <- wages[!is.na(wages$tenure), ]

# problem 6 
install.packages("stargazer")
library('stargazer')
stargazer(wages)

# problem 7 
est.1 <- lm(logwage ~ hgc + college + tenure +tenure^2 + age + married , data = wages)
stargazer(est.1)

df.mean.imp <- wages
mean.log.wage <- mean(wages$logwage, na.rm = TRUE)
df.mean.imp$logwage[is.na(wages$logwage)] <- mean.log.wage
stargazer(df.mean.imp)

est.2 <- lm(logwage ~ hgc + college + tenure +tenure^2 + age + married , data = df.mean.imp)
stargazer(est.2)

df.mar <- wages
df.mar$mar.logwage <- predict(est.1, df.mar)
df.mar$logwage[is.na(wages$logwage)] <- df.mar$mar.logwage[is.na(wages$logwage)]

est.3 <- lm(logwage ~ hgc + college + tenure +tenure^2 + age + married , data = df.mar)
stargazer(df.mar)

install.packages("mice")
library('mice')

df.mice <- mice(wages, seed = 1234)
est.4 <- lm(logwage ~ hgc + college + tenure +tenure^2 + age + married , data = df.mice$data)
stargazer(est.4)

stargazer(est.1, est.2, est.3, title="Regression Results")

