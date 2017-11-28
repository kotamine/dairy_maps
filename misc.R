
library(dplyr)
library(lme4)

library(quantreg)

# load necessary packages for importing the function
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)


lm(weight ~ poly(Time, 3) + I(Diet) + I(Chick), data=ChickWeight) %>% summary()

lm(weight ~ poly(Time, 3) + I(Chick) +  I(Diet) , data=ChickWeight) %>% summary()

lmer(weight ~ poly(Time, 3) + I(Chick) +  I(Diet) + (1|Chick) , data=ChickWeight) %>% summary()

lmer(weight ~ poly(Time, 3) +I(Diet) + I(Chick) +  (1|Time) , data=ChickWeight) %>% summary()


m1 <- lm(weight ~ poly(Time, 3) + I(Diet), data=ChickWeight) 

summary(m1)
summary(m1,robust = T)



m2 <- rlm(weight ~ poly(Time, 3) + I(Diet), data=ChickWeight) 


