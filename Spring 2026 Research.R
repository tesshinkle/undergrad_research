# Spring 2026 Research Project

require(tidyverse)
require(mosaic)
require(ggfortify)
require(lme4)
require(lmerTest)
require(jtools)
require(sjPlot)
require(mgcv)
require(bbmle)


library(readxl)
F1_data_s25 <- read_excel("C:/Users/isabe/OneDrive/Presidential Fellow/April Poster Project.xlsx", 
                          sheet = "F1_data")
View(F1_data_s25)
str(F1_data_s25)

head(F1_data_s25)

F1_data_s25 = F1_data_s25 |>
  mutate(across(c(points), as.numeric)) |>
  mutate(Season=season-2015)

ggplot(F1_data_s25,aes(age, points)) + geom_point() + theme_bw() +
  stat_smooth(method=lm,se=FALSE) + stat_smooth(col= "pink", se= FALSE) +
  geom_smooth(method=lm,formula=y~poly(x,5),color="purple",se=FALSE)


F1_LMM1 = lmer(points~age*Season+(1|name),data=F1_data_s25)
summary(F1_LMM1)
anova(F1_LMM1)

F1_LMM2 = lmer(points~age*Season+(age|name),data=F1_data_s25)
summary(F1_LMM2)
#got a model failed to converge message
anova(F1_LMM2)

plot_model(F1_LMM1, type="eff", terms=c("Season","age"))
plot_summs(F1_LMM1,F1_LMM2)

summ(F1_LMM1)
summ(F1_LMM2)

anova(F1_LMM1,F1_LMM2)

ggplot(data=F1_data_s25,aes(x=age,y=points,group=Season,color=Season)) +
  theme_bw() +
  geom_point() + 
  geom_line(aes(group=name,color=Season),alpha=0.5) + 
  geom_smooth(method=lm,se=FALSE,lwd=2,lty="dashed")

F1_data_s25$name = factor(F1_data_s25$name)

#varying intercepts
F1.gam1 = gam(points~s(age)+s(name,bs="re"),data=F1_data_s25,method="REML")
summary(F1.gam1)
summary(F1.gam1)$s.table

#varying slopes
F1.gam2 = gam(points~s(age)+s(age,name,bs="re"),data=F1_data_s25,method = "REML")
summary(F1.gam2)
summary(F1.gam2)$s.table

#varying intercepts and slopes
F1.gam3 = gam(points~s(age)+s(name,bs="re")+s(age,name,bs="re"),data=F1_data_s25,method="REML")
summary(F1.gam3)
summary(F1.gam3)$s.table

F1.gam4 = gam(points~s(age)+s(name,bs="re")+s(Season,bs="re")+s(age,name,bs="re")+s(name,Season,bs="re"),data=F1_data_s25,method="REML")
summary(F1.gam4)
summary(F1.gam4)$s.table

#use this one
F1.gam5 = gam(points~s(age)+s(name,bs="re")+s(Season,bs="re")+s(age,name,bs="re")+s(name,Season,bs="re")+s(age,Season,bs="re"),data=F1_data_s25,method="REML")
summary(F1.gam5)


#AIC Table
AICtab(F1_LMM1,F1_LMM2,F1.gam1,F1.gam2,F1.gam3,F1.gam4, base=TRUE, sort=TRUE)

#plot of F1.gam4 and four specific drivers
require(itsadug)
plot_smooth(F1.gam4, view = "age", rm.ranef = TRUE, main = "Models of Four Drivers Predicted Points with Age", ylim = c(-100,600))

plot_smooth(F1.gam4, view="age",rm.ranef = FALSE,cond=list(name="Hamilton"),main="intercept + s(age) + s(name)",col="red2", add=TRUE,se=FALSE, lwd=3)
plot_smooth(F1.gam4, view="age",rm.ranef = FALSE,cond=list(name="Verstappen"),main="intercept + s(age) + s(name)", col="blue2", add=TRUE,se=FALSE,lwd=3)
plot_smooth(F1.gam4, view="age",rm.ranef = FALSE,cond=list(name="Vettel"),main="intercept + s(age) + s(name)",col="darkgreen", add=TRUE,se=FALSE,lwd=3)
plot_smooth(F1.gam4, view="age",rm.ranef = FALSE,cond=list(name="Latifi"),main="intercept+s(age)+s(name)", col= "purple3",add=TRUE,se=FALSE,lwd=3)

require(ggeffects)

set.seed(6)
dat.gam2 = predict_response(F1.gam4, terms = c("age","name [sample=6]"))
plot(dat.gam2, facets=TRUE, color=c("darkred","lightblue3","darkgreen","purple3","pink3","goldenrod"), line_size=1.5)

