#1 
#b 
week<-c(3,4,4,4,8,8,16,22,24,30)
censored<-c(0,0,0,1,0,1,0,0,1,1)

week.surv <- survfit(Surv(week, censored==0)~ 1, conf.type="none", se.fit=FALSE) 
summary(week.surv)

#plot
plot(week.surv, mark.time = TRUE, 
     pch = 1, col = 4, 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Weeks', 
     ylab = 'Survival Distribution Function')

#2
#a
year<-c(1.1,2.6,2.8,3.1,3.4,3.5,3.5,3.6,3.7,3.8,3.8,4.0,4.1,5.6)
censored<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
year.surv <- survfit(Surv(year, censored==0)~ 1, conf.type="none", se.fit=FALSE) 
summary(year.surv)
#b
#plot
plot(year.surv, mark.time = TRUE, 
     pch = 1, col = 4, 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')

#3
#c
#KM curves
gender<-c('F','F','F','F','F','F','F','F','M','M','M','M','M','M','M')
year<-c(3.6,6.7,8.2,9.3,10,10,10,10,4.2,5,5.3,6.7,8.2,10,10)
censored<-c(0,0,0,0,1,1,1,1,0,0,0,0,0,1,1)
year.surv <- survfit(Surv(year, censored==0)~ gender, conf.type="none", se.fit=FALSE) 
summary(year.surv)
#plot
plot(year.surv, mark.time = TRUE, 
     pch = 1, col = c('blue','red'),
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')
#be sure to note that female is blue and male is red

#log-rank
survdiff(Surv(year, censored==0)~ gender)

#4
smoker<-c('y','y','y','y','y','y','y','y','y','n','n','n',
          'n','n','n','n','n','n','n','n')
year<-c(1.1,1.6,2.1,2.4,2.7,3.6,4.7,4.8,5.1,3.6,4.5,4.6,4.8,5.7,5.8,
        6.7,7.8,10.5,11.3,12.6)
censored<-c(0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,1,0,1)
year.surv <- survfit(Surv(year, censored==0)~ smoker, conf.type="none", se.fit=FALSE) 
#plot
#be sure to note that blue line is for non-smokers and red line
#is for smokers
plot(year.surv, mark.time = TRUE, 
     pch = 1, col = c('blue','red'),
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')
#log-rank
survdiff(Surv(year, censored==0)~ smoker)

#5
#a
pres<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter6/DATA for Exercise 6.5.csv')
model<-coxph(Surv(pres$lifespan,pres$assassinated==0)~age+yearstart+yearsinoffice,data=pres)
summary(model)
#b
reduced_model<-coxph(Surv(pres$lifespan,pres$assassinated==0)~age,data=pres)
summary(reduced_model)

#6
#a
install.packages("tidyverse")
library(tidyverse)
heartmod<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter6/DATA for Exercise 6.6.csv')
heartmod<-heartmod %>% mutate(nyha1=if_else(heartmod$NYHAclass=='I',1,0),
                        nyha2=if_else(heartmod$NYHAclass=='II',1,0),
                        nyha3=if_else(heartmod$NYHAclass=='III',1,0),
                        isMale=if_else(heartmod$gender=='M',1,0))
responseheart<-Surv(heartmod$duration,heartmod$censored==0)
coxmodel<-coxph(responseheart~ age + isMale + diameter
             + nyha1 + nyha2 + nyha3,data=heartmod)
summary(coxmodel)

sbar<-survfit(coxmodel,conf.type='none',se.fit=FALSE)
summary(sbar)
#d
heartmod2<-heartmod %>% mutate(NYHAclass=relevel(as.factor(NYHAclass),ref='IV'),
                             gender=relevel(as.factor(gender),ref='F'))
coxmodeld<-coxph(Surv(heartmod2$duration,heartmod2$censored==0)~age+gender+diameter+NYHAclass,data=heartmod2)
summary(coxmodeld)
sbarbar<-survfit(coxmodeld,conf.type='none',se.fit=FALSE)
summary(sbarbar)





