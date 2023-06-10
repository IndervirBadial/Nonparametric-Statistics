#4.1
df<-read_csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/DATA for Exercises 4.1 and 4.4.csv')%>%
  mutate(population=population/100000000,totalcrimes=totalcrimes/1000000)%>%print()
#a
plot_41a<-df %>% ggplot(aes(x=population,y=totalcrimes)) +
  geom_point(color='red') +
  labs(x='Population',y='Crimes')
plot_41a +
  ggtitle('Population vs Crimes')
#b
smoothed<-loess.as(df$population,df$totalcrimes,
                   degree=1,criterion = 'aicc')
summary(smoothed)
plot_41a+
  ggtitle('Loess')+
  stat_smooth(geom='smooth',method='loess',span=smoothed$pars$span,color='blue')
#new input
new<-loess.as(df$population,df$totalcrimes
              ,degree=1,user.span=.10784)
new_pred<-predict(new,data.frame(df$population))
new_pred[51]
#c
smoothedquad<-loess.as(df$population,df$totalcrimes,degree=2,criterion='aicc')
summary(smoothedquad)
plot_41a+
  ggtitle('Loess')+
  stat_smooth(geom='smooth',method='loess',span=.16667,color='blue'
              )

#new input
new_pred2<-predict(smoothedquad,data.frame(df$population))
new_pred2[51]

#4.2
#a
df42<-read_csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/DATA for Exercises 4.2 and 4.5.csv')
df42new = subset(df42, select = -c(...2,...3) )
datanewest<-na.omit(df42new)%>%
  mutate(month=1:36) %>% print()
plot_42a<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42a +
  stat_smooth(geom='smooth',method='loess',span=.43056,color='blue') #OSP from SAS
#b
plot_42b<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42b +
  stat_smooth(geom='smooth',method='loess',span=0.54167,color='blue') #OSP from SAS
#c
plot_42c1<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42c1 +
  ggtitle('SP=.1')+
  stat_smooth(geom='smooth',method='loess',span=.1,color='blue') #OSP from SAS
plot_42c2<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42c2 +
  ggtitle('SP=.2')+
  stat_smooth(geom='smooth',method='loess',span=.2,color='blue') #OSP from SAS
plot_42c3<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42c3 +
  ggtitle('SP=.3')+
  stat_smooth(geom='smooth',method='loess',span=.3,color='blue') #OSP from SAS
plot_42c4<- datanewest %>% ggplot(aes(x=month,y=revenue)) +
  geom_point(color='red') +
  labs(x='month',y='revenue')
plot_42c4 +
  ggtitle('SP=.4')+
  stat_smooth(geom='smooth',method='loess',span=.4,color='blue') #OSP from SAS

#4.3
#a
df43<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/DATA for Exercises 4.3 and 4.6 and 4.7.csv')
model<-loess(fruits~sodas + fries,data=df43,degree=1,span=.95)
threed<-expand.grid(list(sodas=seq(min(df43$sodas),max(df43$sodas),1),
                         fries=seq(min(df43$fries),max(df43$fries,1))))
fruitpred<-predict(model,newdata=threed)
xx<-list(title='Sodas',showbackground=TRUE,backgroundcolor='white')
xy<-list(title='Fries',showbackground=TRUE,backgroundcolor='white')
xz<-list(title='Prediction',showbackground=TRUE,backgroundcolor='white')
plot3dim<-plot_ly(z=~fruitpred)%>%add_surface(showscale=F)
plot3dim %>% layout(title='X-Y-Z plot', scene=list(xaxis=xx,yaxis=xy,zaxis=xz,aspectmode='cube'))

plot3d(df43$sodas, df43$fries, df43$fruits)
fruits.loess<-loess(df43$fruits~ df43$sodas + df43$fries, degree=1, span=0.95)
fruits.fit<-expand.grid(list(income=seq(min(sodas), max(sodas), 1), fries =seq(min(fries), max(fries), 1)))
fruits.predict<-predict(fruits.loess, newdata= fruits.fit)
persp(fruits.predict)
#persp=perspective plots
predict(fruits.loess, data.frame(sodas =14, fries =2))

#b
childeat<-data.frame(sodas=14,fries=2)
predict(model,childeat)

#4.4
library(rgl)
library(spam)
library(maps)
years<-c(1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2007, 2008, 2008, 2009, 2009, 2010, 2010)
pop<-c(179323175, 182992000, 185771000, 188483000, 191141000, 193526000, 195576000, 197457000, 199399000,  201385000, 203235298, 206212000, 208230000, 209851000, 211392000, 213124000, 214659000, 216332000, 218059000, 220099000, 225349264, 229146000, 231534000, 233981000, 236158000, 238740000, 240132887, 242282918, 245807000, 248239000, 248709873, 252177000, 255082000, 257908000, 260341000, 262755000, 265228572, 267637000, 270296000, 272690813, 281421906, 285317559, 287973924, 290690788, 293656842, 296507061, 299398484, 301621157, 301621157, 304374846, 304374846, 307006550, 307006550, 308745538, 308745538)
totl<-c(3384200, 3488000, 3752200, 4109500, 4564600, 4739400, 5223500, 5903400, 6720200, 7410900, 8098000, 8588200, 8248800, 8718100, 10253400, 11292400, 11349700, 10984500, 11209000, 12249500, 13408300, 13423800, 12974400, 12108600, 11881800, 12431400, 13211869, 13508700, 13923100, 14251400, 14475600, 14872900, 14438200, 14144800, 13989500, 13862700, 13493863, 13194571, 12475634, 11634378, 11608072, 11876669, 11878954, 11826538, 11679474, 11565499, 11401511, 11251828, 11251828, 11160543, 11160543, 10762956, 10762956, 10329135, 10329135)

population=pop/100000000
total=totl/1000000
plot(population, total)


#m=2
total <- Tps(df$population, df$totalcrimes, m=3)
population.grid<- seq(min(population), max(population), 1)
totalp<-predict(total, population)
se<-predictSE(total, population)
lines(population, totalp-1.96*se, lty=2, col= "blue")
lines(population, totalp+1.96*se, lty=2, col= "blue")
lines(population, totalp, lty=1, col= "blue")
#m=3


total3 <- Tps(df$population, df$totalcrimes, m=3)
totalp3<-predict(total3, population)
se<-predictSE(total3, population)
lines(population, totalp34-1.96*se, lty=2, col= "blue")
lines(population, totalp3+1.96*se, lty=2, col= "blue")
lines(population, totalp3, lty=1, col= "blue")

#m=4
total4 <- Tps(df$population, df$totalcrimes, m=3)
totalp4<-predict(total4, population)
se<-predictSE(total4, population)
lines(population, totalp4-1.96*se, lty=2, col= "blue")
lines(population, totalp4+1.96*se, lty=2, col= "blue")
lines(population, totalp4, lty=1, col= "blue")

#4.5
#m=2

plot(datanewest$month, datanewest$revenue,main ='m=2')

revtps <- Tps(datanewest$month, datanewest$revenue, m=2)
month.grid<- seq(min(datanewest$month), max(datanewest$month), 1)
revp<-predict(revtps, newdata=month.grid)
standarderror45<-predictSE(revtps, newdata=month.grid)
lines(datanewest$month, revp-1.96*standarderror45, lty=2,col='blue')
lines(datanewest$month, revp+1.96*standarderror45, lty=2,col='blue')
lines(datanewest$month, revp, lty=1,col='red')
#m=3
plot(datanewest$month, datanewest$revenue,main ='m=3')

revtps3 <- Tps(datanewest$month, datanewest$revenue, m=3)
revp3<-predict(revtps3, newdata=month.grid)
standarderror453<-predictSE(revtps3, newdata=month.grid)
lines(datanewest$month, revp3-1.96*standarderror453, lty=2,col='blue')
lines(datanewest$month, revp3+1.96*standarderror453, lty=2,col='blue')
lines(datanewest$month, revp3, lty=1,col='red')

#m=4
plot(datanewest$month, datanewest$revenue,main ='m=4')
revtps4 <- Tps(datanewest$month, datanewest$revenue, m=4)
revp4<-predict(revtps4, newdata=month.grid)
standarderror454<-predictSE(revtps4, newdata=month.grid)
lines(datanewest$month, revp4-1.96*standarderror454, lty=2,col='blue')
lines(datanewest$month, revp4+1.96*standarderror454, lty=2,col='blue')
lines(datanewest$month, revp4, lty=1,col='red')

#5
plot(datanewest$month, datanewest$revenue,main ='m=')
revtps5 <- Tps(datanewest$month, datanewest$revenue, m=5)
revp5<-predict(revtps5, newdata=month.grid)
standarderror455<-predictSE(revtps5, newdata=month.grid)
lines(datanewest$month, revp5-1.96*standarderror455, lty=2,col='blue')
lines(datanewest$month, revp5+1.96*standarderror455, lty=2,col='blue')
lines(datanewest$month, revp5, lty=1,col='red')

#4.6
plot3d(df43$sodas, df43$fries, df43$fruits)
fruits.tps<-Tps(cbind(df43$sodas, df43$fries), df43$fruits, m=2)
fruits.fit<-expand.grid(list(seq(min(df43$sodas), max(df43$sodas), 1), seq(min(df43$fries), max(df43$fries), 1)))
fruits.predict<-predict(fruits.tps, newdata= fruits.fit)
persp(seq(0, 1, length.out = nrow(fruits.predict)),  seq(0, 1, length.out = ncol(fruits.predict)), fruits.predict)
predict(fruits.tps, data.frame(sodas =14, grocery=10))


surface46<-Tps(x=cbind(x,y),Y=z,m=2,method='GCV.model')
surf46<-plot_ly(z=~fruitpred)
surf46<-surf46%>%add_surface(showscale=T)
surf46 %>% layout(title='TPS',scene=list(xaxis=xx,yaxis=xy,zaxis=xz,aspectmode='cube'))
                             
#4.7
#a
df47<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/DATA for Exercises 4.3 and 4.6 and 4.7.csv')
model47<-gam(fruits~sodas + s(fries),data=df47,family=gaussian)
coefficients(summary.glm(model47))
foodprediction<-predict(model47,threed)
plot47<-plot_ly(z=~foodprediction)
plot47<-plot47%>%add_surface()
plot47%>%layout(scene=list(xaxis=xx,yavis=xy,zaxis=xz))
#b
newdata47<-data.frame(sodas=14,fries=2)
predict(model47,newdata47)

#5.1
#a
df51<-read_csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.1.csv')
  df51 <- df51 %>% 
  mutate(won = case_when(
    won == 'yes' ~ 1,
    won == 'no' ~ 0
  ))

library(gam)
model51<-gam(won~lo(margin),data=df51,family=binomial)
coefficients(summary.glm(model51))
#b
newdata51<-data.frame(margin=-11)
predict(model51,newdata51,type='response')

#c
plot_51b<-df51 %>% ggplot(aes(x=margin,y=model51$fitted.values)) +
  geom_point(color='red') +
  labs(x='Margin',y='Pr')
plot_51b +
  ggtitle('Margin vs Predicted probabilities')

#d
model51d<-gam(won~s(margin),data=df51,family=binomial)
coefficients(summary.glm(model51d))

predict(model51d,newdata51,type='response')

#5.2
gender<-c('F', 'F', 'M', 'M', 'F', 'F', 'M', 'M', 'M', 'M', 'M', 'F', 'M', 'F', 'M', 'F', 'M', 'M', 'F', 'M', 'F', 'F', 'M', 'F', 'M', 'F', 'M', 'F', 'F', 'M', 'F', 'F', 'F', 'M', 'M', 'M', 'M', 'F', 'M', 'M')
intervention<-c('no ', 'no ', 'yes', 'yes', 'yes', 'yes', 'no ', 'no ', 'yes', 'yes', 'yes', 'yes', 'no ', 'yes', 'no ', 'yes', 'no ', 'no ', 'no ', 'yes', 'yes', 'yes', 'yes', 'yes', 'yes', 'yes', 'no ', 'no ', 'no ', 'yes', 'no ', 'no ', 'yes', 'no ', 'no ', 'no ', 'yes', 'no ', 'yes', 'yes')
age<-c(10, 9, 8, 11, 10, 7, 6, 6, 7, 12, 10, 8, 7, 6, 12, 7, 8, 6, 8, 10, 8, 10, 11, 8, 6, 12, 7, 12, 11, 6, 12, 11, 10, 11, 9, 10, 6, 10, 6, 8)
adhere<-c('yes', 'yes', 'yes', 'yes', 'yes', 'no ', 'no ', 'no ', 'no ', 'yes', 'yes', 'yes', 'no ', 'yes', 'no ', 'yes', 'yes', 'no ', 'no ', 'yes', 'yes', 'yes', 'yes', 'yes', 'no ', 'yes', 'no ', 'no ', 'no ', 'no ', 'no ', 'no ', 'yes', 'no ', 'no ', 'no ', 'yes', 'no ', 'no ', 'yes')
data52<-data.frame(gender,age,adhere,intervention)
data52<-data52  %>%
  mutate (adhererecode=case_when(
    adhere=='yes'~ 1,
    adhere=='no '~ 0
  ))
data52<-data52 %>%
  mutate(genderrecoded1=relevel(as.factor(data52$gender), ref = 'M'),
         interrecoded1=relevel(as.factor(data52$intervention), ref = 'yes'),)


model52<-gam(adhererecode~genderrecoded1+interrecoded1+s(age),data=data52,family=binomial)
coefficients(summary.glm(model52))
data52$response<- model52$fitted.values

#b
data52filter1<-data52 %>% filter(genderrecoded1=='M',interrecoded1=='yes')

plot_1<-data52filter1%>%  ggplot(aes(x=age,y=response)) +
  geom_point(color='red') +
  labs(x='AGE',y='Prob of Adherence')
plot_1+
  ggtitle('gender=M, Intervention=yes')

data52filter2<-data52 %>% filter(genderrecoded1=='M',interrecoded1=='no ')

plot_2<-data52filter2%>%  ggplot(aes(x=age,y=response)) +
  geom_point(color='red') +
  labs(x='AGE',y='Prob of Adherence')
plot_2+
  ggtitle('gender=M, Intervention=no')

data52filter3<-data52 %>% filter(genderrecoded1=='F',interrecoded1=='yes')

plot_3<-data52filter3%>%  ggplot(aes(x=age,y=response)) +
  geom_point(color='red') +
  labs(x='AGE',y='Prob of Adherence')
plot_3+
  ggtitle('gender=F, Intervention=yes')

data52filter4<-data52 %>% filter(genderrecoded1=='F',interrecoded1=='no ')

plot_4<-data52filter4%>%  ggplot(aes(x=age,y=response)) +
  geom_point(color='red') +
  labs(x='AGE',y='Prob of Adherence')
plot_4+
  ggtitle('gender=F, Intervention=no')

#5.3
df53<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.3.csv')
df53<-df53 %>% mutate( genderlevel= relevel(as_factor(gender), ref = 'M'), 
                     injurylevel= relevel(as_factor(injury), ref = 'yes'),
                     ptsd=case_when(score>=50 ~1,
                     score<50 ~0))%>%select (genderlevel,injurylevel,age,ptsd,deployment)

#a
detach(package:gam)
library(mgcv)
model53<-gam(ptsd~genderlevel+injurylevel+s(age,deployment,bs='ts',k=10),data=df53,family =binomial)
summary(model53)
newobs<-data.frame(genderlevel='M',injurylevel='yes',deployment=12,age=25)
predict(model53,newobs,type='response')
detach(package:mgcv)
#b
library(gam)
model53b<-gam(ptsd~genderlevel+injurylevel+s(age)+s(deployment),data=df53,family =binomial)
coefficients(summary.glm(model53b))
predict(model53b,newobs,type='response')

#5.4
#a
library(gam)
df54<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.4.csv')
df54mutate<-df54 %>% mutate(time=relevel(as_factor(time_bound),ref='morning_west'))
model54<-gam(naccidents~time+lo(brighness),data=df54mutate,family=poisson(link='log'))
coefficients(summary.glm(model54))

#b
model54b<-gam(naccidents~time+s(brighness),data=df54mutate,family=poisson)
coefficients(summary.glm(model54b))

#5.5
#a
circulation<-c(10.0, 9.0, 1.5, 15.0, 40.0, 2.5, 6.5, 8.5, 1.5, 0.5, 10.5, 25.5, 4.5, 2.0, 45.0, 10.5, 13.5, 13.0, 1.5, 2.0, 3.5, 18.5, 23.5, 4.5, 14.5, 22.0, 11.5, 44.5)
cost<-c(2.80, 2.99, 0.25, 3.85, 3.5, 0.99, 1.7, 1.75, 0.0, 0.0, 5.45, 3.45, 1.5, 1.0, 4.50, 0.99, 3.45, 2.99, 0.45, 1.60, 0.75, 3.0, 2.0, 3.8, 3.6, 2.5, 1.65, 2.45)
ntypos<-c(2, 2, 7, 1, 1, 5, 2, 4, 7, 8, 0, 0, 3, 6, 6, 4, 0, 2, 7, 6, 8, 2, 3, 3, 2, 1, 2, 5)
df55<-data.frame(circulation,cost,ntypos)
model55<-gam(ntypos~s(circulation)+s(cost),data=df55,family=poisson)
coefficients(summary.glm(model55))

#b
newobs55<-data.frame(circulation=13.5,cost=1.75)
predict(model55,newobs55,type='response')

#c
threed55<- expand.grid(circulation=seq(min(df55$circulation),max(df55$circulation),0.1), cost=seq(min(df55$cost),
                                                                                                  max(df55$cost),0.1))
predicthreed<-predict(model55,threed55,type='response')
plt55<-plot_ly(z=~predicthreed)%>% add_surface()
plt55%>%layout(title='Cost Circulation')

#d
detach(package:gam)
library(mgcv)
model55d<-gam(ntypos~s(circulation,cost,bs='tp',k=28),data=df55,family=poisson)
summary(model55d)

predict(model55d,newobs55,type='response')

predict55d<-predict(model55d,threed55,type='response')
length(predict55d)
mat<-matrix(predict55d,nrow = 446, ncol = 55)
plot55d<-plot_ly(z=~mat)%>%add_surface
plot55d%>%layout(title="Bivariate")









