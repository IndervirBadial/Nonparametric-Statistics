#7.1
#a
df<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.1.csv')
kernel.default<-density(df$times)
plot(kernel.default)


df %>% ggplot(aes(x=times))+
  theme_classic()+
  ggtitle('KDE Histogram')+
  stat_bin(aes(y=..density..),bins=5,center=mean(df$times),closed='left',
color='black',fill='red')+
  stat_density(kernel='gaussian',bw=bw.nrd0(df$times),
               color='blue',fill='green',alpha=.3)
#b
unstandard=function(x,p){
  n=length(x)
  bw=p*IQR(x)*(n^(-1/5))
  return(bw)
}
plots=function(data,x,breaks,bw){
  df %>% ggplot(aes(x=x))+
    ggtitle('KDE Histogram')+
    stat_bin(aes(y=..density..),breaks=breaks,color='black',fill='white')+
    stat_density(kernel='gaussian',bw=unstandard(x,bw[[1]]),
                 color='blue',alpha=.3)+
  stat_density(kernel='epanechnikov',bw=unstandard(x,bw[[2]]),
               color='red',alpha=.3)+
  stat_density(kernel='triangular',bw=unstandard(x,bw[[3]]),
               color='yellow',alpha=.3)
}
breaks=seq(47,103,5)
plot1=plots(df,df$times,breaks,bw =c(.4,.4,.4))
plot2=plots(df,df$times,breaks,bw=c(.7,.7,.7))
plot3=plots(df,df$times,breaks,bw=c(.9,.9,.9))
plot1
plot2
plot3


#7.2
#a
df<-read_csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.2.csv')
diff=df$husband-df$wife
df %>% ggplot(aes(x=diff))+
  theme_classic()+
  ggtitle('KDE Histogram')+
  stat_bin(aes(y=..density..),bins=6,center=8,closed='left',
           color='black',fill='red')+
  stat_density(kernel='gaussian',bw=bw.nrd0(diff),
               color='blue',fill='green',alpha=.3)

#b
breaks=seq(-6,18,2)
plot1=plot1=plots(df,diff,breaks,bw =c(.4,.7,.9))
plot2=plots(df,diff,breaks,bw=c(.7,1,1.1))
plot3=plots(df,diff,breaks,bw=c(.8,.9,1.1))
plot1
plot2
plot3

#7.3
#a
df<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.3.csv')
df %>% ggplot(aes(x=length))+
  theme_classic()+
  ggtitle('KDE Histogram')+
  stat_bin(aes(y=..density..),bins=8,center=4.5,closed='left',
           color='black',fill='red')+
  stat_density(kernel='gaussian',bw=bw.nrd0(df$length),
               color='blue',fill='green',alpha=.3)
#b
breaks=seq(1,8,.5)
plot1=plot1=plots(df,df$length,breaks,bw =c(.4,.6,.7))
plot2=plots(df,df$length,breaks,bw=c(.3,.4,.6))
plot3=plots(df,df$length,breaks,bw=c(.4,.5,.6))
plot1
plot2
plot3

#8.1
install.packages("bootstrap")
library(bootstrap)
jack.results<-jackknife(df$times, mean)

lcl<- mean(df$times)-jack.results$jack.bias+qt(0.025,length(df$times)-1)*jack.results$jack.se
ucl<- mean(df$times)-jack.results$jack.bias - qt(0.025,length(df$times)-1)*jack.results$jack.se
lcl
ucl
#8.2
husb<-df$husband
wif<-df$wife
data<-matrix(cbind(husb,wif),ncol=2)
theta<-function(x,xdata){cor(df[x,1], df[x,2], method="pearson")}
jack.results<-jackknife(1:60, theta, df)
lcl<- cor(husb,wif, method="pearson") -jack.results$jack.bias+qt(0.005,length(husb)-1)* jack.results$jack.se
ucl<- cor(husb,wif, method="pearson")-jack.results$jack.bias-qt(0.005,length(husb)-1)*jack.results$jack.se
lcl
ucl

#8.3
df<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.1.csv')
gas<-df$pgas
milk<-df$pmilk
data<-matrix(cbind(gas,milk),ncol=2)
theta<-function(x,xdata){cor(df[x,1], df[x,2], method="spearman")}
jack.results<-jackknife(1:12, theta, df)
lcl<- cor(gas,milk, method="spearman") -jack.results$jack.bias+qt(0.025,length(gas)-1)* jack.results$jack.se
ucl<- cor(gas,milk, method="spearman")-jack.results$jack.bias-qt(0.025,length(gas)-1)*jack.results$jack.se
lcl
ucl

#8.4

jack.results<-jackknife(df$length, var)

lcl<- var(df$length)-jack.results$jack.bias+qt(0.05,length(df$length)-1)*jack.results$jack.se
ucl<- var(df$length)-jack.results$jack.bias-qt(0.05,length(df$length)-1)*jack.results$jack.se
lcl
ucl

#8.5
set.seed(178387)
boot.results<-bootstrap(df$times,1000, mean)
quantile(boot.results$thetastar, c(.025, .975))

#8.6
husb<-df$husband
wif<-df$wife
data<-matrix(cbind(husb,wif),ncol=2)
theta<-function(x,xdata){cor(data[x,1], data[x,2], method="pearson")}

set.seed(239479734)
boot.results<-bootstrap(1:60, 1000, theta, data)
quantile(boot.results$thetastar, c(.005, .995))

#8.7
df<-read.csv('/Users/indobadial/Downloads/STUDY_MATERIALS/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.1.csv')
gas<-df$pgas
milk<-df$pmilk
data<-matrix(cbind(gas,milk),ncol=2)
theta<-function(x,xdata){cor(df[x,1], df[x,2], method="spearman")}
set.seed(239479734)
boot.results<-bootstrap(1:12, 1000, theta, data)
quantile(boot.results$thetastar, c(.025, .975))

#8.8
set.seed(7297346)
boot.results<-bootstrap(df$length,1000, var)
quantile(boot.results$thetastar, c(.05, .95))
