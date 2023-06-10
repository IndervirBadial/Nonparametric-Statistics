install.packages("BSDA")	
library(BSDA)
#1
x<-c(22.125, 23.500, 23.500, 25.875, 26.375, 21.375, 28.875, 24.625, 23.125, 25.000)
y<-c(23.375, 23.125, 24.750, 25.750, 26.750, 22.625, 29.000, 24.000, 24.125, 27.250)
SIGN.test(x,y, alternative="l")
#2
x2<-c(36,22,10,12,28,12,23,6)
y2<-c(17,15,-8,-11,14,20,24,6)
SIGN.test(x2,y2, alternative="g")
#3
x3<-c(405.65, 400.51, 408.25, 401.34, 409.09)
y3<-c(403.02, 399.49, 396.10, 403.59, 405.68)
SIGN.test(x3,y3, alternative="t")
 
#4
install.packages("exactRankTests")
library(exactRankTests)
wilcox.exact(x,y, paired=TRUE, alternative="l")
#5
wilcox.exact(x2,y2, paired=TRUE, alternative="g")
#6
wilcox.exact(x3,y3, paired=TRUE, alternative="t")
#7
x7=c(5, 4, 6, 4, 3, 4, 4, 3, 5, 5)
y7=c(7, 8, 12, 10, 8, 9, 10)
wilcox.exact(x7,y7, paired=FALSE, alternative="l")
#8
x8=c(15, 8, 8, 10, 6, 9, 7, 8)
y8=c(13, 17, 10, 12, 13, 17, 15, 17, 17, 19)
wilcox.exact(x8,y8, paired=FALSE, alternative="l")
#9
x9=c(10, 7, 11, 8, 5, 12, 13)
y9=c(7, 6, 8, 5, 3, 6, 7, 6, 3, 2)
wilcox.exact(x9,y9, paired=FALSE, alternative="t")
#10
#a
x10<-c(1,4,14,7,11,1,8,10)
y10<-c(1, 3, 5, 5, 4, 3, 4, 5)
wilcox.exact(x10,y10, paired=FALSE, alternative="t")
#b
ansari.test(x10,y10, alternative="t")
#c
ansari.test(x10,y10, alternative="g")
#11
x11=c(27, 37, 40, 63, 31, 81, 63, 57, 90, 94)
y11=c(56, 78, 60, 55, 67, 68, 64)
wilcox.exact(x11,y11, paired=FALSE, alternative="t")
ansari.test(x11,y11, alternative="g")