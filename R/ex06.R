###exercício 06###
#lendo, sumário e calculando sumário e variância
arv<-read.csv("./data/trees.csv", sep =  ",")
summary(arv)
var(arv$Girth)
var(arv$Height)
var(arv$Volume)
sd(arv$Girth)
sd(arv$Height)
sd(arv$Volume)


#gráficos unidos em 3 dos boxplots
par(mfrow=c(1,3))
boxplot(arv$Girth)
boxplot(arv$Height)
boxplot(arv$Volume)
par(mfrow=c(1,1))


#gráficos unidos em 3 dos histogramas
par(mfrow=c(1,3))
hist(arv$Girth)
hist(arv$Height)
hist(arv$Volume)
par(mfrow=c(1,1))

#conferindo os outliers
bp <- boxplot(arv$Girth, plot=FALSE)
bp
outliers <- bp$out
which(arv$Girth %in% outliers) #sem outliers

bp2 <- boxplot(arv$Height, plot=FALSE)
bp2
outliers2 <- bp2$out
which(arv$Height %in% outliers2) #sem tbm

bp3 <- boxplot(arv$Volume, plot=FALSE)
bp3
outliers3 <- bp3$out
which(arv$Volume %in% outliers3) #sem tbm

#unico valor outlier é em volume

cor(arv) #correlação par a par
