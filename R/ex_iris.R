#baixando o dataset que vamos usar
data(iris)

# criando vetor com o nome das espécies
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")

sp <- paste("I.", unique(iris$Species), sep=" ")
par(mfrow=c(2,2), bty='l', las=1)
boxplot(Sepal.Length ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Sepal.Width ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Length ~ Species, data=iris,  col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Width ~ Species, data=iris, col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
par(mfrow=c(1,1))

set.seed(42) #criando o mesmo grupo aleatório

d2 <- data.frame(name=letters[1:5], #criando o df relacionado ao seedset42
                 value=sample(seq(4,15),5),
                 sd=c(1,0.2,3,2,4))
plot(x=1:5, d2$value, las=1, bty='l', ylim=c(0, 18), pch=19, xaxt='n', #plotando
     xlab="names", ylab="value")
axis(1, at=1:5, labels=d2$name)
arrows(x0=1:5,
       y0=d2$value+d2$sd,
       y1=d2$value-d2$sd, angle=90, length=0.05, code=3)

###########################

##exercício

#Definindo o dispositivo gráfico numa pasta
png("figs/resultadoiris.png", res=300, width=2400, height=1200)

#Fazendo os coeficientes de intercepto
seto <- lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="setosa",])
virg <- lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="virginica",])
vers <- lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="versicolor",])
coefseto <- coef(seto)
coefvirg <- coef(virg)
coefvers <- coef(vers)

#Plotando os gráficos
#Primeiro: colocando todos na mesma coluna e direcionando os gráficos
par(mfrow=c(1,3), bty='l', las=1,
    mar=c(7, 4.5, 2, 1))
#Indexando os valores para apenas uma espécies (I. virginica)
plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="virginica",],
     #Selecionando cor
     col="orange",
     #Definindo limites e rótulos
     ylab="Comprimento de Sépala", xlab="I. virginica")
#Adicionando os coeficientes
abline(a=coefvirg[1], b=coefvirg[2],
       col='orange', lwd=3)
mtext("A", 3, adj=0, font=2)

#Repetindo o processo de plotagem
plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="setosa",],
     col="tomato",
     #Nos próximos plots, serão escondidos os rótulos do eixo Y
     xlab="I. setosa", yaxt='n', ylab = "")
abline(a=coefseto[1], b=coefseto[2],
       col='tomato', lwd=3)
mtext("B", 3, adj=0, font=2)
#Adicionando a legenda compartilhada no eixo X
mtext("Largura de pétala",side=1,outer=TRUE,padj=-1.5)

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="versicolor",],
     col="navy",
     xlab="I. versicolor", yaxt='n', ylab = "")
abline(a=coefseto[1], b=coefseto[2],
       col='navy', lwd=3)
mtext("C", 3, adj=0, font=2)

par(mfrow=c(1,1))
dev.off()


