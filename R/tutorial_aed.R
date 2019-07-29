###tutorial aed
#pacotes necessários
library(GGally)

#carregar os dados que já existem no R:
data(anscombe)
data(iris)

#normalmente faria assim
#mean(anscombe$x1)
#mean(anscombe$x2)
#mean(anscombe$x3)
#mean(anscombe$x4)

#mesma função pode ser feita em apenas uma linha:
## media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

#calcular a variância dos dados:
apply(anscombe, 2, var)

#correlação dos conjuntos x e y:
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

# coeficiente de regressão:
#objetos com as regressoes dos quatro conjuntos e depois puxar uma lista
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
mlist <- list(m1, m2, m3, m4)

# agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2,2), #abre uma janela gráfica com 2 linhas e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L
plot(anscombe$y1 ~ anscombe$x1) #plotar as avriáveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1,1))


###parte 2
head(iris)
summary(iris)

#infomorções por espécies:
table(iris$Species)

#media do comprimento de sepala por especie:
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)

# a mesma tarefa, executada por outra função. Outros argumentos e outra saída:
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

# ainda a mesma tarefa, com a mesma função mas em uma notação diferente:
aggregate(Sepal.Length ~ Species, data=iris, mean)

#o mesmo para outras variáveis:
aggregate(Sepal.Length ~ Species, data=iris, mean)
aggregate(Sepal.Width ~ Species, data=iris, mean)
aggregate(Petal.Length ~ Species, data=iris, mean)

#calcular o desvio-padrão das variáveis:
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol=3, nrow=4)

# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]

#fazendo um loop - treinar isso
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}


#calcular a média:
vars <- iris[,-5]
apply(vars, 2, mean)

#cacular a mediana:
apply(vars, 2, median)

#moda:
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

#calcular a variância:desvio da média
apply(vars, 2, var)

#calcular o desvio-padrão:
sd01 <- apply(vars, 2, sd)

# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01==sd02

#coeficiência de variação:
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

#calcular os quantis:
# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

#intervalo retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado

my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

#intervalo interquartil:
apply(vars, 2, IQR)

#calcular a correlação:
cor(vars)

## parte 3 - gráficos

#gráfico de barras:
barplot(table(iris$Species))

#histograma:
par(mfrow=c(2,2))
hist(iris$Sepal.Length,main="", xlab="Sepal.Lenght")
hist(iris$Sepal.Width,main="", xlab="Sepal.Width")
hist(iris$Petal.Length,main="", xlab="Petal.Lenght")
hist(iris$Petal.Width,main="", xlab="Petal.Width")

par(mfrow=c(1,1))

#ver o efeito do número de intervalos no histograma com o argumento breaks
par(mfrow=c(1,2))
hist(iris$Sepal.Width,main="", xlab="Sepal.Width")
hist(iris$Sepal.Width,main="", xlab="Sepal.Width", breaks = 4)
par(mfrow=c(1,1))

#curva de densidade:
# ploar os gráficos
par(mfrow=c(1,2))
hist(iris$Sepal.Width,main="", xlab="Sepal.Width")
hist(iris$Sepal.Width, main="", xlab="Sepal.Width", freq = FALSE)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE, main="")
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1,1))


#boxplot das variáveis do objeto iris:
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#valores por espécie:
boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Width ~ Species, data=iris)

#identificando os outliers:
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

boxplot(Sepal.Width ~ Species, data=iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species=="setosa",
     c("Sepal.Width", "Species")]
## PARTE O4 ________________________________________________________________________
#Entendendo a distribuição dos dados

par(mfrow=c(1,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"],
       main="setosa")
qqline(iris$Sepal.Length[iris$Species=="setosa"])
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"],
       main="versicolor")
qqline(iris$Sepal.Length[iris$Species=="versicolor"])
qqnorm(iris$Sepal.Length[iris$Species=="virginica"],
       main="virginica")
qqline(iris$Sepal.Length[iris$Species=="virginica"])

par(mfrow=c(1,1))

#relação entre variáveis:
pairs(vars)
# usando o pct ggpais
ggpairs(vars)
