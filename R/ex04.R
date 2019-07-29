## exercícios aula 04
#pcts necessários
library(vegan)

#exercício 2
poa<-read.csv("./Data/exercicio_02.csv", sep =  ",")
str(poa)
head(poa)
poa2<-poa[poa$var=="Quebec",] #indexei pela coluna de variedade para coiaas só iguais (==) a quebec
poa3<-poa2[poa2$temp=="chilled",] #indexei só aos chilled

#OU poa3<-poa[poa$temp=="chilled" & poa$var=="quebec",]
#OU poa3<- subset(poa$uptake, subset poa$var="chilled" & poa$var=="quebec",)
md<-mean(poa3$uptake)
md

####################
#exercício 3

data(BCI)
dim(BCI)
colSums(t(BCI)) #soma das colunas o dataframe transposto - mt prático cabritinho

#OU
aplly(BCI, 1, sum) #aplicar a soma das linhas (coluna 1) do df BCI

####################


#exercício 4 - estudar mais esse read delim - confuso

dados<-read.delim("./Data/0012594-190621201848488.csv")
gbif4<-dados[,c(10,17,18)]
?read.delim
head(gbif4)


