###aula análise exploratória de dados
#baixando pacotes necessários
library(png)

#lendo a tabela direto do doc raw do github - legal

sal<-read.csv("https://raw.githubusercontent.com/AndreaSanchezTapia/analise_de_dados_ENBT_2019/master/aula07/data/salarios.csv")

#fazendo os coefs
head(sal)
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)
coefm <- coef(mm)

# definindo os limites dos eixos
limy <- c(min(sal$salario),max(sal$salario))
limx <- c(min(sal$experiencia),max(sal$experiencia))


#definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"

#plotando a 2
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="darkgreen",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# adicionando a linha do previsto pelo modelo (a+b*x)
abline(a=coefh[1], b=coefh[2],
       col='pink', lwd=2)
mtext("A", 3, adj=0, font=2)


## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo (a + b*x)
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=3)

#mudei as cores só pra testar

############################

png("figs/fig01.png", res=300, width=2400, height=1200) #definindo um dispositivo gráfico numa pasta

par(mfrow=c(1,2), las=1, bty="l") #setando o layout de leitura do graph
plot(salario ~ experiencia, data=sal[sal$sexo=="H",], #plotando
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)

abline(a=coefh[1], b=coefh[2], #plotando argumentos secundários (abline, mtext e tal)
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)

plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)

abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
dev.off() #gerar o arquivo na pasta

plot(salario ~ experiencia, data=sal[sal$sexo=="H",], #plotando
     col="orange",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
points(salario ~ experiencia, data=sal[sal$sexo=="M",],
       col="navy")
legend("topleft", legend=c("homens", "mulheres"),  #usando legendas novas
       col=c("tomato", "navy"),
       lty=1, bty='n')
dev.off() #fechando a saída lá com a entrada do png

