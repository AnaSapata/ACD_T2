# Análise Categórica e Dados
# MMEAD - 2017/2018 - 2º semestre
# Modelos Lineares Generalizados v1.0
# Ana Sapata n.º39504 
# 21/06/2018


dados<-read.table("C:\\Users\\rebis1\\Desktop\\Universidade\\Categorica\\Nova pasta\\loja.txt", header=T)
attach(dados)
View(dados)
str(dados)

# -------------------------- Modelo Normal --------------------------------
#histograma para a variavel cliente
hist(dados$Client)
library(fBasics)
basicStats(Client)

#aplicação do modelo aos dados
fit1<-glm(Client ~ habit + rendim + idade + dist_conc + 
            dist_loja, family="gaussian", data=dados)
summary(fit1)

#Verificação dos pressupostos
# 1.Homogeneidade de variâncias
plot(fitted(fit1),rstudent(fit1))
plot(fitted(fit1),rstandard(fit1))
library(lmtest)
bptest(fit1, studentize=F) #p-value=0.01245<0.05 rejeita-se H0
library (car)
ncvTest (lm(Client ~ habit + rendim + idade + dist_conc + 
              dist_loja)) #p-value=0.0005<0.05 rejeita-se H0
#O pressuposto não se verifica

# 2.Normalidade
rs<-rstandard(fit1)
qqnorm(rs)
qqline (rs, col=2)
hist(rs, ylab=, xlab=, col=rainbow(60))
boxplot(rs)
library (nortest)
lillie.test(rs) #p-value=0.4803>0.05, não se rejeita H0
library (tseries)
jarque.bera.test (rs) #p-value=0.2536
a<-ad.test (rs)
a #p-value=0.2721
#Verifica-se o pressuposto

# 3.Multicolinaridade
library(corpcor)
mcor<-cor(data.frame(habit, rendim, idade, dist_conc, dist_loja))
mcor #existem algumas correlações significativas entre as variaveis
#Pelo que existe multicolinearidade entre as variaveis independentes

# 4.Independência
durbinWatsonTest(fit1) #p-value=0.158>0.05 não se rejeita H0
# Não se verifica a independencia


# -------------------------- Modelo Poisson --------------------------------

mod1 <- glm(Client ~ habit + rendim + idade + dist_conc + 
              dist_loja, family=poisson,dados) 
summary(mod1)

exp(mod1$coefficients)

#Simplificação do modelo saturado
mod1a<-step(mod1)
summary (mod1a)
#dá o mesmo modelo

# Adequabilidade da função de ligação
mu <- 2*sqrt(predict (mod1, type="response")) #2*sqrt(mu) p.79 livro MLG prof
z <- predict (mod1)+(Client-mu)/mu
plot(z ~ predict(mod1,type="link"), xlab=expression(hat(eta)), dados,ylab="Resposta linearizada")
#embora exista alguma dispersão os dados seguem +/- ums linha pelo que é adequada

# Avaliação da Função de Variância
ra<-resid(mod1,type="response")
tr<-2*sqrt(predict(mod1,type="response"))
plot(ra ~ tr, xlab=expression(2*sqrt(hat(mu))),ylab="Resíduos Absolutos")
plot(residuals (mod1) ~ predict(mod1,type="link"), xlab=expression(hat(u)),ylab="Resíduos Absolutos")
lines (lowess (predict(mod1,type="link"), residuals (mod1)), col="red")

# Resíduos deviance
rd<-resid(mod1,type="deviance")
tr<-2*sqrt(predict(mod1,type="response"))
plot(rd ~ tr, xlab=expression(2*sqrt(hat(mu))),ylab="Resíduos Deviance")
abline(h=0, col=2)
plot(rd ~ predict(mod1,type="link"), xlab=expression(hat(eta)),ylab="Resíduos Deviance")
identify(predict(mod1,type="link"), rd)
lines (lowess (predict(mod1,type="link"), rd), col="red")

#Influência
plot(Client,cooks.distance(mod1),xlab="Número de nascimentos",ylab="Distância de Cook")
identify(Client,cooks.distance(mod1))
library(faraway)
halfnorm(cooks.distance(mod1))

#Leverage
library(car)
dadosi <- influence(mod1)
halfnorm(dadosi$hat)
h<-hatvalues(mod1)
dim(dados)
traco<-sum(diag(hatvalues(mod1)))
traco
plot (round(y,0), h*70/10)
abline(h=2,col=2)
identify(round(y,0), h*70/10)

#Outlier
plot (rstudent(mod1))
identify(rstudent(mod1))
plot(rd)
identify(rd)
halfnorm(rstudent(mod1))

Client ~ habit + rendim + idade + dist_conc + 
  dist_loja
# Estimar o número de nascimentos para uma mulher com um casamento de 5 anos, a viver na cidade e a tirar um mestrado 
x0 <- data.frame(habit=500,rendim=38000,idade=45, dist_conc=5, dist_loja=7) 
x0
predict(mod1,new=x0,se=T,type="response")
pred<-predict(mod1,new=x0,se=T,type="response")
c(pred$fit-qnorm(0.975)*pred$se.fit,pred$fit+qnorm(0.975)*pred$se.fit)

# Estimar o número de nascimentos para uma mulher com um casamento de 10 anos, a viver na cidade e a tirar um mestrado
x0 <- data.frame(dur="10-14",educn="sec",res="urban", n=1) 
x0
predict(mod1,new=x0,se=T,type="response")
pred<-predict(mod1,new=x0,se=T,type="response")
c(pred$fit-qnorm(0.975)*pred$se.fit,pred$fit+qnorm(0.975)*pred$se.fit)


# -------------------------- Modelo Gamma --------------------------------

AG <- c(rep(1,17), rep(0,16)); AG
WBC <- c(2300, 750, 4300, 2600, 6000, 10500, 10000, 17000, 5400, 7000, 9400,
         32000, 35000, 100000, 10000, 52000, 100000, 4400, 3000, 4000, 1500,
         9000, 5300, 10000, 19000, 27000, 28000, 31000,26000, 21000, 79000,
         100000, 100000)
Temp <- c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5,
          65, 56, 65, 17, 7, 16, 22, 3, 4, 2, 3, 8, 4, 3, 30, 4, 43)
tab <- data.frame(AG, WBC, Temp)
View(tab)
tab$logWBC <- log(tab$WBC)
tab$AG <- as.factor(tab$AG)
str(tab)

par(mfrow=c(1,2))
hist(tab$Temp, col=rainbow(50))
boxplot (tab$Temp, col="red")
library(fBasics)
basicStats (tab$Temp)

mod1 <- glm(tab$Temp ~ tab$AG + tab$logWBC, family=Gamma(link=log), tab, maxit=100) 
summary(mod1)
exp(mod1$coefficients)
# Variação explicada
1-(41.608/30)/(58.138/32) #0.2366 / 23.66%


#Verificação dos pressupostos
# 1.Homogeneidade de variâncias
plot(fitted(mod1),rstudent(mod1))
plot(fitted(mod1),rstandard(mod1))
library(lmtest)
bptest(mod1, studentize=F) #p-value=0.08025>0.05 não se rejeita H0
library (car)
ncvTest (lm(tab$Temp ~ tab$AG + tab$logWBC)) #p-value=0.04<0.05 rejeita-se H0
#O pressuposto não se verifica

# 2.Normalidade
r <- residuals(mod1)
rs<-rstandard(mod1)
qqnorm(rs)
qqline (rs, col=2)
hist(rs, ylab=, xlab=, col=rainbow(60))
boxplot(rs)
library (nortest)
lillie.test(rs) #p-value=0.3912>0.05, não se rejeita H0
library (tseries)
jarque.bera.test (rs) #p-value=0.601
a<-ad.test (rs)
a #p-value=0.3856
#Verifica-se o pressuposto

# 3.Multicolinaridade
library(corpcor)
mcor<-cor(data.frame(as.numeric(tab$AG), tab$logWBC))
mcor #não existe multicolinearidade

# 4.Independência
durbinWatsonTest(mod1) #p-value=0<.05 rejeita-se H0
# Verifica-se a independencia






# Adequabilidade da função de ligação
mu <- 2*log(predict (mod1, type="response"))
z <- predict (mod1)+(tab$Temp-mu)/mu
par(mfrow=c(1,1))
plot(z ~ predict(mod1,type="link"), xlab=expression(hat(eta)), tab,ylab="Resposta linearizada")
#função de ligação não é a mais adequada

# Avaliação da Função de Variância
ra<-resid(mod1,type="response")
tr<-2*log(predict(mod1,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Resíduos Absolutos")
lines (lowess (ra~tr), col="red")

rd<-resid(mod1,type="deviance")
plot(rd ~ tr, xlab=expression(2*log(hat(mu))),ylab="Resíduos Deviance")
plot(rd ~ predict(mod1,type="link"), xlab=expression(hat(eta)),ylab="Resíduos Deviance")
lines (lowess (predict(mod1,type="link"), rd), col="red")

#Influência
plot(tab$Temp,cooks.distance(mod1),xlab="tempo",ylab="Distância de Cook")
identify(tab$Temp,cooks.distance(mod1))
library(faraway)
halfnorm(cooks.distance(mod1))

#Leverage
library(car)
tabi <- influence(mod1)
halfnorm(tabi$hat)
h<- hatvalues(mod1)
plot (tab$Temp, h*156/32)
abline(h=1)
identify(tab$Temp, h*156/32)

#Outliers
plot (rstudent(mod1))
identify(rstudent(mod1))
plot(rd)
identify(rd)
halfnorm(rstudent(mod1))
