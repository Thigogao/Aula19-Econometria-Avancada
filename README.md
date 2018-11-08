# Aula19-Econometria-Avancada
Previsao e Função Impulso em VAR
                    #Aula 19 -PrevisÃ£o e Impulso no VAR(p)

pacotes <- c("forecast","dplyr",
             "vars","urca","pwt8")

install.packages(pacotes)


suppressMessages(require(forecast))
suppressMessages(require(dplyr))
suppressMessages(require(vars))
suppressMessages(require(urca))
suppressMessages(require(pwt8))

data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoinÃ­veis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0

br1 <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr","ctfp","hc"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (paÃ­s) igual a "Brazil", selecionando as colunas cujas variÃ¡veis sÃ£o "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÃMBIO)

#Transformando em VariaÃ§Ã£o

br <- data.frame()

    for (i in 1:62) {
      for (j in 1:5) {
        br[i,j] <- br1[i+1,j]/br1[i,j]
      }
    }
br <- br[1:61,]
colnames(br) <-  c("PIB","Emprego","Cambio", "PTF","KHumano")   #Renomeia as colunas para PIB, Trabalho e CÃ¢mbio
BR <- br[45:61,1:5]


#Separando as variÃ¡veis
                    #Cria o vetor para variÃ¡vel PIB 
PIb <- ts(br$PIB, start = 1950, frequency = 1)
Emprego <- ts(br$Emprego, start = 1950, frequency = 1)
Cambio <- ts(br$CÃ¢mbio, start = 1950, frequency = 1)
PTF <- ts(br$PTF, start = 1950, frequency = 1)
KHumano <- ts(br$KHumano, start = 1950, frequency = 1)

Brasil <- cbind(BR$PIB,BR$Emprego,BR$Cambio,BR$PTF,BR$KHumano)
Anos <- seq(from=1994, to=2011, by=1)         #Cria um vetor para o tempo em anos de 1994 atÃ© 2011
BRA <- ts(Brasil, start = 1994, frequency = 1)
plot(BRA,main="VariaÃ§Ã£o do PIB, Emprego, Cambio, PTF, Capital Humano no Brasil",type="o", 
     col=c("Blue","Black","Red","Green","Purple"), plot.type="single")
grid(lty = "dotted",col = "lightgray")

#CorrelaÃ§Ã£o 

correlacao <- cor(BR)
View(correlacao)

plot(BR$PIB,BR$Emprego)
plot(BR$PIB,BR$Cambio)
plot(BR$PIB,BR$PTF)
plot(BR$PIB,BR$KHumano)



#Estimando um Var

modelobra = vars::VAR(y = BR, p = 1, type = "const")
summary(modelobra)


#FunÃ§Ã£o Impulso

impulso <- irf(modelobra, n.ahead = 10, cumulative = T)
plot(impulso)

#Modelo de PrevisÃ£o

previsao2 <- predict(modelobra,10)

fanchart(previsao2)
