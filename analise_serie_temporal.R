########################################################################################
#                     Análise do mercado de Tabacarias e HeadShop                      #
#                     e previsão do mercado                                            #
########################################################################################

#Caso a biblioteca não esteja instalada na maquina de voces, utilizar o comando: install.packages()
install.packages("dplyr")
install.packages("forecast")
###################### Carrega Bibliotecas ###############################################
library(dplyr)     #Permite manipular dados com instrucoes semelhantes ao SQL
library(forecast)  #Diversas ferramentas para trabalhar com Séries Temporais
##########################################################################################

#Cria o vetor com a váriavel que será explorada, nesse caso a quantidade de tabacarias abertas 
#mês a mês desde janeiro de 2015 até novembro de 2020
qtde_tabacarias <- c(87,81,150,112,135,111,135,152,140,123,133,91,147,150,194,163,206,204,
                     186,199,200,195,213,161,248,226,304,239,299,295,322,327,270,313,276,223,
                     323,282,352,368,400,361,357,401,326,344,303,252,368,374,359,400,397,357,
                     452,407,370,398,326,234,360,321,286,275,450,515,695,638,640,615,395)

#Visualiza o vetor criado
View(qtde_tabacarias)

#Informa o R que o vetor é uma séria temporal 
Tabacarias <- ts(qtde_tabacarias,
                       frequency = 12,
                       start = c(2015,1))


#Visualiza série temporal e autocorrelações
ggtsdisplay(Tabacarias)

#Visualiza estatísticas descritivas
summary(Tabacarias)

boxplot(Tabacarias)

hist(Tabacarias)

#Mes vs ano
seasonplot(Tabacarias,12,
           col=rainbow(12),
           year.labels=TRUE,
           type = "o",
           pch = 16);grid()

#Ajusta modelo linear por Mínimos Quadrados 
modelo <- tslm(formula = Tabacarias ~
                 trend +
                 season) 

summary(modelo)

plot(Tabacarias,type = 'b',col = 'black',pch = 16)

lines(modelo$fitted.values,
      col = "blue",
      type ="b",
      pch =16)
legend("topleft", c("Aberturas","Previsoes"),
       col=c("black",'blue'), lty=c(1,1),bty="n", cex=0.6, lwd=2)

accuracy(Tabacarias, modelo$fitted.values)

predicoes <- forecast(modelo, h = 24)
plot(predicoes)

predicoes
