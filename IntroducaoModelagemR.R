# definindo o projeto de curso
# pergunta: o que afeta a qualidade do ar? e como?
# install.packages("Ecdat")

library(Ecdat) #carregando
data(Airq)     #carregando o banco de dados do pagote
names(Airq)    #exibe os nomes das variaveis


#Descrevendo as variaveis

#airq: indice de qualidade do ar(quanto menor melhor)
#vala: valor das empresas nas cidades (Em milhares de dolares)
#rain: quantidade de chuvas em polegadas
#coas: posição costeira da cidade (sim ou nao)
#dens: densidade populacional (milhas quadrada)
#medi: renda media per capital(em dolares)

#analise descritiva ou exploratória

summary(Airq) #sumário das variaveis 

#as variaveis podem ser continuas ou categoricas (divididas em categorias)
#a variavel reposta é a qualidde do ar (airq)

plot(airq~vala,data = Airq)

# criando um modelo estatistico
# y ~ x  (~ em função de ) y em função de x

# y eh a variavel resposta x é a variavel explicativa 
# y(resposta) ~ x(explicativa)
# podemos colocar mais variaveos em nosso modelo  y - x1 + x2 + x3 
# airq ~ vala + coas + rain

#-----------------------------------------------
#termos: anova (variavel continua ~ de uma variavel categorica)
#regressão(variavel continua ~ variavel continua)
#regressão multipla (variavel continua ~ variaveis continuas ou nao)
#-----------------------------------------------

#montando o modelo
# regressão linear é quando temos 2 variaveis continuas no seu modelo

# lm (modelo linear) 
# atribuindo um valor para o modelo (m1 <-)
# mostrando que x ~ y ( airq ~ vala )
# data = Airq pegando nosso banco de dados 

m1<-lm(airq~vala,data = Airq) 
summary(m1) #para saber a significancia do modelo
plot(airq~vala,data = Airq) #plot de regressão linear

#p-valor indica a significancia do modelo ou da variável
#(existe um efeito daquela variavel explicativa (X) para variavel resposta (Y) )
#se p-valor for menor (<)  0.05 a variavel é significativa 
#se p-valor for maior que  0.05 não existe o efeito esperado
#a variavel "vala" não influenciou a qualidade do ar nas cidades("airq")
#a variavel coas afeta a variavel airq 

m2<-lm(airq~coas,data = Airq)
summary(m2)

# sim a posição costeira da cidade influencia a qualidade do ar das cidades
# as cidads costeiras apresentão uma melhor qualidade do ar

plot(airq~coas,data = Airq,col = 'lightblue')

#lendrando que a formula para a reta é y=a+b*x
#eta = intercepto + inclinação * x
#como plotar uma curva
#curve(9.936e+01+5.638e-04*x,add=TRUE)
#curve(1.054e+02 + -3.857e-04*x,add=TRUE)

# a variavel medi afeta a qualidade do ar?
m3 <- lm(airq~medi,data=Airq)
summary(m3)
plot(airq~medi,data=Airq)

# a variavel nao afetou a qualidade do ar
# o R ja entende o tipo de dados aonde ele sabe se a variavel é do tipo 
# categorica ou variveis continuas, assim ele vai plotar diferentes tipos de graficos 
# caso de variaveis continuas ele fará graficos de pontos 
# caso tenha variaveis categoricas ele fará outros tipos de plot (barras ou boxplot)

# a quantidade de chuvas influencia na qualidade do ar?

m4 <- lm(airq ~ rain,data = Airq)
summary(m4)

# a quantidade de chuva nao afeta a qualidade do ar

# a densidade populacional afeta a qualidade do ar?

m5 <- lm(airq ~ dens,data = Airq)
summary(m5)

# nao existe um efeito da densidade populacional da qualidade do ar

# retas de modelos não significativos são opcionais nos graficos

summary(m3)
plot(airq~medi,data=Airq)

# y = a + b * x
# a <- intercepto (onde a reta vai tocar no eixo y)
# b <- é a inclinação da reta
#add = TRUE inclui no grafico já plotado a reta medida

curve(9.936e+01 +5.638e-04 *x,add = TRUE)

#melhorar o gráfico 

#xlab = "Renda media per capita", legenda do eixo X
#ylab = "Qualidade do ar" legenda do eixo X
#pch  = 1 tipo de plotagem dos pontos
#col  = "blue"  trocar a cor dos pontos do gráfico
#cex.lab =1.3 # modificar o tamaho do gráfico
#main = 'Renda media - 2010' Colocar a legenda no gráfico 
#curve(9.936e+01 +  intercepto
#5.638e-04  inclinação da reta * 
#x, 
#add = TRUE, inclui no grafico já plotado a reta medida
#col = "darkblue", cor da linha 
#lwd = 2, espessura da linha   
#lty = 2 tipo da linha)


plot(airq~medi,data=Airq,xlab = "Renda media per capita", ylab ="Qualidade do ar",pch =1,col ="blue",cex.lab =1.3,main = 'Renda media - 2010')
curve(9.936e+01 +5.638e-04 *x,add = TRUE,col = "darkblue",lwd = 2, lty = 2)
summary(m1)

plot(airq ~ vala,data = Airq,xlab ="Valor das empresas ($)",
     ylab = "Qualidade do Ar",col = 'blue',pch = 1, cex = 1.2 ) 
curve(96.451419 + 0.001969 * x, add = TRUE, col = "darkblue",lwd = 2,lty =2 )

plot(airq ~ coas,data = Airq, xlab = "Posição costeira", ylab="Qualidade do ar",
     col = "lightblue", ylim =c(50,170),cex.çab = 1.3,main = "Analise da qualidade do ar")

#regressão multipla

mRM1<- lm(airq~vala+coas,data=Airq)
summary(mRM1)
# entao existe um efeito da posição da costeira e do valor das empresas na qualidade do ar

#gráfico regressão multipla
plot(airq ~ vala,data = Airq,xlab ="Valor das empresas ($)", ylab = "Qualidade do Ar" ) 

#plotando as curvas 

#curva referente á primeira variável
curve(1.171e+02 + 1.999e-03 * x, add = TRUE ) # cidade não costeiras
#vala    : mostra tudo que nao está na costeiras 
#coas(yes) : mostra valores que estão nas costeiras

#curva referente á segunda variável (depois do X deve colocar o valor da segunda variavel)
curve(1.171e+02 + 1.999e-03 * x+-2.968e+01, add = TRUE, lty =2 )#cidades costerias

#colocando legenda 
legend("bottomright" # canto beixo  direto 
       ,c("nao - costeiras","costeiras") # contatenando valores 
       ,pch =1 # colonado pontos usados no grafico
       ,lty=c(1,2) # concatenando os tipos de linhas 
       ,bty="n" #removendo bordas da legenda 
       )

# a qualidade do ar das cidades eh afetada tanto pelo valor das empresas 
# quanto pela posição costeira das cidades.
# quanto maior o valor das empresas pior a qualidade do ar das cidades, 
# alem disso as cidades não costeiras apresentam qualidade do ar pior que as cidades costeiras

mRM2<- lm(airq~vala+coas+dens,data=Airq)
summary(mRM2)

# contraste de modelo
# comparar um modelo completo com um modelo sem a variavel em questão
modelocompleto<- lm(airq~vala+coas+dens,data=Airq)
modeloimcompleto<- lm(airq~vala+coas,data=Airq)
#os modelos são iguais?
#se p >0.05 nao existe diferença entre os modelos, entao continuo com o modelo mais simples
#se p <0.05 os modelos são diferentes e a variavel nao  deve ser retirada do modelo fico com o modelo mais completo

#comparando os modelos 
anova(modelocompleto,modeloimcompleto) # valor de p 0.5732

# O que afeta a qualidade do ar nas cidades?
# as variaveis que afetaram foram: o valor das empresas e a posição costeira das cidades.
# quanto maior o valor das empresas, pior a qualidade dos ar, 
#cidades costeiras apresetam uma melhor qualidade do ar.














