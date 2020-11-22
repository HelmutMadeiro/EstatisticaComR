# definindo o projeto de curso
# pergunta: o que afeta a qualidade do ar? como?
#install.packages("Ecdat")

library(Ecdat) #carregando
data(Airq) # carregando o banco de dados do pagote
names(Airq) # exibe os nomes das variaveis


#Descrevendo as variaveis
# airq: indice de qualidade do ar(quanto menor , melhor)
# vala: valor das empresas nas cidades (milhares de dolares)
#rain quantidade de chuvas em polegadas
#coas posição costeira da cidade (sim ou nao)
#dens densidade populacional (milha quadrada)
#medi renda media per capital(dolares)

# analise descritiva  ou exploratoria

summary(Airq) # sumario das variaveis 

#as variaveis podem ser continuas ou categoricas ( divididas em categorias)
# a variavel reposta é a qualidde do ar(airq)

plot(airq~vala,data = Airq)
