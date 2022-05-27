#Instalando pacotes

install.packages("tidyverse") #Pacote com varios componente que é composto tais como dplyr, ggplot2.
install.packages("dplyr") #Conjunto de funções projetado para permitir a manipulação de dataframe.
install.packages("csv") #Pacote de leitura de arquivo CSV.
install.packages("xlsx") #Pacote de leitura de arquivo CSV.
install.packages("ggplot2") #Pacote de visualização de dados em graficos.

#Carregamento da bibliotecas
library(csv)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

###################################################################################

#Atribuo o DF a variavel
Producao = read.csv2(file.choose())

glimpse(Producao) #consulta dados da planilha

#Alterando as tabelas de data que estavam chr para date
Producao$Data.do.Plantio <- as.Date(Producao$Data.do.Plantio, format = "%d/%m/%Y")
Producao$Data.de.Colheita <- as.Date(Producao$Data.de.Colheita, format = "%d/%m/%Y")

#mudar nome da tabela
Producao <- rename(Producao, Safra = ï..Safra)

glimpse(Producao) #consulta dados da planilha

###################################################################################

#---------------------------------------#---------------------------------------#---------------------------------------#
#Filtramos a Produção somente da Soja que tem o nome cientifico de "Glycine max (L.) Merr." atrbuindo a uma nova Variavel Soja
soja <- Producao %>% filter(Especie == 'Glycine max (L.) Merr.')

sojaBrasil2122 <- soja %>% filter(Safra == '2021/2022')
sojaSinop2122 <- soja %>% filter(Municipio == 'Sinop' & Safra == '2021/2022')
sojaSorriso2122 <- soja %>% filter(Municipio == 'Sorriso' & Safra == '2021/2022')
sojaMatoGrosso2122 <- soja %>% filter(UF == 'MT' & Safra == '2021/2022')
#---------------------------------------#---------------------------------------#---------------------------------------#

#Producao de soja por tipo de semente em comparação com as areas dos estados do Brasil
ggplot(soja, aes(x = UF, y = Area))+geom_col()+scale_y_continuous(n.breaks = 40, expand = expansion(add = c(0, 5000000)))
#Imagem: Produção Nacional Area plantada x UF

#Producao de soja por tipo de semente em comparação com as areas dos estados do Brasil safra 2021/2022
ggplot(sojaBrasil2122, aes(x = UF, y = Area))+geom_col()+scale_y_continuous(n.breaks = 40, expand = expansion(add = c(0, 5000000)))
#Imagem: Produção Nacional Area plantada x UF
sum(sojaMatoGrosso2122$Area)

#---------------------------------------#---------------------------------------#---------------------------------------#
#Producao de soja por tipo de semente, no Brasil, classificados por area de cada categoria plantada.
ggplot(soja, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 0, expand = expansion(add = c(0, 5000000)))

#Producao de soja por tipo de semente, no Brasil na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaBrasil2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 10, expand = expansion(add = c(0, 500000)))
#Imagem: Producao Campo semente soja safra 21 22 Brasil

#Producao de soja por tipo de semente, no estado do MT na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaMatoGrosso2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500000)))
gMt <- sojaMatoGrosso2122 %>% filter(Categoria == 'S1')
sum(gMt$Area)
mean(gMt$Area)
#Imagem: Producao campo semente soja safra 21 22 Mato Grosso

#Producao de soja por tipo de semente, na cidade de Sinop na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaSinop2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500)))
gSinop <- ggplot(sojaSinop2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500)))
#Imagem: Producao campo semente soja 21 22 Sinop MT