#Instalando pacotes

install.packages("tidyverse")
install.packages("dplyr")
install.packages("csv")
install.packages("xlsx")
install.packages("tidyverse")
install.packages("ggplot2")

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

#Producao de campos de SEMENTE de soja em comparação com as areas dos estados do Brasil
ggplot(soja, aes(x = UF, y = Area))+geom_col()+scale_y_continuous(n.breaks = 40, expand = expansion(add = c(0, 5000000)))
#Imagem: Produção Nacional Area plantada x UF

#Producao de campos de SEMENTE de soja em comparação com as areas dos estados do Brasil safra 2021/2022
ggplot(sojaBrasil2122, aes(x = UF, y = Area))+geom_col()+scale_y_continuous(n.breaks = 40, expand = expansion(add = c(0, 5000000)))
#Imagem: Produção Nacional Area plantada x UF

#---------------------------------------#---------------------------------------#---------------------------------------#
#Produção de campos de semente de soja, no Brasil, classificados por area de cada categoria plantada.
ggplot(soja, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 5000000)))

#Produção de campos de semente de soja, no Brasil na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaBrasil2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 10, expand = expansion(add = c(0, 500000)))
#Imagem: Producao Campo semente soja safra 21 22 Brasil

#Produção de campos de semente de soja, no estado do MT na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaMatoGrosso2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500000)))
gMt <- ggplot(sojaMatoGrosso2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500000)))
#Imagem: Producao campo semente soja safra 21 22 Mato Grosso

#Produção de campos de semente de soja, na cidade de Sinop na safra de 2021/2022, classificados por area de cada categoria plantada.
ggplot(sojaSinop2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500)))
gSinop <- ggplot(sojaSinop2122, aes(x = Categoria, y = Area))+geom_col()+scale_y_continuous(n.breaks = 30, expand = expansion(add = c(0, 500)))
#Imagem: Producao campo semente soja 21 22 Sinop MT

#---------------------------------------#---------------------------------------#---------------------------------------#
#Grafico verificando a produção de cada Cultivar na cidade de Sinop na safra 2021/2022 separadas por classes
gSinop+facet_grid( ~ Cultivar)
#Imagem: Categorias Semente Soja Plantadas SINOP safra 21 22

#Grafico verificando a produção de cada Cultivar na cidade de Sinop na safra 2021/2022 separadas por classes e status
gSinop+facet_grid(Status ~ Cultivar)
#Imagem: Cultivares Status Sinop safra 21 22

#Verificando sobre o estato do Mato Grosso
gMt+facet_grid( ~ Status)
#---------------------------------------#---------------------------------------#---------------------------------------#
