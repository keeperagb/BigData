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

#Filtramos a Produção somente da Soja que tem o nome cientifico de "Glycine max (L.) Merr." atrbuindo a uma nova Variavel Soja
soja <- Producao %>% filter(Especie == 'Glycine max (L.) Merr.')

#Criamos uma nova Variavel de soja com agrupamento por UF
sojaEstado <-
  Producao %>% group_by(UF)



