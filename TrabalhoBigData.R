file.choose()

df = read.csv2(file.choose())
df

install.packages("tidyverse")
install.packages("dplyr")



#Instalando pacotes

install.packages("csv")
install.packages("xlsx")



# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just ggplot2:
install.packages("ggplot2")

#Carregamento da bibliotecas
library(csv)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

glimpse(df) #consulta dados da planilha

#Alterando as tabelas de data que estavam chr para date
df$Data.do.Plantio <- as.Date(df$Data.do.Plantio, format = "%d/%m/%Y")
df$Data.de.Colheita <- as.Date(df$Data.de.Colheita, format = "%d/%m/%Y")

df <- df %>% mutate(AnoPlantio = format(Data.do.Plantio, "%Y"), #Criei mais 2 colunas, uma do ano do plantio e a outra do mes que foi plantado
                    MesPlantio = format(Data.do.Plantio, "%m"))

#operações com colunas
sum(df$ï..Safra) #soma
mean(df$Area)#media
count(df, )

#mudar nome da tabela
df <- rename(df, Safra = safra)

############Filter############
Sinop2122 <- df  %>% filter(UF == 'MT' & Municipio == 'Sinop' & Safra == '2021/2022')
Sinop2122

MatoGrosso2122 <- df %>%  filter(UF == 'MT' & Safra == '2021/2022')
MatoGrosso2122

#Agrupamento a na safra 21/22 por UF trazendo as 2 maiores Areas
agrupamento <- df %>% filter(Safra == '2021/2022') %>% group_by(UF) %>% slice_max(Area , n=2)

############Ordenando############
df %>% arrange(Area) #Do menor para o maior
df %>%  arrange(desc(Area)) #Do maior para o menor
ordenado <- df %>%  arrange(desc(Area), desc(Producao.estimada))#Ordenando do maior pro menor em 2 ou mais colunas

############Select############
df %>% select(Safra,Cultivar,Municipio, UF)

df %>% select(-Data.de.Colheita, -Producao.bruta, -Producao.estimada, -Especie) #select excluindo as colunas com - na frente

#Selecionar colunas por nome
df %>% select(Status("Homologado"))

############Mutate############
#Mutate somente adciona novas variaveis ou transforma variaveis existentes

#Multiplicando uma coluna em outra
df %>% mutate(mediaProducao = Area*Producao.estimada)#Nao deu certo ;(

############Função Summarise############
summarise(mediaProducao = n()) #apenas um ex de aplicação do summrise
#Media Central: mean(), median()
#Variacao: sd(), IQR(), mad()
#Amplitude: min(), max(), quantile()
#Posicao: first(), last(), nth()
#Frequencia: n(), n_distinct()
#Logicas: any(), all()


df$cod_raca_cor_pessoa <- NULL #Excluir uma coluna

#Filtrando Sinop e Sorriso
Sinop2122 <- df %>% filter(Safra == '2021/2022' & Municipio =='Sinop') 
Sorriso2122 <- df %>% filter(Safra == '2021/2022' & Municipio =='Sorriso')

#Agrupamento pela safra 2021/22 por estado
agrupamento <- df %>% filter(Safra == '2021/2022') %>% group_by(UF) %>% 
  ggplot()+geom_point(data = agrupamento, aes(x = Categoria, y = UF))
  
#Primeiro Grafico mostrando a producao estimada em comparativo com o tamanho das areas
ggplot()+geom_point(data = Sinop2122, aes(x = Area, y = Producao.estimada),colour = 'red')+
        geom_point(data = Sorriso2122, aes(x = Area, y = Producao.estimada),colour = "blue")+#Grafico em barras comparando a produção de 2021/2022 de sinop e sorriso
        geom_line(data = Sinop2122, aes(x = Area, y = Producao.estimada), colour = 'red')+
        geom_line(data = Sorriso2122, aes(x = Area, y = Producao.estimada), colour = 'blue')

MatoGrosso <- df %>% filter(UF == 'MT')
ggplot(data = MatoGrosso)+
  geom_histogram(aes(x = Area))

#Grafico da principal cultura com campo de produção no MT
ggplot(data = MatoGrosso)+
  geom_bar(aes(x = Categoria, y= ..count..))

#Grafico comparando a categoria com os campos plantados
ggplot(data = MatoGrosso)+
  geom_boxplot(aes(x = Categoria, y = Area))

ggplot(data = df)+
  geom_boxplot(aes(x = Categoria, y = Area))

#Grafico de linhas - Data de colheitas
ggplot(data = MatoGrosso)+
  geom_line(aes(x=Data.de.Colheita, group = 1), stat = "count")


#Pegando os dados da safra 21/22 das categorias, separamos elas por Status
g<-ggplot(MatoGrosso2122, aes(Categoria))
g+geom_bar()
g + geom_bar(aes(fill = Status))

#Pegando os dados da safra Geal do estado do MAATO GROSSO das categorias, separamos elas por Status
ge<-ggplot(MatoGrosso, aes(Categoria))
ge+geom_bar()
ge + geom_bar(aes(fill = Status))

#Pegando os dados de todas safras das categorias, separamos elas por Status
gee<-ggplot(df, aes(Categoria))
gee+geom_bar()
gee + geom_bar(aes(fill = Status))


aaa