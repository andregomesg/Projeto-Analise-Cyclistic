# Instalação e carregamento das bibliotecas
  install.packages("tidyverse")
  install.packages("summarytools")
  library(summarytools)
  library(tidyverse)
  library(dplyr)
  library(purrr)
  library(lubridate)
  library(knitr)
  

# Verifica o diretório atual
  getwd()

# Define o diretório dos arquivos
  diretorio <- "C:/Users/André/Documents/ProjetoBike/ProjetoAnaliseCyclistic"

# Listagem dos arquivos existentes na pasta
  arquivos <- list.files(diretorio, pattern = "*.csv", full.names = TRUE)

# Leitura de cada arquivo csv
  leitura_arq <- function(file){
  read.csv(file)
  }

# Criação do dataframe com os arquivos existentes
  df_historico <- map_df(arquivos, leitura_arq)

# Visualização das variáveis e seus tipos
  str(df_historico)

# Remove linhas duplicadas com base na coluna 'ride_id'
  df_historico_limpo <- df_historico %>%
    distinct(ride_id, .keep_all = TRUE)

# Verifica ainda existem linhas duplicadas após a limpeza com base na coluna ride_id
  existe_duplicadas <- df_historico_limpo %>%
    filter(duplicated(ride_id)) %>%
    nrow() > 0
  print(existe_duplicadas)

# Verifica se há pelo menos um valor nulo ou vazio em cada linha
  verifica_vazio_ou_nulo <- function(x) {
    if (is.character(x)) {
      return(is.na(x) | x == "")
    } else {
      return(is.na(x))
    }
  }

# Aplicar a função para cada linha do dataframe
  linha_com_problemas <- apply(df_historico_limpo, 1, function(row) any(verifica_vazio_ou_nulo(row)))

# Contar o número de linhas com pelo menos um valor nulo ou vazio
  numero_linhas_com_problemas <- sum(linha_com_problemas)

# Filtrar o dataframe para remover as linhas com erros e manter o df_origem
  df_historico_limpo <- df_historico_limpo[!linha_com_problemas, ]

#Mostrar a quantidade de linhas antes e depois da limpeza
 
 numero_de_linhas_antes <- nrow(df_historico)
 numero_de_linhas_depois <- nrow(df_historico_limpo)

  print(paste("Número de linhas antes da limpeza: ", numero_de_linhas_antes))
  print(paste("Número de linhas depois da limpeza: ", numero_de_linhas_depois))

# Adissão da coluna "duration_trip" ao dataframe
  #conversão da colunas started_at e ended_at para "difftime"
  df_historico_limpo$started_at <- ymd_hms(df_historico_limpo$started_at)
  df_historico_limpo$ended_at <- ymd_hms(df_historico_limpo$ended_at)
  
 # Calculo da duração e conversão para minutos arrendondados para cima
  df_historico_limpo <- df_historico_limpo %>% 
    mutate(duration_trip = ceiling(difftime(ended_at, started_at, units = "mins"))) #em minutos

# Criação da coluna "day_of_the_week" 
  df_historico_limpo <- df_historico_limpo %>%
    mutate(day_of_the_week = wday(started_at, label = TRUE))
# Com essa etapa, finalizamos a etapa de processar
 
# Análise descritiva do dataframe
df_summary <- dfSummary(df_historico_limpo)
print(df_summary)
# resumo da summary do df para o arq rmarkdown no Notion
 
# Moda do day_of_the_week
freq_day_of_the_week <- table(df_historico_limpo$day_of_the_week)
moda <- names(freq_day_of_the_week)[which.max(freq_day_of_the_week)]
moda

# Moda do rideable_type
  freq_rideable_type <- table(df_historico_limpo$rideable_type)
  moda <- names(freq_rideable_type[which.max(freq_rideable_type)])
  moda


# Comparação da qtde de viagens segundo dia da semana por tipo de membro
  ggplot(data = df_historico_limpo) + 
  geom_bar(mapping = aes(x = day_of_the_week)) + 
  facet_wrap(~member_casual) 

# Duração Média de viagens segundo os dias da semana por tipo de membro
 df_historico_limpo %>%
 mutate(duration_trip_min = as.numeric(duration_trip, units = "mins")) %>%
 group_by(day_of_the_week, member_casual) %>%
    summarize(mean_duration_trip = mean(duration_trip_min)) %>%
    ggplot(aes(x = day_of_the_week, y = mean_duration_trip)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~member_casual)

# Comparação da qtde de viagens segundo o tipo de bicicleta
  ggplot(data = df_historico_limpo) + 
    geom_bar(mapping = aes(x = rideable_type)) + 
    facet_wrap(~member_casual)

# ACABEI PORRAAAAAAAAA, FAZER O RMARKDOWN AGORA