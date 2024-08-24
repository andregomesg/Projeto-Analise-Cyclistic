# Carregamento das bibliotecas
  library(summarytools)
  library(tidyverse)
  library(lubridate)
  library(knitr)

# Verifica o diretório atual
  getwd()

# Definição do diretório dos arquivos
diretorio <- "C:/Users/André/Documents/ProjetoBike/ProjetoAnaliseCyclistic"

# Listagem dos arquivos existentes na pasta
  leitura_arq <- function(file){
  read.csv(file)
  }

# Leitura de cada arquivo csv
arquivos <- list.files(diretorio, pattern = "*.csv", full.names = TRUE)

# Criação do dataframe com os arquivos existentes
  df_historico <- map_df(arquivos, leitura_arq)

# Sumário estátistico do data frame
df_summary <- dfSummary(df_historico)
  print(df_summary)

# Remove as linhas duplicadas com base na coluna ride_id
  df_historico_limpo <- df_historico %>%
    distinct(ride_id, .keep_all = TRUE)

# Verificação se ainda há linhas duplicadas
existe_duplicadas <- df_historico_limpo %>%
  filter(duplicated(ride_id) | duplicated(ride_id, fromLast = TRUE)) %>%
  nrow() > 0
print(existe_duplicadas)

# Verificação se há pelo menos um valor nulo ou vazio em cada linha
verifica_vazio_ou_nulo <- function(x) {
  if (is.character(x)) {
    return(is.na(x) | x == "")
  } else {
    return(is.na(x))
  }
}

# Aplicar a função para cada linha do dataframe
linha_com_problemas <- apply(df_historico_limpo, 1, function(row) any(verifica_vazio_ou_nulo(row)))

# Filtrar o dataframe para remover as linhas com erros e manter o dataframe original
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
 
# Análise descritiva do dataframe
df_summary <- dfSummary(df_historico_limpo)
print(df_summary)
# resumo da summary do df para o arq rmarkdown no Notion
 
# Moda do day_of_the_week
freq_day_of_the_week <- table(df_historico_limpo$day_of_the_week)
moda <- names(freq_day_of_the_week)[which.max(freq_day_of_the_week)]
print(paste("💡 O dia em que os clientes mais utilizam o serviço é no(a)", moda ))

# Moda do rideable_type
 freq_rideable_type <- table(df_historico_limpo$rideable_type)
moda <- names(freq_rideable_type[which.max(freq_rideable_type)])
print(paste("💡 O tipo de bicicleta que mais utilizam são as bicicletas ", moda))


# Comparação da quantidade de viagem segundo o tipo de bicicleta segmentado por tipo de membro
title_text <- str_wrap("Quantidade de viagens por tipo de bicicleta e membro")
subtitle_text <- str_wrap("Ambos os membros utilizam os modelos de bicicleta de forma parecida, sendo a clássica a principal escolha.")
  ggplot(data = df_historico_limpo) + 
    geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) + 
    facet_wrap(~member_casual) +
    labs(title = title_text,
       subtitle = subtitle_text, 
       y = "Quantidade de viagens", 
       x = "Tipos de bicicleta",
       fill = "Tipos de bicicleta") +
   scale_y_continuous(labels = scales::comma_format()) # separação de milhares

# Comparação da quantidade de viagem por dia da semana segmentado por tipo de membro
title_text <- str_wrap("Quantidade de viagens durante a semana por tipo de membro")
subtitle_text <- str_wrap("Os membros Cyclistic costumam utilizar as bicicletas durante a semana, enquanto os membros casuais tendem a utilizar o serviço aos finais de semana")
  ggplot(data = df_historico_limpo) + 
  geom_bar(mapping = aes(x = day_of_the_week, fill = day_of_the_week)) + 
  facet_wrap(~member_casual) +
  labs(title = title_text,
       subtitle = subtitle_text,
       y = "Quantidade de viagens",
       x = "Dias da semana",
       fill = "Dias da semana") +
  scale_y_continuous(labels = scales::comma_format()) # separação de milhares

# Duração média de viagens segundo os dias da semana segmentado por tipo de membro
  title_text <- str_wrap("Duração média de viagens por dia da semana e tipo de membro")
    subtitle_text <- str_wrap("O tempo médio de viagem dos membros casuais segue o padrão esperado, com viagens mais longas nos finais de semana. Enquanto os membros Cyclistic também apresentam um comportamento semelhante..") 
df_historico_limpo %>%
 mutate(duration_trip_min = as.numeric(duration_trip, units = "mins")) %>%
 group_by(day_of_the_week, member_casual) %>%
    summarize(mean_duration_trip = mean(duration_trip_min), .groups = 'drop') %>%
    ggplot(aes(x = day_of_the_week, y = mean_duration_trip, fill = day_of_the_week)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~member_casual) +
labs(title = title_text,
     subtitle = subtitle_text, 
     y = "Duração média das viagens",
     x = "Dias da semana",
     fill = "Dias da semana")
