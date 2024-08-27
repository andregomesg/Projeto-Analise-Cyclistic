# 🔍 Análise sobre o padrão de utilização dos clientes do sistema de compartilhamento de bicicletas 🔎
## Sobre 
Este projeto utiliza base de dados referentes a utilização do serviço de bicicletas compartilháveis de Chicago, NY. As bases foram disponibilizados através de um [Docker](https://divvy-tripdata.s3.amazonaws.com/index.html) pela empresa Divvy, operada pela Motivate Internation Inc, uma entidade reconhecida e respeitada no setor de compartilhamento de bicicletas. 

## 🤯 Contexto do problema
O objetivo é realizar uma análise de dados através de análise exploratória e descritiva para encontrar padrões buscando responder 3 perguntas levantadas pela Lily Moreno e sua equipe de marketing: 

1. Como os membros Cyclistic e ciclistas casuais usam as bicicletas da Cyclistic de forma diferente?
2. Por que os ciclistas casuais iriam querer adquirir planos planos anuais da Cyclistic?
3. Como a Cyclistic pode usar a mídia digital para influenciar os passageiros casuais  a se tornarem membros cyclistic?

## 📝 Compreensão dos dados
As bases de dados fornecidas se encontram em pastas zips e separadas mês a mês. Para está análise, precisam garantir o pilar de Atualidade, por isso, iremos utilizar os arquivos mais recentes (2023-05 a 2024-06) para a data da análise (2024-07 a 2024-08)

## ⚙️ Demandas
- Download e descompactação dos arquivos do Docker
- Processar os arquivos dos últimos 12M
- Realizar a limpeza
- Descobrir as principais variáveis
- Realizar as análises
- Compartilhar diferentes visões levando em consideração o nível dos stakeholders
- Sugerir ações

##  🎲 Dicionário dos dados
| Variáveis | Descrição | Tipo | Origem da Variável |
| --- | --- | --- | --- |
| ride_id               | Identificador único de cada viagem | Int | Legado |
| rideable_type   | Tipo de bicicleta  | Char | Legado |
| started_at   | Horário de Início da Viagem | Char | Legado |
| ended_at | Horário de Término da Viagem | Char | Legado |
| start_station_name | Nome da Estação de Início da Viagem | Char | Legado |
| start_station_id | Identificar único da estação de início da viagem | Char | Legado |
| end_station_name | Nome da Estação de Término da Viagem | Char | Legado |
| start_lat | Latitude da estação de início da viagem | Num | Legado |
| start_lng | Longitude da estação de início da viagem | Num | Legado |
| end_lat | Latitude da estação de início da viagem | Num | Legado |
| end_lng | Longitude da estação de início da viaggem | Num | Legado |
| member_casual | Tipo de cliente | Char | Legado |
| duration_trip | Diferença do horário de início da viagem e horário de término | Num | Criada |
| day_for_week | Dia de início de cada viagem | Num | Criada |

## 💻 Tecnologias utilizadas
- Linguagem  R 
  - Mega pacote Tidyverse
    - Pacote dply
    - Pacote ggplot2
    - Pacote tidyr 
    - Pacote purrr
    - Pacote lubridate
    - Pacote knitr 
  - Pacote Summarytools

## 🔐 Licença
Este arquivo faz parte do projeto Projeto-Analise-Cyclistic
(c) [2024] [André Christian F G Almeida]
Licenciado sob a licença MIT.
Veja o arquivo [LICENSE](LICENSE.MD) no diretório principal para mais informações.
