install.packages("dplyr")
require(dplyr)
library(readr)
library(stringr)
require(tidyverse)
library(tibble)

#PROVA
#1)
# Definindo a matriz A
A <- matrix(c(28, 32, 8, 9, 49,
              7, 21, 35, 28, 10,
              47, 43, 15, 34, 2,
              48, 42, 19, 32, 26,
              45, 44, 39, 50, 26), nrow = 5, byrow = TRUE)

# Definindo a matriz B
B <- matrix(c(0, 26, 3, 8, 30,
              35, 12, 19, 27, 27,
              27, 24, 12, 17, 29,
              31, 36, 40, 35, 8,
              24, 43, 31, 21, 39), nrow = 5, byrow = TRUE)

# Calcule os autovetores para cada matriz
eigen_A <- eigen(A)$vectors
eigen_B <- eigen(B)$vectors

# Some os autovetores
soma_autovetores <- eigen_A + eigen_B

# Calculando P
P <- B %*% (t(B) %*% B) %*% t(B)

# Calculando os autovetores de P
eigen_P <- eigen(P)$vectors

# Calculando a soma dos autovetores de P
soma_autovetores_P <- colSums(eigen_P)

# Exibindo a soma dos autovetores de P
print(soma_autovetores_P)

# Arredondando para 4 casas decimais para facilitar a comparação
soma_autovetores_P_arredondada <- round(sum(soma_autovetores_P), 4)

soma_autovetores_P_arredondada

#b)
# Calculando C
C <- solve(A %*% t(B))

# Calculando a soma dos valores absolutos da diagonal de C
soma_diagonal_C <- sum(abs(diag(C)))

# Exibindo a soma
soma_diagonal_C

#c)
sum(A[lower.tri(A)])
#não a soma é 384

#d)
# Calculando o log10 do valor absoluto do determinante de A
log_det_A <- log10(abs(det(A)))

# Exibindo o resultado
print(log_det_A)

# Calculando o log10 do valor absoluto do determinante de B
log_det_B <- log10(abs(det(B)))

# Exibindo o resultado
print(log_det_B)

# Calculando o determinante da matriz resultante do produto matricial entre A e B
det_AB <- det(A %*% B)

# Calculando o log10 do valor absoluto do determinante de AB
log_det_AB <- log10(abs(det_AB))

# Exibindo o resultado
print(log_det_AB)

#e)
# Calculando o produto matricial entre A e o transposto de B
ABt <- A %*% t(B)

# Calculando o inverso da matriz resultante
inv_ABt <- solve(ABt)

# Obtendo a diagonal da matriz inversa
diagonal_inv_ABt <- diag(inv_ABt)

# Encontrando o maior elemento da diagonal
maior_elemento_diagonal <- max(diagonal_inv_ABt)

# Exibindo o resultado
maior_elemento_diagonal



#2)
require(data.table)
require(dplyr)
require(tidyr)
chocolate = fread("./avaliação_01-datasets/chocolate.csv.gz")
glimpse(chocolate)

#a)
num_paises <- chocolate %>%
  select(origem_cacau) %>%
  distinct() %>%
  nrow()

# Exibir o número de países
print(num_paises)

#b)
chocolate$qtdade_caracteristicas <- lengths(strsplit(chocolate$caracteristicas, ","))

q2 <- chocolate %>%
  filter(substr(ingredientes, 1, 1) == "4",
         qtdade_caracteristicas == "2")

# Contar o número de chocolates resultantes
num_chocolates <- nrow(q2)

# Exibir o número de chocolates
print(num_chocolates)

#c)
# Filtrar chocolates que contenham 5 ingredientes
chocolates_5_ingredientes <- chocolate %>%
  filter(substr(ingredientes, 1, 1) == "5")

# Calcular a frequência absoluta
frequencia_absoluta <- nrow(chocolates_5_ingredientes)

# Exibir a frequência absoluta
print(frequencia_absoluta)

#d)
# Criar um novo dataframe com as características separadas em linhas
caracteristicas_chocolate <- chocolate %>%
  separate_rows(caracteristicas, sep = ",") %>%
  group_by(caracteristicas) %>%
  summarise(frequencia = n()) %>%
  arrange(desc(frequencia))

caracteristicas_chocolate

# Selecionar apenas as características desejadas
palavras_chave <- c("sweet", "nutty", "cocoa", "roasty", "creamy", "earthy", "sandy", "fatty")

# Criar uma expressão regular para corresponder a variações nas palavras-chave
padrao_regex <- paste0("\\b", paste(palavras_chave, collapse = "|"), "\\b", ignore.case = TRUE)

# Filtrar as características desejadas e somar as frequências
q2b <- caracteristicas_chocolate %>%
  filter(grepl(padrao_regex, caracteristicas))  %>%
  group_by(caracteristicas)

# Exibir a soma das frequências
print(q2b)

sum(q2b$frequencia)

#e)
# Criar um novo dataframe com as características separadas em linhas
ingredientes_chocolate <- chocolate %>%
  separate_rows(ingredientes, sep = ",") %>%
  separate_rows(ingredientes, sep = "-") %>%
  group_by(ingredientes) %>%
  summarise(frequencia = n()) %>%
  arrange(desc(frequencia))

sum(ingredientes_chocolate$frequencia[ingredientes_chocolate$ingredientes == "S*"])

#3) a)

require(data.table)
require(dplyr)
require(tidyr)

art_moma = fread("./avaliação_01-datasets/Art_Moma.csv.gz")
art = fread("./avaliação_01-datasets/Art.csv.gz")
glimpse(art_moma)
glimpse(art)
q3a <- inner_join(art, art_moma,
                  by = "artist_unique_id") %>%
  group_by(artist_name) %>%
  summarise(sum_whitney_count_to_year = sum(whitney_count_to_year)) %>%
  arrange(desc(sum_whitney_count_to_year))

q3a

#b)
q3b <- inner_join(art, art_moma,
                  by = "artist_unique_id") %>%
  group_by(artist_nationality) %>%
  summarise(frequencia = n()) 

q3b

palavras_chave <- c("Swiss", "Mexican", "Japanese")


# Filtrar as características desejadas e somar as frequências
q3b <- q3b %>%
  filter(artist_nationality %in% palavras_chave)

# Exibir a soma das frequências
print(q3b)

sum(q3b$frequencia)

#c)

q3c <- inner_join(art, art_moma,
                  by = "artist_unique_id") %>%
  group_by(artist_name, artist_nationality) %>%
  summarise(sum_whitney_count_to_year = sum(whitney_count_to_year))

q3c


palavras_chave <- c("Swiss")


# Filtrar as características desejadas e somar as frequências
q3c <- q3c %>%
  filter(artist_nationality %in% palavras_chave &
           sum_whitney_count_to_year <= 1)

# Exibir a soma das frequências
print(q3c)

nrow(q3c)

#d)

q3d <- inner_join(art, art_moma,
                  by = "artist_unique_id") 

# Filtrar por artist_race == "white" e calcular a média
media_white <- q3d %>%
  filter(artist_race_nwi == "White") %>%
  summarise(media_space_ratio = mean(space_ratio_per_page_total))

# Filtrar por artist_race diferente de "white" e calcular a média
media_nao_white <- q3d %>%
  filter(artist_race_nwi != "White") %>%
  summarise(media_space_ratio = mean(space_ratio_per_page_total))

# Calcular a diferença
diferenca <- media_white$media_space_ratio - media_nao_white$media_space_ratio

# Exibir o resultado
print(diferenca)

#e)

q3e <- inner_join(art, art_moma,
                  by = "artist_unique_id") 

unique(q3e$book)

q3e <- q3e %>%
  filter(whitney_count_to_year > 0) %>%
  group_by(artist_name) %>%
  summarise(frequencia = n())

nrow(q3e)

#4)

require(data.table)
require(dplyr)
require(tidyr)

refugiados_pais= fread("./avaliação_01-datasets/refugiados_pais.csv.gz")
refugiados = fread("./avaliação_01-datasets/refugiados.csv.gz")


glimpse(refugiados_pais)

glimpse(refugiados)

# Realizar o left join
tb4 <- refugiados %>%
  left_join(refugiados_pais, by = c("id_origem" = "id")) %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"), suffix = c("_origem", "_destino"))

# Exibir a planilha
print(tb4)

#a)

# Filtrar para o ano de 2006
tb4_2006 <- tb4 %>% filter(ano == 2006)

# Criar a matriz de migração [origem, destino]
matriz_migracao_2006 <- tb4_2006 %>%
  group_by(regiao_origem, regiao_destino) %>%
  summarise(total_migrantes = sum(refugiados)) %>%
  pivot_wider(names_from = regiao_destino, values_from = total_migrantes, values_fill = 0)

# Exibir a matriz de migração no formato desejado
print(matriz_migracao_2006)

#b)

# Especificar o nome_origem, nome_destino e ano desejados
ano_desejado <- 1972

q4b <- tb4 %>%
  group_by(nome_origem, nome_destino, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") 

resultado <- q4b %>%
  filter(ano >= ano_desejado) %>%
  group_by(nome_origem, nome_destino) %>%
  summarise(total_refugiados = sum(refugiados))

refugiados_af_can <- resultado %>%
  filter(nome_origem == "Afghanistan", nome_destino == "Canada")
print(refugiados_af_can)

refugiados_paq_can <- resultado %>%
  filter(nome_origem == "Pakistan", nome_destino == "Canada")
print(refugiados_paq_can)

#C)

ano_c <- 1965

q4c <- tb4 %>%
  group_by(nome_origem, subregiao_origem, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") 

q4c <- q4c %>%
  filter(ano == ano_c) %>%
  group_by(nome_origem, subregiao_origem) %>%
  summarise(total_refugiados = sum(refugiados)) %>%
  arrange(desc(total_refugiados))

q4c

#D)

ano_d <- 1982

q4d <- tb4 %>%
  group_by(nome_destino, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") %>%
  na.omit()

q4d <- q4d %>%
  filter(ano >= ano_d) %>%
  group_by(nome_destino) %>%
  summarise(total_refugiados = sum(refugiados)) %>%
  arrange(desc(total_refugiados))

q4d

#E)

n_refugiados <- 5382652

q4e<- tb4 %>%
  group_by(nome_destino) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") %>%
  na.omit()

q4e <- q4e %>%
  filter(refugiados >= n_refugiados) %>%
  arrange(desc(refugiados))

cat('Existem', nrow(q4e), 'países que receberam pelo menos', n_refugiados, 'refugiados.\n')
