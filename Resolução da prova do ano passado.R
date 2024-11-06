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
BT = t(B)
c <- solve(A %*% BT)
c

p <- B %*% (BT %*% B) %*% BT
p

#a)
eigen_result = eigen(p)
autovetores <- eigen_result$vectors
soma_autovetores <- colSums(autovetores)
soma_total_autovetores <- sum(soma_autovetores)
soma_total_autovetores
#ENT É FALSO ,POIS O RESULTADO APRESENTADO FOI -2.80603

#b)
sum(abs(diag(c)))
#SIM O RESULTADO é 0.0722

#c)
sum(A[lower.tri(A)])
#não a soma é 384

#d)
det_a = det(A)
det_b = det(B)
det_ab = det(A %*% B)
loga = log10(abs(det_a))
logb = log10(abs(det_b))
logab = log10(abs(det_ab))
loga
logb
logab
#SIM, Os resultados estão corretos

#e)
inversoab = solve(A %*% t(B))
diagab = diag(inversoab)
max(diagab)
#Não o maior valor é 0.01596


#2)
dt <- readr::read_csv("Dados/chocolate.csv.gz")
view(dt)

#a) 
num_paises <-dt %>%
  distinct(origem_cacau) %>%
  count()
num_paises

#b)
chocolates_filtrados <- dt %>%
  filter(str_extract(ingredientes, "^\\d+") %>% as.numeric() == 4 & 
           str_count(caracteristicas, ",") == 1)
num_chocolates <- nrow(chocolates_filtrados)
num_chocolates
#sim existem 104 

#c)
frequencia_5_ingredientes <- dt %>%
  filter(str_extract(ingredientes, "^\\d+") %>% as.numeric() == 5)
frequencia_absoluta <- nrow(frequencia_5_ingredientes)
frequencia_absoluta

#d)
caracteristicas = dt %>% 
  pull(caracteristicas)
caracteristicas

#e)existem 81 chocolates que incluem o ingrediente Adoçante em sua composição

cont_adocante = dt %>%
  filter(grepl("S*", ingredientes)) %>%
  pull(ingredientes) %>%
  length()
cont_adocante

##3)
artmoma = readr::read_csv("Dados/Art_Moma.csv.gz")
art = readr::read_csv("Dados/Art.csv.gz")
view(art)
view(artmoma)
#a)Os 3 artista(s) com mais exposições no The Whitney classificados em ordem decrescente de exposições são: Edward Hopper, Georgia O’Keeffe e Stuart Davis.
# Agrupar os dados pelo nome do artista e somar o número de exposições no Whitney
top_artists_whitney <- art %>%
  group_by(artist_name) %>%
  summarize(total_whitney_exhibitions = max(artmoma$whitney_count_to_year, na.rm = TRUE)) %>%
  arrange(desc(total_whitney_exhibitions)) %>%
  head(3)
top_artists_whitney
#b)Do total de artistas, 152 são Swiss, Mexican ou Japanese
nacionalidades = c("Swiss", "Mexican", "Japanese")
art %>% 
  filter(artist_nationality %in% nacionalidades) %>%
  nrow()
#não o total é 26.

#C)apenas 6 artista(s) com a nacionalidade Swiss tiveram entre 0 e 1 exposições no The Whitney.
###
art2 = inner_join(art, artmoma,by= "artist_unique_id")

#filtrando artista coma nacionalidade Swiss:

artist_swiss = art2 %>%  
  group_by(artist_nationality)%>%
  filter(artist_nationality %in% "Swiss") 
view(artist_swiss)
#Observando quantos tiveram entre 0 e 1 exposições:

swiss_01 = artist_swiss %>%
  filter(whitney_count_to_year == 1)
view(swiss_01)

#não, apenas 5 artistas tiveram entre 0 e 1 exposições no the Whitney 

#d)A diferença entre a média de páginas para artistas Brancos e Não Brancos no ano de 2007 é -0,24.

#filtrando artista brancos e calculando a media

mean_artist_white = art2 %>%
  filter(artist_race_nwi == "White", year == 2007) %>%
  summarise(media_page = mean(space_ratio_per_page_total))
mean_artist_white

#filtrando artistas não brancos
mean_artist_non_white = art2 %>%
  filter(artist_race_nwi == "Non-White", year== 2007) %>%
  summarise(media_page = mean(space_ratio_per_page_total))
mean_artist_non_white

diferenca = mean_artist_white - mean_artist_non_white
diferenca

#Sim, a diferença é de 0,24 

#e)Dos artista(s) que expuseram no The Whitney, apenas 164 aparecem nos livros ‘Gardner’ e ‘Janson’

#filtrando apenas artistas que expuseram no the whitney


artist_whitney = art2 %>%
  filter(whitney_count_to_year != 0)%>%
  select(artist_unique_id)

#Filtrando os que aparecem nos livros 
artist_books = art2 %>%
  filter(book %in% c("Gardner", "Janson")) %>% 
  select(artist_unique_id)
artists_common <- semi_join(artist_whitney, artist_books, by = "artist_unique_id")
num_artists_common <- nrow(artists_common)
num_artists_common
view(artists_common)
#não 





