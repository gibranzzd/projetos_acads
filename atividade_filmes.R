# 1

UCBAdmissions

ucba = data.frame(UCBAdmissions)

install.packages('vcd')
library('vcd')

resumoUCBA_A = xtabs(Freq ~ Gender+Admit, data=ucba[ucba$Dept=="A",])
mosaic(Freq ~ Gender+Admit, data=resumoUCBA_A)

resumoUCBA_B = xtabs(Freq ~ Gender+Admit, data=ucba[ucba$Dept=="B",])
mosaic(Freq ~ Gender+Admit, data=resumoUCBA_B)

resumoUCBA_C = xtabs(Freq ~ Gender+Admit, data=ucba[ucba$Dept=="C",])
mosaic(Freq ~ Gender+Admit, data=resumoUCBA_C)

resumoUCBA_D = xtabs(Freq ~ Gender+Admit, data=ucba[ucba$Dept=="D",])
mosaic(Freq ~ Gender+Admit, data=resumoUCBA_D)

# No geral, há uma diferença discrepante da admissão por gênero apenas no departamento A tendo mais mulheres, e os demais
# departamentos havendo pouca diferença entre os gêneros.

# 2

install.packages('tibble')
install.packages('dplyr')
install.packages('tidyr')
library(tibble)
library(dplyr)
library(tidyr)

as_tibble(mtcars)

basedf = data.frame(mtcars)

basedf

"Na nova base de dados gerada, não é mostrado a base inteira, apenas informado
o número de linhas e colunas, assim como não é apresentado a coluna dos
modelos dos veículos"

#3

imdb_csv <- read_csv("C:/Users/gideo/Downloads/imdb.csv")
glimpse(imdb_csv)

"A função glimpse informa o número de linhas e colunas, e mostra os dados 
contidos dentro de cada coluna"

#4
imdb_simples = select(imdb_csv, titulo, direcao, orcamento)
imdb_simples

#5
imdb_csv  %>% select(contains(c('duracao', 'direcao', 'descricao', 'producao')))

#6
imbd_filtro = imdb_csv %>% select(-num_avaliacoes, -num_criticas_publico, -num_criticas_critica)
imbd_filtro

#7
filmes_ordenados = (imdb_csv) %>% arrange(ano, desc(orcamento))
filmes_ordenados

#8
filmes_decrescentes = (imdb_csv) %>% arrange(titulo, desc(orcamento))
filmes_decrescentes

#9
filmes_ingles = imdb_csv %>% filter(idioma %in%c("English"))
filmes_ingles

#10
curtos_legais = imdb_csv %>% filter(duracao <= 90, nota_imdb > 8.5)
curtos_legais

#11

str(imdb_csv)

#a 

filmes_de_acao = imdb_csv %>% filter(generos %in%c("Action") & ano < 1950)
filmes_de_acao

#b

dirigidos = imdb_csv %>% filter(direcao %in%c("Woody Allen", "Wes Anderson"))
dirigidos

#c

steven = imdb_csv %>%  filter(direcao %in%c("Steven Spielberg")) %>% select(titulo, ano) %>% arrange(titulo, desc(ano))
steven

#d

action_comedy = imdb_csv %>% filter(generos %in%c("Action", "Comedy"))
action_comedy

#e

action_nota = imdb_csv %>% filter(generos %in%c("Action", "Comedy")) %>% filter(nota_imdb > 8)
action_nota

#f

filmes_sem = imdb_csv %>% filter(is.na(orcamento)) %>% filter(is.na(receita))
filmes_sem

#12

barplot(table(imdb_csv$orcamento))



