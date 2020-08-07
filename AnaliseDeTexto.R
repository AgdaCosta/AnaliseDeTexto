#*******************************************************************************
# Construindo Nuvem de palavras de tweets relacionados ao assunto: Feminicídio
#*******************************************************************************

#_______________________________________________________________________________
#-----------------------Pacotes necessários-------------------------------------

#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("tm")
#install.packages("readr")
#install.packages("magrittr")
#install.packages("stringi")
#install.packages("stringr")
#install.packages("wordcloud2")
#install.packages("kntnr")
#install.packages("kableExtra")
#install.packages("devtools")
#install.packages("pkload")

library(twitteR)   # Para ter acesso à API do Twitter
library(wordcloud) # Para fazer a nuvem de palavras
library(wordcloud2)# Para fazer a nuvem de palavras com mais personalização
library(dplyr)     # Para tratamento dos dados
library(tidytext)  # Para transformar tweets em palavras
library(tm)        # Para processamento e limpesa do corpus
library(readr)     # Para guardar arquivo
library(magrittr)  # Para usar %>%
library(stringi)   # Para tratamento de acentuação
library(stringr)   # Para formatação de palavras
library(knitr)     # Para criação de tabela
library(kableExtra)# Para Personalização tabela
require(devtools)  
require(pkgload)   
#_______________________________________________________________________________


#_______________________________________________________________________________
#-----------------------Fontes de Dados-----------------------------------------

api_key             <- "XXXXXXXXXXXXXXXXXX"
api_secret          <- "XXXXXXXXXXXXXXXXXX"
access_token        <- "XXXXXXXXXXXXXXXXXX"
access_token_secret <- "XXXXXXXXXXXXXXXXXX"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets <- searchTwitter("#feminicidio", n = 1000, locale = 'pt')
#_______________________________________________________________________________


#_______________________________________________________________________________
#-----------------------Transformação-------------------------------------------

dados = twitteR::twListToDF(tweets)

dados = dados %>%
  filter(isRetweet == FALSE) %>%
  select(text, id,created, screenName) 

write.table(dados,"tweets_feminicidio.csv", append=T,
                      row.names=F, col.names=T, sep=",")
remove(tweets)
#_______________________________________________________________________________


#_______________________________________________________________________________
#-----------------------Limpeza dos Dados---------------------------------------

dados$text <- gsub("#([a-z|A-Z|0-9|_])*","", dados$text)    # remove hashtags
dados$text <- gsub('@([a-z|A-Z|0-9|_])*', '', dados$text)   # remove palavras com @ (menções)
dados$text <- gsub('https://','', dados$text)               # removes https://
dados$text <- gsub('http://','', dados$text)                # removes http://
dados$text <- gsub('[^[:graph:]]', ' ', dados$text)         # removes graphic characters like emoticons 
dados$text <- gsub('[[:punct:]]', '', dados$text)           # removes punctuation 
dados$text <- gsub('[[:cntrl:]]', '', dados$text)           # removes control characters
dados$text <- gsub("\\w*[0-9]+\\w*\\s*", "", dados$text)    # removes numbers
dados$text <- tolower(dados$text)                           # Caixa baixa
dados$text <- gsub('feminicidio','feminicídio', dados$text) # Substituindo por palavra com acento

palavras = dados$text
corpus = VCorpus(VectorSource(palavras), readerControl = list(reader = readPlain, language = "pt"))
corpus = tm_map(corpus, removeWords, stopwords('pt'))
#_______________________________________________________________________________


#_______________________________________________________________________________
#-----------------------Análise Exploratória-----------------------------------------

freq = TermDocumentMatrix(corpus)                            # Matriz de Palvras frequentes 
matriz = as.matrix(freq)                                     # Transformando a matriz no formato R
matriz = sort(rowSums(matriz), decreasing = T)               # Ordenar de forma decrescente
matriz = data.frame(Palavra = names(matriz), Freq = matriz)  # Transformando em Data Frame
matriz$Palavra = str_to_title(matriz$Palavra)                # Primeira letra maiuscúla e demais minuscúla

head(matriz, n = 10) %>%
  mutate_if(is.numeric, function(x) {
    Frequência = cell_spec(x, "html", bold = T, color = spec_color(x, end = 0.9),
                           font_size = spec_font_size(x))
  }) %>%
  mutate(Palavra = cell_spec(
    Palavra, "html", color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1)
  )) %>%
  kable("html", escape = F, align = "c") %>%
  kable_styling(bootstrap_options = c("striped",  "condensed","responsive"), 
                full_width = F, font_size = 20) 
#_______________________________________________________________________________


#_______________________________________________________________________________
#-----------------------Visualização-----------------------------------------

par(bg = "black")
wordcloud(str_to_title(matriz$Palavra), matriz$Freq, random.order=FALSE, 
          colors=brewer.pal(8,"RdBu"), max.words = 50, rot.per = 0.2, scale = c (20,0.5))
#_______________________________________________________________________________
