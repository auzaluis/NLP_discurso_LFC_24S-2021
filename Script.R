
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(tidytext)
library(igraph)
library(ggraph)



# Importación

discurso <- readLines("LFC24S.txt",
                      warn = F,
                      encoding = "UTF-8")



DF <- as.data.frame(x = discurso)
colnames(DF) <- "comment"



# Limpieza

## Fase 1: Con uso de regex

clean_data_1 <- function(text){
  
  text = gsub("https:\\S+", " ", text)                   # Links html
  text = gsub("[[:punct:]]", " ", text)                  # Puntuaciones
  text = gsub("[ \t]{2,}", " ", text)                    # Tab
  text = gsub("\\s\\s+", " ", text)                      # Espacios extras
  text = gsub("wh?iph?ala", "wiphala", text)             # wiphala
  text = gsub("(?i)sta cruz", "santa cruz", text)        # santa cruz
  text = gsub("(?i)scz", "santa cruz", text)             # santa cruz
  
}

DF$comment <- clean_data_1(text = DF$comment)



## Fase 2: Uso de las funciones chartr

DF$comment <- chartr("áéíóúü", "aeiouu", DF$comment)         # Eliminar tildes minúsculas
DF$comment <- chartr("ÁÉÍÓÚÜ", "AEIOUU", DF$comment)         # Eliminar tildes mayúsculas
DF$comment <- tolower(DF$comment)                            # Cambiar a minúsculas



# Uso de la función Content Transformer

### Conversión a Corpus Linguístico

Comentarios <- VectorSource(DF$comment)                      # De vector a lista
Comentarios <- Corpus(Comentarios)                           # De caracteres a documentos (corpus)
inspect(Comentarios)

## Nota: Queda pendiente aplicar un lematizador



# Creación de la BoW (Bag of Words)

## Term Document Matrix

Matriz <- TermDocumentMatrix(Comentarios)
Matriz <- as.matrix(Matriz)

## Nota: Word Counts Method



## BoW: Matriz transpuesta + aplicación de alpha = 1

BoW <- data.frame(t(Matriz))



## Nube de pablabras ####

DF_wordcloud <- data.frame(colSums(BoW))
colnames(DF_wordcloud) <- "frecuencia"
DF_wordcloud$word <- rownames(DF_wordcloud)
rownames(DF_wordcloud) <- NULL
DF_wordcloud <- DF_wordcloud[, c(2,1)]



stopwords <- c(stopwords("es"),
               "q", "che", "aunque", "vos", "disque", "pal", "voj", "ami", "habia", "asta",
               "osea", "uds", "ustedes", "que", "ja", "f", "r", "u", "d", "s", "c", "y")

stopwords <- as.data.frame(x = stopwords)
colnames(stopwords) <- "stopword"

DF_wordcloud <- DF_wordcloud %>% anti_join(stopwords,
                                           by = c("word" = "stopword"))



display.brewer.all()
wordcloud(words = DF_wordcloud$word,
          freq = DF_wordcloud$frecuencia,
          max.words = 23,
          random.order = F,
          colors = brewer.pal(8, "Dark2"))



## Análisis de sentimientos

DF_sentiment <- get_nrc_sentiment(char_v = DF$comment,
                                  language = "spanish")

table_sentiment <- apply(X = DF_sentiment,
                         MARGIN = 2,
                         FUN = sum)

DF_table_sentiment <- as.data.frame(x = table_sentiment)
DF_table_sentiment$sentiment <- rownames(DF_table_sentiment)
rownames(DF_table_sentiment) <- NULL
colnames(DF_table_sentiment)[1] <- "count"

View(DF_table_sentiment)

DF_table_sentiment$sentiment <- c("Ira",
                                  "Anticipación",
                                  "Aversión",
                                  "Miedo",
                                  "Alegría",
                                  "Tristeza",
                                  "Sorpresa",
                                  "Confianza",
                                  "Negativo",
                                  "Positivo")

DF_table_sentiment$type <- c(rep(x = "sentimiento", times = nrow(x = DF_table_sentiment) - 2),
                             rep(x = "polaridad", times = 2))

#DF_table_sentiment <- DF_table_sentiment[order(DF_table_sentiment$count,
#decreasing = T),]

DF_table_sentiment <- DF_table_sentiment[, c(3,2,1)]

ggplot(data = DF_table_sentiment[c(1:8),],
       aes(x = count,
           y = reorder(sentiment, count),
           fill = count)) +
  
  geom_col() +
  
  scale_fill_gradient(low = "darkblue",
                      high = "red4") +
  
  theme(axis.text.y = element_text(size = 17),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank())
  


## Bigram

DF_bigram <- DF %>%
  
  select(comment) %>%
  
  unnest_tokens(output = bigram,
                input = comment,
                token = "ngrams",
                n = 2) %>%
  
  tidyr::separate(bigram,
                  c("word1", "word2"),
                  sep = " ") %>%
  
  filter(!word1 %in% stopwords$stopword) %>%
  filter(!word2 %in% stopwords$stopword) %>%
  
  count(word1,
        word2,
        sort = T) %>%
  
  tidyr::drop_na()



bigram_graph <- DF_bigram %>%
  filter(n >= 3) %>%
  graph_from_data_frame()



a <- grid::arrow(type = "closed",
                 length = unit(.15, "inches"))

ggraph(bigram_graph,
       layout = "fr") +
  
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = F,
                 arrow = a,
                 end_cap = circle(.07, 'inches')) +
  
  geom_node_point(color = "lightblue",
                  size = 4) +
  
  geom_node_text(aes(label = name),
                 vjust = 1, hjust = 1) +
  
  theme_void()







