# Coletando dados da API do Twitter

# pacotes

require(install.load)
install_load('twitteR','igraphdata','dplyr','rtweet', 'ggplot2', 'tidytext','tm', 'igraph', 'ggraph', 'tidyr', 'widyr')

# chaves

consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
appname <- ''

setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)

# query

query <- "((Digital humanities)OR(Nova de Lisboa)OR(História)OR(Instituto)OR(humanidades)OR(digitais))"

# request

request_twitter <- searchTwitter(query, n=5000)

# request com lat long

request_twitter <- searchTwitter(query, n=100,sinceID = NULL, lang='pt',  geocode='35.8421992,-36.8566103, 10000km',retryOnRateLimit=10000)

# list to df

request_twitter <- twListToDF(request_twitter)

# Verificando os dados adquiridos

head(request_twitter$text)

# Removendo os http 

request_twitter$stripped_text <- gsub("http.*","",  request_twitter$text)
request_twitter$stripped_text <- gsub("https.*","", request_twitter$stripped_text)

# Removendo a pontuacao

top_15 <- request_twitter %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# top 15 palavras

top_15 %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Contagem",
       y = "Palavras únicas",
       title = "Contagem de palavras únicas encontradas nos tweets")

# Lista de stops words

data("stop_words")

# Removendo palavras que não são relevantes

cleaned_tweet_words <- top_15 %>%
  anti_join(stop_words)

# top 15 palavras limpa

top_15 %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Contagem",
       y = "Palavras únicas",
       title = "Contagem de palavras únicas encontradas nos tweets")

# Bigrama

bigram <- request_twitter %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

bigram %>%
  count(paired_words, sort = TRUE)

# separando o bigrama em duas colunas

bigram_separated_words <- bigram %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

search_filtered <- bigram_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Nova contagem de bigram

words_counts <- search_filtered %>%
  count(word1, word2, sort = TRUE)

# plotando o grafo 

words_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(color = "red", aes(label = name), vjust = 1.8, size=3) +
  labs(title= "Grafo de Palavras: Humanidades Digitais ",
       subtitle = "Text mining de dados ",
       x = "", y = "")


geo <- lat_lng(request_twitter)

## plotando no mapa

par(mar = c(0,0,0,0))
maps::map("portugal",  lwd = 0.75)
with(geo, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# salvando

write.csv(request_twitter, 'twitter_Humanidades_Digitais.csv')
get_retweeted <- function(text){
  if(grepl('(RT @\\S\\w+:)', text)){
    pos <- regexpr('(RT @\\S\\w+:)', text)
    name <- substr(text,5,attr(pos,"match.length")-1)
    return(name)
  }
}

edges <- request_twitter %>% transmute(source = ifelse(isRetweet, get_retweeted(text),screenName),target = screenName)
write.csv(edges,'edges_Humanidades_Digitais.csv', row.names = F)
