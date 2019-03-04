library(pdftools)
library(tidyverse)
library(tidytext)
#para tener corpus en español
library(tm)
#para wordcloud
library(wordcloud)

#sentimientos en español
#traducidos
#https://rpubs.com/jboscomendoza/analisis_sentimientos_lexico_afinn
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

pdf2tidytext <- function(pdffile){
  text <- pdf_text(pdffile) 
  text <- text[text!=""]
  text <- sapply(text, function(x) read_lines(x))
  text_df <- lapply(text, function(x) tibble(text=x))
  text_df <- bind_rows(text_df)
  tidytext <- text_df %>% unnest_tokens(word,text)
  return(tidytext)
}

frecuencia_palabras <- function(tidytext,color){
  custom_stop_words <- data_frame(word = tm::stopwords("spanish"),lexicon = "custom")
  wc <- tidytext %>% anti_join(custom_stop_words) %>% dplyr::filter(!str_detect(word, "^[0-9]")) %>% count(word,sort=TRUE) %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50,colors = color))
  return(wc)
}

analisis_sentimientos <- function(tidytext){
  sentimientos <- tidytext %>% anti_join(custom_stop_words) %>% dplyr::filter(!str_detect(word, "^[0-9]")) %>% unnest_tokens(input = "word", output = "Palabra") %>% 
    inner_join(afinn, ., by = "Palabra")  %>% 
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% dplyr::rename(word=Palabra)
  l.sentimientos <- split(sentimientos,sentimientos$Tipo)
  wc.sent <- lapply(l.sentimientos, function(x) frecuencia_palabras(x,color))
}

