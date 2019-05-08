library(tidytext)
sentiments
str(sentiments)
get_sentiments("afinn")
get_sentiments("bing")
x <- c("ichchhayakant","sharma")
get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)
tidy_books <- austen_books() %>%
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                                ignore_case = T)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment=="joy")
nrcjoy

tidy_books %>%
  filter(book== "Emma")%>%
  inner_join(nrcjoy) %>%
  count(word, sort = T)

library(tidyr)
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book,index= linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment=positive-negative)
janeaustensentiment
library(ggplot2)
ggplot(janeaustensentiment,aes(index, sentiment, fill=book)+
       geom_col(show.legend  = F)+
       facet_wrap(~book, ncol = 2,scales = "free_x"))


pride_prejudice <- tidy_books %>%
  filter(book=="Pride & Prejudice")
pride_prejudice

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=linenumber%/% 80)%>%
  summarise(sentiment=sum(score)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method= "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("Positive",
                           "negative"))) %>%
  mutate(method="NRC")%>%
  count(method,index= linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment=positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill= method))+
  geom_col(show.legend = F)+
   facet_wrap(~method,ncol=1,scales = "free_y")



get_sentiments("nrc") %>% 
  filter(sentiment%in% c("positive","negative")) %>% 
  count(sentiment)
get_sentiments("bing")%>%
  count(sentiment)
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort=T) %>%
  ungroup()
bing_word_counts
bing_word_counts %>%
  group_by(sentiment)%>%
  top_n(10) %>%
  ungroup()%>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment))+ 
  geom_col(show.legend = F)+
  facet_wrap(~sentiment,scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+coord_flip()


custom_stop_words <- bind_rows(data_frame(word=c("miss"),
                                          lexicon=c("custom")),
                               stop_words)
custom_stop_words


library(wordcloud)
tidy_books %>%
  anti_join(stop_words)%>%
  count(word) %>%
  with(wordcloud(word,n,max.words = 100))
library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=T)%>%
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20","gray80"),
                   max.words = 100)


PandP_sentences <- data_frame(text=prideprejudice)%>%
  unnest_tokens(sentence, text,token="sentences")
PandP_sentences

austen_chapters <- austen_books()%>%
  group_by(book)%>%
  unnest_tokens(chapter, text,token="regex",
                pattern="Chapter|CHAPTER[\\dIVXLC]")%>%
  ungroup()
austen_chapters %>%
  group_by(book)%>%
  summarise(chapters=n())
bingnegative <- get_sentiments("bing")%>%
  filter(sentiment=="negative")
wordcounts <- tidy_books %>%
  group_by(book,chapter) %>%
  summarise(words=n())
tidy_books %>%
  semi_join(bingnegative)%>%
  group_by(book,chapter)%>%
  summarise(negativewords=n())%>%
  left_join(wordcounts, by=c("book","chapter"))%>%
  mutate(ratio =negativewords/words)%>%
  filter(chapter!=0) %>%
  top_n(1) %>%
  ungroup()



## Chapter 3
library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>%
  unnest_tokens(word,text) %>%
  count(book,word,sort=T)%>%
  ungroup()


total_words <- book_words %>%
  group_by(book)%>%
  summarize(total=sum(n))
book_words <- left_join(book_words, total_words)
book_words
library(ggplot2)
ggplot(book_words,aes(n/total,fill=book))+
  geom_histogram(show.legend = F)+
  xlim(NA,.0009)+
  facet_wrap(~book,ncol = 2,scales = "free_y")
freq_by_rank <-book_words %>%
  group_by(book) %>%
  mutate(rank=row_number(),
         'term frquency'=n/total)
freq_by_rank
freq_by_rank %>%
  ggplot(aes(rank,`term frquency`, color=book))+
  geom_line(size=1.1,alpha=0.8,show.legend = F)+
  scale_x_log10()+
  scale_y_log10()
rank_subset <- freq_by_rank %>%
  filter(rank<500,
         rank>10)
rank_subset
lm(log10(`term frquency`)~ log10(rank), data=rank_subset)

freq_by_rank %>%
  ggplot(aes(rank,`term frquency`,color=book))+
  geom_abline(intercept = -0.62,slope=-1.1, color="gray50", linetype=2)+
  geom_line(size=1.1, alpha=0.8,show.legend = T)+
  scale_x_log10()+
  scale_y_log10()


book_words <- book_words %>%
  bind_tf_idf(word,book,n)
book_words
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word= factor(word, levels = rev(unique(word)))) %>%
  group_by(book)%>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word,tf_idf,fill=book))+
  geom_col(show.legend = F)+
  labs(x=NULL, y="tf_idf")+
  facet_wrap(~book,ncol=2,scales = "free")+
  coord_flip()



library(gutenbergr)
physics <- gutenberg_download(c(37729,14725,13476,5001),
                              meta_fields = "author")
physics
physics_words<- physics %>%
  unnest_tokens(word, text) %>%
  count(author,word,sort=T)%>%
  ungroup()
physics_words
plot_physics <- physics_words %>%
  bind_tf_idf(word,author,n)%>%
  arrange(desc(tf_idf))%>%
  mutate(word=factor(word,levels=rev(unique(word)))) %>%
  mutate(author=factor(author,levels = c("Galilei, Galileo",
                                         "Huygens,Christiaan",
                                         "Tesla Nikola",
                                         "Einstein, Albert")))

plot_physics %>%
  group_by(author)%>%
  top_n(15,tf_idf) %>%
  ungroup() %>%
  mutate(word=reorder(word,tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend = F)+
  labs(x=NULL,y="td_idf")+
  facet_wrap(~author,ncol=2,scales="free")+
  coord_flip()
library(stringr)
physics %>%
  filter(str_detect(text,"eq\\.")) %>%
  select(text)

physics %>%
  filter(str_detect(text,"K1")) %>%
  select(text)

physics %>%
  filter(str_detect(text,"AK"))%>%
  select(text)
mystopwords <- data_frame(word=c("eq","co","rc","ac","ak","bn","fig"
                                 ,"file","cg","cb","cm"))
physics_words <- anti_join(physics_words,mystopwords,by="word")
plot_physics<- physics_words%>%
  bind_tf_idf(word,author,n)%>%
  arrange(desc(tf_idf))%>%
  mutate(word=factor(word, levels = rev(unique(word))))%>%
  group_by(author)%>%
  top_n(15,tf_idf) %>%
  ungroup %>%
  mutate(author=factor(author,levels = c("Galilei, Galileo",
                                         "Huygens, Christiaan",
                                         "Tesla, Nikola",
                                         "Einstein, Albert")))

ggplot(plot_physics,aes(word,tf_idf,fill=author))+
  geom_col(show.legend = F)+
  labs(x=NULL,y="tf_idf")+
  facet_wrap(~author,ncol=2,scales = "free")+
  coord_flip()




## Chapter 4
library(dplyr)
library(tidytext)
library(janeaustenr)
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram,text,token="ngrams",n=2)
austen_bigrams
austen_bigrams %>%
  count(bigram,sort=T)
library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram,c("word1","word2"), sep=" ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

## New bigram counts :
 bigram_counts <- bigrams_filtered %>%
count(word1,word2, sort = T)   
bigram_counts 


bigram_united <- bigrams_filtered %>%
  unite(bigram,word1,word2, sep=" ")

bigram_united

austen_books() %>%
  unnest_tokens(trigram, text,token="ngrams",n=3)%>%
  separate(trigram,c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word)%>%
         count(word1,word2,word3,sort=T)

bigrams_filtered %>%
  filter(word2=="street")%>%
  count(book,word1,sort=T)

bigram_tf_idf <- bigram_united %>%
  count(book,bigram) %>%
  bind_tf_idf(bigram,book,n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigrams_separated %>%
  filter(word1=="not")%>%
  count(word1,word2,sort=T)
AFINN <- get_sentiments("afinn")
AFINN
not_words <- bigrams_separated %>%
  filter(word1=="not") %>%
  inner_join(AFINN,by=c(word2="word")) %>%
  count(word2,score,sort=T)%>%
  ungroup()
not_words



library(ggplot2)
not_words %>%
  mutate(contribution=n*score)%>%
  arrange(desc(abs(contribution)))%>%
  head(20)%>%
  mutate(word2=reorder(word2,contribution))%>%
  ggplot(aes(word2,n*score, fill=n*score>0))+
  geom_col(show.legend = F)+
  xlab("Words preceded by \"not\"")+
  ylab("Sentiment score* number of occurrences")+
  coord_flip()


negation_words <- c("not","no","never","without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words)%>%
  inner_join(AFINN,by=c(word2="word")) %>%
  count(word1,word2,score,sort = T)%>%
  ungroup()

library(igraph)
bigram_counts
bigram_graph <- bigram_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()
bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph,layout="fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),vjust=1,hjust=1)


set.seed(2016)
a <- grid::arrow(type="closed", length=unit(.15,"inches"))

ggraph(bigram_graph, layout="fr")+
  geom_edge(aes(edge_alpha=n), show.legend=F,
            arrow=a,end_cap=circle(0.07,"inches"))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name), vjust=1,hjust=1)+
  theme_void()

library(tidytext)
ap_topics <- tidy(ap_lda,matrix="beta")
ap_topics
