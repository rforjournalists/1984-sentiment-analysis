#load source data

txt <- read.delim('path/to/1984.txt', header = FALSE)

#create one huge character vector
cvec <- txt %>% as.character()

chapters <- cvec %>% strsplit('Chapter [0-9]') %>% as.data.frame()

#tidy up df
names(chapters) <- 'chapter'
chapters$chapter <- gsub('"','',chapters$chapter)
chapters$chapter <- gsub(',','',chapters$chapter)
chapters$chapter <- gsub('c\(','',chapters$chapter)
chapters$chapter <- trimws(chapters$chapter, which = 'both')

#remove parts and reorder chapter numbers
chapters$chapter <- gsub(' PART TWO','',chapters$chapter)
chapters$chapter <- gsub(' PART THREE','',chapters$chapter)
chapters <- chapters[-1,] %>% as.data.frame()
names(chapters) <- 'chapter'
chapters$chapter_no <- seq(1,23,1)

chapter_words <- data.frame()
chapter_and_word <- function(x,y) {
  x <- data.frame(text = x, chapter = y) %>% unnest_tokens(word, text) %>% anti_join(stop_words)
  chapter_words <<- rbind(chapter_words,x)
}

mapply(chapters$chapter, FUN = chapter_and_word, y = chapters$chapter_no)

#merge with sentiment
#use afinn
afinn <- get_sentiments('afinn')

#keep position of words in text
chapter_words$position <- row.names(chapter_words) %>% as.numeric()

#merge with sentiment
chapter_words <- merge(chapter_words,afinn, by = 'word', all.x = TRUE)
chapter_words <- arrange(chapter_words, position)
chapter_words$value[is.na(chapter_words$value)] <- 0

#create table of sentiment by chapter
chapter_sentiment <- chapter_words %>%
  group_by(chapter) %>% summarise(sentiment = sum(value))

#capitalise x and y axes
names(chapter_sentiment) <- c('Chapter','Sentiment')

#plot
ggplot() + geom_line(data = chapter_sentiment, aes(x = Chapter, y = Sentiment), size = 1.2) +
  geom_point(data = chapter_sentiment, aes(x = Chapter, y = Sentiment), size = 2.8) +
  annotate("text", x = 12, y = 50, label = "Winston finds \nMr Charrington's shop") +
  annotate("text", x = 15, y = -320, label = "The book reveals the true \nnature of the world") +
  annotate("text", x = 22, y = -300, label = "Winston is tortured\n in the Ministry of Love") +
  annotate("segment", x = 15.5, xend = 16.8, y = -350, yend = -350, size=1.3, arrow=arrow()) +
  annotate("segment", x = 21, xend = 19.2, y = -330, yend = -330, size=1.3, arrow=arrow()) +
  scale_x_continuous(breaks= seq(1,23,1), labels = seq(1,23,1)) + 
  scale_y_continuous(limits= c(-400,100)) + 
  labs(title = 'Sentiment in Nineteen Eighty-Four', subtitle = 'Higher scores mean more positive chapters of the novel', caption = 'Source: Project Gutenberg') +
  theme_minimal()
