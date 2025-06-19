#Preparing data
data1<- read.csv("C:/Users/edlsx/Desktop/retour entretien - sondés.csv")
data<-data1[,7:11]
write.table(data[,1],"C:/Users/edlsx/Desktop/ukrain/sense.txt", sep="", row.names=FALSE, , quote=FALSE)
write.table(data[,2],"C:/Users/edlsx/Desktop/ukrain/experience.txt", sep="", row.names=FALSE, , quote=FALSE)
write.table(data[,3],"C:/Users/edlsx/Desktop/ukrain/unrolled.txt", sep="", row.names=FALSE, , quote=FALSE)
write.table(data[,4],"C:/Users/edlsx/Desktop/ukrain/difficulties.txt", sep="", row.names=FALSE, , quote=FALSE)
write.table(data[,5],"C:/Users/edlsx/Desktop/ukrain/improvement.txt", sep="", row.names=FALSE, , quote=FALSE)


# Attach R packages -------------------------------------------------------

if (!require(tidytext)) install.packages("tidytext")
if (!require(textdata)) install.packages("textdata")
if (!require(tidyverse)) install.packages("tidyverse")

library(tidytext) # text mining using tidy tools
library(textdata) # get sentiment lexicons
library(tidyverse)

# Get data ----------------------------------------------------------------

sense_raw <- readr::read_lines(file = "C:/Users/edlsx/Desktop/ukrain/sense.txt", skip_empty_rows = TRUE, skip = 1) # remove appendix
sense_raw <- c("Chapter 1",sense_raw)
experience_raw<-readr::read_lines(file = "C:/Users/edlsx/Desktop/ukrain/experience.txt", skip_empty_rows = TRUE, skip = 1) # remove appendix
experience_raw <- c("Chapter 2",experience_raw)
unrolled_raw<-readr::read_lines(file = "C:/Users/edlsx/Desktop/ukrain/unrolled.txt", skip_empty_rows = TRUE, skip = 1) # remove appendix
unrolled_raw<-c("Chapter 3",unrolled_raw)
difficulties_raw<-readr::read_lines(file = "C:/Users/edlsx/Desktop/ukrain/difficulties.txt", skip_empty_rows = TRUE, skip = 1) # remove appendix
difficulties_raw<-c("Chapter 4",difficulties_raw)
improvement_raw<-readr::read_lines(file = "C:/Users/edlsx/Desktop/ukrain/improvement.txt", skip_empty_rows = TRUE, skip = 1) # remove appendix
improvement_raw<-c("Chapter 5",improvement_raw)
text_raw<-c(sense_raw,experience_raw,unrolled_raw,difficulties_raw,improvement_raw)
head(text_raw)
# Create chapter variable -------------------------------------------------

text_df <- tibble(text = text_raw) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [1-5]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [1-5]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 7)

text_df


# Get sentiment lexicon ---------------------------------------------------

bing <- tidytext::get_sentiments("bing")
afinn <- tidytext::get_sentiments("afinn")
nrc <- tidytext::get_sentiments("nrc")


# Sentiment Lexicons Limitations ------------------------------------------

text_pos <- "This is my favorite book. I like it."
text_neg <- "This is not my favorite book. I don't like it."

df_limitations <- tibble(
  text = c(text_pos, text_neg),
  examples = c("text_pos", "text_neg")
)

df_limitations %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word", relationship = "many-to-many") %>%
  inner_join(bing, by = "word", relationship = "many-to-many")


# Tokenize and join lexicons ----------------------------------------------

text_df_bing <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word", relationship = "many-to-many")

text_df_nrc <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word",relationship = "many-to-many")

text_df_afinn <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word", relationship = "many-to-many")


# General statistics --------------------------------------------

# number of words by pos/neg binary sentiment
A<-text_df_bing %>%
  count(sentiment, sort = TRUE)

# number of words by emotion
B<-text_df_nrc %>%
  count(sentiment, sort = TRUE)

# overall sentiment score
C<-text_df_afinn %>%
  summarise(overall_score = sum(value, na.rm = TRUE))


# Top words by emotion --------------------------------------------------

D<-text_df_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Negative/Positive Words in Volunteers interview",
       subtitle = "Top 10 words, BING lexicon",
       x = "Number of words",
       y = NULL)

E<-text_df_nrc %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(!word %in% c("words", "feeling")) %>% #remove tokens
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Emotional Words in Volunteers Interview",
       subtitle = "Top 5 words, NRC lexicon",
       x = NULL, y = NULL,
       caption = "Edouard Lansiaux")


# Emotional score by question ----------------------------------------------

text_df_afinn_chapter_score <- text_df_afinn %>%
  group_by(chapter) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

G<-text_df_afinn_chapter_score %>%
  mutate(positive = sentiment > 0,
         chapter = as.factor(chapter)) %>%
  ggplot(aes(chapter, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Sentiment analysis of Volunteer Interview",
       subtitle = "Score by question, afinn lexicon",
       caption = "Edouard Lansiaux")


#Visualisation
D
E
G
