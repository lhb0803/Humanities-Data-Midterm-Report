library(wordcloud)
library(extrafont)
word_df <- subset(df, country == "USA")
#
tag_list <- with(word_df, strsplit(x = tags, split = "[\\|,#]+"))
tag_list <- unlist(tag_list)

write.csv(tag_list, "data/tag_US.csv", row.names = FALSE)

tag_df <- read.csv("data/text.txt", na.strings = "[none]", colClasses = c("NULL","character"))
head(tag_df)
tail(tag_df)

wordcount <- table(tag_df$x)
head(wordcount)
tail(wordcount)

word_df <- as.data.frame(wordcount, stringAsFactors = FALSE)
head(word_df)
names(word_df) <- c("word", "Freq")

word_df <- subset(word_df, Freq >= 2)
word_df <- subset(word_df, !grepl(pattern = "<", x = word, fixed = FALSE))

pal <- brewer.pal(8, "PuOr")
par(mar = rep(0, 4))
word_cloud <- wordcloud(words = word_df$word,
                        freq = word_df$Freq,
                        min.freq = 10,
                        max.words = 800,
                        random.order = FALSE,
                        rot.per = .1,
                        scale = c(4, 0.3),
                        colors = pal,
                        family = "serif")


