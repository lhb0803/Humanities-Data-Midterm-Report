drawPieDisable <- function(df, country_name = NULL, category_name = NULL, disable_which = c("comments_disabled", "ratings_disabled")) {
  if (is.null(country_name)) {
    temp_df <- df
  }
  else {
    temp_df <- subset(df, country == country_name)
  }
  
  if (!is.null(category_name)) {
    temp_df <- subset(temp_df, category_id == category_name)
  }
  
  temp_df <- temp_df[c("country", "category_id", disable_which)]
  names(temp_df) <- c("Country", "Category", "Comment", "Rating")
  
  tb_1 <- data.frame(table(temp_df$Comment))
  names(tb_1) <- c("Bool", "Comment")
  tb_2 <- data.frame(table(temp_df$Rating))
  names(tb_2) <- c("Bool", "Rating")
  tb <- merge(tb_1, tb_2, by = "Bool")
  
  comment_disable_per <- paste0(round(tb$Comment[2] / sum(tb$Comment) * 100,2), "%")
  rating_disable_per <- paste0(round(tb$Rating[2] / sum(tb$Rating) * 100,2), "%")
  
  comment_gg <- ggplot(tb, aes(x = "", y = Comment, fill = Bool)) + geom_bar(stat = "identity") 
  comment_gg <- comment_gg + coord_polar("y") + theme_void() + scale_fill_brewer(name = "Comment", palette = "RdPu") + theme(legend.position = "bottom")
  comment_gg <- comment_gg + geom_text(aes(y = cumsum(Comment) - 0.5*Comment, label = c(NA,comment_disable_per)), size = 4)
  
  rating_gg <- ggplot(tb, aes(x = "", y = Rating, fill = Bool)) + geom_bar(stat = "identity")
  rating_gg <- rating_gg + coord_polar("y") + theme_void() + scale_fill_brewer(name = "Rating", palette = "Purples") + theme(legend.position = "bottom")
  rating_gg <- rating_gg + geom_text(aes(y = cumsum(Comment) - 0.5*Comment, label = c(NA,rating_disable_per)), size = 4)
  
  if (is.null(category_name)) {
    category_name = "Total"
  }
  
  temp_gg <- grid.arrange(comment_gg, rating_gg,
                          top = textGrob(sprintf("%s", category_name, size = 16)),
                          ncol = 2)
  
  return(temp_gg) 
}
disable_which = c("comments_disabled", "ratings_disabled")

pie_disable_news <- drawPieDisable(df, category_name = c("News & Politics"))
pie_disable_news
pie_disable_music <- drawPieDisable(df, category_name = c("Music"))
pie_disable_music
pie_disable_people <- drawPieDisable(df, category_name = c("People & Blogs"))
pie_disable_people

pie_disable_news_kr <- drawPieDisable(df, country_name = "Korea",category_name = c("News & Politics"))
pie_disable_news_kr

pie_disable <- drawPieDisable(df)
pie_disable

pie(tb$Comment)
pie(tb$Rating)
