drawBox <- function(df, country_name=NULL, category_name=NULL) {
  if (is.null(country_name)) {
    temp_df <- df
  }
  else {
    temp_df <- subset(df, country == country_name)
  }

  temp_df <- temp_df[c("views", "likes", "dislikes", "comment_count", "category_id")]
  if (!is.null(category_name)) {
    temp_df <- subset(temp_df, category_id == category_name)
  }
  temp_df <- reshape(data = temp_df, 
                     varying = 1:4,
                     v.names = "value",
                     direction = "long",
                     timevar = "type",
                     times = c("views", "likes", "dislikes", "comments"))
  
  temp_df <- temp_df[c("type", "value", "category_id")]
  names(temp_df) <- c("type", "value", "Category")
  temp_df$value <- log10(temp_df$value+1)
  temp_df$Category <- factor(temp_df$Category)
  temp_gg <- ggplot(temp_df, aes(x = Category, y = value)) + geom_boxplot(fill='slategrey',color='darkslategrey', width=0.5) + coord_flip() 
  temp_gg <- temp_gg + facet_wrap(~type, ncol = 4) + theme(panel.background = element_rect(size = 0.01, fill = "white", color = "black"), panel.grid = element_blank(), 
                                                           strip.background = element_blank(),
                                                           strip.text.x = element_text(face = "bold"))
  
  temp_gg <- temp_gg + xlab("") + ylab("") 
  temp_gg <- temp_gg + theme(legend.position = "right", legend.background = element_rect(fill = "white", color = "black"),legend.direction = "vertical")
  
  return(temp_gg)
}

box <- drawBox(df, category_name = c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports"))
box + ggtitle("<Box plots of views, likes, dislikes and comments>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

