drawHistogram <- function(df, country_name=NULL, numeric_name, category_name=NULL, position_name = "stack") {
  if (is.null(country_name)) {
    temp_df <- df
  }
  else {
    temp_df <- subset(df, country == country_name)
  }
  
  if (is.null(category_name)) {
    temp_df <- temp_df[c(numeric_name)]+1
    temp_df <- data.frame(apply(temp_df, 1, log10))
    names(temp_df) <- c("value")
    temp_gg <- ggplot(temp_df, aes(x = value)) + geom_histogram(fill = "gray", color = "darkgray", alpha = 0.8, bins = 50) + theme_minimal()
    temp_gg
  }
  else {
    temp_df <- temp_df[c(numeric_name, "category_id")]
    temp_df <- subset(temp_df, category_id == category_name)
    names(temp_df) <- c("value", "Category")
    temp_df$value <- log10(temp_df$value+1)
    ix <- match(category_name, re_order_factor)
    line_color <- colorCategoryBrewer[ix]
    temp_df$Category <- factor(temp_df$Category)
    
    temp_gg <- ggplot(temp_df, aes(x = value, color = Category, fill = Category)) + geom_histogram(alpha = 0.8, bins = 50, position = position_name) + theme_minimal() 
    temp_gg <- temp_gg + scale_color_manual(values = line_color) + scale_fill_manual(values = line_color) + theme(legend.position = c(0.9,0.65), legend.background = element_rect(fill = "white", color = "black"),legend.direction = "vertical")
  }
  
  if (numeric_name == "comment_count") {
    x_name = "comments"
    x_lim = c(0,6)
  }
  else {
    x_name = numeric_name
    if (numeric_name == "views") {
      x_lim = c(3,8)
    }
    else if (numeric_name == "likes") {
      x_lim = c(0,7)
    }
    else if (numeric_name == "dislikes") {
      x_lim = c(0,6)
    }
  }
  
  temp_gg <- temp_gg + xlab(x_name) + ylab("") + coord_cartesian(xlim = x_lim)
  temp_gg <- temp_gg + theme(axis.title.x = element_text(face = "bold", angle = 0, size = 16, hjust = 1.0))
  
  return(temp_gg)
}

hist <- drawHistogram(df, , numeric_name = "dislikes", category_name = c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports"), position_name = "identity")
hist

hist_CA <- drawHistogram(df, country_name = "Canada", numeric_name = "views")
hist_CA

hist_CA_category <- drawHistogram(df, country_name = "Canada", numeric_name = "views", category_name = "Sports")
hist_CA_category

hist_USA <- drawHistogram(df, country_name = "USA", numeric_name = "views")
hist_USA

hist_KR <- drawHistogram(df, country_name = "Korea", numeric_name = "dislikes")
hist_KR+ ggtitle("<Korea: 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

select <- "dislikes"

hist_KR_category <- drawHistogram(df, country_name = "Korea", numeric_name = select, category_name = c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports"), position_name = "identity")
hist_KR_category + ggtitle(sprintf("<Korea: '%s'>", select)) + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

hist <- drawHistogram(df, numeric_name = "views", category_name = c("Entertainment", "People & Blogs", "News & Politics", "Comedy", "Film & Animation"))
hist <- drawHistogram(df, numeric_name = select, category_name = c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports"), position_name = "identity")
hist+ ggtitle(sprintf("<Total: '%s'>", select)) + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
