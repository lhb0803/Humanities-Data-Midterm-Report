library(hexbin)

drawScatter <- function(df, country_name=NULL, numeric_name1, numeric_name2, category_name=NULL, elipse_on = TRUE, se_on = TRUE) {
  if (is.null(country_name)) {
    temp_df <- df
  }
  else {
    temp_df <- subset(df, country == country_name)
  }
  
  if (is.null(category_name)) {
    temp_df <- temp_df[c(numeric_name1, numeric_name2)]
    names(temp_df) <- c("value1", "value2")
    temp_df$value1 <- log10(temp_df$value1+1)
    temp_df$value2 <- log10(temp_df$value2+1)
    temp_gg <- ggplot(temp_df, aes(x = value1, y = value2)) + geom_point(color = "black", alpha = 0.1) + theme_minimal()
    temp_gg <- temp_gg + theme(legend.position = c(0.1,0.9), legend.background = element_rect(fill = "white", color = "black"),legend.direction = "horizontal")
  }
  else {
    temp_df <- temp_df[c(numeric_name1, numeric_name2, "category_id")]
    temp_df <- subset(temp_df, category_id == category_name)
    names(temp_df) <- c("value1", "value2", "Category")
    temp_df$value1 <- log10(temp_df$value1+1)
    temp_df$value2 <- log10(temp_df$value2+1)
    ix <- match(category_name, re_order_factor)
    line_color <- colorCategoryBrewer[ix]
    temp_df$Category <- factor(temp_df$Category)
    
    if (length(category_name) == 1) {
      temp_gg <- ggplot(temp_df, aes(x = value1, y = value2, color = Category, fill = Category)) + geom_point(alpha = 0.1) + stat_smooth(method = loess, se = se_on) + theme_minimal() 
    }
    else {
      temp_gg <- ggplot(temp_df, aes(x = value1, y = value2, color = Category, fill = Category)) + geom_point(alpha = 0.2) + theme_minimal() 
      if (elipse_on) {
        temp_gg <- temp_gg + stat_ellipse(geom = "polygon", type = "norm", alpha = 0.2, color = NA)
      }
    }

    temp_gg <- temp_gg + scale_color_manual(values = line_color) + scale_fill_manual(values = line_color) + theme(legend.position = c(0.1,0.9), legend.background = element_rect(fill = "white", color = "black"),legend.direction = "vertical")
  }
  
  if (numeric_name1 == "comment_count") {
    x_name = "comments"
  } else {
    x_name = numeric_name1
  }
  if (numeric_name2 == "comment_count") {
    y_name = "comments"
  } else {
    y_name = numeric_name2
  }
  
  temp_gg <- temp_gg + xlab(x_name) + ylab(y_name)
  
  return(temp_gg)
}

scatter <- drawScatter(df, numeric_name1 = "likes", numeric_name2 = "dislikes",category_name=c("People & Blogs", "Pets & Animals"))
scatter + ggtitle("<Total: 'likes' and 'dislikess'\n at 'People & Blogs and Pets & Animals'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_CA <- drawScatter(df, country_name = "Canada", "views", "likes", c("News & Politics", "Comedy"))
scatter_CA

scatter_KR <- drawScatter(df, country_name = "Korea", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_KR + ggtitle("<Korea: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_US <- drawScatter(df, country_name = "USA", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_US + ggtitle("<USA: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
                                                                                                                                                    
scatter_GB <- drawScatter(df, country_name = "UK", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_GB + ggtitle("<UK: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_RU <- drawScatter(df, country_name = "Russia", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_RU + ggtitle("<Russia: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_JP <- drawScatter(df, country_name = "Japan", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_JP + ggtitle("<Japan: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_MX <- drawScatter(df, country_name = "Mexico", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_MX + ggtitle("<Mexico: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_IN <- drawScatter(df, country_name = "India", "likes", "comment_count", c("People & Blogs", "Pets & Animals"))
scatter_IN + ggtitle("<India: 'likes' and 'comments'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_FR <- drawScatter(df, country_name = "France", "likes", "dislikes", c("People & Blogs", "Pets & Animals"))
scatter_FR + ggtitle("<France: 'likes' and 'dislikes'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

scatter_JP <- drawScatter(df, country_name = "Japan", "views", "comment_count", c("People & Blogs", "Pets & Animals"))
scatter_JP + ggtitle("<Japan: 'views' and 'comments'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))


