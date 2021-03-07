drawScatter2 <- function(df, country_name=NULL, numeric_name1, numeric_name2, numeric_name3, category_name=NULL, elipse_on = TRUE) {
  if (is.null(country_name)) {
    temp_df <- df
  }
  else {
    temp_df <- subset(df, country == country_name)
  }
  
  if (!is.null(category_name)) {
    temp_df <- subset(temp_df, category_id == category_name)
  }
  
  temp_df <- temp_df[c(numeric_name1, numeric_name2, numeric_name3)]
  names(temp_df) <- c("x", "y1", "y2")
  temp_df$x <- scale(log10(temp_df$x+1))
  temp_df$y1 <- scale(log10(temp_df$y1+1))
  temp_df$y2 <- scale(log10(temp_df$y2+1))
  
  temp_df <- reshape(data = temp_df,
                          varying = 2:3,
                          v.names = "y",
                          direction = "long",
                          timevar = "value",
                          times = c(numeric_name2, numeric_name3))
  temp_df$value <- factor(temp_df$value)
  
  temp_gg <- ggplot(temp_df, aes(x = x, y = y, color = value, fill = value)) + geom_point(alpha = 0.3) + theme_minimal()
  if (elipse_on) {
    temp_gg <- temp_gg + stat_ellipse(geom = "polygon", type = "norm", alpha = 0.2, color = NA)
  }

  x_lim = c(-3.0,5.0)
  y_lim = c(-3.0,5.0)

  temp_gg <- temp_gg + xlab(numeric_name1) + ylab("") + coord_cartesian(xlim = x_lim, ylim = y_lim)
  temp_gg <- temp_gg + theme(legend.position = c(0.15,0.9), legend.background = element_rect(fill = "white", color = "black"),legend.direction = "horizontal")
  
  return(temp_gg)
}

select <- "People & Blogs"

scatter_KR2 <- drawScatter2(df, country_name = "Korea", "views", "likes", "dislikes", select, elipse_on = FALSE)
scatter_KR2 + ggtitle(sprintf("<Korea: '%s'>", select)) + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

