numeric_name1 = "views"
numeric_name2 = "likes"
numeric_name3 = "dislikes"

temp_df <- subset(df, country == "Korea")
temp_df <- subset(temp_df, category_id == "News & Politics")

temp_df <- temp_df[c(numeric_name1, numeric_name2, numeric_name3)]
names(temp_df) <- c("x", "y1", "y2")
temp_df$x <- scale(log10(temp_df$x+1))
temp_df$y1 <- scale(log10(temp_df$y1+1))
temp_df$y2 <- scale(log10(temp_df$y2+1))

temp_gg <- (ggplot(temp_df) + geom_point(aes(x = x, y = y1), color = as.character("#00BFC4"), alpha = 0.3) +
                              stat_smooth(aes(x = x, y = y2), color = as.character("#F8766D"), method = lm, se = FALSE) ) + theme_minimal()
temp_gg <- temp_gg + xlab(numeric_name1) + ylab(numeric_name2)
temp_gg + theme(legend.position = c(0.15,0.9), legend.background = element_rect(fill = "white", color = "black"),legend.direction = "horizontal")
