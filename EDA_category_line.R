re_order_factor <- c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports", "Film & Animation", "Howto & Style", "Gaming", "Science & Technology", "Education", "Autos & Vehicles", "Nonprofits & Activism", "Shows", "Pets & Animals", "Movies", "Travel & Events", "Trailers")   
colorCategoryBrewer <-
  c("#FF00CC", "#CC0099", "#9900CC", "#FF0000",
    "#FF9933", "#FFFF00", "#99CC00", "#00CC99",
    "#003333", "#33FF66", "#3399FF", "#333399",
    "#666699", "#330099", "#996600", "#333366",
    "#000099", "#000066")

drawCategoryLine <- function(df, country_name, category_name) {
  temp_df <- subset(df, country == country_name)
  temp_df <- temp_df[c('trending_date', 'category_id', 'country')]
  temp_df$count <- c(1)
  
  ix <- match(category_name, re_order_factor)
  line_color <- colorCategoryBrewer[ix]
  temp_df <- with(subset(temp_df, category_id == category_name), aggregate(count, list(trending_date, category_id, country), sum))
  names(temp_df) <- c("date", "Category", "Country","Freq")
  temp_gg <- ggplot(temp_df, aes(x = date, y = Freq)) + geom_line(aes(color = Category, linetype = Country), size = 0.8) + xlab("") + ylab("")
  temp_gg <- temp_gg + scale_x_date(date_labels = "%y-%m-%d", date_breaks = "1 month", limit=c(as.Date("2017-11-14"),as.Date("2018-06-14")))
  temp_gg <- temp_gg + theme_minimal() + theme(axis.text.x=element_text(angle = 30, size = 9, face = "bold"), axis.text.y=element_text(size = 12)  ,legend.position = "bottom")
  temp_gg <- temp_gg + scale_color_manual(values = line_color)
  return(temp_gg)
}

line_CA <- drawCategoryLine(df, "Canada", "News & Politics")
line_CA

line_US <- drawCategoryLine(df, "USA", c("People & Blogs", "Gaming"))
line_US + ggtitle("<USA: 'People & Blogs' and 'Gaming'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

line_KR <- drawCategoryLine(df, "Korea", c("People & Blogs", "Gaming"))
line_KR + ggtitle("<Korea: 'People & Blogs' and 'Gaming'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))

line_KRandUS <- drawCategoryLine(df, c("Korea", "USA"), c("News & Politics"))
line_KRandUS

line_KRandUS + ggtitle("<Korea and USA: 'News & Politics'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
line_KR + ggtitle("<Korea: 'News & Politics' and 'Comedy'>") + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
