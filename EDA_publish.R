colorVectors_publish_at <- brewer.pal(3, "Blues")

# pie graph
tb <- data.frame(table(df$publish_at))
names(tb) <- c("Publish_Time", "Freq")
tb$fraction <- tb$Freq / sum(tb$Freq)
tb$ymax <- cumsum(tb$fraction)
tb$ymin <- c(0, head(tb$ymax, n=-1))
tb$per <- with(tb, paste0(round(fraction * 100, 1), "%"))

pie <- ggplot(tb, aes(x = "", y = Freq, fill = Publish_Time)) + geom_bar(stat = "identity")
pie <- pie + coord_polar("y") + theme_void()
pie <- pie + scale_fill_brewer(name = "Publish At", palette = "Blues", labels = c("Off Time", "Work Time", "Sleep Time")) + theme(legend.title = element_text(size = 18, face = "plain"))
pie <- pie + theme(title = element_blank(), legend.position = "right", plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 12))
pie <- pie + theme(plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")) + theme(legend.box.margin = margin(-5, 0, -5, -10, "mm"))
pie <- pie + geom_text(aes(label = per),color = c("#3182BD", "white", "white"), position = position_stack(vjust = 0.5),size = 4) # + theme(text = element_text(size = 18))
pie

pie_legend <- get_legend(pie)
ggdraw(pie_legend)
pie <- pie + ggtitle("Total") + theme(legend.position = "none", title = element_text(size = 12, face = "bold"), plot.title = element_text(hjust = 0.2, vjust = -1.0), plot.margin = unit(c(-2.5,-2.5,-2.5,-2.5), "mm"))


drawPublishPie <- function(df, country_name) {
  tb <- data.frame(table(subset(df, country == country_name)$publish_at))
  names(tb) <- c("Publish_Time", "Freq")
  tb$fraction <- tb$Freq / sum(tb$Freq)
  tb$ymax <- cumsum(tb$fraction)
  tb$ymin <- c(0, head(tb$ymax, n=-1))
  tb$per <- with(tb, paste0(round(fraction * 100, 1), "%"))
  
  pie<- ggplot(tb, aes(x = "", y = Freq, fill = Publish_Time)) + geom_bar(stat = "identity")
  pie <- pie + coord_polar("y") + theme_void() + ggtitle(country_name)
  pie <- pie + scale_fill_brewer(name = "Publish At", palette = "Blues", labels = c("Off Time", "Work Time", "Sleep Time")) + theme(legend.title = element_text(size = 18, face = "plain"))
  pie <- pie + theme(title = element_text(size = 10, face="bold"), legend.position = "none", plot.title = element_text(hjust = 0.5))
  pie <- pie + theme(plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")) 
  pie + geom_text(aes(label = per),color = c("#3182BD", "white", "white"), position = position_stack(vjust = 0.5),size = 6) # + theme(text = element_text(size = 18))
  return(pie)
}

pie_CA <- drawPublishPie(df, "Canada")
pie_DE <- drawPublishPie(df, "Germany")
pie_FR <- drawPublishPie(df, "France")
pie_GB <- drawPublishPie(df, "UK")
pie_IN <- drawPublishPie(df, "India")
pie_JP <- drawPublishPie(df, "Japan")
pie_KR <- drawPublishPie(df, "Korea")
pie_MX <- drawPublishPie(df, "Mexico")
pie_RU <- drawPublishPie(df, "Russia")
pie_US <- drawPublishPie(df, "USA")

grid.arrange(pie, pie_legend, pie_CA, pie_DE, pie_FR, pie_GB, pie_IN, pie_JP, pie_KR, pie_MX, pie_RU, pie_US, widths = widths_v, heights = heights_v, layout_matrix = lay, top = textGrob("<Publish Time Distribution per each Country>\n", gp=gpar(fontsize=24, fontface = "bold.italic")))
