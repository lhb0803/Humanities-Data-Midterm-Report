library(ggplot2)
library(cowplot)
library(gridExtra)

# donut graph
tb <- data.frame(table(df$category_id))
names(tb) <- c("Category", "Freq")
tb$fraction <- tb$Freq / sum(tb$Freq)
tb$ymax <- cumsum(tb$fraction)
tb$ymin <- c(0, head(tb$ymax, n=-1))

donut <- ggplot(tb, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,fill=Category)) + geom_rect()
donut <- donut + coord_polar(theta="y") + xlim(c(2, 4)) + theme_void() + scale_fill_manual(values = colorCategoryBrewer)
donut <- donut + theme(title = element_text(size = 24, face = "bold.italic"),legend.position = "right", plot.title = element_text(hjust = 0.5))
donut <- donut + theme(legend.text = element_text(size = 8, face = "plain"))
donut <- donut + theme(legend.title = element_text(size = 12, face = "bold"))
donut <- donut + theme(plot.margin = unit(c(0,0,0,0), "mm")) + theme(legend.box.margin = margin(-5, -3, -5, -6, "mm"))
donut

donut_legend <- get_legend(donut)
ggdraw(donut_legend)
donut <- donut + ggtitle("Total") + theme(legend.position = "none", title = element_text(size = 12, face = "bold"), plot.title = element_text(hjust = 0.2, vjust = -1.0), plot.margin = unit(c(-2.5,-2.5,-2.5,-2.5), "mm"))

drawcategoryDonut <- function(df, country_name) {
  tb <- data.frame(table(subset(df, country == country_name)$category_id))
  names(tb) <- c("Category", "Freq")
  tb$fraction <- tb$Freq / sum(tb$Freq)
  tb$ymax <- cumsum(tb$fraction)
  tb$ymin <- c(0, head(tb$ymax, n=-1))
  donut <- ggplot(tb, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,fill=Category)) + geom_rect() + ggtitle(country_name)
  donut <- donut + coord_polar(theta="y") + xlim(c(2, 4)) + theme_void() + scale_fill_manual(values = colorCategoryBrewer)
  donut <- donut + theme(title = element_text(size = 10, face = "bold"), plot.title = element_text(hjust = 0.2))
  donut <- donut + theme(legend.title = element_blank())
  donut <- donut + theme(legend.position = "none") + theme(plot.margin = unit(c(-2.5,-2.5,-2.5,-2.5), "mm"))
  donut + theme(legend.title = element_text(size = 20, face = "plain"), legend.position = "left", plot.margin = unit(c(0.2,0.2,0.2,0.2), "mm"), title = element_text(size = 20, face = "bold"), plot.title = element_text(hjust = 0.3))
  return(donut)
}

donut_CA <- drawcategoryDonut(df, "Canada")
donut_DE <- drawcategoryDonut(df, "Germany")
donut_FR <- drawcategoryDonut(df, "France")
donut_GB <- drawcategoryDonut(df, "UK")
donut_IN <- drawcategoryDonut(df, "India")
donut_JP <- drawcategoryDonut(df, "Japan")
donut_KR <- drawcategoryDonut(df, "Korea")
donut_MX <- drawcategoryDonut(df, "Mexico")
donut_RU <- drawcategoryDonut(df, "Russia")
donut_US <- drawcategoryDonut(df, "USA")

heights_v <- c( 2, 1, 2, 1, 2) 
widths_v <-  c( 2, 2, 2, 2, 1)
lay <- rbind(c( 3, 4, 5, 6, 2),
             c(NA,NA, 1, 1, 2),
             c( 7, 8, 1, 1, 2),
             c(NA,NA, 1, 1, 2),
             c( 9,10,11,12, 2))

panel <- list(donut, donut_CA, donut_DE, donut_FR, donut_GB, donut_IN, donut_JP, donut_KR, donut_MX, donut_RU, donut_US)

grid.arrange(donut, donut_legend, donut_CA, donut_DE, donut_FR, donut_GB, donut_IN, donut_JP, donut_KR, donut_MX, donut_RU, donut_US, widths = widths_v, heights = heights_v, layout_matrix = lay, top = textGrob("<Category Distribution per each Country>\n", gp=gpar(fontsize=24, fontface = "bold.italic")))
