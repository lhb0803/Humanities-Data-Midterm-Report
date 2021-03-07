
#               video_id  trendint_date  title  chnnel_title  category_id  time       tag         view       likes   dislikes   comment_count  thumbnail_link                       description
colClasses_vec <- c("NULL", "character", "NULL", "character","factor", "character", "character", "numeric", "numeric", "numeric", "numeric", "NULL", "logical", "logical", "logical", "NULL" )
raw <- read.csv("data/KRvideos.csv", encoding = 'UTF-8', colClasses = colClasses_vec)

raw$trending_date <- format(as.Date(raw$trending_date, "%Y.%d.%m"), "20%y-%m-%d")
raw$trending_date <- as.Date(raw$trending_date, "20%y-%m-%d")
raw$publish_time <- format(as.POSIXct(raw$publish_time, tz = "GMT", format = c("%Y-%m-%dT%H:%M:%OSZ")), "20%y-%m-%dT%H:%M")
raw$publish_time <- as.POSIXct(raw$publish_time, tz = "GMT", "20%y-%m-%dT%H:%M")
raw$publish_time <- raw$publish_time + hours(9)

summary(raw)

raw$channel_title[raw$category_id == 29]
raw$category_id <- factor(raw$category_id, labels = c('Film & Animation', 
                                                      'Music',
                                                      'Pets & Animals',
                                                      'Sports',
                                                      'Travel & Events',
                                                      'Autos & Vehicles',
                                                      'Gaming',
                                                      'People & Blogs',
                                                      'Comedy',
                                                      'Entertainment',
                                                      'News & Politics',
                                                      'Howto & Style',
                                                      'Education',
                                                      'Science & Technology',
                                                      'Nonprofits & Activism',
                                                      'Shows',
                                                      'Trailers'))
raw$category_id

# numeric data
hist(log10(raw$views))
hist(log10(raw$likes))
hist(log10(raw$dislikes))
hist(log10(raw$comment_count))

# categoric data
par(mfrow = c(1,1), mar = c(5.1, 12, 4.1, 2.1))
boxplot(log10(views) ~ category_id, raw, horizontal = TRUE, las = 2, ylab = "")
par(mfrow = c(1,1), mar = c(5.1, 5.1, 4.1, 2.1))
with(raw, plot(category_id))
plot(log10(views) ~ log10(likes), raw, col = category_id)

# count per trending date
with(raw[raw$category_id == "Entertainment",], barplot(table(trending_date)))
with(raw[raw$category_id == "News & Politics",], barplot(table(trending_date)))
with(raw[raw$category_id == "People & Blogs",], barplot(table(trending_date)))

# count per publish time
publish_1 <- with(raw, raw[hour(publish_time) >= 2 & hour(publish_time) < 10 , ] )
publish_2 <- with(raw, raw[hour(publish_time) >= 10 & hour(publish_time) < 18 , ] )
publish_3 <- with(raw, raw[hour(publish_time) >= 18 | hour(publish_time) < 2 , ] )

plot(log10(views) ~ log10(likes), publish_1, col = category_id)

publish_1$publish_at <- c("Sleep_Time")
publish_2$publish_at <- c("Work_Time")
publish_3$publish_at <- c("Off_Time")

df <- rbind(rbind(publish_1, publish_2), publish_3)
df$publish_at <- factor(df$publish_at)
summary(df)
plot(log10(views) ~ log10(likes), df, col = publish_at)
mosaicplot(xtabs(~publish_at + category_id, df),las = c(1,2))
boxplot(log10(views) ~ publish_at, df, horizontal = TRUE)

plot(log10(dislikes) ~ log10(likes), df, col = category_id)


plot(log10(dislikes) ~ log10(views), df, col = category_id)
legend(x = "topleft", legend = levels(df$category_id), cex = 0.7, lwd = 3)

colorVectors_category_id <- c(brewer.pal(12, "Paired"), (brewer.pal(5, "Dark2")))
# colorVectors_category_id <- colorRampPalette(colorVectors_category_id)(17)

colorVectors_publish_at <- brewer.pal(3, "Set3")

with(df, pie(table(category_id), main = "Category", col = colorVectors_category_id, border = TRUE))
with(df, pie(table(publish_at), main = "Publish Time", col = colorVectors_publish_at, border = TRUE))

df$category_id_color <- factor(df$category_id, labels = colorVectors_category_id)
df$publish_at_color <- factor(df$publish_at, labels = colorVectors_publish_at)

with(df, symbols(x = log10(views),
                 y = log10(dislikes),
                 bg = as.character(category_id_color)))
#c('Film & Animation', 'Music','Pets & Animals','Sports','Travel & Events',
#  'Autos & Vehicles','Gaming','People & Blogs','Comedy','Entertainment',
#  'News & Politics','Howto & Style','Education','Science & Technology',
#  'Nonprofits & Activism','Shows','Trailers')
selected <- c("Comedy", "News & Politics", "Music")
df_sub <- subset(df, !is.na(match(category_id, selected)))

plot(log10(dislikes) ~ log10(views), df_sub, col = as.character(category_id_color))
legend(x = "topleft", legend = levels(df_sub$category_id), cex = 0.7, lwd = 3, col = colorVectors_category_id)
