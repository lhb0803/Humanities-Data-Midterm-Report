# numeric data
hist(log10(df$views))
hist(log10(df$likes))
hist(log10(df$dislikes))
hist(log10(df$comment_count))

# categoric data
with(df, pie(table(category_id), main = "Category", col = colorVectors_category_id, border = TRUE))
par(mfrow = c(1,1), mar = c(5.1, 12, 4.1, 2.1))
boxplot(log10(views) ~ category_id, df, horizontal = TRUE, las = 2, ylab = "")
par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

selected <- c("Comedy", "News & Politics", "Music")
df_sub <- subset(df, !is.na(match(category_id, selected)))

plot(log10(dislikes) ~ log10(views), df_sub, col = as.character(category_id_color))
legend(x = "topleft", legend = levels(df_sub$category_id), cex = 0.7, lwd = 3, col = colorVectors_category_id)

# count per trending date
with(df[df$category_id == "Entertainment",], barplot(table(trending_date)))
with(df[df$category_id == "News & Politics",], barplot(table(trending_date)))
with(df[df$category_id == "People & Blogs",], barplot(table(trending_date)))

# count_per publish time
with(df, pie(table(publish_at), main = "Publish Time", col = colorVectors_publish_at, border = TRUE))

plot(log10(views) ~ log10(likes), df, col = as.character(publish_at_color))
mosaicplot(xtabs(~publish_at + category_id, df),las = c(1,2))
boxplot(log10(views) ~ publish_at, df, horizontal = TRUE)
