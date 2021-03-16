
df <- do.call("rbind", list(df_CA, df_DE, df_FR, df_GB, df_IN, df_JP, df_KR, df_MX, df_RU, df_US))
write.csv(df, "data/df.csv", row.names = FALSE)
colClasses_vec <- c("character", "character", "factor", "character", "character", "numeric", "numeric", "numeric", "numeric", "logical", "logical", "logical", "factor", "factor" )
df <- read.csv("data/df.csv", encoding = 'UTF-8', colClasses = colClasses_vec)
df$trending_date <- as.Date(df$trending_date, "20%y-%m-%d")
df$publish_time <- as.POSIXct(df$publish_time, tz = "GMT", "20%y-%m-%d %H:%M:%S")


df$tags <- with(df, strsplit(x = tags, split = "[\\|,#]+"))

category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows', 'Trailers')
publish_vector <- c('Off_Time', 'Work_Time', 'Sleep_Time')
df$category_id <- factor(df$category_id, labels = category_vector)
df$publish_at <- factor(df$publish_at, levels = publish_vector)
df$country <- factor(df$country)

colorCategoryBrewer <-
  c("#FF00CC", "#CC0099", "#9900CC", "#FF0000",
    "#FF9933", "#FFFF00", "#99CC00", "#00CC99",
    "#003333", "#33FF66", "#3399FF", "#333399",
    "#666699", "#330099", "#996600", "#333366",
    "#000099", "#000066")

re_order_factor <- c("Entertainment", "People & Blogs", "Music", "News & Politics", "Comedy", "Sports", "Film & Animation", "Howto & Style", "Gaming", "Science & Technology", "Education", "Autos & Vehicles", "Nonprofits & Activism", "Shows", "Pets & Animals", "Movies", "Travel & Events", "Trailers")   

df$category_id <- factor(df$category_id, levels = re_order_factor)

df$category_id_color <- factor(df$category_id, labels = colorCategoryBrewer)
