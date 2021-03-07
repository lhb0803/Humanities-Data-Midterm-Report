library(lubridate)
library(RColorBrewer)

data_path = "data/MXvideos.csv"

# Load data
colClasses_vec <- c("NULL", "character", "NULL", "character","factor", "character", "character", "numeric", "numeric", "numeric", "numeric", "NULL", "logical", "logical", "logical", "NULL" )
raw <- read.csv(data_path, encoding = 'UTF-8', colClasses = colClasses_vec)

# set time data
raw$trending_date <- format(as.Date(raw$trending_date, "%Y.%d.%m"), "20%y-%m-%d")
raw$trending_date <- as.Date(raw$trending_date, "20%y-%m-%d")
raw$publish_time <- format(as.POSIXct(raw$publish_time, tz = "GMT", format = c("%Y-%m-%dT%H:%M:%OSZ")), "20%y-%m-%dT%H:%M")
raw$publish_time <- as.POSIXct(raw$publish_time, tz = "GMT", "20%y-%m-%dT%H:%M")
raw$publish_time <- raw$publish_time + hours(9) # Korea, Japan Time
#raw$publish_time <- raw$publish_time + hours(2) # France Time
#raw$publish_time <- raw$publish_time + hours(6) # India Time
#raw$publish_time <- raw$publish_time + hours(6) # Canada Time
raw$publish_time <- raw$publish_time + hours(1) # Germany, UK Time
raw$publish_time <- raw$publish_time + hours(-6) # Mexico Time
raw$publish_time <- raw$publish_time + hours(3) # Russia Time(Moscow)

# set category data
### SHOULD CHECK!!! ###
levels(raw$category_id)
raw$channel_title[raw$category_id == 44]
# Korea category
#category_vector <- c('Film & Animation', 'Music','Pets & Animals','Sports','Travel & Events','Autos & Vehicles','Gaming','People & Blogs','Comedy','Entertainment','News & Politics','Howto & Style','Education','Science & Technology','Nonprofits & Activism','Shows','Trailers')

# France category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows', 'Trailers')

# US category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Shows')

# Japan category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism')

# India category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows')

# Canada category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows')

# Germany category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows', 'Trailers')

# UK category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Shows')

# Mexico category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Shows')

# Russia category
#category_vector <- c('Film & Animation', 'Music', 'Pets & Animals', 'Sports', 'Travel & Events', 'Autos & Vehicles', 'Gaming', 'People & Blogs', 'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style', 'Education', 'Science & Technology', 'Nonprofits & Activism', 'Movies', 'Shows')

#raw$category_id <- factor(raw$category_id, labels = category_vector)

# set publish time
publish_1 <- with(raw, raw[hour(publish_time) >= 2 & hour(publish_time) < 10 , ] )
publish_2 <- with(raw, raw[hour(publish_time) >= 10 & hour(publish_time) < 18 , ] )
publish_3 <- with(raw, raw[hour(publish_time) >= 18 | hour(publish_time) < 2 , ] )

publish_1$publish_at <- c("Sleep_Time")
publish_2$publish_at <- c("Work_Time")
publish_3$publish_at <- c("Off_Time")

df <- rbind(rbind(publish_1, publish_2), publish_3)
df$publish_at <- factor(df$publish_at)

summary(df)

# set color data for EDA
# Korea 17, France 18, US 16, Japan 15, India 17
# Canada 17, Germany 18, UK 16, Mexico 16, Russia 17
#colorVectors_category_id <- c(brewer.pal(12, "Paired"), (brewer.pal(5, "Dark2")))

#colorVectors_publish_at <- brewer.pal(3, "Set3")

#df$category_id_color <- factor(df$category_id, labels = colorVectors_category_id)
#df$publish_at_color <- factor(df$publish_at, labels = colorVectors_publish_at)

# upload to the data
df_KR <- df
df_KR$country <- "Korea"

#df_FR <- df
#df_FR$country <- "France"

df_US <- df
df_US$country <- "USA"

#df_JP <- df
#df_JP$country <- "Japan"

#df_IN <- df
#df_IN$country <- "India"

df_CA <- df
df_CA$country <- "Canada"

df_DE <- df
df_DE$country <- "Germany"

df_GB <- df
df_GB$country <- "UK"

df_MX <- df
df_MX$country <- "Mexico"

df_RU <- df
df_RU$country <- "Russia"


# save the data

write.csv(df_CA, "data/df_CA.csv", row.names = FALSE)
df_CA
df_DE
df_FR
df_GB
df_IN
df_JP

df_KR
write.csv(df_KR, "data/df_KR.csv", row.names = FALSE)
df_MX
df_RU
df_US

