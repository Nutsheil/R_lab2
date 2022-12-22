setwd("C:\\Users\\Evgeniy\\Desktop\\study\\R\\lab-2")
df6 <- read.csv(file = "data\\RH_T.csv")

func <- function(df){
  first_date <- as.Date(c("06.01.2020"), "%d.%m.%Y")
  last_date <- as.Date(c("27.12.2020"), "%d.%m.%Y")
  
  dates = as.Date(df$YYYYMMDD, "%Y-%m-%d")
  df <- filter(df, dates >= first_date & dates <= last_date)
  df <- mutate(df, WOY=((df$DOY + (7-6)) %/% 7))
  
  omg <- data.frame(df %>% group_by(WOY) %>%  summarise(ATOW=mean(T2M)))
  omg <- filter(omg, ATOW == max(ATOW))
  
  res <- data.frame(df$YYYYMMDD, df$WOY, df$T2M)
  res <- rename_with(res, ~ gsub("df.", "", .x, fixed = TRUE))
  res <- filter(res, WOY==omg[1,'WOY'])
  
  return (res)
}

res <- func(df6)
print(res)
