library(readr)
library(dplyr)

setwd("C:\\Users\\Evgeniy\\Desktop\\study\\R\\lab-2")
df20 <- read_csv(file = "data\\Payment_and_Value_of_Care-Hospital.csv")

func <- function(df, state_name, procedure, max_money){
  df <- filter(df, df$'State' == state_name)
  df <- filter(df, df$'Payment Measure Name' == paste("Payment for", procedure, "patients"))
  
  money <- as.numeric(gsub("$", "", gsub(",", "", df$'Higher Estimate'), fixed = TRUE))
  reserv <- df
  df <- filter(df, money <= max_money)
  if(dim(df)[1] == 0)
    return (paste("You need - ", min(reserv$'Payment')))
  
  lower_estimate <- as.numeric(gsub("$", "", gsub(",", "", df$'Lower Estimate'), fixed = TRUE))
  higher_estimate <- as.numeric(gsub("$", "", gsub(",", "", df$'Higher Estimate'), fixed = TRUE))
  df <- arrange(df, (lower_estimate + higher_estimate)/2)
  
  res <- data.frame(df$'Facility Name',
                    df$'City',
                    df$'Address',
                    df$'Lower Estimate',
                    df$'Higher Estimate')
  
  res <- rename_with(res, ~ gsub(".", " ", .x, fixed = TRUE))
  res <- rename_with(res, ~ gsub("df", "", .x, fixed = TRUE))
  
  return(res)
}

res <- func(df20, "AL", "pneumonia", 20000)
print(res)
