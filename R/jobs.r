library(tidyverse)

# set the url for the csv file
p_tpt_address <- 'https://www.thepredictiontracker.com/ncaapredictions.csv'

# create a file path and name with the timestamp for the csv file
p_csv_file <- paste(Sys.Date(), "ncaa-predictions.csv", sep="-")

# download the file and save it to the raw-data directory
download.file(p_tpt_address, paste("raw-data", p_csv_file, sep="/"))

# read the file
df_tpt_file <- read_csv(paste("raw-data", p_csv_file, sep="/"))

# create the data table
df_avg <-
  df_tpt_file %>%
    select(road,home,lineopen, lineavg, line, linemedian, phcover, phwin) %>%
    mutate(diff = lineavg-line) %>%
    arrange(desc(diff))

# save the data table
save(df_avg, paste0("raw-data/", paste(Sys.Data(),"ncaa-predictions.csv", sep="-")))
