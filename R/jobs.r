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

df_avg <-
  df_avg %>%
    mutate(
      my_pred = ifelse(lineavg > line, home, road),
      my_pred_id = ifelse(lineavg > line, "home", "road"),
      my_abs_diff = abs(diff)) %>%
    arrange(desc(my_abs_diff))

# save the original data (downloaded from url)
write.csv(df_tpt_file, paste0("raw-data/", paste(Sys.Date(),"raw-ncaa-predictions.csv", sep="-")))

# save the data table
write.csv(df_avg, paste0("raw-data/", paste(Sys.Date(),"ncaa-predictions.csv", sep="-")))

#-------------------------------------------------------------------------------
# first plot (absolute diff)

# limit to top 20 picks
my_plot1 <- df_avg[1:20,] %>% arrange(desc(my_abs_diff))

# team names
my_plot1_teams <- my_plot1$my_pred

# JPEG device
jpeg(paste0("raw-data/", paste(Sys.Date(),"abs-top-20-predictions.jpeg", sep="-")), quality = 75)

# Code
plot(my_plot1$my_abs_diff,
     type = "p",
     lwd=3,
     col="blue",
     main = paste("Absolute Difference - NCAAF Games -", Sys.Date(),sep = " "),
     xlab = "Game Number",
     ylab = "Abs Diff Prediction Avg",
     ylim=c(0, (max(my_plot1$diff)+2))
)
text(seq_along(my_plot1$my_pred), my_plot1$my_abs_diff, labels = my_plot1$my_pred, cex=0.4, pos=3, col="black")

# Close device
dev.off()

#-------------------------------------------------------------------------------
# second plot (relative diff)

# limit to top 20 picks
my_plot2 <- df_avg %>% filter(line != 0) %>% arrange(desc(my_abs_diff_pct)) %>% head(.,20)

# team names
my_plot2_teams <- my_plot2$my_pred

# JPEG device
jpeg(paste0("raw-data/", paste(Sys.Date(),"abs-top-20-predictions.jpeg", sep="-")), quality = 75)

# Code
plot(my_plot2$my_abs_diff_pct, type = "p", lwd=3, col="blue",
     main = paste("Relative (Pct) Difference - NCAAF Games -", Sys.Date(),sep = " "),
     xlab = "Game Number",
     ylab = "Relative (Pct) Diff Prediction Avg",
     ylim=c(0, max(my_plot2$my_abs_diff_pct)+1))
text(seq_along(my_plot2_teams), my_plot2$my_abs_diff_pct, my_plot2_teams, cex=0.4, pos=3, col="black")

# Close device
dev.off()
