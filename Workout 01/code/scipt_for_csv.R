
##############################
##Title: shot data script
##Description: 
##Inputs: dataframe with names and filepaths from workout01
##Outputs: single csv file
##############################

make_shots_csv <- function(x) {
  shots_df <- data.frame()
  for (row in 1:nrow(x[1:length(x)])) {
    pfile <- paste("/home/cream/CS/133/workout01/", x[row, "Paths"], sep = "")
    namex <- x[row, "Names"]
    newdf <- read.csv(pfile , quote = "", stringsAsFactors = FALSE)
    newdf["name"] <- namex
    newdf[which(newdf$shot_made_flag == "y"), "shot_made_flag"] <- "shot_yes"
    newdf[which(newdf$shot_made_flag == "n"), "shot_made_flag"] <- "shot_no"
    newdf["minute"] <- (newdf$period * 12) - newdf$minutes_remaining
    sink_path <- paste("output/", gsub(" ", "-", 
                                       tolower(paste(toString(namex),
                                       "-summary.txt",sep = ""))), sep = "")
    sink(sink_path)
    print(summary(newdf))
    sink()
    shots_df <- rbind(shots_df, newdf)
  }
  shots_csv <- write.csv(shots_df, "data/shots-data.csv")
  sink("output/shots-outputs-summary.txt")
  summary(shots_df)
  sink()
  return(as.data.frame(shots_df))
}
Names <- c("Stephen Curry", "Kevin Durant", "Andre Iguodala", "Klay Thompson", "Draymond Green")
Paths <- c("data/stephen-curry.csv", "data/kevin-durant.csv", "data/andre-iguodala.csv", "data/klay-thompson.csv", "data/draymond-green.csv")


