##############################
##Title: shot data script
##Description: 
##Inputs: dataframe with names and filepaths from workout01
##Outputs: single csv file
##############################

make_charts <- function(dat) {
  names <- unique(dat$name)
  print(names)
  for (player in names) {
    ggti <- paste(paste("Shot Chart: ", player, sep = ""),
                     "(2016 Season)", sep = "")
    player_shot_chart <- ggplot(data = dat) +
      annotation_custom(court_image, -250, 250, -50, 420) +
      geom_point(aes(x=x, y=y, color = shot_made_flag)) +
      ylim(-50, 420) +
      ggtitle(ggti) + 
      theme_minimal()
    filename <- paste(paste("images/Shot Chart: ", player, sep = ""), " (2016 season).pdf", sep = "" )
    print(filename)
    ggsave(filename, width = 6.5, height = 5)
  }
  player_shot_chart <- ggplot(data = dat) +
    annotation_custom(court_image, -250, 250, -50, 420) +
    geom_point(aes(x=x, y=y, color = shot_made_flag)) +
    ylim(-50, 420) +
    ggtitle(ggti) + 
    theme_minimal() +
    facet_wrap(~name)
  ggsave("images/gsw-shot-charts.pdf", width = 6.5, height = 5)
}

court_file <- "images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))
