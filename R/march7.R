best_player <- function(epv, which_skill, min_attempts, top_X) {
  a <- aggregate(epv_added ~ skill*team*player_name, subset(epv, skill==which_skill), mean)
  colnames(a) <- c("skill","team", "player_name", "epv_added_avg")
  b <- aggregate(count ~ skill*team*player_name, subset(epv, skill==which_skill), sum)
  colnames(b) <- c("skill","team", "player_name", "attempts")
  c <- merge(a,b)
  c <- subset(c, attempts > min_attempts)
  c <- c[order(-c$epv_added),]
  best_player <- c[1:top_X,1:5]
  return(best_player)
}

best_player_sum <- function(epv, which_skill, min_attempts, top_X) {
  a <- aggregate(epv_added ~ skill*team*player_name, subset(epv, skill==which_skill), sum)
  colnames(a) <- c("skill","team", "player_name", "epv_added_sum")
  b <- aggregate(count ~ skill*team*player_name, subset(epv, skill==which_skill), sum)
  colnames(b) <- c("skill","team", "player_name", "attempts")
  c <- merge(a,b)
  c <- subset(c, attempts > min_attempts)
  c <- c[order(-c$epv_added),]
  best_player <- c[1:top_X,1:5]
  return(best_player)
}





library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(magick)
library(OpenImageR)

best <- best_player_sum(epv_data, which_skill = "Attack", min_attempts = 100, top_X = 15)
best <- best[,c(2:4)]
best$wordmark <- "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/illinois_fighting_illini_2014-present_w.png"
best <- best %>% relocate(team, wordmark)

best %>%
  mutate(epv_added_sum = round(epv_added_sum, 1)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(team = "School",
             player_name = "Player",
             epv_added_sum = "Total EPV Added") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_hulk_col_numeric(epv_added_sum) %>%
  gtExtras::gt_img_rows(wordmark)


#######

load <- "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/illinois_fighting_illini_2014-present_w.png" %>%
  magick::image_read() %>%
  magick::image_trim()

info <- magick::image_info(load)

rl <- (700 - info$width) / 2
tb <- (192 - info$height) / 2

image <- magick::image_border(load, "transparent", paste0(rl, "x", tb))

magick::image_write(image, path = "illinois.png", format = "png")









a <- aggregate(epv_added ~ skill*input_type*output_type, epv_data, sum)
a$epv_added <- round(a$epv_added,2)
a$abs_epv_added <- abs(a$epv_added)
a$direction <- ifelse(a$epv_added > 0, "positive", "negative")
