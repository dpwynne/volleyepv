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

bigten <- data.frame(c("University of Illinois Urbana-Champaign",
                       "University of Indiana, Bloomington",
                       "University of Iowa",
                       "University of Maryland",
                       "University of Michigan",
                       "Michigan State University",
                       "University of Minnesota",
                       "University of Nebraska-Lincoln",
                       "Northwestern University",
                       "Ohio State University",
                       "Pennsylvania State University",
                       "Purdue University",
                       "Rutgers University",
                       "University of Wisconsin-Madison"),
                     c("https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/illinois.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/indiana.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/iowa.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/maryland.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/michigan.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/michiganstate.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/minnesota.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/nebraska.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/northwestern.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ohiostate.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/pennstate.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/purdue.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/rutgers.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/wisconsin.png"))
colnames(bigten) <- c("team", "wordmark")

best <- best_player_sum(epv_data, which_skill = "Attack", min_attempts = 100, top_X = 10)
best <- merge(best, bigten)
best <- best[,c(2:4)]

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











a <- aggregate(epv_added ~ skill*input_type*output_type, epv_data, sum)
a$epv_added <- round(a$epv_added,2)
a$abs_epv_added <- abs(a$epv_added)
a$direction <- ifelse(a$epv_added > 0, "positive", "negative")
