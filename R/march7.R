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
library(webshot2)

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
best <- best %>% select(player_name, wordmark, attempts, epv_added_sum)
best <- best[order(-best$epv_added_sum),]

best %>%
  mutate(epv_added_sum = round(epv_added_sum, 1)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(player_name = "Player",
             wordmark = "School",
             attempts = "Attempts",
             epv_added_sum = "Total EPV Added") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_hulk_col_numeric(epv_added_sum) %>%
  gtExtras::gt_img_rows(wordmark) %>%
  gt::tab_header(title = "Best Attackers in the Big Ten - 2022")






attempts <- aggregate(count ~ player_name*team, subset(epv_data, skill == "Attack"), sum)
colnames(attempts) <- c("player_name", "team", "attempts")

kills <- aggregate(count ~ player_name*team, subset(epv_data, skq == "Attack #"), sum)
colnames(kills) <- c("player_name", "team", "kills")

insys <- aggregate(poss_eff ~ player_name*team, subset(epv_data, skill == "Attack" & substr(attack_code, 1, 1) %in% c("C", "P", "X")), mean)
colnames(insys) <- c("player_name", "team", "insys")

oos <- aggregate(poss_eff ~ player_name*team, subset(epv_data, skill == "Attack" & substr(attack_code, 1, 1) == "V"), mean)
colnames(oos) <- c("player_name", "team", "oos")

oos_count <- aggregate(count ~ player_name*team, subset(epv_data, skill == "Attack" & substr(attack_code, 1, 1) == "V"), sum)
colnames(oos_count) <- c("player_name", "team", "oos_count")

epv_added_total <- aggregate(epv_added ~ player_name*team, subset(epv_data, skill == "Attack"), sum)
colnames(epv_added_total) <- c("player_name", "team", "epv_added_total")

epv_added_avg <- aggregate(epv_added ~ player_name*team, subset(epv_data, skill == "Attack"), mean)
colnames(epv_added_avg) <- c("player_name", "team", "epv_added_avg")

epv_ratio <- aggregate(epv_ratio ~ player_name*team, subset(epv_data, skill == "Attack"), mean)
colnames(epv_ratio) <- c("player_name", "team", "epv_ratio")

df <- merge(attempts, kills)
df <- merge(df, insys, all.x = TRUE)
df <- merge(df, oos, all.x = TRUE)
df <- merge(df, oos_count, all.x = TRUE)
df <- merge(df, epv_added_total, all.x = TRUE)
df <- merge(df, epv_added_avg, all.x = TRUE)
df <- merge(df, epv_ratio, all.x = TRUE)
df$oos_percent <- (df$oos_count / df$attempts) * 100
df$oos <- round(df$oos, 3)
df$oos <- ifelse((df$oos_percent < 15) | (is.na(df$oos_percent)), " - ", df$oos)

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

chart <- merge(df, bigten)
chart <- subset(chart, attempts > 100)
chart$mean <- mean(chart$epv_added_avg)
chart$sd <- sd(chart$epv_added_avg)
chart$z_score <- (chart$epv_added_avg - chart$mean) / chart$sd
chart <- chart[order(-chart$z_score),]
chart <- chart[1:20,]
chart$oos_percent <- NULL
chart$oos_count <- NULL
chart$team <- NULL
chart$mean <- NULL
chart$sd <- NULL
chart <- chart %>%
  mutate(epv_added_total = round(epv_added_total, 1),
         insys = round(insys, 3),
         epv_added_avg = round(epv_added_avg, 3),
         epv_ratio = round(epv_ratio, 2),
         z_score = round(z_score, 2),
         better_than = paste0(round(pnorm(z_score)*100, 1), "%"))
chart <- chart %>% relocate(player_name, wordmark)

save_me <- chart %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(player_name = "Player",
             wordmark = "School",
             attempts = "Attempts",
             kills = "Kills",
             insys = "In-Sys Eff",
             oos = "OOS Eff",
             epv_added_total = "Points Over Expectation",
             epv_added_avg = "Points per Attack Over Expectation",
             epv_ratio = "Multiplier vs. Expectation",
             z_score = "Z-Score",
             better_than = "She's Better Than...") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_hulk_col_numeric(z_score) %>%
  gtExtras::gt_img_rows(wordmark) %>%
  gt::tab_header(title = "Best Attackers in the Big Ten - 2022")
gtsave(save_me, "bigten-2022-v1.png")









a <- aggregate(count ~ team, epv_data, sum)
a$conference <- "blank"
write.csv(a, "teams.csv")

conf <- read.csv("conference_men.csv")
epv_data <- merge(epv_data, conf, all.x = TRUE)

mens_test <- subset(epv_data, conference %in% c("MPSF", "Big West", "EIVA", "MIVA"))

