library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(magick)
library(OpenImageR)
library(webshot2)

setters <- subset(mens_test, position == "Setter")

setters$player_name <- ifelse(str_sub(setters$player_name, start = -9) == "Constable", "Donovan Constable", setters$player_name)
setters$player_name <- ifelse(setters$player_name == "GABRIEL Dyer", "Gabriel Dyer", setters$player_name)



attempts <- aggregate(count ~ player_name*position*team*conference, subset(setters, skill == "Set"), sum)
colnames(attempts) <- c("player_name", "position", "team", "conference", "attempts")

poor_fault <- aggregate(count ~ player_name*position*team*conference, subset(setters, skq == "Set -" | skq == "Set ="), sum)
colnames(poor_fault) <- c("player_name", "position", "team", "conference", "poor_fault")

team_eff_insys <- aggregate(poss_eff ~ player_name*position*team*conference, subset(setters, skill == "Set" & kcode == "yes"), mean)
colnames(team_eff_insys) <- c("player_name", "position", "team", "conference", "team_eff_insys")

epv_added_total <- aggregate(epv_added ~ player_name*position*team*conference, subset(setters, skill == "Set"), sum)
colnames(epv_added_total) <- c("player_name", "position", "team", "conference", "epv_added_total")

epv_added_avg <- aggregate(epv_added ~ player_name*position*team*conference, subset(setters, skill == "Set"), mean)
colnames(epv_added_avg) <- c("player_name", "position", "team", "conference", "epv_added_avg")

epv_added_avg_insys <- aggregate(epv_added ~ player_name*position*team*conference, subset(setters, skill == "Set" & kcode == "yes"), mean)
colnames(epv_added_avg_insys) <- c("player_name", "position", "team", "conference", "epv_added_avg_insys")

epv_added_avg_oos <- aggregate(epv_added ~ player_name*position*team*conference, subset(setters, skill == "Set" & kcode == "no"), mean)
colnames(epv_added_avg_oos) <- c("player_name", "position", "team", "conference", "epv_added_avg_oos")

df <- merge(attempts, poor_fault)
df <- merge(df, team_eff_insys, all.x = TRUE)
df <- merge(df, epv_added_total, all.x = TRUE)
df <- merge(df, epv_added_avg, all.x = TRUE)
df <- merge(df, epv_added_avg_insys, all.x = TRUE)
df <- merge(df, epv_added_avg_oos, all.x = TRUE)
df$poor_fault_percent <- (df$poor_fault / df$attempts) * 100
df[is.na(df)] <- 0

schools <- data.frame(c("Lewis University (Men's)",
                        "Ball State University (Men's)",
                        "Pepperdine University (Men's)",
                        "Stanford University (Men's)",
                        "Concordia University Irvine (Men's)",
                        "Loyola University Chicago (Men's)",
                        "University of California, Irvine (Men's)",
                        "Grand Canyon University (Men's)",
                        "Purdue University Fort Wayne (Men's)",
                        "California State University, Northridge (Men's)",
                        "Ohio State University (Men's)",
                        "University of California, San Diego (Men's)",
                        "University of California, Santa Barbara (Men's)",
                        "University of California, Los Angeles (Men's)",
                        "Brigham Young University (Men's)",
                        "University of Southern California (Men's)",
                        "Lindenwood University (Men's)",
                        "McKendree University (Men's)",
                        "Pennsylvania State University (Men's)",
                        "Quincy University (Men's)",
                        "Princeton University (Men's)",
                        "George Mason University (Men's)",
                        "New Jersey Institute of Technology (Men's)",
                        "Harvard University (Men's)",
                        "University of Charleston (WV) (Men's)",
                        "University of Hawaii (Men's)",
                        "California State University, Long Beach (Men's)"),
                      c("https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/lewis.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ballstate.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/pepperdine.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/stanford.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/cui.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/loyola.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/uci.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/gcu.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/pfw.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/csun.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ohiostate.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ucsd.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ucsb.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ucla.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/byu.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/usc.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/lindenwood.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/mckendree.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/pennstate.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/quincy.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/princeton.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/georgemason.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/njit.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/harvard.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/ucwv.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/hawaii.png",
                        "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/lbsu.png"))

conferences <- data.frame(c("Big West", "MPSF", "EIVA", "MIVA"
),
c("https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/conference_bigwest.png",
  "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/conference_mpsf.png",
  "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/conference_eiva.png",
  "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/conference_miva.png"
))
colnames(schools) <- c("team", "wordmark")
colnames(conferences) <- c("conference", "conference_logo")



chart <- merge(df, schools, all.x = TRUE)
chart <- merge(chart, conferences, all.x = TRUE)
chart <- subset(chart, attempts > 300)
chart$mean <- mean(chart$epv_added_avg)
chart$sd <- sd(chart$epv_added_avg)
chart$z_score <- (chart$epv_added_avg - chart$mean) / chart$sd
chart <- chart %>% arrange(desc(z_score)) %>% group_by(position) %>% slice(1:15) %>% ungroup(position)
chart$oos_percent <- NULL
chart$oos_count <- NULL
chart$position <- NULL
chart$team <- NULL
chart$mean <- NULL
chart$sd <- NULL
chart$poor_fault <- NULL
chart$conference <- NULL
chart <- chart %>%
  mutate(epv_added_total = round(epv_added_total, 1),
         team_eff_insys = round(team_eff_insys, 3),
         poor_fault_percent = paste0(round(poor_fault_percent, 1), "%"),
         epv_added_avg = round(epv_added_avg, 3),
         epv_added_avg_insys = round(epv_added_avg_insys, 3),
         epv_added_avg_oos = round(epv_added_avg_oos, 3),
         percentile = round(pnorm(z_score)*100, 1))

chart <- chart %>% relocate(player_name, wordmark, conference_logo, attempts, poor_fault_percent)
chart$z_score <- NULL

chart <- chart %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(player_name = "Player",
             wordmark = "School",
             conference_logo = "Conference",
             attempts = "Attempts",
             poor_fault_percent = "Poor / Fault",
             team_eff_insys = "Team In-Sys Eff",
             epv_added_total = "Total EPA",
             epv_added_avg = "EPA per Attempt",
             epv_added_avg_insys = "In-Sys EPA",
             epv_added_avg_oos = "OOS EPA",
             percentile = "He's Better Than...") %>%
  gtExtras::gt_theme_espn() %>%
  gt_color_rows(percentile, palette = "ggsci::blue_material") %>%
  gtExtras::gt_img_rows(wordmark) %>%
  gtExtras::gt_img_rows(conference_logo) %>%
  gt::tab_header(title = "Best Setters in Men's Volleyball (2023)",
                 subtitle = "EPA = Expected Points Added | Attempts > 300") %>%
  gt::tab_source_note("volleydork.blog")


chart$`_data`$percentile <- paste0(chart$`_data`$percentile, "%")
chart
save_me <- chart

gtsave(save_me, "charts/20230310-men-setters.png")
