library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(magick)
library(OpenImageR)
library(webshot2)

liberos <- subset(mens_test, position == "L/DS")

#liberos$player_name <- ifelse(str_sub(liberos$player_name, start = -9) == "Constable", "Donovan Constable", liberos$player_name)
#liberos$player_name <- ifelse(liberos$player_name == "GABRIEL Dyer", "Gabriel Dyer", liberos$player_name)
attempts <- aggregate(count ~ player_name*position*team*conference, subset(liberos, skill == "Reception" | skill == "Dig"), sum)
colnames(attempts) <- c("player_name", "position", "team", "conference", "attempts")

rec <- aggregate(count ~ player_name*position*team*conference, subset(liberos, skill == "Reception"), sum)
colnames(rec) <- c("player_name", "position", "team", "conference", "rec")

dig <- aggregate(count ~ player_name*position*team*conference, subset(liberos, skill == "Dig"), sum)
colnames(dig) <- c("player_name", "position", "team", "conference", "dig")

rec_fault <- aggregate(count ~ player_name*position*team*conference, subset(liberos, skq == "Reception ="), sum)
colnames(rec_fault) <- c("player_name", "position", "team", "conference", "rec_fault")

xso <- aggregate(epv_out ~ player_name*position*team*conference, subset(liberos, skill == "Reception"), mean)
colnames(xso) <- c("player_name", "position", "team", "conference", "xso")

epv_added_avg_rec <- aggregate(epv_added ~ player_name*position*team*conference, subset(liberos, skill == "Reception"), mean)
colnames(epv_added_avg_rec) <- c("player_name", "position", "team", "conference", "epv_added_avg_rec")

epv_added_avg_dig <- aggregate(epv_added ~ player_name*position*team*conference, subset(liberos, skill == "Dig"), mean)
colnames(epv_added_avg_dig) <- c("player_name", "position", "team", "conference", "epv_added_avg_dig")

xtrans <- aggregate(epv_out ~ player_name*position*team*conference, subset(liberos, skill == "Dig"), mean)
colnames(xtrans) <- c("player_name", "position", "team", "conference", "xtrans")

epv_added_avg_recdig <- aggregate(epv_added ~ player_name*position*team*conference, subset(liberos, skill == "Dig" | skill == "Reception"), mean)
colnames(epv_added_avg_recdig) <- c("player_name", "position", "team", "conference", "epv_added_avg_recdig")

df <- merge(attempts, rec, all.x = TRUE)
df <- merge(df, dig, all.x = TRUE)
df <- merge(df, rec_fault, all.x = TRUE)
df <- merge(df, xso, all.x = TRUE)
df <- merge(df, epv_added_avg_rec, all.x = TRUE)
df <- merge(df, epv_added_avg_dig, all.x = TRUE)
df <- merge(df, xtrans, all.x = TRUE)
df <- merge(df, epv_added_avg_recdig, all.x = TRUE)
df$recfault_percent <- (df$rec_fault / df$rec) * 100
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
chart <- subset(chart, attempts > 100)
chart$mean <- mean(chart$epv_added_avg_recdig)
chart$sd <- sd(chart$epv_added_avg_recdig)
chart$z_score <- (chart$epv_added_avg_recdig - chart$mean) / chart$sd
chart <- chart %>% arrange(desc(z_score)) %>% group_by(position) %>% slice(1:15) %>% ungroup(position)
chart$oos_percent <- NULL
chart$oos_count <- NULL
chart$position <- NULL
chart$team <- NULL
chart$mean <- NULL
chart$attempts <- NULL
chart$rec_fault <- NULL
chart$sd <- NULL
chart$poor_fault <- NULL
chart$conference <- NULL
chart <- chart %>%
  mutate(recfault_percent = paste0(round(recfault_percent, 1), "%"),
         xso = paste0(100*round(xso, 3), "%"),
         xtrans = paste0(100*round(xtrans, 3), "%"),
         epv_added_avg_rec = round(epv_added_avg_rec, 3),
         epv_added_avg_dig = round(epv_added_avg_dig, 3),
         epv_added_avg_recdig = round(epv_added_avg_recdig, 3),
         percentile = round(pnorm(z_score)*100, 1))

chart <- chart %>% relocate(player_name, wordmark, conference_logo, rec, xso, recfault_percent, epv_added_avg_rec, dig, xtrans, epv_added_avg_dig, epv_added_avg_recdig, percentile)
chart$z_score <- NULL

chart <- chart %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(player_name = "Player",
             wordmark = "School",
             conference_logo = "Conference",
             rec = "Receptions",
             recfault_percent = "Reception Error",
             xso = "Expected Sideout",
             dig = "Dig Touches",
             epv_added_avg_rec = "Reception EPA",
             xtrans = "Expected Trans",
             epv_added_avg_dig = "Dig EPA",
             epv_added_avg_recdig = "Rec & Dig EPA",
             percentile = "He's Better Than...") %>%
  gtExtras::gt_theme_espn() %>%
  gt_color_rows(percentile, palette = "ggsci::blue_material") %>%
  gtExtras::gt_img_rows(wordmark) %>%
  gtExtras::gt_img_rows(conference_logo) %>%
  gt::tab_header(title = "Best Liberos in Men's Volleyball (2023)",
                 subtitle = "EPA = Expected Points Added | Attempts > 100 | Rec/Dig counts include errors") %>%
  gt::tab_source_note("volleydork.blog")


chart$`_data`$percentile <- paste0(chart$`_data`$percentile, "%")
chart
save_me <- chart

gtsave(save_me, "charts/20230310-men-liberos.png")
