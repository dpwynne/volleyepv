attempts <- aggregate(count ~ player_name*team, subset(mens_test, skill == "Attack"), sum)
colnames(attempts) <- c("player_name", "team", "attempts")

kills <- aggregate(count ~ player_name*team, subset(mens_test, skq == "Attack #"), sum)
colnames(kills) <- c("player_name", "team", "kills")

insys <- aggregate(poss_eff ~ player_name*team, subset(mens_test, skill == "Attack" & substr(attack_code, 1, 1) %in% c("C", "P", "X")), mean)
colnames(insys) <- c("player_name", "team", "insys")

oos <- aggregate(poss_eff ~ player_name*team, subset(mens_test, skill == "Attack" & substr(attack_code, 1, 1) == "V"), mean)
colnames(oos) <- c("player_name", "team", "oos")

oos_count <- aggregate(count ~ player_name*team, subset(mens_test, skill == "Attack" & substr(attack_code, 1, 1) == "V"), sum)
colnames(oos_count) <- c("player_name", "team", "oos_count")

epv_added_total <- aggregate(epv_added ~ player_name*team, subset(mens_test, skill == "Attack"), sum)
colnames(epv_added_total) <- c("player_name", "team", "epv_added_total")

epv_added_avg <- aggregate(epv_added ~ player_name*team, subset(mens_test, skill == "Attack"), mean)
colnames(epv_added_avg) <- c("player_name", "team", "epv_added_avg")

epv_ratio <- aggregate(epv_ratio ~ player_name*team, subset(mens_test, skill == "Attack"), mean)
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
df$epv_added_per10 <- 10*df$epv_added_avg

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
                        "California State University, Long Beach (Men's)",
                        "Saint Francis University (Men's)"),
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
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/charleston.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/hawaii.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/lbsu.png",
                       "https://raw.githubusercontent.com/volleydork/volleyR/main/ncaa_logos/sfu.png"))
colnames(schools) <- c("team", "wordmark")

chart <- merge(df, schools, all.x = TRUE)
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
         epv_added_avg = round(epv_added_avg, 2),
         epv_ratio = round(epv_ratio, 2),
         epv_added_per10 = round(epv_added_per10, 2),
         z_score = round(z_score, 2),
         better_than = paste0(round(pnorm(z_score)*100, 1), "%"))
chart <- chart %>% relocate(player_name, wordmark)

chart %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(player_name = "Player",
             wordmark = "School",
             attempts = "Attempts",
             kills = "Kills",
             insys = "In-Sys Eff",
             oos = "OOS Eff",
             epv_ratio = "EPV Out / EPV In",
             epv_added_total = "Total EPA",
             epv_added_avg = "EPA per Attack",
             epv_added_per10 = "EPA per 10 Attacks",
             z_score = "Z-Score",
             better_than = "He's Better Than") %>%
  gtExtras::gt_theme_espn() %>%
  #gtExtras::gt_hulk_col_numeric(z_score) %>%
  gt_color_rows(z_score, palette = "ggsci::blue_material") %>%
  gtExtras::gt_img_rows(wordmark) %>%
  gt::tab_header(title = "Best Attackers in Men's Volleyball - 2023")
#gtsave(save_me, "mpsf-bigwest-2023-v1.png")
