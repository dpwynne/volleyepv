library(readxl)

team_conference_logos <- read_excel("teams.xlsx")
opp_conference_logos <- team_conference_logos
colnames(opp_conference_logos) <- c("opponent", "opponent_short", "opp_conference", "opp_conference_short", "opp_link", "opp_conference_link")

usethis::use_data(team_conference_logos, overwrite = TRUE)
usethis::use_data(opp_conference_logos, overwrite = TRUE)
