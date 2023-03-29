
build_epv_average <- function(my.files) {
  library(tidyverse)
  library(datavolley)
  library(readxl)

  epv_data <- data.frame(matrix(ncol=0,nrow=0))

  for(i in 1:length(my.files)) {
    x <- read_dv(my.files[i], skill_evaluation_decode = "volleymetrics")
    x$plays$match_date <- x$meta$match$date
    x$plays$setscore_home1 <- x$meta$result$score_home_team[1]
    x$plays$setscore_home2 <- x$meta$result$score_home_team[2]
    x$plays$setscore_home3 <- x$meta$result$score_home_team[3]
    x$plays$setscore_home4 <- x$meta$result$score_home_team[4]
    x$plays$setscore_home5 <- x$meta$result$score_home_team[5]
    x$plays$setscore_away1 <- x$meta$result$score_visiting_team[1]
    x$plays$setscore_away2 <- x$meta$result$score_visiting_team[2]
    x$plays$setscore_away3 <- x$meta$result$score_visiting_team[3]
    x$plays$setscore_away4 <- x$meta$result$score_visiting_team[4]
    x$plays$setscore_away5 <- x$meta$result$score_visiting_team[5]
    epv_data <- rbind(epv_data, x$plays)
    ifelse(i == 1, print("1 match completed"), print(paste0(i, " matches completed")))
  }

  #remove non-action plays
  epv_data <- subset(epv_data, substring(code, 2, 2)!="P" & substring(code, 2, 2)!="p"& substring(code, 2, 2)!="z" & substring(code, 2, 2)!="c" & substring(code, 2, 2)!="*" & substring(code, 2, 2)!="$")
  '%!in%' <- function(x,y)!('%in%'(x,y))
  #create touch_id for all rows - helps w/ ability to reorder properly if I screw something up down the road
  epv_data$id_touch <- seq.int(nrow(epv_data))
  epv_data$year <- as.numeric(format(epv_data$match_date, "%Y"))

  #create column for who the team making the contact is playing against
  epv_data$opponent <- NA
  epv_data$opponent <- ifelse(epv_data$team == epv_data$home_team, epv_data$visiting_team, epv_data$opponent)
  epv_data$opponent <- ifelse(epv_data$team == epv_data$visiting_team, epv_data$home_team, epv_data$opponent)

  #create column for each set score differential
  epv_data$setscorediff <- NA
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$home_team & epv_data$set_number == 1, epv_data$setscore_home1-epv_data$setscore_away1, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$home_team & epv_data$set_number == 2, epv_data$setscore_home2-epv_data$setscore_away2, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$home_team & epv_data$set_number == 3, epv_data$setscore_home3-epv_data$setscore_away3, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$home_team & epv_data$set_number == 4, epv_data$setscore_home4-epv_data$setscore_away4, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$home_team & epv_data$set_number == 5, epv_data$setscore_home5-epv_data$setscore_away5, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$visiting_team & epv_data$set_number == 1, epv_data$setscore_away1-epv_data$setscore_home1, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$visiting_team & epv_data$set_number == 2, epv_data$setscore_away2-epv_data$setscore_home2, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$visiting_team & epv_data$set_number == 3, epv_data$setscore_away3-epv_data$setscore_home3, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$visiting_team & epv_data$set_number == 4, epv_data$setscore_away4-epv_data$setscore_home4, epv_data$setscorediff)
  epv_data$setscorediff <- ifelse(epv_data$team == epv_data$visiting_team & epv_data$set_number == 5, epv_data$setscore_away5-epv_data$setscore_home5, epv_data$setscorediff)

  #create column for match score differential
  epv_data$matchscorediff <- NA

  a <- epv_data %>%
    filter(team == home_team) %>%
    rowwise() %>%
    mutate(matchscorediff = sum(setscore_home1, setscore_home2, setscore_home3, setscore_home4, setscore_home5, na.rm = TRUE) - sum(setscore_away1, setscore_away2, setscore_away3, setscore_away4, setscore_away5, na.rm = TRUE))

  b <- epv_data %>%
    filter(team == visiting_team) %>%
    rowwise() %>%
    mutate(matchscorediff = sum(setscore_away1, setscore_away2, setscore_away3, setscore_away4, setscore_away5, na.rm = TRUE) - sum(setscore_home1, setscore_home2, setscore_home3, setscore_home4, setscore_home5, na.rm = TRUE))

  epv_data <- rbind(a,b)
  epv_data <- epv_data[order(epv_data$id_touch),]
  rm(a,b)

  # various data cleaning + modification
  epv_data$count <- 1
  epv_data$evaluation_code <- ifelse(epv_data$skill=="Set" & epv_data$evaluation_code == "+", "#", epv_data$evaluation_code)
  epv_data$add_cover <- ifelse(epv_data$skill=="Dig" & lag(epv_data$skill, 1)=="Block" & lag(epv_data$team, 2)==epv_data$team, "Cover", epv_data$skill)
  epv_data$skq <- paste(epv_data$add_cover, epv_data$evaluation_code)
  epv_data$two_touch_ago <- ifelse(epv_data$point_id == lag(epv_data$point_id, 2), lag(epv_data$skq, 2), NA)
  epv_data$one_touch_ago <- ifelse(epv_data$point_id == lag(epv_data$point_id, 1), lag(epv_data$skq, 1), NA)
  epv_data$one_touch_future <- ifelse(epv_data$point_id == lead(epv_data$point_id, 1), lead(epv_data$skq, 1), NA)
  epv_data$two_touch_future <- ifelse(epv_data$point_id == lead(epv_data$point_id, 2), lead(epv_data$skq, 2), NA)

  #create possession data
  epv_data$possession <- NA
  epv_data$possession <- ifelse(epv_data$skill=="Serve", 1, epv_data$possession)
  epv_data$possession <- ifelse(epv_data$skill=="Reception", 2, epv_data$possession)
  x <- 0
  repeat {
    x = x+1
    epv_data$possession <- ifelse(epv_data$skill!="Serve" & epv_data$skill!="Reception" & lag(epv_data$team,1)==epv_data$team, lag(epv_data$possession), epv_data$possession)
    epv_data$possession <- ifelse(epv_data$skill!="Serve" & epv_data$skill!="Reception" & lag(epv_data$team,1)!=epv_data$team, lag(epv_data$possession) + 1, epv_data$possession)
    if (x == 50){
      break
    }
  }

  #create rally x possession combination
  epv_data$rally_possession <- paste0(epv_data$point_id, "-", epv_data$possession)

  #create contact count w/in each possession
  epv_data$contact <- NA
  epv_data$contact <- ifelse(epv_data$skill=="Serve" | epv_data$skill=="Reception", 1, epv_data$contact)
  x <- 0
  repeat {
    x = x+1
    epv_data$contact <- ifelse(epv_data$skill!="Serve" & epv_data$skill!="Reception" & lag(epv_data$possession,1)==epv_data$possession, lag(epv_data$contact) + 1, epv_data$contact)
    epv_data$contact <- ifelse(epv_data$skill!="Serve" & epv_data$skill!="Reception" & lag(epv_data$possession,1)!=epv_data$possession, 1, epv_data$contact)
    if (x == 10){
      break
    }
  }
  # sometimes opponent touch missing, so possession seems too long. Fix that...
  epv_data$contact <- ifelse(epv_data$contact == 5, 1, epv_data$contact)
  epv_data$contact <- ifelse(epv_data$contact == 6, 2, epv_data$contact)
  epv_data$contact <- ifelse(epv_data$contact == 7, 3, epv_data$contact)
  epv_data$contact <- ifelse(epv_data$contact == 8, 4, epv_data$contact)

  #create rally winner & rally loser
  epv_data$rally_winner <- ifelse(epv_data$team==epv_data$point_won_by, 1, 0)
  epv_data$rally_loser <- ifelse(epv_data$team!=epv_data$point_won_by, 1, 0)
  epv_data$rally_eff <- epv_data$rally_winner-epv_data$rally_loser

  #create unique set_id
  epv_data$set_id <- paste0(epv_data$match_id, "- Set ", epv_data$set_number)

  #create if team won the set overall
  epv_data <- epv_data[order(epv_data$id_touch),]
  epv_data$who_won_set <- NA
  epv_data$who_lost_set <- NA
  epv_data$who_won_set <- ifelse(epv_data$set_id!=lead(epv_data$set_id,1) & epv_data$team==epv_data$point_won_by, epv_data$team, epv_data$who_won_set)
  epv_data$who_won_set <- ifelse(epv_data$set_id!=lead(epv_data$set_id,1) & epv_data$team!=epv_data$point_won_by, epv_data$opponent, epv_data$who_won_set)
  epv_data$who_lost_set <- ifelse(epv_data$set_id!=lead(epv_data$set_id,1) & epv_data$team!=epv_data$point_won_by, epv_data$team, epv_data$who_lost_set)
  epv_data$who_lost_set <- ifelse(epv_data$set_id!=lead(epv_data$set_id,1) & epv_data$team==epv_data$point_won_by, epv_data$opponent, epv_data$who_lost_set)
  epv_data$who_won_set <- ifelse(epv_data$id_touch==max(epv_data$id_touch) & epv_data$team==epv_data$point_won_by, epv_data$team, epv_data$who_won_set)
  epv_data$who_won_set <- ifelse(epv_data$id_touch==max(epv_data$id_touch) & epv_data$team!=epv_data$point_won_by, epv_data$opponent, epv_data$who_won_set)
  epv_data$who_lost_set <- ifelse(epv_data$id_touch==max(epv_data$id_touch) & epv_data$team!=epv_data$point_won_by, epv_data$team, epv_data$who_lost_set)
  epv_data$who_lost_set <- ifelse(epv_data$id_touch==max(epv_data$id_touch) & epv_data$team==epv_data$point_won_by, epv_data$opponent, epv_data$who_lost_set)

  epv_data <- fill(epv_data, who_won_set, who_lost_set, .direction = c("up"))

  epv_data$teamwonset <- ifelse(epv_data$team==epv_data$who_won_set, 1, 0)
  epv_data$teamlostset <- ifelse(epv_data$team==epv_data$who_lost_set, 1, 0)
  epv_data$who_won_set <- NULL
  epv_data$who_lost_set <- NULL

  epv_data$passer_rating <- NA
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception #", 3, epv_data$passer_rating)
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception +", 3, epv_data$passer_rating)
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception !", 2, epv_data$passer_rating)
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception -", 1, epv_data$passer_rating)
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception /", 1, epv_data$passer_rating)
  epv_data$passer_rating <- ifelse(epv_data$skq=="Reception =", 0, epv_data$passer_rating)

  epv_data$gender <- ifelse(grepl("Men's", epv_data$team), "men", "women")

  epv_data$kcode <- NA
  epv_data$kcode <- ifelse((epv_data$skill %in% c("Reception", "Dig", "Freeball") & !is.na(lead(epv_data$set_code))) | (epv_data$skill == "Set" & !is.na(epv_data$set_code)), "yes", "no")


  #create column to denote if there is a winning or losing contact & fill in other touches within same possession if poss is won/lost
  winners <- c("Serve #", "Attack #", "Block #")
  losers <- c("Serve =", "Reception =", "Set =", "Attack =", "Attack /", "Block =", "Dig =", "Freeball =", "Cover =")
  epv_data$possession_winner <- ifelse(epv_data$skq %in% winners, 1, 0)
  epv_data$possession_winner <- ifelse(lead(epv_data$skq,1) %in% winners & epv_data$rally_possession==lead(epv_data$rally_possession, 1), 1, epv_data$possession_winner)
  epv_data$possession_winner <- ifelse(lead(epv_data$skq,2) %in% winners & epv_data$rally_possession==lead(epv_data$rally_possession, 2), 1, epv_data$possession_winner)
  epv_data$possession_winner <- ifelse(lead(epv_data$skq,3) %in% winners & epv_data$rally_possession==lead(epv_data$rally_possession, 3), 1, epv_data$possession_winner)

  epv_data$possession_loser <- ifelse(epv_data$skq %in% losers, 1, 0)
  epv_data$possession_loser <- ifelse(lead(epv_data$skq,1) %in% losers & epv_data$rally_possession==lead(epv_data$rally_possession, 1), 1, epv_data$possession_loser)
  epv_data$possession_loser <- ifelse(lead(epv_data$skq,2) %in% losers & epv_data$rally_possession==lead(epv_data$rally_possession, 2), 1, epv_data$possession_loser)
  epv_data$possession_loser <- ifelse(lead(epv_data$skq,3) %in% losers & epv_data$rally_possession==lead(epv_data$rally_possession, 3), 1, epv_data$possession_loser)

  #create efficiency w/in possession
  epv_data$poss_eff <- epv_data$possession_winner-epv_data$possession_loser

  #reorder properly...in case I did anything stupid
  epv_data <- epv_data[order(epv_data$id_touch),]

  #create input and output situations based on the logistics of sequential touches
  epv_data$input_type <- NA
  epv_data$input_type <- ifelse(epv_data$skill == "Serve", "serve_baseline", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Reception", "reception_baseline", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Set", "set_regular", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Attack" & lag(epv_data$team) != epv_data$team, "attack_overpass", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Attack" & lag(epv_data$team) == epv_data$team & lag(epv_data$skill == "Set"), "attack_regular", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Attack" & lag(epv_data$team) == epv_data$team & lag(epv_data$skill != "Set"), "attack_weird", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Attack" & lag(epv_data$team) == epv_data$team & lag(epv_data$skq == "Set -"), "attack_poor_set", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Block" & lag(epv_data$team, 2) == epv_data$team & lag(epv_data$team != epv_data$team) & lag(epv_data$skill) == "Attack", "block_overpass", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Block" & lag(epv_data$team) != epv_data$team & lag(epv_data$skill) == "Attack", "block_regular", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Block" & lag(epv_data$team) != epv_data$team & lag(epv_data$skill) != "Attack", "block_weird", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Dig" & lag(epv_data$team) != epv_data$team & lag(epv_data$skill) == "Attack" & lag(epv_data$team, 2) == epv_data$team, "dig_overpass", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Dig" & lag(epv_data$team) != epv_data$team & lag(epv_data$skill) == "Attack" & lag(epv_data$team, 2) != epv_data$team, "dig_regular", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$add_cover == "Dig" & lag(epv_data$team) == epv_data$team & lag(epv_data$skill) == "Block", "dig_blocktouch", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$add_cover == "Cover" & lag(epv_data$team) != epv_data$team & lag(epv_data$skill) == "Block", "cover_blocktouch", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Dig" & lag(epv_data$skill) != "Block" & lag(epv_data$skill) != "Attack", "dig_weird", epv_data$input_type)
  epv_data$input_type <- ifelse(epv_data$skill == "Freeball", "freeball_baseline", epv_data$input_type)

  epv_data$output_type <- NA
  epv_data$output_type <- ifelse(epv_data$skq == "Serve #", "serve_ace", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Serve =", "serve_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Serve" & epv_data$evaluation_code %!in% c("#", "=") & lead(epv_data$team, 2) != epv_data$team, "serve_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Serve" & epv_data$evaluation_code %!in% c("#", "=") & lead(epv_data$team, 2) == epv_data$team & lead(epv_data$skill, 2) == "Attack", "serve_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Serve" & epv_data$evaluation_code %!in% c("#", "=") & lead(epv_data$team, 2) == epv_data$team & lead(epv_data$skill, 2) != "Attack", "serve_overpass_non_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Reception =", "reception_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Reception" & epv_data$evaluation_code != "=" & lead(epv_data$team) == epv_data$team, "reception_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Reception" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) == "Attack", "reception_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Reception" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) != "Attack", "reception_overpass_non_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Set =", "set_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Set -", "set_poor", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Set #", "set_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Attack #", "attack_kill", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Attack =", "attack_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Attack /", "attack_blocked", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) == "Block" & lead(epv_data$team, 2) == epv_data$team, "attack_covered", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) == "Block" & lead(epv_data$team, 2) == epv_data$team & lead(epv_data$team, 3) != epv_data$team, "attack_covered_overpass", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) == "Block" & lead(epv_data$team, 2) != epv_data$team & lead(epv_data$team, 3) != epv_data$team, "attack_regular_block", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) != "Block" & lead(epv_data$team, 2) != epv_data$team & lead(epv_data$team) != epv_data$team, "attack_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) == "Block" & lead(epv_data$team, 2) != epv_data$team & lead(epv_data$team, 3) == epv_data$team & lead(epv_data$skill, 3) == "Attack", "attack_block_into_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) == "Block" & lead(epv_data$team, 2) != epv_data$team & lead(epv_data$team, 3) == epv_data$team & lead(epv_data$skill, 3) != "Attack", "attack_block_into_overpass_non_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) != "Block" & lead(epv_data$team, 2) == epv_data$team & lead(epv_data$team) != epv_data$team & lead(epv_data$skill, 2) == "Attack", "attack_into_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & epv_data$evaluation_code %!in% c("#", "=", "/") & lead(epv_data$skill) != "Block" & lead(epv_data$team, 2) == epv_data$team & lead(epv_data$team) != epv_data$team & lead(epv_data$skill, 2) != "Attack", "attack_into_overpass_non_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Attack" & lead(epv_data$team) == epv_data$team & lead(epv_data$point_id) == epv_data$point_id, "attack_incorrect_tagging", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Block #", "block_stuff", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Block =", "block_out", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Block !", "block_into_dig_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Block" & epv_data$evaluation_code %!in% c("#", "=", "!") & lead(epv_data$team) == epv_data$team, "block_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Block" & epv_data$evaluation_code %!in% c("#", "=", "!") & lead(epv_data$team) != epv_data$team, "block_covered", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Dig =", "dig_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Cover =", "cover_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Dig" & epv_data$evaluation_code != "=" & lead(epv_data$team) == epv_data$team, "dig_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Dig" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) == "Attack", "dig_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Dig" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) != "Attack", "dig_overpass_non_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skq == "Freeball =", "freeball_error", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Freeball" & epv_data$evaluation_code != "=" & lead(epv_data$team) == epv_data$team, "freeball_regular", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Freeball" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) == "Attack", "freeball_overpass_attack", epv_data$output_type)
  epv_data$output_type <- ifelse(epv_data$skill == "Freeball" & epv_data$evaluation_code != "=" & lead(epv_data$team) != epv_data$team & lead(epv_data$skill) != "Attack", "freeball_overpass_non_attack", epv_data$output_type)

  #reorder properly...in case I did anything stupid
  epv_data <- epv_data[order(epv_data$id_touch),]

  #add in XY coordinates based on the key contacts since these are how other touches are valued
  epv_data$input_coord_x <- NA
  epv_data$input_coord_x <- ifelse(epv_data$input_type == "set_regular", epv_data$start_coordinate_x, epv_data$input_coord_x)
  epv_data$input_coord_x <- ifelse(epv_data$input_type == "attack_regular", lag(epv_data$start_coordinate_x), epv_data$input_coord_x)
  epv_data$input_coord_x <- ifelse(epv_data$input_type == "block_regular", lag(epv_data$start_coordinate_x, 2), epv_data$input_coord_x)
  epv_data$input_coord_y <- NA
  epv_data$input_coord_y <- ifelse(epv_data$input_type == "set_regular", epv_data$start_coordinate_y, epv_data$input_coord_y)
  epv_data$input_coord_y <- ifelse(epv_data$input_type == "attack_regular", lag(epv_data$start_coordinate_y), epv_data$input_coord_y)
  epv_data$input_coord_y <- ifelse(epv_data$input_type == "block_regular", lag(epv_data$start_coordinate_y, 2), epv_data$input_coord_y)
  epv_data$input_kcode <- NA
  epv_data$input_kcode <- ifelse(epv_data$input_type == "set_regular", epv_data$kcode, epv_data$input_kcode)
  epv_data$input_kcode <- ifelse(epv_data$input_type == "attack_regular", lag(epv_data$kcode), epv_data$input_kcode)
  epv_data$input_kcode <- ifelse(epv_data$input_type == "block_regular", lag(epv_data$kcode, 2), epv_data$input_kcode)

  epv_data <- epv_data[order(epv_data$id_touch),]

  epv_data$output_coord_x <- NA
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "serve_regular", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "serve_overpass_non_attack", lead(epv_data$start_coordinate_x, 3), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "reception_regular", lead(epv_data$start_coordinate_x), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "reception_overpass_non_attack", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_covered", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_regular", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_regular_block", lead(epv_data$start_coordinate_x, 3), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_into_overpass_non_attack", lead(epv_data$start_coordinate_x, 3), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_block_into_overpass_non_attack", lead(epv_data$start_coordinate_x, 4), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "attack_incorrect_tagging", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "block_regular", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "block_covered", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "dig_regular", lead(epv_data$start_coordinate_x), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "dig_overpass_non_attack", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "freeball_regular", lead(epv_data$start_coordinate_x), epv_data$output_coord_x)
  epv_data$output_coord_x <- ifelse(epv_data$output_type == "freeball_overpass_non_attack", lead(epv_data$start_coordinate_x, 2), epv_data$output_coord_x)
  epv_data$output_coord_y <- NA
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "serve_regular", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "serve_overpass_non_attack", lead(epv_data$start_coordinate_y, 3), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "reception_regular", lead(epv_data$start_coordinate_y), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "reception_overpass_non_attack", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_covered", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_regular", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_regular_block", lead(epv_data$start_coordinate_y, 3), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_into_overpass_non_attack", lead(epv_data$start_coordinate_y, 3), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_block_into_overpass_non_attack", lead(epv_data$start_coordinate_y, 4), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "attack_incorrect_tagging", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "block_regular", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "block_covered", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "dig_regular", lead(epv_data$start_coordinate_y), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "dig_overpass_non_attack", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "freeball_regular", lead(epv_data$start_coordinate_y), epv_data$output_coord_y)
  epv_data$output_coord_y <- ifelse(epv_data$output_type == "freeball_overpass_non_attack", lead(epv_data$start_coordinate_y, 2), epv_data$output_coord_y)
  epv_data$output_kcode <- NA
  epv_data$output_kcode <- ifelse(epv_data$output_type == "serve_regular", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "serve_overpass_non_attack", lead(epv_data$kcode, 3), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "reception_regular", lead(epv_data$kcode), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "reception_overpass_non_attack", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_covered", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_regular", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_regular_block", lead(epv_data$kcode, 3), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_into_overpass_non_attack", lead(epv_data$kcode, 3), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_block_into_overpass_non_attack", lead(epv_data$kcode, 4), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "attack_incorrect_tagging", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "block_regular", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "block_covered", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "dig_regular", lead(epv_data$kcode), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "dig_overpass_non_attack", lead(epv_data$kcode, 2), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "freeball_regular", lead(epv_data$kcode), epv_data$output_kcode)
  epv_data$output_kcode <- ifelse(epv_data$output_type == "freeball_overpass_non_attack", lead(epv_data$kcode, 2), epv_data$output_kcode)

  epv_data$output_coord_x <- ifelse(epv_data$input_type == "set_regular", epv_data$input_coord_x, epv_data$output_coord_x)
  epv_data$output_coord_y <- ifelse(epv_data$input_type == "set_regular", epv_data$input_coord_y, epv_data$output_coord_y)
  epv_data$output_kcode <- ifelse(epv_data$input_type == "set_regular", epv_data$input_kcode, epv_data$output_kcode)

  epv_data$blockers <- NA
  epv_data$blockers <- ifelse(epv_data$num_players == "1 player block", "solo", epv_data$blockers)
  epv_data$blockers <- ifelse(epv_data$num_players == "2 player block", "double", epv_data$blockers)
  epv_data$blockers <- ifelse(epv_data$num_players == "3 player block", "triple", epv_data$blockers)
  epv_data$blockers <- ifelse(epv_data$num_players == "Hole block", "seam", epv_data$blockers)
  epv_data$blockers <- ifelse(epv_data$num_players == "No block", "none", epv_data$blockers)
  epv_data$blockers <- ifelse(epv_data$num_players == "Unexpected +", "solo", epv_data$blockers)

  epv_data <- epv_data[order(epv_data$id_touch),]
  epv_data$blockers <- ifelse(epv_data$skq == "Set #" & lead(epv_data$skill)=="Attack", lead(epv_data$blockers), epv_data$blockers)

  #for attacks without blockers listed, we randomly sample at the rate of the population
  a <- subset(epv_data, skq=="Set #" & is.na(blockers) & input_kcode=="yes")
  b <- subset(epv_data, skq=="Set #" & is.na(blockers) & input_kcode=="no")
  c <- subset(epv_data, skq!="Set #" | !is.na(blockers))
  a$blockers <- sample(c("double", "solo", "seam", "none", "triple"), size = nrow(a), replace = TRUE, prob = c(0.48,0.23,0.224,0.016,0.007))
  b$blockers <- sample(c("double", "solo", "seam", "none", "triple"), size = nrow(b), replace = TRUE, prob = c(0.724,0.097,0.063,0.075,0.012))
  epv_data <- rbind(a,b,c)
  epv_data <- epv_data[order(epv_data$id_touch),]
  rm(a,b,c)

  #create a feature for spike vs. non-spike
  epv_data$speed <- NA
  epv_data$speed <- ifelse(epv_data$skill=="Dig" & lag(epv_data$skill=="Block") & lag(epv_data$skill_subtype,2)=="Hard spike", "hard_spike", epv_data$speed)
  epv_data$speed <- ifelse(epv_data$skill=="Dig" & lag(epv_data$skill=="Block") & (lag(epv_data$skill_subtype,2)!="Hard spike" | is.na(lag(epv_data$skill_subtype,2))), "not_hard_spike", epv_data$speed)

  #create a feature for time between block touch and successive dig
  epv_data$block_time <- NA
  epv_data$block_time <- ifelse(epv_data$skill=="Dig" & lag(epv_data$skill=="Block"), epv_data$video_time - lag(epv_data$video_time), epv_data$block_time)
  epv_data$block_time <- ifelse(epv_data$block_time >= 2, "2+", epv_data$block_time)


  epv_data$input_type <- ifelse(is.na(epv_data$input_type) & epv_data$team == lag(epv_data$team), lag(epv_data$input_type), epv_data$input_type)



  # create models for situations that do not require XY coordinates, but the situation itself is defined and modeled as the situation
  serve_baseline_model <- mean(subset(epv_data, input_type == "serve_baseline")$rally_winner)
  attack_overpass_model <- mean(subset(epv_data, input_type == "attack_overpass")$rally_winner)
  #  attack_weird_model <- mean(subset(epv_data, input_type == "attack_weird")$rally_winner)
  attack_poor_set_model <- mean(subset(epv_data, input_type == "attack_poor_set")$rally_winner)
  block_weird_model <- mean(subset(epv_data, input_type == "block_weird")$rally_winner)
  dig_overpass_model <- mean(subset(epv_data, input_type == "dig_overpass")$rally_winner)
  dig_weird_model <- mean(subset(epv_data, input_type == "dig_weird")$rally_winner)
  freeball_baseline_model <- mean(subset(epv_data, input_type == "freeball_baseline")$rally_winner)
  set_poor_model <- mean(subset(epv_data, output_type == "set_poor")$rally_winner)
  block_touch_stuff_model <- glm(rally_winner ~ speed + block_time, subset(epv_data, skill=="Dig"), family = "binomial")


  #create models for attack eff based on location of the set
  a <- subset(epv_data, skq == "Set #" & !is.na(input_coord_x))
  fit_input_func <- function(a){
    fit_input <- try(lm(rally_winner ~ poly(input_coord_x,2) + poly(input_coord_y,2) + input_kcode, a), silent = TRUE)
    if (class(fit_input) == "try-error") {
      fit_input <- lm(rally_winner ~ poly(input_coord_x,2) + poly(input_coord_y,2), a)
    }
    return(fit_input)
  }

  b <- subset(epv_data, skq == "Set #" & !is.na(output_coord_x))
  fit_output_func <- function(b){
    fit_output <- try(lm(rally_winner ~ poly(output_coord_x,2) + poly(output_coord_y,2) + output_kcode, b), silent = TRUE)
    if (class(fit_output) == "try-error") {
      fit_output <- lm(rally_winner ~ poly(output_coord_x,2) + poly(output_coord_y,2), b)
    }
    return(fit_output)
  }

  fit_setoutput_func <- function(a){
    fit_set_output <- try(lm(rally_winner ~ poly(input_coord_x,2) + poly(input_coord_y,2) + input_kcode + blockers, a), silent = TRUE)
    if (class(fit_set_output) == "try-error") {
      fit_set_output <- lm(rally_winner ~ poly(input_coord_x,2) + poly(input_coord_y,2) + blockers, a)
    }
    return(fit_set_output)
  }

  fit_input <- fit_input_func(a)
  fit_output <- fit_output_func(b)
  fit_set_output <- fit_setoutput_func(a)





  epv_data$epv_in <- NA
  epv_data$epv_out <- NA
  epv_data$epv_added <- NA

  epv_data <- epv_data %>% relocate(player_name, skq, input_type, output_type, epv_in, epv_out, epv_added, match_date, team, opponent, skill, two_touch_ago, one_touch_ago, one_touch_future, two_touch_future, input_kcode, output_kcode, blockers)


  a <- subset(epv_data, !is.na(input_coord_x) | !is.na(output_coord_x))
  b <- subset(epv_data, is.na(input_coord_x) & is.na(output_coord_x))
  b$epv_in <- NA
  b$epv_out <- NA
  a$epv_in <- predict(fit_input, newdata = a)
  a$epv_out <- predict(fit_output, newdata = a)
  epv_data <- rbind(a,b)
  epv_data <- epv_data[order(epv_data$id_touch),]
  rm(a,b)


  a <- subset(epv_data, skq=="Set #" & !is.na(input_coord_x))
  b <- subset(epv_data, skq!="Set #" | is.na(input_coord_x))
  a$epv_out <- predict(fit_set_output, newdata = a)
  epv_data <- rbind(a,b)
  epv_data <- epv_data[order(epv_data$id_touch),]
  rm(a,b)

  epv_data$epv_out <- ifelse(epv_data$output_type %in% c("serve_ace",
                                                         "attack_kill",
                                                         "block_stuff"), 1.0, epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type %in% c("serve_error",
                                                         "reception_error",
                                                         "set_error",
                                                         "attack_error",
                                                         "attack_blocked",
                                                         "block_out",
                                                         "dig_error",
                                                         "cover_error",
                                                         "freeball_error"), 0.0, epv_data$epv_out)

  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "serve_baseline", serve_baseline_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "attack_overpass", attack_overpass_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "attack_poor_set", attack_poor_set_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "block_weird", block_weird_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "dig_overpass", dig_overpass_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "dig_weird", dig_weird_model, epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type == "freeball_baseline", freeball_baseline_model, epv_data$epv_in)

  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "serve_overpass_attack", attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "reception_overpass_attack", 1 - attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "dig_overpass_attack", 1 - attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "freeball_overpass_attack", 1 - attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "attack_block_into_overpass_attack", attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & epv_data$output_type == "attack_into_overpass_attack", attack_overpass_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "set_poor" & lead(epv_data$team) == epv_data$team, set_poor_model, epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "set_poor" & lead(epv_data$team) != epv_data$team & lead(epv_data$point_id) == epv_data$point_id, 1 - lead(epv_data$epv_in), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "set_poor" & lead(epv_data$point_id) != epv_data$point_id & lead(epv_data$team) != epv_data$team, 0.0, epv_data$epv_out)



  # things I'm adding Monday pre-volley
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$skill=="Dig" & lag(epv_data$skill=="Block"), predict(block_touch_stuff_model, epv_data, type = "response"), epv_data$epv_in)
  epv_data$epv_out <- ifelse(epv_data$skill == "Block" & lead(epv_data$skill == "Dig"), lead(epv_data$epv_in), epv_data$epv_out)


  epv_data$epv_in <- ifelse(epv_data$input_type == "attack_regular" & lag(epv_data$output_type == "set_regular"), lag(epv_data$epv_out), epv_data$epv_in)
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & epv_data$input_type=="dig_regular" & lag(epv_data$input_type)=="attack_regular", 1 - lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_in <- ifelse(epv_data$input_type == "attack_weird", lag(epv_data$epv_out), epv_data$epv_in)
  epv_data$epv_out <- ifelse(epv_data$output_type == "reception_overpass_non_attack" |
                               epv_data$output_type == "dig_overpass_non_attack" |
                               epv_data$output_type == "cover_overpass_non_attack" |
                               epv_data$output_type == "freeball_overpass_non_attack", 1 - lead(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$skill == "Serve" & lead(epv_data$skill == "Reception"), 1 - lead(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_in <- ifelse(epv_data$skill == "Reception" & epv_data$input_type == "reception_baseline", 1 - lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_in <- ifelse(epv_data$skill=="Block" & epv_data$input_type=="block_regular" & lag(epv_data$skill=="Attack"), 1 - lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_out <- ifelse(epv_data$output_type == "attack_regular_block", 1 - lead(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "attack_regular" & lead(epv_data$input_type == "dig_regular"), 1 - lead(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$skill=="Block" & epv_data$output_type=="block_regular" & lag(epv_data$output_type=="attack_regular_block"), 1 - lag(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "attack_covered" & lead(epv_data$input_type == "cover_blocktouch", 2), lead(epv_data$epv_in, 2), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type == "attack_covered_overpass", lead(epv_data$epv_in, 2), epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type=="block_covered", 1 - lag(epv_data$epv_out), epv_data$epv_out)
  epv_data$epv_in <- ifelse(epv_data$input_type == "dig_regular" & lag(epv_data$input_type == "attack_weird"), 1 - lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_in <- ifelse(epv_data$input_type == "dig_regular" & lag(epv_data$input_type == "attack_poor_set"), 1 - lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_out <- ifelse(epv_data$output_type == "block_into_dig_error", lead(epv_data$epv_in), epv_data$epv_out)

  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in) & lag(epv_data$team) == epv_data$team, lag(epv_data$epv_in), epv_data$epv_in)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out) & lag(epv_data$team) == epv_data$team, lag(epv_data$epv_out), epv_data$epv_out)



  epv_data$epv_in <- round(epv_data$epv_in, 3)
  epv_data$epv_out <- round(epv_data$epv_out, 3)
  epv_data$epv_added <- epv_data$epv_out - epv_data$epv_in
  epv_data$epv_added <- round(epv_data$epv_added, 3)
  epv_data <- epv_data %>% relocate(player_name, skq, input_type, output_type, epv_in, epv_out, match_date, team, opponent, skill, two_touch_ago, one_touch_ago, one_touch_future, two_touch_future, input_kcode, output_kcode, blockers)

  epv_data$epv_out <- ifelse(epv_data$output_type %in% c("serve_ace",
                                                         "attack_kill",
                                                         "block_stuff"), 1.0, epv_data$epv_out)
  epv_data$epv_out <- ifelse(epv_data$output_type %in% c("serve_error",
                                                         "reception_error",
                                                         "set_error",
                                                         "attack_error",
                                                         "attack_blocked",
                                                         "block_out",
                                                         "dig_error",
                                                         "cover_error",
                                                         "freeball_error"), 0.0, epv_data$epv_out)

  sum(is.na(epv_data$epv_in))
  sum(is.na(epv_data$epv_out))
  epv_data$epv_in <- ifelse(is.na(epv_data$epv_in), 0.5, epv_data$epv_in)
  epv_data$epv_out <- ifelse(is.na(epv_data$epv_out), 0.5, epv_data$epv_out)


  epv_data$home_epv_in <- ifelse(epv_data$team == epv_data$home_team, epv_data$epv_in, 1 - epv_data$epv_in)
  epv_data$home_epv_out <- ifelse(epv_data$team == epv_data$home_team, epv_data$epv_out, 1 - epv_data$epv_out)
  epv_data$home_epv_added <- epv_data$home_epv_out - epv_data$home_epv_in
  epv_data$visiting_epv_in <- ifelse(epv_data$team == epv_data$visiting_team, epv_data$epv_in, 1 - epv_data$epv_in)
  epv_data$visiting_epv_out <- ifelse(epv_data$team == epv_data$visiting_team, epv_data$epv_out, 1 - epv_data$epv_out)
  epv_data$visiting_epv_added <- epv_data$visiting_epv_out - epv_data$visiting_epv_in

  add_position <- function(data) {
    touches <- aggregate(count ~ team*player_name*year, data, sum)
    colnames(touches)[4] <- "totaltouches"
    sets <- aggregate(count ~ team*player_name*year, subset(data, skill=="Set"), sum)
    colnames(sets)[4] <- "sets"
    rec_dig <- aggregate(count ~ team*player_name*year, subset(data, skill %in% c("Dig", "Reception")), sum)
    colnames(rec_dig)[4] <- "rec_dig"
    a <- aggregate(count ~ team*player_name*year, subset(data, skill == "Attack"), sum)
    b <- aggregate(count ~ team*player_name*year, subset(data, skill == "Attack" & start_zone %in% c("2", "9")), sum)
    colnames(b)[4] <- "Opposite"
    c <- aggregate(count ~ team*player_name*year, subset(data, skill == "Attack" & start_zone %in% c("4", "8", "7")), sum)
    colnames(c)[4] <- "Outside"
    d <- aggregate(count ~ team*player_name*year, subset(data, skill == "Attack" & (start_zone == 3 | attack_code %in% c("X1", "X7", "XM", "X2", "CF", "CD"))), sum)
    colnames(d)[4] <- "Middle"

    total <- merge(touches, sets, all.x = TRUE)
    total <- merge(total, rec_dig, all.x = TRUE)
    total <- merge(total, a, all.x = TRUE)
    total <- merge(total, b, all.x = TRUE)
    total <- merge(total, c, all.x = TRUE)
    total <- merge(total, d, all.x = TRUE)

    total[is.na(total)] <- 0

    total$p2 <- total$Opposite/total$count
    total$p3 <- total$Middle/total$count
    total$p4 <- total$Outside/total$count
    total$position <- NA
    total$position <- ifelse(total$sets / total$totaltouches > 0.5, "Setter", total$position)
    total$position <- ifelse(is.na(total$position) & total$p2 > total$p3 & total$p2 > total$p4 & (total$count / total$totaltouches) > 0.2, "Opposite", total$position)
    total$position <- ifelse(is.na(total$position) & total$p3 > total$p2 & total$p3 > total$p4 & (total$count / total$totaltouches) > 0.2, "Middle", total$position)
    total$position <- ifelse(is.na(total$position) & total$p4 > total$p3 & total$p4 > total$p2 & (total$count / total$totaltouches) > 0.2, "Outside", total$position)
    total$position <- ifelse(is.na(total$position) & (total$rec_dig / total$totaltouches) > 0.45, "L/DS", total$position)
    total$position <- ifelse(is.na(total$position), "unsure", total$position)
    total <- select(total, team, year, player_name, position)
    return(total)
  }

  positions <- add_position(epv_data)
  epv_data <- merge(epv_data, positions, all.x = TRUE)

  epv_data <- merge(epv_data, volleyR::team_conference_logos, all.x = TRUE)
  epv_data <- merge(epv_data, volleyR::opp_conference_logos, all.x = TRUE)

  epv_data <- epv_data %>% relocate(player_name, skq, input_type, output_type, epv_in, epv_out, epv_added, position, match_date, team, conference, opponent, opp_conference, skill, two_touch_ago, one_touch_ago, one_touch_future, two_touch_future, input_kcode, output_kcode, blockers)
  epv_data <- epv_data[order(epv_data$id_touch),]
  print("Boom! Done.")
  return(epv_data)
}
