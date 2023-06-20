#' Import DataVolley Files
#'
#' Extracts the plays objects from a set of .dvw files and concatenates into one data frame
#'
#' @importFrom datavolley read_dv
#' @importFrom stringr str_detect
#'
#' @param file_list a vector of file names with .dvw extensions
#' @param verbose a logical indicating whether to print out the cumulative number of files imported
#'
#' @return a data frame containing the plays objects of each file in the set
#'
#' @export

vepv_import_files <- function(file_list, verbose = TRUE){

  dvw_ext <- stringr::str_detect(file_list, "\\.dvw$")

  dvw_files <- file_list[dvw_ext]

  n <- length(dvw_files)

  if(n < 1){
    stop("No DataVolley files found.")
  }

  plays_data <- vector("list", length = n)

  for(i in 1:length(dvw_files)) {
    x <- datavolley::read_dv(dvw_files[i], skill_evaluation_decode = "volleymetrics")
    x$plays$match_date <- x$meta$match$date
    plays_data[[i]] <- x$plays
    if(verbose){
      if((i %% 5 == 0) | (i == n)){
        cat(paste0(i, "/", n, " matches completed\n"))
      }
    }
  }
  epv_data <- dplyr::bind_rows(plays_data)

  return(epv_data)
}

#' Remove Non-Actions
#'
#' Removes the non-actions from a data frame containing plays objects
#'
#' @param plays the data frame containing the plays
#'
#' @return a subset of the data frame containing only the touches

vepv_remove_nonactions <- function(plays){

  #remove non-action plays
  plays_new <- subset(plays, substring(code, 2, 2)!="P" & substring(code, 2, 2)!="p"& substring(code, 2, 2)!="z" & substring(code, 2, 2)!="c" & substring(code, 2, 2)!="*" & substring(code, 2, 2)!="$")

  return(plays_new)
}

#' Add Touch ID
#'
#' Adds a column indicating the row number in the original dataset of plays
#'
#' @param plays the data frame containing the plays
#'
#' @return the dataset with a new column `id_touch`


vepv_add_touch_id <- function(plays){

  return(data.frame(plays, id_touch = seq.int(nrow(plays))))

}

#' Add year
#'
#' Adds a column indicating the match year to a dataset of plays
#'
#' @param plays the data frame containing the plays
#'
#' @return the dataset with a new column `year`

vepv_add_year <- function(plays){

  return(data.frame(plays, year = as.numeric(format(plays$match_date, "%Y"))))

}

#' Adds a column indicating the match year to a dataset of plays
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate case_when
#'
#' @param plays the data frame containing the plays
#'
#'
#' @return the dataset with a new column `opponent`

vepv_add_opponent <- function(plays){

  plays <- plays |> mutate(
    opponent = dplyr::case_when(
      .data$team == .data$home_team ~ .data$visiting_team,
      .data$team == .data$visiting_team ~ .data$home_team,
      TRUE ~ NA_character_
    )
  )

  return(plays)
}

#' Recode skills
#'
#' Add a skill-quality combination column to a dataset and recode problematic skills, e.g., add a "Cover" skill
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom rlang .data
#' @importFrom dplyr if_else mutate lag
#'
#' @return the dataset with updated `evaluation_code` and `skill` columns and a new `skq` column for skill-quality combination

vepv_recode_skills <- function(plays){

  plays <- plays |> dplyr::mutate(
    evaluation_code = dplyr::if_else(.data$skill=="Set" & .data$evaluation_code == "+", "#", .data$evaluation_code),
    skill = dplyr::if_else(.data$skill=="Dig" & lag(.data$skill, 1)=="Block" & dplyr::lag(.data$team, 2)==.data$team, "Cover", .data$skill),
    skq = paste(.data$skill, .data$evaluation_code, sep = " ")
  )

  return(plays)
}
# NTS: we no longer have add_cover we have skill including Cover

#' Possession and Contact Numbers
#'
#' Adds possession numbers and contact number within possession
#'
#' Add a skill-quality combination column to a dataset and recode problematic skills, e.g., add a "Cover" skill
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate select group_by if_else case_when lag n
#'
#' @return the dataset with new `skq` column for skill-quality combination


vepv_add_possession_contacts <- function(plays){

  output <- plays |> dplyr::group_by(match_id, point_id) |>  # grab match-points
    dplyr::mutate(team_switch = (.data$team != dplyr::lag(.data$team) | is.na(dplyr::lag(.data$team))),  # find where the team changes
           possession_start = dplyr::case_when(  # find touch where possession starts
             is.na(.data$skill) ~ NA,  # if skill is NA then no touch
             .data$skill == "Serve" ~ TRUE, # Serve is its own possession
             .data$skq %in% c("Dig =", "Cover =", "Reception =", "Freeball =") ~ FALSE,
             #.data$skill %in% c("Dig", "Cover", "Reception", "Freeball") & .data$evaluation_code == "=" ~ FALSE,
             dplyr::lag(.data$skill, 1) == "Block" ~ TRUE,  # possession always changes after a block
             .data$team_switch & .data$skill != "Block" ~ TRUE, # possession changes if new team touches unless it's a block
             TRUE ~ FALSE  # if otherwise, it's not a possession-starting touch
           ),
           possession = cumsum(.data$possession_start & !is.na(.data$possession_start))  # counts possessions in the point; serve = 1, reception/first ball = 2
    ) |> dplyr::group_by(match_id, point_id, possession) |>
    dplyr::mutate(contact = seq(1, dplyr::n())
    ) |> dplyr::ungroup() |>
    dplyr::mutate(contact = dplyr::if_else(is.na(.data$skill), NA_integer_, .data$contact)) |>
    dplyr::select(-team_switch, -possession_start)  # get rid of confusing team_switch variable

  return(output)

}

#' Add touch inputs and outputs
#'
#' Adds the touch input and the touch result (output)
#'
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom dplyr mutate case_when lag lead
#'
#' @return the same data frame with `input_type` and `output_type` columns
#'
#' @export

vepv_touch_input_output <- function(plays){

  output <-  plays |> dplyr::mutate(
    input_type = dplyr::case_when(
      .data$skill == "Serve" ~ "serve_baseline",
      .data$skill == "Reception" ~ "reception_baseline",
      .data$skill == "Set" ~ "set_regular",
      .data$skill == "Attack" & dplyr::lag(.data$team, 1) != .data$team ~ "attack_overpass",
      .data$skill == "Attack" & dplyr::lag(.data$team, 1) == .data$team & dplyr::lag(.data$skill != "Set") ~ "attack_weird", # attack on two
      .data$skill == "Attack" & dplyr::lag(.data$team, 1) == .data$team & dplyr::lag(.data$skq == "Set -") ~ "attack_poor_set",
      .data$skill == "Attack" & dplyr::lag(.data$team, 1) == .data$team & dplyr::lag(.data$skill, 1) == "Set"  ~ "attack_regular",
      .data$skill == "Block" & dplyr::lag(.data$team, 2) == .data$team & dplyr::lag(.data$team, 1) != .data$team & dplyr::lag(.data$skill, 1) == "Attack" ~ "block_overpass",
      .data$skill == "Block" & dplyr::lag(.data$team, 1) != .data$team & dplyr::lag(.data$skill, 1) != "Attack" ~ "block_weird",
      .data$skill == "Block" & dplyr::lag(.data$team, 1) != .data$team & dplyr::lag(.data$skill, 1) == "Attack" ~ "block_regular",
      .data$skill == "Dig" & dplyr::lag(.data$team, 1) != .data$team & dplyr::lag(.data$skill, 1) == "Attack" & dplyr::lag(.data$team, 2) == .data$team ~ "dig_overpass",
      .data$skill == "Dig" & dplyr::lag(.data$team, 1) == .data$team & dplyr::lag(.data$skill, 1) == "Block" ~ "dig_blocktouch",
      .data$skill == "Cover" & dplyr::lag(.data$team) != .data$team & dplyr::lag(.data$skill) == "Block" ~ "cover_blocktouch",
      .data$skill == "Dig" & dplyr::lag(.data$skill) != "Block" & dplyr::lag(.data$skill) != "Attack" ~ "dig_weird",
      .data$skill == "Dig" & dplyr::lag(.data$team) != .data$team & dplyr::lag(.data$skill) == "Attack" & dplyr::lag(.data$team, 2) != .data$team ~ "dig_regular",
      .data$skill == "Freeball" ~ "freeball_baseline",
      TRUE ~ NA_character_
    ),
    output_type = dplyr::case_when(
      .data$skq == "Serve #" ~ "serve_ace",
      .data$skq == "Serve =" ~ "serve_error",
      .data$skill == "Serve" & dplyr::lead(.data$team, 2) == .data$team & dplyr::lead(.data$skill, 2) == "Attack" ~ "serve_overpass_attack",
      .data$skill == "Serve" & dplyr::lead(.data$team, 2) == .data$team & dplyr::lead(.data$skill, 2) != "Attack" ~ "serve_overpass_non_attack",
      .data$skill == "Serve"  ~ "serve_regular",
      .data$skq == "Reception =" ~ "reception_error",
      .data$skill == "Reception" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) == "Attack" ~ "reception_overpass_attack",
      .data$skill == "Reception" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) != "Attack" ~ "reception_overpass_non_attack",
      .data$skill == "Reception" ~ "reception_regular",
      .data$skq == "Set =" ~ "set_error",
      .data$skq == "Set -" ~ "set_poor",
      .data$skill == "Set" ~ "set_regular",
      .data$skq == "Attack #" ~ "attack_kill",
      .data$skq == "Attack =" ~ "attack_error",
      .data$skq == "Attack /" ~ "attack_blocked",
      .data$skq == "Attack" & dplyr::lead(.data$skill, 1) == "Block" & dplyr::lead(.data$team, 2) == .data$team & dplyr::lead(.data$team, 3) != .data$team ~ "attack_covered_overpass",
      .data$skq == "Attack" & dplyr::lead(.data$skill, 1) == "Block" & dplyr::lead(.data$team, 2) == .data$team ~ "attack_covered",
      .data$skill == "Attack" & dplyr::lead(.data$skill, 1) == "Block" & dplyr::lead(.data$team, 2) != .data$team & dplyr::lead(.data$team, 3) == .data$team & dplyr::lead(.data$skill, 3) == "Attack" ~ "attack_block_into_overpass_attack",
      .data$skill == "Attack" & dplyr::lead(.data$skill, 1) == "Block" & dplyr::lead(.data$team, 2) != .data$team & dplyr::lead(.data$team, 3) == .data$team & dplyr::lead(.data$skill, 3) != "Attack" ~ "attack_block_into_overpass_non_attack",
      .data$skill == "Attack" & dplyr::lead(.data$skill, 1) == "Block" ~ "attack_block_regular",
      .data$skill == "Attack" & dplyr::lead(.data$team, 2) == .data$team & dplyr::lead(.data$team) != .data$team & dplyr::lead(.data$skill, 2) == "Attack" ~ "attack_into_overpass_attack",
      .data$skill == "Attack" & dplyr::lead(.data$team, 2) == .data$team & dplyr::lead(.data$team) != .data$team & dplyr::lead(.data$skill, 2) != "Attack" ~ "attack_into_overpass_non_attack",
      .data$skill == "Attack" & dplyr::lead(.data$team, 1) == .data$team & dplyr::lead(.data$point_id, 1) == .data$point_id ~ "attack_incorrect_tagging",
      .data$skill == "Attack" ~ "attack_regular",
      .data$skq == "Block #" ~ "block_stuff",
      .data$skq == "Block =" ~ "block_out",
      .data$skq == "Block !" ~ "block_into_dig_error",
      .data$skill == "Block" & dplyr::lead(.data$team, 1) != .data$team ~ "block_covered",
      .data$skill == "Block" ~ "block_regular",
      .data$skq == "Dig =" ~ "dig_error",
      .data$skill == "Dig" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) == "Attack" ~ "dig_overpass_attack",
      .data$skill == "Dig" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) != "Attack" ~ "dig_overpass_non_attack",
      .data$skill == "Dig" ~ "dig_regular",
      .data$skq == "Cover =" ~ "cover_error",
      .data$skill == "Cover" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) == "Attack" ~ "dig_overpass_attack", # keeping these - Chad's code implies equivalent
      .data$skill == "Cover" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) != "Attack" ~ "dig_overpass_non_attack",
      .data$skill == "Cover" ~ "cover_regular",
      .data$skq == "Freeball =" ~ "freeball_error",
      .data$skill == "Freeball" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) == "Attack" ~ "freeball_overpass_attack",
      .data$skill == "Freeball" & dplyr::lead(.data$team, 1) != .data$team & dplyr::lead(.data$skill, 1) != "Attack" ~ "freeball_overpass_non_attack",
      .data$skill == "Freeball" ~ "freeball_regular",
      TRUE ~ NA_character_
    )
  )

  output <- output |> dplyr::mutate(
    input_type = dplyr::if_else(is.na(.data$input_type) & .data$team == dplyr::lag(.data$team, 1), dplyr::lag(.data$input_type, 1), .data$input_type)
  )

  return(output)

}

#' Add touch coordinates
#'
#' Adds the input and output x and y-coordinates for the touch
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom dplyr mutate case_when lag lead if_else
#'
#' @return the same data frame with `input_coord_x`, `input_coord_y`, `output_coord_x`, and `output_coord_y` columns
#'
#' @export

vepv_touch_coordinates <- function(plays){
  output <- plays |> dplyr::mutate(
    input_coord_x = dplyr::case_when(
      .data$input_type == "set_regular" ~ .data$start_coordinate_x,
      .data$input_type == "attack_regular" ~ dplyr::lag(.data$start_coordinate_x, 1),
      .data$input_type == "block_regular" ~ dplyr::lag(.data$start_coordinate_x, 2),
      TRUE ~ NA_real_
    ),
    input_coord_y = dplyr::case_when(
      .data$input_type == "set_regular" ~ .data$start_coordinate_y,
      .data$input_type == "attack_regular" ~ dplyr::lag(.data$start_coordinate_y, 1),
      .data$input_type == "block_regular" ~ dplyr::lag(.data$start_coordinate_y, 2),
      TRUE ~ NA_real_
    ),
    output_coord_x = dplyr::case_when(
      .data$output_type %in% c("reception_regular", "dig_regular", "freeball_regular") ~ dplyr::lead(.data$start_coordinate_x, 1),
     .data$output_type %in% c("serve_regular", "reception_overpass_non_attack", "attack_covered", "attack_regular", "attack_incorrect_tagging", "block_regular", "block_covered", "dig_overpass_non_attack", "freeball_overpass_non_attack") ~ dplyr::lead(.data$start_coordinate_x, 2),
     .data$output_type %in% c("serve_overpass_non_attack", "attack_regular_block", "attack_into_overpass_non_attack") ~ dplyr::lead(.data$start_coordinate_x, 3),
     .data$output_type == "attack_block_into_overpass_non_attack" ~ dplyr::lead(.data$start_coordinate_x, 4),
     TRUE ~ NA_real_
    ),
    output_coord_y = dplyr::case_when(
      .data$output_type %in% c("reception_regular", "dig_regular", "freeball_regular") ~ dplyr::lead(.data$start_coordinate_y, 1),
      .data$output_type %in% c("serve_regular", "reception_overpass_non_attack", "attack_covered", "attack_regular", "attack_incorrect_tagging", "block_regular", "block_covered", "dig_overpass_non_attack", "freeball_overpass_non_attack") ~ dplyr::lead(.data$start_coordinate_y, 2),
      .data$output_type %in% c("serve_overpass_non_attack", "attack_regular_block", "attack_into_overpass_non_attack") ~ dplyr::lead(.data$start_coordinate_y, 3),
      .data$output_type == "attack_block_into_overpass_non_attack" ~ dplyr::lead(.data$start_coordinate_y, 4),
      TRUE ~ NA_real_
    )
  )


  output <- output |> dplyr::mutate(
    output_coord_x = dplyr::if_else(.data$input_type == "set_regular", .data$input_coord_x, .data$output_coord_x),
    output_coord_y = dplyr::if_else(.data$input_type == "set_regular", .data$input_coord_y, .data$output_coord_y)
  )

  return(output)
}

#' Add k-code
#'
#' Adds a variable indicating whether the touch leads to/comes from a set with a k-code
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom dplyr mutate case_when lag lead if_else
#'
#' @return the same data frame with `input_kcode` and `output_kcode` columns
#'
#' @export

vepv_touch_kcode <- function(plays){

  output <- plays |> dplyr::mutate(
    kcode = dplyr::if_else((.data$skill %in% c("Reception", "Dig", "Cover", "Freeball") & !is.na(dplyr::lead(.data$set_code, 1))) | (.data$skill == "Set" & !is.na(.data$set_code)), "yes", "no")
  )

  output <- output |> dplyr::mutate(
    input_kcode = dplyr::case_when(
      .data$input_type == "set_regular" ~ .data$kcode,
      .data$input_type == "attack_regular" ~ dplyr::lag(.data$kcode, 1),
      .data$input_type == "block_regular" ~ dplyr::lag(.data$kcode, 2),
      TRUE ~ "no" # if any other type then there shouldn't be a kcode for the input
    ),
    output_kcode = dplyr::case_when(
      .data$output_type %in% c("reception_regular", "dig_regular", "freeball_regular") ~ dplyr::lead(.data$kcode, 1),
      .data$output_type %in% c("serve_regular", "reception_overpass_non_attack", "attack_covered", "attack_regular", "attack_incorrect_tagging", "block_covered", "block_regular", "dig_overpass_non_attack", "freeball_overpass_non_attack") ~ dplyr::lead(.data$kcode, 2),
      .data$output_type %in% c("serve_overpass_non_attack", "attack_regular_block", "attack_into_overpass_non_attack") ~ dplyr::lead(.data$kcode, 3),
      .data$output_type %in% c("attack_block_overpass_into_non_attack") ~ dplyr::lead(.data$kcode, 4),
      TRUE ~ "no" # if any other type then there shouldn't be a kcode for the output
    )
  )

  output <- output |> dplyr::mutate(
    output_kcode = dplyr::if_else(.data$input_type == "set_regular", .data$input_kcode, .data$output_kcode)
  )

  return(output)

}


#' Add number of blockers
#'
#' Adds the number of blockers
#'
#'
#' @param plays the data frame containing the plays
#' @param impute a logical variable indicating whether to impute (TRUE) or not (FALSE) a value for the number of blockers when it is missing
#' @param impute_seed the seed to use for the imputation
#' @param impute_probs a data frame containing three columns:
#' types of blocks, probability of facing each type of block when the attack has a k-code and probability of facing each type of block when the attack does not have a k-code
#'
#' @importFrom dplyr mutate case_when lag lead
#'
#' @return the same data frame with `input_type` and `output_type` columns
#'
#' @export

vepv_blockers <- function(plays, impute = TRUE, impute_seed = 100,
                          impute_probs = data.frame(types = c("double", "solo", "triple", "seam", "none"), yes = c(0.48,0.23,0.224,0.016,0.007), no = c(0.724,0.097,0.063,0.075,0.012))){

  output <- plays |> dplyr::mutate(
    blockers = dplyr::case_when(
      .data$num_players %in% c("1 player block", "Unexpected +") ~ "solo",
      .data$num_players == "2 player block" ~ "double",
      .data$num_players == "3 player block" ~ "triple",
      .data$num_players == "Hole block" ~ "seam",
      .data$num_players == "No block" ~ "none",
      TRUE ~ NA_character_
    )
  )

  set.seed(impute_seed) # for reproducibility of imputation

  n <- nrow(output)

  output <- output |> dplyr::mutate(
    blockers = dplyr::case_when(
      .data$skq == "Set #" & dplyr::lead(.data$skill, 1) == "Attack" ~ dplyr::lead(.data$blockers, 1),
      .data$skq == "Set #" & is.na(.data$blockers) & .data$input_kcode == "yes" & impute ~ sample(impute_probs[,1], size = n, replace = TRUE, prob = impute_probs[,2]),
      .data$skq == "Set #" & is.na(.data$blockers) & .data$input_kcode == "yes" ~ "unknown",
      .data$skq == "Set #" & is.na(.data$blockers) & .data$input_kcode == "no" & impute ~ sample(impute_probs[,1], size = n, replace = TRUE, prob = impute_probs[,3]),
      .data$skq == "Set #" & is.na(.data$blockers) & .data$input_kcode == "no" ~ "unknown",
      TRUE ~ .data$blockers
    )
  )

  return(output)

}


#' Add dig difficulty
#'
#' Adds variables related to the difficulty of the dig
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom dplyr mutate case_when lag if_else
#'
#' @return the same data frame with `speed`, `block_touch`, and `reaction_time` columns. `speed` represents whether the previous attack was a hard spike or not,
#' `block_touch` indicates whether the dig was directly off an attack or off a block-touch,
#' and `reaction_time` indicates the (approximate) time between the previous touch and the dig
#'
#' @export

vepv_dig_difficulty <- function(plays){

  output <- plays |> dplyr::mutate(
    speed = dplyr::case_when(
      .data$skill %in% c("Dig", "Cover") & dplyr::lag(.data$skill, 1)=="Block" & dplyr::lag(.data$skill_subtype, 2) == "Hard spike" ~ "hard_spike",
      .data$skill=="Dig" & dplyr::lag(.data$skill, 1)=="Attack" & dplyr::lag(.data$skill_subtype, 1) == "Hard spike" ~ "hard_spike",
      .data$skill %in% c("Dig", "Cover")  ~ "not_hard_spike",
      TRUE ~ NA_character_
    ),
    block_touch = dplyr::if_else(.data$skill %in% c("Dig", "Cover") & dplyr::lag(.data$skill, 1) == "Block" & dplyr::lag(.data$skill, 2) == "Attack", "yes", "no", missing = NA_character_),
    reaction_time = dplyr::if_else(.data$skill %in% c("Dig", "Cover"), .data$video_time - dplyr::lag(.data$video_time), NA_real_)
  )

  output <- output |> dplyr::mutate(
    reaction_time = dplyr::if_else(.data$reaction_time > 2, 2, .data$reaction_time, missing = NA_real_)
  )

  return(output)
}

#' Add rally winner
#'
#' Adds a variable indicating whether the point was won by the team touching the ball
#'
#' @param plays the data frame containing the plays
#'
#' @importFrom dplyr mutate case_when if_else
#'
#' @return the same data frame with `rally_winner` and `rally_eff` columns. `rally_winner` indicates whether the touching team won the point (1) or not (0),
#' and `rally_eff` converts this to an efficiency (1 or -1)
#'
#' @export

vepv_add_rally_winner <- function(plays){

  output <- plays |> dplyr::mutate(
    rally_winner = dplyr::if_else(.data$team == .data$point_won_by, 1, 0),
    rally_eff = dplyr::case_when(
      .data$team == .data$point_won_by ~ 1,
      .data$team != .data$point_won_by ~ -1,
      TRUE ~ 0
    )
  )

  return(output)
}

