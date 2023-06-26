#' Simple mean model
#'
#' Calculates expected rally efficiency using a simple proportion of these types of touches that result in winning the point.
#' Useful for touches that either don't have useful predictors or can't make use of them because they're coded weird.
#'
#' @param plays the data frame containing the plays
#' @param touch_type the type of touch (as indicated by the `input_type` column of `plays`)
#' @param impute_value the value to impute for the rally efficiency for this type of touch, if not estimated directly from data
#'
#' @return the proportion of these touches that eventually resulted in winning the point
#'
#' @export

vepv_mean_model <- function(plays, touch_type, impute_value = NA_real_){

  if(!is.na(impute_value) & !is.null(impute_value) & length(impute_value) > 0){
    return(impute_value)
  } else {

    plays_subset <- subset(plays, input_type == touch_type)

    return(mean(plays_subset$rally_winner))
  }

}

#' Input and Output Models
#'
#' Calculates expected rally efficiency using a logistic regression model, which depends on the touch type
#'
#' @rdname vepv_location_model
#'
#' @param plays the data frame containing the plays
#' @param touch_type the type of touch (as indicated by the `input_type` or `output_type` column of `plays`)
#' @param trained_model a pre-trained model (of class "`glm`") used to make predictions
#'
#' @return the proportion of these touches that eventually resulted in winning the point
#'
#' @export

vepv_input_location_model <- function(plays, touch_type, trained_model = NULL){

  if(!is.na(touch_type)){
    plays_subset <- subset(plays, input_type == touch_type)
  } else {
    plays_subset <- subset(plays, !(input_type %in% c("set_regular", "dig_blocktouch", "cover_blocktouch")))
  }


  if(data.class(trained_model) == "glm"){

    eff_model <- update(trained_model, data = plays_subset, na.action = na.exclude)

  } else {

    predictor_variables <- dplyr::case_when(
      input_type %in% c("dig_blocktouch", "cover_blocktouch") ~ "speed + block_touch + reaction_time",
      input_type == "set_regular" ~ "poly(input_coord_x, 4) + poly(input_coord_y, 2) + input_kcode",
      TRUE ~ "poly(input_coord_x, 4) + poly(input_coord_y, 2)"
    )

    glm_formula <- as.formula(paste("rally_winner ~", predictor_variables))

    eff_model <- glm(glm_formula, data = plays_subset, family = binomial(link = "logit"), na.action = na.exclude)

  }

  return(predict(eff_model, plays, type = "response"))

}

#' @rdname vepv_location_model
vepv_output_location_model <- function(plays, touch_type, trained_model = NULL){

  if(!is.na(touch_type)){
    plays_subset <- subset(plays, output_type == touch_type)
  } else {
    plays_subset <- subset(plays, !(output_type %in% c("set_regular")))
  }

  predictor_variables <-  dplyr::case_when(
    touch_type == "set_regular" ~ "poly(output_coord_x, 4) + poly(output_coord_y, 2) + output_kcode + blockers",
    TRUE ~ "poly(output_coord_x, 4) + poly(output_coord_y, 2) + output_kcode"
  )

  glm_formula <- as.formula(paste("rally_winner ~", predictor_variables))

  eff_model <- glm(glm_formula, data = plays, family = binomial(link = "logit"), na.action = na.exclude)

  return(predict(eff_model, plays, type = "response"))

}

#' Expected Point Value
#'
#' Calculates expected rally efficiency for each touch
#'
#' @param plays the data frame containing the plays
#' @param touch_epv a data frame with two columns: `touch`, the type of touch (as indicated by`input_type` or `output_type`), and the expected value of that touch to impute.
#' If `NULL`, or for any touch types not found in `touch_epv`, a value will be imputed using the `plays` data
#'
#' @return a data frame containing input, output, and added expected point value for each touch
#'
#' @export

vepv_epv <- function(plays, touch_epv = NULL){

  plays_out2 <- plays %>% dplyr::mutate(
    epv_in = NA_real_,
    epv_out = NA_real_
  )

  iter <- 1

  # up to 5 iterations - if we still somehow have NAs after going through the loop 5 times, something's seriously wrong
  while((any(is.na(plays_out2$epv_in) | is.na(plays_out2$epv_out))) & iter <= 5){
    if(data.class(touch_epv) == "data.frame"){

      # First pass - input models
      plays_in <- plays_out2 |> dplyr::mutate(
        epv_in = dplyr::case_when(
          is.na(.data$epv_in) & .data$input_type == "serve_baseline" ~ vepv_mean_model(plays, touch_type = "serve_baseline", impute_value = touch_epv$epv[touch_epv$touch == "serve_baseline"]),
          is.na(.data$epv_in) & .data$input_type == "attack_overpass" ~ vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = touch_epv$epv[touch_epv$touch == "attack_overpass"]),
          is.na(.data$epv_in) & .data$input_type == "attack_weird" ~ vepv_mean_model(plays, touch_type = "attack_weird", impute_value = touch_epv$epv[touch_epv$touch == "attack_weird"]),
          is.na(.data$epv_in) & .data$input_type == "attack_poor_set" ~ vepv_mean_model(plays, touch_type = "attack_poor_set", impute_value = touch_epv$epv[touch_epv$touch == "attack_poor_set"]),
          is.na(.data$epv_in) & .data$input_type == "block_weird" ~ vepv_mean_model(plays, touch_type = "block_weird", impute_value = touch_epv$epv[touch_epv$touch == "block_weird"]),
          is.na(.data$epv_in) & .data$input_type == "dig_overpass" ~ vepv_mean_model(plays, touch_type = "dig_overpass", impute_value = touch_epv$epv[touch_epv$touch == "dig_overpass"]),
          is.na(.data$epv_in) & .data$input_type == "dig_weird" ~ vepv_mean_model(plays, touch_type = "dig_weird", impute_value = touch_epv$epv[touch_epv$touch == "dig_weird"]),
          is.na(.data$epv_in) & .data$input_type == "freeball_baseline" ~ vepv_mean_model(plays, touch_type = "freeball_baseline", impute_value = touch_epv$epv[touch_epv$touch == "freeball_baseline"]),
          is.na(.data$epv_in) & .data$input_type == "set_poor" ~ vepv_mean_model(plays, touch_type = "set_poor", impute_value = touch_epv$epv[touch_epv$touch == "set_poor"]),
          is.na(.data$epv_in) & .data$input_type %in% c("dig_blocktouch", "cover_blocktouch") ~ vepv_input_location_model(plays, touch_type = "dig_blocktouch"),
          is.na(.data$epv_in) & .data$input_type == "set_regular" & !is.na(.data$input_coord_x) & !is.na(.data$input_coord_y) ~ vepv_input_location_model(plays, touch_type = "set_regular"),
          is.na(.data$epv_in) & .data$input_type == "set_regular" ~ vepv_input_location_model(plays, touch_type = "set_regular", trained_model = glm(rally_winner ~ blockers, data = plays, subset = (input_type == "set_regular"))),
          is.na(.data$epv_in) & !is.na(.data$input_coord_x) & !is.na(.data$input_coord_y) ~ vepv_input_location_model(plays, touch_type = NA_character_),
          TRUE ~ .data$epv_in
        )
      )

      # first pass - output models
      plays_out <- plays_in |> dplyr::mutate(
        epv_out = dplyr::case_when(
          is.na(.data$epv_out) & .data$output_type %in% c("serve_ace", "attack_kill", "block_stuff") ~ 1.0,
          is.na(.data$epv_out) & .data$output_type %in% c("serve_error", "reception_error", "set_error", "attack_error", "attack_blocked", "block_out", "dig_error", "cover_error", "freeball_error") ~ 0.0,
          is.na(.data$epv_out) & .data$output_type %in% c("serve_overpass_attack", "attack_block_into_overpass_attack", "attack_into_overpass_attack") ~ vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = touch_epv$epv[touch_epv$touch == "attack_overpass"]),
          is.na(.data$epv_out) & .data$output_type %in% c("reception_overpass_attack", "dig_overpass_attack", "freeball_overpass_attack") ~ (1 - vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = touch_epv$epv[touch_epv$touch == "attack_overpass"])),
          is.na(.data$epv_out) & .data$output_type == "set_poor" & .data$team == dplyr::lead(.data$team, 1) ~ vepv_mean_model(plays, touch_type = "set_poor", impute_value = touch_epv$epv[touch_epv$touch == "set_poor"]),
          is.na(.data$epv_out) & .data$output_type == "set_poor" & .data$point_id == dplyr::lead(.data$point_id, 1) ~ 1 - dplyr::lead(.data$epv_in, 1),
          is.na(.data$epv_out) & .data$output_type == "set_poor" ~ 0.0,
          is.na(.data$epv_out) & .data$skill == "Block" & dplyr::lead(.data$skill, 1) %in% c("Dig", "Cover") ~ dplyr::lead(.data$epv_in, 1),
          TRUE ~ .data$epv_out

        )
      )

    } else {
      # First pass - input models
      plays_in <- plays_out2 |> dplyr::mutate(
        epv_in = dplyr::case_when(
          is.na(.data$epv_in) & .data$input_type == "serve_baseline" ~ vepv_mean_model(plays, touch_type = "serve_baseline", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "attack_overpass" ~ vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "attack_weird" ~ vepv_mean_model(plays, touch_type = "attack_weird", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "attack_poor_set" ~ vepv_mean_model(plays, touch_type = "attack_poor_set", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "block_weird" ~ vepv_mean_model(plays, touch_type = "block_weird", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "dig_overpass" ~ vepv_mean_model(plays, touch_type = "dig_overpass", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "dig_weird" ~ vepv_mean_model(plays, touch_type = "dig_weird", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "freeball_baseline" ~ vepv_mean_model(plays, touch_type = "freeball_baseline", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type == "set_poor" ~ vepv_mean_model(plays, touch_type = "set_poor", impute_value = NA_real_),
          is.na(.data$epv_in) & .data$input_type %in% c("dig_blocktouch", "cover_blocktouch") ~ vepv_input_location_model(plays, touch_type = "dig_blocktouch"),
          is.na(.data$epv_in) & .data$input_type == "set_regular" & !is.na(.data$input_coord_x) & !is.na(.data$input_coord_y) ~ vepv_input_location_model(plays, touch_type = "set_regular"),
          is.na(.data$epv_in) & .data$input_type == "set_regular" ~ vepv_input_location_model(plays, touch_type = "set_regular", trained_model = glm(rally_winner ~ blockers, data = plays, subset = (input_type == "set_regular"))),
          is.na(.data$epv_in) & !is.na(.data$input_coord_x) & !is.na(.data$input_coord_y) ~ vepv_input_location_model(plays, touch_type = NA_character_),
          TRUE ~ .data$epv_in
        )
      )

      # first pass - output
      plays_out <- plays_in |> dplyr::mutate(
        epv_out = dplyr::case_when(
          is.na(.data$epv_out) & .data$output_type %in% c("serve_ace", "attack_kill", "block_stuff") ~ 1.0,
          is.na(.data$epv_out) & .data$output_type %in% c("serve_error", "reception_error", "set_error", "attack_error", "attack_blocked", "block_out", "dig_error", "cover_error", "freeball_error") ~ 0.0,
          is.na(.data$epv_out) & .data$output_type %in% c("serve_overpass_attack", "attack_block_into_overpass_attack", "attack_into_overpass_attack") ~ vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = NA_real_),
          is.na(.data$epv_out) & .data$output_type %in% c("reception_overpass_attack", "dig_overpass_attack", "freeball_overpass_attack") ~ (1 - vepv_mean_model(plays, touch_type = "attack_overpass", impute_value = NA_real_)),
          is.na(.data$epv_out) & .data$output_type == "set_poor" & .data$team == dplyr::lead(.data$team, 1) ~ vepv_mean_model(plays, touch_type = "set_poor", impute_value = NA_real_),
          is.na(.data$epv_out) & .data$output_type == "set_poor" & .data$point_id == dplyr::lead(.data$point_id, 1) ~ 1 - dplyr::lead(.data$epv_in, 1),
          is.na(.data$epv_out) & .data$output_type == "set_poor" ~ 0.0,
          is.na(.data$epv_out) & .data$skill == "Block" & dplyr::lead(.data$skill, 1) %in% c("Dig", "Cover") ~ dplyr::lead(.data$epv_in, 1),
          TRUE ~ .data$epv_out
        )
      )

    }

    # second pass - fill in using lag and lead
    plays_in2 <- plays_out |> dplyr::mutate(
      epv_in = dplyr::case_when(
        .data$input_type == "attack_regular" & dplyr::lag(.data$output_type, 1) == "set_regular" ~ dplyr::lag(.data$epv_out, 1),
        .data$input_type %in% c("dig_regular", "cover_regular") & dplyr::lag(input_type, 1) == "attack_regular" ~ dplyr::lag(.data$epv_in, 1),
        .data$input_type == "attack_weird" ~ dplyr::lag(.data$epv_out, 1),
        .data$input_type == "reception_baseline" ~ 1 - dplyr::lag(.data$epv_in, 1),
        (.data$input_type == "block_regular" & dplyr::lag(.data$skill, 1) == "Attack") | (.data$input_type == "dig_regular" & dplyr::lag(input_type, 1) %in% c("attack_weird", "attack_poor_set")) ~ 1 - dplyr::lag(.data$epv_in, 1),
        is.na(.data$epv_in) & .data$team == dplyr::lag(.data$team) ~ dplyr::lag(.data$epv_in, 1),
        is.na(.data$epv_in) & .data$team != dplyr::lag(.data$team) ~ 1 - dplyr::lag(.data$epv_in, 1),
        TRUE ~ .data$epv_in
      )
    )

    plays_out2 <- plays_in2 |> dplyr::mutate(
      epv_out = dplyr::case_when(
        (.data$output_type %in% paste0(c("reception", "dig", "cover", "freeball"), "_overpass_non_attack")) | (.data$skill == "Serve" & dplyr::lead(.data$skill, 1) == "Reception") ~ 1 - dplyr::lead(.data$epv_out, 1),
        (.data$output_type == "attack_regular" & dplyr::lead(.data$input_type, 1) == "dig_regular") | (.data$output_type == "attack_regular_block") ~ 1 - dplyr::lead(.data$epv_out, 1),
        (.data$output_type == "block_regular" & dplyr::lag(.data$input_type, 1) == "attack_regular_block") | (.data$output_type == "block_covered") ~ 1 - dplyr::lag(.data$epv_out, 1),
        (.data$output_type == "attack_covered_overpass") | (.data$output_type == "attack_covered" & dplyr::lead(.data$input_type, 2) == "cover_blocktouch") ~ dplyr::lead(.data$epv_in, 2),
        .data$output_type == "block_into_dig_error" ~ dplyr::lead(.data$epv_in, 1),
        is.na(.data$epv_out) & .data$team == dplyr::lag(.data$team) ~ dplyr::lag(.data$epv_out, 1),
        is.na(.data$epv_out) & .data$team != dplyr::lag(.data$team) ~ 1 - dplyr::lag(.data$epv_out, 1),
        TRUE ~ .data$epv_out
      )
    )
    # continue until all the NA's have been filled in
    iter <- iter + 1
  }

  plays_output <- plays_out2 |> dplyr::mutate(
    epv_added = round(.data$epv_out - .data$epv_in, 3),
    epv_in = round(.data$epv_in, 3),
    epv_out = round(.data$epv_out, 3)
  )


  return(plays_output)
}


#' Add positions
#'
#' Adds a position for each player. Will guess the position if none is provided.
#'
#' @param plays the data frame containing the plays
#' @param position_df a data frame with two columns: `player_name`, the name of the player (as given in `plays`), and the player's position.
#' If `NULL`, or for any players not found in `position_df`, a position will be guessed based on the types of touches in the `plays` data
#'
#' @return the same data frame, with a `position` column indicating the actual (or guessed) position of the player
#'
#' @export
vepv_add_positions <- function(plays, position_df = NULL){

  if(data.class(position_df) == "data.frame"){
    output <- plays |> dplyr::left_join(
      position_df, by = "player_name"
    )

  } else {
    output <- plays |> dplyr::mutate(
      position = NA_character_
    )
  }

  touches <- plays |> dplyr::group_by(player_name, team, year) |>
    summarize(
      touches = dplyr::n(),
      serves = sum(.data$skill == "Serve"),
      sets = sum(.data$skill == "Set"),
      receptions = sum(.data$skill == "Reception"),
      digs = sum(.data$skill == "Dig"),
      covers = sum(.data$skill == "Cover"),
      attacks_total = sum(.data$skill == "Attack"),
      attacks_opp = sum(.data$skill == "Attack" & .data$start_zone %in% c(2, 9)),
      attacks_oh = sum(.data$skill == "Attack" & .data$start_zone %in% c(4, 8, 7)),
      attacks_mh = sum(.data$skill == "Attack" & (.data$start_zone == 3 | .data$attack_code %in% c("X1", "X7", "XM", "X2", "CF", "CD")))
    ) |> dplyr::ungroup()

  guessed_positions <- touches |> dplyr::mutate(
    guessed_position = dplyr::case_when(
      .data$touches <= 5 ~ "Unknown", # needs at least 5 touches to guess position
      .data$sets/(.data$touches - .data$serves) >= 0.5 ~ "Setter",
      .data$attacks_total/(.data$touches - .data$serves) < 0.2 ~ "Libero/DS",
      # For attacking positions, guess whichever position they have the plurality of attacks from
      .data$attacks_opp > .data$attacks_oh & .data$attacks_opp >= .data$attacks_mh ~ "Opposite",
      .data$attacks_oh >= .data$attacks_opp & .data$attacks_oh >= .data$attacks_mh ~ "Outside",
      .data$attacks_mh > .data$attacks_opp & .data$attacks_mh > .data$attacks_oh ~ "Middle",
      TRUE ~ "Unknown"
    )
  ) |> dplyr::select(player_name, team, year, guessed_position)

  output <- output |> dplyr::left_join(guessed_positions, by = c("player_name", "team", "year")) |>
    dplyr::mutate(
      position = dplyr::if_else(is.na(.data$position), .data$guessed_position, .data$position)
    ) |>
    dplyr::select(-guessed_position)

  return(ouptut)

}
