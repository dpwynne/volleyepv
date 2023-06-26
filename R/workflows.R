#' Expected Point Value Data Processing Workflow
#'
#' Runs all data processing functions in the correct order to get the data in a form for modeling.
#'
#' @param folder the file path to the folder containing the .dvw files to process
#'
#' @return a data frame containing the processed data from the .dvw files
#'
#' @export

vepv_data_processing_workflow <- function(folder){

  file_list <- list.files(path = file_folder, full.names = TRUE)

  processed_plays <- file_list |>
    vepv_remove_nonactions() |>  # Step 1: remove non-actions
    vepv_add_touch_id() |>  # Step 2: add touch id
    vepv_add_year() |>  # Step 3: add year
    vepv_add_rally_winner() |>  # Step 4: add who won the point
    vepv_recode_skills() |>  # Step 5: recode the skills
    vepv_add_possession_contacts() |> # Step 6: add possession and contact numbers
    vepv_touch_input_output() |> # Step 7: add input and output types
    vepv_touch_coordinates() |> vepv_touch_kcode() |> # Step 8: add coordinates and kcode
    vepv_blockers() |>  # Step 9: add blockers with default imputation
    vepv_dig_difficulty() # Step 10: add features for dig difficulty estimation

  return(processed_plays)
}


#' Expected Point Value Modeling Workflow
#'
#' Runs all modeling functions in the correct order to get the data in a form for modeling.
#'
#' @param plays the data frame containing the plays
#' @param touch_epv a data frame with two columns: `touch`, the type of touch (as indicated by`input_type` or `output_type`), and the expected value of that touch to impute.
#' If `NULL`, or for any touch types not found in `touch_epv`, a value will be imputed using the `plays` data
#' @param position_df a data frame with two columns: `player_name`, the name of the player (as given in `plays`), and the player's position.
#' If `NULL`, or for any players not found in `position_df`, a position will be guessed based on the types of touches in the `plays` data
#'
#' @return a data frame containing input, output, and added expected point value for each touch, and the (possibly guessed) position for each player
#'
#' @export
vepv_modeling_workflow <- function(plays, touch_epv = NULL, position_df = NULL){

  stopifnot(
    "You passed in the file path. Please run vepv_data_processing_workflow first." = is.character(plays),
    "Something is wrong with your plays data. Try running vepv_data_processing_workflow first." = !is.data.frame(plays),
    "You haven't created the touch types needed to run the modeling yet. Try running vepv_data_processing_workflow or vepv_touch_input_output first." = (!("input_type" %in% colnames(plays)))
  )

  processed_plays <- plays |>
    vepv_epv(touch_epv = touch_epv) |> # Step 1: get input and output and added epv
    vepv_add_positions(position_df = position_df) # Step 2: add positions

  return(processed_plays)

}


#' Expected Point Value Full Workflow
#'
#' Runs all data processing and modeling functions in the correct order to get the data in a form for modeling.
#'
#' @param folder the file path to the folder containing the .dvw files to process
#' @param touch_file optionally, either the name of an Excel or CSV file containing the data to pass on to the modeling functions as `touch_epv`, or the data frame itself.
#' The default (= `NULL`) is to not give a file and have all values imputed using the data from the .dvw files.
#' @param position_file either the name of an Excel or CSV file containing the data to pass on to the modeling functions as `position_df`, or the data frame itself
#' The default (= `NULL`) is to not give a file and have all values imputed using the data from the .dvw files.
#' @return a data frame containing the processed data from the .dvw files
#'
#' @importFrom dplyr case_when
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#'
#' @export
vepv_full_workflow <- function(folder, touch_file = NULL, position_file = NULL){

  # import touch file
  if(is.character(touch_file)){

    if(!(stringr::str_detect(touch_file, "\\\\") | stringr::str_detect(touch_file, "\\/"))){
      if(stringr::str_detect(folder, "\\\\")){
        touch_file <- paste0(folder, "\\", touch_file)
      } else{
        touch_file <- paste0(folder, "/", touch_file)
      }
      touch_file <- stringr::str_replace(touch_file, "\\/\\/", "\\/") |>
        stringr::str_replace("\\\\\\\\", "\\\\")
    }

    touch_df <- dplyr::case_when(
      tools::file_ext(touch_file) == "xlsx" ~ readxl::read_xlsx(touch_file),
      tools::file_ext(touch_file) == "csv" ~ readr::read_csv(touch_file),
      TRUE ~ NULL
    )
  } else{

    if(is.data.frame(touch_file)){
      touch_df <- touch_file
    } else {
      touch_df <- NULL
    }

  }

  # import position file
  if(is.character(position_file)){

    if(!(stringr::str_detect(position_file, "\\\\") | stringr::str_detect(position_file, "\\/"))){
      if(stringr::str_detect(folder, "\\\\")){
        position_file <- paste0(folder, "\\", position_file)
      } else{
        position_file <- paste0(folder, "/", position_file)
      }
      position_file <- stringr::str_replace(position_file, "\\/\\/", "\\/") |>
        stringr::str_replace("\\\\\\\\", "\\\\")
    }

    position_df <- dplyr::case_when(
      tools::file_ext(position_file) == "xlsx" ~ readxl::read_xlsx(position_file),
      tools::file_ext(position_file) == "csv" ~ readr::read_csv(position_file),
      TRUE ~ NULL
    )
  } else{

    if(is.data.frame(position_file)){
      position_df <- position_file
    } else {
      position_df <- NULL
    }

  }

  full_data <- vepv_data_processing_workflow(folder) |>
    vepv_modeling_workflow(touch_epv = touch_df, position_df = position_df)

  cat("Boom! Done.\n")

  return(full_data)

}
