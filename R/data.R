#' Women's Volleyball Pablo Rankings
#'
#' A dataset containing the end-of-season Pablo rankings for NCAA Division I Women's Volleyball teams
#' since 1999. The variables are:
#'
#' @format A data frame with 8280 rows and 10 variables:
#' \describe{
#' \item{ranking}{the end-of-season team ranking (1 = best); a ranking is listed as NA if the team did not compete that season}
#' \item{pablo_name}{the name of the team as it appears in the Pablo rankings}
#' \item{conference}{the abbreviated name of the conference (e.g., "SEC" for "Southeastern Conference")}
#' \item{conference_long}{the full name of the conference (not including "Conference")}
#' \item{season}{the competition season}
#' \item{gender}{the competition gender (only "women" currently available)}
#' \item{league}{the competition governing body (only "NCAA" currently available)}
#' \item{division}{the competition division (only "D1" currently available)}
#' \item{rank_type}{the source of the ranking (only "Pablo" currently available)}
#' \item{team}{the long-form name of the school (e.g., "University of California, Berkeley" for "Cal")}
#' }
"pablo"

#' Team and Conference Logos
#'
#' A dataset containing links to team and conference logos. The variables are:
#'
#' @format A data frame with 155 rows and 6 variables:
#' \describe{
#' \item{team}{the long-form name of the school (e.g., "University of California, Berkeley" for "Cal")}
#' \item{team_short}{a lowercase, one-word, shortened school identifier (e.g., "cal" for "Cal")}
#' \item{conference}{the abbreviated name of the conference (e.g., "SEC" for "Southeastern Conference")}
#' \item{conference_short}{a lowercase, one-word, abbreviated name for the conference}
#' \item{team_link}{the url for the team logo}
#' \item{conference_link}{the url for the conference logo}
#' }
"team_conference_logos"
