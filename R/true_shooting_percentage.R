#' True shooting percentage
#'
#'
#' @param pts Points scored
#' @param fga Field goals attempted
#'
#' @param pts
#' @param fta Free throws attempted
#' @param fta_coef Coefficient used to multiply free throws attempted. Some
#' formulas use 0.475 as the coefficient. However, the default value is 0.44.
#' The .44 multiplier is because not all free throws take up a possession.
#' Technical foul shots and "and-ones" do not, while there are more than two
#' free throws on one possession with a three-shot foul.
#' Research has determined that about 44% of all free throws take up
#' possessions, thus .44 is used as the multiplier.
#' From https://www.nba.com/thunder/news/stats101.html.
#'
#'
#' @return True shooting percentage
#' @export
true_shooting_percentage <- function(pts, fga, fta, fta_coef = 0.44) {
  pts <- as.numeric(pts)
  fga <- as.numeric(fga)
  fta <- as.numeric(fta)
  pts / (2 * (fga + (fta_coef * fta)))
}
