#' Title
#'
#' @param x Horizontal distance of the shot taken from the baseline in feet
#' @param y Vertical distance of the shot taken from the baseline in feet
#'
#' @return The distance of the shot from the basket in feet.
#' @export
#'
#' @examples
#' get_shot_distance(0, 0) # 0
#' get_shot_distance(0, 10) # 10
#' get_shot_distance(10, 0) # 10
#' get_shot_distance(10, 10) # 14.1421356237
get_shot_distance <- function(x, y) {
  sqrt(x^2 + y^2)
}
