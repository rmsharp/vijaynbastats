#' Get point value of shot based on distance and angle
#'
#' @param x Horizontal distance of the shot taken from the baseline in feet
#' @param y Vertical distance of the shot taken from the baseline in feet
#' @param score MADE or MISSED
#'
#' @return Point value of shot
#' @export
#'
#' @examples
#' get_points(0, 0) # 2
#' get_points(23.75, 0) # 3
#' get_points(20.75, 23) # 2
#' get_points(22, 100) # 2
#' get_points(22, 0) # 3
#' get_points(23.75, 0) # 3
#' get_points(23.75, 100) # 3
#' get_points(23.75, 140) # 3
#'
get_points <- function(x, y, score) {
  if (score == "MADE") {
    if (is_three_point(x, y)) {
      return(3)
    } else {
      return(2)
    }
  } else {
    return(0)
  }
}
