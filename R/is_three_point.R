#' Is three point shot based on X and Y coordinates
#'
#' The calculations are based on the NBA three point line rules found at
#' https://official.nba.com/rule-no-1-court-dimensions-equipment/
#' Section 1 D. The three-point field goal area has parallel lines 3’ from the
#' sidelines, extending from the baseline and an arc of 23’9” from the
#' middle of the basket which intersects the parallel lines.
#'
#' The court is 50 feet wide and 94 feet long. The three point line is 23.75
#' when the shot is over 14 feet from the baseline. It decreases to 22 feet as
#' the shooter gets closer to the baseline along the 14 foot line on each side
#' of the court.
#'
#' @param x Horizontal distance of the shot taken from the baseline in feet
#' @param y Vertical distance of the shot taken from the baseline in feet
#'
#' @return TRUE if 3 point shot, FALSE otherwise
#' @export
#'
is_three_point <- function(x, y) {
  distance <- sqrt(x^2 + y^2)

  if (distance > 23.75)
    return(TRUE)
  else if (distance <= 22)
      return(FALSE)
  else {
    if (y <= 14) { # 14 foot line from baseline parallel to the sideline
      if (abs(x) > 22)
        return(TRUE)
      else
        return(FALSE)
    } else {
      return(FALSE)
    }
  }
}
