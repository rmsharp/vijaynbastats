#' Is three point shot based on X and Y coordinates
#'
#' The calculations are based on the NBA three point line rules found at
#' https://official.nba.com/rule-no-1-court-dimensions-equipment/
#' Section 1 D. The three-point field goal area has parallel lines 3’ from the
#' sidelines, extending from the baseline and an arc of 23’9” from the
#' middle of the basket which intersects the parallel lines.
#'
#' The court is 50 feet wide and 94 feet long. The three point line is 23.75
#' from the center of the basket. It decreases to 22 feet as
#' the shooter gets closer to the baseline along the 14 foot line three feet
#' from the side of the court.
#'
#' The center of the basket is 5.25 feet from the
#' nearest baseline.

#'
#' @param x Horizontal distance of the shot taken from the baseline in feet
#' @param y Vertical distance of the shot taken from the baseline in feet
#'
#' @return TRUE if 3 point shot, FALSE otherwise
#' @export
#'
is_three_point <- function(x, y) {
  b_y <- 5.25 # center of basket y;  center of basket x == 0 and thus ignored
  distance <- sqrt(x^2 + abs(y - b_y)^2) # distance from center of basket

  if (distance >= 23.75)
    return(TRUE)
  else if (abs(x) >= 21.9)
    return(TRUE)
  else if (distance < 21.9)
      return(FALSE)
  else
    return(FALSE)
}
