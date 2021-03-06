#' Summarize One-Way ANOVA Results
#'
#' summary.oneway summarizes the results of the one-way ANOVA
#'
#' @param x an onject of class 'oneway'.
#' @export
#' @return object of type anova and data.frame
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' summary(mileage)
summary.oneway <- function(x) {
  if(!inherits(x, "oneway")){
    stop("Must be class 'oneway'")}
  anova(x$anova)
}

