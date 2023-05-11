


is_na_scalar <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is_null_scalar <- function(x) {
  all(is.null(x)) & length(x) == 1
}


NA_to_0 <- function(val) {
  if(is.na(val)) 0 else val
}


FALSE_to_0 <- function(val) {
  if(val == FALSE) 0 else val
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}


#' Is NULL or...?
#'
#' Returns \code{TRUE} if \code{x} is either \code{NULL} or \code{f(x)}
#' is \code{TRUE}.
#'
#' @param x Object to check.
#'
#' @param f Function to apply.
#'
#' @keywords internal
#' @export
is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

