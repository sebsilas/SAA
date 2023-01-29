


is_na_scalar <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is_null_scalar <- function(x) {
  all(is.null(x)) & length(x) == 1
}
