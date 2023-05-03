


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
