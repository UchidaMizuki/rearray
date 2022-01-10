#' @import rlang
#' @import vctrs
NULL

## usethis namespace: start
#' @useDynLib rearray, .registration = TRUE
## usethis namespace: end
NULL

.onUnload = function(libpath) {
  library.dynam.unload("rearray", libpath)
}
