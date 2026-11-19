# Saves objects in R/sysdata.rda and keeps the other internal data, e.g.
# `save_sysdata(xpass_model = xpass_model)`. Sourced by the scripts that build
# the internal data. Run from the package root.
#
# R/sysdata.rda holds several objects. This loads them, replaces (or adds) the
# named arguments and writes all of them back, and then makes sure nothing else
# changed. Do NOT rebuild or re-serialize anything else in a script that calls
# this (in particular no xgboost models) because a different xgboost version on
# the machine running the script would change those bytes too.
save_sysdata <- function(..., file = "R/sysdata.rda") {
  new <- list(...)
  stopifnot(
    length(new) > 0,
    !is.null(names(new)),
    all(nzchar(names(new))),
    !anyDuplicated(names(new))
  )
  e <- new.env()
  load(file, envir = e)
  untouched <- setdiff(ls(e), names(new))
  before <- mget(untouched, envir = e)
  list2env(new, envir = e)
  save(
    list = ls(e),
    file = file,
    envir = e,
    compress = "bzip2",
    version = 2
  )
  # make sure we saved the new objects and did not change anything else
  e2 <- new.env()
  load(file, envir = e2)
  after <- mget(ls(e2), envir = e2)
  stopifnot(identical(after, c(before, new)[names(after)]))
  invisible(file)
}
