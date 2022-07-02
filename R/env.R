set_taenv <- function(name, value) {
  assign(name, value, envir = .bitTAEnv)
}

unset_taenv <- function(name) {
  value <- .getTAEnv(name)
  if (!is.null(value)) {
    rm(list = name, envir = .bitTAEnv)
  }
}

get_taenv <- function(name) {
  if (missing(name)) {
    as.list(.bitTAEnv)
  } else {
    .bitTAEnv[[name]]
  }
}
