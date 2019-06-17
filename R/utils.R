## Functions not exported from ggplot2

find_global <-
  function (name, env, mode = "any")
  {
    if (exists(name, envir = env, mode = mode)) {
      return(get(name, envir = env, mode = mode))
    }
    nsenv <- asNamespace("ggformula")
    if (exists(name, envir = nsenv, mode = mode)) {
      return(get(name, envir = nsenv, mode = mode))
    }
    nsenv <- asNamespace("ggplot2")
    if (exists(name, envir = nsenv, mode = mode)) {
      return(get(name, envir = nsenv, mode = mode))
    }
    NULL
  }

check_subclass <-
  function (x, subclass, argname = tolower(subclass), env = parent.frame())
  {
    if (inherits(x, subclass)) {
      x
    }
    else if (is.character(x) && length(x) == 1) {
      name <-  paste0(subclass, camelize(x, first = TRUE))
        paste0(subclass, camelize(x, first = TRUE))
      obj <- find_global(name, env = env)
      if (is.null(obj) || !inherits(obj, subclass)) {
        stop("Can't find `", argname, "` called \"", x, "\"",
             call. = FALSE)
      }
      else {
        obj
      }
    }
    else {
      stop("`", argname, "` must be either a string or a ",
           subclass, " object, ", "not ", obj_desc(x), call. = FALSE)
    }
  }

camelize <-
  function (x, first = FALSE)
  {
    x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
    if (first)
      x <- firstUpper(x)
    x
  }

firstUpper <-
  function (s)
  {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  }

## Use above to create below

get_aesthetics <- function(x, subclass, env = parent.frame()) {
  obj <- find_geom_or_stat(x, subclass, env)
  if (is.null(obj)) {
    stop(paste("No", tolower(subclass), "named", tolower(x)))
  }
  obj$aesthetics()
}

get_parameters <- function(x, subclass, env = parent.frame()) {
  obj <- find_geom_or_stat(x, subclass, env)
  if (is.null(obj)) {
    stop(paste("No", subclass, "named", x))
  }
  obj$parameters()
}

get_args <- function(x, subclass, env = parent.frame()) {
  union(
    get_aesthetics(x, subclass, env),
    get_parameters(x, subclass, env)
  )
}

find_geom_or_stat <-
  function(x, subclass, env = parent.frame()) {
    # check_subclass(x, subclass, env = env)
    find_global(paste0(firstUpper(subclass), firstUpper(x)), env)
  }

### other utils

cull_list <- function(x, names) {
  x[intersect(names(x), names)]
}

remove_from_list <- function(x, names) {
  for (n in names) {
    x[[n]] <- NULL
  }
  x
}