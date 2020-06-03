#' Initialize a Multinames Object
#'
#' This serves as the backend for all other functions
multiname <- R6::R6Class(
  "multiname",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(.data, ...) {
      # Initialize multiname with original
      private$.multinames <- data.frame("original" = as.character(names(.data)),
                                        stringsAsFactors = FALSE)
      return(invisible(self))
    },

    get_multinames = function(from = NULL) {
      if (is.null(from)) {
        return(private$.multinames)
      } else {
        from <- match.arg(from, colnames(private$.multinames))
        if (isFALSE(private$.check_multiname_exists(from))) {
          msg <- sprintf("The specified multiname, '%s', does not exist. Available multinames are: %s",
                         from, paste("'", colnames(private$.multinames), "'", sep = "", collapse = ", "))
          stop(msg)
        }
        return(private$.multinames[, from])
      }
    },

    set_multinames = function(.data, to = colnames(private$.multinames)) {
      to <- match.arg(to)
      multinames <- private$.multinames[which(!is.na(private$.multinames[, to])), ]
      from_names <- c()
      to_names <- c()
      from_indices <- lapply(multinames[, ], function(x) which(x %in% colnames(.data)))
      from_indices <- from_indices[!names(from_indices) == to]
      for (i in seq_along(from_indices)) {
        from_names <- c(from_names, multinames[from_indices[[i]], names(from_indices)[i]])
        to_names <- c(to_names, multinames[from_indices[[i]], to])
      }
      colnames(.data)[match(from_names, colnames(.data))] <- to_names
      return(.data)
    },

    add_multinames = function(named_vector, to, from = NULL, verbose = FALSE) {
      if (isTRUE(private$.check_multiname_exists(to))) {
        msg <- sprintf("The specified multiname, '%s', already exist. Please use `replace_multinames()` to update an existing multiname",
                       to)
        stop(msg)
      }
      if (is.null(from)) {
        from <- private$.guess_from(named_vector, verbose = verbose)
      }
      from_multiname <- private$.multinames[, from]
      from_index <- which(from_multiname %in% names(named_vector))
      to_index <- which(names(named_vector) %in% from_multiname)
      private$.multinames[from_index, to] <- unname(named_vector)[to_index]
      return(invisible(self))
    },

    replace_multinames = function(named_vector, to = colnames(private$.multinames), from = NULL, verbose = FALSE) {
      to <- match.arg(to)
      if (isFALSE(private$.check_multiname_exists(to))) {
        msg <- sprintf("The specified multiname, '%s', doesn't exist. Available multinames are: %s. Please use `add_multinames()` to add your new names.",
                       to, paste0("'", colnames(private$.multinames), "'", collapse = ", "))
        stop(msg)
      }
      if (is.null(from)) {
        from <- private$.guess_from(named_vector, verbose = verbose)
      }
      from_multiname <- private$.multinames[, from]
      from_index <- which(from_multiname %in% names(named_vector))
      to_index <- which(names(named_vector) %in% from_multiname)
      private$.multinames[from_index, to] <- unname(named_vector)[to_index]
      return(invisible(self))
    }

  ),
  private = list(
    .multinames = NULL,

    .check_multiname_exists = function(check) {
      return(any(check %in% colnames(private$.multinames)))
    },

    .guess_from = function(named_vector, check_self = FALSE, verbose = FALSE) {
      if (isTRUE(check_self)) {
        counts <- apply(private$.multinames, 2, function(x) sum(named_vector %in% x))
      } else {
        counts <- apply(private$.multinames, 2, function(x) sum(names(named_vector) %in% x))
      }
      if (isTRUE(verbose)) {
        print(sprintf("from was not specified. Based on max counts, from is set to: '%s'", names(counts[which.max(counts)])))
        print(counts)
      }
      return(names(counts[which.max(counts)]))
    }

  ),
  active = list(
    multinames = function() {
      return(private$.multinames)
    }
  )
)

#' Initialize a Multinames Object
#'
#' This function allows you to initialize a multiname object.
#' Initializing an object, instead of an attribute, is a good idea,
#' if you have multiple data.frame objects with shared naming conventions.
#'
#' Using the object follows the R6 syntax of object$function().
#'
#' The R6 object and the S3 implementation share function names,
#' so if it should be easy to switch from one to the other.
#'
#' @param .data A data.frame, tibble, or data.table
#' @export
#' @examples
#' df1 <- data.frame(a=1:10, b=11:20)
#' df2 <- data.frame(a=21:30, b=31:40)
#' CAPS_NAMES <- c("a" = "A", "b" = "B")
#' obj <- initialize_multinames_object(df1)
#' obj$add_multinames(CAPS_NAMES, "CAPS")
#' df1 <- obj$set_multinames(df1, "CAPS")
#' df2 <- obj$set_multinames(df2, "CAPS")
#' print(df1)
#' print(df2)
initialize_multinames_object <- function(.data) {
  return(multiname$new(.data))
}


#' Initialize Multinames on a Data.Frame Object
#'
#' This function allows you to initialize multinames on a data.frame object,
#' which is prefered if you are using multiple names for just this object.
#'
#' This function doesn't do much by itself, but allows you to use the all of
#' the corresponding S3 methods:
#' - add_multinames(),
#' - replace_multinames(),
#' - get_multinames() and
#' - set_multinames()
#'
#' @param .data An initialized data.frame, tibble, or data.table
#' @export
#' @examples
#' df <- initialize_multinames(data.frame(a=1:10, b=11:20))
#' print(df)
initialize_multinames <- function(.data) {
  multinames <- multiname$new(.data)
  attr(.data, "multinames") <- multinames
  return(invisible(.data))
}

#' Add New Multinames
#'
#' @param .data An initialized data.frame, tibble, or data.table
#' @param named_vector A named_vector following the convention c("from" = "to", "initial" = "target")
#' @param to A character string specifying the name of the provided named_vector
#' @param from A character string specifying the current multiname used, if `NULL` then this will be guessed.
#' @param verbose Should the function print?
#' @export
#' @examples
#' df <- initialize_multinames(data.frame(a=1:10, b=11:20))
#' add_multinames(df, c("a" = "A", "b" = "B"), "CAPS")
#' print(df)
add_multinames <- function(.data, named_vector, to, from = NULL, verbose = FALSE) {
  multinames <- attr(.data, "multinames")
  if (is.null(multinames)) {
    stop("Multinames have not yet been initialized. Please initialize multinames with `initialize_multinames()`")
  }
  multinames$add_multinames(named_vector = named_vector, to = to, from = from, verbose = verbose)
  attr(.data, "multinames") <- multinames
  return(invisible(.data))
}

#' Replace Existing Multinames
#'
#' @param .data An initialized data.frame, tibble, or data.table
#' @param named_vector A named_vector following the convention c("from" = "to", "initial" = "target")
#' @param to A character string specifying the name of the provided named_vector
#' @param from A character string specifying the current multiname used, if `NULL` then this will be guessed.
#' @param verbose Should the function print?
#' @export
#' @examples
#' df <- initialize_multinames(data.frame(a=1:10, b=11:20))
#' add_multinames(df, c("a" = "A", "b" = "B"), "CAPS")
#' print(df)
#' replace_multinames(df, c("a" = "A_NEW", "b" = "B_NEW"), "CAPS")
#' print(df)
replace_multinames <- function(.data, named_vector, to, from = NULL, verbose = FALSE) {
  multinames <- attr(.data, "multinames")
  if (is.null(multinames)) {
    stop("Multinames have not yet been initialized. Please initialize multinames with `initialize_multinames()`")
  }
  multinames$replace_multinames(named_vector = named_vector, to = to, from = from, verbose = verbose)
  attr(.data, "multinames") <- multinames
  return(invisible(.data))
}

#' Get Assigned Multinames
#'
#' @param .data An initialized data.frame, tibble, or data.table
#' @param from Specifies which multinames to collect. If `NULL`, then a data.frame with all multinames is returned.
#' @export
#' @examples
#' df <- initialize_multinames(data.frame(a=1:10, b=11:20))
#' add_multinames(df, c("a" = "A", "b" = "B"), "CAPS")
#' get_multinames(df)           # Returns all multinames in a data.frame
#' get_multinames(df, "CAPS")   # Returns just multinames in "CAPS"
get_multinames <- function(.data, from = NULL) {
  multinames <- attr(.data, "multinames")
  if (is.null(multinames)) {
    stop("Multinames have not yet been initialized. Please initialize multinames with `initialize_multinames()`")
  }
  if (is.null(from)) {
    return(multinames$multinames)
  } else {
    return(multinames$get_multinames(from = from))
  }
}

#' Set Assigned Multinames
#'
#' @param .data An initialized data.frame, tibble, or data.table
#' @param to Specifies which multinames to collect.
#' @export
#' @examples
#' df <- initialize_multinames(data.frame(a=1:10, b=11:20))
#' add_multinames(df, c("a" = "A", "b" = "B"), "CAPS")
#' set_multinames(df, "CAPS") # Returns df with names specified in "CAPS"
set_multinames <- function(.data, to) {
  multinames <- attr(.data, "multinames")
  if (is.null(multinames)) {
    stop("Multinames have not yet been initialized. Please initialize multinames with `initialize_multinames()`")
  }
  return(multinames$set_multinames(.data, to = to))
}
