% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiname.R
\name{initialize_multinames_object}
\alias{initialize_multinames_object}
\title{Initialize a Multinames Object}
\usage{
initialize_multinames_object(.data)
}
\arguments{
\item{.data}{A data.frame, tibble, or data.table}
}
\description{
This function allows you to initialize a multiname object.
Initializing an object, instead of an attribute, is a good idea,
if you have multiple data.frame objects with shared naming conventions.
}
\details{
Using the object follows the R6 syntax of object$function().

The R6 object and the S3 implementation share function names,
so if it should be easy to switch from one to the other.
}
\examples{
df1 <- data.frame(a=1:10, b=11:20)
df2 <- data.frame(a=21:30, b=31:40)
CAPS_NAMES <- c("a" = "A", "b" = "B")
obj <- initialize_multinames_object(df1)
obj$add_multinames(CAPS_NAMES, "CAPS")
df1 <- obj$set_multinames(df1, "CAPS")
df2 <- obj$set_multinames(df2, "CAPS")
print(df1)
print(df2)
}
