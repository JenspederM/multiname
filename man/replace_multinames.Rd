% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiname.R
\name{replace_multinames}
\alias{replace_multinames}
\title{Replace Existing Multinames}
\usage{
replace_multinames(.data, named_vector, to, from = NULL, verbose = FALSE)
}
\arguments{
\item{.data}{An initialized data.frame, tibble, or data.table}

\item{named_vector}{A named_vector following the convention c("from" = "to", "initial" = "target")}

\item{to}{A character string specifying the name of the provided named_vector}

\item{from}{A character string specifying the current multiname used, if `NULL` then this will be guessed.}

\item{verbose}{Should the function print?}
}
\description{
Replace Existing Multinames
}
\examples{
df <- initialize_multinames(data.frame(a=1:10, b=11:20))
add_multinames(df, c("a" = "A", "b" = "B"), "CAPS")
print(df)
replace_multinames(df, c("a" = "A_NEW", "b" = "B_NEW"), "CAPS")
print(df)
}
