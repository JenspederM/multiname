% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiname.R
\name{initialize_multinames}
\alias{initialize_multinames}
\title{Initialize Multinames on a Data.Frame Object}
\usage{
initialize_multinames(.data)
}
\arguments{
\item{.data}{An data.frame, tibble, or data.table}
}
\description{
This function allows you to initialize multinames on a data.frame object,
which is prefered if you are using multiple names for just this object.
}
\details{
This function doesn't do much by itself, but allows you to use the all of
the corresponding S3 methods: add_multinames(), replace_multinames(), get_multinames(),
and set_multinames()
}
\examples{
df <- initialize_multinames(data.frame(a=1:10, b=11:20))
print(df)
}
