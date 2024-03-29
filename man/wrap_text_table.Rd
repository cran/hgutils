% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-utils.R
\name{wrap_text_table}
\alias{wrap_text_table}
\title{Wrap string table}
\usage{
wrap_text_table(string, exdent, min_size = 9, table_width = 80 - exdent)
}
\arguments{
\item{string}{character vector of strings to reformat.}

\item{exdent}{non-negative integer giving indentation of following lines in
each paragraph}

\item{min_size}{minimal size where a table is constructed, otherwise elements are concatenated with ', '.}

\item{table_width}{table character width.}
}
\value{
A character vector of a wrapped table where rows are separated by the newline character.
}
\description{
Wrap string table
}
\examples{
cat(wrap_text_table(LETTERS, exdent=0))
}
\seealso{
\code{\link[stringr]{str_wrap}}, \code{\link{get_square_grid}}.
}
