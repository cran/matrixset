% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{meta}
\alias{meta}
\alias{row_group_meta}
\alias{row_group_vars}
\alias{row_group_keys}
\alias{row_group_where}
\alias{row_group_indices}
\alias{row_groups}
\alias{column_group_meta}
\alias{column_group_vars}
\alias{column_group_keys}
\alias{column_group_where}
\alias{column_group_indices}
\alias{column_groups}
\title{Matrixset group metadata}
\usage{
row_group_meta(.ms)

row_group_vars(.ms)

row_group_keys(.ms)

row_group_where(.ms)

row_group_indices(.ms)

row_groups(.ms)

column_group_meta(.ms)

column_group_vars(.ms)

column_group_keys(.ms)

column_group_where(.ms)

column_group_indices(.ms)

column_groups(.ms)
}
\arguments{
\item{.ms}{a \code{matrixset} object}
}
\description{
\itemize{
\item \code{\link[=row_group_meta]{row_group_meta()}} and \code{\link[=column_group_meta]{column_group_meta()}} returns the grouping structure,
in a data frame format. See \code{dplyr}'s \code{\link[dplyr:group_data]{dplyr::group_data()}}, from which the
functions are based. Returns \code{NULL} for ungrouped \code{matrixset}s.
\item \code{\link[=row_group_keys]{row_group_keys()}} and \code{\link[=column_group_keys]{column_group_keys()}} retrieve the grouping data,
while the locations (row or column indices) are retrieved with
\code{\link[=row_group_where]{row_group_where()}} and \code{\link[=column_group_where]{column_group_where()}}.
\item \code{\link[=row_group_indices]{row_group_indices()}} and \code{\link[=column_group_indices]{column_group_indices()}} each return an integer
vector the same length as the number of rows or columns of \code{.ms}, and
gives the group that each row or column belongs to.
\item \code{\link[=row_group_vars]{row_group_vars()}} and \code{\link[=column_group_vars]{column_group_vars()}} give names of grouping
variables as character vector; \code{\link[=row_groups]{row_groups()}} and \code{\link[=column_groups]{column_groups()}} give
the names as a list of symbols.
}
}
