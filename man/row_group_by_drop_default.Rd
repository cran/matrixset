% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{row_group_by_drop_default}
\alias{row_group_by_drop_default}
\title{Default value for .drop argument of function row_group_by()}
\usage{
row_group_by_drop_default(.ms)
}
\arguments{
\item{.ms}{a \code{matrixset} object}
}
\value{
Returns \code{TRUE} for row-ungrouped \code{matrixset}s. For row-grouped objects, the
default is also \code{TRUE} unless \code{.ms} has been previously grouped with
\code{.drop = FALSE}.
}
\description{
Default value for \code{.drop} argument of function \code{\link[=row_group_by]{row_group_by()}}
}
\examples{
student_results |>
    row_group_by(class, .drop = FALSE) |>
    row_group_by_drop_default()


}
