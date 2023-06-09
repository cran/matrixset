% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate.R
\name{mutate_matrix}
\alias{mutate_matrix}
\title{Create/modify/delete matrices from a \code{matrixset} object}
\usage{
mutate_matrix(.ms, ...)
}
\arguments{
\item{.ms}{A \code{matrixset} object.}

\item{...}{Name-value pairs, ala \code{dplyr}'s \code{\link[dplyr:mutate]{dplyr::mutate()}}. The value
can be one of:
\itemize{
\item a \code{matrix}, with same dimension and dimnames as \code{.ms}.
\item \code{NULL}, which will turn the matrix as an empty placeholder.
\item \code{\link[=remove_matrix]{remove_matrix()}}, to remove the matrix
}}
}
\value{
A \code{matrixset} with updated matrices.
}
\description{
Applies functions that takes matrices as input and return similar matrices.
The definition of similar is that the new matrix has the same dimension and
dimnames as \code{.ms}.

If the returned matrix is assigned to a new matrix, this matrix is added to the
\code{matrixset} object. If it is assigned to an already existing matrix, it
overwrites the matrix of the same name.

Setting a matrix value to \code{NULL} will \emph{\strong{not}} delete the matrix, but will
create an empty slot (\code{NULL}) for the matrix.

To delete a matrix, use the function \code{\link[=remove_matrix]{remove_matrix()}}. See examples below.

Note that matrices are created sequentially and can be used by other
name-value pairs. There is an example that showcases this.
}
\examples{
# Notice how FC can be used as soon as created
ms <- mutate_matrix(student_results,
                    FC = remedial/failure,
                    foo = NULL,
                    logFC = log2(FC),
                    FC = remove_matrix())
# this is NULL
matrix_elm(ms, "foo")

# running this would return an error, since FC was deleted
# matrix_elm(ms, "FC")

}
