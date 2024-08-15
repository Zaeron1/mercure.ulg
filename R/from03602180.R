#' Swap Halves of a Matrix
#'
#' This function takes a matrix and swaps its left and right halves.
#'
#' @param mat A matrix with an even number of columns.
#' @return A matrix where the left and right halves are swapped.
#' @examples
#' mat <- matrix(1:8, nrow = 2)
#' from03602180(mat)
#' @export
from03602180 <- function(mat) {
  # Calculate the middle index of the columns
  middle <- dim(mat)[2] / 2

  # Split the matrix into two halves
  mat1 <- mat[, 1:middle]
  mat2 <- mat[, (middle + 1):(2 * middle)]

  # Combine the halves with their positions swapped
  return(cbind(mat2, mat1))
}
