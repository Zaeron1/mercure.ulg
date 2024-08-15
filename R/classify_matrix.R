#' Classify a Matrix Layer Based on Noniles
#'
#' This function classifies values in a specified layer of a 3-dimensional array into categories based on noniles.
#'
#' @param layer_index Integer. The index of the layer to be classified. Must be between 1 and the number of layers in the array.
#'
#' @details
#' The function extracts a specified layer from a 3-dimensional array named `result_array` and classifies each value based on noniles. It assigns labels such as "Very Low", "Medium", "High", etc., to classify the values into ten distinct categories.
#'
#' @return A matrix of the same dimensions as the input layer, with numeric values representing the classified categories.
#'
#' @examples
#' \dontrun{
#' # Assuming `result_array` is a 3D array with suitable dimensions
#' classified_matrix <- classify_matrix(layer_index = 1)
#' print(classified_matrix)
#' }
#'
#' @export
classify_matrix <- function(layer_index){
  # Vérifie si l'index de couche est dans les limites
  if (layer_index < 1 || layer_index > dim(result_array)[3]) {
    stop("Layer index out of bounds.")
  }

  # Extrait la couche spécifiée sous forme de matrice
  layer_matrix <- get_layer_as_matrix(result_array, layer_index)

  # Calcul des quantile
  quantile <- quantile(layer_matrix, probs = seq(0, 1, by = 0.1))

  # Étiquettes de classification
  labels <- c("Lowest","Very Very Low", "Very Low", "Low", "Medium Low", "Medium", "Medium High", "High", "Very High", "Very Very High", "Highest")

  # Classification des valeurs
  classes <- cut(as.vector(layer_matrix), breaks = c(quantile, Inf), labels = labels, include.lowest = TRUE)

  # Conversion en matrice numérique
  class_matrix <- matrix(as.numeric(classes), nrow = nrow(layer_matrix), byrow = FALSE)

  return(class_matrix)
}
