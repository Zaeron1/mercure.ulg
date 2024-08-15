#' Get Layer as Matrix
#'
#' Cette fonction extrait une couche spécifique d'un tableau 3D et la retourne sous forme de matrice 2D.
#'
#' @param result_array Un tableau 3D contenant plusieurs couches de données.
#' @param layer_index Un entier indiquant l'index de la couche à extraire. Doit être compris entre 1 et le nombre de couches dans `result_array`.
#'
#' @return Une matrice 2D représentant la couche spécifiée du tableau 3D.
#' @export
#'
#' @examples
#' # Création d'un tableau 3D pour l'exemple
#' result_array <- array(runif(300), dim = c(10, 10, 3))
#' # Extraction de la première couche sous forme de matrice
#' matrice <- get_layer_as_matrix(result_array, layer_index = 1)
#' print(matrice)
get_layer_as_matrix <- function(result_array, layer_index) {

  if (layer_index < 1 || layer_index > dim(result_array)[3]) {
    stop("Layer index out of bounds.")
  }

  # Extrait et retourne la couche spécifiée sous forme de matrice
  return(result_array[,,layer_index])
}
