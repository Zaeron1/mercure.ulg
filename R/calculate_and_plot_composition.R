#' Calculate and Plot Composition
#'
#' This function calculates the mean composition of various elements from provided matrices
#' representing different chemical layers. It then plots a pie chart to visually represent
#' the composition of Mercury's surface using Plotly.
#'
#' @details
#' The function retrieves data from specific layers of a given result array. It calculates
#' the mean values for each matrix and determines the relative percentage composition of
#' elements such as Si, Mg, Ca, Al, Fe, and S. The resulting composition is then plotted
#' as a pie chart.
#'
#' The matrices are extracted from layers of an external array `result_array`. Each layer
#' represents a specific chemical component of Mercury's surface.
#'
#' @note
#' Ensure that the `result_array` is loaded in the environment and that the indices specified
#' correspond to the correct layers. The Plotly library must be installed and loaded for the
#' pie chart to be displayed.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' \dontrun{
#' # Assuming 'result_array' is available in the workspace
#' calculate_and_plot_composition()
#' }
#'
#' @seealso \code{\link{plot_ly}}, \code{\link{layout}}
#'
#' @export
calculate_and_plot_composition <- function() {
  matrice_mgsi <- get_layer_as_matrix(result_array, layer_index = 8)
  matrice_casi <- get_layer_as_matrix(result_array, layer_index = 2)
  matrice_fesi <- get_layer_as_matrix(result_array, layer_index = 6)
  matrice_alsi <- get_layer_as_matrix(result_array, layer_index = 1)
  matrice_ssi <- get_layer_as_matrix(result_array, layer_index = 9)

  # Liste des matrices
  matrices <- list(
    mgsi = matrice_mgsi,
    casi = matrice_casi,
    fesi = matrice_fesi,
    alsi = matrice_alsi,
    ssi = matrice_ssi
  )

  # Calcul des moyennes
  mean_values <- sapply(matrices, function(matrix) mean(matrix, na.rm = TRUE))

  # Fonction pour calculer la composition
  calculate_composition_from_means <- function(means) {
    total_moles <- 1 + sum(means)
    percentages <- c(1, means) / total_moles * 100
    names(percentages) <- c("Si", "Mg", "Ca", "Al", "Fe", "S")
    return(percentages)
  }

  # Composition
  composition_means <- calculate_composition_from_means(mean_values)
  print(composition_means)

  # Création du diagramme circulaire avec Plotly
  plot_ly(
    labels = names(composition_means),
    values = composition_means,
    type = 'pie',
    textinfo = 'label+percent',
    insidetextorientation = 'radial'
  ) %>%
    layout(
      title = "Composition de la surface de Mercure"
    )
}
