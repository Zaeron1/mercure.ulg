#' Plot 3D Surface
#'
#' Cette fonction génère un graphique 3D interactif de la surface pour une couche spécifiée dans un tableau 3D.
#'
#' @param layer_index Un entier indiquant l'index de la couche à tracer. Doit être compris entre 1 et le nombre de couches dans `result_array`.
#' @param plot_title Un titre pour le graphique.
#'
#' @return Un graphique 3D interactif de la surface pour la couche spécifiée.
#' @export
#'
#' @examples
#' # Exemple d'utilisation de plot_3D_surface
#' result_array <- array(runif(100), dim = c(10, 10, 5))
#' plot_3D_surface(layer_index = 1, plot_title = "3D Surface Plot")
plot_3D_surface <- function(layer_index, plot_title) {

  # Vérifie si l'index de couche est dans les limites
  if (layer_index < 1 || layer_index > dim(result_array)[3]) {
    stop("Layer index out of bounds.")
  }

  # Extrait la couche spécifiée sous forme de matrice
  layer_matrix <- get_layer_as_matrix(result_array, layer_index)
  # Reverse the order of elements in each row
  reversed_matrix <- t(apply(layer_matrix, 1, rev))

  # Define axis labels
  axx <- list(title = "X Axis")
  axy <- list(title = "Y Axis")
  axz <- list(title = "Z Axis")

  # Create the interactive 3D plot
  fig <- plot_ly(z = ~reversed_matrix) %>%
    add_surface(colorscale = list(
      list(0, 'rgb(0, 0, 255)'),       # Blue for the lowest value
      list(0.5, 'rgb(255, 255, 255)'), # White for the median value
      list(1, 'rgb(255, 0, 0)')        # Red for the highest value
    )) %>%
    layout(
      title = plot_title,
      scene = list(
        xaxis = axx,
        yaxis = axy,
        zaxis = axz,
        aspectmode = 'manual',
        aspectratio = list(x = 2, y = 1, z = 0.07)
      )
    )

  # Display the plot
  fig
}
