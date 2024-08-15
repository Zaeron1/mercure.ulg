#' Perform Linear Regression Analysis Between Density and Chemical Layers
#'
#' This function performs linear regression analysis between a specified density layer and multiple chemical layers within a 3-dimensional array, generating interactive regression plots using Plotly.
#'
#'
#' @param density_index Integer. The index of the layer in `result_array` that represents the density.
#' @param compare_indices Integer vector. A vector of indices corresponding to the layers in `result_array` to compare against the density layer.
#'
#' @details
#' The function extracts the specified density layer and each chemical layer from `result_array` using the provided indices. It filters the data to include only positive values, performs linear regression analysis, and plots the regression results interactively using Plotly. The function outputs a data frame containing the R-squared values for each regression analysis.
#'
#' @return A data frame with two columns: \code{Couche}, indicating the index of the compared layer, and \code{R_squared}, representing the coefficient of determination for each regression analysis.
#'
#' @examples
#' \dontrun{
#' # Assuming `result_array` is a 3D array with suitable data
#' regression_results <- analyse_regressions(density_index = 5, compare_indices = c(6, 7, 8))
#' print(regression_results)
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats lm
#'
#' @export
analyse_regressions <- function(density_index, compare_indices) {
  # Initialisation du tableau de résultats
  resultats_reg <- data.frame(Couche = character(), R_squared = numeric(), stringsAsFactors = FALSE)

  # Extraction de la matrice de densité
  matrice_density <- get_layer_as_matrix(result_array, density_index)

  # Define custom titles for specific indices
  custom_titles <- list(
    "1" = "Al/Si",
    "2" = "Ca/Si",
    "6" = "Fe/Si",
    "8" = "Mg/Si",
    "9" =  "SSi"
  )

  for (index in compare_indices) {
    # Extraction de la matrice chimique
    matrice_chim <- get_layer_as_matrix(result_array, index)

    # Filtrer les valeurs positives
    matrice_density_filtered <- matrice_density[matrice_chim > 0]
    matrice_chim_filtered <- matrice_chim[matrice_chim > 0]

    # Vérifier si les matrices sont non vides
    if (length(matrice_density_filtered) == 0 || length(matrice_chim_filtered) == 0) {
      warning(paste("Pas de valeurs positives pour l'indice", index))
      r_squared <- NA
    } else {
      # Déterminer la taille de l'échantillon
      sample_size <- min(10000, length(matrice_density_filtered))
      if (sample_size < 10000) {
        warning(paste("Sample size réduit à", sample_size, "pour l'indice", index))
      }

      # Échantillonnage
      positions <- sample(1:length(matrice_density_filtered), sample_size)
      valeurs_matrice_dens <- matrice_density_filtered[positions]
      valeurs_matrice_chim <- matrice_chim_filtered[positions]

      # Calcul du modèle de régression
      model <- lm(valeurs_matrice_chim ~ valeurs_matrice_dens)

      # Calcul du coefficient de détermination (R^2)
      r_squared <- summary(model)$r.squared

      # Determine the title for the plot and the Y-axis label
      if (index %in% names(custom_titles)) {
        custom_text <- custom_titles[[as.character(index)]]
        plot_title <- paste("Régression linéaire pour le rapport", custom_text, "et la densité")
        yaxis_title <- paste("Rapport", custom_text)
      } else {
        plot_title <- paste("Régression linéaire pour l'indice", index)
        yaxis_title <- paste("Rapport Indice", index)
      }

      # Création du graphique interactif avec R^2 affiché
      plot <- plot_ly() %>%
        add_trace(x = ~valeurs_matrice_dens, y = ~valeurs_matrice_chim, type = 'scatter', mode = 'markers',
                  marker = list(color = 'blue'), name = 'Data Points') %>%
        add_trace(x = ~valeurs_matrice_dens, y = fitted(model), mode = 'lines', line = list(color = 'red', width = 2), name = 'Regression Line') %>%
        layout(title = plot_title,
               xaxis = list(title = "Density"),
               yaxis = list(title = yaxis_title),
               annotations = list(
                 list(
                   x = max(valeurs_matrice_dens), y = max(valeurs_matrice_chim),
                   xref = 'x', yref = 'y',
                   text = paste("R^2 =", round(r_squared, 4)),
                   showarrow = TRUE,
                   arrowhead = 2,
                   ax = 20, ay = -30,
                   font = list(color = "black", size = 20)
                 )
               ))

      # Display the plot
      print(plot)
    }

    # Stockage des résultats
    resultats_reg <- rbind(resultats_reg, data.frame(Couche = paste("Indice", index), R_squared = r_squared))
  }

  return(resultats_reg)
}
