#' Generate Histograms for Mg/Si Ratios by Region
#'
#' This function generates histograms for the Magnesium to Silicon (Mg/Si) ratios
#' across different geological regions. Each region is plotted separately, with the
#' distribution of Mg/Si ratios visualized as density plots.
#'
#' @param corrected_mgsi A matrix or data layer representing the corrected Mg/Si ratios.
#'   This should be obtained using the \code{get_layer_as_matrix} function to ensure
#'   the data is in the correct format for processing.
#' @param regions A matrix or data layer indicating the region classifications for
#'   each data point. This should also be processed using the \code{get_layer_as_matrix}
#'   function to match the format of \code{corrected_mgsi}.
#'
#' @return This function does not return a value. Instead, it creates histograms
#'   as side effects, displaying the density distribution of Mg/Si ratios for each region.
#'
#' @details The function iterates over predefined region names, generating a histogram
#'   for each one. The regions include:
#'   \enumerate{
#'     \item High-Mg region
#'     \item High-Al region
#'     \item Caloris Planitia
#'     \item Rachmaninoff
#'     \item High-Mg NVP
#'     \item Low-Mg NVP
#'     \item Unclassified
#'   }
#'   The histogram for the "Unclassified" region is managed using the modulo operation
#'   to correctly index the regions array.
#'
#' @note Ensure that both \code{corrected_mgsi} and \code{regions} are properly
#'   pre-processed and transformed into matrix form using \code{get_layer_as_matrix}
#'   to avoid mismatches in data dimensions.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' corrected_mgsi_matrix <- get_layer_as_matrix(result_array, corrected_mgsi)
#' regions_matrix <- get_layer_as_matrix(result_array, regions)
#' generate_histograms(corrected_mgsi_matrix, regions_matrix)
#' }
#'
#' @export
generate_histograms <- function(corrected_mgsi, regions) {
  # Liste des noms des régions
  region_names <- c(
    "1: High-Mg region",
    "2: High-Al region",
    "3: Caloris Planitia",
    "4: Rachmaninoff",
    "5: High-Mg NVP",
    "6: Low-Mg NVP",
    "0: Unclassified"
  )

  corrected_mgsi <- get_layer_as_matrix(result_array, corrected_mgsi)
  regions <- get_layer_as_matrix(result_array, regions)


  for (i in seq_along(region_names)) {
    # Crée l'histogramme pour la région spécifique
    hist(
      corrected_mgsi[regions == (i %% 7)], # i %% 7 est utilisé pour gérer le cas du "Unclassified" avec index 0
      freq = FALSE,
      xlim = c(0, 1),
      col = "lightblue",
      xlab = "Rapport Mg/Si",
      ylab = "Densité",
      main = region_names[i]
    )
  }
}
