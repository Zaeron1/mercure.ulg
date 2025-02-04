% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_matrix.R
\name{plot_matrix}
\alias{plot_matrix}
\title{Plot a 3D Array Layer or a Provided Matrix with Plotly}
\usage{
plot_matrix(layer_index_or_name, main_title, legend_name, color_palette)
}
\arguments{
\item{layer_index_or_name}{Numeric or Matrix. If numeric, it is the index of the layer to be extracted and plotted from `result_array`. If a matrix, the matrix will be plotted directly.}

\item{main_title}{Character. The main title of the plot.}

\item{legend_name}{Character. The label for the color legend.}

\item{color_palette}{Function. A color palette function that generates the colors for the plot.}
}
\value{
A Plotly object. The function returns an interactive heatmap plot.
}
\description{
This function creates an interactive heatmap plot using Plotly, displaying either a specified layer from a 3-dimensional array or a provided matrix.
}
\details{
This function leverages Plotly to create an interactive heatmap from a matrix or a specific layer from a 3-dimensional array named `result_array`. The plot includes customized axes labels for longitude and latitude and a color legend to represent values in the matrix.
}
\examples{
\dontrun{
# Assuming `result_array` is a 3D array with suitable dimensions
plot_matrix_plotly(1, "Sample Plot", "Legend", colorRampPalette(c("blue", "yellow", "red")))

# Using a predefined matrix
my_matrix <- matrix(1:100, nrow = 10)
plot_matrix(my_matrix, "My Matrix Plot", "Values", colorRampPalette(c("blue", "green", "red")))
}

}
