#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importClassesFrom tibble tbl_df
#' @importFrom dplyr mutate slice_head slice_sample filter select everything
#' @importFrom ggplot2 aes coord_cartesian coord_fixed element_blank element_line element_rect element_text geom_curve geom_freqpoly geom_histogram geom_point geom_segment
#' @importFrom ggplot2  ggplot ggtitle guide_legend rel scale_colour_identity theme theme_bw xlab ylab
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid arrow unit
#' @importFrom here here
#' @importFrom magrittr %>%
#' @importFrom methods new setMethod validObject setGeneric setValidity is initialize show
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom readr read_rds write_rds read_tsv write_tsv read_csv write_csv
#' @importFrom rlang .data env env_poke env_get env_get_list env_has env_names env_unbind env_print pkg_env
#' @importFrom shiny column conditionalPanel fluidPage fluidRow mainPanel navbarPage sidebarLayout sidebarPanel tabPanel titlePanel shinyApp downloadButton
#' @importFrom shiny actionButton checkboxGroupInput checkboxInput fileInput numericInput radioButtons selectInput sliderInput downloadHandler
#' @importFrom shiny eventReactive observeEvent reactive updateNumericInput updateSelectInput
#' @importFrom shiny h1 h2 h3 h4 h5 h6 hr icon p
#' @importFrom shiny plotOutput renderPlot renderTable renderText tableOutput textOutput renderPrint textInput
#' @importFrom shinythemes shinytheme
#' @importFrom stats quantile rexp rnorm runif rweibull
#' @importFrom stringr str_ends str_c
#' @importFrom tibble add_row tibble as_tibble is_tibble add_column
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname){
  packageStartupMessage(paste0("kindisperse v", utils::packageVersion("kindisperse")))
}
