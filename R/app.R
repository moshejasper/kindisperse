
# The real deal app... here we come!!!!

#library(plotly)
#library(tidyverse)
#library(here)
#library(ggrepel)
#library(shiny)
##library(shinythemes)
#library(fitdistrplus)
#library(LaplacesDemon)

# preload some data here!

#source("kindisperse_functions.R")

# run early setup functions

sim_storage <- list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)


ui <- fluidPage(

  titlePanel("Kin Dispersal Simulator"),

  theme = shinytheme("flatly"),

  fluidRow(
    column(offset = 4, width = 4,

           textOutput(outputId = "appstatus")),
    column(width = 4,
           h4("________________________Stored Data_____________________"),
           h4(style = "font-size:12px;",
              tableOutput(
                outputId = "est_std_sumtable"
              )),
           actionButton(
             inputId = "top_data_update",
             label = "Update"
           )
    )),


  navbarPage(
    "App Functions",

    selected = "Simulate",
    #theme = "bootstrap (2).css",

    ############# Tutorial Tab #############

    tabPanel("Tutorial",
             h1("Intergenerational Dispersal"),
             p("Intergenerational dispersal is a key process that connects biological events across the lifespan of an organism with broader demographic and population genetic processes of interest (such as isolation by distance, and ultimately selective processes and speciation). "),
             p("At its broadest, it refers to the change in the geographical positions of descendent generations when compared to ancestral generations. Its simplest unit is parent-offspring dispersal - the displacement that occurs between a parent and its offspring across a lifespan. "),
             h2("Introduction"),
             p("Dispersal is an important process in biology generally, especially in conservation and pest management. It is one of the many reasons we wished to do this research. Few others are as apparent."),
             p("Below is an example of a simple dispersal process that we have preloaded"),

             navbarPage(
               "The Intergenerational Dispersal Kernel",

               ########## Tute Tab 1 ############

               tabPanel(
                 "1. The Lifespan",

                 p("The most basic measurement of intergenerational dispersal is that across one lifespan from parent to offspring, e.g. the geographical location of the birth of the mother compared to that of the birth of her offspring"),
                 p("This parent-offspring distance (PO) is fundamental to Wright's theory of isolation by distance (Wright, 19xx), and can be understood statistically via a dispersal kernel"),
                 p("Such a kernel describes the probability distribution of parents to offspring across two dimensions, and can be modelled with a variety of distributions (Gaussian, Laplace, etc.)"),
                 p("For this example, we will be using a Gaussian distribution"),
                 fluidRow(
                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_1a"
                     )
                   ),


                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_1b"
                     )
                   )
                 )
               ),

               ############ Tute Tab 2 #############

               tabPanel(
                 "2. The Kernel",
                 p("Filler text here"),
                 fluidRow(
                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_2a"
                     )
                   ),
                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_2b"
                     )
                   )
                 )
               ),

               ############# Tute Tab 3 #############

               tabPanel(
                 "3. Extending to further generations",
                 p("Now look what happens when you add the GG generation..."),
                 fluidRow(
                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_3a"
                     )
                   ),
                   column(
                     width = 6,
                     plotOutput(
                       outputId = "tutorial_3b"
                     )
                   )
                 )
               ),

               ############ Tute Tab 4 #############

               tabPanel(
                 "4. Decomposing the kernel"
               ),

               ########### Tute Tab 5 ###########

               tabPanel(
                 "5. Sandbox",

                 sidebarLayout(

                   sidebarPanel(

                     # Input: Slider for the number of bins
                     sliderInput(inputId = "sand_dsigma",
                                 label = "Axial dispersal Sigma (m):",
                                 min = 5,
                                 max = 250,
                                 value = 25),

                     sliderInput(inputId = "sand_nsims",
                                 label = "Number of families to trace",
                                 min = 1,
                                 max = 100,
                                 value = 5),

                     sliderInput(inputId = "sand_dims",
                                 label = "Dimensions of parent area (m)",
                                 min = 50,
                                 max = 1000,
                                 value = 250),

                     selectInput(inputId = "sand_category",
                                 label = "Choose kinship category to examine",
                                 choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG",
                                             "1C", "1C1", "2C", "GAV"),
                                 selected = "1C"),

                     checkboxInput(inputId = "sand_labls",
                                   label = "Show Labels",
                                   value = FALSE),

                     checkboxInput(inputId = "sand_moves",
                                   label = "Show Movements",
                                   value = TRUE),

                     checkboxInput(inputId = "sand_shadows",
                                   label = "Show Shadows",
                                   value = FALSE),

                     checkboxInput(inputId = "sand_show_area",
                                   label = "Show Parent Area",
                                   value = TRUE),

                     checkboxInput(inputId = "sand_lengths",
                                   label = "Show Final Distances",
                                   value = TRUE),

                     checkboxInput(inputId = "sand_lengthlabs",
                                   label = "Show Distance Value",
                                   value = TRUE),

                     selectInput(inputId = "sand_graphtype",
                                 label = "Choose graph type",
                                 choices = c("Basic", "Pinwheel", "Scatter",
                                             "Histogram", "FreqPoly", "Centred",
                                             "Individual"),
                                 selected = "Basic"),

                     sliderInput(inputId = "sand_binwidth",
                                 label = "Histogram binwidth (m)",
                                 min = 1,
                                 max = 50,
                                 value = 5),

                     checkboxInput(inputId = "sand_scaled",
                                   label = "Scale by sigma (individual plot)",
                                   value = TRUE),

                     sliderInput(inputId = "sand_scalefactor",
                                 label = "Scale factor (multiples of sigma)",
                                 min = 1,
                                 max = 10,
                                 value = 4),

                     sliderInput(inputId = "sand_pairs",
                                 label = "Number of pairs to trace (pin, hist & scatter)",
                                 min = 1,
                                 max = 10000,
                                 value = 20)
                   ),

                   mainPanel(

                     # Output: histogram --
                     plotlyOutput(outputId = "sand_dispersalPlot", height = 750),

                     textOutput(outputId = "sand_dispersalcheck")

                   )


                 )
               )
             ),


             sidebarLayout(
               sidebarPanel(
                 "Technical Details"
               ),

               mainPanel("Boring graph here...")
             )

    ),

    tabPanel("Load",
             h1("Load Dispersal & Kinship Data from Files"),

             sidebarLayout(
               sidebarPanel(

                 p("Upload a .csv file with a 'category' column obeying standard conventions and a 'distance' column (numeric)"),

                 fileInput(
                   inputId = "load_file1",
                   label = "Choose .csv to upload",
                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                 )

               ),
               mainPanel(

                 tableOutput("load_stats1"),

                 tableOutput("load_contents")

               )
             )
    ),

    ####_####
    ######################### Simulate Tab #############################

    tabPanel("Simulate",
             h1("Generate and Test Dispersal Scenarios"),
             p("This tab is for testing the impact of various parameters on dispersal estimates. Efforts are made to make these simulations as extensive as possible. (1 million iterations)"),
             hr(),

             navbarPage(
               "Simulation Type",


               ######### 1. Simple Tab ##########

               tabPanel(
                 "Simple",

                 sidebarLayout(
                   sidebarPanel(
                     numericInput(inputId = "sim_simple_nsims",
                                  label = "Number to simulate (maximum 1 million)",
                                  min = 1, max = 1000000, value = 100000),

                     radioButtons(
                       inputId = "sim_type",
                       label = "Simulation Type",
                       choices = c("Simple", "Composite")
                     ),

                     conditionalPanel(
                       condition = "input.sim_type == 'Simple'",

                       sliderInput(
                         inputId = "sim_simple_sigma",
                         label = "Simple dispersal sigma",
                         min = 1, max = 250, value = 50
                       )),

                     sliderInput(
                       inputId = "sim_simple_dims",
                       label = "Site dimensions (n x n)",
                       min = 25, max = 1000, value = 1000
                     ),

                     selectInput(
                       inputId = "sim_simple_category",
                       label = "Kinship Category",
                       choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                   "HGAV", "H1C", "H1C1", "H2C"),
                       selected = "PO"
                     ),

                     radioButtons(
                       inputId = "sim_simple_lifestage",
                       label = "Lifestage at sampling",
                       choices = c("larva", "oviposition"),
                       selected = "larva"
                     ),

                     radioButtons(
                       inputId = "sim_simple_method",
                       label = "Dispersal Kernel Type",
                       choices = c("Gaussian", "Laplace"),
                       selected = "Gaussian"
                     ),

                     sliderInput(
                       inputId = "sim_simple_binwidth",
                       label = "Binwidth",
                       min = 1, max = 50, value = 10
                     ),

                     selectInput(
                       inputId = "sim_simple_saveops",
                       label = "Choose storage slot",
                       choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                   "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                     ),

                     actionButton(
                       inputId = "sim_simple_storeclick",
                       label = "Store",
                       icon = icon("archive")
                     )

                   ),

                   mainPanel(

                     plotOutput(
                       outputId = "sim_simple_hist",
                       height = "600px"
                     )
                   )
                 )

               ),


               ############ 2. Composite Tab #############
               tabPanel(
                 "Composite",

                 sidebarLayout(
                   sidebarPanel(
                     numericInput(inputId = "sim_composite_nsims",
                                  label = "Number to simulate (maximum 1 million)",
                                  min = 1, max = 1000000, value = 100000),

                     numericInput(
                       inputId = "sim_composite_juvsigma",
                       label = "Pre-breeding dispersal sigma",
                       min = 1, max = 250, value = 50
                     ),

                     numericInput(
                       inputId = "sim_composite_breedsigma",
                       label = "Breeding dispersal sigma",
                       min = 1, max = 250, value = 50
                     ),

                     numericInput(
                       inputId = "sim_composite_gravsigma",
                       label = "Post-breeding dispersal sigma",
                       min = 1, max = 250, value = 50
                     ),

                     numericInput(
                       inputId = "sim_composite_ovisigma",
                       label = "Oviposition dispersal sigma",
                       min = 1, max = 250, value = 50
                     ),

                     numericInput(
                       inputId = "sim_composite_dims",
                       label = "Site dimensions (n x n)",
                       min = 25, max = 1000, value = 1000
                     ),

                     selectInput(
                       inputId = "sim_composite_category",
                       label = "Kinship Category",
                       choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                   "HGAV", "H1C", "H1C1", "H2C"),
                       selected = "PO"
                     ),

                     radioButtons(
                       inputId = "sim_composite_lifestage",
                       label = "Lifestage at sampling",
                       choices = c("larva", "oviposition"),
                       selected = "larva"
                     ),

                     radioButtons(
                       inputId = "sim_composite_method",
                       label = "Dispersal Kernel Type",
                       choices = c("Gaussian", "Laplace"),
                       selected = "Gaussian"
                     ),

                     numericInput(
                       inputId = "sim_composite_binwidth",
                       label = "Binwidth",
                       min = 1, max = 50, value = 10
                     ),

                     selectInput(
                       inputId = "sim_composite_saveops",
                       label = "Choose storage slot",
                       choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                   "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                     ),

                     actionButton(
                       inputId = "sim_composite_storeclick",
                       label = "Store",
                       icon = icon("archive")
                     )

                   ),

                   mainPanel(

                     plotOutput(
                       outputId = "sim_composite_hist",
                       height = "600px"
                     )
                   )
                 )
               ),

               ##### 3. Compare Tab #####
               tabPanel(
                 "Compare Distributions",

                 sidebarLayout(
                   sidebarPanel(

                     numericInput(
                       inputId = "testnum",
                       label = "enter a number here",
                       min = 0, max = 1000, value = 1
                     ),

                     checkboxGroupInput(
                       inputId = "testsaveops",
                       label = "choose store slot",
                       choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4',
                                   "Slot 5" = '5', "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8',
                                   "Slot 9" = '9', "Slot 10" = '10')
                     ),

                     actionButton(
                       inputId = "storeclick",
                       label = "Store",
                       icon = icon("archive")
                     ),

                     actionButton(
                       inputId = "retrieveclick",
                       label = "Retrieve"
                     ),

                     actionButton(
                       inputId = "clearclick",
                       label = "Clear"
                     ),

                     textOutput(
                       outputId = "testshow"
                     )
                   ),

                   mainPanel(
                     plotlyOutput(
                       outputId = "sim_compare_plot"
                     ),
                     h5(
                       tableOutput(
                         outputId = "sim_compare_table"
                       ))
                   ))
               ),

               tabPanel(
                 "FiRa comparisons"
               )

             )

    ),

    ####_####
    ##################### Sample Tab ##########################

    tabPanel("Sample",
             h1("Use Sample Simulations to Design Sampling Scheme"),
             p("This tab uses data from the simulation tab to design sampling schemes that increase the reliability of the sigma estimates"),
             hr(),



             sidebarLayout(
               sidebarPanel(

                 radioButtons(
                   inputId = "samp_distribution_select",
                   label = "Which simulated distribution do you wish to sample?",
                   choices = c("Simple" = "samp_simple", "Composite" = "samp_composite", "Stored" = "samp_stored"),
                   selected = "samp_simple"
                 ),

                 conditionalPanel(
                   condition = "input.samp_distribution_select == 'samp_stored'",

                   selectInput(
                     inputId = "samp_retrieve_choice",
                     label = "Which data do you wish to retrieve?",
                     choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                 "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                   ),

                   actionButton(
                     inputId = "samp_retrieveclick",
                     label = "Retrieve"
                   ),

                   textOutput(
                     outputId = "samp_retrieve_current",
                   )

                 ),

                 checkboxGroupInput(
                   inputId = "samp_checkbox",
                   label = "Which sample  settings do you wish to apply?",
                   choiceNames = c("Lower limit", "Upper Limit", "Dimensions", "Trap Spacing", "Number Sampled"),
                   choiceValues = c("use_samp_lower", "use_samp_upper", "use_samp_dims", "use_samp_spacing", "use_samp_n")
                 ),

                 conditionalPanel(condition = "input.samp_checkbox.includes('use_samp_lower')",
                                  numericInput(
                                    inputId = "samp_lower",
                                    label = "choose trap lower range",
                                    min = 1, max = 250, value = 0
                                  )),

                 conditionalPanel(condition = "input.samp_checkbox.includes('use_samp_upper')",
                                  numericInput(
                                    inputId = "samp_upper",
                                    label = "choose trap upper range",
                                    min = 0, max = 1000, value = 1000
                                  )),

                 conditionalPanel(condition = "input.samp_checkbox.includes('use_samp_dims')",
                                  numericInput(
                                    inputId = "samp_dims",
                                    label = "choose trap dimensions (n by n)",
                                    min = 25, max = 1000, value = 100
                                  )),

                 conditionalPanel(condition = "input.samp_checkbox.includes('use_samp_spacing')",
                                  numericInput(
                                    inputId = "samp_spacing",
                                    label = "choose trap spacing",
                                    min = 0.1, max = 250, value = 0.1
                                  )),

                 conditionalPanel(condition = "input.samp_checkbox.includes('use_samp_n')",
                                  numericInput(
                                    inputId = "samp_n",
                                    label = "choose maximum number sampled",
                                    min = 1, max = 100000, value = 10000
                                  )),


                 numericInput(
                   inputId = "samp_binwidth",
                   label = "Choose graph binwidth",
                   min = 1, max = 100, value = 5
                 ),

                 selectInput(
                   inputId = "samp_saveops",
                   label = "Choose storage slot",
                   choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                               "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                 ),

                 actionButton(
                   inputId = "samp_storeclick",
                   label = "Store",
                   icon = icon("archive")
                 )
               ),



               mainPanel(

                 plotOutput(
                   outputId = "samphist",
                   height = "600px"
                 ),

                 h4(
                   tableOutput(
                     outputId = "sampstats"
                   )
                 ),

                 tableOutput(
                   outputId = "samp_retrieve_table"
                 )
               )
             )
    ),

    ####_####
    ######################### Estimate Tab ##############################

    tabPanel("Estimate",
             h1("Estimate dispersal sigmas from data"),
             p("This tab is for estimating one or multiple sigmas from test data"),

             navbarPage(
               "Type",

               ##### a. single #####
               tabPanel(
                 "Single Kernel",

                 sidebarLayout(
                   sidebarPanel(

                     selectInput(
                       inputId = "est_smp_source",
                       label = "Choose data source",
                       choices = c("Simulation (simple)" = "simple", "Simulation (composite)" = "composite",
                                   "Sampled" = "sampled", "Stored" = "stored", "Data file" = "filedata")
                     ),

                     conditionalPanel(
                       condition = "input.est_smp_source.includes('stored')",
                       selectInput(
                         inputId = "est_smp_retrieve_choice",
                         label = "Which data do you wish to retrieve?",
                         choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                     "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                       ),

                       actionButton(
                         inputId = "est_smp_retrieveclick",
                         label = "Retrieve"
                       )
                     ),

                     numericInput(
                       inputId = "est_smp_bootstraps",
                       label = "Bootstraps",
                       min = 100, max = 100000, value = 1000, step = 100
                     ),
                     p("Large values could crash the app"),

                     numericInput(
                       inputId = "est_smp_bootnum",
                       label = "Samples per bootstrap",
                       min = 5, max = 1000, value = 50
                     ),

                     radioButtons(
                       inputId = "est_smp_mode",
                       label = "Choose calculation type",
                       choices = c("Raw (e.g. PO, GG)" = 1, "Decompose (e.g. FS, 1C)" = 2)
                     )
                   ),

                   mainPanel(
                     h3(
                       tableOutput(
                         outputId = "est_smp_ci_table"
                       )
                     )
                   )
                 )
               ),

               ##### b. standard #####
               tabPanel(
                 "Standard",

                 sidebarLayout(
                   sidebarPanel(

                     selectInput(
                       inputId = "est_std_retrieve_choice_lrg",
                       label = "Choose larger axial distribution",
                       choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                   "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                     ),

                     selectInput(
                       inputId = "est_std_rcl_category",
                       label = "Kinship Category",
                       choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                   "HGAV", "H1C", "H1C1", "H2C"),
                       selected = "1C"
                     ),

                     checkboxInput(
                       inputId = "est_std_rclmix",
                       label = "Mixture",
                       value = FALSE
                     ),

                     conditionalPanel(
                       condition = "input.est_std_rclmix == true",
                       selectInput(
                         inputId = "est_std_rcl_category2",
                         label = "Mixture Kinship Category",
                         choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                     "HGAV", "H1C", "H1C1", "H2C"),
                         selected = "H1C"
                       )
                     ),

                     checkboxInput(
                       inputId = "est_std_rclcomp",
                       label = "Composite",
                       value = FALSE
                     ),

                     conditionalPanel(
                       condition = "input.est_std_rclcomp == true",
                       selectInput(
                         inputId = "est_std_retrieve_choice_lrg2",
                         label = "...      composite distribution",
                         choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                     "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                       ),

                       selectInput(
                         inputId = "est_std_rcl2_category",
                         label = "... kinship category",
                         choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                     "HGAV", "H1C", "H1C1", "H2C"),
                         selected = "H1C"
                       )
                     ),

                     selectInput(
                       inputId = "est_std_retrieve_choice_sml",
                       label = "Choose smaller axial distribution",
                       choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                   "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                     ),

                     selectInput(
                       inputId = "est_std_rcs_category",
                       label = "Kinship Category",
                       choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                   "HGAV", "H1C", "H1C1", "H2C"),
                       selected = "FS"
                     ),

                     checkboxInput(
                       inputId = "est_std_rcsmix",
                       label = "Mixture",
                       value = FALSE
                     ),

                     conditionalPanel(
                       condition = "input.est_std_rcsmix == true",
                       selectInput(
                         inputId = "est_std_rcs_category2",
                         label = "Mixture Kinship Category",
                         choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                     "HGAV", "H1C", "H1C1", "H2C"),
                         selected = "HS"
                       )
                     ),

                     checkboxInput(
                       inputId = "est_std_rcscomp",
                       label = "Composite",
                       value = FALSE
                     ),

                     conditionalPanel(
                       condition = "input.est_std_rcscomp == true",
                       selectInput(
                         inputId = "est_std_retrieve_choice_sml2",
                         label = "...      composite distribution",
                         choices = c("Slot 1" = '1', "Slot 2" = '2', "Slot 3" = '3', "Slot 4" = '4', "Slot 5" = '5',
                                     "Slot 6" = '6', "Slot 7" = '7', "Slot 8" = '8', "Slot 9" = '9', "Slot 10" = '10')
                       ),

                       selectInput(
                         inputId = "est_std_rcs2_category",
                         label = "... kinship category",
                         choices = c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                                     "HGAV", "H1C", "H1C1", "H2C"),
                         selected = "HS"
                       )
                     ),

                     numericInput(
                       inputId = "est_std_bootstraps",
                       label = "Bootstraps",
                       min = 100, max = 100000, value = 1000, step = 100
                     ),
                     p("Large values could crash the app"),

                     radioButtons(
                       inputId = "est_std_samptype",
                       label = "Samples per bootstrap",
                       choices = c("Standard" = "std", "Set Manually" = "man"),
                       selected = "std"
                     ),

                     conditionalPanel(
                       condition = "input.est_std_samptype.includes('man')",

                       numericInput(
                         inputId = "est_std_bootnum",
                         label = "Pick number",
                         min = 5, max = 1000, value = 50
                       ),
                     ),

                     hr(),

                     actionButton(
                       inputId = "est_std_run",
                       label = "Run"
                     )

                   ),

                   mainPanel(
                     h3(
                       tableOutput(
                         outputId = "est_std_ci_table"
                       )
                     )

                     #tableOutput(
                     #   outputId = "est_std_sumtable"
                     #)
                   )
                 )
               ),

               ##### c. custom #####
               tabPanel(
                 "Custom"
               )
             )

    )
  ),

  hr(),
  textOutput(
    outputId = "errorbar"
  ),
  hr()
)




####_####
####_####





server <- function(input, output, session){
  #output$dhist <- renderPlot({
  #  hist(rnorm(input$sigma))
  #  })
  tutorial_data1 <- simgraph_data(nsims = 10000, dsigma = 25, category = "PO")


  # Simulate Tab


  ############# Setup ################

  output$appstatus <- renderText({
    "Everything is A-OK"
  })


  ############## Maintenance ###############


  ####_####
  ############# Tutorial ##################


  ### Tab 1 ###

  output$tutorial_1a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 5, category = "PO")
  })

  output$tutorial_1b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 20, category = "PO", pinwheel = TRUE)
  })

  ### Tab 2 ###

  output$tutorial_2a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, category = "PO", scattered = TRUE)
  })

  output$tutorial_2b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, category = "PO", histogram = TRUE, binwidth = 2.5)
  })

  ### Tab 3 ###

  output$tutorial_3a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 1, category = "GG", show_area = F, labls = F)
  })

  output$tutorial_3b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, category = "GG", histogram = TRUE, binwidth = 2.5)
  })

  ### Tab 4 ###


  ### Tab 5 ###

  sandbox_data <- reactive({
    simgraph_data(nsims = input$sand_nsims, dsigma = input$sand_dsigma, dims = input$sand_dims, category = input$sand_category)
  })

  output$sand_dispersalcheck <- renderText({
    dim(sandbox_data())
  })

  output$sand_dispersalPlot <- renderPlotly({
    ggplotly(simgraph_graph(sandbox_data(), nsims = input$sand_nsims, dsigma = input$sand_dsigma, dims = input$sand_dims, labls = input$sand_labls,
                            moves = input$sand_moves, shadows = input$sand_shadows, category = input$sand_category,
                            show_area = input$sand_show_area, #centred = input$sand_centred, #pinwheel = input$sand_pinwheel, scattered = input$sand_scattered,
                            lengths = input$sand_lengths, lengthlabs = input$sand_lengthlabs))#, histogram = input$sand_histogram, binwidth = input$sand_binwidth)#,
    #freqpoly = input$sand_freqpoly)
  })

  ####_####
  ################## Simulate #####################


  #### Simple ####

  sim_simple_kindata <- reactive({

    if (input$sim_simple_nsims > 1000000) {
      updateNumericInput(
        session, "sim_simple_nsims",
        value = 1000000
      )
      return(NULL)
    }

    simulate_kindist_simple(nsims = input$sim_simple_nsims, sigma = input$sim_simple_sigma, method = input$sim_simple_method,
                            category = input$sim_simple_category, lifestage = input$sim_simple_lifestage, dims = input$sim_simple_dims)
  })

  sim_simple_store <- observeEvent(input$sim_simple_storeclick, {
    saveval <- sim_simple_kindata()
    saveRDS(saveval, file = here(paste0("temp/", input$sim_simple_saveops, ".R")))
  })

  output$sim_simple_hist <- renderPlot({
    if (is.null(sim_simple_kindata())) { return(NULL)}
    ggplot(sim_simple_kindata()@tab) + aes(x = .data$distance) +
      geom_histogram(binwidth = input$sim_simple_binwidth, fill = "white", colour = "grey30") +
      theme_bw()
  })


  #### Composite ####

  sim_composite_kindata <- reactive({

    if (input$sim_composite_nsims > 1000000){
      updateNumericInput(session, "sim_composite_nsims", value = 1000000)
      return(NULL)
    }
    simulate_kindist_composite(nsims = input$sim_composite_nsims, juvsigma = input$sim_composite_juvsigma, breedsigma = input$sim_composite_breedsigma,
                               gravsigma = input$sim_composite_gravsigma, ovisigma = input$sim_composite_ovisigma, dims = input$sim_composite_dims,
                               method = input$sim_composite_method, category = input$sim_composite_category, lifestage = input$sim_composite_lifestage)
  })

  sim_composite_store <- observeEvent(input$sim_composite_storeclick, {
    saveval <- sim_composite_kindata()
    saveRDS(saveval, file = here(paste0("temp/", input$sim_composite_saveops, ".R")))
  })

  output$sim_composite_hist <- renderPlot({
    if (is.null(sim_composite_kindata())) {return(NULL)}
    ggplot(sim_composite_kindata()@tab) + aes(x = .data$distance) +
      geom_histogram(binwidth = input$sim_composite_binwidth, fill = "white", colour = "grey30") +
      theme_bw()
  })


  #### Compare ####

  # store test...

  #teststorage <- reactiveValues('1' = NULL, '2'=NULL, '3'=NULL, '4'=NULL, '5'=NULL, '6'=NULL, '7'=NULL, '8'=NULL, '9'=NULL, '10'=NULL)

  testevent <- observeEvent(input$storeclick, {
    sim_storage[[as.integer(input$testsaveops)]] <- input$testnum
    write_rds(input$testnum, here(paste0("temp/", input$testsaveops, ".R")))
  })

  delevent <- observeEvent(input$clearclick, {
    if (file.exists(here(paste0("temp/",input$testsaveops, ".R")))) {file.remove(here(paste0("temp/",input$testsaveops, ".R")))}
  })


  retrieved <- eventReactive(input$retrieveclick, {

    if (! file.exists(here(paste0("temp/",input$testsaveops, ".R")))) {return(NULL)}

    read_rds(here(paste0("temp/", input$testsaveops, ".R")))

  })

  output$sim_compare_plot <- renderPlotly({
    gp <- ggplot(sim_simple_kindata()@tab) + aes(x = .data$distance)
    if ('1' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/1.R")))@tab, colour = "blue", binwidth = 5)
    }
    if ('2' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/2.R")))@tab, colour = "red", binwidth = 5)
    }
    if ('3' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/3.R")))@tab, colour = "orange", binwidth = 5)
    }
    if ('4' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/4.R")))@tab, colour = "green", binwidth = 5)
    }
    if ('5' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/5.R")))@tab, colour = "purple", binwidth = 5)
    }
    if ('6' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/6.R")))@tab, colour = "black", binwidth = 5)
    }
    if ('7' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/7.R")))@tab, colour = "grey", binwidth = 5)
    }
    if ('8' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/8.R")))@tab, colour = "pink", binwidth = 5)
    }
    if ('9' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/9.R")))@tab, colour = "brown", binwidth = 5)
    }
    if ('10' %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = readRDS(here(paste0("temp/10.R")))@tab, colour = "yellow", binwidth = 5)
    }
    gp
    ggplotly()
  })

  output$sim_compare_table <- renderTable({
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Category" = "d", "Lifestage" = "e", "Dims" = 0, "Colour" = "f", .rows = 0)
    if ('1' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/1.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "blue")
    }
    if ('2' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/2.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "red")
    }
    if ('3' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/3.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "orange")
    }
    if ('4' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/4.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "green")
    }
    if ('5' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/5.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "purple")
    }
    if ('6' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/6.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "black")
    }
    if ('7' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/7.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "grey")
    }
    if ('8' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/8.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "pink")
    }
    if ('9' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/9.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "brown")
    }
    if ('10' %in% input$testsaveops) {
      temp <- readRDS(here(paste0("temp/10.R")))
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Colour" = "yellow")
    }
    rtable
  })

  output$testshow <- renderText({
    #is.character(input$testsaveops)
    retrieved()
    #teststorage$a
    #input$storeclick
  })

  #### FiRa Compare ####

  ##### _ #####
  ############### Sample ################

  res_lower <- reactive({
    if ("use_samp_lower" %in% input$samp_checkbox) {
      temp <- input$samp_lower
    }
    else {temp <- NULL}
    return(temp)
  })

  res_upper <- reactive({
    if ("use_samp_upper" %in% input$samp_checkbox) {
      temp <- input$samp_upper
    }
    else {temp <- NULL}
    return(temp)
  })

  res_dims <- reactive({
    if ("use_samp_dims" %in% input$samp_checkbox) {
      temp <- input$samp_dims
    }
    else {temp <- NULL}
    return(temp)
  })

  res_spacing <- reactive({
    if ("use_samp_spacing" %in% input$samp_checkbox) {
      temp <- input$samp_spacing
    }
    else {temp <- NULL}
    return(temp)
  })

  res_n <- reactive({
    if ("use_samp_n" %in% input$samp_checkbox) {
      temp <- input$samp_n
    }
    else {temp <- NULL}
    return(temp)
  })

  samp_loaded_kindata <- eventReactive(input$samp_retrieveclick, {
    if (input$samp_distribution_select == "samp_stored") {
      if (! file.exists(here(paste0("temp/",input$samp_retrieve_choice, ".R")))) {return(NULL)}
      return(readRDS(here(paste0("temp/", input$samp_retrieve_choice, ".R"))))
    }
  })

  samp_retrieve_val <- eventReactive(input$samp_retrieveclick, {
    input$samp_retrieve_choice
  })

  output$samp_retrieve_current <- renderText({
    paste0("Current:   Slot ", samp_retrieve_val())
  })

  output$samp_retrieve_table <- renderTable({
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Category" = "d", "Lifestage" = "e", "Dims" = 0, .rows = 0)
    if (! is.null(samp_loaded_kindata())) {
      temp <- samp_loaded_kindata()
      rtable <- rtable %>% add_row("Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims)
    }
  })

  samp_kindata <- reactive({

    if (input$samp_distribution_select == "samp_simple") {
      return(sample_kindist(sim_simple_kindata(), lower = res_lower(), upper = res_upper(), dims = res_dims(), spacing = res_spacing(), n = res_n()))
    }
    if (input$samp_distribution_select == "samp_composite") {
      return(sample_kindist(sim_composite_kindata(), lower = res_lower(), upper = res_upper(), dims = res_dims(), spacing = res_spacing(), n = res_n()))
    }
    if (input$samp_distribution_select == "samp_stored") {
      return(sample_kindist(samp_loaded_kindata(), lower = res_lower(), upper = res_upper(), dims = res_dims(), spacing = res_spacing(), n = res_n()))
    }
  })

  samp_store <- observeEvent(input$samp_storeclick, {
    saveval <- samp_kindata()
    saveRDS(saveval, file = here(paste0("temp/", input$samp_saveops, ".R")))
  })

  unbiased_kernel <- reactive({
    if (input$samp_distribution_select == "samp_simple") {
      return(axials(sim_simple_kindata()@tab$distance, 1))
    }
    if (input$samp_distribution_select == "samp_composite"){
      return(axials(sim_composite_kindata()@tab$distance, 1))
    }
    if (input$samp_distribution_select == "samp_stored"){
      return(axials(samp_loaded_kindata()@tab$distance, 1))
    }
  })

  samp_kindata_stats <- reactive({
    sampled_kernel <- axials(samp_kindata()@tab$distance, 1)
    sampled_number <- nrow(samp_kindata()@tab)
    return(tibble(`Original Kernel` = unbiased_kernel(), `Sampled Kernel` = sampled_kernel, `Number Sampled` = sampled_number))
  })

  samphistmax <- reactive({
    if (input$samp_distribution_select == "samp_simple") {
      return(max(sim_simple_kindata()@tab$distance))
    }
    if  (input$samp_distribution_select == "samp_composite"){
      return(max(sim_composite_kindata()@tab$distance))
    }
    if  (input$samp_distribution_select == "samp_stored"){
      return(max(samp_loaded_kindata()@tab$distance))
    }
  })

  output$samphist <- renderPlot({
    ggplot(samp_kindata()@tab) + aes(x = .data$distance) +
      geom_histogram(colour = "grey30", fill = "white", binwidth = input$samp_binwidth) +
      coord_cartesian(xlim = c(0, samphistmax()))
    #theme_bw()
  })

  output$sampstats <- renderTable({
    samp_kindata_stats()
  })

  ################ Estimate #################


  ##### a. single #####

  est_smp_loaded <- eventReactive(input$est_smp_retrieveclick, {
    if (input$est_smp_source == "stored") {
      if (! file.exists(here(paste0("temp/",input$est_smp_retrieve_choice, ".R")))) {return(NULL)}
      return(readRDS(here(paste0("temp/", input$est_smp_retrieve_choice, ".R"))))
    }
  })

  est_smp_ci <- reactive({

    if (input$est_smp_source == "simple") {
      dataset <- sim_simple_kindata()@tab$distance
    }
    else if (input$est_smp_source == "composite") {
      dataset <- sim_composite_kindata()@tab$distance
    }
    else if (input$est_smp_source == "sampled") {
      dataset <- samp_kindata()@tab$distance
    }
    else if (input$est_smp_source == "stored") {
      dataset <- est_smp_loaded()@tab$distance
    }
    else if (input$est_smp_source == "filedata"){
      dataset <- esti_data1()$distance
    }

    composite <- as.integer(input$est_smp_mode)

    if (input$est_smp_bootstraps > 10000){
      updateNumericInput(session, "est_smp_bootstraps", value = 10000)
      return(NULL)
    }
    if (input$est_smp_bootnum > 10000){
      updateNumericInput(session, "est_smp_bootnum", value = 10000)
      return(NULL)
    }

    #return(axials(dataset))
    return(axpermute(dataset, nreps = input$est_smp_bootstraps, num = input$est_smp_bootnum, output = "confs", composite = composite))

  })

  output$est_smp_ci_table <- renderTable({
    #est_smp_ci()
    tibble("Lower" = est_smp_ci()[1], "Median" = est_smp_ci()[2], "Upper" = est_smp_ci()[3])
    #tibble("a" = 1, "b" = 2, "c" = 3)
  })


  esti_data1 <- reactive({
    inFile <- input$esti_file1

    if (is.null(inFile)) {
      return(NULL)
    }

    read.csv(inFile$datapath)
  })

  esti_kindata_stats1 <- reactive({

    if (is.null(esti_data1())) {
      return(NULL)
    }

    kernel <- axials(esti_data1()$distance, 1)
    number <- nrow(esti_data1())
    return(tibble(`Raw Kernel` = kernel, `N` = number))
  })


  output$esti_stats1 <- renderTable({

    esti_kindata_stats1()
  })

  output$esti_contents <- renderTable({
    esti_data1()[1:10,]
  })

  ##### b. standard #####

  est_std_ci <- eventReactive(
    input$est_std_run,
    {
      avect <- readRDS(here(paste0("temp/", input$est_std_retrieve_choice_lrg, ".R")))@tab$distance
      acat <- input$est_std_rcl_category
      amix <- input$est_std_rclmix
      amixcat <- input$est_std_rcl_category2
      acomp <- input$est_std_rclcomp
      acompvect <- readRDS(here(paste0("temp/", input$est_std_retrieve_choice_lrg2, ".R")))@tab$distance
      acompcat <- input$est_std_rcl2_category

      bvect <- readRDS(here(paste0("temp/", input$est_std_retrieve_choice_sml, ".R")))@tab$distance
      bcat <- input$est_std_rcs_category
      bmix <- input$est_std_rcsmix
      bmixcat <- input$est_std_rcs_category2
      bcomp <- input$est_std_rcscomp
      bcompvect <- readRDS(here(paste0("temp/", input$est_std_retrieve_choice_sml2, ".R")))@tab$distance
      bcompcat <- input$est_std_rcs2_category

      nreps <- input$est_std_bootstraps
      if (input$est_std_samptype == "man"){
        nsamp <- input$est_std_bootnum
      }
      else {nsamp <- input$est_std_samptype}

      return(axpermute_standard(avect = avect, acat = acat, amix = amix, amixcat = amixcat, acomp = acomp, acompvect = acompvect, acompcat = acompcat,
                                bvect = bvect, bcat = bcat, bmix = bmix, bmixcat = bmixcat, bcomp = bcomp, bcompvect = bcompvect, bcompcat = bcompcat,
                                nreps = nreps, nsamp = nsamp, output = "confs"))
    }
  )

  output$est_std_ci_test <- renderText({
    #est_smp_ci()
    paste0("temp/", input$est_std_retrieve_choice_sml, ".R")
    #tibble("a" = 1, "b" = 2, "c" = 3)
  })

  output$est_std_ci_table <- renderTable({
    #est_smp_ci()
    tibble("Lower" = est_std_ci()[1], "Median" = est_std_ci()[2], "Upper" = est_std_ci()[3])
    #tibble("a" = 1, "b" = 2, "c" = 3)
  })


  est_std_sum <- eventReactive(
    input$top_data_update,
    {
      rtable <- tibble("Slot" = "a", "Type" = "a", "Kernel" = "b", "Category" = "d", "Lifestage" = "e", "Dims" = 0, "Count" = 0, .rows = 0)
      temp <- readRDS(here(paste0("temp/1.R")))
      rtable <- rtable %>% add_row("Slot" = "1", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Count" = nrow(temp@tab))
      temp <- readRDS(here(paste0("temp/2.R")))
      rtable <- rtable %>% add_row("Slot" = "2", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Count" = nrow(temp@tab))
      temp <- readRDS(here(paste0("temp/3.R")))
      rtable <- rtable %>% add_row("Slot" = "3", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Count" = nrow(temp@tab))
      temp <- readRDS(here(paste0("temp/4.R")))
      rtable <- rtable %>% add_row("Slot" = "4", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Count" = nrow(temp@tab))
      temp <- readRDS(here(paste0("temp/5.R")))
      rtable <- rtable %>% add_row("Slot" = "5", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
                                   "Lifestage" = temp@lifestage, "Dims" = temp@dims, "Count" = nrow(temp@tab))

      return(rtable)
    })

  output$est_std_sumtable <- renderTable({
    est_std_sum()
  })
  #temp <- readRDS(here(paste0("temp/6.R")))
  #rtable <- rtable %>% add_row("Number" = 6, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@dims)
  #temp <- readRDS(here(paste0("temp/7.R")))
  #rtable <- rtable %>% add_row("Number" = 7, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@dims,)
  #temp <- readRDS(here(paste0("temp/8.R")))
  #rtable <- rtable %>% add_row("Number" = 8, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@dims)
  #temp <- readRDS(here(paste0("temp/9.R")))
  #rtable <- rtable %>% add_row("Number" = 9, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@dims)
  #temp <- readRDS(here(paste0("temp/10.R")))
  #rtable <- rtable %>% add_row("Number" = 10, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Category" = temp@category,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@dims)
  #
  #  rtable
  #})

  ############# Error Functions #############

  output$errorbar <- renderText({
    error_report()
  })

  # Need to gather checks and put them together here!

  error_report <- reactive({
    "No Errors "
  })
}


#shinyApp(ui = ui, server = server)

#' Run kindisperse app
#'
#' @return
#' @export
#' @importFrom utils read.csv
#' @examples
run_kindisperse <- function(){
  shinyApp(ui = ui, server = server)
}
