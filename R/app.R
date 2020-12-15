
# The real deal app... here we come!!!!

# library(plotly)
# library(tidyverse)
# library(here)
# library(ggrepel)
# library(shiny)
## library(shinythemes)
# library(fitdistrplus)
# library(LaplacesDemon)

# preload some data here!

# source("kindisperse_functions.R")
# run early setup functions
# set environment for data storage
app_env <- env(
  d1 = KinPairSimulation(),
  d2 = KinPairSimulation(),
  d3 = KinPairSimulation(),
  d4 = KinPairSimulation(),
  d5 = KinPairSimulation(),
  d6 = KinPairSimulation(),
  d7 = KinPairSimulation(),
  d8 = KinPairSimulation(),
  d9 = KinPairSimulation(),
  d10 = KinPairSimulation(),
)


ui <- fluidPage(

  titlePanel("kindisperse"),
  theme = shinytheme("flatly"),
  h5(paste0("v", packageVersion("kindisperse"))),

  fluidRow(
    column(
      offset = 4, width = 4,

      textOutput(outputId = "appstatus")
    ),
    column(
      width = 4,
      h4("________________________Stored Data_____________________"),
      h4(
        style = "font-size:12px;",
        tableOutput(
          outputId = "est_std_sumtable"
        )
      ),
      actionButton(
        inputId = "top_data_update",
        label = "Update"
      )
    )
  ),


  navbarPage(
    "App Functions",
    selected = "Simulate",
    # theme = "bootstrap (2).css",

    ############# Tutorial Tab #############

    tabPanel(
      "Tutorial",
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
              sliderInput(
                inputId = "sand_dsigma",
                label = "Axial dispersal Sigma (m):",
                min = 5,
                max = 250,
                value = 25
              ),

              sliderInput(
                inputId = "sand_nsims",
                label = "Number of families to trace",
                min = 1,
                max = 100,
                value = 5
              ),

              sliderInput(
                inputId = "sand_dims",
                label = "Dimensions of parent area (m)",
                min = 50,
                max = 1000,
                value = 250
              ),

              selectInput(
                inputId = "sand_category",
                label = "Choose kinship category to examine",
                choices = c(
                  "PO", "FS", "HS", "AV", "GG", "HAV", "GGG",
                  "1C", "1C1", "2C", "GAV"
                ),
                selected = "1C"
              ),

              checkboxInput(
                inputId = "sand_labls",
                label = "Show Labels",
                value = FALSE
              ),

              checkboxInput(
                inputId = "sand_moves",
                label = "Show Movements",
                value = TRUE
              ),

              checkboxInput(
                inputId = "sand_shadows",
                label = "Show Shadows",
                value = FALSE
              ),

              checkboxInput(
                inputId = "sand_show_area",
                label = "Show Parent Area",
                value = TRUE
              ),

              checkboxInput(
                inputId = "sand_lengths",
                label = "Show Final Distances",
                value = TRUE
              ),

              checkboxInput(
                inputId = "sand_lengthlabs",
                label = "Show Distance Value",
                value = TRUE
              ),

              selectInput(
                inputId = "sand_graphtype",
                label = "Choose graph type",
                choices = c(
                  "Basic", "Pinwheel", "Scatter",
                  "Histogram", "FreqPoly", "Centred",
                  "Individual"
                ),
                selected = "Basic"
              ),

              sliderInput(
                inputId = "sand_binwidth",
                label = "Histogram binwidth (m)",
                min = 1,
                max = 50,
                value = 5
              ),

              checkboxInput(
                inputId = "sand_scaled",
                label = "Scale by sigma (individual plot)",
                value = TRUE
              ),

              sliderInput(
                inputId = "sand_scalefactor",
                label = "Scale factor (multiples of sigma)",
                min = 1,
                max = 10,
                value = 4
              ),

              sliderInput(
                inputId = "sand_pairs",
                label = "Number of pairs to trace (pin, hist & scatter)",
                min = 1,
                max = 10000,
                value = 20
              )
            ),

            mainPanel(

              # Output: histogram --
              plotOutput(outputId = "sand_dispersalPlot", height = 750),

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

    #### _####
    ########################### Load Tab ###################################

    tabPanel(
      "Load",
      h1("Load Dispersal & Kinship Data from Files"),

      sidebarLayout(
        sidebarPanel(
          h3("Load KinPairData"),

          selectInput(
            inputId = "load_source",
            label = "Choose data source",
            choices = c(
              "Stored" = "stored", "File" = "filedata", "Mounted from R" = "mounted",
              "Simulation (simple)" = "simple", "Simulation (composite)" = "composite",
              "Sampled" = "sampled"
            )
          ),

          conditionalPanel(
            condition = "input.load_source.includes('stored')",
            h4("Retrieving from app tempdata"),
            selectInput(
              inputId = "load_retrieve_choice_source",
              label = "Choose dataslot to load to staging area",
              choices = c(
                "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
              )
            )
          ),

          conditionalPanel(
            condition = "input.load_source.includes('filedata')",
            h4("Loading data from filesystem"),
            selectInput(
              inputId = "load_filetype",
              label = "Choose filetype to load",
              choices = c(".csv" = "csv", ".tsv" = "tsv", ".kindata" = "kindata")
            ),
            p("Load a .csv  or .tsv file with a 'kinship' column obeying standard conventions and a 'distance' column (numeric),
                     or load a .kindata file storing a KinPairData or KinPairSimulation object."),

            checkboxGroupInput(
              inputId = "load_usekin",
              label = "Manually select kinship or lifestage to extract from file?",
              choices = c("kinship", "lifestage")
            ),

            conditionalPanel(
              condition = "input.load_usekin.includes('kinship')",

              selectInput(
                inputId = "load_kinship_choice",
                label = "Choose kinship category to extract/assign",
                choices = c(
                  "UN", "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                  "HGAV", "H1C", "H1C1", "H2C"
                ),
                selected = "UN"
              )
            ),

            conditionalPanel(
              condition = "input.load_usekin.includes('lifestage')",

              selectInput(
                inputId = "load_lifestage_choice",
                label = "Choose lifestage category to extract/assign",
                choices = c("unknown", "larva", "oviposition"),
                selected = "unknown"
              )
            ),

            fileInput(
              inputId = "load_file1",
              label = "Choose file to load",
              accept = c(
                "text/csv", "text/comma-separated-values,text/plain", ".csv",
                "text/tsv", "text/tab-separated-values,text/plain", ".tsv",
                ".kindata"
              )
            )
          ),

          conditionalPanel(
            h6("Enter one of the items below:"),

            textOutput(
              outputId = "appenv_list"
            ),
            condition = "input.load_source.includes('mounted')",
            textInput(
              inputId = "load_retrieve_choice_mounted",
              label = "Enter object name to be loaded from appdata",
              value = "name"
            )
          ),

          actionButton(
            inputId = "load_retrieveclick",
            label = "Load",
            icon = icon("upload")
          ),

          hr(),
          hr(),

          h3("Save KinPairData"),

          selectInput(
            inputId = "save_source",
            label = "Choose output type",
            choices = c("Save to File" = "filedata", "Mount to R" = "mounted")
          ),

          conditionalPanel(
            condition = "input.save_source.includes('filedata')",
            h4("Saving data to filesystem"),
            selectInput(
              inputId = "save_filetype",
              label = "Choose filetype to save as",
              choices = c(".csv" = "csv", ".tsv" = "tsv", ".kindata" = "kindata")
            ),
            p("Save a .csv  or .tsv file with a 'kinship' column obeying standard conventions and a 'distance' column (numeric),
                     or save a .kindata file storing a KinPairData or KinPairSimulation object."),

            textInput(
              inputId = "save_filename",
              label = "Enter filename to save as",
              value = "kin_pairs"
            ),

            downloadButton(
              outputId = "save_button",
              label = "Download"
            )
          ),

          conditionalPanel(
            condition = "input.save_source.includes('mounted')",

            p("This interface saves the staged object to the appdata interface with the active R session
                     for coding access after the app is closed. Data mounted here can be accessed in your
                     R session with the functions retrieve_appdata() and retrieveall_appdata() - see help files."),

            textInput(
              inputId = "save_choice_mounted",
              label = "Enter name to save object as in appdata",
              value = "name"
            ),

            actionButton(
              inputId = "mount_button",
              label = "Mount",
              icon = icon("box-open")
            ),

            actionButton(
              inputId = "unmount_button",
              label = "Unmount",
              icon = icon("ban")
            )
          ),
        ),
        mainPanel(
          h3("Staged Data"),

          shiny::verbatimTextOutput("load_mount"),

          hr(),

          h3("Pass staged data to app memory"),

          selectInput(
            inputId = "load_saveops",
            label = "Choose storage slot",
            choices = c(
              "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
              "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
            )
          ),

          actionButton(
            inputId = "load_storeclick",
            label = "Store",
            icon = icon("archive")
          ),
          hr(),

          tableOutput("load_stats1"),

          tableOutput("load_contents")
        )
      )
    ),

    #### _####
    ######################### Simulate Tab #############################

    tabPanel(
      "Simulate",
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
              numericInput(
                inputId = "sim_simple_nsims",
                label = "Number to simulate (maximum 1 million)",
                min = 1, max = 1000000, value = 100000
              ),

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
                )
              ),

              sliderInput(
                inputId = "sim_simple_dims",
                label = "Site dimensions (n x n)",
                min = 25, max = 1000, value = 1000
              ),

              selectInput(
                inputId = "sim_simple_category",
                label = "Kinship Category",
                choices = c(
                  "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                  "HGAV", "H1C", "H1C1", "H2C"
                ),
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
                choices = c(
                  "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                  "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                )
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
              numericInput(
                inputId = "sim_composite_nsims",
                label = "Number to simulate (maximum 1 million)",
                min = 1, max = 1000000, value = 100000
              ),

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
                choices = c(
                  "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                  "HGAV", "H1C", "H1C1", "H2C"
                ),
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
                choices = c(
                  "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                  "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                )
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
              checkboxGroupInput(
                inputId = "testsaveops",
                label = "choose store slot to add to comparison",
                choices = c(
                  "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4",
                  "Slot 5" = "5", "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8",
                  "Slot 9" = "9", "Slot 10" = "10"
                )
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
                )
              )
            )
          )
        )
      )
    ),

    #### _####
    ##################### Sample Tab ##########################

    tabPanel(
      "Sample",
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
              choices = c(
                "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
              )
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

          conditionalPanel(
            condition = "input.samp_checkbox.includes('use_samp_lower')",
            numericInput(
              inputId = "samp_lower",
              label = "choose trap lower range",
              min = 1, max = 250, value = 0
            )
          ),

          conditionalPanel(
            condition = "input.samp_checkbox.includes('use_samp_upper')",
            numericInput(
              inputId = "samp_upper",
              label = "choose trap upper range",
              min = 0, max = 1000, value = 1000
            )
          ),

          conditionalPanel(
            condition = "input.samp_checkbox.includes('use_samp_dims')",
            numericInput(
              inputId = "samp_dims",
              label = "choose trap dimensions (n by n)",
              min = 25, max = 1000, value = 100
            )
          ),

          conditionalPanel(
            condition = "input.samp_checkbox.includes('use_samp_spacing')",
            numericInput(
              inputId = "samp_spacing",
              label = "choose trap spacing",
              min = 0.1, max = 250, value = 0.1
            )
          ),

          conditionalPanel(
            condition = "input.samp_checkbox.includes('use_samp_n')",
            numericInput(
              inputId = "samp_n",
              label = "choose maximum number sampled",
              min = 1, max = 100000, value = 10000
            )
          ),


          numericInput(
            inputId = "samp_binwidth",
            label = "Choose graph binwidth",
            min = 1, max = 100, value = 5
          ),

          selectInput(
            inputId = "samp_saveops",
            label = "Choose storage slot",
            choices = c(
              "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
              "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
            )
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

    #### _####
    ######################### Estimate Tab ##############################

    tabPanel(
      "Estimate",
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
                choices = c(
                  "Simulation (simple)" = "simple", "Simulation (composite)" = "composite",
                  "Sampled" = "sampled", "Stored" = "stored", "Data file" = "filedata"
                )
              ),

              conditionalPanel(
                condition = "input.est_smp_source.includes('stored')",
                selectInput(
                  inputId = "est_smp_retrieve_choice",
                  label = "Which data do you wish to retrieve?",
                  choices = c(
                    "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                    "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                  )
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
                choices = c(
                  "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                  "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                )
              ),

              selectInput(
                inputId = "est_std_rcl_category",
                label = "Kinship Category",
                choices = c(
                  "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                  "HGAV", "H1C", "H1C1", "H2C"
                ),
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
                  choices = c(
                    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                    "HGAV", "H1C", "H1C1", "H2C"
                  ),
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
                  choices = c(
                    "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                    "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                  )
                ),

                selectInput(
                  inputId = "est_std_rcl2_category",
                  label = "... kinship category",
                  choices = c(
                    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                    "HGAV", "H1C", "H1C1", "H2C"
                  ),
                  selected = "H1C"
                )
              ),

              selectInput(
                inputId = "est_std_retrieve_choice_sml",
                label = "Choose smaller axial distribution",
                choices = c(
                  "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                  "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                )
              ),

              selectInput(
                inputId = "est_std_rcs_category",
                label = "Kinship Category",
                choices = c(
                  "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                  "HGAV", "H1C", "H1C1", "H2C"
                ),
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
                  choices = c(
                    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                    "HGAV", "H1C", "H1C1", "H2C"
                  ),
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
                  choices = c(
                    "Slot 1" = "1", "Slot 2" = "2", "Slot 3" = "3", "Slot 4" = "4", "Slot 5" = "5",
                    "Slot 6" = "6", "Slot 7" = "7", "Slot 8" = "8", "Slot 9" = "9", "Slot 10" = "10"
                  )
                ),

                selectInput(
                  inputId = "est_std_rcs2_category",
                  label = "... kinship category",
                  choices = c(
                    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                    "HGAV", "H1C", "H1C1", "H2C"
                  ),
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

              # tableOutput(
              #   outputId = "est_std_sumtable"
              # )
            )
          )
        )

        ##### c. custom #####
        # tabPanel(
        #   "Custom"
        # )
      )
    )
  ),

  hr(),
  textOutput(
    outputId = "errorbar"
  ),
  hr()
)




#### _####
#### _####





server <- function(input, output, session) {
  # output$dhist <- renderPlot({
  #  hist(rnorm(input$sigma))
  #  })
  tutorial_data1 <- simgraph_data(nsims = 10000, dsigma = 25, kinship = "PO")


  # Simulate Tab


  ############# Setup ################

  output$appstatus <- renderText({
    "Everything is A-OK"
  })


  ############## Maintenance ###############


  #### _####
  ############# Tutorial ##################


  ### Tab 1 ###

  output$tutorial_1a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 5, kinship = "PO")
  })

  output$tutorial_1b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 20, kinship = "PO", pinwheel = TRUE)
  })

  ### Tab 2 ###

  output$tutorial_2a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, kinship = "PO", scattered = TRUE)
  })

  output$tutorial_2b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, kinship = "PO", histogram = TRUE, binwidth = 2.5)
  })

  ### Tab 3 ###

  output$tutorial_3a <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 1, kinship = "GG", show_area = F, labls = F)
  })

  output$tutorial_3b <- renderPlot({
    simgraph_graph(tutorial_data1, nsims = 10000, kinship = "GG", histogram = TRUE, binwidth = 2.5)
  })

  ### Tab 4 ###


  ### Tab 5 ###

  sandbox_data <- reactive({
    simgraph_data(nsims = input$sand_nsims, dsigma = input$sand_dsigma, dims = input$sand_dims, kinship = input$sand_category)
  })

  output$sand_dispersalcheck <- renderText({
    dim(sandbox_data())
  })

  output$sand_dispersalPlot <- renderPlot({
    simgraph_graph(sandbox_data(),
      nsims = input$sand_nsims, dsigma = input$sand_dsigma, dims = input$sand_dims, labls = input$sand_labls,
      moves = input$sand_moves, shadows = input$sand_shadows, kinship = input$sand_category,
      show_area = input$sand_show_area, # centred = input$sand_centred, #pinwheel = input$sand_pinwheel, scattered = input$sand_scattered,
      lengths = input$sand_lengths, lengthlabs = input$sand_lengthlabs
    ) # , histogram = input$sand_histogram, binwidth = input$sand_binwidth)#,
    # freqpoly = input$sand_freqpoly)
  })

  #### _####
  ################## Load #######################

  mounted_select_refresh <- eventReactive(input$load_mounted_refresh, {
    updateSelectInput(
      session,
      inputId = "load_retrieve_choice_mounted",
      choices = env_names(env_appdata)
    )
  })

  output$appenv_list <- renderText({
    env_names(env_appdata)
  })

  staged_object <- eventReactive(input$load_retrieveclick, {
    if (input$load_source == "stored") {
      return(app_env[[paste0("d", input$load_retrieve_choice_source)]])
    }
    else if (input$load_source == "filedata") {
      if (input$load_filetype == "csv") {
        return(csv_to_kinpair(input$load_file1$datapath,
          kinship = ifelse("kinship" %in% input$load_usekin, input$load_kinship_choice, NULL),
          lifestage = ifelse("lifestage" %in% input$load_usekin, input$load_lifestage_choice, NULL)
        ))
      }
      else if (input$load_filetype == "tsv") {
        return(tsv_to_kinpair(input$load_file1$datapath,
          kinship = ifelse("kinship" %in% input$load_usekin, input$load_kinship_choice, NULL),
          lifestage = ifelse("lifestage" %in% input$load_usekin, input$load_lifestage_choice, NULL)
        ))
      }
      else if (input$load_filetype == "kindata") {
        return(read_kindata(input$load_file1$datapath))
      }
    }
    else if (input$load_source == "mounted") {
      if (input$load_retrieve_choice_mounted != "") {
        return(retrieve_appdata(input$load_retrieve_choice_mounted))
      }
      else {
        return(KinPairSimulation())
      }
    }
    else if (input$load_source == "simple") {
      return(sim_simple_kindata())
    }
    else if (input$load_source == "composite") {
      return(sim_composite_kindata())
    }
    else if (input$load_source == "sampled") {
      return(samp_kindata())
    }
  })

  output$load_mount <- renderText({
    if (is.KinPairSimulation(staged_object())) {
      out <- "KINDISPERSE SIMULATION of KIN PAIRS\n"
      out <- str_c(out, "-----------------------------------\n")
      out <- str_c(out, "simtype:\t\t", ifelse(is.na(staged_object()@simtype), "NA", staged_object()@simtype), "\n")
      out <- str_c(out, "kerneltype:\t\t", ifelse(is.na(staged_object()@kerneltype), "NA", staged_object()@kerneltype), "\n")
      out <- str_c(out, "kinship:\t\t", staged_object()@kinship, "\n")
      out <- str_c(out, "simdims:\t\t", ifelse(is.na(staged_object()@simdims), "NA", staged_object()@simdims), "\n")
      if (is.na(staged_object()@simtype)) {
        out <- str_c(out, "")
      }
      else if (staged_object()@simtype == "simple") {
        out <- str_c(out, "dsigma:\t\t\t", ifelse(is.na(staged_object()@dsigma), "NA", staged_object()@dsigma), "\n")
      }
      else if (staged_object()@simtype == "composite") {
        out <- str_c(
          out, "juvsigma\t\t", ifelse(is.na(staged_object()@juvsigma), "NA", staged_object()@juvsigma),
          "\nbreedsigma\t\t", ifelse(is.na(staged_object()@breedsigma), "NA", staged_object()@breedsigma),
          "\ngravsigma\t\t", ifelse(is.na(staged_object()@gravsigma), "NA", staged_object()@gravsigma),
          "\novisigma\t\t", ifelse(is.na(staged_object()@ovisigma), "NA", staged_object()@ovisigma), "\n"
        )
      }
      out <- str_c(out, "lifestage:\t\t", staged_object()@lifestage, "\n")
      out <- str_c(out, "rows:\t\t\t", nrow(staged_object()@tab), "\n\n")
      if (!is.na(staged_object()@filtertype)) {
        if (staged_object()@filtertype == "filtered") {
          out <- str_c(out, "FILTERED\n")
          out <- str_c(out, "--------\n")
          if (!is.na(staged_object()@upper)) {
            out <- str_c(out, "upper:\t\t\t", staged_object()@upper, "\n")
          }
          if (!is.na(staged_object()@lower)) {
            out <- str_c(out, "lower:\t\t\t", staged_object()@lower, "\n")
          }
          if (!is.na(staged_object()@spacing)) {
            out <- str_c(out, "spacing:\t\t", staged_object()@spacing, "\n")
          }
          if (!is.na(staged_object()@samplenum)) {
            out <- str_c(out, "samplenum:\t\t", staged_object()@samplenum, "\n")
          }
          if (!is.na(staged_object()@sampledims)) {
            out <- str_c(out, "sampledims:\t\t", staged_object()@sampledims, "\n")
          }
          out <- str_c(out, "\n")
        }
      }
      out <- str_c(out, "tab\n")
      out <- str_c(out, paste(colnames(staged_object()@tab), collapse = "\t"), "\n")
      temp <- staged_object()@tab[1:5, ]
      for (nm in 1:ncol(temp)) {
        if (is.numeric(temp[[nm]])) temp[nm] <- round(temp[nm], digits = 1)
      }
      out <- str_c(out, paste(temp[1, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[2, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[3, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[4, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[5, ], collapse = "\t"), "\n")
      out <- str_c(out, "-----------------------------------")
      out
    }
    else if (is.KinPairData(staged_object())) {
      out <- ("KINDISPERSE RECORD OF KIN PAIRS\n")
      out <- str_c(out, "-------------------------------\n")
      out <- str_c(out, "kinship:\t\t", staged_object()@kinship, "\n")
      out <- str_c(out, "lifestage:\t\t", staged_object()@lifestage, "\n")
      out <- str_c(out, "rows:\t\t\t", nrow(staged_object()@tab), "\n\n")
      out <- str_c(out, "tab\n")
      out <- str_c(out, paste(colnames(staged_object()@tab), collapse = "\t"), "\n")
      temp <- staged_object()@tab[1:5, ]
      for (nm in 1:ncol(temp)) {
        if (is.numeric(temp[[nm]])) temp[nm] <- round(temp[nm], digits = 1)
      }
      out <- str_c(out, paste(temp[1, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[2, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[3, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[4, ], collapse = "\t"), "\n")
      out <- str_c(out, paste(temp[5, ], collapse = "\t"), "\n")
      out <- str_c(out, "-------------------------------")
    }
  })

  load_store <- observeEvent(input$load_storeclick, {
    saveval <- staged_object()
    if (is.KinPairData(saveval)) {
      env_poke(app_env, paste0("d", input$load_saveops), saveval)
    }
  })

  output$save_button <- downloadHandler(
    filename = function() {
      if (input$save_filetype == "csv") {
        paste0(input$save_filename, ".csv")
      } else if (input$save_filetype == "tsv") {
        paste0(input$save_filename, ".tsv")
      } else if (input$save_filetype == "kindata") paste0(input$save_filename, ".kindata")
    },
    content = function(file) {
      if (input$save_filetype == "csv") {
        kinpair_to_csv(staged_object(), file)
      } else if (input$save_filetype == "tsv") {
        kinpair_to_tsv(staged_object(), file)
      } else if (input$save_filetype == "kindata") write_kindata(staged_object(), file)
    }
  )

  data_mount <- observeEvent(input$mount_button, {
    mount_appdata(staged_object(), input$save_choice_mounted)
  })

  data_unmount <- observeEvent(input$unmount_button, {
    unmount_appdata(input$save_choice_mounted)
  })

  #### _####
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

    simulate_kindist_simple(
      nsims = input$sim_simple_nsims, sigma = input$sim_simple_sigma, method = input$sim_simple_method,
      kinship = input$sim_simple_category, lifestage = input$sim_simple_lifestage, dims = input$sim_simple_dims
    )
  })

  sim_simple_store <- observeEvent(input$sim_simple_storeclick, {
    saveval <- sim_simple_kindata()
    env_poke(app_env, paste0("d", input$sim_simple_saveops), saveval)
  })

  output$sim_simple_hist <- renderPlot({
    if (is.null(sim_simple_kindata())) {
      return(NULL)
    }
    ggplot(sim_simple_kindata()@tab) +
      aes(x = .data$distance) +
      geom_histogram(binwidth = input$sim_simple_binwidth, fill = "white", colour = "grey30") +
      theme_bw()
  })


  #### Composite ####

  sim_composite_kindata <- reactive({
    if (input$sim_composite_nsims > 1000000) {
      updateNumericInput(session, "sim_composite_nsims", value = 1000000)
      return(NULL)
    }
    simulate_kindist_composite(
      nsims = input$sim_composite_nsims, juvsigma = input$sim_composite_juvsigma, breedsigma = input$sim_composite_breedsigma,
      gravsigma = input$sim_composite_gravsigma, ovisigma = input$sim_composite_ovisigma, dims = input$sim_composite_dims,
      method = input$sim_composite_method, kinship = input$sim_composite_category, lifestage = input$sim_composite_lifestage
    )
  })

  sim_composite_store <- observeEvent(input$sim_composite_storeclick, {
    saveval <- sim_composite_kindata()
    env_poke(app_env, paste0("d", input$sim_composite_saveops), saveval)
  })

  output$sim_composite_hist <- renderPlot({
    if (is.null(sim_composite_kindata())) {
      return(NULL)
    }
    ggplot(sim_composite_kindata()@tab) +
      aes(x = .data$distance) +
      geom_histogram(binwidth = input$sim_composite_binwidth, fill = "white", colour = "grey30") +
      theme_bw()
  })


  #### Compare ####

  # store test...

  # teststorage <- reactiveValues('1' = NULL, '2'=NULL, '3'=NULL, '4'=NULL, '5'=NULL, '6'=NULL, '7'=NULL, '8'=NULL, '9'=NULL, '10'=NULL)

  testevent <- observeEvent(input$storeclick, {
    NULL
  })

  delevent <- observeEvent(input$clearclick, {
    NULL
  })


  retrieved <- eventReactive(input$retrieveclick, {
    NULL
  })

  output$sim_compare_plot <- renderPlotly({
    gp <- ggplot(sim_simple_kindata()@tab) +
      aes(x = .data$distance)
    if ("1" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d1@tab, colour = "pink", binwidth = 5)
    }
    if ("2" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d2@tab, colour = "red", binwidth = 5)
    }
    if ("3" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d3@tab, colour = "orange", binwidth = 5)
    }
    if ("4" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d4@tab, colour = "green", binwidth = 5)
    }
    if ("5" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d5@tab, colour = "purple", binwidth = 5)
    }
    if ("6" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d6@tab, colour = "black", binwidth = 5)
    }
    if ("7" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d7@tab, colour = "grey", binwidth = 5)
    }
    if ("8" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d8@tab, colour = "pink", binwidth = 5)
    }
    if ("9" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d9@tab, colour = "brown", binwidth = 5)
    }
    if ("10" %in% input$testsaveops) {
      gp <- gp + geom_freqpoly(data = app_env$d10@tab, colour = "yellow", binwidth = 5)
    }
    gp
    ggplotly()
  })

  output$sim_compare_table <- renderTable({
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = 0, "Colour" = "f", .rows = 0)
    if ("1" %in% input$testsaveops) {
      temp <- app_env$d1
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "blue"
      )
    }
    if ("2" %in% input$testsaveops) {
      temp <- app_env$d2
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "red"
      )
    }
    if ("3" %in% input$testsaveops) {
      temp <- app_env$d3
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "orange"
      )
    }
    if ("4" %in% input$testsaveops) {
      temp <- app_env$d4
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "green"
      )
    }
    if ("5" %in% input$testsaveops) {
      temp <- app_env$d5
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "purple"
      )
    }
    if ("6" %in% input$testsaveops) {
      temp <- app_env$d6
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "black"
      )
    }
    if ("7" %in% input$testsaveops) {
      temp <- app_env$d7
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "grey"
      )
    }
    if ("8" %in% input$testsaveops) {
      temp <- app_env$d8
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "pink"
      )
    }
    if ("9" %in% input$testsaveops) {
      temp <- app_env$d9
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "brown"
      )
    }
    if ("10" %in% input$testsaveops) {
      temp <- app_env$d10
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Colour" = "yellow"
      )
    }
    rtable
  })

  output$testshow <- renderText({
    # is.character(input$testsaveops)
    retrieved()
    # teststorage$a
    # input$storeclick
  })

  #### FiRa Compare ####

  ##### _ #####
  ############### Sample ################

  res_lower <- reactive({
    if ("use_samp_lower" %in% input$samp_checkbox) {
      temp <- input$samp_lower
    }
    else {
      temp <- NULL
    }
    return(temp)
  })

  res_upper <- reactive({
    if ("use_samp_upper" %in% input$samp_checkbox) {
      temp <- input$samp_upper
    }
    else {
      temp <- NULL
    }
    return(temp)
  })

  res_dims <- reactive({
    if ("use_samp_dims" %in% input$samp_checkbox) {
      temp <- input$samp_dims
    }
    else {
      temp <- NULL
    }
    return(temp)
  })

  res_spacing <- reactive({
    if ("use_samp_spacing" %in% input$samp_checkbox) {
      temp <- input$samp_spacing
    }
    else {
      temp <- NULL
    }
    return(temp)
  })

  res_n <- reactive({
    if ("use_samp_n" %in% input$samp_checkbox) {
      temp <- input$samp_n
    }
    else {
      temp <- NULL
    }
    return(temp)
  })

  samp_loaded_kindata <- eventReactive(input$samp_retrieveclick, {
    if (input$samp_distribution_select == "samp_stored") {
      return(app_env[[paste0("d", input$samp_retrieve_choice)]])
    }
  })

  samp_retrieve_val <- eventReactive(input$samp_retrieveclick, {
    input$samp_retrieve_choice
  })

  output$samp_retrieve_current <- renderText({
    paste0("Current:   Slot ", samp_retrieve_val())
  })

  output$samp_retrieve_table <- renderTable({
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = 0, "Count" = 0L, .rows = 0)
    if (!is.null(samp_loaded_kindata())) {
      temp <- samp_loaded_kindata()
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }
    }
    rtable
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
    env_poke(app_env, paste0("d", input$samp_saveops), saveval)
  })

  unbiased_kernel <- reactive({
    if (input$samp_distribution_select == "samp_simple") {
      return(axials(sim_simple_kindata()@tab$distance, 1))
    }
    if (input$samp_distribution_select == "samp_composite") {
      return(axials(sim_composite_kindata()@tab$distance, 1))
    }
    if (input$samp_distribution_select == "samp_stored") {
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
    if (input$samp_distribution_select == "samp_composite") {
      return(max(sim_composite_kindata()@tab$distance))
    }
    if (input$samp_distribution_select == "samp_stored") {
      return(max(samp_loaded_kindata()@tab$distance))
    }
  })

  output$samphist <- renderPlot({
    ggplot(samp_kindata()@tab) +
      aes(x = .data$distance) +
      geom_histogram(colour = "grey30", fill = "white", binwidth = input$samp_binwidth) +
      coord_cartesian(xlim = c(0, samphistmax()))
    # theme_bw()
  })

  output$sampstats <- renderTable({
    samp_kindata_stats()
  })

  ################ Estimate #################


  ##### a. single #####

  est_smp_loaded <- eventReactive(input$est_smp_retrieveclick, {
    if (input$est_smp_source == "stored") {
      return(app_env[[paste0("d", input$est_smp_retrieve_choice)]])
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
    else if (input$est_smp_source == "filedata") {
      NULL
      # dataset <- esti_data1()$distance
    }

    composite <- as.integer(input$est_smp_mode)

    if (input$est_smp_bootstraps > 10000) {
      updateNumericInput(session, "est_smp_bootstraps", value = 10000)
      return(NULL)
    }
    if (input$est_smp_bootnum > 10000) {
      updateNumericInput(session, "est_smp_bootnum", value = 10000)
      return(NULL)
    }

    # return(axials(dataset))
    return(axpermute(dataset, nreps = input$est_smp_bootstraps, nsamp = input$est_smp_bootnum, output = "confs", composite = composite))
  })

  output$est_smp_ci_table <- renderTable({
    # est_smp_ci()
    tibble("Lower" = est_smp_ci()[1], "Median" = est_smp_ci()[2], "Upper" = est_smp_ci()[3])
    # tibble("a" = 1, "b" = 2, "c" = 3)
  })


  ##### b. standard #####

  est_std_ci <- eventReactive(
    input$est_std_run,
    {
      avect <- app_env[[paste0("d", input$est_std_retrieve_choice_lrg)]]@tab$distance
      acat <- input$est_std_rcl_category
      amix <- input$est_std_rclmix
      amixcat <- input$est_std_rcl_category2
      acomp <- input$est_std_rclcomp
      acompvect <- app_env[[paste0("d", input$est_std_retrieve_choice_lrg2)]]@tab$distance
      acompcat <- input$est_std_rcl2_category

      bvect <- app_env[[paste0("d", input$est_std_retrieve_choice_sml)]]@tab$distance
      bcat <- input$est_std_rcs_category
      bmix <- input$est_std_rcsmix
      bmixcat <- input$est_std_rcs_category2
      bcomp <- input$est_std_rcscomp
      bcompvect <- app_env[[paste0("d", input$est_std_retrieve_choice_sml2)]]@tab$distance
      bcompcat <- input$est_std_rcs2_category

      nreps <- input$est_std_bootstraps
      if (input$est_std_samptype == "man") {
        nsamp <- input$est_std_bootnum
      }
      else {
        nsamp <- input$est_std_samptype
      }

      return(axpermute_standard(
        avect = avect, acat = acat, amix = amix, amixcat = amixcat, acomp = acomp, acompvect = acompvect, acompcat = acompcat,
        bvect = bvect, bcat = bcat, bmix = bmix, bmixcat = bmixcat, bcomp = bcomp, bcompvect = bcompvect, bcompcat = bcompcat,
        nreps = nreps, nsamp = nsamp, output = "confs"
      ))
    }
  )

  output$est_std_ci_test <- renderText({
    # est_smp_ci()
    paste0("temp/", input$est_std_retrieve_choice_sml, ".R")
    # tibble("a" = 1, "b" = 2, "c" = 3)
  })

  output$est_std_ci_table <- renderTable({
    # est_smp_ci()
    tibble("Lower" = est_std_ci()[1], "Median" = est_std_ci()[2], "Upper" = est_std_ci()[3])
    # tibble("a" = 1, "b" = 2, "c" = 3)
  })


  est_std_sum <- eventReactive(
    input$top_data_update,
    {
      rtable <- tibble("Slot" = "a", "Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = 0, "Count" = 0L, .rows = 0)
      temp <- app_env$d1
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Slot" = "1", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Slot" = "1", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }
      temp <- app_env$d2
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Slot" = "2", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Slot" = "2", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }
      temp <- app_env$d3
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Slot" = "3", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Slot" = "3", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }
      temp <- app_env$d4
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Slot" = "4", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Slot" = "4", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }
      temp <- app_env$d5
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Slot" = "5", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = temp@simdims, "Count" = nrow(temp@tab)
        )
      }
      else {
        rtable <- rtable %>% add_row(
          "Slot" = "5", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
        )
      }

      return(rtable)
    }
  )

  output$est_std_sumtable <- renderTable({
    est_std_sum()
  })
  # temp <- readRDS(here(paste0("temp/6.R")))
  # rtable <- rtable %>% add_row("Number" = 6, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@simdims)
  # temp <- readRDS(here(paste0("temp/7.R")))
  # rtable <- rtable %>% add_row("Number" = 7, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@simdims,)
  # temp <- readRDS(here(paste0("temp/8.R")))
  # rtable <- rtable %>% add_row("Number" = 8, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@simdims)
  # temp <- readRDS(here(paste0("temp/9.R")))
  # rtable <- rtable %>% add_row("Number" = 9, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@simdims)
  # temp <- readRDS(here(paste0("temp/10.R")))
  # rtable <- rtable %>% add_row("Number" = 10, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = temp@simdims)
  #
  #  rtable
  # })

  ############# Error Functions #############

  output$errorbar <- renderText({
    error_report()
  })

  # Need to gather checks and put them together here!

  error_report <- reactive({
    "No Errors "
  })
}


# shinyApp(ui = ui, server = server)

#' Run kindisperse app
#'
#'
#' @export
#'
run_kindisperse <- function() {
  shinyApp(ui = ui, server = server)
}
