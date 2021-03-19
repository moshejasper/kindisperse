
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

  theme = shinytheme("flatly"),

  fluidRow(
    column(
      width = 4,
      titlePanel("kindisperse"),
      h5(paste0("v", utils::packageVersion("kindisperse")))
    ),
    column(
      width = 4

      #textOutput(outputId = "appstatus")
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
      column(
        width = 4,
        actionButton(
          inputId = "top_data_update",
          label = "Update"
        )

      ),
      column(
        width = 4,
        radioButtons(
          inputId = "top_data_page",
          label = "Page",
          choices = c(1, 2),
          selected = 1,
          inline = TRUE
        )
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
      h1("Introduction to kindisperse"),

      navbarPage(
        "The Intergenerational Dispersal Kernel",

        ########## Introduction ############

        tabPanel(
          "Introduction to dispersal",

          h3("Modelling intergenerational dispersal"),
          p("Dispersal is an important process in biology generally, especially in conservation and pest management.
          Intergenerational dispersal is a key process that connects biological events across the lifespan of an organism with broader demographic and population genetic
        processes of interest (such as isolation by distance, and ultimately selective processes and speciation).
        At its broadest, it refers to the change in the geographical positions of descendent generations when compared to ancestral generations.
          The most basic measurement of intergenerational dispersal is that across exactly one lifespan, such as
            the geographical location of the birth of the mother compared to that of the birth of her offspring. This parent-offspring distance (PO)
            is fundamental to Wright's theory of isolation by distance (Wright, 1946), and can be understood statistically via a dispersal kernel. Below,
            we have an example field site where a series of PO kin pairs have been detected (arrows lead from parent to offspring across a lifespan."),
          plotOutput(
            outputId = "tutorial_1a",
            height = 480
          ),

          p("To begin to understand the picture such a collection tells about dispersal, imagine that every parent started at the exact same site, then dispersed as usual
            across their lifespan before laying eggs leading to the next generation. All of these dispersal directions and distances can be summarised by a pinwheel diagram (Fig 2)."),
          plotOutput(
            outputId = "tutorial_1b",
            height = 480
          ),
          p("As we continue to pile up more and more PO dispersal pairs, the apparently random shape resolves into a 2D scatter distribution - the disersal kernel
            (in this case, a 2D Gaussian distribution, Fig 3) Such a kernel describes the probability distribution of parents to offspring across two dimensions,
            and can be modelled with a variety of distributions (Gaussian, Laplace, etc.). In all cases, the parameter sigma models the magnitude of the variance in
            each of the two dimensions (length and width) that dispersal occurs over"),
          plotOutput(
            outputId = "tutorial_2a"
          ),
          p("We can summarise the absolute lengths of all of these points with a histogram (Fig 4)."),
          plotOutput(
            outputId = "tutorial_2b"
          ),
          p(),
          p("However, we don't have to stop with one generation. Let's extend this simulation to the next (GG) generation"),
          plotOutput(
            outputId = "tutorial_3a"
          ),
          p("The histogram of the GG distribution is given below. It represents the outcomes of two PO dispersal events, the second dependent on the outcome
            of the first. This is modelled by two draws from a dispersal kernel with an axial (1D) standard deviation of sigma - in this case, 25m. Note
            that the distribution is more dispersed than that of the PO (above) with a tail approaching out to 150 meters. This 'addition' of two dispersal events
            too each other is easy to model statistically. We can also derive a single dispersal event by 'decomposing' the double dispersal event associated with the
            GG category. This points the way to a whole range of ways of estimating intergenerational dispersal by carefully considering the properties of different classes.
            (so, we could extend this to the GGG (great grandparent) class, partly by dividing the variance in that case by three."),
          plotOutput(
            outputId = "tutorial_3b"
          ),

          h3("Extending the kernel to differing kinship classes"),
          p("Intergenerational dispersal kernels are constructed by two extensions: (1) the dispersal of one individual is extended to dispersal across multiple
            individuals within a pedigree via variance addition (described abov). (2) the dispersal of one individual is split into multiple phases across its
            lifespan by variance subtraction"),
          p("If we find a pair of full sibling mosquitoes (FS), distances between these will reflect the movement of their mother during oviposition.
            For a pair of half siblings (FS), this represents the movement of their father between matings (breeding) and the movement of the mother searching
            for bloodmeals (gravid phase) as well as oviposition. For a pair of full first cousins (1C), this represents an entire lifespan of PO dispersal as well
            as an additional movement of their mother during oviposition, so that the 1C distribution is found by the variance addition of the FS and PO distributions.
            Kin categories can be grouped by phase based on these dispersal subcategories, with any kin pair having either PO, FS or HS phases."),
          p("PO life phases include parent offspring (PO), grandparent-grandchild (GG) and great-grandparent-great-grandchild (GGG)."),
          p("FS life phases include full siblings (FS), avuncular (AV), first cousins (1C), great-avuncular (GAV), first cousins once removed (1C1), and second cousins (2C)"),
          p("HS life phases include half siblings (HS), half avuncular (HAV), half first cousins (H1C), half great-avuncular (HGAV), half first cousin once removed (H1C1) and half second cousin (H2C)"),
          p("Any of these kin phases can be sampled at different parts of the lifespan: as immatures (eggs, larva, pupa) or as ovipositing adults (egg-laying females) - or other points. Such
            sampling times affect how much dispersal has already occured in a lifespan. Importantly, two categories that are sampled in the same phase have the same relationship as they would have
            if they were both sampled in a different life phase."),
          p("So, the final distributions of dispersed kin are dependent on three factors: (1) the kinship phase (FS, HS, PO); (2) the number of pedigree generations; (3)
             the life stage at which the kin were sampled. But where phase and life stage align, the distributions of two kin categories (e.g. 1C and FS) can be used to find
            the distribution of the PO lifespan that separates them")

        ),

        ########## Using the app #############

        tabPanel(
          "Using the app",

          p("This app has four interwoven functions which combine to enable all package features to be used."),
          h4("Load"),
          p("The 'load' tab enables the import of data from the R coding environment (via the 'mounted from R' option), from the filesystem, or from either the 'simulate' or 'sample' tabs, or
            from the app tempdata (memory). Once loaded in this tab, kinship dispersal objects can then be passed to the tempdata for use elsewhere in the app, or
            saved to the filesystem or exported to the R environment. It thus functions as the go-between that connects all aspects of the app and computer. Currenly staged data is summarised
            on the right."),
          h4("Simulate"),
          p("The 'simulate' tab enables both simple PO and more complex FS or HS-class simulations to be conducted and summarised under a wide variety of parameters, before being compared or
            passed elsewhere within the app (via direct access or being passed to the tempdata). Summaries of distributions are shown on the right. This tab lays the foundation for any sampling or
            estimation simulations elsewhere in the app."),
          h4("Sample"),
          p("The 'sample' tab takes data from the 'simulate' tab or tempdata (including from the system via 'Load') and applies a series of filters to the data to investigate the impact of
            study design on detected distributions of kinpairs, and thus on estimates of intergenerational dispersal. A summary of the distribution used and raw kernel estimate results is shown
            on the right. Once filtered, these distributions can be passed back to tempdata and compared via 'simulate/compare distributions' or used as a basis of kernel estimates."),
          h4("Estimate"),
          p("The 'estimate' tab takes data from any of the other sources and runs the Jasper et al. estimation methods on them. There is a choice of 'simple kernel' which estimates an individual kernel,
            or the 'standard' tab which takes several kernels (e.g. 1C and FS) to create a direct estimate of the PO intergenerational kernel. This estimator has the ability to accept mixtures between
            kernels and composite kernel categories (e.g. a mix of 1C and H1C, compensated for by compositing FS and HS distributions). The estimators use bootstraps to construct 95% confidence intervals
            (shown on the right)"),
          p("Stored data is summarised on the top right of the app."),
          p(),
          p("For further information, consult the kindisperse manual and help files")
        ),


        ########### Sandbox ###########

        tabPanel(
          "Sandbox",

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
            condition = "input.load_source.includes('mounted')",

            h6("Enter one of the items below:"),

            textOutput(
              outputId = "appenv_list"
            ),
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

              numericInput(
                inputId = "sim_simple_sigma",
                label = "Simple dispersal sigma",
                min = 1, max = 250, value = 50
              ),

              h5("Site dimensions"),

              column(width = 4,

              numericInput(
                inputId = "sim_simple_dimx",
                label = "(n x n)",
                min = 25, max = 1000, value = 1000
              )),

              column(width = 4,
                     hr(),
                     checkboxInput(
                       inputId = "sim_simple_bothdims",
                       label = h5("link dimensions"),
                       value = TRUE
                     )
              ),

              column(width = 4,
                     numericInput(
                       inputId = "sim_simple_dimy",
                       label = "",
                       min = 25, max = 1000, value = 1000
                     )
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
                choices = c("Gaussian", "Laplace", "Gamma"),
                selected = "Gaussian"
              ),

              conditionalPanel(
                condition = "input.sim_simple_method == 'Gamma'",

                numericInput(
                  inputId = "sim_simple_gshape",
                  label = "Select value of shape parameter",
                  min = 0, max = 2, value = 1, step = 0.01
                )
              ),

              numericInput(
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
                inputId = "sim_composite_initsigma",
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

              h5("Site dimensions"),

              column(width = 4,

                     numericInput(
                       inputId = "sim_composite_dimx",
                       label = "(n x n)",
                       min = 25, max = 1000, value = 1000
                     )),

              column(width = 4,
                     hr(),
                     checkboxInput(
                       inputId = "sim_composite_bothdims",
                       label = h5("link dimensions"),
                       value = TRUE
                     )
              ),

              column(width = 4,
                     numericInput(
                       inputId = "sim_composite_dimy",
                       label = "",
                       min = 25, max = 1000, value = 1000
                     )
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
                choices = c("Gaussian", "Laplace", "Gamma"),
                selected = "Gaussian"
              ),

              conditionalPanel(
                condition = "input.sim_composite_method == 'Gamma'",
                numericInput(
                  inputId = "sim_composite_shape",
                  label = "Shape of component kernels",
                  min = 0, max = 100, value = 1
                )
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
            h5("choose trap dimensions"),

            column(width = 5,

                   numericInput(
                     inputId = "samp_dimx",
                     label = "(n x n)",
                     min = 25, max = 1000, value = 1000
                   )),

            column(width = 2,
                   hr(),
                   checkboxInput(
                     inputId = "samp_bothdims",
                     label = h5("link"),
                     value = TRUE
                   )
            ),

            column(width = 5,
                   numericInput(
                     inputId = "samp_dimy",
                     label = "",
                     min = 25, max = 1000, value = 1000
                   )
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
              h3("95% C.I."),
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
              h3("95% C.I."),
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

  hr()
  #textOutput(
  #  outputId = "errorbar"
  #),
  #hr()
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

  #output$appstatus <- renderText({
  #  "Everything is A-OK"
  #})


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
    simgraph_graph(
      sandbox_data(), nsims = input$sand_nsims, labls = input$sand_labls,
      moves = input$sand_moves, shadows = input$sand_shadows, kinship = input$sand_category,
      show_area = input$sand_show_area, # centred = input$sand_centred, #pinwheel = input$sand_pinwheel, scattered = input$sand_scattered,
      lengths = input$sand_lengths, lengthlabs = input$sand_lengthlabs
    ) # , histogram = input$sand_histogram, binwidth = input$sand_binwidth)#,
    # freqpoly = input$sand_freqpoly)
  })

  #### _####
  ################## Load #######################


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
      if (! is.na(staged_object()@kernelshape)) out <- str_c(out, "kernelshape:\t\t", staged_object()@kernelshape, "\n")
      out <- str_c(out, "kinship:\t\t", staged_object()@kinship, "\n")
      out <- str_c(out, "simdims:\t\t", ifelse(is.na(staged_object()@simdims[1]), "NA", paste(signif(staged_object()@simdims, 3), collapse = "x")), "\n")
      if (is.na(staged_object()@simtype)) {
        out <- str_c(out, "")
      }
      else if (staged_object()@simtype == "simple") {
        out <- str_c(out, "dsigma:\t\t\t", ifelse(is.na(staged_object()@dsigma), "NA", staged_object()@dsigma), "\n")
      }
      else if (staged_object()@simtype == "composite") {
        out <- str_c(
          out, "initsigma\t\t", ifelse(is.na(staged_object()@initsigma), "NA", staged_object()@initsigma),
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
          if (!is.na(staged_object()@sampledims[1])) {
            out <- str_c(out, "sampledims:\t\t", paste(signif(staged_object()@sampledims, 3), collapse = "x"), "\n")
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
    if (input$sim_simple_gshape < 0){
      updateNumericInput(session, "sim_simple_gshape", value = 0)
      return(NULL)
    }

    if (input$sim_simple_bothdims == TRUE & input$sim_simple_dimx != input$sim_simple_dimy){
      updateNumericInput(session, "sim_simple_dimy", value = input$sim_simple_dimx)
    }

    simulate_kindist_simple(
      nsims = input$sim_simple_nsims, sigma = input$sim_simple_sigma, method = input$sim_simple_method,
      kinship = input$sim_simple_category, lifestage = input$sim_simple_lifestage, dims = c(input$sim_simple_dimx, input$sim_simple_dimy),
      shape = input$sim_simple_gshape
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
    if (input$sim_composite_shape < 0) {
      updateNumericInput(session, "sim_composite_shape", value = 0)
      return(NULL)
    }
    if (input$sim_composite_bothdims == TRUE & input$sim_composite_dimx != input$sim_composite_dimy){
      updateNumericInput(session, "sim_composite_dimy", value = input$sim_composite_dimx)
    }
    simulate_kindist_composite(
      nsims = input$sim_composite_nsims, initsigma = input$sim_composite_initsigma, breedsigma = input$sim_composite_breedsigma,
      gravsigma = input$sim_composite_gravsigma, ovisigma = input$sim_composite_ovisigma, dims = c(input$sim_composite_dimx, input$sim_composite_dimy),
      method = input$sim_composite_method, kinship = input$sim_composite_category, lifestage = input$sim_composite_lifestage,
      shape = input$sim_composite_shape
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
      gp <- gp + geom_freqpoly(data = app_env$d1@tab, colour = "blue", binwidth = 5)
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
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = "f", "Colour" = "g", .rows = 0)
    if ("1" %in% input$testsaveops) {
      temp <- app_env$d1
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "blue"
      )
    }
    if ("2" %in% input$testsaveops) {
      temp <- app_env$d2
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "red"
      )
    }
    if ("3" %in% input$testsaveops) {
      temp <- app_env$d3
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "orange"
      )
    }
    if ("4" %in% input$testsaveops) {
      temp <- app_env$d4
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "green"
      )
    }
    if ("5" %in% input$testsaveops) {
      temp <- app_env$d5
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "purple"
      )
    }
    if ("6" %in% input$testsaveops) {
      temp <- app_env$d6
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "black"
      )
    }
    if ("7" %in% input$testsaveops) {
      temp <- app_env$d7
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "grey"
      )
    }
    if ("8" %in% input$testsaveops) {
      temp <- app_env$d8
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "pink"
      )
    }
    if ("9" %in% input$testsaveops) {
      temp <- app_env$d9
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "brown"
      )
    }
    if ("10" %in% input$testsaveops) {
      temp <- app_env$d10
      rtable <- rtable %>% add_row(
        "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
        "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Colour" = "yellow"
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
      if (input$samp_bothdims == TRUE & input$samp_dimx != input$samp_dimy){
        updateNumericInput(session, "samp_dimy", value = input$samp_dimx)
      }
      temp <- c(input$samp_dimx, input$samp_dimy)
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
    rtable <- tibble("Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = "f", "Count" = 0L, .rows = 0)
    if (!is.null(samp_loaded_kindata())) {
      temp <- samp_loaded_kindata()
      if (is.KinPairSimulation(temp)) {
        rtable <- rtable %>% add_row(
          "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
          "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
      if (input$top_data_page == 1){
        rtable <- tibble("Slot" = "a", "Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = "f", "Count" = 0L, .rows = 0)
        temp <- app_env$d1
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "1", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
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
      else if (input$top_data_page == 2){
        rtable <- tibble("Slot" = "a", "Type" = "a", "Kernel" = "b", "Kinship" = "d", "Lifestage" = "e", "Dims" = "f", "Count" = 0L, .rows = 0)
        temp <- app_env$d6
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "6", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
          )
        }
        else {
          rtable <- rtable %>% add_row(
            "Slot" = "6", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
          )
        }
        temp <- app_env$d7
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "7", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
          )
        }
        else {
          rtable <- rtable %>% add_row(
            "Slot" = "7", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
          )
        }
        temp <- app_env$d8
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "8", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
          )
        }
        else {
          rtable <- rtable %>% add_row(
            "Slot" = "8", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
          )
        }
        temp <- app_env$d9
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "9", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
          )
        }
        else {
          rtable <- rtable %>% add_row(
            "Slot" = "9", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
          )
        }
        temp <- app_env$d10
        if (is.KinPairSimulation(temp)) {
          rtable <- rtable %>% add_row(
            "Slot" = "10", "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"), "Count" = nrow(temp@tab)
          )
        }
        else {
          rtable <- rtable %>% add_row(
            "Slot" = "10", "Type" = "KinPairData", "Kernel" = NA, "Kinship" = temp@kinship,
            "Lifestage" = temp@lifestage, "Dims" = NA, "Count" = nrow(temp@tab)
          )
        }

        return(rtable)
      }
    }
  )

  output$est_std_sumtable <- renderTable({
    est_std_sum()
  })
  # temp <- readRDS(here(paste0("temp/6.R")))
  # rtable <- rtable %>% add_row("Number" = 6, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"))
  # temp <- readRDS(here(paste0("temp/7.R")))
  # rtable <- rtable %>% add_row("Number" = 7, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"),)
  # temp <- readRDS(here(paste0("temp/8.R")))
  # rtable <- rtable %>% add_row("Number" = 8, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"))
  # temp <- readRDS(here(paste0("temp/9.R")))
  # rtable <- rtable %>% add_row("Number" = 9, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"))
  # temp <- readRDS(here(paste0("temp/10.R")))
  # rtable <- rtable %>% add_row("Number" = 10, "Type" = temp@simtype, "Kernel" = temp@kerneltype, "Kinship" = temp@kinship,
  #                             "Lifestage" = temp@lifestage, "Dims" = paste(signif(temp@simdims, 3), collapse="x"))
  #
  #  rtable
  # })

  ############# Error Functions #############

  #output$errorbar <- renderText({
  #  error_report()
  #})

  # Need to gather checks and put them together here!

  error_report <- reactive({
    "No Errors "
  })
}


# shinyApp(ui = ui, server = server)

#' Run kindisperse app
#'
#' @return returns a shiny app instance of kindisperse
#' @export
#'
run_kindisperse <- function() {
  shinyApp(ui = ui, server = server)
}
