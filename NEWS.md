# kindisperse 0.9.1
Changed all occurrences of the 'gamma' kernelshape/method parameter value to 'vgamma' to make it clear we are using a variance-gamma distribution, not a gamma distribution. 
(effects simulation functions)
Removed unused dispersal kernels (simulation functions)
Changed simulation shape parameter default from 1 to 0.5. 

# kindisperse 0.9.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed several display and layout issues in Shiny app
* Changed parameter of 'elongate' function from 'shape' to 'aspect' to make clear we are manipulating aspect ratios of a rectangular area. 

# kindisperse 0.9.0

* Permutation functions now output mean instead of median
* Changed all 'dsigma' parameters & references to 'posigma'
* Changed simulation 'method' parameter input from 'Gamma' to 'gamma'
* Changed 'lifestage' parameter values from 'larva' to 'immature' and from 'oviposition' to 'ovipositional'
* Enable package function messages to be suppressed
* Updated app tutorial and documentation (including vignettes)
* Updated KinPairSimulation '<-sampledim' method
* Added Table of Contents to README

# kindisperse 0.8.0

* Added Mentari Court field data for example calculations
* Added 2d rectangular dimensions to simulate & sample functions
* Changed the way 'sample_kindist' deals with dimensions
* Added helper function 'elongate' for dimension handling
* Changed 'composite' default parameter value from 2 to 1 in function 'axpermute'
* Changed 'juvsigma' parameter & references to 'initsigma' in line with usage in paper
* included rgamma function
* Improved simgraph commands (simgraph_data, simgraph_graph)
* Cleaned up app
* Removed unneeded class methods (to_tibble). Fixed kernelshape method
* Improved package load
* Updated app interface
* Updated documentation (CRAN compatibility)

# kindisperse 0.7.0

* Added options to import data from files within app
* Updated readme, package startup & vignettes
* Added gamma kernel with shape parameter to simulations

# kindisperse 0.6.0

* Changed 'category' parameter in all functions and classed to 'kinship' parameter
* Added file writing and reading functions (write_kindata, read_kindata, kinpair_to_tsv, tsv_to_kinpair, kinpair_to_csv, csv_to_kinpair, kinpair_to_tibble)
* Added appdata interface environment & functions to access it (mount_appdata, unmount_appdata, display_appdata, reset_appdata, retrive_appdata)
* Enable loading of appdata into app (functions and interface)
* Improved KinPairSimulation object print function

# kindisperse 0.5.0

* Replaced file-based memory storage with environment storage in app
* Added 'distances' method for KinPairData & KinPairSimulation classes
* Added functions to test class membership
* Updated axial functions to take KinPairData &c classes as input
* Limit sample size in axial permuation functions under 'std' option
* Updated class definitions
* Added class constructor functions
* Added class import functions
* Updated dependencies

# kindisperse 0.4.0

* Introduced KinPairData & KinpairSimulation classes
* Added and updated class methods & documentation
* Updated app with axial functions
* Fixed bug in 'axials_decompose' function

# kindisperse 0.3.0

* improved NAMESPACE
* add axial function
* document many functions

# kindisperse 0.2.0

* added temp folder & contents to fix app bug
* removed broken dependencies

# kindisperse 0.1.0 & prior

* initial commit
