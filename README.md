# AgriR

**AgriR** – Statistical tools for agricultural experiments with an interactive Shiny interface.

## Description

The **AgriR** package provides functions for statistical analysis of agricultural experiments as well as an interactive Shiny application (`menu_agriR`) that makes it easier to use the tools without directly writing R code.

Main features:
- Functions for the analysis of experimental designs.
- Graphical tools built on top of `ggplot2`.
- An included Shiny app for interactive use.

## Installation

The package is currently available on GitHub:

```r
# Install devtools if you don’t have it yet
install.packages("devtools")

# Install AgriR directly from GitHub
devtools::install_github("cicerocarlos-ufal/AgriR")

# Load the package
library(AgriR)

##  Basic usage

**After installation and loading:**

library(AgriR)

## Run the included Shiny application
runMenuAgriR()

List all available functions:

ls("package:AgriR")

## Manual Opening

If the automatic browser does not open, the console will print something like:

Listening on http://127.0.0.1:3604

Simply copy this URL and paste it into your browser to run the app.


## Dependencies

ggplot2

shiny

Other dependencies listed in the DESCRIPTION file

All dependencies are automatically installed when you install the package via devtools::install_github().

Contributing

Suggestions, bug reports and pull requests are welcome.
Please open an issue on GitHub
