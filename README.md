
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IVODE

<!-- badges: start -->
<!-- badges: end -->

This *R*-package aims to model the gaining and waning of immunity to
various vaccine-preventable diseases. The backend of this package is
[Odin](https://github.com/mrc-ide/odin).

## Installation

You can install the development version of IVODE like so:

``` r
devtools::install_github("IVODE")
```

Current models avaiable are:

- static_model: static (meaning fixed force of infection) ODE model of
  immunity to a vaccine-preventable disease
- dynamic_model: dynamic (meaning infections and recovery are explictly
  modelled) ODE model of immunity to a vaccine-preventable disease

## Example

Models are access through the functions: - `simulate` which simulates
the given model for single set of parameters and returns the output as a
tibble, - `project_point_estimate` which simulates the given model for a
range of parameters (representing different diseases) and returns select
outputs as a tibble

Other functions are: - `format_output` which can be used to format the
output of `simulate` from compartment values to more understandable
outputs such as people with immunity to infection - `sample_parameters`
which can be used to sample parameters for range of diseases
