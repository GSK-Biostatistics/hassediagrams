hassediagram: generates the layout structure and restricted layout
structure of experimental designs
================
Damianos Michaelides, Simon Bate, Marion Chatfield

## Overview

The `hassediagram` package provides tools to visualize the **structure
of experimental designs** using Hasse diagrams. The package determines
the structure of the design, summarised by the layout structure, and
uses this structure to generate a Hasse diagram. This diagram describes
the structure of the design and the relationships between the factors
that define the design. By considering the randomisation performed, in
conjunction with the layout structure, a list of randomisation objects
can be identified, known as the restricted layout structure. The package
can also be used to generate a Hasse Diagram of this restricted layout
structure. Objects in the restricted layout structure can be used to
identify the terms to include in the statistical model.

The package is an implementation of the methodology described in Bate
and Chatfield (2016a and 2016b).

Bate S.T., Chatfield M.J. (2016a). “Identifying the structure of the
experimental design.” Journal of Quality Technology, 48(4), 343–364.

Bate S.T., Chatfield M.J. (2016b). “Using the structure of the
experimental design and the randomization to construct a mixed model.”
Journal of Quality Technology, 48(4), 365–387.

## Features

### Functions

`hasselayout()`

Creates a Hasse diagram of the layout structure. To generate the diagram
all this is needed is a dataset consisting of the experimental factors
that define the experimental design.

`itemlist()`

A function needed to generate the objects required to generate the Hasse
diagram of the restricted layout structure

`hasserls()`

Creates the Hasse diagram of the restricted layout structure. Inputs to
the function are the `rls` object, generated using the itemlist()
function, an object that defines the randomisation objects and an object
that defines the randomisation arrows.

## Installation

### Install from GitHub (Development Version)

The development version of `hassediagram` can be installed with:

``` r
# Install devtools (if not already installed)
install.packages("devtools")

# Install hassediagram from GitHub
devtools::install_github("GSK-Biostatistics/hassediagram")
```

### Install from CRAN (when made available)

``` r
install.packages("hassediagram")
```

## Usage

Load the package:

``` r
library(hassediagram)
```

## Example: Generate Hasse Diagrams of the Layout Structure and Restricted Layout Structure

### A fractional factorial design for investigating asphalt concrete production

#### Generate the Hasse diagram of the layout structure

``` r
hasselayout(datadesign=concrete, larger.fontlabelmultiplier=1.6, 
            smaller.fontlabelmultiplier=1.3, table.out="Y")
```

#### Generate objects required to create the Hasse diagram of the restricted layout structure

``` r
concrete_objects <- itemlist(datadesign=concrete) 
```

#### Define the names of the randomisation objects corresponding to the structural objects

``` r
concrete_rls <- concrete_objects$TransferObject 
concrete_rls[,2] <- concrete_rls[,1]
concrete_rls[27,2] <- c("AC^AG^CC^CoT^CuT \u2192 Run")
```

### Generate the Hasse diagram of the restricted layout structure

``` r
hasserls(object=concrete_objects, randomisation.objects=concrete_rls, 
         larger.fontlabelmultiplier=1.6, smaller.fontlabelmultiplier=1.3, 
         table.out="Y", arrow.pos=8)
```

## Documentation

For an introduction to the methodology, check the package vignette:

``` r
vignette("Introduction_to_hassediagram")
```

and for an introduction to the package implementation visit the
documentation.

## Maintenance

This package is in a stable state and will only be updated for bug
fixes.

The person responsible for monitoring this package is Simon Bate
<simon.t.bate@gsk.com>.
