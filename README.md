###############################
Overview of the Project
###############################

# To do

- [ ] `./R/`: add documentation to functions 
- [ ] `./R/`: add stopifnot conditions
- [ ] `./tests/`: add tests to functiqns 

# Resume

The goal of this project is to investigate the modulation of shrub-saplings
interactions at the community scale.

We set up an experiment near Murcia in collaboration with the group of the
professor Susana Bautista of the University of Alicante.

This git repository contains all the datas and codes needed to compute the
analysis.

# How to 

# Structure of the repository

The structure of the repo was inspired from the
[`rrtools`](https://github.com/benmarwick/rrtools) and the book of Hadley
Wickham on creating [`R package`](http://r-pkgs.had.co.nz/), especially to
organize the [`data set`](http://r-pkgs.had.co.nz/data.html#data-extdata).

  . thesis_chap2_alicante
    ├── data
    │   ├── processed
    │   └── raw
    │       ├── leafs
    │       │   ├── mars
    │       │   └── t0
    │       ├── mart
    │       ├── pilot
    │       └── scan_data
    ├── figures
    ├── inst
    │   ├── doc
    │   └── examples
    │       ├── concept
    │       ├── mart
    │       └── pilot
    ├── R
    └── tests

## data-raw

The raw data are the same than the ones that were recorded on the datasheets.

The raw data had been processed in order to analyze them. The procedures used
can be found in this file:

- [`00_prepare_plant_data.r`](./data-raw/00_prepare_plant_data.r)

The raw data are not supposed to be used directly. Use the processed data instead as
described in the next subsection.  

## data

This folder contains all the processed data from the experiment. 
The processed files are in `.rda` format.

Once the package is loaded, you can directly load them by running for example:

     data(holes_data) 

## figures

All the figures produced by the `rmarkdown` files are found in this folder.

## inst 

This folder contains the `rmarkdown` files that performs the analysis of the
datas.

The sub folder `examples` contains rmd performing exploratory analysis.

The sub folder `doc` contains all the analysis that will be included in the
paper.

## R

This folder contains all the functions that are used in the analysis.

## tests

This folder contains the tests of the functions that are placed in the R
subfolder.


# To do list

- [ ] document data files
- [ ] put script in function
