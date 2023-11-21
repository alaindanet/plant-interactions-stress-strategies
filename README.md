# Effect of species richness and nursing in drylands

This repository contains all the data analysis for the manuscript entitled
"Species diversity promotes facilitation under stressful conditions" submitted
to Oikos.

## Repository structure

### Raw data

The raw data files have been curated in two scripts located under `data-raw`
folder:
- `00_prepare_plant_data.R`
- `01_prepare_trait_data.R`

The raw data can be found on Zenodo: https://doi.org/10.5281/zenodo.10171339

### Data analysis

The curated data have been then analyzed using the R package `targets` to ensure
that the analysis are reproducible and up to date.

The code performing the analysis and producing the plots can be found in
`uncheck_targets.Rmd`.

### Manuscript

- Main text: `paper/main_text.Rmd`
- Supplementary information: `paper/supplementary_figures.Rmd`
- Figures: `paper/graphics/`

