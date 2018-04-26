# swimr
Visualizations and Reports for SWIM

### User's Guide
See https://github.com/tlumip/tlumip/wiki/Visualizing-Outputs

### Developer's Guide
  - Getting Started
    - Make sure you have a SWIM reporting database (*.db sqlite file)
    - Open `swimr.Rproj` in RStudio
    - Open the `vignettes\pop_and_emp.Rmd` Rmarkdown file
    - Update the input reporting datatbase file name - `db_path`
    - Render the Rmd as html with [knitr](https://yihui.name/knitr/) via the Knit button
    - swimR contains four template Rmd files:
      - assignment validation
      - compare scenarios
      - population analysis
      - reference scenario
    - The template Rmd files are available via File + New File + R Markdown + From Template
    - After loading a template Rmd, update any required settings such as the input databases, and then knit it
  - Static Package Data
    - swimr contains a number of static data sets in the data folder, including the zone polygons used for mapping
    - To update these files, do the following:
      - Open `swimr.Rproj` in RStudio
      - Find the relevant R script and/or data in data-raw folder
      - Update the relevant raw data and re-run the script to update the version in the data folder
    - For example, to update the zone polygons, do the following:
      - Open the VISUM version file in VISUM and export the zones to shapefile via File + Export + Shapefile
      - Make sure to export all fields
      - Copy the shapefile to the data-raw folder
      - Run data-raw\layers.R
      - The data\zones.rda, data\zones_data.rda, and data\zones_shp.rda will be updated
