# swimr
Visualizations and Reports for SWIM

### Installation
Install with the `devtools` package:

```r
devtools::install_github("tlumip/swimr")
```

This package uses some dependencies that are not on CRAN. Devtools should find most of these, 
but the following code will ensure you have all of them.

```r
# CRAN Dependencies
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("RSQLite" %in% rownames(installed.packages()) == FALSE) {install.packages("RSQLite")}
if("plotly" %in% rownames(installed.packages()) == FALSE) {install.packages("plotly")}
if("kfigr" %in% rownames(installed.packages()) == FALSE) {install.packages("kfigr")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")}
if("digest" %in% rownames(installed.packages()) == FALSE) {install.packages("digest")}
if("DT" %in% rownames(installed.packages()) == FALSE) {install.packages("DT")}

# Bioconductor dependency for omxr
if("rhdf5" %in% rownames(installed.packages()) == FALSE) {
  source("http://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
}

# Github libraries
if("omxr" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("gregmacfarlane/omxr")
}
if("dplyrExtras" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("skranz/dplyrExtras")
}
if("outviz" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("pbsag/outviz")
}
if("swimr" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("tlumip/swimr")
}

```

If you are working inside a proxy (like if you're ODOT staff), you will need to
configure your proxy for GitHub installs.

```r
library(httr)
set_config(use_proxy(url="proxynew.odot.state.or.us", port=8080)) 
set_config( config( ssl_verifypeer = 0L ) )
```

### Getting Started
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
  - After loading a template Rmd, update any required settings such as the input databases
