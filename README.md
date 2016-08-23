# swimr
Visualizations and Reports for SWIM

### Installation

This package uses some dependencies that are not on CRAN. This should install all necessary dependencies.
```r
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
if("kfigr" %in% rownames(installed.packages()) == FALSE) {install.packages("kfigr")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("ggmap" %in% rownames(installed.packages()) == FALSE) {install.packages("ggmap")}
if("RSQLite" %in% rownames(installed.packages()) == FALSE) {install.packages("RSQLite")}
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")}
if("kfigr" %in% rownames(installed.packages()) == FALSE) {install.packages("digest")}
if("rhdf5" %in% rownames(installed.packages()) == FALSE) {
  source("http://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
}
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
  devtools::install_github("pbsag/swimr")
}

```
