packages <- c(
    "tidyverse", "rvest", "stringr", "Hmisc", "httr", "rjson", "lubridate",
    "lfe", "grid", "gridExtra", "lazyeval", "geosphere", "haven", "forcats"
)

for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
        install.packages(package, repos = "https://cloud.r-project.org/")
        if (package == "tidyverse") {
            library(package, pos = 2, character.only = TRUE)
        }
        else {
            library(package, character.only = TRUE)
        }
        
    }
}

# custom build of stargazer is needed if using bookdown to generate pdf
# install.packages("devtools")
# detools::install_github("rstudio/rmarkdown)
# devtools::install_github("jiewpeng/stargazer_bookdown")
# devtools::install_github("rstudio/bookdown")

library(stargazer)
library(bookdown)

rm(packages)
rm(package)