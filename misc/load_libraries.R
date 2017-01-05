packages <- c(
    "tidyverse", "rvest", "stringr", "Hmisc", "httr", "rjson", "lubridate",
    "bookdown", "lfe", "grid", "gridExtra"
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
# devtools::install_github("jiewpeng/stargazer_bookdown")

library(stargazer)

rm(packages)
rm(package)