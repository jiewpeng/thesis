packages <- c(
    "tidyverse", "rvest", "stringr", "Hmisc", "httr", "rjson", "lubridate",
    "bookdown", "lfe"
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

rm(packages)
rm(package)