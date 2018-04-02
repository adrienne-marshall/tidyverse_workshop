## This script installs packages needed for the UI Library workshop, 
## "Fun and efficient data wrangling with R: An introduction to the tidyverse."
## The script checks for whether these packages are already installed, and only installs them if not already installed.
## If you want to update pre-installed packages, take out the if statement. 

packages <- c("tidyverse", "reshape2", "lubridate", "tidytext")

for(i in 1:length(packages)){
  if (is.element(packages[i], installed.packages()[,1]) == FALSE) { 
    install.packages(packages[i]) 
    } else {print(paste0(packages[i], " is already installed."))} # end if statement
} # end looping through packages. 

