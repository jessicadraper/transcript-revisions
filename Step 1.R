 # ----------------------------------------------
 #   
 #   Automated Transcript Cleanup
 #   STEP 1: Removing Distracting Lines
 #
 #   Help: https://stringr.tidyverse.org/articles/regular-expressions.html
 #
 #   *This project has been funded in part by National Endowment for the Humanities: Exploring the Human Endeavor. http://www.neh.gov *
 #
 # ----------------------------------------------

# ==============================
# Packages
# ==============================

# Install (only need to install once)
install.packages("readr")
install.packages("stringr")

# Load
library(readr)
library(stringr)


# ==============================
# Begin Editing
# (can highlight from HERE to end after packages above are installed)
# ==============================

# (!) YOUR working directory, ending with "Step 1" folder (wd):
your_wd <- ".../Step 1"

setwd(your_wd)
getwd() # Confirm correct wd

# Getting file name list from working directory
filelist <- list.files(pattern = ".txt")

# Creating empty data frame
files <- data.frame(Name = NA, Text = NA)

# Collecting (new) names and text content into db
for (i in 1:length(filelist)) {
  files[i,1] <- paste("S1_",filelist[i], sep="") # Adding "S1_" prefix to file names (for step 1)
  files[i,2] <- read_file(filelist[i])
}

# Creating function for editing transcripts
transcribe <- function (x) {
  
  x %>%
    str_remove_all("\r\n\r\nNOTE .*")  %>% #--- removes all NOTES and blank lines underneath
    str_remove_all("\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}(-\\w{1})?\\r\\n") # --- removes all random letter/number combinations (updated Feb 2024)
    # str_remove_all("\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}\\r\\n") # --- older version
}

# Running function over text column of "files" data frame
files$Text <- sapply(files[,2], transcribe)


# ==============================
# Save Edited Files
# ==============================

# Set folder location for saving modified files and change working directory to this location
setwd(paste(your_wd,"/Post R Step 1", sep=""))
getwd()

# Function for saving files
savefile <- function (df, i) {
  write_file(df[i,2], df[i,1])
}

# Running function
for (i in 1:nrow(files)) {
  savefile(files, i)
}

# Notification when script completes
paste("Step 1 script complete!")

# ==============================
# End
# ==============================
