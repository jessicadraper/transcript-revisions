# ----------------------------------------------
# 
#   Automated Transcript Cleanup
#   STEP 2: Correcting format and timestamps
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
install.packages("chron")

# Load
library(readr)
library(stringr)
library(chron)


# ==============================
# Begin Editing
# (can highlight from HERE to end after packages above are installed)
# ==============================

# (!) YOUR working directory, ending with "Step 2" folder (wd):
your_wd <- "/Users/jdraper/Jess Personal/git repos/transcript-revisions/Step 2"

setwd(your_wd)
getwd() # Confirm correct wd

# Getting file name list from working directory
filelist <- list.files(pattern = ".txt")

# Creating empty data frame
files <- data.frame(Name = NA, Text = NA, End_Time = NA)

# Collecting (new) names and text content into db
for (i in 1:length(filelist)) {
  files[i,1] <- paste("S2_",filelist[i], sep="") # Adding "S2_" prefix to file names (for step 2)
  files[i,2] <- read_file(filelist[i])
}

# Extracting end time
for (i in 1:length(filelist)) {
  txt <- files[i,2]
  end_time <- str_extract_all(txt,"--> (\\d{1,2}:\\d{2}:\\d{2}.\\d{3})", simplify = TRUE)
  end_time <- end_time[ncol(end_time)]
  files[i,3] <- str_replace(end_time, "--> ", "")
}

# Creating function for editing transcripts
transcribe <- function (x) {
  
  x %>% 
    str_replace_all("<.? (INV|PAR|INV)>", "<v \\1>") %>% # --- corrects to lowercase v
    str_replace_all("(<v (INV|PAR|INV)>)\\s?(\\d{1,2}:\\d{2}:\\d{2}.\\d{3} --> \\d{1,2}:\\d{2}:\\d{2}.\\d{3})\\r\\n(.*)", "\\3\r\n\\1 \\4") %>% # --- fixes if speaker codes are before timestamp
    str_replace_all("(\\r\\n\\r\\n)\\s{1}(\\d{1,2})","\\1\\2") %>% # --- removes single space before timestamps
    str_replace_all("(\\d{3}\\r\\n)\\s{1}","\\1") %>% # --- removes single space before text line
    str_replace_all("(\\d{1,2}:\\d{2}:\\d{2}.)(\\d{2,3})","\\1000") %>%   # --- adjusts all the partials time suffixes to .000
    str_replace_all("(INV|PAR|INT):","<v \\1>") %>%   # --- corrects speaker ID e.g. from INV: to <v INV>
    str_replace_all("/\\s+<v (INV|PAR|INT)>","/") %>%   # --- removes speaker IDs after /text/
    str_replace_all("—->","-->") %>% # --- fixing hyphen issues
    str_replace_all("-—>","-->") %>% # --- fixing hyphen issue 2
    str_replace_all("\\n\\n(\\d{1,2}:\\d{2}:\\d{2}.\\d{3})","\r\n\r\n\\1") %>%  # --- fixing line issue before some timestamps
    str_replace_all(".000\\s*\\n<v (INV|PAR|INT)>",".000\r\n<v \\1>") %>% # --- fixing line issue before speaker ids
    str_replace_all("\\r\\n\\r\\n\\r?\\n?(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) --> (\\d{1,2}:\\d{2}:\\d{2}.\\d{3})\\r?\\n(?!<)"," ") %>%   # --- collapses timestamps within a speaker's dialogue
    str_replace_all("([:alnum:]|[:punct:])\\s?\\r\\n([:alnum:]|[:punct:])","\\1 \\2") %>%   # --- removes unnecessary line breaks within dialogue
    str_replace_all("(\\r\\n\\s{3,}|\\r\\n\\t|\\s{2}\\r\\n\\s{2})"," ") %>%  # --- removes further unnecessary line breaks and tabs within dialogue
    str_replace_all("  "," ")  %>% # --- fixing double spaces (fixes some, not all)
    str_replace_all("  "," ")  %>% # --- fixing double spaces again (fixes the rest)
    str_replace_all("\\s{1,}(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) -->","\r\n\r\n\\1 -->") %>% # making sure all timestamps start new line
    str_replace_all("0{3,}:","00:") %>% # fixing cases of wrong number of timestamp digits
    str_replace_all("(?<!\\r\\n)<v (INV|PAR|INT)>","\r\n\r\n00:00:00.000 --> 00:00:00.000\r\n<v \\1>") %>% # --- breaks different speakers to new lines, empty timestamps
    str_replace_all("(?<!000\\r\\n)<v (INV|PAR|INT)>","\r\n00:00:00.000 --> 00:00:00.000\r\n<v \\1>") %>% # --- breaks different speakers with short lines back to back to new lines, empty timestamps
    str_replace_all("([:alnum:]|[:punct:])\\s?\\r\\n([:alnum:]|[:punct:])","\\1 \\2") %>%   # --- removes unnecessary line breaks within dialogue (again)
    str_replace_all("(?<=<v PAR>)(.*)\\r\\n\\r\\n\\r?\\n?(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) --> (\\d{1,2}:\\d{2}:\\d{2}.\\d{3})\\r\\n<v PAR>","\\1 ") %>%   # --- collapses timestamps within a speaker's dialogue with consecutive speaker codes
    str_replace_all("(?<=<v (INT|INV)>)(.*)\\r\\n\\r\\n\\r?\\n?(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) --> (\\d{1,2}:\\d{2}:\\d{2}.\\d{3})\\r\\n<v (INT|INV)>","\\2 ") %>%   # --- collapses timestamps within a speaker's dialogue with consecutive speaker codes
    str_replace_all("\\r\\n\\r\\n\\r\\n", "\r\n\r\n")
}

# Running function over text column of "files" data frame
files$Text <- sapply(files[,2], transcribe)


# ---------------------------------------
# BIG Function: Assigning New Timestamps
# ---------------------------------------

assign_timestamps <- function (name, txt, endTime) {
  
  cat("Assigning new timestamps to ", name, "...\n", sep="")
  # Creating data frame of timestamps
  ts <- str_extract_all(txt,"(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) -->", simplify = TRUE)
  ts <- as.data.frame(t(ts))
  colnames(ts) = "start"
  
  ts$start <- str_replace_all(ts$start, " -->", "")
  ts$end <- NA
  
  # Loop for defining end times as the next speaker's start time
  for (i in 1:nrow(ts)) {
    ts[i,2] <- ts[(i+1),1]
  }
  
  # assigning last timestamp to df
  ts[nrow(ts),2] <- endTime #ts_last
  
  # putting dialogue text as column
  split <- t(str_split(txt, "(\\d{1,2}:\\d{2}:\\d{2}.\\d{3}) --> (\\d{1,2}:\\d{2}:\\d{2}.\\d{3})", simplify = TRUE))
  split <- split[-1,]
  if (nrow(ts) == length(split)) {
    ts$text <- split
  } else {
    cat("ERROR! There is likely a major formatting issue in the following transcript:", name, "\n")
  }
  
  # removing extra .000 at end of times (temporarily)
  ts$start <- str_replace_all(ts$start, "(\\d{1,2}:\\d{2}:\\d{2}).\\d{3}","\\1")
  ts$end <- str_replace_all(ts$end, "(\\d{1,2}:\\d{2}:\\d{2}).\\d{3}","\\1")
  
  # converting chr time to actual calculatable time
  ts$start <- chron(times = ts$start)
  ts$end <- chron(times = ts$end)
  
  # Loop to calculate and assign new timestamps for missing timestamps
  for (i in 1:nrow(ts)) {
    non_zero <- which(ts[,2] != chron(times = "00:00:00")) # Get indexes of times where end is not 00:00:00
    # if start time is real and end time is 00:00:00, or if first timestamp is 00:00:00
    if ((ts[i,2] == chron(times = "00:00:00") & ts[i,1] != chron(times = "00:00:00")) | (ts[i,2] == chron(times = "00:00:00") & ts[i,1] == chron(times = "00:00:00") & i == 1)) {
      # get next real end time
      next_r <- non_zero[min(which(non_zero > i))]
      next_t <- ts[next_r,2]
      # calculate difference in steps between next real and current timestamp position
      diff <- next_r - i
      # increment by distance between real times divided by number of steps in between
      increment <- (next_t - ts[i,1])/(diff + 1)
      ts[i,4] <- paste(next_r, diff, increment)
      
      for (j in 1:diff) {
        if (j == 1) {
          ts[i,2] <- ts[i,1] + (increment*j)
        }
        ts[(i+j),1] <- ts[i,1] + (increment*j)
        ts[(i+j),2] <-  ts[i,1] + (increment*(j+1))
      }
      
    } else {}
  }
  
  # new timestamps with .000
  ts$new_start <- paste(ts$start,".000", sep="")
  ts$new_end <- paste(ts$end,".000", sep="")
  
  # if start and end equal each other, then add .250 and .750 increments to following timestamps
  for (i in 1:nrow(ts)) {
    if (ts[i,1] == ts[i,2]) {
      ts[i,6] <- paste(ts[i,2], ".250", sep="")
      ts[(i+1),5] <- paste(ts[(i+1),1], ".750", sep="")
      
    } else {}
  }
  
  ts$new_text <- paste(ts$new_start," --> ", ts$new_end, ts$text,sep = "")
  new_file <- paste(ts$new_text, collapse = "")
  txt <- paste("WEBVTT\r\n\r\n",new_file,sep="")
}

# Running function over text column of "files" data frame
files$Text <- mapply(assign_timestamps,files[,1],files[,2],files[,3])

# ==============================
# Save Edited Files
# ==============================

# Set folder location for saving modified files and change working directory to this location
setwd(paste(your_wd,"/Post R Step 2", sep=""))
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
paste("Step 2 script complete!")


# ==============================
# End
# ==============================
