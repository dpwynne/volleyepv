# hey there!

# you only need to install a package one time - ex: like downloading Spotify one time to your computer
install.packages("tidyverse")
install.packages("remotes")

# you'll need to load the libraries each time you re-open R - ex: like opening Spotify each time you restart your computer
library(tidyverse)
library(remotes)

# these packages aren't available in R's main repository (CRAN), so we get them directly from GitHub
remotes::install_github("openvolley/datavolley")
remotes::install_github("volleydork/volleyR")

# load these GH libraries too
library(datavolley)
library(volleyR)

# check which folder / directory you are in
# create a new folder, within your current folder and call it "dvwfiles"
# you will need to actually create this folder manually... this is not a piece of the code
getwd()

# throw maybe a dozen dvw files from volleymetrics (not stuff you have created from scratch) - VM files you have cleaned are fine
# check the number of files using the length function to make sure it's the same number you think you added to the folder
my.files <- list.files(path = "./dvwfiles", pattern = "*.dvw", full.names = TRUE)
length(my.files)

# do magic, should take a few seconds - depending on how many files you put in the folder
fancy_dataset <- volleyR::build_epv_average(my.files)

# take a look under the hood
head(fancy_dataset)

# take it for a test drive
volleyR::best_player_epv_avg(data = fancy_dataset, skill = "Attack", min_attempts = 20, top_X = 5)
