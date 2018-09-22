# test that out

source("fancydht.R")
source("unflatten.R")
library(Distance)

# birbs
birds <- read.csv("http://workshops.distancesampling.org/stand-intermed-2018/R_tutorial/montrave-line.csv")



# get effort right
birds$Effort <- birds$Effort * birds$repeats
birds$repeats <- NULL
birds$visit <- NULL
# insert object field
birds$object <- 1:nrow(birds)

bird_df <- ds(birds, truncation=95, key="hr", convert.units=.1)

new_dht <- fancydht(bird_df, birds, ~species, convert.units=.1)

