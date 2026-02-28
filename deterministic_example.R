library(tidyverse)
setwd("G:/Shared drives/EMA/Data/latest")

the.data <- read_sav("expiwell_03052025.sav")
# Let's use the first set of daily ratings for subject 1
example <- data.frame(the.data[1:9 , ])

# OK, so this function is going to give us everything we need to work with in the decision tree
# Seven quantities of interest here:
# average: The average weekly rating of a given emotion
# peak: The peak rating of a given emotion
# end: The last rating of a given emotion
# c.peak: The peak rating of any concordant emotion
# c.end: The average of the last ratings of concordant emotions
# d.peak: The peak rating of any discordant emotion
# d.end: The average of the last ratings of any discordant emotions
# We put the data and the name of the focal emotion into the function 
# (e.g., organize(the.data, "sad"))
organize <- function(a, emotion) {
  
  ##### Focal emotion is whatever emotion we're dealing with
  focal <- a[,emotion]
  #Peak defined as max of whichever isn't the end
  peak <- max(focal[1:(length(focal) - 1)], na.rm = TRUE) 
  end <- focal[length(focal)] #End is whatever the last one was
  average <- mean(focal, na.rm = TRUE)
  
  ##### Identify concordant emotions
  positive <- c("amused", "awe", "content", "glad", "grateful", 
                "hopeful", "inspired", "interested", "love", "proud")
  negative <- c("angry", "ashamed", "scared", "disgust", "embarrassed",
                "repentant", "sad", "contemptuous", "stressed", "hate")
  # If emotion in question is negative, "concordant" emotions are also negative; so too for positive
  if (emotion %in% negative == TRUE) {
    concordant <- a[,negative]
    discordant <- a[,positive]
  } else {
    concordant <- a[,positive]
    discordant <- a[,negative]
  }
  
  # Now let's get the peak and end for concordant and discordant emotions
  # We're going to define the peak as any peak experience of emotion in the corresponding group
  c.peak <- max(concordant[1:(nrow(concordant) - 1),], na.rm = TRUE) 
  c.end <- mean(t(concordant[nrow(concordant),]))
  
  d.peak <- max(discordant[1:(nrow(discordant) - 1),], na.rm = TRUE) 
  d.end <- mean(t(discordant[nrow(discordant),]))

  list(average = average, 
       peak = peak,
       end = end,
       c.peak = c.peak,
       c.end = c.end,
       d.peak = d.peak,
       d.end = d.end)
}


# Model 1
# Under this set of assumptions, peak takes priority. 
# Step 1: If you have a high peak (let's call it greater than 4/5), you list that. 
# If not, step 2:
# If you have a high peak for a concordant emotion (again, 4/5), you list that.
# If not, step 3: 
# If your end rating is more than 1 point away from your average weekly rating, you list your average weekly rating.
# If not, step 4:
# You list your end rating.
# There are much more sophisticated versions of this that incorporate probability -- 
# so if you have a high peak, for instance, maybe there will be a 60% chance you list your peak
# Let's pick a completely deterministic example for now.

sad.example <- organize(example, "sad")

sad.example$retro <- with(sad.example, case_when(
        peak    > 4 ~ peak,                             # Step 1: high peak
        c.peak > 4 ~ c.peak,                              # Step 2: high concordant peak
        abs(end - average) > 1 ~ average,                 # Step 3: end far from average
        TRUE ~ end                                        # Step 4: default to end
    )
  )
