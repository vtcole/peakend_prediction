library(haven)
library(tidyr)
library(dplyr)
library(ggplot2)


wide <- read_sav('G:/Shared drives/EMA/Data/latest/expiwell_03052025.sav')
wide <- subset(wide, wide$which != "weekly")


#Why do we have one observation with more than 840 hours?
wide <- subset(wide, wide$hours < 841)

#Convert from wide to long by emotion
long <- wide %>%
  pivot_longer(
  cols = amused:hate,
  names_to = "emotion",
  values_to = "value"
  )

#Make a subscale variable for positive vs. negative emotions
long$subscale <- NA
long$subscale[long$emotion == "amused" |
                long$emotion == "awe" |
                long$emotion == "content" |
                long$emotion == "glad" |
                long$emotion == "grateful" |
                long$emotion == "hopeful" |
                long$emotion == "inspired" |
                long$emotion == "interested" |
                long$emotion == "love" |
                long$emotion == "proud"] <- "positive"
long$subscale[long$emotion == "angry" |
                long$emotion == "ashamed" |
                long$emotion == "contemptuous" |
                long$emotion == "disgust" |
                long$emotion == "embarrassed" |
                long$emotion == "hate" |
                long$emotion == "repentant" |
                long$emotion == "sad" |
                long$emotion == "scared" |
                long$emotion == "stressed"] <- "negative"

#Get the mean emotion variable, across both subscales
long <- long %>%
  group_by(mplusID, hours) %>%
  mutate(grand_mean = mean(value, na.rm = TRUE)) %>%
  ungroup()

#Get the mean emotion variable, by subscale
long <- long %>%
  group_by(mplusID, hours, subscale) %>%
  mutate(subscale_mean = mean(value, na.rm = TRUE)) %>%
  ungroup()

#Get ICC's -- need to write a function for this
get.ICC <- function(x) {
  1 - var(x$subscale_mean)/var(x$value)
}

#Now, at long last, here's the dataset with granularity
ICCdata <- long %>% 
  group_by(mplusID, hours) %>%
  group_modify(~ get.ICC(.x) %>% as.data.frame()) %>%
  ungroup()

names(ICCdata)[3] <- "granularity"

#Let's plot it!
#First, just select a subset
subdata <- subset(ICCdata, ICCdata$mplusID < 26)

#Now plot lines
ggplot(subdata, aes(x = hours, y = granularity, group = mplusID, color = factor(mplusID))) +
  geom_line(alpha = 0.4) +
  theme_minimal() +
  labs(x = "Hours", y = "Granularity", title = "Granularity Over Time by Person") +
  theme(legend.position = "none")

#Now plot ~*~smoothed~*~ lines
ggplot(subdata, aes(x = hours, y = granularity, group = mplusID, color = factor(mplusID))) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 0.8) +
  theme_minimal() +
  labs(x = "Hours", y = "Granularity", title = "Granularity Over Time by Person") +
  theme(legend.position = "none")