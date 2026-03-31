library(haven)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tseries)
library(purrr)

wide <- read_sav('/Users/wangxinran/Library/CloudStorage/GoogleDrive-wangx225@wfu.edu/Shared drives/EMA/Data/latest/expiwell_03052025.sav')
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

#calculating autocorrelations of state granularity 
#function
get.ACF <- function(x,var) {
  clean <- na.omit(x[[var]])
  if (length(clean) < 2) return(data.frame(lag = 1, acf = NA))
  acf_result <- acf(clean, pl = FALSE)
  data.frame(
    lag = as.numeric(acf_result$lag),
    acf = as.numeric(acf_result$acf)
  )
}

#dataset with autocorrelations
ACFdata <- ICCdata %>% 
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var="granularity") %>% as.data.frame()) %>%
  ungroup()

#create a person-level summary from ACFdata
GACF_summary <- ACFdata %>%
  filter(lag == 1) %>%
  group_by(mplusID) %>%
  summarise(Ginertia = acf)

#calculating positive and negative affect inertia (autocorrelations of PA/NA), using the same function

#PA inertia
ACF_PA <- wide %>% 
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var = "positive") %>% as.data.frame()) %>%
  ungroup()
#person-level summary
PAIn_summary <- ACF_PA %>%
  filter(lag == 1) %>%
  group_by(mplusID) %>%
  summarise(PA_inertia = acf)

#NA inertia 
ACF_NA <- wide %>% 
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var = "negative") %>% as.data.frame()) %>%
  ungroup()

#person-level summary
NAIn_summary <- ACF_NA %>%
  filter(lag == 1) %>%
  group_by(mplusID) %>%
  summarise(NA_inertia = acf)

#calculate mean state granularity for each person
Mean_granularity <- ICCdata %>%
  group_by(mplusID) %>%
  summarise(MeanGranularity = mean(granularity, na.rm = TRUE))

#merge the three person-level ACFs and mean granularity
ACF_merged <- list(GACF_summary, NAIn_summary, PAIn_summary,Mean_granularity) %>%
  reduce(left_join, by = "mplusID")

#get qualtrics data
qualtrics<- read_sav('/Users/wangxinran/Library/CloudStorage/GoogleDrive-wangx225@wfu.edu/Shared drives/EMA/Qualtrics Data Management/qualtrics_02172025.sav')
#Qualtrics data management
## Removing trial rounds from the dataset
BaselineData_Participants <- subset(qualtrics, is.na(Q6_1) == FALSE)
View(BaselineData_Participants)

## Code for creating Mean Scores
# Reverse Coding for Q5: 1, 21, 26, 7, 17, 27, 3, 8, 28, 14, 19, 24, 10, 20, 30
BaselineData_Participants$Q5_1 <- 6 - BaselineData_Participants$Q5_1
BaselineData_Participants$Q5_21 <- 6 - BaselineData_Participants$Q5_21
BaselineData_Participants$Q5_26 <- 6 - BaselineData_Participants$Q5_26
BaselineData_Participants$Q5_7 <- 6 - BaselineData_Participants$Q5_7
BaselineData_Participants$Q5_17 <- 6 - BaselineData_Participants$Q5_17
BaselineData_Participants$Q5_27 <- 6 - BaselineData_Participants$Q5_27
BaselineData_Participants$Q5_3 <- 6 - BaselineData_Participants$Q5_3
BaselineData_Participants$Q5_8 <- 6 - BaselineData_Participants$Q5_8
BaselineData_Participants$Q5_28 <- 6 - BaselineData_Participants$Q5_28
BaselineData_Participants$Q5_14 <- 6 - BaselineData_Participants$Q5_14
BaselineData_Participants$Q5_19 <- 6 - BaselineData_Participants$Q5_19
BaselineData_Participants$Q5_24 <- 6 - BaselineData_Participants$Q5_24
BaselineData_Participants$Q5_10 <- 6 - BaselineData_Participants$Q5_10
BaselineData_Participants$Q5_20 <- 6 - BaselineData_Participants$Q5_20
BaselineData_Participants$Q5_30 <- 6 - BaselineData_Participants$Q5_30

# Creating Mean Scores for the Big Five
BaselineData_Participants$Extraversion <- apply(BaselineData_Participants
                                                [,c("Q5_1", "Q5_6", "Q5_11",  "Q5_16" , "Q5_21" , "Q5_26")], 1, mean, na.rm = TRUE)

BaselineData_Participants$Agreeableness <- apply(BaselineData_Participants
                                                 [,c("Q5_2", "Q5_7", "Q5_12",  "Q5_17" , "Q5_22" , "Q5_27")], 1, mean, na.rm = TRUE)

BaselineData_Participants$Conscientiousness <- apply(BaselineData_Participants
                                                     [,c("Q5_3", "Q5_8", "Q5_13",  "Q5_18" , "Q5_23" , "Q5_28")], 1, mean, na.rm = TRUE)

BaselineData_Participants$NegativeEmotionality <- apply(BaselineData_Participants
                                                        [,c("Q5_4", "Q5_9", "Q5_14",  "Q5_19" , "Q5_24" , "Q5_29")], 1, mean, na.rm = TRUE)

BaselineData_Participants$OpenMindedness <- apply(BaselineData_Participants
                                                  [,c("Q5_5", "Q5_10", "Q5_15",  "Q5_20" , "Q5_25" , "Q5_30")], 1, mean, na.rm = TRUE)

View(BaselineData_Participants)

# Creating a Mean Score for Ego Resilience
BaselineData_Participants$EgoResilience <- apply(BaselineData_Participants
                                                 [,c("Q23_1", "Q23_2", "Q23_3",  "Q23_4" , "Q23_5" , "Q23_6", "Q23_7", "Q23_8",
                                                     "Q23_9", "Q23_10", "Q23_11",  "Q23_12" , "Q23_13" , "Q23_14")], 1, mean, na.rm = TRUE)

#seperating big five and ego resilience from the qualtrics data
Personality<-BaselineData_Participants[,c("LoginID","Extraversion","Agreeableness","Conscientiousness", "NegativeEmotionality","OpenMindedness","EgoResilience")]

#restoring LoginID from wide dataset
# extract just the two ID columns from wide
id_lookup <- wide[, c("mplusID", "LoginID")]  # replace loginID with whatever it's actually called

# keep only unique combinations (one row per person)
id_lookup <- unique(id_lookup)

# merge onto your summary dataframe
ACF_merged <- merge(ACF_merged, id_lookup, by = "mplusID", all = FALSE)

#merge granularity with big five and ego resilience 
person_level <- merge(ACF_merged,Personality,by = "LoginID", all = FALSE)

#correlations
# select the variables you want
cor_data <- person_level %>%
  select(-LoginID, -mplusID)
# get pairwise correlations
cor(cor_data, use = "complete.obs")
library(Hmisc)
rcorr(as.matrix(cor_data))

#moderation (mean granularity,granularity inertia, neuroticism)
moderation <- lm(NegativeEmotionality~MeanGranularity*Ginertia,data=person_level)
summary(moderation)