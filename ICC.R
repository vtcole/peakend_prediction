# ===========================================================================
# Project:      State and Trait Emotional Granularity Analysis
# Date created: 2026-03-24
# Last updated: 2026-04-18
#
# Description:
#   Computes multiple operationalizations of emotional granularity from EMA 
#   data and examines their psychometric properties. Specifically:
#     (1) State granularity (Lane & Trull's within-moment ICC)
#     (2) Trait granularity (Hoemann et al.'s person-level ICC,Ebras's two ICCs)
#     (3) Momentary emotional differentiation (Ebras et al.'s approach
#         via the emodiff package)
#
#   Analyses address whether state granularity reflects stable individual
#   differences, whether its within-person dynamics (MSSD, SD, 
#   autocorrelation) overlap with simpler affect dynamics measures, and
#   how the different granularity operationalizations relate to each
#   other and to personality traits.
#
# Data sources:
#   - EMA data:          expiwell_03052025.sav
#   - Qualtrics baseline: qualtrics_02172025.sav
# ===========================================================================
library(haven)
library(tidyverse)
library(easystats)
library(tseries)
library(lme4)
library(lmerTest) 
install.packages('devtools')
library(devtools)
install_github("seanchrismurphy/emodiff")
library(emodiff)

wide <- read_sav('/Users/wangxinran/Library/CloudStorage/GoogleDrive-wangx225@wfu.edu/Shared drives/EMA/Data/latest/expiwell_03052025.sav')
wide <- subset(wide, wide$which != "weekly")

#get rid of participant 26, with only 1 observation and all the values missing
wide <- wide %>%
  filter(mplusID != 26)

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

#calculating autocorrelations of state granularity, PA and NA (temporary, will be merged into person_level)
#granularity inertia
G_inertia <- ICCdata %>% 
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var = "granularity") %>% as.data.frame()) %>%
  filter(lag == 1) %>%
  summarise(G_inertia = acf) %>%
  ungroup() 

#PA inertia
PA_inertia <- wide %>%
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var = "positive") %>% as.data.frame()) %>%
  filter(lag == 1) %>%
  summarise(PA_inertia = acf) %>%
  ungroup() 

#NA inertia 
NA_inertia <- wide %>%
  group_by(mplusID) %>%
  group_modify(~ get.ACF(.x, var = "negative") %>% as.data.frame()) %>%
  filter(lag == 1) %>%
  summarise(NA_inertia = acf) %>%
  ungroup() 

#merge the three person-level ACFs and mean granularity
ACF_merged <- list(G_inertia, PA_inertia , NA_inertia) %>%
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
#extract just the two ID columns from wide
id_lookup <- wide[, c("mplusID", "LoginID")]

# keep only unique combinations (one row per person)
id_lookup <- unique(id_lookup)

# merge onto the summary dataframe
ACF_merged <- left_join(ACF_merged, id_lookup, by = "mplusID")

# convert LoginID to character in both
ACF_merged <- ACF_merged %>%
  mutate(LoginID = as.character(LoginID))

Personality <- Personality %>%
  mutate(LoginID = as.character(LoginID))

#merge granularity with big five and ego resilience 
person_level <- left_join(ACF_merged,Personality,by = "LoginID")

#calculating intra-individual standard deviation and mean state granularity
person_level <- person_level %>%
  left_join(
    ICCdata %>%
    group_by(mplusID) %>%
    summarise(
      mean_granularity = mean(granularity, na.rm = TRUE),
      sd_granularity = sd(granularity, na.rm = TRUE)
  ),
  by = "mplusID"
)

#define emotions
emotions <- c("amused", "awe", "content", "glad", "grateful", 
              "hopeful", "inspired", "interested", "love", "proud",
              "angry", "ashamed", "contemptuous", "disgust", "embarrassed", 
              "hate", "repentant", "sad", "scared", "stressed")

#checking missing values
#emotions
wide %>%
  summarise(across(all_of(emotions), ~ sum(is.na(.))))
#state granularity 
sum(is.na(ICCdata$granularity))

#impute missing emotions data with person-level mean 
wide <- wide %>%
  group_by(mplusID) %>%
  mutate(across(all_of(emotions), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# verify imputation
wide %>%
  summarise(across(all_of(emotions), ~ sum(is.na(.))))

#Trait emotional granularity (Hoemann et al., 2020)
#group emotions by positive and negative affect
PAf <- c("amused", "awe", "content", "glad", "grateful", 
              "hopeful", "inspired", "interested", "love", "proud")
NAf <- c("angry", "ashamed", "contemptuous", "disgust", "embarrassed", 
              "hate", "repentant", "sad", "scared", "stressed")

#calculate person-specific ICC separately for positive and negative affect and then average, to avoid negative ICC values
#Take Hoemann's MATLAB code and translate into R (without the F test)
#ICC type: A-k (two-way mixed effect, mean of k measures, absolute agreement)

#function for person-specific positive affect granularity
ICC.positive <- function(person_data){
  ##subset positive affect
  person_data <- person_data[,PAf]
  ##step1:define n (number of measurements) and k (numbers of emotions[raters])
  n <- nrow(person_data)
  k <- ncol(person_data)
  ##step2:variance decomposition
  SStotal <- var(as.vector(as.matrix(person_data))) * (n*k - 1) #total sum of squares (total variance*df)
  MSR <- var(rowMeans(person_data))*k #mean square for rows (main effect of time[between])
  MSC <- var(colMeans(person_data))*n #mean square for columns (main effect of discrete positive emotions[within])
  MSE <- (SStotal - MSR*(n-1) - MSC*(k-1))/((n-1)*(k-1)) #error variance after accounting for the two between-effects (MSR and MSC are converted back to SS to calculate residual sum of squares, then divide by the residual degrees of freedom)
  ##step3:calculating ICC
  ICC.PAf <- (MSR - MSE) / (MSR + (MSC-MSE)/n)
  return(ICC.PAf)
}
#function for person-specific negative affect granularity
ICC.negative <- function(person_data){
  ##subset negative affect
  person_data <- person_data[,NAf]
  ##step1:define n (number of measurements) and k (numbers of emotions[raters])
  n <- nrow(person_data)
  k <- ncol(person_data)
  ##step2:variance decomposition
  SStotal <- var(as.vector(as.matrix(person_data))) * (n*k - 1) #total sum of squares (total variance*df)
  MSR <- var(rowMeans(person_data))*k #mean square for rows (main effect of time[between])
  MSC <- var(colMeans(person_data))*n #mean square for columns (main effect of discrete negative emotions[within])
  MSE <- (SStotal - MSR*(n-1) - MSC*(k-1))/((n-1)*(k-1)) #left-over variance after accounting for the two between-effects (MSR and MSC are converted back to SS to calculate residual sum of squares, then divide by the residual degrees of freedom)
  ##step3:calculating ICC
  ICC.NAf <- (MSR - MSE) / (MSR + (MSC-MSE)/n)
  return(ICC.NAf)
}

#calculate person-specific PA/NA and trait granularity scores using the functions, and merge with the person_level data
person_level <- wide %>%
  group_by(mplusID) %>%
  group_modify(~ data.frame(
    ICC.PAf = ICC.positive(.x),
    ICC.NAf = ICC.negative(.x)
  )) %>%
  mutate(trait.granularity = (ICC.PAf + ICC.NAf) / 2) %>%
  ungroup() %>%
  left_join(person_level, by = "mplusID")

#calculate MSSD for state granularity
person_level <- person_level %>%
  left_join(
    ICCdata %>%
      group_by(mplusID) %>%
      summarise(mssd_granularity = mean(diff(granularity)^2, na.rm = TRUE)),
    by = "mplusID"
  )
# Note: Person-level ICC not computed for state granularity --
# ICC requires k > 1, and any split of random time points would be arbitrary.

#calculate MSSD for positive and negative affect
person_level <- person_level %>%
  left_join(
    wide %>%
      group_by(mplusID) %>%
      summarise(
        mssd_positive = mean(diff(positive)^2, na.rm = TRUE),
        mssd_negative = mean(diff(negative)^2, na.rm = TRUE)
        ),
    by = "mplusID"
  )

#filter rows with missing cases in person_level
person_level <- person_level %>%
  filter(complete.cases(.))

#correlations
# select the variables you want
cor_data <- person_level %>%
  select(-LoginID, -mplusID)
# get pairwise correlations
cor(cor_data, use = "complete.obs")
library(Hmisc)
rcorr(as.matrix(cor_data))

#See if there are stable individual differences in state granularity
#this can potentially inform us whether a state/trait approach is more suitable in studying granularity(?)
#use MLM to estimate sample-level ICC, given all the missing data
model <- lmer(granularity ~ 1 + (1|mplusID), data = ICCdata)
summary(model)
icc(model) #high icc - strong individual differences; low icc - granularity fluctuates within people more than it differs between people(?)
#ICC= .443 (This suggests both state/trait approaches are appropriate?)
r2(model) #conditional: .443, marginal: .000

#Are there any individual differences in state granularity, and in its change over time?
#rescale hours for better model convergence 
ICCdata <- ICCdata %>%
  mutate(hours_scaled = scale(hours))
#model with random intercept and slope
model_time <- lmer(granularity ~ hours_scaled + (hours_scaled | mplusID), data = ICCdata)
summary(model_time)
icc(model_time) #ICC=.468
r2(model_time) #conditional: .469, marginal: .002

#are there meaningful individual differneces in baseline granularity and the effect of time?
#dropping the random intercept 
model0_time <- lmer(granularity ~ hours_scaled + (0+hours_scaled | mplusID), data = ICCdata)
#dropping the random slope
model1_time <- lmer(granularity ~ hours_scaled + (1 | mplusID), data = ICCdata)

#model comparison
anova(model_time,model0_time,model1_time)

#Will's approach
# split time points into two halves per person
ICCdata <- ICCdata %>%
  group_by(mplusID) %>%
  mutate(half = ifelse(row_number() <= n()/2, "first", "second")) %>%
  ungroup()

# compute mean granularity per half per person
split_half <- ICCdata %>%
  group_by(mplusID, half) %>%
  summarise(mean_granularity = mean(granularity, na.rm = TRUE)) %>%
  pivot_wider(names_from = half, values_from = mean_granularity)

# correlate the two halves
cor.test(split_half$first, split_half$second)
#r=.766,p<.001 - state granularity reflects stable individual differences(?)

#multiple regression
#get person-level mean PA and NA from wide
person_level <- person_level %>%
  left_join(
    wide %>%
      group_by(mplusID) %>%
      summarise(
        mean_PA = mean(positive, na.rm = TRUE),
        mean_NA = mean(negative, na.rm = TRUE)
      ),
    by = "mplusID"
  )
#granularity and personality

#Controlling for PA/NA, what is the relationship between mean state granularity and neuroticism
N1 <- lm(NegativeEmotionality ~ mean_granularity + mean_PA + mean_NA, data = person_level)
summary(N1)
#now do the same with trait granularity
N2 <- lm(NegativeEmotionality ~ trait.granularity + mean_PA + mean_NA, data = person_level)
summary(N2)

#now Openness
#mean state granularity
O1 <- lm(OpenMindedness ~ mean_granularity + mean_PA + mean_NA, data = person_level)
summary(O1)
#trait granularity
O2 <- lm(OpenMindedness ~ trait.granularity + mean_PA + mean_NA, data = person_level)
summary(O2)

# ---- Ebras et al. Emotion Differentiation (ED) Calculation ----
# Computes six measures from Erbas et al. / Murphy's emodiff package:
#   c_nonED - classic non-differentiation (person-level, ICC3k of emotions over time)
#   c_ED    - classic ED (inverse of c_nonED)
#   m_nonED - momentary non-differentiation (varies by time point)
#   m_ED    - momentary ED
#   L2_nonED, L2_ED - person-level ED aggregated from momentary measures
#   These should be calculated seperately for positive and negative affect

# Inspect observation counts per participant
n_observations <- wide %>% 
  count(mplusID) %>% 
  arrange(n)
hist(n_observations$n)

# Filter for ED calculation:
# - At least 5 (??) observations per person (needed to estimate within-person variance)
#######this needs to be checked############
# - Nonzero variance on every emotion (otherwise ICC can't be computed)
# Note: this filter applies only to ED analyses; other analyses use full `wide` dataset
ed_input <- wide %>% 
  group_by(mplusID) %>% 
  filter(n() >= 5) %>% 
  ungroup()
# Calculating momenary granularity for PA and NA seperately
calculate_ed_prefixed <- function(dat, emotions, prefix, ...) {
  ed_cols <- c("m_nonED", "m_ED", "c_ED", "c_nonED", "L2_nonED", "L2_ED")
  calculate_ed(dat=dat,emotions=emotions,...) %>%
    rename_with(~ paste0(prefix, "_", .x), .cols = all_of(ed_cols))}
# PA/NA momentary granularity
result_PA <- calculate_ed_prefixed(ed_input, PAf, "PA", mplusID)
result_NA <- calculate_ed_prefixed(ed_input, NAf, "NA", mplusID)

# Create a data frame storing the two versions of momentary granularity 
momentary <- ICCdata %>% 
  select(-half) %>% 
  left_join(select(result_PA, mplusID, hours, PA_m_ED), by = c("mplusID", "hours")) %>% 
  left_join(select(result_NA, mplusID, hours, NA_m_ED), by = c("mplusID", "hours"))
#compute the mean of positive and negative momentary ED
momentary <- momentary %>% 
  mutate(mean_m_ED = (PA_m_ED + NA_m_ED) / 2)

# ---- Plot Momentary ED ----
#select a subset to plot
subset <- subset(momentary, momentary$mplusID < 26)

#Plot lines
ggplot(subset, aes(x = hours, y = mean_m_ED, group = mplusID, color = factor(mplusID))) +
  geom_line(alpha = 0.4) +
  theme_minimal() +
  labs(x = "Hours", y = "Momentary ED", title = "Momentary ED Over Time by Person") +
  theme(legend.position = "none")

#smoothed lines
ggplot(subset, aes(x = hours, y = mean_m_ED, group = mplusID, color = factor(mplusID))) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 0.8) +
  theme_minimal() +
  labs(x = "Hours", y = "Momentary ED", title = "Momentary ED Over Time by Person") +
  theme(legend.position = "none")

#positive momentary ED
ggplot(subset, aes(x = hours, y = PA_m_ED, group = mplusID, color = factor(mplusID))) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 0.8) +
  theme_minimal() +
  labs(x = "Hours", y = "Momentary Positive ED", title = "Momentary Positive ED Over Time by Person") +
  theme(legend.position = "none")

#negative momentary ED
ggplot(subset, aes(x = hours, y = NA_m_ED, group = mplusID, color = factor(mplusID))) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 0.8) +
  theme_minimal() +
  labs(x = "Hours", y = "Momentary Negative ED", title = "Momentary Negative ED Over Time by Person") +
  theme(legend.position = "none")

#correlate the two momentary granularity measures (r=0.05)
library(correlation)
momentary %>% 
  mutate(mplusID = as.factor(mplusID)) %>% 
  correlation(select = c("granularity", "mean_m_ED", "mplusID"),
              multilevel = TRUE,
              include_factors = TRUE)

# ---- individual differences (momentary ED) ----
model2 <- lmer(mean_m_ED ~ 1 + (1|mplusID), data = momentary)
summary(model2)
icc(model2) #high icc - strong individual differences; low icc - granularity fluctuates within people more than it differs between people(?)
#icc=.011 
#no reliable individual differences, the measure is very "state-like"?

#merge classic ED and L2ED with person level data
person_level <- person_level %>% 
  left_join(result_PA %>% 
              select(mplusID, PA_c_ED, PA_L2_ED) %>% 
              distinct(),
            by = "mplusID") %>% 
  left_join(result_NA %>% 
              select(mplusID, NA_c_ED, NA_L2_ED) %>% 
              distinct(),
            by = "mplusID")
#get the mean of positive and negative person_level ED
person_level <- person_level %>% 
  mutate(mean_c_ED = (PA_c_ED + NA_c_ED) / 2) %>% 
  mutate(mean_L2_ED = (PA_L2_ED + NA_L2_ED) / 2)

#correlate mean state granularity (from Lane&Trull) with the three person_level ICCs
person_level %>% 
  select(mean_granularity, trait.granularity, mean_c_ED, mean_L2_ED,PA_c_ED,PA_L2_ED, NA_c_ED,NA_L2_ED) %>% 
  correlation()
