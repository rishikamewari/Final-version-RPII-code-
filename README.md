title: 'RPII: Legal Identity and Roma Populations in North Macedonia'
author: "Noah Plane"
date: "2024-06-12"
output: html_document
---

# Downloading package facilitating the importing of SPSS files into R

```{r}
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven", repos = "http://cran.us.r-project.org")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
}

library(haven)
library(dplyr)
```


# Loading UNICEF MICS North-Macedonia SPSS data form 2018-19 into R all data that is specifically about the Roma population gets the indicator r

```{r}
data_directory <- "~/Desktop/Research Project R Files/RESEARCH-PROJECT-R-FILES/Data/Republic of North Macedonia (Roma Settlements) MICS6 SPSS Datasets"
sav_files <- list.files(data_directory, pattern = "\\.sav$", full.names = TRUE)
datasets <- list()
for (file in sav_files) {
  dataset_name <- paste0(tools::file_path_sans_ext(basename(file)), "r") 
  datasets[[dataset_name]] <- read_sav(file)
}

names(datasets)

data_directory_2 <- "~/Desktop/Research Project R Files/RESEARCH-PROJECT-R-FILES/Data/Republic of North Macedonia MICS6 SPSS Datasets"
sav_files_2 <- list.files(data_directory_2, pattern = "\\.sav$", full.names = TRUE)
for (file in sav_files_2) {
  dataset_name <- tools::file_path_sans_ext(basename(file)) 
  datasets[[dataset_name]] <- read_sav(file)
}
names(datasets)
```




#Checking for if Hl1 needs to be recoded into LN for the hlr dataset

```{r}
names(datasets$hlr)
```

# Recoding Hl1 into LN for the hlr dataset
```{r}
datasets$hlr$LN <- datasets$hlr$HL1
```

# Opening all datasets as a sepreate objects the global environment to check for recoding and cleaning
```{r}
chr <- datasets$chr
hhr <- datasets$hhr
hlr <- datasets$hlr
wmr <- datasets$wmr
bhr <- datasets$bhr
fsr <- datasets$fsr
ch <- datasets$chr
hh <- datasets$hhr
hl <- datasets$hlr
wm <- datasets$wmr
bh <- datasets$bhr
fs <- datasets$fsr
```

# Recoding primary and control variables:

# Recoding dependant variable
# Creating new variable Index_Birth_Registered_R for the dependant variable that mesures birth registration, combining variable BR1 on birth cerfiticates and BR2 on birth registration into one new binary variable Index_Birth_Registered_R that mesures if the child has a birth certificate or is registered with 1 (BR1=1,2 or BR2=1) beeing yes and 0 (BR1=3 or BR2=2) beeing no with 8 and being missing data (BR1=8 or BR2=8) adding this new variable to the chr dataset next to the original variables BR1 and BR2
```{r}
chr <- chr %>%
  mutate(
    Index_Birth_Registered_R = case_when(
      BR1 %in% c(1, 2) | BR2 == 1 ~ 1,
      BR1 == 3 | BR2 == 2 ~ 0,
      BR1 %in% c(8, NA) | BR2 %in% c(8, NA) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Index_Birth_Registered_R, .after = BR2) 

```

# Recoding independent variable 
# Creating new variable Index_Disabled_Child_R for the independent variablet hat indicates if a child is disabled (1) or not (0) 
# based on a combination of variables related to disabilities (UCF3, UCF4, UCF7, UCF9, UCF11, UCF12, UCF13-UCF18)
# The variable is assigned 1 if any of the disability-related variables indicate disability
# The variable is assigned 0 if none of the disability-related variables indicate disability
# AND at least one variable indicates non-disability
# If all relevant variables are missing, the variable is assigned NA

#binary variable Index_Disabled_Child, where 1 is disabled, 0 is not disabled, and the rest is coded as missing.
```{r}
chr <- chr %>%
  mutate(
    Index_Disabled_Child_R = case_when(
      UCF3 == 1 | UCF4 == 1 | UCF7 %in% c(2, 3, 4) | UCF9 %in% c(2, 3, 4) |
        UCF11 %in% c(2, 3, 4) | UCF12 %in% c(1, 2, 3, 4) |
        UCF13 %in% c(3, 4) | UCF14 %in% c(3, 4) | UCF15 %in% c(3, 4) |
        UCF16 %in% c(3, 4) | UCF17 %in% c(3, 4) | UCF18 %in% c(3, 4) ~ 1,
      (UCF3 == 2 | UCF4 == 2 | UCF7 == 1 | UCF9 == 1 |
        UCF12 == 0 | UCF13 %in% c(1, 2) | UCF14 %in% c(1, 2) |
        UCF15 %in% c(1, 2) | UCF16 %in% c(1, 2) | UCF17 %in% c(1, 2) |
        UCF18 %in% c(1, 2)) &
        !(UCF3 == 1 | UCF4 == 1 | UCF7 %in% c(2, 3, 4) | UCF9 %in% c(2, 3, 4) |
          UCF11 %in% c(2, 3, 4) | UCF12 %in% c(1, 2, 3, 4) |
          UCF13 %in% c(3, 4) | UCF14 %in% c(3, 4) | UCF15 %in% c(3, 4) |
          UCF16 %in% c(3, 4) | UCF17 %in% c(3, 4) | UCF18 %in% c(3, 4)) ~ 0,
      is.na(UCF3) & is.na(UCF4) & is.na(UCF7) & is.na(UCF9) &
        is.na(UCF11) & is.na(UCF12) & is.na(UCF13) & is.na(UCF14) &
        is.na(UCF15) & is.na(UCF16) & is.na(UCF17) & is.na(UCF18) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Index_Disabled_Child_R, .after = UCF18)
```



```{r}
# Recoding independent variable/moderator 
# Creating new variable Index_Mother_Disabled_R for the independent variable that indicates if the mother is disabled (1) or not (0)
# based on a combination of variables related to disabilities ((AF2, AF3, AF6, AF8, AF9, AF10, AF11, AF12)
# The variable is assigned 1 if any of the disability-related variables indicate disability
# The variable is assigned 0 if none of the disability-related variables indicate disability
# AND at least one variable indicates non-disability
# If all relevant variables are missing, the variable is assigned NA

wmr <- wmr %>%
  mutate(
    Index_Mother_Disabled_R = case_when(
      # Assign 1 (disabled) if any of the variables indicate disability
      AF2 == 1 | AF3 == 1 | AF6 %in% c(2, 3, 4) | AF8 %in% c(2, 3, 4) |
        AF9 %in% c(2, 3, 4) | AF10 %in% c(2, 3, 4) |
        AF11 %in% c(2, 3, 4) | AF12 %in% c(2, 3) ~ 1,
      (AF2 == 2 | AF3 == 2 | AF6 == 1 | AF8 == 1 |
        AF9 == 1 | AF10 == 1 | AF11 == 1 | AF12 == 1) &
        !(AF2 == 1 | AF3 == 1 | AF6 %in% c(2, 3, 4) | AF8 %in% c(2, 3, 4) |
          AF9 %in% c(2, 3, 4) | AF10 %in% c(2, 3, 4) |
          AF11 %in% c(2, 3, 4) | AF12 %in% c(2, 3)) ~ 0,
      is.na(AF2) & is.na(AF3) & is.na(AF6) & is.na(AF8) &
        is.na(AF9) & is.na(AF10) & is.na(AF11) & is.na(AF12) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Index_Mother_Disabled_R, .after = AF12)
```


#Sort datasets by key variables HH1, HH2, LN

```{r}
datasets$wmr <- datasets$wmr %>% arrange(HH1, HH2, LN)
datasets$chr <- datasets$chr %>% arrange(HH1, HH2, LN)
```

#Merge data sets by key variables HH1, HH2, LN
# Merge the wmr (women) and chr (children under 5) datasets
# The relationship between wmr and chr is one-to-many, where one woman can be the mother/caretaker of multiple children
# Key variables used for merging:
#   - HH1 (cluster number)
#   - HH2 (household number)
#   - LN (line number of woman in wmr dataset)
#   - UF4 (line number of mother/caretaker in chr dataset)
# The chr and wmr datasets where ordered by the key variables before merging and the variable UF4 was renamed to LN in the wmr dataset to facilitate the merge 
```{r}
wmr <- wmr %>% rename(UF4 = LN)
chr <- chr[order(chr$HH1, chr$HH2, chr$UF4), ]
wmr <- wmr[order(wmr$HH1, wmr$HH2, wmr$UF4), ]
merged_data <- merge(chr, wmr, by = c("HH1", "HH2", "UF4"), all.x = TRUE)
```



# Placing the new index variables at the beginning of the dataset 
```{r}
library(dplyr)
merged_data <- merged_data %>%select(HH1, HH2, UF4, Index_Birth_Registered_R, Index_Disabled_Child_R, Index_Mother_Disabled_R)
```


# Save merged dataset for better sharing and in the case of crash
```{r}
write.csv(merged_data, "merged_data.csv", row.names = FALSE)
```


# Start of analysis
# Summary statistics of the merged dataset:
```{r}
summary(merged_data)
```
#More summary statistics of the merged dataset:
```{r}

# addding code to show the number of missing values for each variable:
colSums(is.na(merged_data))

```

#Frequency distribution of birth registration:
```{r}
library(ggplot2)


unique(merged_data$Index_Birth_Registered_R)


plot_data <- merged_data %>%
  mutate(Index_Birth_Registered_R_Factor = factor(Index_Birth_Registered_R, levels = c(0, 1), labels = c("Not Registered", "Registered")))


ggplot(plot_data, aes(x = Index_Birth_Registered_R_Factor)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")), vjust = 1.5, size = 5, color = "black") +
  labs(title = "Frequency Distribution of Birth Registration",
       x = "Birth Registration Status",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

```



#Frequency distribution of child disability
# bars in chart also show the number of children with disabilities and without disabilities:
```{r}
library(ggplot2)

# Check unique values of Index_Disabled_Child_R
unique(merged_data$Index_Disabled_Child_R)

# Create a temporary factor variable for plotting, handling NA separately
plot_data <- merged_data %>%
  mutate(Index_Disabled_Child_R_Factor = factor(Index_Disabled_Child_R, levels = c(0, 1), labels = c("Not Disabled", "Disabled")))

# Create the plot with count and percentage labels
ggplot(plot_data, aes(x = Index_Disabled_Child_R_Factor)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")), vjust = 1.5, size = 5, color = "black") +
  labs(title = "Frequency Distribution of Child Disability",
       x = "Child Disability Status",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

```

#Frequency distribution of mother disability:
```{r}
library(ggplot2)


unique(merged_data$Index_Mother_Disabled_R)


plot_data <- merged_data %>%
  mutate(Index_Mother_Disabled_R_Factor = factor(Index_Mother_Disabled_R, levels = c(0, 1), labels = c("Not Disabled", "Disabled")))

ggplot(plot_data, aes(x = Index_Mother_Disabled_R_Factor)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")), vjust = 1.5, size = 5, color = "black") +
  labs(title = "Frequency Distribution of Mother Disability",
       x = "Mother Disability Status",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
```



```{r}
#Cross-tabulation of birth registration and child disability:
table(merged_data$Index_Birth_Registered_R, merged_data$Index_Disabled_Child_R, dnn = c("Birth Registration", "Child Disability"))

```

#Cross-tabulation of birth registration and mother disability:
```{r}
table(merged_data$Index_Birth_Registered_R, merged_data$Index_Mother_Disabled_R, dnn = c("Birth Registration", "Mother Disability"))
```

#Cross-tabulation of child disability and mother disability:
```{r}
table(merged_data$Index_Disabled_Child_R, merged_data$Index_Mother_Disabled_R, dnn = c("Child Disability", "Mother Disability"))

```


#creating control variables for the logistic regression model
# Recoding control variable for religion of the head of the household to determine whether the household follows any religion or follows atheism

# creating new variable HouseHead_Religiosity_R for the control variable

# Based on the values on HC1A, the variable is assigned 0, if the response is =7 (7 represenets no religion)

# Based on HC1A, the variable is assigned 1, if the resposnse is= 1,2,3,4 0r 6 (each resposne corresponds affiliation to a certain religion)

# for any other values, NA is alloted to the variable
```{r}
library(dplyr)


hhr <- hhr %>%
  mutate(
    HouseHead_Religiosity_R = case_when(
      HC1A %in% c(1, 2, 3, 4, 6) ~ 1, 
      HC1A == 7 ~ 0,                 
      TRUE ~ NA_real_               
    )
  ) %>%
  relocate(HouseHead_Religiosity_R, .after = HC1B) # Move the new variable to be right after HC1B

```


#creating other control variables for the logistic regression model:
```{r}
library(dplyr)
# Creating control variable that measure of the birth of the child was wanted by the mother, when variable DB2 is 1, the birth was wanted, when it is 2, the birth was not wanted, for the new variable Birth_of_Child_not_wanted , 1 is not-wanted(DB2 = 2) and 0 is wanted (DB2 = 1), else = NA.
wmr <- wmr %>%
  mutate( 
    Birth_of_Child_not_wanted = case_when(
      DB2 == 2 ~ 1,
      DB2 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Birth_of_Child_not_wanted, .after = DB2) # Move the new variable to be right after DB2
```


```{r}
# Creating dummy variables based on the given subcategories
wmr <- wmr %>%
  mutate(
    Disability_Dummy_Ref = case_when(
      VT22F == 1 ~ 1,
      VT22F == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    Ethnic_Immigration_Dummy = case_when(
      VT22A == 1 ~ 1,
      VT22A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    Sex_Dummy = case_when(
      VT22B == 1 ~ 1,
      VT22B == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    Sexual_Orientation_Dummy = case_when(
      VT22C == 1 ~ 1,
      VT22C == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    Age_Dummy = case_when(
      VT22D == 1 ~ 1,
      VT22D == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    Religion_Belief_Dummy = case_when(
      VT22E == 1 ~ 1,
      VT22E == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    Other_Reason_Dummy = case_when(
      VT22X == 1 ~ 1,
      VT22X == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Ethnic_Immigration_Dummy, .after = VT22X) %>%
  relocate(Sex_Dummy, .after = Ethnic_Immigration_Dummy) %>%
  relocate(Sexual_Orientation_Dummy, .after = Sex_Dummy) %>%
  relocate(Age_Dummy, .after = Sexual_Orientation_Dummy) %>%
  relocate(Religion_Belief_Dummy, .after = Age_Dummy) %>%
  relocate(Other_Reason_Dummy, .after = Religion_Belief_Dummy)

```


```{r}
# Creating a new variable for education level among mothers
wmr <- wmr %>%
  mutate(
    level_of_education_mother_r = case_when(
      CM1 == 1 & WB6A %in% c(3, 4, 5, 6) ~ 1, # If CM1=1 and WB6A is 3, 4, 5, 6, then educated (1)
      CM1 == 1 & WB6A %in% c(1, 2, 000) ~ 0,  # If CM1=1 and WB6A is 1, 2, or 000, then not educated (0)
      CM1 == 2 ~ NA_real_,                   # If CM1=2, then NA (not a mother)
      TRUE ~ NA_real_                        # Any other case, assign NA
    )
  ) %>%
  relocate(level_of_education_mother_r, .after = WB6A) # Relocate the new variable after WB6A
```


```{r}
# Creating a new variable for health insurance status among mothers
wmr <- wmr %>%
  mutate(
    Mother_Health_Insurance = case_when(
      CM1 == 1 & WB18 == 1 ~ 1,  # If CM1 is 1 (mother) and WB18 is 1 (has health insurance), set to 1 (insured)
      CM1 == 1 & WB18 == 2 ~ 0,  # If CM1 is 1 (mother) and WB18 is 2 (no health insurance), set to 0 (not insured)
      CM1 == 2 ~ NA_real_,       # If CM1 is 2 (not a mother), set to NA (not applicable)
      TRUE ~ NA_real_            # For any other case, set to NA
    )
  ) %>%
  relocate(Mother_Health_Insurance, .after = WB18) # Relocate the new variable after WB18
```

```{r}
hhr <- hhr %>%
  rename(ST3_1 = `ST3$1`) %>%
  rename(ST3_2 = `ST3$2`) %>%
  rename(ST3_3 = `ST3$3`) %>%
  rename(ST3_4 = `ST3$4`) %>%
  rename(ST3_5 = `ST3$5`) %>%
  rename(ST3_6 = `ST3$6`)

# Create the new variable for family economic status based on social assistance responses
hhr <- hhr %>%
  mutate(
    # Calculate the number of 'yes' responses for each family
    num_assistance_yes = (ST3_1 == 1) + (ST3_2 == 1) + (ST3_3 == 1) + 
                         (ST3_4 == 1) + (ST3_5 == 1) + (ST3_6 == 1),
    
    # Classify the families based on the number of 'yes' responses
    family_economic_status = case_when(
      # Poor: Yes to 3 or more programs
      num_assistance_yes >= 3 ~ 0,
      
      # Average: Yes to 1 or 2 programs
      num_assistance_yes >= 1 & num_assistance_yes <= 2 ~ 1,
      
      # Rich: No to all programs
      num_assistance_yes == 0 ~ 2,
      
      # Any other case should be treated as NA (not applicable here, but added for completeness)
      TRUE ~ NA_real_
    )
  ) %>%
  # Remove the temporary num_assistance_yes column
  select(-num_assistance_yes) %>%
  # Relocate the new variable after the last ST3 column
  relocate(family_economic_status, .after = ST3_6)


```


```{r}
# Check the column names in wmr
names(wmr)

# Check the column names in chr
names(chr)
```





#Adding control variables to the merged dataset
```{r}
# Add control variables from wmr to merged_data
control_vars_wmr <- wmr %>%
  select(HH1, HH2, Birth_of_Child_not_wanted, Disability_Dummy_Ref, Religion_Belief_Dummy, Ethnic_Immigration_Dummy, Sex_Dummy, 
         Sexual_Orientation_Dummy, Age_Dummy, , 
         Other_Reason_Dummy, level_of_education_mother_r, Mother_Health_Insurance)
```


```{r}
# Check the columns in merged_data
str(merged_data)

# Check the columns in control_vars_wmr
str(control_vars_wmr)
```


```{r}
# Merge control variables from wmr into merged_data
merged_data <- merge(merged_data, control_vars_wmr, by = c("HH1", "HH2"), all.x = TRUE)

```


```{r}
# Add control variables from hhr to merged_data
control_vars_hhr <- hhr %>%
  select(HH1, HH2, HouseHead_Religiosity_R, family_economic_status)

# Check the columns in control_vars_hhr
str(control_vars_hhr)
```


```{r}
# Merge control variables from hhr into merged_data
merged_data <- merge(merged_data, control_vars_hhr, by = c("HH1", "HH2"), all.x = TRUE)

# Save merged dataset for better sharing and in the case of crash
write.csv(merged_data, "merged_data_with_controls.csv", row.names = FALSE)

```



# Running four differen logistic regression models to test the relationship between birth registration and child disability and mother disability
```{r}
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(effects)
library(interactions)

# Load the dataset
merged_data <- read.csv("merged_data_with_controls.csv")


# Model 1: Logistic regression of birth registration on child disability
model1 <- glm(Index_Birth_Registered_R ~ Index_Disabled_Child_R, data = merged_data, family = binomial)
summary(model1)
```

```{r}
# Model 2 tests primary relationship between birth registration and child disability with an interaction term

model2 <- glm(Index_Birth_Registered_R ~ Index_Disabled_Child_R * Index_Mother_Disabled_R, data = merged_data, family = binomial)
summary(model2)
```



```{r}

# Running the second model with interaction term and control variables
model3 <- glm(
  Index_Birth_Registered_R ~ Index_Disabled_Child_R * Index_Mother_Disabled_R +
    HouseHead_Religiosity_R + Birth_of_Child_not_wanted + Ethnic_Immigration_Dummy + 
    Sex_Dummy + Sexual_Orientation_Dummy + Age_Dummy + Religion_Belief_Dummy + 
    Other_Reason_Dummy + level_of_education_mother_r + Mother_Health_Insurance + 
    family_economic_status,
  data = merged_data, family = binomial
)

# Summary of the model
summary(model3)
```

```{r}
# Model 4 logistic regression model with all control variables and without interaction term
model4 <- glm(
  Index_Birth_Registered_R ~ Index_Disabled_Child_R + Index_Mother_Disabled_R +
    HouseHead_Religiosity_R + Birth_of_Child_not_wanted + Ethnic_Immigration_Dummy + 
    Sex_Dummy + Sexual_Orientation_Dummy + Age_Dummy + Religion_Belief_Dummy + 
    Other_Reason_Dummy + level_of_education_mother_r + Mother_Health_Insurance + 
    family_economic_status,
  data = merged_data, family = binomial
)

# Summary of the model
summary(model4)
```

```{r}
# Odds ratios for the model without interaction term
exp(coef(model4))
```

```{r}
# Odds ratios for the model with interaction term
exp(coef(model3))
```

```{r}
# Odds ratios for the model with interaction term
exp(coef(model2))
```

```{r}
# Odds ratios for the model without interaction term
exp(coef(model1))
```


# creating visualizations for the logistic regression models
```{r}
# Create a function to plot the odds ratios with confidence intervals
plot_odds_ratios <- function(model, title) {
  odds_ratios <- exp(coef(model))
  conf_intervals <- exp(confint(model))
  conf_intervals <- conf_intervals[, "2.5 %", drop = FALSE]
  conf_intervals <- cbind(odds_ratios, conf_intervals)
  conf_intervals <- as.data.frame(conf_intervals)
  conf_intervals$variable <- rownames(conf_intervals)
  colnames(conf_intervals) <- c("Odds Ratio", "Lower CI", "Variable")
  
  ggplot(conf_intervals, aes(x = Variable, y = `Odds Ratio`, ymin = `Lower CI`, ymax = `Odds Ratio`)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Odds Ratio") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}

# Plot the odds ratios for model 1
plot_odds_ratios(model1, "Odds Ratios for Model 1")

```


```{r}
# Plot the odds ratios for model 2
plot_odds_ratios(model2, "Odds Ratios for Model 2")
```

# other usful statistical tests 
```{r}
# Chi-square test of independence between birth registration and child disability
chisq.test(table(merged_data$Index_Birth_Registered_R, merged_data$Index_Disabled_Child_R))
```

```{r}
# Chi-square test of independence between birth registration and mother disability
chisq.test(table(merged_data$Index_Birth_Registered_R, merged_data$Index_Mother_Disabled_R))
```

```{r}
# Chi-square test of independence between child disability and mother disability
chisq.test(table(merged_data$Index_Disabled_Child_R, merged_data$Index_Mother_Disabled_R))
```


```{r}
# Chi-square test of independence between birth registration and mother disability
chisq.test(table(merged_data$Index_Birth_Registered_R, merged_data$Index_Mother_Disabled_R))
```
# End of analysis

variables <- c("Index_Birth_Registered_R", "Index_Disabled_Child_R", "Index_Mother_Disabled_R")

# Initialize an empty data frame to store results
central_tendency <- data.frame(Variable = character(), Mean = numeric(), Median = numeric(), stringsAsFactors = FALSE)

# Loop through each variable and calculate mean, median, and mode
for (var in variables) {
  mean_val <- mean(merged_data[[var]], na.rm = TRUE)
  median_val <- median(merged_data[[var]], na.rm = TRUE)
  
  # Add results to the data frame
  central_tendency <- rbind(central_tendency, data.frame(Variable = var, Mean = mean_val, Median = median_val,stringsAsFactors = FALSE))
}

# Print the results
print(central_tendency)


# Load the libraries
library(ggplot2)
library(gridExtra)

# Load your data


# Ensure the column names match your actual data
# Plot distribution for Index_Birth_Registered_R
plot1 <- ggplot(data, aes(x = as.factor(Index_Birth_Registered_R))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Birth Registered", x = "Value", y = "Count") +
  theme_minimal()

# Plot distribution for Index_Disabled_Child_R
plot2 <- ggplot(data, aes(x = as.factor(Index_Disabled_Child_R))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Disabled Child", x = "Value", y = "Count") +
  theme_minimal()

# Plot distribution for Index_Mother_Disabled_R
plot3 <- ggplot(data, aes(x = as.factor(Index_Mother_Disabled_R))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Mother Disabled", x = "Value", y = "Count") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, ncol = 3)

# Save the grid of plots as one image
g <- arrangeGrob(plot1, plot2, plot3, ncol = 3)
ggsave("plot_grid.png", g, width = 18, height = 6)

# Create a summary table
create_summary_table <- function(model, model_name) {
  tidy(model) %>%
    mutate(model = model_name) %>%
    select(model, term, estimate, std.error, statistic, p.value)
}

# Generate summary tables for each model
summary_table_model1 <- create_summary_table(model1, "Model 1")
summary_table_model2 <- create_summary_table(model2, "Model 2")
summary_table_model3 <- create_summary_table(model3, "Model 3")
summary_table_model4 <- create_summary_table(model4, "Model 4")

# Combine all summary tables
summary_table <- bind_rows(summary_table_model1, summary_table_model2, summary_table_model3, summary_table_model4)

# Display the summary table
print(summary_table)

# Install and load the car package
install.packages("car")
library(car)

# Load necessary packages
library(car)

# Fit the models as per your data

log_likelihood <- logLik(model1) 
print(log_likelihood)

log_likelihood <- logLik(model2) 
print(log_likelihood)

log_likelihood <- logLik(model3) 
print(log_likelihood)

log_likelihood <- logLik(model4) 
print(log_likelihood)

#Function to calculate McFadden's pseudo R^2
calculate_pseudo_r2 <- function(model) {
  log_lik_model <- logLik(model)
  log_lik_null <- logLik(update(model, . ~ 1))
  pseudo_r2 <- 1 - (log_lik_model / log_lik_null)
  return(as.numeric(pseudo_r2))
}

# Calculate McFadden's pseudo R^2 for each model
pseudo_r2_model1 <- calculate_pseudo_r2(model1)
pseudo_r2_model2 <- calculate_pseudo_r2(model2)
pseudo_r2_model3 <- calculate_pseudo_r2(model3)
pseudo_r2_model4 <- calculate_pseudo_r2(model4)


# Display the results
cat("McFadden's Pseudo R^2 for model1:", pseudo_r2_model1, "\n")
cat("McFadden's Pseudo R^2 for model2:", pseudo_r2_model2, "\n")
cat("McFadden's Pseudo R^2 for model3:", pseudo_r2_model3, "\n")
cat("McFadden's Pseudo R^2 for model4:", pseudo_r2_model4, "\n")

#calculating number of obs
nobs_model1 <- nobs(model1)
nobs_model2 <- nobs(model2)
nobs_model3 <- nobs(model3)
nobs_model4 <- nobs(model4)

#printing number of obs
cat("Model 1: Number of observations =", nobs_model1, "\n")
cat("Model 2: Number of observations =", nobs_model2, "\n")
cat("Model 3: Number of observations =", nobs_model3, "\n")
cat("Model 4: Number of observations =", nobs_model4, "\n")

pearson_r <- cor(data$Index_Disabled_Child_R, data$Index_Mother_Disabled_R)

# Print the result
cat("Pearson's R between Index_Disabled_Child_R and Index_Mother_Disabled_R:", pearson_r, "\n")
