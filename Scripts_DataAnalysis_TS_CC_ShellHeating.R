library(readxl)
rawdata <- read_excel("TS_CC_RData_ShellHeating.xlsx")
str(rawdata)

# Variables as factors and taking the mean of the 2 technical measurements
library(tidyverse)
library(zoo) # Roll mean
datamean <- rawdata %>% 
  mutate(shell_temp = rollmean(shell_temp, k = 2, fill = NA)) %>% 
  filter(row_number() %% 2 != 0, !is.na(shell_temp)) %>% 
  dplyr::select(-tech)

# Data arrangement and overview
data <- datamean %>% 
  mutate(ria = factor(case_when(ria == "1" ~ "Arousa",
                                ria == "2" ~ "Pontevedra")),
         pop = factor(case_when(pop == "1" ~ "Allopatric",
                                pop == "2" ~ "Sympatric")),
         col = factor(case_when(col == "1" ~ "fulva",
                                col == "2" ~ "lineata"),
                      levels = c("lineata", "fulva")),
         temp = factor(case_when(temp == "1" ~ "35",
                                 temp == "2" ~ "40",
                                 temp == "3" ~ "45",
                                 temp == "4" ~ "50")),
         snail = factor(case_when(snail == "0" ~ "Empty",
                                  snail == "1" ~ "Alive"))) %>% 
  mutate(col_pop = factor(interaction(col, pop, sep = "_"), # Interaction variable for plots
                          levels = c("lineata_Allopatric",
                                     "lineata_Sympatric",
                                     "fulva_Sympatric",
                                     "fulva_Allopatric")))

# Alive Analysis ----------------------------------------------------------
# Living snails
alive_data <- data %>% filter(snail == "Alive")
# Boxplots of shell temperature by experimental temperature and color morph from each population type and ria
alive_data %>%
  ggplot(aes(x = temp, 
             y = shell_temp,
             fill = col_pop)) + # By color and  
  geom_violin(linetype = "blank", # Violin plot
              position = position_dodge(0.9),
              scale = "width") +
  geom_boxplot(alpha = 0, # Boxplot
               width = 0.5,
               position = position_dodge(0.9)) +
  facet_grid(. ~ ria) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "Shell Temperature (⁰C)",
       fill = "Morph",
       pattern = "Morph") +
  scale_fill_manual(values = c("lineata_Allopatric" = "#7b7869",
                               "lineata_Sympatric" = "#bcb7a1",
                               "fulva_Sympatric" = "#d1cbac",
                               "fulva_Allopatric" = "#fffbe7")) +
  theme_classic() +
  theme(legend.position = "top")

# Alive snails groups
sympatric_data <- alive_data %>% filter(pop == "Sympatric") # Sympatric populations from both Rias
arousa_data <- alive_data %>% filter(ria == "Arousa") # Ria de Arousa, with sympatric and allopatric populations
pontevedra_data <- alive_data %>% filter(ria == "Pontevedra") # Ria de Pontevedra, with a single sympatric population

# ANOVA of shell temperature over color, experimental temperature and ria within sympatric populations
# Assumptions
library(car) # Levene test
sympatric_data %>%
  group_by(col, temp, ria) %>%
  summarise(sw_p_value = shapiro.test(shell_temp)$p.value)
leveneTest(shell_temp ~ col * temp * ria, data = sympatric_data)
# OUT: normal distribution except for 3 groups, but not homoscedasticity found

# Assumptions using logarithmic transformation on dependent variable
sympatric_data %>%
  group_by(col, temp, ria) %>%
  summarise(sw_p_value = shapiro.test(log(shell_temp))$p.value)
leveneTest(log(shell_temp) ~ col * temp * ria, data = sympatric_data)
# OUT: normal distribution except for 4 groups and homoscedasticity

# ANOVA model; Supplementary Table 5
aov(log(shell_temp) ~ col * temp * ria, data = sympatric_data) %>% summary
# OUT: no effect of shell color, but experimental temperature and its interaction with ria

# Plot alive Arousa; not included
arousa_data %>%
  ggplot(aes(x = temp,
             y = shell_temp,
             fill = col)) + 
  geom_violin(linetype = "blank",
              position = position_dodge(0.8),
              scale = "width") +
  geom_boxplot(alpha = 0,
               width = 0.5,
               position = position_dodge(0.8),) +
  facet_grid(. ~ pop) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "Shell Temperature (⁰C)",
       fill = "Morph",
       pattern = "Morph") +
  scale_fill_manual(values = c("lineata" = "#7b7869",
                               "fulva" = "#fffbe7")) +
  theme_classic()

# ANOVA of shell temperature over color, experimental temperature and ria
# Assumptions
arousa_data %>%
  group_by(col, temp, pop) %>%
  summarise(sw_p_value = shapiro.test(shell_temp)$p.value)
leveneTest(shell_temp ~ col * temp * pop, data = arousa_data)
# OUT: normal distribution except for 2 groups and homoscedasticity

# ANOVA model; Supplementary Table 6
aov(shell_temp ~ col * temp * pop, data = arousa_data) %>% summary
# OUT: no effect of color per se, only its interaction with population type


# Recovery Analysis -------------------------------------------------------
# Plot recovery; Figure 5
alive_data %>%
  group_by(col, temp, pop, col_pop, ria) %>% 
  summarise(recovery = mean(recovery == 1) * 100) %>% 
  ggplot(aes(x = temp,
             y = recovery,
             fill = col_pop)) +
  geom_col(color = "#333333",
           position = position_dodge()) +
  facet_grid(. ~ ria) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "% Recovery",
       fill = "Morph",
       pattern = "Morph") +
  scale_fill_manual(values = c("lineata_Allopatric" = "#555349",
                               "lineata_Sympatric" = "#bcb7a1",
                               "fulva_Sympatric" = "#d1cbac",
                               "fulva_Allopatric" = "#fffbe7")) +
  theme_classic() +
  theme(legend.position = "top")


# Function for Fisher's exact test; recovery ~ color
results <- list()
FTest_color <- function(data) {
  for (temp in c("35", "40", "45", "50")){
    obs <- data %>% # Observed frequencies
      filter(temp == !!temp) %>%
      group_by(col) %>% 
      summarise(alive = sum(recovery == 1),
                dead = sum(recovery == 0)) %>% 
      {matrix(c(.$alive, .$dead), nrow = 2, byrow = TRUE)}
    results[[temp]] <- list(obs %>% fisher.test) # Fisher exact test performance
  }
  return(results)
}

sympatric_data %>% FTest_color
# OUTPUT: fulva morph showed significantly higher survival rates at 35 and 40 C considering both Rias

arousa_data %>% FTest_color
# OUTPUT: no differences between color morphs at any temperature in the Ria de Arousa

arousa_data %>% filter(pop == "Sympatric") %>% FTest_color
# OUTPUT: no differences between sympatric color morphs at any temperature in the Ria de Arousa

arousa_data %>% filter(pop == "Allopatric") %>% FTest_color
# OUTPUT: no differences between allopatric color morphs at any temperature in the Ria de Arousa

# Fisher's exact tests for each case in the Ria de Pontevedra (sympatric populations only)
pontevedra_data %>% FTest_color
# OUTPUT: no differences between allopatric color morphs at any temperature in the Ria de Arousa


# Binary logistic regression of shell temperature over all the variables in sympatric populations; Supplementary Table 7
library(pscl)
glm(recovery ~ col * temp * ria, data = sympatric_data,
    family = "binomial") %>% 
  {list(summary(.), pR2(.))} # 0.6008 McFadden's pseudo R2

# Binary logistic regression of shell temperature over all the variables on Arousa data; Supplementary Table 8
glm(recovery ~ col * temp * pop, data = arousa_data,
    family = "binomial") %>%  
  {list(summary(.), pR2(.))} # 0.6144 McFadden's pseudo R2



# Function for Fisher's exact test; recovery ~ population type 
FTest_pop <- function(data) {
  for (temp in c("35", "40", "45", "50")){
    obs <- data %>% # Observed frequencies
      filter(temp == !!temp) %>%
      group_by(pop) %>% 
      summarise(alive = sum(recovery == 1),
                dead = sum(recovery == 0)) %>% 
      {matrix(c(.$alive, .$dead), nrow = 2, byrow = TRUE)}
    results[[temp]] <- list(obs %>% fisher.test) # Fisher exact test performance
  }
  return(results)
}

arousa_data %>% filter(col == "fulva") %>% FTest_pop
# OUTPUT: no differences between sympatric vs allopatric fulva morph at any temperature in the Ria de Arousa

arousa_data %>% filter(col == "lineata") %>% FTest_pop
# OUTPUT: no differences between sympatric vs allopatric lineata morph  at any temperature in the Ria de Arousa

# Empty data set ----------------------------------------------------------
empty_data <- data %>% filter(snail == "Empty")

## Empty within sympatric populations
emptysym_data <- empty_data %>% filter(pop == "Sympatric")

# Plot
emptysym_data %>% 
  ggplot(aes(x = temp,
             y = shell_temp,
             fill = col)) +
  geom_violin(linetype = "blank",
              position = position_dodge(0.8),
              scale = "width") +
  geom_boxplot(alpha = 0,
               width = 0.4,
               position = position_dodge(0.8)) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "Shell Temperature (⁰C)",
       fill = "Snail") +
  scale_fill_manual(values = c("#e4ffed", "#ffd7c6")) +
  theme_classic()

# LR shell temperature over all the variables on alivempty data
emptysym_data %>% lm(shell_temp ~ col * temp * ria , .) %>% 
  {list(shapiro.test(residuals(.)), ncvTest(.))}
# OUT: normal distribution of residuals, but not homoscedasticity found

# Log-LR model for shell temperature over all the variables on alivempty data
emptysym_data %>% lm(log(shell_temp) ~ col * temp * ria, .) %>% 
  {list(shapiro.test(residuals(.)), ncvTest(.))}
# OUT: normal distribution of residuals and homoscedasticity

# Log-LR model; Supplementary Table 9
emptysym_data %>% lm(log(shell_temp) ~ col * temp * ria, .) %>% summary
# OUT: significance on body presence overall and when reaching 50 Celisius



# ANOVA of shell temperature over color and experimental temperature
# Assumptions
emptysym_data %>%
  group_by(col, temp) %>%
  summarise(sw_p_value = shapiro.test(shell_temp)$p.value)
leveneTest(shell_temp ~ col * temp, data = emptysym_data)
# OUT: normal distribution in all cases, but not homoscedasticity found

# Assumptions using logarithmic transformation on dependent variable
emptysym_data %>%
  group_by(col, temp) %>%
  summarise(sw_p_value = shapiro.test(log(shell_temp))$p.value)
leveneTest(log(shell_temp) ~ col * temp, data = emptysym_data)
# OUT: normal distribution in all cases and homoscedasticity

# ANOVA model; Supplementary Table 10
aov(log(shell_temp) ~ col * temp, data = emptysym_data) %>% summary
# OUT: significance on body presence, experimental temperature and their interaction

# Assumptions using logarithmic transformation on dependant variable
emptyarousa_data <- empty_data %>% filter(ria == "Arousa")
emptyarousa_data %>%  
  group_by(col, temp, pop) %>%
  summarise(sw_p_value = shapiro.test(shell_temp)$p.value)
leveneTest(shell_temp ~ col * temp * pop, data = emptyarousa_data)
# OUT: normal distribution except for one group and homoscedasticity

# ANOVA model; Supplementary Table 11
aov(shell_temp ~ col * temp * pop, data = emptyarousa_data) %>% summary
# OUT: significance on experimental temperature and treatment
