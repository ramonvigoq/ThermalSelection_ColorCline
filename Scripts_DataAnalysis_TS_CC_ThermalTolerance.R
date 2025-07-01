setwd("")# Working directory with the raw data

library(readxl)
rawdata <- read_excel("TS_CC_RData_ThermalTolerance.xlsx")
str(rawdata)

# Data arrangement and overview
library(tidyverse)
data <- rawdata %>% 
  mutate(temp = case_when(temp == 1 ~ 41,
                          temp == 2 ~ 42,
                          temp == 3 ~ 43,
                          temp == 4 ~ 44,
                          temp == 5 ~ 45,
                          temp == 6 ~ 46,
                          temp == 7 ~ 47),
         col = factor(case_when(col == "1" ~ "fulva",
                                col == "2" ~ "lineata"),
                        levels = c("lineata", "fulva")),
         treat = factor(case_when(treat == "1" ~ "Chronic",
                                  treat == "2" ~ "Acute"),
                        levels = c("Acute", "Chronic")),
         sex = factor(case_when(sex == "1" ~ "Male",
                                sex == "2" ~ "Female"),
                      levels = c("Male", "Female")))
str(data)

# Binary logistic regression models ---------------------------------------
# Binary logistic regression for foot attachment by color morph, sex, their interaction and treatment; Supplementary Table 3
library(pscl) # Pseudo R2
data %>% glm(fattach ~ col * sex + treat, .,
             family = "binomial") %>%   
  {list(summary(.), pR2(.))}
# OUTPUT: only exposure duration had an impact on foot attachment
# Neither shell color or sex showed an influence on foot attachment 

# Binary logistic regression for survival by color morph, sex, their interaction and treatment; Supplementary Table 4
data %>% glm(survival ~ col * sex + treat, .,
             family = "binomial") %>% 
  {list(summary(.), pR2(.))}
# OUTPUT: overall, survival is only influenced by the duration of the exposure: significantly lower rates after an hour
# Neither shell color or sex showed an influence on survival

# Foot attachment; lethal temperatures ---------------------------------------------------------
# Foot attachment by color morph and exposure plot; Figure 3A
data %>%
  group_by(col, temp, treat) %>% 
  summarise(fattach = mean(fattach == 1)) %>% 
  ggplot(aes(x = temp,
             y = fattach)) +
  geom_point(aes(shape = col),
             size = 4) +
  geom_smooth(data = data,
              aes(x = temp,
                  y = as.numeric(fattach == 1)),
              method = "glm",
              method.args = list(family = binomial(link = "logit")),
              formula = y ~ x,
              se = FALSE,
              color = "black",
              alpha = 0.2) +
  geom_smooth(data = data,
              aes(x = temp,
                  y = as.numeric(fattach == 1),
                  color = col),
              method = "glm",
              method.args = list(family = binomial(link = "logit")),
              formula = y ~ x,
              se = FALSE,
              alpha = 0.2) +
  geom_hline(yintercept = 0.5,
             color = "#770003",
             linetype = "dotted",
             linewidth = 0.3) +
  facet_grid(. ~ treat) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "Foot attachment") +
  scale_shape_manual(values = c("lineata" = 4,
                                "fulva" = 1)) +
  theme_classic() +
  scale_x_continuous(breaks = seq(41, 47, by = 1)) +
  theme(text = element_text(size = 20),
        legend.position = "top")
# AT50 estimation
library(MASS)
# Acute exposure
data %>% filter(treat == "Acute") %>% 
  glm(fattach ~ temp, .,
      family = "binomial") %>% 
  {list(lt50 = dose.p(.))}
# OUTPUT: acute AT50 of 44.8 C

# Chronic exposure
data %>% filter(treat == "Chronic") %>% 
  glm(fattach ~ temp, .,
      family = "binomial") %>% 
  {list(lt50 = dose.p(.))}
# OUTPUT: chronic AT50 of 42.4 C

# Exploring relevant cases (non coincident result between morphs) for foot attachment

data %>%
  filter(treat == "Acute",
         temp == "42") %>% 
  group_by(col) %>% 
  summarise(attached = sum(fattach == 1),
            non_attached = sum(fattach == 0)) %>% 
  {fisher.test(matrix(c(.$attached, .$non_attached),
                      nrow = 2, byrow = TRUE))}
# OUTPUT: no significant differences between color morphs after acute exposure at 42 C

data %>%
  filter(treat == "Acute",
         temp == "45") %>% 
  group_by(col) %>% 
  summarise(attached = sum(fattach == 1),
            non_attached = sum(fattach == 0)) %>% 
  {fisher.test(matrix(c(.$attached, .$non_attached),
                      nrow = 2, byrow = TRUE))}
# OUTPUT: lineata morph shows a signficantly lower foot attachment rate than fulva after acute exposure at 45 C
data %>%
  filter(treat == "Chronic",
         temp == "43") %>% 
  group_by(col) %>% 
  summarise(attached = sum(fattach == 1),
            non_attached = sum(fattach == 0)) %>% 
  {fisher.test(matrix(c(.$attached, .$non_attached),
                      nrow = 2, byrow = TRUE))}
# OUTPUT: no significant differences between color morphs after chronic exposure at 42 C

# Survival; lethal temperatures  ---------------------------------------------------------------
# survival by color morph and exposure plot; Figure 3B
data %>%
  group_by(col, temp, treat) %>% 
  summarise(survival = mean(survival == 1)) %>% 
  ggplot(aes(x = temp,
             y = survival)) +
  geom_point(aes(shape = col),
             size = 4) +
  geom_smooth(data = data,
              aes(x = temp,
                  y = as.numeric(survival == 1)),
              method = "glm",
              method.args = list(family = binomial(link = "logit")),
              formula = y ~ x,
              se = FALSE,
              color = "black",
              alpha = 0.2) +
  geom_smooth(data = data,
              aes(x = temp,
                  y = as.numeric(fattach == 1),
                  color = col),
              method = "glm",
              method.args = list(family = binomial(link = "logit")),
              formula = y ~ x,
              se = FALSE,
              alpha = 0.2) +
  geom_hline(yintercept = 0.5,
             color = "#770003",
             linetype = "dotted",
             linewidth = 0.3) +
  facet_grid(. ~ treat) +
  labs(x = "Experimental Temperature (⁰C)",
       y = "survival") +
  scale_shape_manual(values = c("lineata" = 4,
                                "fulva" = 1)) +
  theme_classic() +
  scale_x_continuous(breaks = seq(41, 47, by = 1)) +
  theme(text = element_text(size = 20),
        legend.position = "top")

#LT50 estimation
# Acute exposure
data %>% filter(treat == "Acute") %>% 
  glm(survival ~ temp, .,
      family = "binomial") %>% 
  {list(lt50 = dose.p(.))}
# OUTPUT: acute LT50 of 44.5 C

# Chronic exposure
data %>% filter(treat == "Chronic") %>% 
  glm(survival ~ temp, .,
      family = "binomial") %>% 
  {list(lt50 = dose.p(.))}
#### OUTPUT: chronic LT50 of 44.0 C

# Exploring relevant cases (non coincident result between morphs) for survival
data %>%
  filter(treat == "Chronic",
         temp == "44") %>% 
  group_by(col) %>% 
  summarise(alive = sum(survival == 1),
            dead = sum(survival == 0)) %>% 
  {fisher.test(matrix(c(.$alive, .$dead),
                      nrow = 2, byrow = TRUE))}
# OUTPUT: no significant differences between color morphs after chronic exposure at 44 C