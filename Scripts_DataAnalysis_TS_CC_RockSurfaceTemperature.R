setwd("") # Working directory with the raw data

# Reading and combining all Excel sheets (locations)
library(readxl)
data_N <- do.call(rbind,lapply(c("1N", "2N", "3N", "4N", "5N", "6N"), function(sheet)
  {read_excel("TS_CC_RData_RockSurfaceTemperature_North.xlsx", sheet = sheet, na = "N/A")}))

data_S <- do.call(rbind,lapply(c("1S", "2S", "3S", "4S", "5S", "6S"), function(sheet)
  {read_excel("TS_CC_RData_RockSurfaceTemperature_South.xlsx", sheet = sheet, na = "N/A")}))

alldata <- rbind(data_N, data_S)
str(alldata) # Summary of data structure

# Data arrangement and overview
library(tidyverse)
data <- alldata %>% 
  mutate(Coast = factor(case_when(Coast == "1" ~ "North",
                                  Coast == "2" ~ "South"),
                   levels = c("North", "South")),
         Location = factor(LocName,
                           levels = c("Cesantes", "PuntaSoutelo", "Areiño", "Coruxo", "Panxon", "BaionaCuncheiro",
                                      "PuntaCabalo", "SanAdrian", "Borna", "PuntaNiñoCorvo", "PuntaIgreixiña", "CaboHome")),
         Ecosystem_Type = factor(Ecosystem_Type,
                                 levels = c("WaveSheltered", "WaveIntermediate", "WaveExposed")),
         season = factor(case_when(month(time) %in% c(12, 1, 2) ~ "Winter",
                                   month(time) %in% c(3, 4, 5) ~ "Spring",
                                   month(time) %in% c(6, 7, 8) ~ "Summer",
                                   month(time) %in% c(9, 10, 11) ~ "Fall"),
                         levels = c("Summer", "Fall", "Winter", "Spring")),
         time = as.POSIXct(time, format = "%Y-%m-%d"),
         year = floor_date(time, unit = "year"),
         month = floor_date(time, unit = "month"),
         day = floor_date(time, unit = "day"),
         hour = floor_date(time, unit = "hour"),
         chour = format(hour, "%H:%M"),
         peak_sun = factor(ifelse(between(chour, "12:00", "16:00"), 1, 0)))
str(data)

# Setting an hour as unit and joining hourly sea level data (source: Puertos del Estado, Vigo)
data_hour_temp <- data %>%
  group_by(Coast, Location, EcolPC1, Ecosystem_Type, year, month, season, day, hour) %>%
  summarise(temp = mean(temp),
            time = last(time))

data_hour_sealevel <- read_excel("TS_CC_SeaLevel_PORTUS.xlsx", na = "-9999.9") %>%
  mutate(time_UTC = as.POSIXct(time_GMT, format = "%Y %m %d %H", tz = "UTC"))

data_hour <- left_join(data_hour_temp, data_hour_sealevel, 
                       by = c("hour" = "time_UTC"))

# Exploratory Analysis -------------------------------------------------------------
# Tide level during temperature recording period; NOT INCLUDED
data_hour %>%
  ggplot(aes(x = hour,
             y = sea_level)) +
  geom_line(color = "blue",
            alpha = 0.3) +
  geom_hline(yintercept = mean(data_hour$sea_level, na.rm = TRUE),
             linewidth = 0.8) +
  geom_hline(yintercept = mean(data_hour$sea_level, na.rm = TRUE) - 0.5 * sd(data_hour$sea_level, na.rm = TRUE),
             color = "blue",
             linewidth = 0.8) +
  theme_bw() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

# Setting a day as unit for plotting
data_day <- data_hour %>% 
  group_by(Coast, Ecosystem_Type, Location, EcolPC1, year, month, season, day) %>% 
  reframe(mean_temp = mean(temp),
          sd_temp = sd(temp),
          max_temp = max(temp),
          min_temp = min(temp),
          mean_sealevel = mean(temp),
          sd_sealevel = sd(sea_level),
          max_sealevel = max(sea_level),
          min_sealevel = min(sea_level),
          time = last(time),
          sea_level = sea_level)

# Rock surface temperature over time by location; Supplementary Figure 3
data_hour %>% 
  ggplot(aes(x = hour)) +
  geom_line(aes(y = temp, # Hourly mean temperature
                color = "Hourly mean"),
            linewidth = 0.05,
            alpha = 0.2) +
  geom_ribbon(data = data_day, # Daily mean temperature +- SD
              aes(x = day,
                  ymin = mean_temp - sd_temp,
                  ymax = mean_temp + sd_temp,
                  fill = Ecosystem_Type),
              alpha = 0.2) +
  geom_line(data = data_day, # Daily maximum temperature
            aes(x = day,
                y = max_temp,
                color = "Daily maximum"),
            linewidth = 0.5,
            alpha = 0.8) +
  geom_line(data = data_day, # Daily minimum temperature
            aes(x = day,
                y = min_temp,
                color = "Daily minimum"),
            linewidth = 0.5,
            alpha = 0.8) +
  geom_line(data = data_day, # Daily mean temperature
            aes(x = day,
                y = mean_temp,
                color = "Daily mean +- SD"),
            linewidth = 0.3,
            alpha = 0.3) +
  geom_hline(yintercept = c(35, 40, 45, 50), # Experimental temperatures
             color = "#770003",
             linetype = "dotted",
             linewidth = 0.3,
             alpha = 0.2) +
  geom_hline(yintercept = c(44), # Chronic LT50 
             color = "#770003",
             linewidth = 0.5,
             alpha = 1) +
  facet_wrap(~Coast + Location, 
             dir = "v", 
             ncol = 2, 
             nrow = 6) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 55)) +
  scale_color_manual(values = c("Daily maximum" = "#FF0000",
                                "Daily mean +- SD" = "black",
                                "Daily minimum" = "#38C1FF",
                                "Hourly mean" = "black")) +
  theme(legend.position = "top")

# Rock surface temperature over time by ecosystem type (sheltered, intermediate or exposed); not included
data_hour %>%
  ggplot(aes(x = hour)) +
  geom_line(aes(y = temp, # Hourly mean temperature
                color = "Hourly mean"),
            linewidth = 0.05,
            alpha = 0.2) +
  geom_ribbon(data = data_day, # Daily mean temperature +- SD
              aes(x = day,
                  ymin = mean_temp - sd_temp,
                  ymax = mean_temp + sd_temp,
                  fill = Ecosystem_Type),
              alpha = 0.2) +
  geom_line(data = data_day, # Daily maximum temperature
            aes(x = day,
                y = max_temp,
                color = "Daily maximum"),
            linewidth = 0.5,
            alpha = 0.8) +
  geom_line(data = data_day, # Daily minimum temperature
            aes(x = day,
                y = min_temp,
                color = "Daily minimum"),
            linewidth = 0.5,
            alpha = 0.8) +
  geom_line(data = data_day, # Daily mean temperature
            aes(x = day,
                y = mean_temp,
                color = "Daily mean +- SD"),
            linewidth = 0.3,
            alpha = 0.3) +
  geom_hline(yintercept = c(35, 40, 45, 50), # Experimental temperatures
             color = "#770003",
             linetype = "dotted",
             linewidth = 0.3,
             alpha = 0.2) +
  geom_hline(yintercept = c(44), # Chronic LT50
             color = "#770003",
             linewidth = 0.5,
             alpha = 1) +
  facet_wrap(~Ecosystem_Type, 
             dir = "v", 
             ncol = 2, 
             nrow = 6) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 55)) +
  scale_color_manual(values = c("Daily maximum" = "#FF0000",
                                "Daily mean +- SD" = "black",
                                "Daily minimum" = "#38C1FF",
                                "Hourly mean" = "black")) +
  theme(legend.position = "top")


# Season Analysis ----------------------------------------------------------------
# Stats and proportion of hours above chronic LT50 per PC1 per season (data_season) and for all pooled (data_global)
data_season <- data_hour %>%
  group_by(Coast, season, Ecosystem_Type, EcolPC1) %>%
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp),
            max_temp = max(temp),
            min_temp = min(temp),
            pct_above_lt50 = mean(temp >= 44) * 100)

data_global <- data_hour %>%
  group_by(Coast, Ecosystem_Type, EcolPC1) %>%
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp),
            max_temp = max(temp),
            min_temp = min(temp),
            pct_above_lt50 = mean(temp >= 44) * 100)

# Boxplots per season; Supplementary Figure 4
data_season %>% 
  ggplot(aes(x = season, 
             y = mean_temp,
             fill = season)) +
  geom_boxplot(alpha = 0.3) +
  labs(x = "Season",
       y = "Mean temperature (°C)") +
  theme_classic() +
  scale_fill_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  theme(legend.position = c(0.9, 0.9))

#ANOVA model
library(car) # Levene and Non-Constant Variance tests
data_season %>%
  group_by(season) %>%
  summarise(sw_W = shapiro.test(mean_temp)$statistic, 
            sw_P_value = shapiro.test(mean_temp)$p.value) # Shapiro-Wilk P-value
leveneTest(mean_temp ~ season, data = data_season)
aov(temp ~ season, data = data_hour) %>% 
  {list(summary(.), # ANOVA test summary
        TukeyHSD(.))} # Tukey HSD post-hoc test


# Scatter plot of mean temperature against PC1 by season
data_season %>%
  ggplot(aes(x = EcolPC1,
             y = mean_temp,
             color = season)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              aes(fill = season),
              alpha = 0.2) +
  labs(x = "PC1",
       y = "Mean temperature (°C)") +
  theme_classic() +
  scale_color_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  scale_fill_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  theme(legend.position = c(0.9, 0.9))

# Regression model for each season mean temperature ~ PC1
seasons <- c("Summer", "Fall", "Winter", "Spring"); results <- list()
for (season in seasons) {
  lm_meanPC1 <- data_season %>%
    filter(season == !!season) %>%
    lm(mean_temp ~ EcolPC1, .)
  
  results[[season]] <- list(model = lm_meanPC1,
                            shapiro = shapiro.test(residuals(lm_meanPC1)),
                            ncv = ncvTest(lm_meanPC1),
                            summary = summary(lm_meanPC1))
}

# Results for each season; Supplementary Table 2
results$Summer
results$Fall
results$Winter
results$Spring

# All seasons; Supplementary Table 2
data_global %>%
  lm(mean_temp ~ EcolPC1, .) %>% 
  {list(shapiro.test(residuals(.)),
        ncvTest(.),
        summary(.))}

# Low tide analysis -------------------------------------------------------
# Subsample for low tide data (observations under average sea level)
# Function for taking 200 random observations for each season
subsample_lowtide <- function(season_name, data_hour) {
  season_data <- data_hour %>% 
    filter(season == season_name,
           sea_level < mean(sea_level, na.rm = TRUE),
           !is.na(sea_level)) # Omitting observations with no sea_level data
  
  season_hours <- season_data %>% # Selecting hours with available data for all 12 locations
    group_by(hour) %>% 
    filter(n_distinct(Location) == 12) %>%
    ungroup() %>% 
    distinct(hour) %>% 
    pull(hour)
  
  set.seed(1234) 
  sampled_hours <- sample(season_hours, size = 200, replace = FALSE)
  
  season_data %>% 
    filter(hour %in% sampled_hours)
}

# Applying the function to all seasons and joining the data
data_hour_lowtide <- bind_rows(subsample_lowtide("Summer", data_hour),
                               subsample_lowtide("Fall", data_hour),
                               subsample_lowtide("Winter", data_hour),
                               subsample_lowtide("Spring", data_hour))

# Stats and proportion of hours above chronic LT50 per PC1 per season (data_season_lowtide) during low tide
data_season_lowtide <- data_hour_lowtide %>% 
  group_by(EcolPC1, season) %>% 
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp),
            max_temp = max(temp),
            min_temp = min(temp),
            pct_above_lt50 = mean(temp >= 44) * 100)

# Scatter plot of mean temperature against PC1 by season during low tide; not included
data_season_lowtide %>%
  ggplot(aes(x = EcolPC1,
             y = mean_temp,
             color = season)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              aes(fill = season),
              alpha = 0.2) +
  labs(x = "PC1",
       y = "Mean temperature (°C)") +
  theme_classic() +
  scale_color_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  scale_fill_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  theme(legend.position = c(0.9, 0.9))

# Scatter plot of mean temperature against PC1 during low tide in Summer
data_season_lowtide %>% 
  filter(season == "Summer") %>% 
  ggplot(aes(x = EcolPC1,
             y = mean_temp)) +
  geom_point(size = 3,
             alpha = 0.5,
             color = "#f8c03c",
             fill = "#f8c03c") +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              alpha = 0.2,
              color = "#f8c03c",
              fill = "#f8c03c") +
  labs(x = "PC1",
       y = "Mean temperature (°C)") +
  theme_classic()

# Regression model for each season mean temperature ~ PC1
results <- list()
for (season in c("Summer", "Fall", "Winter", "Spring")) {
  lm_meanPC1 <- data_season_lowtide %>%
    filter(season == !!season) %>%
    lm(mean_temp ~ EcolPC1, .)
  
  results[[season]] <- list(model = lm_meanPC1,
                            shapiro = shapiro.test(residuals(lm_meanPC1)),
                            ncv = ncvTest(lm_meanPC1),
                            summary = summary(lm_meanPC1))
}

# Results for each season
results$Summer
results$Fall
results$Winter
results$Spring

# Scatter plot of % hours above LT50 against EcolPC1 by season; not included
data_season_lowtide %>% ggplot(aes(x = EcolPC1,
                                   y = pct_above_lt50,
                                   color = season)) +
  geom_point(size = 2,
             alpha = 0.2) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              aes(fill = season),
              alpha = 0.2) +
  labs(x = "PC1",
       y = "% hours with T > LT50") +
  theme_bw() +
  scale_color_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  scale_fill_manual(values = c("#f8c03c", "#c06039", "#00bfc4", "#7cae00")) +
  theme(legend.position = c(0.9, 0.9))

# Regression model for each season percentange of observations above chronic LT50  ~ PC1
results <- list()
for (season in c("Summer", "Spring")) {  # Winter excluded because pct_above_lt50 = 0 in all cases
  lm_lt50PC1 <- data_season_lowtide %>%
    filter(season == !!season) %>%
    lm(pct_above_lt50 ~ EcolPC1, .)
  
  results[[season]] <- list(model = lm_lt50PC1,
                            shapiro = shapiro.test(residuals(lm_lt50PC1)),
                            ncv = ncvTest(lm_lt50PC1),
                            summary = summary(lm_lt50PC1))
}

# Results for each season
results$Summer
results$Spring
