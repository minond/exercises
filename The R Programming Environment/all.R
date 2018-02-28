library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

daily_spec <- read_csv("data/daily_SPEC_2014.csv.bz2")
aqs_sites  <- read_xlsx("data/aqs_sites.xlsx")

# 1. What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of
#    Wisconsin in this dataset?
daily_spec %>%
  filter(`State Name` == "Wisconsin" & `Parameter Name` == "Bromine PM2.5 LC") %>%
  summarize(AverageArithmeticMean = mean(`Arithmetic Mean`, na.rm = TRUE))

# 2. Which constituent Parameter.Name has the highest average level?
daily_spec %>%
  group_by(`Parameter Name`) %>%
  summarize(Avg = mean(`Observation Count`, na.rm = TRUE)) %>%
  arrange(desc(Avg))

# 3. Which monitoring site has the highest average level of “Sulfate PM2.5 LC”
#    across all time? Indicate the state code, county code, and site number.
daily_spec %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`, `Parameter Name`) %>%
  summarize(Avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(Avg))

# 4. What is the absolute difference in the average levels of “EC PM2.5 LC TOR”
#    between the states California and Arizona, across all time and all
#    monitoring sites?
daily_spec %>%
  group_by(`State Name`) %>%
  filter(
    `Parameter Name` == "EC PM2.5 LC TOR" &
    `State Name` %in% c("California", "Arizona")) %>%
  summarize(Avg = mean(`Arithmetic Mean`, na.rm = TRUE))

# 5. What is the median level of “OC PM2.5 LC TOR” in the western United
#    States, across all time? Define western as any monitoring location that
#    has a Longitude LESS THAN -100.
daily_spec %>%
  filter(`Parameter Name` == "OC PM2.5 LC TOR" & Longitude < -100) %>%
  summarize(Avg = median(`Arithmetic Mean`, na.rm = TRUE))

# 6. How many monitoring sites are labelled as both RESIDENTIAL for "Land Use"
#    and SUBURBAN for "Location Setting"?
aqs_sites %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  summarize(Count = n())

# 7. What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites
#    that are labelled as both “RESIDENTIAL” and “SUBURBAN” in the eastern
#    U.S., where eastern is defined as Longitude greater than or equal to -100?
aqs_sites %>%
  left_join(daily_spec, by = c("Longitude", "Latitude")) %>%
  filter(
    `Land Use` == "RESIDENTIAL" &
    `Location Setting` == "SUBURBAN" &
    `Parameter Name` == "EC PM2.5 LC TOR" &
    Longitude >= -100) %>%
  summarize(Median = median(`Arithmetic Mean`))

# 8. Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use",
#    which month of the year has the highest average levels of "Sulfate PM2.5
#    LC"?
aqs_sites %>%
  left_join(daily_spec, by = c("Longitude", "Latitude")) %>%
  filter(`Land Use` == "COMMERCIAL" & `Parameter Name` == "Sulfate PM2.5 LC") %>%
  mutate(Month = gsub("\\d{4}-(\\d{2})-\\d{2}", "\\1", "2014-02-26")) %>%
  group_by(Month) %>%
  summarize(Avg = mean(`Arithmetic Mean`))

# 9. Take a look at the data for the monitoring site identified by State Code
#    6, County Code 65, and Site Number 8001 (this monitor is in California).
#    At this monitor, for how many days is the sum of "Sulfate PM2.5 LC" and
#    "Total Nitrate PM2.5 LC" greater than 10?
#
#    For each of the chemical constituents, there will be some dates that have
#    multiple `Arithmetic Mean` values at this monitoring site. When there are
#    multiple values on a given date, take the average of the constituent
#    values for that date.
aqs_sites %>%
  left_join(daily_spec, by = c("Longitude", "Latitude")) %>%
  filter(
    `State Code.x` == 6 &
    `County Code.x` == 65 &
    `Site Number` == 8001 &
    `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(`Parameter Name`, `Date Local`) %>%
  summarise(Avg = mean(`Arithmetic Mean`)) %>%
  group_by(`Date Local`) %>%
  summarise(Sum = sum(Avg)) %>%
  filter(Sum > 10)

# 10. Which monitoring site in the dataset has the highest correlation between
#     "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates?
#     Identify the monitoring site by it's State, County, and Site Number code.
#
#     For each of the chemical constituents, there will be some dates that have
#     multiple Sample.Value's at a monitoring site. When there are multiple
#     values on a given date, take the average of the constituent values for
#     that date.
aqs_sites %>%
  left_join(daily_spec, by = c("Longitude", "Latitude")) %>%
  filter(`Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(
    `State Code.x`, `County Code.x`, `Site Number`,
    `Parameter Name`, `Date Local`) %>%
  summarise(Avg = mean(`Arithmetic Mean`)) %>%
  spread(`Parameter Name`, Avg) %>%
  group_by(`State Code.x`, `County Code.x`, `Site Number`) %>%
  summarise(Correlation = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(Correlation))
