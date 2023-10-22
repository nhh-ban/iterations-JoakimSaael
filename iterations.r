library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 
configs <- read_yaml("vegvesen_configs.yml")
gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 
stations_metadata <- GQL(
  query=gql_metadata_qry,
  .url = configs$vegvesen_url
) 

#### 2: Transforming metadata
source("functions/data_transformations.r")
stations_metadata_df <- stations_metadata %>% transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)

### 5: Final volume query: 
source("gql-queries/vol_qry.r")

# Sample a station
sampled_station <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1)

# Extract volume data for the sampled station
volume_data <- vol_qry(
  id = sampled_station$id,
  from = to_iso8601(sampled_station$latestData, -4),
  to = to_iso8601(sampled_station$latestData, 0)
) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes()

# Plot the data
ggplot(data = volume_data, aes(x=Date, y=Volume)) + 
  geom_line() + 
  labs(
    title = "Traffic Volume Over Time",
    subtitle = paste("Station:", sampled_station$name),
    x = "Date-Time",
    y = "Volume",
    caption = "Source: Vegvesen API"
  ) +
  theme_classic()

