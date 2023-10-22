transform_metadata_to_df <- function(metadata) {
  
  df <- tibble(data = metadata$trafficRegistrationPoints) %>% 
    mutate(
      id = map_chr(data, "id"),
      name = map_chr(data, "name"),
      latestData = map_chr(data, ~ ifelse(!is.null(.x$latestData$volumeByHour), .x$latestData$volumeByHour, NA_character_)),
      lat = map_dbl(data, ~ .x$location$coordinates$latLon$lat),
      lon = map_dbl(data, ~ .x$location$coordinates$latLon$lon)
    ) %>%
    select(id, name, latestData, lat, lon) %>%
    mutate(latestData = as.POSIXct(latestData, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  
  return(df)
}

to_iso8601 <- function(datetime_var, offset_days) {
  # Add offset to the datetime_var
  datetime_var <- datetime_var + lubridate::days(offset_days)
  
  # Convert to ISO8601 format and append "Z"
  iso_time <- anytime::iso8601(datetime_var)
  return(paste0(iso_time, "Z"))
}

transform_volumes <- function(json_data) {
  # Parse the JSON to extract relevant data
  dates <- sapply(json_data$trafficData$volume$byHour$edges, function(x) x$node$from)
  volumes <- sapply(json_data$trafficData$volume$byHour$edges, function(x) x$node$total$volumeNumbers$volume)
  
  # Create a data frame
  df <- data.frame(Date = as.POSIXct(dates, format="%Y-%m-%dT%H:%M:%S"), Volume = volumes)
  
  return(df)
}


