#PROBLEM 2

# Define a function to transform metadata into a data frame
transform_metadata_to_df <- function(metadata) {
  
    # Select the elements in the list we want to transform
    df <- metadata[[1]] %>%
    
    # Combine the lists into a single dataframe
    map(as_tibble) %>%
    list_rbind() %>%
    
    # Modify the 'latestData' column, converting it to a datetime format
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
    mutate(latestData = as_datetime(latestData)) %>%
    
    # Unlist the 'location' column
    mutate(location = map(location, unlist)) %>%
    
    # Extract 'lat' and 'lon' values from the 'location' column
    mutate(lat = map_dbl(location, "latLon.lat"), lon = map_dbl(location, "latLon.lon")) %>%
    
    # Remove the 'location' column
    select(-location)
    
    # Return the dataframe
    return(df)
}

#PROBLEM 4a time function

to_iso8601 <- function(dt, offset) {
  
  # Add the specified offset to the datetime variable
  adjusted_dt <- dt + days(offset)
  
  # Format the adjusted datetime in ISO8601 format with "Z" indicating UTC
  iso8601_string <- format(adjusted_dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  return(iso8601_string)
}

#Problem 4B transform json to dataframe function

transform_volumes <- function(json) {
  
  #Transform the list into a tibble
  json[[1]][[1]][[1]]  %>%
    as_tibble() %>%
    
    #Unnest all the columns
    unnest_wider(edges) %>%
    unnest_wider(node) %>%
    unnest_wider(total) %>%
    unnest_wider(volumeNumbers) %>%
    
    #Create "from" and "to" columns using datetime and add the "volume" column as numeric
    mutate(from = ymd_hms(from), to = ymd_hms(to), volume = as.numeric(volume))
    
}


