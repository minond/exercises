R Programming

# Libraries

- swirl, a great way to learn about R and its environment.
- ggplot2, a plotting system based on the grammar of graphics.
- magrittr, defines the %>% operator for chaining functions together in a series of operations on data.
- dplyr, a suite of (fast) functions for working with data frames.
- tidyr, easily tidy data with spread() and gather() functions.
- readr, provides functions for reading in data in different formats.
- stringr, provides a lot of functionality for working with strings.
- pryr, provides functions like mem_used and object_size which are useful for getting memory usage info.


# Piping

    # Without piping
    function(dataframe, argument_2, argument_3)

    # With piping
    dataframe %>% function(argument_2, argument_3)


    # Without piping
    head(select(filter(ext_tracks, storm_name == “KATRINA”),
        month, day, hour, max_wind), 3)

    # With piping
    katrina <- filter(ext_tracks, storm_name == “KATRINA”)
    katrina_reduced <- select(katrina, month, day, hour, max_wind)
    head(katrina_reduced, 3)

    # A tibble: 3 × 4
        month    day        hour    max_wind
        <chr>    <chr>    <chr>    <int>
    1    10        28        18        30
    2    10        29        00        30
    3    10        29        06        30


# Merging

| Function | What it includes in merged data frame |
|------------|-----------------------------------------------------------------------------------------------------------|
| left_join | Includes all observations in the left data frame, whether or not there is a match in the right data frame |
| right_join | Includes all observations in the right data frame, whether or not there is a match in the left data frame |
| inner_join | Includes only observations that are in both data frames |
| full_join | Includes all observations from both data frames |


# readr

| Function | Use |
|------------|----------------------------------------------|
| read_csv | Reads comma-separated file |
| read_csv2 | Reads semicolon-separated file |
| read_tsv | Reads tab-separated file |
| read_delim | General function for reading delimited files |
| read_fwf | Reads fixed width files |
| read_log | Reads log files |
