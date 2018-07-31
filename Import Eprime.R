library(rprime)
# Read in an Eprime text file
experiment_lines <- read_eprime("Circle_Mp_Der-3405-1.txt")
# Extract and parse the log-frames from the file
experiment_data <- FrameList(experiment_lines)

# There are six different kinds of frames in this file
preview_levels(experiment_data)
#Get a preview of the data in each kind of frame with preview_frames
preview_frames(experiment_data)
# Filter (out) by depth of nesting
not_level_1 <- drop_levels(experiment_data, 1)
preview_levels(not_level_1)

# Filter (in) by depth of nesting
just_level_3 <- keep_levels(experiment_data, 4)
preview_levels(just_level_3)

Use filter_in and filter_out to filter frames using attribute values. Use repeated filtering statements to drill down into the list of frames.

# Filter (out) by attribute values
no_header <- filter_out(just_level_3, "Running", values = "PracticeBlock")
preview_levels(no_header)


# Filter (in) by attribute values
not_practice <- filter_in(experiment_data, "Running", "PracticeBlock")
preview_levels(not_practice)


# Drill down further into the trials by filtering again
sue_trials <- filter_in(just_level_3, "Procedure", "Experiment")
preview_eprime(sue_trials)

#Exporting
#Convert to a dataframe with to_dataframe. Attribute names in the log-frames become column names in the dataframe.

# Export to dataframe
sue_trials_df <- to_data_frame(sue_trials)
str(sue_trials_df)

# Don't need every column
columns_to_keep <- c("Eprime.Basename", "Module", "Sample", 
                     "Correct", "Response")
sue_trials_df <- sue_trials_df[columns_to_keep]
head(sue_trials_df)
#Note: rprime thinks that all the values in the final dataframe are character values. You can use type_convert in the readr package to correct the column types:
  
  # Right now the sample numbers are stored as character values
  str(sue_trials_df)


library("readr")
sue_trials_df <- type_convert(sue_trials_df)
# Now, they are stored as integers...
str(sue_trials_df)
