# Align municipality and add municipality code LATER - ONLY WITH SISAM DATA!






#################################################################
#################### GEMINI CODE TO ALIGN NAMES ##################


library(fuzzyjoin)
library(dplyr)
library(stringdist)

# # 1. Get the list of names that failed to match (the 23 cases)
# # Assuming 'fires_ref_sat_names' is your arrow object
# unmatched_names <- fires_ref_sat_names |> 
#   filter(is.na(some_column_from_muni_dict)) |> 
#   select(name_muni) |> 
#   distinct() |> 
#   collect() # Bring ONLY these few rows into R

# 2. Bring your reference dictionary names into R (should be small, ~5570 rows)
reference_names <- muni_dict |> 
  select(correct_name = name_muni) |> 
  distinct() |> 
  collect()

# 3. Fuzzy Join
# max_dist is a threshold (0-1). 0.1 usually allows for "i/y" or "eo/eu" swaps
correction_table <- unmatched_names |> 
  stringdist_left_join(reference_names, 
                       by = c("name_muni" = "correct_name"), 
                       method = "jw", # Jaro-Winkler
                       max_dist = 0.1, 
                       distance_col = "dist") |> 
  # Keep only the best match per name
  group_by(name_muni) |> 
  slice_min(dist, n = 1) |> 
  ungroup()

# Check the table manually once to ensure no false positives!
print(correction_table)