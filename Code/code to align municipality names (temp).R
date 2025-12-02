# Align municipality and add municipality code LATER - ONLY WITH SISAM DATA!

## Create Municipality Dictionary

```{r}
muni_dict <- muni_seat |> 
  as_tibble() |> 
  select(
    code_muni, name_muni, name_state
  ) |> 
  mutate(
    name_muni = stri_trans_general(name_muni, "Latin-ASCII") |> str_to_lower(),
    name_state = stri_trans_general(name_state, "Latin-ASCII") |> str_to_lower()
  )
```

## Reference Satellite

Note that, since stringi functions have not been implemented to arrow. I need to create a custom binding and register it. 

```{r}
str_strip <- function(context, string) {
  string |> 
    stringr::str_to_lower() |> 
    stringi::stri_trans_general("Latin-ASCII")
}

register_scalar_function(
  name = "str_strip",
  fun = str_strip,
  in_type = string(),
  out_type = string(),
  auto_convert = TRUE
)
```


```{r}
fires_ref_sat_names <- fires_ref_sat |> 
  mutate(
    name_muni = str_strip(municipio),
    name_state = str_strip(estado) 
  ) |> 
  select(-municipio, -estado) |>
  left_join(
    muni_dict,
    by = join_by(name_muni, name_state),
    relationship = "many-to-one"
  ) 

# Check if any municipality remains unmatched
fires_ref_sat_names |> 
  anti_join(
    muni_dict,
    by = join_by(name_muni, name_state),
  ) |> 
  select(
    code_muni, name_muni, name_state
  ) |> 
  unique() |> 
  collect()

```




#################################################################
#################### GEMINI CODE TO ALIGN NAMES ##################


library(fuzzyjoin)
library(dplyr)
library(stringdist)

# 1. Get the list of names that failed to match (the 23 cases)
# Assuming 'fires_ref_sat_names' is your arrow object
unmatched_names <- fires_ref_sat_names |> 
  filter(is.na(some_column_from_muni_dict)) |> 
  select(name_muni) |> 
  distinct() |> 
  collect() # Bring ONLY these few rows into R

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