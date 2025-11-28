library(purrr)

#' Wrapper to unzip multiple files safely
#' 
#' @param zip_links A data frame containing at least two columns: 
#'   - `path`: The local file path to the .zip file.
#'   - `zip_path`: The specific file inside the zip to extract (or NA to extract all).
multi_unzip <- function(zip_links, dest_folder) {
  
  # Helper function to handle extraction logic and error catching
  # If the file path inside the zip file is empty, just unzips all as usual.
  # Performs extraction with robust error handling (tryCatch).
  unzip_worker_from_path <- function(zip_file, zip_path, dest_folder) {
    
    tryCatch({
      
      # If a specific file path is provided inside the zip:
      if (!is.na(zip_path)) {
        unzip(
          zipfile = zip_file,
          files = zip_path,
          exdir = dest_folder,
          junkpaths = TRUE
        )
        # If no specific path is given (NA), extract the entire archive:
      } else {
        unzip(
          zipfile = zip_file,
          exdir = dest_folder,
          junkpaths = TRUE
        )
      }
      
      # Log success
      cat(paste("SUCCESS:", basename(zip_file), "\n"))
      
    }, error = function(e) {
      # Log error without stopping the loop
      cat(paste("ERROR:", basename(zip_file), "Failed:", conditionMessage(e), "\n"))
    })
  }
  
  # Iterate over 'path' and 'zip_path' columns simultaneously for side-effects
  purrr::pwalk( 
    list(
      zip_links$path, 
      zip_links$zip_path,
      dest_folder 
    ),
    unzip_worker_from_path
  )
  
}
