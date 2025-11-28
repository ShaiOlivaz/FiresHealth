library(purrr)

#' Clean up ZIP files if the corresponding CSV exists
#' 
#' @param file_names A character vector of filenames (without extensions).
#' @param folder_path The directory path where the files are located.
cleanup_zips <- function(file_names, folder_path) {
  
  # Worker function to check existence and delete
  delete_worker <- function(name) {
    
    # Construct full paths for the zip and the csv
    zip_file <- file.path(folder_path, paste0(name, ".zip"))
    csv_file <- file.path(folder_path, paste0(name, ".csv"))
    
    # Check if BOTH files exist
    if (file.exists(zip_file) && file.exists(csv_file)) {
      
      # Remove the zip file
      file.remove(zip_file)
      cat(paste("CLEANED: Deleted", basename(zip_file), "because CSV exists.\n"))
      
    } else {
      # Do nothing (or optionally log that it was skipped)
      # cat(paste("SKIPPED:", name, "- conditions not met.\n"))
    }
  }
  
  # Iterate over the vector of file names
  # We don't need walk2 here because 'folder_path' is constant, 
  # so we only iterate over 'file_names'.
  purrr::walk(file_names, delete_worker)
  
}