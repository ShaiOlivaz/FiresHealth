library(dplyr)
library(purrr)
library(curl)
library(glue)


##' Download multiple files from API links by iteration
##'
##' This function takes a data frame of API links,
##' checks which files already exist, and downloads only the missing ones
##' in using curl, iterated using purrr::walk
##'
##' @param api_links A data frame containing the links and desired file names.
##'   It must have two columns:
##'   \itemize{
##'     \item \code{url}: A character vector of the API download URLs.
##'     \item \code{file}: A character vector of the base names for the output zip files.
##'     \item \code{path}: A character vector of the full path for the output zip files.
##'   }
##' @param folder_path A \strong{character string} specifying the path to the
##'   directory where the downloaded CSV files should be saved.


multi_download_from_df_iter <- function(api_links) {
  
  # Remove files that already exist
  files_to_download <- api_links |> filter(!file.exists(path))
  
  if (nrow(files_to_download) == 0) {
    message("All files already exist. Nothing to download.")
    return(invisible(NULL))
  }
  
  message(glue("Downloading {nrow(files_to_download)} files sequentially using purrr..."))
  
  # Define the worker function
  # This handles the download, the timeout, and the "polite" pause
  download_worker <- function(url, path) {
    
    message(glue("Downloading: {basename(path)}"))
    
    # Create handle for 10-minute timeout
    h <- new_handle(timeout = 600)
    
    tryCatch({
      curl_download(
        url, 
        path, 
        quiet = FALSE, 
        handle = h)
      Sys.sleep(2) # Polite pause
    }, error = function(e) {
      warning(glue("FAILED: {filename} - {e$message}"))
    })
  }
  
  # Execute with walk2
  # We use walk2 because we have two inputs (url, path) and we want side-effects (files)
  
  purrr::walk2(
    files_to_download$url, 
    files_to_download$path, 
    download_worker)
  
  message("Process complete.")
}