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
  download_worker <- function(url, path) {
    
    message(glue("Downloading: {basename(path)}"))
    
    # --- UPDATED HANDLE ---
    # We add ssl_verifypeer = 0 to ignore certificate issues
    # We add a User-Agent to look like a browser (Chrome)
    h <- new_handle(
      timeout = 1200,
      ssl_verifypeer = 0, 
      ssl_verifyhost = 0,
      useragent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
    )
    
    tryCatch({
      curl_download(
        url, 
        path, 
        quiet = FALSE, 
        handle = h)
      Sys.sleep(2) 
    }, error = function(e) {
      # We keep the fix from the previous step here
      warning(glue("FAILED: {basename(path)} - {e$message}"))
    })
  }
  
  purrr::walk2(
    files_to_download$url, 
    files_to_download$path, 
    download_worker)
  
  message("Process complete.")
}