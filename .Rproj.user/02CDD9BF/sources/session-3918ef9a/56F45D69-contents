library(glue)
library(curl)
library(dplyr)

##' Download multiple files from API links in parallel.
##'
##' This function takes a data frame of API links and a destination folder path,
##' checks which files already exist, and downloads only the missing ones
##' in parallel using curl.
##'
##' @param api_links A data frame containing the links and desired file names.
##'   It must have two columns:
##'   \itemize{
##'     \item \code{url}: A character vector of the API download URLs.
##'     \item \code{file}: A character vector of the base names for the output CSV files.
##'   }
##' @param folder_path A \strong{character string} specifying the path to the
##'   directory where the downloaded CSV files should be saved.

multi_download_from_df <- function(api_links, folder_path) {
  
  
  
  # Convert API links to a character vector and get output file paths
  download_links <- api_links$url
  output_paths <- file.path(folder_path, api_links$file)
  
  # Identify files that need to be downloaded
  missing_links <- download_links[!file.exists(output_paths)]
  missing_paths <- output_paths[!file.exists(output_paths)]
  
  if (length(missing_links) == 0) {
    message("All files already exist. Nothing to download.")
    return(invisible(NULL))
  }
  
  message(glue("Downloading {length(missing_links)} files in parallel..."))
  
  # Perform parallel download using curl::multi_download
  results <- curl::multi_download(
    urls = missing_links,
    destfiles = missing_paths
  )
  
  # Check for errors
  failed <- results[results$status_code != 200, ]
  if (nrow(failed) > 0) {
    warning(glue("{nrow(failed)} files failed to download. Check the logs."))
    print(failed)
  } else {
    message("All files downloaded successfully.")
  }
}