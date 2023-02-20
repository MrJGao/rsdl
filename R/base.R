#'******************************************************************************
#' Description: Base functions and variables.
#' 
#' Author: Xiaojie Gao
#' Date: 2023-02-06
#'******************************************************************************


#' Batch download links into a specified folder
#' 
#' @description
#' This file process batch downloading from a bunch of provided links. It can 
#' be used to download both the image query result from the Appears application 
#' and general files. The input of this script is a "*.txt" file that contains
#' downloadable links.

#' `links` must be a one-dimensional array containing urls needed to be 
#' downloaded as array rows. If the specified `outdir` does not exist, the 
#' function will create it.
#' 
#' @param links One-dimensional array containing urls.
#' @param outdir The output folder.
#' @param method Url downloading method, default is `curl`. It can be 
#'   "internal", "wininet" (Windows only) "libcurl", "wget" and "curl".
#' @param pbar Indicate whether to show a progress bar. Default is `TRUE`.
#'
#' @return `NULL` if successful, otherwise will throw out errors. 
#'
#' @export
BatchDownload <- function(links, outdir, method = "curl", pbar = TRUE) {
    if (dir.exists(out_dir) == FALSE) {
        dir.create(out_dir)
    }
    
    pb <- txtProgressBar(min = 0, max = 100, style = 3)
    for (i in 1:nrow(links)) {
        cur_link <- links[i, 1]
        # Parse filename
        tmp <- unlist(strsplit(cur_link, "/"))
        filename <- tmp[length(tmp)]
        # Download
        download.file(
            cur_link, 
            file.path(out_dir, filename), 
            method = "curl",
            quiet = TRUE
        )

        # update progress
        setTxtProgressBar(pb, i * 100 / nrow(links))
    }
    close(pb)
}



#' Query FTP folder
#' 
#' @description
#' Query files and directories in an FTP server folder.
#' 
#' @param ftp_url FTP address.
#' @param username,password FTP username and password.
#' @param dirlistonly Whether to return directories only.
#'
#' @return Query result in a table.
#'
#' @export
FtpQuery <- function(ftp_url, username, password, dirlistonly = TRUE) {
    # Get file names in the folder
    h <- curl::new_handle(
        dirlistonly = dirlistonly,
        username = username,
        password = password
    )
    con <- curl::curl(ftp_url, "r", h)
    tbl <- read.table(con, stringsAsFactors = TRUE, fill = TRUE)
    close(con)

    return(tbl)
}



