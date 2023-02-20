#'******************************************************************************
#' Description: Download the global GIMMS NDVI3g product.
#'
#' Author: Xiaojie Gao
#' Date: 2022-09-22
#'******************************************************************************


#' Download the global GIMMS NDVI3g product
#' 
#' @description
#' From : https://data.tpdc.ac.cn/en/data/c6113f70-884a-4716-98e3-933421c57f25/?q=gimms
#' Ftp server: 210.72.14.198
#' Ftp username: download_414169
#' Ftp password: 31879907
#' 
#' @param outdir Output directory.
#'
#' @return `NULL`
#'
#' @export
DlGimmisNDVI <- function(outdir) {
    url <- "ftp://210.72.14.198"
    gimmis_username <- "download_414169"
    gimmis_password <- "31879907"
    
    # Query folders to download
    tbl <- FtpQuery(url, gimmis_username, gimmis_password)

    # Download each file
    urls <- paste0(url, "/", tbl[2:nrow(tbl), 1])
    pb <- txtProgressBar(min = 0, max = 100, style = 3)
    for (i in seq_along(urls)) {
        f_url <- urls[i]
        f_name <- basename(f_url)

        curl::curl_download(f_url,
            file.path(dir, f_name),
            handle = curl::new_handle(
                username = gimmis_username,
                password = gimmis_password
            )
        )

        setTxtProgressBar(pb, i * 100 / length(urls)) # update progress
    }
    close(pb)

    return(NULL)
}