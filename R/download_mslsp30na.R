#************************************************************************
# Description: Download MSLSP30NA tiles from the ftp server of LP DAAC.
# Author: Xiaojie(J) Gao
# Date: 2022-02-08
#************************************************************************
# library(httr)



#' Download MSLSP30NA tiles from the ftp server of LP DAAC.
#' @param tiles Tiles needed to download.
#' @param yrs Years.
#' @param out_dir Download directory.
#' @param username Earthdata Login account username.
#' @param password Earthdata Login account password.
#' @export
#' @example 
#' /dontrun {
#' DownloadMSLSP30NA("15TYL", c(2018, 2019), "D:/Temp/", "[username]", "[password]")
#' }
DownloadMSLSP30NA <- function(tiles, yrs, out_dir, username, password) {
    handle <- "https://e4ftl01.cr.usgs.gov/COMMUNITY/MSLSP30NA.011/"

    if (!dir.exists(out_dir)) {
        dir.create(out_dir)
    }

    message("Tiles to download: ", length(yrs) * length(tiles))
    
    # Download
    for (yr in yrs) {
        for (tile in tiles) {
            fname <- paste0("MSLSP_", tile, "_", yr, ".nc")
            response <- httr::GET(
                paste0(handle, "/", yr, ".01.01", "/", fname),
                write_disk(file.path(out_dir, fname), overwrite = TRUE),
                authenticate(user = username, password = password),
                progress()
            )
        }
    }
}