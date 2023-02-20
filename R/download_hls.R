#*******************************************************************************
# Description: Download Harmonized Landsat and Sentinel-2 imagery. 
#   See https://hls.gsfc.nasa.gov/
# 
# Author: Xiaojie Gao
# Date: 2020-10-14
#*******************************************************************************


# outdir <- "Z:/Gao/"
# tiles <- c("17SPC", "17SQC")
# yrs <- c(2015, 2016, 2017, 2018, 2019)

DlHLS <- function(tiles, yrs, outdir) {
    if(!dir.exists(outdir)) dir.create(outdir)

    root_url <- "https://hls.gsfc.nasa.gov/data/v1.4"

    for (prod in c("S30", "L30")) {
        if (!dir.exists(file.path(outdir, prod))) {
            dir.create(file.path(outdir, prod))
        }

        for (yr in yrs) {
            for (tile in tiles) {
                # construct data file urls
                tile_folder_url <- file.path(root_url, prod, yr, 
                    substr(tile, 1, 2), substr(tile, 3, 3), 
                    substr(tile, 4, 4), substr(tile, 5, 5)
                )
                # request for contents
                r <- httr::GET(tile_folder_url)
                doc <- XML::htmlParse(r)
                links <- XML::xpathSApply(doc, "//a/@href")
                data_links <- links[grepl(".*.hdf", links)]
                # construct data links
                data_urls <- paste0(tile_folder_url, "/", data_links)
                # download
                sapply(seq_along(data_urls), function(i) {
                    if (!file.exists(file.path(outdir, prod, data_links[i]))) {
                        download.file(data_urls[i], 
                            file.path(outdir, prod, data_links[i]), 
                            method = "curl"
                        )
                    }
                })

                print(paste("Downloading", tile, "of", yr, "..."))
            }
        }
    }

    print("Done!")
}




