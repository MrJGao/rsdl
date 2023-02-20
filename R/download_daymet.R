#*******************************************************************************
# Description: Download Daymet climate data. Actually, this process is 
#   provided by the "daymetr" package, thanks to the authors of it. 
#   Users can read their full documentation at: 
#   https://cran.r-project.org/web/packages/daymetr/daymetr.pdf

# Author: Xiaojie Gao
# Date: 2020-11-26
#*******************************************************************************
# library(daymetr)


# # user inputs
# # ==============================================================
# startyr <- 1980
# endyr <- 1982

# # VARIABLES - Daymet variables - tmin and tmax are used as examples, variables should be space separated.
# # The complete list of Daymet variables is: tmin, tmax, prcp, srad, vp, swe, dayl
# var <- c("tmin", "tmax")

# # VARIABLES - Spatial subset - bounding box in decimal degrees.
# north <- 36.61
# west <- -85.37
# east <- -81.29
# south <- 33.57

# # output folder
# out_dir <- "Z:/Gao/"
# # ==============================================================


# # Actually, there are several download options provided in the "daymetr" package, here I only include a simpliest and commonly used one.
# daymetr::download_daymet_ncss(location = c(north, west, south, east), 
#     start = startyr, end = endyr, 
#     param = var, frequency = "daily", 
#     path = out_dir
# )

# # Other download functions include: 
# # - download_daymet(): download single location daymet data.
# # - download_daymet_batch(): download several single locations daymet data, locations are specified by a batch file.
# # - download_daymet_tiles: download gridded daymet data tiles.

# # Full documentation can be found here: https://cran.r-project.org/web/packages/daymetr/daymetr.pdf


# # They also provide a function to convert the downloaded daymet netCDF file to tiff format, although I don't recommend doing so.
# daymetr::net2cdf(path = out_dir, files = "tmin_daily_1980_ncss.nc")


