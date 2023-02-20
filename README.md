# Remote sensing data batch download

This repository is just a note for me to conviniently download some remote sensing data using R programming language. I'd be happy if other people find it useful as well.

Currently includes:

* [Harmonized Landsat and Sentinel-2 (HLS)](https://hls.gsfc.nasa.gov/) imagery
* [Daymet](https://daymet.ornl.gov/) climate dataset
* Image query from [AppEEARS](https://lpdaacsvc.cr.usgs.gov/appeears/) API
* General batch download

## How to use

There are several R script files containing needed code to process the downloading from different data sources. 

In each R script file, there are descriptions and examples for helping user understand how to use the code. Usually, users only need to specify the bounding box or tile name of the Area of Interest (AOI) and also provide the destnamtion path of the downloaded files. Users can also adapt the code to fit their specific needs.