#************************************************************************************
# Description: USGS Machine-2-Machine API. See "https://m2m.cr.usgs.gov/".
# Author: Xiaojie Gao
# Date: 2022-01-11
#************************************************************************************
library(httr)
library(jsonlite)
library(geojsonio)
library(sp)
library(rgdal)


api_url <- "https://m2m.cr.usgs.gov/api/api/json/stable/"


#' Login to get the API key.
#' Need to register an ERS account at: https://m2m.cr.usgs.gov/
#' 
#' @param ers_username ERS username
#' @param ers_password ERS password
#' @return The API key
#' @example
#' \dontrun{
#' token <- Login("[username]", "[password]")
#' }
#' @export
Login <- function(ers_username, ers_password) {
    req <- POST(paste0(api_url, "login"),
        body = toJSON(
            list(username = ers_username, password = ers_password), 
            auto_unbox = TRUE
        )
    )
    req <- fromJSON(content(req, "text", encoding = "UTF-8"))

    if (is.null(req$data)) {
        message(req$errorCode)
        message(req$errorMessage)
        return(NULL)
    } else {
        token <- req$data
        return(token)
    }

    return(NULL)
}


#' Internal function used to send requests to the server
#' 
#' @param token The API key
#' @param path The API path
#' @param params Other API needed parameters
#' @return The response object by the server
#' @example
#' /dontrun {
#' req <- SendRequest(token = token, path = "dataset", 
#'    params = list(datasetName = "gls_all")
#' )
#' }
SendRequest <- function(token, path, params = "") {
    req <- POST(paste0(api_url, path),
        add_headers("X-Auth-Token" = token),
        body = toJSON(params, auto_unbox = TRUE, null = "null")
    )
    req <- fromJSON(content(req, "text", encoding = "UTF-8"))

    return(req)
}


#' Get the contact information of the data owner.
#' 
#' @param token The API key.
#' @param data_owner Used to identify the data owner - this value comes from the 
#' dataset-search response.
#' @return The contact information list.
#' @example
#' /dontrun {
#' req <- GetDataOwner(token, "DMID")
#' }
#' @export
GetDataOwner <- function(token, data_owner) {
    req <- SendRequest(token, "data-owner", params = list(dataOwner = data_owner))
    return(req$data)
}


#' Search a data product by id or name
#' 
#' @param token The API key
#' @param dataset_id The dataset identifier - must use this or datasetName
#' @param dataset_name The system-friendly dataset name - must use this or datasetId
#' @return The dataset information object
#' @example
#' /dontrun{
#' req <- GetDataset(token, dataset_name = "gls_all")
#' }
#' @export
GetDataset <- function(token, dataset_id = "", dataset_name = "") {
    if (dataset_id == "" && dataset_name == "") {
        stop("dataset_id or dataset_name must be provided!")
    }
    req <- SendRequest(token, 
        "dataset", 
        params = list(
            datasetName = dataset_name, 
            datasetId = dataset_id
        )
    )
    return(req$data)
}


#' Lists all available bulk products for a dataset - this does not guarantee 
#' scene availability.
#' 
#' @param token The API key
#' @param dataset_name Used to identify which dataset to return results for
#' @return All available bulk products for a dataset
#' @example
#' /dontrun {
#' bulk_prod <- GetBulkProducts(token, "gls_all")
#' }
#' @export
GetBulkProducts <- function(token, dataset_name) {
    req <- SendRequest(token,
        path = "dataset-bulk-products",
        params = list(datasetName = dataset_name)
    )
    return(req$data)
}


#' List all the datasets available under a category.
#' 
#' @param token The API key returned by the `Login` function
#' @param catalog Used to identify datasets that are associated with a given 
#' application
#' @param include_msg Optional parameter to include messages regarding specific 
#' dataset components
#' @param public_only Used as a filter out datasets that are not accessible to 
#' unauthenticated general public users
#' @param use_customization Used as a filter out datasets that are excluded by 
#' user customization
#' @param parent_id If provided, returned categories are limited to categories that 
#' are children of the provided ID
#' @param dataset_filter If provided, filters the datasets - this automatically adds 
#' a wildcard before and after the input value
#' @return The dataset information
#' @example
#' /dontrun {
#' req <- SearchDatasetsInCatalog(token, 
#'     catalog = "EE", 
#'     public_only = FALSE, 
#'     use_customization = FALSE
#' )
#' }
#' @export
SearchDatasetsInCatalog <- function(token, catalog = "", include_msg = NULL, 
    public_only = NULL, use_customization = NULL, parent_id = "", 
    dataset_filter = ""
) {
    req <- SendRequest(token, "dataset-categories", params = list(
        catalog = catalog, includeMessages = include_msg,
        publicOnly = public_only, useCustomization = use_customization,
        parentId = parent_id, dataFilter = dataset_filter
    ))

    if (!is.null(req$data)) {
        return(req$data)
    } else {
        message("Didn't find any dataset, check token or api")
        return(NULL)
    }
}



SearchDataset <- function(token, catalog = "", category_id = "", 
    dataset_name = "", include_msg = NULL, public_only = NULL, 
    include_unknown_spatial = NULL, temporal_filter = NULL, 
    spatial_filter = NULL, use_customization = NULL
) {
    req <- SendRequest(token, "dataset-search", params = list(
        catalog = catalog,
        datasetName = dataset_name,
        includeMessages = include_msg,
        publicOnly = public_only,
        includeUnknownSpatial = include_unknown_spatial,
        temporalFilter = temporal_filter,
        spatailFilter = spatial_filter,
        useCustomization = use_customization
    ))

    return(req$data)
}


SearchScenes <- function(token, dataset_name = "", max_results = NULL, 
    starting_number = NULL, metadata_type = "", sort_field = "", 
    sort_direction = "", sort_customization = NULL, use_customization = NULL,
    scene_filter = NULL, compare_list_name = "", bulk_list_name = "", 
    order_list_name = "", exclude_list_name = "", 
    include_null_metadata_values = NULL
) {
    req <- SendRequest(token, path = "scene-search", params = list(
        datasetName = dataset_name,
        maxResults = max_results,
        startingNumber = starting_number,
        metadataType = metadata_type,
        sortField = sort_field,
        sortDirection = sort_direction,
        sortCustomization = sort_customization,
        useCustomization = use_customization,
        sceneFilter = scene_filter,
        compareListName = compare_list_name,
        bulkListName = bulk_list_name,
        orderListName = order_list_name,
        excludeListName = exclude_list_name,
        includeNullMetadataValues = include_null_metadata_values
    ))

    return(req$data)
}

#' I don't understand what this function is for
#' 
#' @param token The API key.
#' @param dataset_name Used to identify the dataset to clear. If null, all dataset 
#' customizations will be cleared.
#' @param metadataType If populated, identifies which metadata to clear (export, 
#' full, res_sum, shp)
#' @return
#' @example
#' @export
DatasetClearCustomization <- function(token, dataset_name = "", 
    metadata_type = c("")
) {
    # TODO:
}

#' Returns coverage for a given dataset.
#' 
#' @param token The API key.
#' @param dataset_name Determines which dataset to return coverage for.
#' @return A list containing the coverage bounding box `bbox` and actual 
#' polygon(s) `shp`. Both `bbox` and `shp` are `SpatialPolygonsDataFrame` objects.
#' @example
#' /dontrun {
#' req <- GetDatasetCoverage(token, "gls_all")
#' sp::plot(req$bbox)
#' sp::plot(req$shp)
#' }
#' @export
GetDatasetCoverage <- function(token, dataset_name = "") {
    req <- SendRequest(token, 
        path = "dataset-coverage", 
        params = list(datasetName = dataset_name)
    )
    # bounding box
    bbox <- toJSON(req$data$bounds, auto_unbox = TRUE)
    bbox <- rgdal::readOGR(bbox, "OGRGeoJSON", verbose = FALSE)
    # actually polygons
    shp <- toJSON(req$data$geoJson, auto_unbox = TRUE)
    shp <- rgdal::readOGR(shp, "OGRGeoJSON", verbose = FALSE)

    return(list(bbox = bbox, shp = shp))
}


#' This request lists all available products for a given dataset - this does not 
#' guarantee scene availability.
#' 
#' @param token The API key.
#' @param dataset_name Used to identify the which dataset to return results for
#' @param scene_filter Used to filter data within the dataset. 
#' See [here](https://m2m.cr.usgs.gov/api/docs/datatypes/#sceneFilter)
#' @return A list containing all available datasets for download
#' @example
#' /dontrun {
#' dataset_name <- "gls_all"
#' scene_filter <- list(
#'     spatialFilter = list(
#'         filterType = "mbr",
#'         lowerLeft = list(
#'             latitude = 44.60847,
#'             longitude = -99.69639
#'         ),
#'         upperRight = list(
#'             latitude = 44.60847,
#'             longitude = -99.69639
#'         )
#'     ),
#'     metadataFilter = NULL,
#'     cloudCoverFilter = list(
#'         max = 100,
#'         min = 0,
#'         includeUnknown = TRUE
#'     ),
#'     acquisitionFilter = NULL
#' )
#' req <- GetDatasetDownloadOptions(token, dataset_name, scene_filter)
#' }
#' @export
GetDatasetDownloadOptions <- function(token, dataset_name = "", 
    scene_filter = NULL
) {
    req <- SendRequest(token, path = "dataset-download-options", params = list(
        datasetName = dataset_name, sceneFilter = scene_filter
    ))
    return(req$data)
}


#' This request is used to return the metadata filter fields for the 
#' specified dataset. These values can be used as additional criteria when 
#' submitting search and hit queries.
#' 
#' @param token The API key.
#' @param dataset_name Determines which dataset to return filters for
#' @return Metadata filter fields
#' @example
#' /dontrun {
#' req <- GetDatasetFilters(token, "gls_all")
#' }
#' @export
GetDatasetFilters <- function(token, dataset_name) {
    req <- SendRequest(token, 
        path = "dataset-filters", 
        params = list(datasetName = dataset_name)
    )
    return(req$data)
}


# TODO: this function doesn't work
#' This method is used to retrieve metadata customization for a specific dataset.
#' 
#' @param dataset_name Determines which dataset to return filters for
#' @return
#' @example
#' /dontrun {
#' req <- GetDatasetCustomization(token, "asas")
#' }
#' @export
GetDatasetCustomization <- function(token, dataset_name) {
    req <- SendRequest(token, 
        "dataset-get-customization", 
        params = list(datasetName = dataset_name)
    )
    return(req$data)
}


GetDatasetCustomizations <- function(token, dataset_names = list(), 
    metadata_type = list()
) {

}


#' This method is used to retrieve all metadata fields for a given dataset.
#' 
#' @param token The API key.
#' @param dataset_name Determines which dataset to return filters for
#' @return Metadata information
#' @example
#' /dontrun {
#' req <- GetDatasetMetadata(token, "asas")
#' }
#' @export
GetDatasetMetadata <- function(token, dataset_name) {
    req <- SendRequest(token, 
        path = "dataset-metadata", 
        params = list(datasetName = dataset_name)
    )
    return(req$data)
}


#' Lists all available order products for a dataset - this does not guarantee 
#' scene availability.
#' @param token The API key.
#' @param dataset_name Determines which dataset to return filters for
#' @return Order product information
#' @example
#' /dontrun {
#' req <- GetDatasetOrderProducts(token, "gls_all")
#' }
#' @export
GetDatasetOrderProducts <- function(token, dataset_name) {
    req <- SendRequest(token, 
        path = "dataset-order-products", 
        params = list(datasetName = dataset_name)
    )
    return(req$data)
}









