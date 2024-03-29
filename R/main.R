#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   setwd("~/R packages/plusCode2")

#' @title Latitude normalization
#' @description
#' Limit latitude within coherence range (-90, 90)
#' @param latitude A number.
#' @return A number.
#' @examples
#' normalizeLatitude(80)
#' normalizeLatitude(100)
#' @export
normalizeLatitude <- function(latitude) {
  LATITUDE_MAX <- 90
  return(min(max(latitude, -LATITUDE_MAX), LATITUDE_MAX))
}

#' @title Longitude normalization
#' @description Limit longitude within coherence range (-180, 180)
#' @param longitude A number.
#' @return A number.
#' @examples
#' normalizeLongitude(100)
#' normalizeLongitude(181)
#' @export
normalizeLongitude <- function(longitude) {
  LONGITUDE_MAX <- 180
  if (longitude >= -LONGITUDE_MAX && longitude < LONGITUDE_MAX) {
    return(longitude)
  }
  CIRCLE_DEG <- 2 * LONGITUDE_MAX
  return((longitude %% CIRCLE_DEG + CIRCLE_DEG + LONGITUDE_MAX) %% CIRCLE_DEG - LONGITUDE_MAX)
}

#' @title Longitude normalization
#' @description Calculates the latitude precision value for a given code length.
#' Lengths <= 10 have the same precision for latitude and longitude,
#' but lengths > 10 have different precisions due to the grid method having fewer columns than rows.
#' @param codeLength A number.
#' @return A number.
#' @examples
#' computeLatitudePrecision(10)
#' computeLatitudePrecision(12)
#' @export
computeLatitudePrecision <- function(codeLength) {
  CODE_PRECISION_NORMAL <- 10
  CODE_ALPHABET <- "23456789CFGHJMPQRVWX"
  ENCODING_BASE <- nchar(CODE_ALPHABET)
  GRID_ROWS <- 5
  PAIR_CODE_LENGTH <- 10
  if (codeLength <= CODE_PRECISION_NORMAL) {
    return ((ENCODING_BASE ^ (codeLength / -2 + 2)))
  } else {
    return ((ENCODING_BASE ^ -3) / (GRID_ROWS ^ (codeLength - PAIR_CODE_LENGTH)))
  }
}

#' @title Check and convert input object to sfc_POINT object
#' @description
#' Checks on the input object and any conversions:
#'
#' - Check that the input object is of length 1, if it is not an error appears.
#'
#' - Verifies that the input object is of the sfc (Simple Features Collection) class, if it is not, it returns NA and reports a warning.
#'
#' - If the input object is of sfc class, checks that it is of type POINT, if it is not, the centroid of the geometric object is calculated.
#'
#' - If the input object is of the sfc_POINT class, check that it has a reference system and that this is the WGS84 for the decimal coordinates; if it is not, it is set or converted.
#' @param x A generic object.
#' @return An object of class sfc_POINT.
#' @examples
#' library(sf)
#' checkAndConvert(0)
#' ###
#' polygon <- st_sfc(st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))))
#' checkAndConvert(polygon)
#' ###
#' point <- st_sfc(st_point(c(0, 0)))
#' checkAndConvert(point)
#' @export
checkAndConvert <- function(x){
  if (length(x) > 1) {
    stop("Input must be of length 1")
  }
  if (!startsWith(class(x), "sfc_")[1]) {
    warning("Input must be of class sfc or (sf, data.frame), class x -> ",class(x)[1])
    return(NA)
  }
  if (st_geometry_type(x) != "POINT") {
    warning ("Input is not a sfc_POINT, class x -> ", st_geometry_type(x)[1])
    x <- st_centroid(x)
  }
  if (is.na(st_crs(x))) {
    x <- st_set_crs(x, 4326)
  }
  if (st_crs(x)[1] != "EPSG:4326") {
    x <- st_transform(x, crs = 4326)
  }
  return(x)
}

#' @title Deal with data frame
#' @description Adapt the genPluscode function to a class object (sf, data.frame), it returns a vector
#' containing the pluscode of the coordinates in the records of the input data frame.
#' @param x An object of class (sf, data.frame).
#' @param codeLength A number.
#' @return A character vector of length nrow(x).
#' @examples
#' library(sf)
#' coordinates <- data.frame(lon = -6.56718, lat = 52.50145)
#' geom_df <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)
#' dealWithDf(geom_df, 12)
#' ###
#' coordinates_df <- data.frame(lon = c(-6.56718, -6), lat = c(52.50145, 52))
#' geom_df <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
#' dealWithDf(geom_df, 12)
#' @export
dealWithDf <- function(x, codeLength){
  lista_df <- lapply(1:nrow(x), function(i) x[i, , drop = FALSE]$geometry)
  x <- mapply(genPluscode, lista_df, codeLength)
  return(x)
}

#' @title Generate Plus Code
#' @description Main function of the package. It generates a plus code giving as input a sfc object or a sf data.frame and a code length.
#' Code length is related to the precision as dimension of the area.
#' The precision of a plus code is indicated by the number of digits after the "+" sign. For example:
#'
#' 10 (2 digits after "+") -> area of 13.7 by 13.7 meters.
#'
#' 11 (3 digits after "+") -> area of 2.7 by 3.5 meters.
#'
#' 12 (4 digits after "+") -> area of 0.5 by 0.8 meters.
#' @param geom An object of class sfc or (sf, data.frame).
#' @param codeLength An optional number between 4 and 15 exluding 5, 7, 9; default is 10.
#' @return Depending on input it returns a character or an object of class (sf, data.frame).
#' @examples
#' library(sf)
#' point <- st_sfc(st_point(c(0, 0)))
#' genPluscode(point,10)
#' ###
#' coordinates_df <- data.frame(lon = c(-6.56718, -6), lat = c(52.50145, 52))
#' geom_df <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
#' genPluscode(geom_df)
#' @export
genPluscode <- function(geom, codeLength = 10) {

  # "Constants"
  CODE_PRECISION_NORMAL <- 10
  CODE_ALPHABET <- "23456789CFGHJMPQRVWX"
  SEPARATOR <- "+"
  PADDING_CHARACTER <- "0"
  SEPARATOR_POSITION <- 8
  MAX_DIGIT_COUNT <- 15
  PAIR_CODE_LENGTH <- 10
  GRID_CODE_LENGTH <- MAX_DIGIT_COUNT - PAIR_CODE_LENGTH
  ENCODING_BASE <- nchar(CODE_ALPHABET)
  LATITUDE_MAX <- 90
  LONGITUDE_MAX <- 180
  GRID_COLUMNS <- 4
  GRID_ROWS <- 5
  LAT_INTEGER_MULTIPLIER <- 8000 * 3125
  LNG_INTEGER_MULTIPLIER <- 8000 * 1024
  LAT_MSP_VALUE <- LAT_INTEGER_MULTIPLIER * ENCODING_BASE * ENCODING_BASE
  LNG_MSP_VALUE <- LNG_INTEGER_MULTIPLIER * ENCODING_BASE * ENCODING_BASE

  # Limit the maximum number of digits in the code.
  codeLength <- min(codeLength, MAX_DIGIT_COUNT)

  # Verify that the code length is valid.
  if (codeLength < PAIR_CODE_LENGTH && codeLength %% 2 == 1 || codeLength < 4) {
    stop("Lunghezza del codice non valida -->  ", codeLength)
  }

  # If geom is a dataframe the function is adapted
  if(all(class(geom)==c("sf","data.frame"))){
    plusCodeArray <- dealWithDf(geom, codeLength)
    df <- cbind(geom, data.frame(plusCode = plusCodeArray))
    return(df)
  }

  # Input evaluation
  geom_point <- checkAndConvert(geom)
  if (is.na(geom_point)) {return(NA)}
  coords <- st_coordinates(geom_point)
  longitude <- coords[1]
  latitude <- coords[2]

  # Coordinates coerence check
  if (abs(latitude) > 10000 || abs(longitude) > 10000) {
    warning("Coordinate non valide --> ", latitude, ", ", longitude)
    return(NA)
  }

  # Ensure coordinates validity
  latitude <- normalizeLatitude(latitude)
  longitude <- normalizeLongitude(longitude)

  # Latitude 90 needs to be adjusted to be slightly lower, so that the return code can be decoded
  if (latitude == LATITUDE_MAX) {
    latitude <- latitude - 0.9 * computeLatitudePrecision(codeLength)
  }

  # Memorize the code: we build it in reverse order and reorder it later
  revCodeBuilder <- character()

  # Calculate the code
  latVal <- round((latitude + LATITUDE_MAX) * LAT_INTEGER_MULTIPLIER * 1e6) / 1e6
  lngVal <- round((longitude + LONGITUDE_MAX) * LNG_INTEGER_MULTIPLIER * 1e6) / 1e6

  # Calculate the grid part of the code if necessary (if code > 10)
  if (codeLength > PAIR_CODE_LENGTH) {
    for (i in 1:GRID_CODE_LENGTH) {
      latDigit <- latVal %% GRID_ROWS
      lngDigit <- lngVal %% GRID_COLUMNS
      ndx <- (latDigit * GRID_COLUMNS + lngDigit) + 1
      revCodeBuilder <- paste0(revCodeBuilder, substr(CODE_ALPHABET, ndx, ndx))
      latVal <- latVal %/% GRID_ROWS
      lngVal <- lngVal %/% GRID_COLUMNS
    }
  } else {
    latVal <- latVal / GRID_ROWS^GRID_CODE_LENGTH
    lngVal <- lngVal / GRID_COLUMNS^GRID_CODE_LENGTH
  }
  # Calculate the pair section of the code
  for (i in 1:(PAIR_CODE_LENGTH / 2)) {
    revCodeBuilder <- paste0(revCodeBuilder, substr(CODE_ALPHABET, (lngVal %% ENCODING_BASE) + 1, (lngVal %% ENCODING_BASE) + 1))
    revCodeBuilder <- paste0(revCodeBuilder, substr(CODE_ALPHABET, (latVal %% ENCODING_BASE) + 1, (latVal %% ENCODING_BASE) + 1))
    latVal <- latVal %/% ENCODING_BASE
    lngVal <- lngVal %/% ENCODING_BASE
    if (i == 1) {
      revCodeBuilder <- paste0(revCodeBuilder, SEPARATOR)
    }
  }
  # Revert the code
  codeBuilder <- codeBuilder <- paste(rev(strsplit(revCodeBuilder, "")[[1]]), collapse = "")

  # If needed to add zeros, it replaces some of the digits.
  if (codeLength < SEPARATOR_POSITION) {
    for (i in (codeLength + 1):SEPARATOR_POSITION) {
      codeBuilder[i] <- PADDING_CHARACTER
    }
  }

  code <- substr(codeBuilder, 1, max(SEPARATOR_POSITION + 1, codeLength + 1))
  return(code)
}
