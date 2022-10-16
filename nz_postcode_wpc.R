library(rvest)
library(httr)
library(sf)

# globals ----------------------------------------------------------------------
root <- "https://www.worldpostalcodes.org"

# utils ------------------------------------------------------------------------
`%/%` <- function(x, y) sprintf("%s/%s", x, y)

#' get postcodes for a single New Zealand region
#'
#' @param url, e.g. https://www.worldpostalcodes.org/l1/en/nz/new-zealand/list/r1/list-of-postcodes-in-auckland
#' @returns a data frame listing all postcodes and postcode names.
get_region <- function(url) {
  process_row <- function(row) {
    td <- html_elements(row, "td")
    if (length(td) != 3) return(NULL)
    code <- html_text(td[1])
    name <- html_text(td[2])
    url <- root %/% {td[1] |> html_element("a") |> html_attr("href") }
    data.frame(code = code, name = name, url = url)
  }

  rows <- GET(url, timeout(180)) |> read_html() |> html_elements("tr")
  lapply(rows, process_row) |> (\(x) do.call(rbind, x))()
}

#' get postcodes for all of New Zealand
#'
#' @returns a data frame listing all postcodes and postcode names.
get_postcode_list <- function() {
  regions <- read_html(root %/% "search/l1/en/nz/new-zealand-postcodes") |>
    html_elements("tr > td > a") |>
    html_attr("href") |>
    sapply(\(x) root %/% x,USE.NAMES = FALSE)

  lapply(regions, \(x) {
    cat(x, "\n")
    Sys.sleep(1)
    get_region(x)
  }) |>
    (\(x) do.call(rbind, x))()
}

#' get the polygons for a collection of postcodes
#'
#' @param xs a list of postcodes, e.g. \code{c("5018", "5019")}
#' @returns an sf / multipolygon object containing the requested postcodes
get_postcode_shapes <- function(xs) {
  lapply(xs, \(x) {
    cat("\t", x, "\n", sep = "")

    json <- GET(
      sprintf("https://s3.amazonaws.com/gm-wpc/gj/cntry/nz/nz_pc_%s.geojson", x),
      timeout(180)
    ) |> content(as = "text", encoding = "UTF-8")

    res <- st_read(json, drivers = "geojson", quiet = TRUE)
    colnames(res) <- c("code", "description", "geom")
    st_geometry(res) <- "geom"
    res
  }) |>
    (\(x) do.call(rbind, x))()
}

# create feature class ---------------------------------------------------------
postcodes <- get_postcode_list()
postcodes_sf <- get_postcode_shapes(postcodes$code)

# make valid.  lot's of edges cross edges.
sf_use_s2(FALSE)
postcodes_sf <- st_make_valid(postcodes_sf)

# get rid of geometry collections
geom <- lapply(st_geometry(postcodes_sf), \(x) {
  if ("GEOMETRYCOLLECTION" %in% class(x)) {
     res <- st_collection_extract(x, type = "POLYGON")
     if (length(res) > 1) st_multipolygon(res) else res
  } else x
})

postcodes_sf$geom <- st_sfc(geom, crs = 4326)

# export -----------------------------------------------------------------------
st_write(
  postcodes_sf |>
    st_transform(2193),
  "data/postcode.gpkg",
  append = FALSE
)
