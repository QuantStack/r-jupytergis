# JupyterGIS document widget, mirroring the Python `GISDocument`

#' Generate a random RFC 4122-style UUID string.
#'
#' @return A length-one character vector containing a 36-character UUID.
#' @noRd
.uuid <- function() {
  bytes <- sample.int(256L, 16L, replace = TRUE) - 1L
  hex <- sprintf("%02x", bytes)
  paste0(
    paste(hex[1:4], collapse = ""),
    "-",
    paste(hex[5:6], collapse = ""),
    "-",
    paste(hex[7:8], collapse = ""),
    "-",
    paste(hex[9:10], collapse = ""),
    "-",
    paste(hex[11:16], collapse = "")
  )
}

#' Build comm metadata for the JupyterGIS widget.
#'
#' @param path Path to a `.jGIS`, `.qgz`, or `.qgs` file, or `NULL` for an
#'   ephemeral in-memory document.
#' @return A named list of comm metadata fields consumed by the frontend.
#' @noRd
.make_comm_metadata <- function(path) {
  if (is.null(path)) {
    return(list(
      ymodel_name = "@jupytergis:widget",
      path = NULL,
      format = NULL,
      contentType = NULL,
      create_ydoc = TRUE
    ))
  }

  ext <- tolower(tools::file_ext(path))
  if (ext == "jgis") {
    format <- "text"
    contentType <- "jgis"
  } else if (ext == "qgz") {
    format <- "base64"
    contentType <- "QGZ"
  } else if (ext == "qgs") {
    format <- "base64"
    contentType <- "QGS"
  } else {
    stop("File extension is not supported: ", ext)
  }

  list(
    ymodel_name = "@jupytergis:widget",
    path = path,
    format = format,
    contentType = contentType,
    create_ydoc = FALSE
  )
}

#' JupyterGIS document
#'
#' Comm-backed widget mirroring `jupytergis_lab.GISDocument`. Exposes the
#' document's CRDT roots (`layers`, `sources`, `options`, `layerTree`,
#' `metadata`) as fields, and provides `add_raster_layer()`.
#'
#' @export
GISDocument <- R6::R6Class(
  "GISDocument",
  inherit = ywidgets::CommRootWidget,

  public = list(
    #' @field layers Shared `yr::Map` of layer definitions keyed by layer id.
    layers = NULL,
    #' @field sources Shared `yr::Map` of source definitions keyed by source id.
    sources = NULL,
    #' @field options Shared `yr::Map` of document-level options.
    options = NULL,
    #' @field layerTree Shared `yr::Array` describing layer ordering and grouping.
    layerTree = NULL,
    #' @field metadata Shared `yr::Map` of document metadata.
    metadata = NULL,

    #' @description Create a new GISDocument.
    #' @param path Path to a `.jGIS`, `.qgz`, or `.qgs` file. `NULL` creates an
    #'   ephemeral in-memory document.
    #' @param ydoc Optional existing `yr::Doc` to adopt.
    initialize = function(path = NULL, ydoc = NULL) {
      super$initialize(
        ydoc = ydoc,
        comm_metadata = .make_comm_metadata(path)
      )

      self$layers <- self$register_storage(
        "layers",
        yr::Prelim$map(list())
      )$read()
      self$sources <- self$register_storage(
        "sources",
        yr::Prelim$map(list())
      )$read()
      self$options <- self$register_storage(
        "options",
        yr::Prelim$map(list())
      )$read()
      self$layerTree <- self$register_storage(
        "layerTree",
        yr::Prelim$array(list(), recursive = FALSE)
      )$read()
      self$metadata <- self$register_storage(
        "metadata",
        yr::Prelim$map(list())
      )$read()
    },

    #' @description Add a Raster Layer to the document.
    #' @param url Tiles URL.
    #' @param name Display name for the layer.
    #' @param attribution Attribution text.
    #' @param opacity Layer opacity in [0, 1].
    #' @param url_parameters Extra URL parameters for tile requests.
    #' @return The new layer id.
    add_raster_layer = function(
      url,
      name = "Raster Layer",
      attribution = "",
      opacity = 1,
      url_parameters = NULL
    ) {
      source_id <- .uuid()
      layer_id <- .uuid()

      source <- list(
        type = "RasterSource",
        name = paste0(name, " Source"),
        parameters = list(
          url = url,
          # Doubles, not integers: yr::Prelim$any does not serialize R
          # integer vectors (`0L`/`24L`) — they become an empty map `{}`,
          # which breaks OpenLayers' tile grid and renders a blank layer.
          minZoom = 0,
          maxZoom = 24,
          attribution = attribution,
          htmlAttribution = attribution,
          provider = "",
          bounds = list(),
          urlParameters = if (is.null(url_parameters)) {
            structure(list(), names = character(0))
          } else {
            url_parameters
          }
        )
      )

      layer <- list(
        type = "RasterLayer",
        name = name,
        visible = TRUE,
        parameters = list(
          source = source_id,
          opacity = opacity,
          color = structure(list(), names = character(0))
        )
      )

      # Source, layer and layer-tree entries must be written in separate
      # transactions: the JupyterGIS frontend decides whether to *add* or
      # *update* a layer by checking if it is already in the layer tree, so a
      # layer and its tree entry arriving in one transaction make it take the
      # update path on a layer that was never added (mainView `_onLayersChanged`).
      self$with_write(function(trans) {
        self$sources$insert(trans, source_id, yr::Prelim$any(source))
      })
      self$with_write(function(trans) {
        self$layers$insert(trans, layer_id, yr::Prelim$any(layer))
      })
      self$with_write(function(trans) {
        self$layerTree$insert(
          trans,
          self$layerTree$len(trans),
          yr::Prelim$any(layer_id)
        )
      })

      layer_id
    }
  )
)

#' `hera::mime_types` method for `GISDocument`.
#'
#' @param x A `GISDocument`.
#' @return A character vector of supported MIME types.
#' @noRd
mime_types.GISDocument <- function(x) {
  c("text/plain", "application/vnd.jupyter.ywidget-view+json")
}

#' `hera::mime_bundle` method for `GISDocument`.
#'
#' @param x A `GISDocument`.
#' @param mimetypes MIME types to include in the bundle.
#' @param ... Unused.
#' @return A list with `data` and `metadata` entries suitable for Jupyter display.
#' @noRd
mime_bundle.GISDocument <- function(x, mimetypes = hera::mime_types(x), ...) {
  list(
    data = list(
      "text/plain" = "",
      "application/vnd.jupyter.ywidget-view+json" = list(
        version_major = 2L,
        version_minor = 0L,
        model_id = x$comm_id()
      )
    ),
    metadata = structure(list(), names = character(0))
  )
}

registerS3method(
  "mime_types",
  "GISDocument",
  mime_types.GISDocument,
  envir = asNamespace("hera")
)
registerS3method(
  "mime_bundle",
  "GISDocument",
  mime_bundle.GISDocument,
  envir = asNamespace("hera")
)
