#' Conversion of meteorological data from basin scale to sub-basin scale
#'
#' @param x either a `GRiwrm` network description (See [CreateGRiwrm]), a [character] id of a node, or a [matrix] containing meteorological data
#' @param ... Parameters passed to the methods
#'
#' @return [matrix] a matrix containing the converted meteorological data
#' @export
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()]
#' @rdname ConvertMeteoSD
#'
ConvertMeteoSD <- function(x, ...) {
  UseMethod("ConvertMeteoSD")
}

#' @param meteo [matrix] or [data.frame] containing meteorological data. Its [colnames] should be equal to the ID of the basins
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.GRiwrm <- function(x, meteo, ...) {
  meteo <- as.matrix(meteo)
  np <- getAllNodesProperties(x)
  id_runoff <- np$id[np$RunOff]
  if (any(!id_runoff %in% colnames(meteo))) {
    stop("`meteo` column names should contain at least: ",
         paste(id_runoff, collapse = ", "))
  }
  output <- lapply(id_runoff, ConvertMeteoSD, griwrm = x, meteo = meteo, ...)
  meteoOut <- do.call(cbind, output)
  dimnames(meteoOut)[[2]] <- id_runoff
  return(meteoOut)
}

#' @param griwrm `GRiwrm` object describing the semi-distributed network (See [CreateGRiwrm])
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.character <- function(x, griwrm, meteo, ...) {
  griwrm <- griwrm[getDiversionRows(griwrm, inverse = TRUE), ]
  upperIDs <- getUpstreamRunOffIds(x, griwrm)
  if (length(upperIDs) == 1) {
    return(meteo[,x])
  }
  areas <- griwrm$area[match(upperIDs, griwrm$id)]
  output <- ConvertMeteoSD(
    meteo[, upperIDs, drop = FALSE],
    areas = areas,
    ...
  )
  return(output)
}

#' @param areas [numeric] vector with the total area of the basin followed by the areas of the upstream basins in km2
#' @param temperature [logical] `TRUE` if the meteorological data contain air temperature. If `FALSE` minimum output values are bounded to zero
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.matrix <- function(x, areas, temperature = FALSE, ...) {
  # Check arguments
  if (nrow(x) < 2) {
    stop("Meteorological data matrix should contain more than one row")
  }
  if (length(areas) != ncol(x)) {
    stop("'areas' length and meteo data matrix number of columns should be equal")
  }
  if (areas[1] <= sum(areas[-1])) {
    stop("Basin area 'areas[1]' should be greater than the sum of the upstream sub-basin areas")
  }
  if (ncol(x) == 1) {
    return(x)
  }
  # Convert mm to 1E3 m3
  V <- x * rep(areas, rep(nrow(x), length(areas)))
  # Sum upstream data
  if (ncol(x) > 2) {
    Vup <- rowSums(V[,-1])
  } else {
    Vup <- V[,2]
  }
  # Remove to basin to get downstream data
  Vdown <- V[,1] - Vup
  if (!temperature) Vdown[Vdown < 0] <- 0
  # Convert to mm
  meteoDown <- Vdown / (areas[1] - sum(areas[-1]))
  return(as.matrix(meteoDown, ncol = 1))
}

getUpstreamRunOffIds <- function(id, griwrm) {
  griwrm <- griwrm[getDiversionRows(griwrm, inverse = TRUE), ]
  upstreamNodeIds <- griwrm$id[griwrm$down == id & !is.na(griwrm$down)]
  upstreamRunOffIds <- griwrm$id[griwrm$id %in% upstreamNodeIds & !is.na(griwrm$area)]
  upstreamNaAreaIds <- upstreamNodeIds[!upstreamNodeIds %in% upstreamRunOffIds]
  if (length(upstreamNaAreaIds) > 0) {
    upstreamRunOffIds <-  c(
      upstreamRunOffIds,
      unlist(sapply(upstreamNaAreaIds, getUpstreamRunOffIds, griwrm = griwrm))
    )
    upstreamRunOffIds <- upstreamRunOffIds[!is.na(griwrm$area[griwrm$id %in% upstreamRunOffIds])]
  }

  if (is.na(griwrm$area[griwrm$id == id])) {
    return(upstreamRunOffIds)
  }

  return(c(id, upstreamRunOffIds))
}
