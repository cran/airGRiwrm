#' @rdname CreateCalibOptions
#' @export
#' @importFrom stats setNames
CreateCalibOptions.GRiwrmInputsModel <- function(x, FixedParam = NULL, ...) {
  dots <- list(...)
  if ("IsHyst" %in% names(dots)) {
    warning("The parameter `IsHyst` will be ignored. It should be defined before with `CreateInputsModel`")
  }
  np <- getAllNodesProperties(attr(x, "GRiwrm"))
  np <- np[!np$DirectInjection, ]
  gaugedIds <- np$id[np$gauged]

  if (!is.null(FixedParam)) {
    if (!(is.list(FixedParam) || is.numeric(FixedParam))) {
      stop("Argument `FixedParam` should be of type numeric or list")
    }
    if (!is.list(FixedParam)) {
      FixedParam <- list("*" = FixedParam)
    }
    if (!all(names(FixedParam) %in% c(np$id, "*"))) {
      stop("Each item of the list `FixedParam` should correspond to a node ids:\n",
           "Unknown id(s): ", paste(names(FixedParam)[which(!names(FixedParam) %in% gaugedIds)], sep = ", "))
    }
    if (!all(sapply(FixedParam, is.numeric))) {
      stop("All items of the list `FixedParam` should be numeric")
    }
    if ("*" %in% names(FixedParam)) {
      aFP <- FixedParam[["*"]]
      FixedParam <- lapply(
        setNames(nm = gaugedIds),
        function(id) {
          if (is.null(FixedParam[[id]])) {
            FP <- aFP[x[[id]]$model$indexParamUngauged]
            if (all(is.na(FP))) FP <- NULL
            return(FP)
          } else {
            return(FixedParam[[id]])
          }
        }
      )
    }
  }

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  for (id in np$id) {
    IM <- x[[id]]
    FP <- NULL
    if (!is.null(FixedParam)) {
      FP <- FixedParam[[id]]
    }
    if (np$gauged[np$id == id]) {
      CalibOptions[[IM$id]] <- CreateCalibOptions(
        IM,
        FixedParam = FP,
        ...
      )
    } else if(!is.null(FP)) {
      CalibOptions[[IM$id]] <- list(FixedParam = FP)
    }
  }

  return(CalibOptions)
}
