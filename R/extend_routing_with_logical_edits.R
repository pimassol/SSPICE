#' Extend routing metadata with terminal logical-edit nodes
#'
#' Appends one routing node for each LEDIT defined in a nine-column metadata
#' table. Every LEDIT has the accepted domain 1 and 2.
#'
#' @param routing Routing data frame or semicolon-separated file.
#' @param logical_edits Logical-edit data frame or semicolon-separated file.
#' @param separator Metadata separator.
#' @param na_strings Missing-value strings used in routing metadata.
#' @param output_file Optional extended-routing output file.
#'
#' @return Extended routing data frame.
#' @keywords internal
extend_routing_with_logical_edits <- function(
  routing,
  logical_edits,
  separator = ";",
  na_strings = c("", ".", "NA", "NULL"),
  output_file = NULL
) {
  read_metadata <- function(x, type) {
    if (is.character(x) && length(x) == 1L) {
      if (!file.exists(x)) {
        stop(type, " file not found: ", x, ".", call. = FALSE)
      }

      return(
        utils::read.csv(
          x,
          header = FALSE,
          sep = separator,
          colClasses = "character",
          na.strings = character(0),
          stringsAsFactors = FALSE,
          check.names = FALSE,
          strip.white = TRUE
        )
      )
    }

    if (is.data.frame(x)) {
      x[] <- lapply(x, as.character)
      return(x)
    }

    stop(type, " must be a file path or data frame.", call. = FALSE)
  }

  routing_data <- read_metadata(routing, "Routing metadata")
  edit_data <- read_metadata(logical_edits, "Logical-edit metadata")

  if (ncol(routing_data) < 6L) {
    stop("Routing metadata must have at least six columns.", call. = FALSE)
  }

  if (ncol(edit_data) != 9L) {
    stop(
      "Logical-edit metadata must have exactly nine columns.",
      call. = FALSE
    )
  }

  names(routing_data) <- paste0("V", seq_len(ncol(routing_data)))
  names(edit_data) <- paste0("V", seq_len(9L))
  edit_data[] <- lapply(edit_data, trimws)

  edit_names <- unique(edit_data$V1)
  edit_names <- edit_names[
    !is.na(edit_names) & edit_names != "" & edit_names != "."
  ]

  if (!length(edit_names)) {
    stop("No LEDIT variables were found.", call. = FALSE)
  }

  existing_names <- trimws(as.character(routing_data$V1))
  duplicated_names <- intersect(existing_names, edit_names)

  if (length(duplicated_names) > 0L) {
    stop(
      "LEDIT variables already present in routing metadata: ",
      paste(duplicated_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  for (edit_name in edit_names) {
    domain <- sort(unique(suppressWarnings(as.numeric(
      edit_data$V2[edit_data$V1 == edit_name]
    ))))

    if (!identical(domain, c(1, 2))) {
      stop(
        "The accepted domain of ",
        edit_name,
        " must be exactly {1, 2}.",
        call. = FALSE
      )
    }
  }

  added_rows <- lapply(
    edit_names,
    function(edit_name) {
      new_row <- as.list(rep(NA_character_, ncol(routing_data)))
      names(new_row) <- names(routing_data)
      new_row$V1 <- edit_name
      new_row$V2 <- "2"
      new_row$V3 <- "1"
      new_row$V4 <- "2"
      new_row$V5 <- "0"
      new_row$V6 <- "0"

      as.data.frame(
        new_row,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  )

  extended_routing <- do.call(
    rbind,
    c(list(routing_data), added_rows)
  )
  row.names(extended_routing) <- NULL

  if (!is.null(output_file)) {
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )

    utils::write.table(
      extended_routing,
      file = output_file,
      sep = separator,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      na = "."
    )
  }

  attr(extended_routing, "logical_edit_names") <- edit_names
  extended_routing
}
