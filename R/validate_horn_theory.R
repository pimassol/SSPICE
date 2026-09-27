#' Validate the Horn theory induced by a routing DAG
#'
#' Builds the elementary Horn basis induced by the modal rows of the
#' transposed incidence matrix, verifies empirical closure equivalence, and
#' evaluates rule indispensability through a deletion test.
#'
#' @param dag A DAG object produced by the SSPICE DAG-building pipeline.
#' @param rules_file Optional path to a `RULES.dat` file to compare with the
#'   elementary rules generated directly from `Bt`.
#' @param items_mapping_file Optional path to the item-mapping CSV file used
#'   to express modal values in the same coding adopted by `RULES.dat`.
#' @param n_random Number of random multi-seed closure tests.
#' @param max_seed_size Maximum size of a random seed set.
#' @param seed Random seed used for the empirical closure tests.
#' @param add_bridges Whether to add technical VALUE-to-FILLED implications.
#'
#' @return An object of class `sspice_horn_validation`.
#' @export
validate_horn_theory <- function(
  dag,
  rules_file = NULL,
  items_mapping_file = NULL,
  n_random = 5000L,
  max_seed_size = 8L,
  seed = 123L,
  add_bridges = TRUE
) {
  required <- c("routes_numeric", "varnames", "Bt", "A")
  missing <- setdiff(required, names(dag))

  if (length(missing)) {
    stop(
      "The DAG object is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!isTRUE(is_DAG(dag$A))) {
    stop("The routing graph is not a DAG.", call. = FALSE)
  }

  Bt <- as.matrix(dag$Bt)
  routes <- dag$routes_numeric
  varnames <- as.character(dag$varnames)
  expanded <- .sspice_expand_modal_items(
    routes,
    varnames,
    items_mapping_file
  )

  if (nrow(Bt) != nrow(expanded)) {
    stop(
      "The number of Bt rows differs from the number of modal transitions.",
      call. = FALSE
    )
  }

  rownames(Bt) <- paste0(
    expanded$source_var,
    "_",
    expanded$item_position
  )
  expanded$modal_row <- rownames(Bt)

  elementary <- .sspice_build_horn_edges(Bt, expanded)
  closure_basis <- if (isTRUE(add_bridges)) {
    .sspice_add_value_bridges(elementary)
  } else {
    elementary
  }
  complete <- .sspice_derive_horn_closure(closure_basis)

  facts <- sort(unique(c(
    closure_basis$lhs,
    closure_basis$rhs,
    complete$lhs,
    complete$rhs
  )))
  adjacency_elementary <- .sspice_horn_adjacency(closure_basis)
  adjacency_complete <- .sspice_horn_adjacency(complete)

  singleton <- .sspice_test_closure_singletons(
    facts,
    adjacency_elementary,
    adjacency_complete
  )
  random <- .sspice_test_closure_random(
    facts,
    adjacency_elementary,
    adjacency_complete,
    n_random,
    max_seed_size,
    seed
  )
  closure_tests <- if (nrow(random)) rbind(singleton, random) else singleton
  minimality <- .sspice_deletion_test(elementary)
  stored_rules <- .sspice_parse_rules(rules_file)
  rule_comparison <- .sspice_compare_stored_rules(elementary, stored_rules)

  canonical <- data.frame(
    test = c(
      "DAG",
      "ONE_SOURCE_PER_BT_ROW",
      "NONEMPTY_DESTINATION_PER_BT_ROW",
      "BT_ROWS_EQUAL_MODAL_TRANSITIONS",
      "UNIQUE_MODAL_IDENTIFIERS",
      "EVERY_BT_ROW_GENERATES_RULES"
    ),
    passed = c(
      TRUE,
      all(rowSums(Bt == 1L) == 1L),
      all(rowSums(Bt == -1L) >= 1L),
      nrow(Bt) == nrow(expanded),
      !anyDuplicated(rownames(Bt)),
      setequal(unique(elementary$bt_row), seq_len(nrow(Bt)))
    ),
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    measure = c(
      "canonical_tests_passed",
      "closure_tests_passed",
      "minimality_deletion_passed",
      "stored_rules_exactly_equal",
      "n_bt_rows",
      "n_elementary_rules",
      "n_derived_or_original_rules",
      "n_closure_tests",
      "n_indispensable_rules"
    ),
    value = c(
      all(canonical$passed),
      all(closure_tests$equivalent),
      all(minimality$indispensable),
      if (nrow(rule_comparison)) {
        rule_comparison$value[rule_comparison$comparison == "EXACT_EQUALITY"]
      } else {
        NA
      },
      nrow(Bt),
      nrow(elementary),
      nrow(complete),
      nrow(closure_tests),
      sum(minimality$indispensable)
    ),
    stringsAsFactors = FALSE
  )

  result <- list(
    summary = summary,
    canonical_tests = canonical,
    expanded_items = expanded,
    elementary_rules = elementary,
    closure_basis = closure_basis,
    complete_rule_set = complete,
    closure_tests = closure_tests,
    closure_failures = closure_tests[!closure_tests$equivalent, , drop = FALSE],
    minimality_tests = minimality,
    stored_rules = stored_rules,
    stored_rule_comparison = rule_comparison,
    Bt = Bt
  )
  class(result) <- "sspice_horn_validation"
  result
}

.sspice_parse_rules <- function(path) {
  if (is.null(path) || !file.exists(path)) return(data.frame())
  lines <- trimws(readLines(path, warn = FALSE, encoding = "unknown"))
  lines <- lines[nzchar(lines)]
  parsed <- vector("list", length(lines))
  for (i in seq_along(lines)) {
    match <- regexec("^([[:alnum:]_.]+)\\(([^)]*)\\)", lines[i], perl = TRUE)
    antecedent <- regmatches(lines[i], match)[[1L]]
    if (length(antecedent) != 3L) next
    lhs <- .sspice_literal_value(antecedent[2L], antecedent[3L])
    remainder <- substring(lines[i], attr(match[[1L]], "match.length") + 1L)
    tokens <- regmatches(
      remainder,
      gregexpr("([[:alnum:]_.]+)(\\(\\)|<)", remainder, perl = TRUE)
    )[[1L]]
    if (!length(tokens)) next
    parsed[[i]] <- do.call(rbind, lapply(tokens, function(token) {
      blank <- grepl("\\(\\)$", token)
      variable <- sub("(\\(\\)|<)$", "", token)
      data.frame(
        source_line = i,
        lhs = lhs,
        rhs = if (blank) .sspice_literal_blank(variable) else
          .sspice_literal_filled(variable),
        stringsAsFactors = FALSE
      )
    }))
  }
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) return(data.frame())
  result <- do.call(rbind, parsed)
  result$key <- .sspice_edge_key(result$lhs, result$rhs)
  result[!duplicated(result$key), , drop = FALSE]
}

.sspice_compare_stored_rules <- function(generated, stored) {
  if (!nrow(stored)) return(data.frame())
  generated_keys <- sort(unique(generated$key))
  stored_keys <- sort(unique(stored$key))
  data.frame(
    comparison = c(
      "GENERATED_ATOMIC_RULES", "STORED_ATOMIC_RULES", "COMMON_RULES",
      "ONLY_GENERATED", "ONLY_STORED", "EXACT_EQUALITY"
    ),
    value = c(
      length(generated_keys), length(stored_keys),
      length(intersect(generated_keys, stored_keys)),
      length(setdiff(generated_keys, stored_keys)),
      length(setdiff(stored_keys, generated_keys)),
      identical(generated_keys, stored_keys)
    ),
    stringsAsFactors = FALSE
  )
}

.sspice_literal_value <- function(variable, value) {
  paste0("VALUE:", trimws(variable), "=", trimws(as.character(value)))
}

.sspice_literal_blank <- function(variable) {
  paste0("BLANK:", trimws(variable))
}

.sspice_literal_filled <- function(variable) {
  paste0("FILLED:", trimws(variable))
}

.sspice_edge_key <- function(lhs, rhs) {
  paste(lhs, rhs, sep = "\u001F")
}

.sspice_expand_modal_items <- function(
  routes,
  varnames,
  items_mapping_file = NULL
) {
  nitems <- abs(suppressWarnings(as.integer(routes[, 2L])))
  nitems[is.na(nitems) | nitems <= 0L] <- 1L
  minimum <- suppressWarnings(as.numeric(routes[, 3L]))
  maximum <- suppressWarnings(as.numeric(routes[, 4L]))
  source_rows <- seq_len(nrow(routes) - 1L)

  output <- lapply(source_rows, function(i) {
    number <- nitems[i]
    lower <- if (is.na(minimum[i])) 1 else minimum[i]
    upper <- if (is.na(maximum[i])) lower else maximum[i]
    values <- if (number == 1L) {
      lower
    } else {
      seq(lower, upper, length.out = number)
    }

    data.frame(
      source_node = i,
      source_var = varnames[i],
      item_position = seq_len(number),
      item_value = values,
      bt_row = seq_len(number),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, output)
  result$bt_row <- seq_len(nrow(result))
  rownames(result) <- NULL

  if (!is.null(items_mapping_file)) {
    if (!file.exists(items_mapping_file)) {
      stop(
        "Item-mapping file not found: ",
        items_mapping_file,
        call. = FALSE
      )
    }

    item_map <- utils::read.csv(
      items_mapping_file,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (ncol(item_map) < 3L) {
      stop(
        "The item-mapping file must contain at least three columns.",
        call. = FALSE
      )
    }

    names(item_map)[1:3] <- c(
      "varname",
      "route",
      "dset"
    )

    for (i in seq_len(nrow(result))) {
      mapped_row <- which(
        item_map$varname == result$source_var[i] &
          as.character(item_map$route) ==
            as.character(result$item_value[i])
      )

      if (length(mapped_row) == 1L &&
          !is.na(item_map$dset[mapped_row])) {
        result$item_value[i] <- item_map$dset[mapped_row]
      }
    }
  }

  result
}

.sspice_build_horn_edges <- function(Bt, expanded) {
  varnames <- colnames(Bt)
  output <- vector("list", nrow(Bt))

  for (i in seq_len(nrow(Bt))) {
    from <- which(Bt[i, ] == 1L)
    targets <- which(Bt[i, ] == -1L)
    if (length(from) != 1L || !length(targets)) {
      stop("Invalid modal row in Bt: ", i, call. = FALSE)
    }

    lhs <- .sspice_literal_value(
      expanded$source_var[i],
      format(expanded$item_value[i], trim = TRUE, scientific = FALSE)
    )
    rows <- list()
    cursor <- 0L

    for (target in sort(targets)) {
      between <- if (target > from + 1L) {
        seq.int(from + 1L, target - 1L)
      } else {
        integer()
      }
      blanks <- between[Bt[i, between] == 0L]

      for (blank in blanks) {
        cursor <- cursor + 1L
        rows[[cursor]] <- data.frame(
          lhs = lhs,
          rhs = .sspice_literal_blank(varnames[blank]),
          rule_type = "SKIP_BLANK",
          bt_row = i,
          stringsAsFactors = FALSE
        )
      }

      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        lhs = lhs,
        rhs = .sspice_literal_filled(varnames[target]),
        rule_type = "TARGET_FILLED",
        bt_row = i,
        stringsAsFactors = FALSE
      )
    }
    output[[i]] <- do.call(rbind, rows)
  }

  result <- do.call(rbind, output)
  result$key <- .sspice_edge_key(result$lhs, result$rhs)
  result <- result[!duplicated(result$key), , drop = FALSE]
  rownames(result) <- NULL
  result
}

.sspice_add_value_bridges <- function(edges) {
  values <- sort(unique(edges$lhs[grepl("^VALUE:", edges$lhs)]))
  if (!length(values)) return(edges)
  variables <- sub("^VALUE:([^=]+)=.*$", "\\1", values)
  bridges <- data.frame(
    lhs = values,
    rhs = .sspice_literal_filled(variables),
    rule_type = "VALUE_TO_FILLED_BRIDGE",
    bt_row = NA_integer_,
    stringsAsFactors = FALSE
  )
  bridges$key <- .sspice_edge_key(bridges$lhs, bridges$rhs)
  result <- rbind(edges, bridges)
  result[!duplicated(result$key), , drop = FALSE]
}

.sspice_horn_adjacency <- function(edges) {
  if (!nrow(edges)) return(list())
  split(edges$rhs, edges$lhs)
}

.sspice_forward_closure <- function(seed_facts, adjacency) {
  reached <- sort(unique(as.character(seed_facts)))
  queue <- reached
  while (length(queue)) {
    current <- queue[1L]
    queue <- queue[-1L]
    next_facts <- adjacency[[current]]
    if (!is.null(next_facts)) {
      new_facts <- setdiff(next_facts, reached)
      if (length(new_facts)) {
        reached <- sort(unique(c(reached, new_facts)))
        queue <- c(queue, new_facts)
      }
    }
  }
  reached
}

.sspice_derive_horn_closure <- function(edges) {
  adjacency <- .sspice_horn_adjacency(edges)
  output <- lapply(sort(unique(edges$lhs)), function(lhs) {
    rhs <- setdiff(.sspice_forward_closure(lhs, adjacency), lhs)
    if (!length(rhs)) return(NULL)
    data.frame(lhs = lhs, rhs = rhs, stringsAsFactors = FALSE)
  })
  output <- Filter(Negate(is.null), output)
  if (!length(output)) return(data.frame(lhs = character(), rhs = character()))
  result <- do.call(rbind, output)
  result$key <- .sspice_edge_key(result$lhs, result$rhs)
  result <- result[!duplicated(result$key), , drop = FALSE]
  result$origin <- ifelse(result$key %in% edges$key, "ORIGINAL", "DERIVED")
  rownames(result) <- NULL
  result
}

.sspice_compare_closures <- function(seed, first, second) {
  a <- .sspice_forward_closure(seed, first)
  b <- .sspice_forward_closure(seed, second)
  only_a <- setdiff(a, b)
  only_b <- setdiff(b, a)
  list(
    equivalent = !length(only_a) && !length(only_b),
    n_a = length(a),
    n_b = length(b),
    difference = length(union(only_a, only_b)),
    only_a = only_a,
    only_b = only_b
  )
}

.sspice_closure_test_row <- function(seed, type, first, second) {
  comparison <- .sspice_compare_closures(seed, first, second)
  data.frame(
    test_type = type,
    seed = paste(seed, collapse = " | "),
    elementary_size = comparison$n_a,
    complete_size = comparison$n_b,
    symmetric_difference_size = comparison$difference,
    equivalent = comparison$equivalent,
    only_elementary = paste(comparison$only_a, collapse = " | "),
    only_complete = paste(comparison$only_b, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

.sspice_test_closure_singletons <- function(facts, first, second) {
  do.call(rbind, lapply(facts, function(fact) {
    .sspice_closure_test_row(fact, "SINGLETON_EXHAUSTIVE", first, second)
  }))
}

.sspice_test_closure_random <- function(
  facts,
  first,
  second,
  n_tests,
  max_size,
  seed
) {
  if (!length(facts) || n_tests <= 0L) return(data.frame())
  set.seed(seed)
  max_size <- min(max_size, length(facts))
  do.call(rbind, lapply(seq_len(n_tests), function(i) {
    size <- sample.int(max_size, 1L)
    selected <- sort(sample(facts, size, replace = FALSE))
    .sspice_closure_test_row(selected, "RANDOM_MULTI_SEED", first, second)
  }))
}

.sspice_deletion_test <- function(edges) {
  if (!nrow(edges)) return(data.frame())
  do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
    removed <- edges[i, , drop = FALSE]
    reduced <- edges[-i, , drop = FALSE]
    closure <- .sspice_forward_closure(
      removed$lhs,
      .sspice_horn_adjacency(reduced)
    )
    derivable <- removed$rhs %in% closure
    data.frame(
      edge_id = i,
      lhs = removed$lhs,
      rhs = removed$rhs,
      rule_type = removed$rule_type,
      bt_row = removed$bt_row,
      rhs_derivable_after_deletion = derivable,
      indispensable = !derivable,
      witness_reduced_closure = paste(closure, collapse = " | "),
      stringsAsFactors = FALSE
    )
  }))
}
