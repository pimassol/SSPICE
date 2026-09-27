# ============================================================
# FUNCTION: check_all_paths
# ============================================================
# Purpose : Check record-level questionnaire paths against routing and incidence information.
# Inputs  : dset_data, routes, Bt, varkey, items.maps, path.items.maps
# Output  : Returns the object produced by the function body.
# Notes   : Item mapping is NA-safe: missing values remain missing.
# ============================================================

check_all_paths <- function(dset_data, 
                            routes, 
                            Bt, 
                            varkey, 
                            items.maps, 
                            path.items.maps) {
 
  vn_route <- colnames(Bt)
  
  # Costruzione percorso completo e lettura file CSV di mappatura ---
  mapping_file <- file.path(path.items.maps, items.maps)
  item_map <- read.csv(mapping_file, stringsAsFactors = FALSE, header = TRUE)
  
  # Ricodifica di min_item e max_item nel file di routing
  for (i in 1:nrow(routes)) {
    varname <- vn_route[routes[i, 1]]
    if (varname %in% item_map[, 1]) {
      mapping <- item_map[item_map$varname == varname, ]
    
      # Ricodifica min
      old_min <- routes[i, 3]
      new_min <- mapping[mapping[, 3] == old_min, 2]
      if (length(new_min) == 1) routes[i, 3] <- new_min
    
      # Ricodifica max
      old_max <- routes[i, 4]
      new_max <- mapping[mapping$old == old_max, 2]
      if (length(new_max) == 1) routes[i, 4] <- new_max
    }
  }

  # Ricodifica delle modalità nei salti (colonne 7, 9, 11, ...)
  jump_cols <- seq(7, ncol(routes), by = 2)

  for (col in jump_cols) {
    for (i in 1:nrow(routes)) {
      if (!is.na(routes[i, col]) && routes[i, col] != ".") {
        from_var <- routes[i, 1]
        old_code <- routes[i, col]
        mapping <- item_map[item_map[, 1] == from_var, ]
        new_code <- mapping[mapping[, 3] == old_code, 2]
        if (length(new_code) == 1) {
          routes[i, col] <- new_code
        }
      }
    }
  }
  
  # Applicazione della mappatura ai dati nel dataframe di input
  for (v in unique(item_map[, 1])) {
    if (v %in% names(dset_data)) {
      mapping <- item_map[item_map[, 1] == v, ]
      for (j in 1:nrow(mapping)) {
        matched_rows <- (
          !is.na(dset_data[[v]]) &
            as.character(dset_data[[v]]) ==
              as.character(mapping[j, 2])
        )
        dset_data[matched_rows, v] <- mapping[j, 3]
      }
      dset_data[[v]] <- as.numeric(dset_data[[v]])
    }
  }

  # numero di variabili del dataset
  nvars <- ncol(dset_data)
  # numero di records del dataset
  nrecs <- nrow(dset_data)
  
  # numero massimo di item per ogni variabile qualitativa 
  no_items <- abs(routes[, 2])
  min_item <- routes[, 3]
  max_item <- routes[, 4]
  vtype <- routes[, 5]
  no_items[no_items == 0] <- 1
  # metto la chiave ai record (se esiste nel dataset di input)	
  if (!is.na(varkey) && varkey %in% names(dset_data)) {
    keys <- dset_data[, varkey]
  } else {
    keys <- rep(NA, nrecs)
  }
  
  # codifica dummy: ogni variabile è trasformata in variabili binarie
  nitems <- sum(no_items)
  Ydummy <- matrix(0, nrecs, nitems)
  
  for (i in 1:nrecs) {
    Ydummy[i, ] <- dummize_vector(dset_data[i, ], no_items)
  }
  
  # inizializzazione matrice per report errori
  rept <- matrix(0, nrecs * nvars, 7)
  
  # aggiunta di una riga fittizia per gestire la variabile finale
  row_vect <- matrix(0, no_items[nvars], nvars)
  row_vect[, nvars] <- 1
  t_inc <- rbind(Bt, row_vect)
  
  row <- 1
  for (i in 1:nrecs) {
    nodes <- which(!is.na(dset_data[i, ]))
    datapath <- matrix(0, 1, nvars)
    datapath[nodes] <- 1
    edges <- which(Ydummy[i, ] == 1)
    le <- length(edges) - 1
    
    for (j in 1:le) {
      source_node <- which(t_inc[edges[j], ] == 1)
      target_node <- which(t_inc[edges[j], ] == -1)
      ltarget <- length(target_node)
      vmiss <- matrix(0, 1, ltarget)
      target_node <- target_node[ltarget]
      count_missing <- 0
      for (k in source_node:target_node) {
        errcode <- 0
        b <- datapath[k] - abs(t_inc[edges[j], k])
        l_group <- if_is_a_group(k, vtype)
        
        if (b > 0) {
          errcode <- 1
        } else if ((b < 0) & !is.na(min_item[k])) {
          if (l_group > 1) {
            count_missing <- count_missing + 1
            vmiss[count_missing] <- k
            if (count_missing == l_group) {
              errcode <- -2
            }
          } else {
            errcode <- -1
          }
        }
        
        valerr <- dset_data[i, k]
        
        if (errcode == 1 || errcode == -1) {
          varerr <- k
          valerr <- dset_data[i, k]
          routevar <- source_node
          routeval <- dset_data[i, routevar]
          
          rept[row, 1] <- i               # id del record
          rept[row, 2] <- as.numeric(keys[i])
          rept[row, 3] <- errcode         # codice errore
          rept[row, 4] <- varerr          # variabile in errore
          rept[row, 5] <- valerr          # valore della variabile
          rept[row, 6] <- routevar
          rept[row, 7] <- routeval
          
          row <- row + 1
          
        } else if (errcode == -2) {
          f_row <- row + ltarget - 1
          routevar <- source_node
          routeval <- dset_data[i, routevar]

          rept[row:f_row, 1] <- i
          rept[row:f_row, 2] <- as.numeric(keys[i])
          rept[row:f_row, 3] <- errcode
          rept[row:f_row, 4] <- t(vmiss)
          rept[row:f_row, 5] <- t(dset_data[i, vmiss])
          rept[row:f_row, 6] <- routevar
          rept[row:f_row, 7] <- routeval
          
          row <- f_row + 1
        }
      }
    }
  }
  
  rept <- as.data.frame(rept)
  names(rept) <- c("id_recprd", varkey, "errcode", "varname", "valname", "routevar", "routeval")
  rept <- rept[rept[, 3] != 0, ]
  return(rept)
}
