# ============================================================
# FUNCTION: create_incidence_matrix
# ============================================================
# Purpose : Build the transposed incidence matrix from questionnaire routing metadata.
# Inputs  : routes, vnames
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

create_incidence_matrix <- function(routes, vnames) {
	routes <- as.matrix(routes)
	nodes <- routes[, 1]  			# colonna 1: nomi delle variabili
	number_of_items <- routes[, 2]	# colonna 2: numero di items delle variabili; se = 1 e VMAX > VMINla variabile è numerica
	# ricodifico a 1 per i campi strings (nitema=0) e i campi come codici provincia/comune (nitems=-1)
	number_of_items[number_of_items <= 0] <- 1 
	min_item <- routes[, 3]			# colonna 3: item/valore minimo delle variabili
	max_item <- routes[, 4]			# colonna 4: item/valore massimo delle variabili
	groups <- routes[, 5] 			# colonna 5: gruppo logico minimo delle variabili
	leaps <- routes[, 6] 			# colonna 6: numero max di salti dalla variabile (leaps <= nitems-1 e il 98 indica il salto incondizionato)
	nV <- nrow(routes)
	# si considerano gli archi multipli
	nE <- sum(number_of_items) 
	# levo il numero di arche che partirebbero dell'ultimo nodo
	nE = nE - number_of_items[nV]	
	# trasposta della matrice di incidenza
	B_transposed <- matrix(0, nrow = nE, ncol = nV)
	# costruzione della matrice di in cidenza
	ri <- 1
	for (k in 1:(nV - 1)) {
		nitems <- number_of_items[k]
		vtype <- groups[k + 1]
		nroutes <- leaps[k]
		v <- matrix(0, nrow = nitems, ncol = nV)
		# se la variabile target fa parte di un gruppo XOR	
		if (vtype < 0) {
			target <- finditem(groups, vtype, "=")
		} else {
			target <- k + 1
		}
		# creo l'arco pr la variabile k-esima
		v[, target] <- -1
		v[, k] <- 1
		# se la variabile k-esima produce dei salti
		if (nroutes > 0) {
			# se il salto è incondizionato, altrimenti 
			nlimit <- ifelse(nroutes == 98, 1, nroutes) 
    		# considero tutti i salti indicati nel file per la variabile k-esima
			for (j in 1:nlimit) {
				item <- routes[k, (2 * j) + 5]
				scaled_item <- NA
				if (!is.na(item)) scaled_item <- item_scaling(item, nitems, min_item[k], max_item[k])
				if (is.na(scaled_item)) scaled_item <- 1:nitems
				jump <- routes[k, (2 * j) + 6]
				if (!is.na(jump)) {
        			v[scaled_item, target] <- 0
        			v[scaled_item, k] <- 1
        			vtype <- groups[jump]
        			if (vtype < 0 || vtype > 0) {
						jump <- finditem(groups, vtype, "=")
        			}
					v[scaled_item, jump] <- -1
				}
			}
		}	
		rf <- ri + nitems - 1
		B_transposed[ri:rf, ] <- v
		ri <- rf + 1
	}
	colnames(B_transposed) <- vnames
	return(B_transposed)
}
