# ============================================================
# FUNCTION: find_shortest_path
# ============================================================
# Purpose : Find a shortest path between two nodes in the questionnaire graph.
# Inputs  : A, start_node, end_node, vnames
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

find_shortest_path <- function(A, start_node, end_node, vnames) { 
	nr <- nrow(A)
	# Inizializziamo una coda per la BFS
	queue <- list(start_node)
	visited <- rep(FALSE, nr)  # Tracciamo i nodi visitati
	parent <- rep(NA, nr)  # Per ricostruire il percorso
	# BFS per trovare il percorso
	while (length(queue) > 0) {
		current <- queue[[1]]
		queue <- queue[-1]  # Rimuoviamo il primo elemento della coda
		if (current == end_node) {  
			# Se siamo arrivati all'ultimo nodo, ricostruiamo il percorso
			path <- c(end_node)
			while (!is.na(parent[path[1]])) {
				path <- c(parent[path[1]], path)
      		}
      		vpath <- vnames[path]
			return(list(vars=vpath, length=length(path), exists_path=1))  # Restituisce il percorso come indici
		}
		# Esploriamo i nodi successivi (colonne con 1)
		for (next_node in which(A[current, ] == 1)) {
			if (!visited[next_node]) {
				queue <- append(queue, next_node)
				visited[next_node] <- TRUE
				parent[next_node] <- current  # Memorizziamo il predecessore
      		}
		}
	}
	return(list(vars=NULL, length=0, exists_path=0))  # Nessun percorso trovato
}
