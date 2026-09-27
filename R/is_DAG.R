# ============================================================
# FUNCTION: is_DAG
# ============================================================
# Purpose : Check whether the supplied graph is a directed acyclic graph.
# Inputs  : A
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

is_DAG <- function(A) {
	nr <- nrow(A)
	in_degree <- colSums(A)  # Conta i gradi entranti per ogni nodo
	queue <- which(in_degree == 0)  # Trova i nodi senza predecessori
	count <- 0  # Conta quanti nodi vengono rimossi
	while (length(queue) > 0) {
		node <- queue[1]  # Prendi il primo nodo della coda
		queue <- queue[-1]  # Rimuovilo dalla coda
		count <- count + 1  # Aumenta il conteggio dei nodi processati
		# Per ogni nodo successivo (uscente da "node")
		for (next_node in which(A[node, ] == 1)) {
			in_degree[next_node] <- in_degree[next_node] - 1  # Riduci il grado entrante
			if (in_degree[next_node] == 0) {  # Se ora è zero, aggiungilo alla coda
				queue <- c(queue, next_node)
			}
		}
	}
	return(count == nr)  # Se abbiamo rimosso tutti i nodi, il grafo è un DAG
}
