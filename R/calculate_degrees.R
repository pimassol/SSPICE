# ============================================================
# FUNCTION: calculate_degrees
# ============================================================
# Purpose : Calculate node degree measures from the adjacency matrix.
# Inputs  : A
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

calculate_degrees <- function(A) {
	nr <- nrow(A)
	nc <- ncol(A)
	out_degree <- (rowSums(A))  # Somma delle righe per il grado in uscita
	in_degree <- (colSums(A))   # Somma delle colonne per il grado in entrata
	total_degree <- in_degree + out_degree  # Grado totale del nodo
	return(list(in_degree = in_degree, out_degree = out_degree, total_degree = total_degree))
}
