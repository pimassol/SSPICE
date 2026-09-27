# ============================================================
# FUNCTION: graph_partitions
# ============================================================
# Purpose : Partition graph paths subject to a maximum number of paths per partition.
# Inputs  : A, max_paths_per_partition
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

graph_partitions <- function(A, max_paths_per_partition) {
  num_nodes <- nrow(A)
  out_degree <- calculate_out_degree(A)
  visited <- rep(FALSE, num_nodes)
  partitions <- list()
  current_partition <- c()
  current_paths <- 1
  
  # Iniziamo dai nodi sorgente (senza predecessori)
  queue <- find_source_nodes(A)
  
  while (length(queue) > 0) {
    node <- queue[1]  # Prendiamo il primo nodo dalla coda (BFS)
    queue <- queue[-1]  # Rimuoviamo il nodo dalla coda
    
    if (!visited[node]) {
      visited[node] <- TRUE
      current_partition <- c(current_partition, node)
      current_paths <- current_paths * out_degree[node]
      
      # Se superiamo la soglia di percorsi, creiamo una nuova partizione
      if (current_paths >= max_paths_per_partition) {
        partitions[[length(partitions) + 1]] <- current_partition
        current_partition <- c()
        current_paths <- 1
      }
      
      # Aggiungiamo i successori del nodo alla coda
      successors <- which(A[node, ] == 1)
      for (succ in successors) {
        if (!visited[succ]) {
          queue <- append(queue, succ)
        }
      }
    }
  }
  
  # Aggiungiamo l'ultima partizione se contiene nodi
  if (length(current_partition) > 0) {
    partitions[[length(partitions) + 1]] <- current_partition
  }
  
  return(partitions)
}
