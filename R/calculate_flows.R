# ============================================================
# FUNCTION: calculate_flows
# ============================================================
# Purpose : Calculate questionnaire flows using the spanning tree and record data.
# Inputs  : stree, list_df, routes, t_inc
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

calculate_flows <- function(stree, list_df, routes, t_inc) {
  vtypes <- routes[, 5]
  nv <- nrow(list_df)
  deg_out <- rep(0, nv)
  flow <- rep(0, nv)
  flow[1] <- 1  # Nodo iniziale

  # Calcolo gradi in uscita
  for (i in 1:nrow(stree)) {
    deg_out[stree$origin[i]] <- deg_out[stree$origin[i]] + 1
  }
  deg_out[deg_out == 0] <- 1  # per evitare divisioni per zero

  # Calcolo flussi
  for (k in 1:nrow(stree)) {
    s <- stree$origin[k]
    t <- stree$target[k]
    if (vtypes[s] < 0) {
      flow[t] <- flow[t] + 0.5 * flow[s] / deg_out[s]
    } else {
      flow[t] <- flow[t] + flow[s] / deg_out[s]
    }
  }

  return(data.frame(node = 1:nv, flow = round(flow, 3)))
}
