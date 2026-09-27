# ============================================================
# FUNCTION: generate_groups_OR
# ============================================================
# Purpose : Generate coarsened logical OR variables in the record-level data.
# Inputs  : data, routing_file_ungrouped, routing_file_grouped
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

generate_groups_OR <- function(data, routing_file_ungrouped, routing_file_grouped) {
	grs <- unique(routing_file_ungrouped[routing_file_ungrouped$V5 > 0, "V5"])
	for(k in grs) {
		vars <- routing_file_ungrouped[routing_file_ungrouped$V5 == k, "V1"]
		condition <- paste(paste0("data$", vars), "> 0", collapse=" | ")
		target_var <- paste0("VOR", k)
		data[, target_var] <- NA
		data[, target_var] <- ifelse(with(data, eval(parse(text = condition))), 1, data[, target_var])
		data <- data[, !(names(data) %in% vars)] 
	}
	data <- data[, routing_file_grouped]
    return(data)
}
