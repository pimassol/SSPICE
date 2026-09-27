# ============================================================
# FUNCTION: coarse_groups_OR
# ============================================================
# Purpose : Collapse logical OR groups in the routing table into synthetic VOR nodes.
# Inputs  : a
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

coarse_groups_OR <- function(a) {
	a$cn <- 1:nrow(a) 
	grs <- unique(a[a$V5 > 0, "V5"])
	for(k in grs) {
		nvars <- nrow(a[a$V5 == k, ])
		vars <- a[a$V5 == k, "cn"]
		a <- a[-which(a$cn %in% vars[2:nvars]), ] 
		a[which(a$cn == vars[1]), "V1"] <- paste("VOR", k, sep="")
		a[which(a$cn == vars[1]), "V2"] <- 1
		a[which(a$cn == vars[1]), "V3"] <- 1
		a[which(a$cn == vars[1]), "V4"] <- 1
		a[which(a$cn == vars[1]), "V5"] <- 0
		a[which(a$cn == vars[1]), "V6"] <- 0
	}
	a <- a[, -ncol(a)]
	return(a)
}
