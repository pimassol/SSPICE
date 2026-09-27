# ============================================================
# FUNCTION: domain_values_from_routes
# ============================================================
# Purpose : Derive variable-domain values from routing and item-mapping metadata.
# Inputs  : routes_df, Bt, items.map.file, items.map.path
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

domain_values_from_routes <- function(routes_df, Bt, items.map.file=NULL, items.map.path=NULL) {
  vnames <- colnames(Bt)
  tmp <- routes_df[,1:4]
  tmp$V2 <- ifelse(is.na(tmp$V2) | tmp$V2 < 1, 1, tmp$V2)
  tmp$V3 <- ifelse(is.na(tmp$V3), 1, tmp$V3)
  exp_idx <- rep(seq_len(nrow(tmp)), tmp$V2)
  route_items <- unlist(mapply(function(start, end, n) seq(start, end, length.out = n),
                               tmp$V3, tmp$V4, tmp$V2))
  var_per_row <- vnames[tmp$V1[exp_idx]]
  df_items <- data.frame(varname = var_per_row, route_item = route_items)
  if (!is.null(items.map.file) && !is.null(items.map.path)) {
    mp <- read.csv(file.path(items.map.path, items.map.file), stringsAsFactors = FALSE)
    names(mp)[1:3] <- c("varname","route","dset")
    df_items <- merge(df_items, mp, by.x=c("varname","route_item"), by.y=c("varname","route"), all.x=TRUE)
    df_items$item <- ifelse(is.na(df_items$dset), df_items$route_item, df_items$dset)
  } else df_items$item <- df_items$route_item
  split(df_items$item, df_items$varname)
}
