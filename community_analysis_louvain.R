#### Louvain per each time point ####

# === STEP 0 - Prepare the data ===
# Assume `net_impute` contains all data (PANSS variables, imputed and residualized)
# Extract 30 PANSS items
symptoms <- colnames(net_impute)[grepl("^PANSS(?!T1|T2)", colnames(net_impute), perl = TRUE)]

# Create subsets for T0, T1, T2
data_t0 <- net_impute %>% dplyr::select(all_of(symptoms))
data_t1 <- net_impute %>% dplyr::select(all_of(paste0("T1", symptoms)))
data_t2 <- net_impute %>% dplyr::select(all_of(paste0("T2", symptoms)))

# Rename columns for consistency
colnames(data_t1) <- colnames(data_t2) <- colnames(data_t0)

# === STEP 1 - Estimate GGM using EBICglasso ===
ggm_t0 <- qgraph::EBICglasso(cor(data_t0), n = nrow(data_t0))
ggm_t1 <- qgraph::EBICglasso(cor(data_t1), n = nrow(data_t1))
ggm_t2 <- qgraph::EBICglasso(cor(data_t2), n = nrow(data_t2))

# === STEP 2 - Convert to igraph objects ===
g_t0 <- graph_from_adjacency_matrix(ggm_t0, mode = "undirected", weighted = TRUE, diag = FALSE)
g_t1 <- graph_from_adjacency_matrix(ggm_t1, mode = "undirected", weighted = TRUE, diag = FALSE)
g_t2 <- graph_from_adjacency_matrix(ggm_t2, mode = "undirected", weighted = TRUE, diag = FALSE)

# Ensure edge weights are positive
E(g_t0)$weight <- abs(E(g_t0)$weight)
E(g_t1)$weight <- abs(E(g_t1)$weight)
E(g_t2)$weight <- abs(E(g_t2)$weight)

# === STEP 3 - Community detection using Louvain ===
comm_t0 <- cluster_louvain(g_t0)
comm_t1 <- cluster_louvain(g_t1)
comm_t2 <- cluster_louvain(g_t2)

# === STEP 4 - Output & summary ===
cat("Number of communities at T0:", length(unique(membership(comm_t0))), "\n")
cat("Number of communities at T1:", length(unique(membership(comm_t1))), "\n")
cat("Number of communities at T2:", length(unique(membership(comm_t2))), "\n")

# === STEP 5 - Plot the graphs with communities (Publication-friendly) ===

library(RColorBrewer)

# Color palette
num_communities <- max(c(
  length(unique(membership(comm_t0))),
  length(unique(membership(comm_t1))),
  length(unique(membership(comm_t2)))
))
col_palette <- brewer.pal(max(3, num_communities), "Set2")

# === Abbreviate node labels ===
abbrev_labels <- function(labels) {
  sub("PANSS([PNG])", "\\1", labels)  # e.g., PANSSP1 -> P1, PANSSN6 -> N6, PANSSG12 -> G12
}
labels_t0 <- abbrev_labels(colnames(data_t0))
labels_t1 <- abbrev_labels(colnames(data_t1))
labels_t2 <- abbrev_labels(colnames(data_t2))

# === T0 Plot ===
plot(
  comm_t0, g_t0,
  vertex.label     = labels_t0,
  vertex.size      = 24,
  vertex.label.cex = 1.4,
  edge.width       = abs(E(g_t0)$weight) * 5,           # more visible
  edge.color       = rgb(0.2, 0.2, 0.2, alpha = 0.7),    # darker and less transparent
  vertex.color     = col_palette[membership(comm_t0)],
  layout           = layout_with_fr(g_t0),
  main             = "T0 - Louvain Community"
)

# === T1 Plot ===
plot(
  comm_t1, g_t1,
  vertex.label     = labels_t1,
  vertex.size      = 24,
  vertex.label.cex = 1.4,
  edge.width       = abs(E(g_t1)$weight) * 5,
  edge.color       = rgb(0.2, 0.2, 0.2, alpha = 0.7),
  vertex.color     = col_palette[membership(comm_t1)],
  layout           = layout_with_fr(g_t1),
  main             = "T1 - Louvain Community"
)

# === T2 Plot ===
plot(
  comm_t2, g_t2,
  vertex.label     = labels_t2,
  vertex.size      = 24,
  vertex.label.cex = 1.4,
  edge.width       = abs(E(g_t2)$weight) * 5,
  edge.color       = rgb(0.2, 0.2, 0.2, alpha = 0.7),
  vertex.color     = col_palette[membership(comm_t2)],
  layout           = layout_with_fr(g_t2),
  main             = "T2 - Louvain Community"
)
