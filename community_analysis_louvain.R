
# 1. Matrice temporale e nomi dei nodi
features_temporal <- getmatrix(model_sat, "beta")
labelsAbb <- rownames(features_temporal)
n_nodes <- length(labelsAbb)


# === LIBRERIE NECESSARIE ===
library(qgraph)
library(igraph)
library(reshape2)
library(ggplot2)

# === STEP 1: Creazione rete e grafo igraph (non-diretto) ===

# Parametri
threshold <- 0.1         # Soglia per filtrare archi deboli
labelsAbb <- rownames(features_temporal)   # Etichette nodi

# Crea grafo non-diretto da qgraph
g1 <- qgraph(
  features_temporal,
  threshold = threshold,
  labels    = labelsAbb,
  diag      = TRUE,
  directed  = FALSE,       # <--- fondamentale!
  layout    = "spring"
)

g_igraph <- as.igraph(g1, attributes = TRUE)

# Rendi pesi tutti positivi
E(g_igraph)$weight <- abs(E(g_igraph)$weight)

# Ora puoi lanciare Louvain
comm <- cluster_louvain(g_igraph)

# === STEP 2: Community detection con Louvain ===

comm <- cluster_louvain(g_igraph)

# Info community
print(comm)
cat("Numero di community rilevate:", length(unique(membership(comm))), "\n")

# === STEP 3: Visualizzazione ===

# Plot base
plot(comm, g_igraph,
     vertex.label = labelsAbb,
     vertex.size = 6,
     edge.arrow.size = 0.3,
     labels = labelsAbb,
     layout = layout_with_fr,
     main = "Louvain Community Detection")

# === STEP 4: Heatmap di co-occorrenza (opzionale per Louvain singolo run) ===
# NB: il metodo Louvain restituisce una sola partizione, quindi non si usa qui la co-occurrence matrix

# === STEP 5: Salva risultati (opzionale) ===

# Aggiungi membership come attributo ai nodi
V(g_igraph)$community <- membership(comm)

# Esporta se vuoi
# write.graph(g_igraph, file = "community_graph.graphml", format = "graphml")
# write.csv(membership(comm), file = "louvain_communities.csv")



??graph_from_adjacency_matrix
#### Louvain ma per ogni time point



# === STEP 0 - Prepara i dati ===
# Supponiamo che `net_impute` contenga tutti i dati (con PANSS imputati e regressati)
# Estrai i 30 item PANSS
symptoms <- colnames(net_impute)[grepl("^PANSS(?!T1|T2)", colnames(net_impute), perl = TRUE)]

# Crea i tre subset per T0, T1, T2
data_t0 <- net_impute %>% dplyr::select(all_of(symptoms))
data_t1 <- net_impute %>% dplyr::select(all_of(paste0("T1", symptoms)))
data_t2 <- net_impute %>% dplyr::select(all_of(paste0("T2", symptoms)))

# Rinominare le colonne per uniformità
colnames(data_t1) <- colnames(data_t2) <- colnames(data_t0)

# === STEP 1 - Stima GGM con EBICglasso ===
ggm_t0 <- qgraph::EBICglasso(cor(data_t0), n = nrow(data_t0))
ggm_t1 <- qgraph::EBICglasso(cor(data_t1), n = nrow(data_t1))
ggm_t2 <- qgraph::EBICglasso(cor(data_t2), n = nrow(data_t2))

# === STEP 2 - Conversione in grafi igraph ===
g_t0 <- graph_from_adjacency_matrix(ggm_t0, mode = "undirected", weighted = TRUE, diag = FALSE)
g_t1 <- graph_from_adjacency_matrix(ggm_t1, mode = "undirected", weighted = TRUE, diag = FALSE)
g_t2 <- graph_from_adjacency_matrix(ggm_t2, mode = "undirected", weighted = TRUE, diag = FALSE)

# Assicurati che i pesi siano positivi
E(g_t0)$weight <- abs(E(g_t0)$weight)
E(g_t1)$weight <- abs(E(g_t1)$weight)
E(g_t2)$weight <- abs(E(g_t2)$weight)

# === STEP 3 - Community detection con Louvain ===
comm_t0 <- cluster_louvain(g_t0)
comm_t1 <- cluster_louvain(g_t1)
comm_t2 <- cluster_louvain(g_t2)

# === STEP 4 - Output e visualizzazione ===
cat("Numero community T0:", length(unique(membership(comm_t0))), "\n")
cat("Numero community T1:", length(unique(membership(comm_t1))), "\n")
cat("Numero community T2:", length(unique(membership(comm_t2))), "\n")

# === STEP 5 - Plot dei grafi con community (Publication-friendly) ===

library(RColorBrewer)

# Palette colori
num_communities <- max(c(
  length(unique(membership(comm_t0))),
  length(unique(membership(comm_t1))),
  length(unique(membership(comm_t2)))
))
col_palette <- brewer.pal(max(3, num_communities), "Set2")

# === Abbreviazione etichette ===
abbrev_labels <- function(labels) {
  sub("PANSS([PNG])", "\\1", labels)  # Esempio: PANSSP1 -> P1, PANSSN6 -> N6, PANSSG12 -> G12
}
labels_t0 <- abbrev_labels(colnames(data_t0))
labels_t1 <- abbrev_labels(colnames(data_t1))
labels_t2 <- abbrev_labels(colnames(data_t2))

# === T0 ===
plot(
  comm_t0, g_t0,
  vertex.label     = labels_t0,
  vertex.size      = 24,
  vertex.label.cex = 1.4,
  edge.width       = abs(E(g_t0)$weight) * 5,           # più visibili
  edge.color       = rgb(0.2, 0.2, 0.2, alpha = 0.7),    # più scuri e meno trasparenti
  vertex.color     = col_palette[membership(comm_t0)],
  layout           = layout_with_fr(g_t0),
  main             = "T0 - Louvain Community"
)

# === T1 ===
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

# === T2 ===
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






