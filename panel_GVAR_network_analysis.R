source("script/pre-processing_ALL_COVARIATES.R")


# Load the dataset
data <- read_csv("data/residuals.csv")



##############################################
# 1. Data preparation
##############################################

# prendo solo gli item PANSS - ricorda che il dataset qui è di 302 elementi
data <- data %>% 
  dplyr::select(contains("PANSS")) %>% 
  as_tibble()


# nomi di tutte le colonne
all_names <- colnames(data)

# sintomi al baseline: quelli che NON iniziano con "T1" o "T2"
symptoms <- all_names[!grepl("^T[0-9]", all_names)]
length(symptoms)          # dovrebbe essere 30
symptoms                  # tipo "PANSSP1", "PANSSP2", ...

# time–points (prefissi)
waves <- c("", "T1", "T2")   # "" = baseline, poi T1, T2

# costruisco la design matrix: righe = sintomi, colonne = time–points
design_mat <- sapply(
  waves,
  function(w) if (w == "") symptoms else paste0(w, symptoms)
)

# ora: nrow = 30 variabili, ncol = 3 time–points
dim(design_mat)
# [1] 30  3

# assegno alle righe il nome del sintomo (senza prefisso di tempo)
rownames(design_mat) <- symptoms

##############################################
# 2. STIMO IL MODELLO PANELGVAR
##############################################

model <- panelgvar(
  data      = data,
  vars      = design_mat,
  estimator = "FIML",
  storedata = TRUE
)

set.seed(1234)

model_sat <- model %>%
  runmodel() # saturated model


model_pruned <- model_sat %>%
  prune(alpha = 0.1, adjust = "none") # pruned model

## fit indices

fit_sat <- model_sat %>% fit()

fit_pruned <- model_pruned %>% fit()



# Compare if saturated or pruned model performs better
psychonetrics::compare(
  saturate = model_sat,
  sparse   = model_pruned
)

# estraggo le tre matrici di interesse - saturated
features_temporal        <- getmatrix(model_sat, "beta")
features_contemporaneous <- getmatrix(model_sat, "omega_zeta_within")
features_between         <- getmatrix(model_sat, "omega_zeta_between")


# estraggo le tre matrici di interesse - pruned
features_temporal        <- getmatrix(model_pruned, "beta")
features_contemporaneous <- getmatrix(model_pruned, "omega_zeta_within")
features_between         <- getmatrix(model_pruned, "omega_zeta_between")

# tutte e tre dovrebbero essere 30 x 30
dim(features_temporal)
dim(features_contemporaneous)
dim(features_between)

##############################################
# 3. LABELS PER I NODI
##############################################

# uso direttamente i nomi dei sintomi (senza T1/T2)
labels_clean <- rownames(design_mat)    # "PANSSP1", "PANSSP2", ...

##############################################
# 4. PLOT DELLE TRE RETI
##############################################

g_temp <- qgraph(
  features_temporal,
  layout  = "spring",
  directed = TRUE,     # forza il trattamento come rete direzionale
  diag    = TRUE,      # mostra le autoregressioni
  labels  = labels_clean,
  curveAll = T,
  curve = 0, 
  vsize   = 6,
  asize   = 4,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title   = "Temporal network (lag-1 PANSS)"
)

g_cont <- qgraph(
  features_contemporaneous,
  layout  = g_temp$layout,
  directed = FALSE,
  diag   = FALSE,
  labels = labels_clean,
  curveAll = F,
  curve = 1,
  vsize  = 6,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title  = "Contemporaneous network (within-person)"
)

g_between <- qgraph(
  features_between,
  layout  = g_temp$layout,
  directed = FALSE,
  diag   = FALSE,
  labels = labels_clean,
  vsize  = 6,
  curveAll = F,
  curve = 1,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title  = "Between-subjects network"
)








#########################################################################################
#                              CENTRALITY - RADARPLOTS                                  #
#########################################################################################



# --------------------------------------
# RADAR CHART della rete temporale
# --------------------------------------

# 1. Calcolo InStrength e OutStrength dalla matrice beta (rete temporale)
in_strength  <- colSums(abs(features_temporal))   # quanto ogni sintomo riceve
out_strength <- rowSums(abs(features_temporal))   # quanto ogni sintomo invia

# 2. Raggruppo per il radar chart (formato richiesto da fmsb)
library(fmsb)

# Creo data.frame per radar (max, min, valori)
global_strength <- c(in_strength, out_strength)
max_min_data <- matrix(NA, ncol = length(in_strength), nrow = 4)
max_min_data[1,] <- max(global_strength)                   # max
max_min_data[2,] <- rep(0, length(in_strength))            # min
max_min_data[3,] <- in_strength                            # InStrength
max_min_data[4,] <- out_strength                           # OutStrength

rownames(max_min_data) <- c("max", "min", "InStrength", "OutStrength")
colnames(max_min_data) <- labels_clean  # le etichette dei nodi già definite prima
max_min_data <- as.data.frame(max_min_data)

# 3. Colori
library(scales)
colors_in <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4))

# 4. Funzione di plotting (estetica)
create_beautiful_radarchart <- function(data, 
                                        color = "#00AFBB", 
                                        vlabels = colnames(data), 
                                        vlcex = 0.7,
                                        caxislabels = NULL, 
                                        title = NULL, ...) { radarchart(
                                          data, axistype = 1,
                                          pcol = color, 
                                          pfcol = scales::alpha(color, 0.5), 
                                          plwd = 2, plty = 1,
                                          cglcol = "black", cglty = 1, cglwd = 0.5,
                                          seg = 4,
                                          axislabcol = "black",
                                          vlcex = vlcex, vlabels = vlabels,
                                          caxislabels = caxislabels, title = title, ...
                                        )
}




# 5. Etichette degli assi
caxislabels = seq(0, round(max(global_strength), 2), length.out = 5)
caxislabels_rounded <- round(caxislabels, 2)


create_beautiful_radarchart(
  max_min_data,
  color = colors_in,
  caxislabels = caxislabels_rounded,
  title = "Temporal Network Strength (In vs Out)"
)

legend(x = 1, y = 1.2, 
       legend = rownames(max_min_data[-c(1, 2),]), 
       bty = "n", pch = 20,
       col = colors_in, text.col = "black",
       cex = 1.2, pt.cex = 3)


# 6. Plot finale
tiff(filename = "results/radar_temporal_strength_adjusted.tiff", width = 3000, height = 2000, res = 300)


dev.off()



