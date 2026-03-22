# Load the dataset
data <- read_csv("residuals.csv")



##############################################
# 1. Data preparation
##############################################

# Keep PANSS items only
# Note: the dataset used here contains 302 individuals
data <- data %>% 
  dplyr::select(contains("PANSS")) %>% 
  as_tibble()


# Names of all columns
all_names <- colnames(data)

# Baseline symptoms: those that do NOT start with "T1" or "T2"
symptoms <- all_names[!grepl("^T[0-9]", all_names)]
length(symptoms)          # should be 30
symptoms                  # e.g., "PANSSP1", "PANSSP2", ...

# Time-points (prefixes)
waves <- c("", "T1", "T2")   # "" = baseline, then T1, T2

# Build the design matrix: rows = symptoms, columns = time-points
design_mat <- sapply(
  waves,
  function(w) if (w == "") symptoms else paste0(w, symptoms)
)

# At this stage: nrow = 30 variables, ncol = 3 time-points
dim(design_mat)
# [1] 30  3

# Assign row names using the symptom name (without time prefix)
rownames(design_mat) <- symptoms

##############################################
# 2. ESTIMATE THE PANEL GVAR MODEL
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

## Fit indices

fit_sat <- model_sat %>% fit()

fit_pruned <- model_pruned %>% fit()



# Compare the saturated and pruned models
psychonetrics::compare(
  saturate = model_sat,
  sparse   = model_pruned
)



# Extract the three matrices of interest from the saturated model
features_temporal        <- getmatrix(model_sat, "beta")
features_contemporaneous <- getmatrix(model_sat, "omega_zeta_within")
features_between         <- getmatrix(model_sat, "omega_zeta_between")



##############################################
# 3. NODE LABELS
##############################################

# Use the symptom names directly (without T1/T2 prefixes)
labels_clean <- rownames(design_mat)    # "PANSSP1", "PANSSP2", ...

# Convert "PANSSP1" -> "P1", "PANSSN7" -> "N7", "PANSSG12" -> "G12"
labels_short <- sub("^PANSS", "", labels_clean)

##############################################
# 4. PLOT THE THREE NETWORKS
##############################################

# Temporal network (directed, including autoregressive effects on the diagonal)
g_temp <- qgraph(
  features_temporal,
  layout  = "spring",
  directed = TRUE,     # force the matrix to be treated as a directed network
  diag    = TRUE,      # show autoregressive effects
  labels  = labels_short,
  curveAll = T,
  curve = 0, 
  vsize   = 6,
  asize   = 4,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title   = "Temporal network (lag-1 PANSS)"
)


# 1. Compute in-strength and out-strength from the beta matrix (temporal network)
in_strength  <- colSums(abs(features_temporal))   # how much each symptom receives
out_strength <- rowSums(abs(features_temporal))   # how much each symptom sends


in_strength

out_strength

# 2. Organize the data for the radar chart (format required by fmsb)
# 3. Radar data (format required by fmsb::radarchart)
library(fmsb)

global_strength <- c(in_strength, out_strength)

max_min_data <- matrix(NA, ncol = length(in_strength), nrow = 4)
max_min_data[1,] <- max(global_strength)              # max
max_min_data[2,] <- rep(0, length(in_strength))       # min
max_min_data[3,] <- in_strength
max_min_data[4,] <- out_strength

rownames(max_min_data) <- c("max", "min", "InStrength", "OutStrength")
colnames(max_min_data) <- labels_short

max_min_data <- as.data.frame(max_min_data)

# 4. Radar chart
library(scales)

colors_in <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4))

caxislabels <- seq(0, round(max(global_strength), 2), length.out = 5)
caxislabels_rounded <- round(caxislabels, 2)

radarchart(
  max_min_data, axistype = 1,
  pcol  = colors_in,
  pfcol = scales::alpha(colors_in, 0.5),
  plwd  = 2, plty = 1,
  cglcol = "black", cglty = 1, cglwd = 0.5,
  seg = 4,
  axislabcol = "black",
  vlcex = 0.7,
  caxislabels = caxislabels_rounded,
  title = "Temporal Network Strength (In vs Out)"
)

legend(
  x = 1, y = 1.2,
  legend = rownames(max_min_data[-c(1,2),]),
  bty = "n", pch = 20,
  col = colors_in, text.col = "black",
  cex = 1.2, pt.cex = 3
)



# Contemporaneous network (within-person, undirected)
g_cont <- qgraph(
  features_contemporaneous,
  layout  = g_temp$layout,   # use the same layout as the temporal network for comparability
  directed = FALSE,
  diag   = FALSE,
  labels = labels_short,
  curveAll = F,
  curve = 0,
  vsize  = 6,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title  = "Contemporaneous network (within-person)"
)



# Between-subject network (undirected)
g_between <- qgraph(
  features_between,
  layout  = g_temp$layout,   # use the same layout as the temporal network for comparability
  directed = FALSE,
  diag   = FALSE,
  labels = labels_short,
  vsize  = 6,
  curveAll = F,
  curve = 0,
  edge.labels = FALSE,
  threshold = 0.20,
  theme = "colorblind",
  title  = "Between-subjects network"
)
