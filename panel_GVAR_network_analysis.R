source("script/pre-processing_ALL_COVARIATES.R")

# Load the dataset
data <- read_csv("data/residuals.csv")

##############################################
# 1. Data preparation
##############################################

# Select only PANSS items – note: this dataset has 302 participants
data <- data %>% 
  dplyr::select(contains("PANSS")) %>% 
  as_tibble()

# Get all column names
all_names <- colnames(data)

# Baseline symptoms: those that do NOT start with "T1" or "T2"
symptoms <- all_names[!grepl("^T[0-9]", all_names)]
length(symptoms)          # should be 30
symptoms                  # e.g., "PANSSP1", "PANSSP2", ...

# Timepoints (prefixes)
waves <- c("", "T1", "T2")   # "" = baseline, then T1, T2

# Build design matrix: rows = symptoms, columns = timepoints
design_mat <- sapply(
  waves,
  function(w) if (w == "") symptoms else paste0(w, symptoms)
)

# Now: nrow = 30 variables, ncol = 3 timepoints
dim(design_mat)

# Assign row names to the matrix (symptom name without time prefix)
rownames(design_mat) <- symptoms

##############################################
# 2. Estimate the PANEL GVAR model
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

# Compare saturated vs pruned model
psychonetrics::compare(
  saturate = model_sat,
  sparse   = model_pruned
)

# Extract the three networks - saturated
features_temporal        <- getmatrix(model_sat, "beta")
features_contemporaneous <- getmatrix(model_sat, "omega_zeta_within")
features_between         <- getmatrix(model_sat, "omega_zeta_between")

# Extract the three networks - pruned
features_temporal        <- getmatrix(model_pruned, "beta")
features_contemporaneous <- getmatrix(model_pruned, "omega_zeta_within")
features_between         <- getmatrix(model_pruned, "omega_zeta_between")

# All matrices should be 30 x 30
dim(features_temporal)
dim(features_contemporaneous)
dim(features_between)

##############################################
# 3. Node labels
##############################################

# Use symptom names directly (without T1/T2 prefix)
labels_clean <- rownames(design_mat)    # e.g., "PANSSP1", ...

##############################################
# 4. Plot the three networks
##############################################

g_temp <- qgraph(
  features_temporal,
  layout  = "spring",
  directed = TRUE,     # force directed network
  diag    = TRUE,      # show autoregressions
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
# RADAR CHART for the temporal network
# --------------------------------------

# 1. Compute InStrength and OutStrength from beta matrix (temporal network)
in_strength  <- colSums(abs(features_temporal))   # how much each symptom receives
out_strength <- rowSums(abs(features_temporal))   # how much each symptom sends

# 2. Prepare radar chart data (format required by fmsb)
library(fmsb)

global_strength <- c(in_strength, out_strength)
max_min_data <- matrix(NA, ncol = length(in_strength), nrow = 4)
max_min_data[1,] <- max(global_strength)                   # max value
max_min_data[2,] <- rep(0, length(in_strength))            # min value
max_min_data[3,] <- in_strength                            # InStrength values
max_min_data[4,] <- out_strength                           # OutStrength values

rownames(max_min_data) <- c("max", "min", "InStrength", "OutStrength")
colnames(max_min_data) <- labels_clean  # node labels
max_min_data <- as.data.frame(max_min_data)

# 3. Colors
library(scales)
colors_in <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4))

# 4. Radar chart plotting function (aesthetic version)
create_beautiful_radarchart <- function(data, 
                                        color = "#00AFBB", 
                                        vlabels = colnames(data), 
                                        vlcex = 0.7,
                                        caxislabels = NULL, 
                                        title = NULL, ...) { 
  radarchart(
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

# 5. Axis labels
caxislabels = seq(0, round(max(global_strength), 2), length.out = 5)
caxislabels_rounded <- round(caxislabels, 2)

# 6. Create radar chart
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

# 7. Save radar plot
tiff(filename = "results/radar_temporal_strength_adjusted.tiff", width = 3000, height = 2000, res = 300)

dev.off()
