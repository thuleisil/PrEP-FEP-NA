source("script/setup.R")

# === STEP 0 - Prepare data ===
# Let's use the dropped dataset - with 368 complete cases
# Is it possible to increase to 368???

net_impute_vars <- prep_df_drop %>% 
  dplyr::select(contains("PANSS"))

# Get all column names
all_names <- colnames(net_impute_vars)

# Baseline symptoms: those NOT starting with "T1" or "T2"
symptoms <- all_names[!grepl("^T[0-9]", all_names)]
length(symptoms)          # should be 30
symptoms                  # e.g. "PANSSP1", "PANSSP2", ...

# Time points (prefixes)
waves <- c("", "T1", "T2")   # "" = baseline, then T1, T2

# Build a design matrix: rows = symptoms, columns = time points
design_mat <- sapply(
  waves,
  function(w) if (w == "") symptoms else paste0(w, symptoms)
)

# Assign row names based on the symptom (without time prefix)
rownames(design_mat) <- symptoms

# === STEP 1 - Residualisation: regress out covariates ===
# Covariates: age, gender, medication, total words, etc.

## 1. Working dataset: covariates + all PANSS (90 variables)
panss_vars <- as.vector(design_mat)      # All PANSS variables across 3 timepoints
panss_vars <- unique(panss_vars)        # Just in case, remove duplicates

covars <- c(
  "GENDER", "ETA", "ETNIA", "ANTIPSIC1", "ANTIDEP1", "STABILIZ1", "BENZO" 
)

net_impute <- prep_df_drop %>%
  dplyr::select(all_of(c(covars, panss_vars))) %>%
  as.data.frame()

## 2. Loop through each symptom × time point: regress on covariates and replace with residuals

n_v <- nrow(design_mat)   # 30 symptoms
n_t <- ncol(design_mat)   # 3 time points

set.seed(1234)

for (k in 1:n_v) {
  for (i in 1:n_t) {
    
    x_var <- design_mat[k, i]   # e.g. "PANSSP1", "T1PANSSP1", "T2PANSSP1"
    
    # Build formula: e.g. "PANSSP1 ~ GENDER + AGE + ..."
    f <- as.formula(
      paste(x_var, "~", paste(covars, collapse = " + "))
    )
    
    fit <- lm(f, data = net_impute)
    
    # Replace original values with residuals
    net_impute[[x_var]] <- residuals(fit)
  }
}

## 3. Center, scale & detrend PANSS variables only

net_impute <- net_impute %>%
  mutate(across(all_of(panss_vars),
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

dim(net_impute)

# Save final cleaned dataset
write.csv(net_impute, "data/residuals.csv")



#### PLOTS ######

# Plot detrended residuals (aggregated, not individual)

# Choose one PANSS item to check
item <- "PANSSP2"   # e.g. "PANSSN5", "PANSSG8", etc.

# Build column names for 3 timepoints
item_cols <- c(
  item,
  paste0("T1", item),
  paste0("T2", item)
)

# Check that these columns exist
item_cols
colnames(net_impute)[colnames(net_impute) %in% item_cols]

# Plot violin plot for selected item
ggplot(
  net_impute %>%
    select(all_of(item_cols)) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = "Time",
      values_to = "Value"
    ) %>%
    mutate(
      Time = factor(
        Time,
        levels = item_cols,
        labels = c("T0", "T1", "T2")   # cleaner labels
      )
    ),
  aes(x = Time, y = Value)
) +
  geom_violin(trim = FALSE) +
  stat_summary(
    color    = "red",
    size     = 0.3,
    fun.data = mean_cl_normal
  ) +
  labs(
    title = item,
    x     = "Time point",
    y     = "Standardised residuals"
  ) +
  theme_bw() +
  theme(
    text       = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



## Loop over all items – full violin plot grid

## 1. List of the 30 PANSS items at baseline
net_impute_vars <- net_impute %>% 
  dplyr::select(contains("PANSS"))

all_names <- colnames(net_impute_vars)
symptoms  <- all_names[!grepl("^T[0-9]", all_names)]   # e.g. "PANSSP1", ..., "PANSSG16"

## 2. Convert to long format for all items × timepoints

panss_long <- net_impute %>%
  dplyr::select(
    dplyr::all_of(c(
      symptoms,
      paste0("T1", symptoms),
      paste0("T2", symptoms)
    ))
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Var",
    values_to = "Value"
  ) %>%
  mutate(
    # Extract timepoint from variable name
    Time = dplyr::case_when(
      grepl("^T1", Var) ~ "T1",
      grepl("^T2", Var) ~ "T2",
      TRUE              ~ "T0"
    ),
    # Extract item name by removing T1/T2 prefix
    Item = gsub("^T[0-9]", "", Var),
    Time = factor(Time, levels = c("T0", "T1", "T2"))
  )

## 3. Plot grid 10 × 3 using facet_wrap

ggplot(
  panss_long,
  aes(x = Time, y = Value)
) +
  geom_violin(trim = FALSE) +
  stat_summary(
    color    = "red",
    size     = 0.3,
    fun.data = mean_cl_normal
  ) +
  facet_wrap(
    ~ Item,
    ncol = 3   # 3 columns → 10 rows for 30 items
  ) +
  labs(
    title = "PANSS – standardised residuals by items and timepoints",
    x     = "Time point",
    y     = "Standardised residuals"
  ) +
  theme_bw() +
  theme(
    text             = element_text(size = 9),
    strip.text       = element_text(size = 7),
    axis.title       = element_text(size = 10),
    axis.text.x      = element_text(size = 7),
    axis.text.y      = element_text(size = 7),
    plot.title       = element_text(size = 12, face = "bold"),
    panel.spacing.x  = unit(0.15, "lines"),
    panel.spacing.y  = unit(0.15, "lines")
  )
