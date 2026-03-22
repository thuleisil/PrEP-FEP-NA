source("script/setup.R")

# Use the dropped dataset - with 368 individuals 


net_impute_vars <- prep_df_drop %>% 
  dplyr::select(contains("PANSS"))


# names of all columns
all_names <- colnames(net_impute_vars)

# baseline symptoms: those that do NOT start with "T1" or "T2"
symptoms <- all_names[!grepl("^T[0-9]", all_names)]
length(symptoms)          # should be 30
symptoms                  # e.g., "PANSSP1", "PANSSP2", ...

# time-points (prefixes)
waves <- c("", "T1", "T2")   # "" = baseline, then T1, T2

# build the design matrix: rows = symptoms, columns = time-points
design_mat <- sapply(
  waves,
  function(w) if (w == "") symptoms else paste0(w, symptoms)
)


# assign row names using the symptom name (without time prefix)
rownames(design_mat) <- symptoms


# Regress out age, gender, medication variables, total words and score (number of EHR entries in follow-up)

## 1. Working dataset: covariates + all PANSS variables (90 variables)

panss_vars <- as.vector(design_mat)      # all PANSS variables: baseline, T1, T2
panss_vars <- unique(panss_vars)        # remove duplicates just in case

covars <- c(
  "GENDER", "ETA", "ETNIA", "ANTIPSIC1", "ANTIDEP1", "STABILIZ1", "BENZO", "T0_DIAGNOSI", "T1_CBT", "T1_PSICED", "T1_CMREC"
)

net_impute <- prep_df_drop %>%
  dplyr::select(all_of(c(covars, panss_vars))) %>%
  as.data.frame()

## 2. Loop: for each symptom × time point, regress on covariates and replace with residuals

n_v <- nrow(design_mat)   # 30 symptoms
n_t <- ncol(design_mat)   # 3 time points

set.seed(1234)

for (k in 1:n_v) {
  for (i in 1:n_t) {
    
    x_var <- design_mat[k, i]   # e.g., "PANSSP1", "T1PANSSP1", "T2PANSSP1"
    
    # build the formula: e.g., "PANSSP1 ~ GENDER + ETA + ... + ANTIDEP1"
    f <- as.formula(
      paste(x_var, "~", paste(covars, collapse = " + "))
    )
    
    fit <- lm(f, data = net_impute)
    
    # replace original values with residuals
    net_impute[[x_var]] <- residuals(fit)
  }
}

## 3. Center, scale, and detrend PANSS variables only

net_impute <- net_impute %>%
  mutate(across(all_of(panss_vars),
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

dim(net_impute)


write.csv(net_impute, "data/residuals.csv")




#### PLOTS ######


# plotting detrended residuals (aggregate rather than individual data)

# choose the PANSS item to inspect
item <- "PANSSP2"   # change to "PANSSN5", "PANSSG8", etc.

# build the column names for the 3 time-points
item_cols <- c(
  item,
  paste0("T1", item),
  paste0("T2", item)
)

# check that the columns actually exist
item_cols
colnames(net_impute)[colnames(net_impute) %in% item_cols]

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
        labels = c("T0", "T1", "T2")   # more readable labels
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
    y     = "Standardized residuals"
  ) +
  theme_bw() +
  theme(
    text       = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



## loop

## 1. list of the 30 baseline PANSS items
## (if you already have them in `symptoms`, you can skip this part)
net_impute_vars <- net_impute %>% 
  dplyr::select(contains("PANSS"))

all_names <- colnames(net_impute_vars)
symptoms  <- all_names[!grepl("^T[0-9]", all_names)]   # e.g., "PANSSP1", ..., "PANSSG16"

## 2. reshape all item × time-point data into long format

panss_long <- net_impute %>%
  # select all PANSS columns: baseline, T1, T2
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
    # derive the time-point from the column name
    Time = dplyr::case_when(
      grepl("^T1", Var) ~ "T1",
      grepl("^T2", Var) ~ "T2",
      TRUE              ~ "T0"
    ),
    # derive the item name by removing the T1/T2 prefix if present
    Item = gsub("^T[0-9]", "", Var),
    Time = factor(Time, levels = c("T0", "T1", "T2"))
  )

## 3. create the 10 × 3 plot with facet_wrap

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
