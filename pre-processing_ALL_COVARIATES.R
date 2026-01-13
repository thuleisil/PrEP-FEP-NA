
source("script/setup.R")

# Prendo il dataset droppato - con 368 elementi - cioè tutte le persone che hanno dati completi - possibile aumentare a 368????

net_impute_vars <- prep_df_drop %>% 
  dplyr::select(contains("PANSS"))


# nomi di tutte le colonne
all_names <- colnames(net_impute_vars)

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


# assegno alle righe il nome del sintomo (senza prefisso di tempo)
rownames(design_mat) <- symptoms


# Regress out age, gender, medication vars, total words and score (number of EHR entries in FUP) 

## 1. Dataset di lavoro: covariate + tutte le PANSS (90 variabili)

panss_vars <- as.vector(design_mat)      # tutte le PANSS: baseline, T1, T2
panss_vars <- unique(panss_vars)        # per sicurezza, rimuovo duplicati

covars <- c(
  "GENDER", "ETA", "ETNIA", "ANTIPSIC1", "ANTIDEP1", "STABILIZ1", "BENZO" 
)

net_impute <- prep_df_drop %>%
  dplyr::select(all_of(c(covars, panss_vars))) %>%
  as.data.frame()

## 2. Loop: per ogni sintomo × time point regredisco sui covariati e sostituisco con i residui

n_v <- nrow(design_mat)   # 30 sintomi
n_t <- ncol(design_mat)   # 3 time points

set.seed(1234)

for (k in 1:n_v) {
  for (i in 1:n_t) {
    
    x_var <- design_mat[k, i]   # es. "PANSSP1", "T1PANSSP1", "T2PANSSP1"
    
    # costruiamo la formula: es. "PANSSP1 ~ GENDER + ETA + ... + ANTIDEP1"
    f <- as.formula(
      paste(x_var, "~", paste(covars, collapse = " + "))
    )
    
    fit <- lm(f, data = net_impute)
    
    # sostituisco i valori originali con i residui
    net_impute[[x_var]] <- residuals(fit)
  }
}

## 3. Center, scale & detrend delle sole variabili PANSS

net_impute <- net_impute %>%
  mutate(across(all_of(panss_vars),
                ~ as.numeric(scale(.x, center = TRUE, scale = TRUE))))

dim(net_impute)


write.csv(net_impute, "data/residuals.csv")




#### PLOTS ######


#  plotting detrended residuals (as aggregate rather than individual data)

# scegli l’item PANSS da controllare
item <- "PANSSP2"   # cambia in "PANSSN5", "PANSSG8", ecc.

# costruisco i nomi delle colonne per i 3 timepoint
item_cols <- c(
  item,
  paste0("T1", item),
  paste0("T2", item)
)

# controllo che le colonne esistano davvero
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
        labels = c("T0", "T1", "T2")   # etichette più leggibili
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
    y     = "Residuali standardizzati"
  ) +
  theme_bw() +
  theme(
    text       = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



## loop

## 1. elenco dei 30 item PANSS al baseline
## (se li hai già in `symptoms` puoi saltare questo pezzo)
net_impute_vars <- net_impute %>% 
  dplyr::select(contains("PANSS"))

all_names <- colnames(net_impute_vars)
symptoms  <- all_names[!grepl("^T[0-9]", all_names)]   # es. "PANSSP1", ..., "PANSSG16"

## 2. porto in formato long TUTTI gli item × timepoint

panss_long <- net_impute %>%
  # prendo tutte le colonne PANSS: baseline, T1, T2
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
    # ricavo il timepoint dalla stringa della colonna
    Time = dplyr::case_when(
      grepl("^T1", Var) ~ "T1",
      grepl("^T2", Var) ~ "T2",
      TRUE              ~ "T0"
    ),
    # ricavo il nome dell’item togliendo l’eventuale prefisso T1/T2
    Item = gsub("^T[0-9]", "", Var),
    Time = factor(Time, levels = c("T0", "T1", "T2"))
  )

## 3. faccio il plot 10 × 3 con facet_wrap

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
    ncol = 3   # 3 colonne → 10 righe per 30 item
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

