library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# =========================
# 1) Load & clean data (once)
# =========================

# Make sure the RDS is in the same folder as app.R
df_raw <- readRDS("BASE_SCR_ENSAE_ANONYME.rds")

# Divide numeric variables by 1e8 (except these)
divide_1e8 <- function(df, exclude = c("SOCIETE", "Annee", "RATIO")) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  num_vars <- setdiff(num_vars, exclude)
  
  df %>%
    mutate(across(all_of(num_vars), ~ . / 1e8))
}

df_raw <- divide_1e8(df_raw)

dff_clean <- df_raw %>%
  mutate(
    Nature = case_when(
      Nature %in% c("Mixte", "Vie/Capitalisation") ~ "Vie/Mixte",
      TRUE ~ Nature
    )
  ) %>%
  filter(
    Nature %in% c("Non Vie", "Vie/Mixte"),
    Remise == "A"
  ) %>%
  mutate(
    Annee = as.integer(Annee),
    SCR = as.numeric(SCR),
    `FONDS PROPRES` = as.numeric(`FONDS PROPRES`),
    RATIO = as.numeric(RATIO)
  ) %>%
  distinct(SOCIETE, Annee, .keep_all = TRUE) %>%
  arrange(SOCIETE, Annee)

soc_list <- sort(unique(dff_clean$SOCIETE))

# =========================
# 2) Plot function (annual values, dual axis)
# =========================
plot_society_dual <- function(df, societe_name) {
  df_s <- df %>% filter(SOCIETE == societe_name)
  
  validate(
    need(nrow(df_s) > 0, "Aucune donnée pour cette société.")
  )
  
  # Use RATIO if finite; otherwise compute FP/SCR
  df_s <- df_s %>%
    mutate(
      RATIO_calc = ifelse(is.finite(RATIO), RATIO, `FONDS PROPRES` / SCR)
    )
  
  left_max  <- suppressWarnings(max(c(df_s$SCR, df_s$`FONDS PROPRES`), na.rm = TRUE))
  right_max <- suppressWarnings(max(df_s$RATIO_calc, na.rm = TRUE))
  
  validate(
    need(is.finite(left_max) && left_max > 0, "SCR / Fonds propres indisponibles."),
    need(is.finite(right_max) && right_max > 0, "Ratio indisponible ou nul.")
  )
  
  # Required for ggplot secondary axis
  k <- left_max / right_max
  
  df_left <- df_s %>%
    select(Annee, SCR, `FONDS PROPRES`) %>%
    pivot_longer(
      cols = c(SCR, `FONDS PROPRES`),
      names_to = "Variable",
      values_to = "Valeur"
    ) %>%
    mutate(
      Variable = recode(Variable,
                        "SCR" = "SCR",
                        "FONDS PROPRES" = "Fonds propres")
    )
  
  cols_levels <- c("Fonds propres" = "#009E73", "SCR" = "#D55E00")
  
  ggplot() +
    # Left axis: annual levels (natural scale after /1e8)
    geom_line(
      data = df_left,
      aes(x = Annee, y = Valeur, color = Variable, group = Variable),
      linewidth = 1.2
    ) +
    geom_point(
      data = df_left,
      aes(x = Annee, y = Valeur, color = Variable),
      size = 2.2
    ) +
    # Right axis: ratio (displayed via *k, read via /k)
    geom_line(
      data = df_s,
      aes(x = Annee, y = RATIO_calc * k),
      color = "#0072B2", linetype = "dashed", linewidth = 1.2
    ) +
    geom_point(
      data = df_s,
      aes(x = Annee, y = RATIO_calc * k),
      color = "#0072B2", size = 2.0
    ) +
    scale_color_manual(values = cols_levels, name = "") +
    scale_y_continuous(
      name = "SCR et Fonds propres (en centaines de millions d'euros)",
      sec.axis = sec_axis(~ . / k, name = "Ratio de solvabilité")
    ) +
    scale_x_continuous(breaks = sort(unique(df_s$Annee))) +
    labs(
      title = paste("Évolution des indicateurs —", societe_name),
      x = "Année"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# =========================
# 3) UI
# =========================
ui <- fluidPage(
  titlePanel("Exploration des indicateurs par société"),
  sidebarLayout(
    sidebarPanel(
      selectInput("societe", "Sélectionner une société :", choices = soc_list)
    ),
    mainPanel(
      plotOutput("soc_plot", height = "650px")
    )
  )
)

# =========================
# 4) Server
# =========================
server <- function(input, output, session) {
  output$soc_plot <- renderPlot({
    req(input$societe)
    plot_society_dual(dff_clean, input$societe)
  }, res = 96)
}

shinyApp(ui, server)
