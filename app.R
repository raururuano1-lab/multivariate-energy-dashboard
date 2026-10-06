# ============================================================
# Universidad de San Carlos de Guatemala\
# Facultad de Ingeniería\
# Escuela de Estudios de Postgrado\
# MAESTRÍA EN ESTADÍSTICA APLICADA\
# Métodos Multivariados aplicados al Análisis de Datos\
# 3er. Trimestre 2026
# ============================================================
# Proyecto Final
# DATASET 2: HOUSEHOLD POWER CONSUMPTION
# Descripción general: Registros minuto a minuto del consumo eléctrico de un hogar durante varios años. Dataset temporal de gran tamaño.
# Objetivo del análisis: Identificar patrones multivariados de consumo eléctrico y perfiles de uso energético.
# ============================================================
# Proyecto académico desarrollado originalmente en equipo.
# Versión de portafolio preparada por Raúl Antonio Ruano Katz.

# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "DT",
  "scales",
  "psych",
  "cluster"
)

cargar_paquete <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Falta el paquete '", pkg, "'. Ejecute requirements.R antes de iniciar la app."))
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

invisible(lapply(paquetes, cargar_paquete))

options(scipen = 999)
set.seed(2026)

# ============================================================
# 2. ARCHIVOS DE DATOS
# ============================================================

archivo_diario <- file.path("data", "daily_energy_metrics.csv")
archivo_horario <- file.path("data", "hourly_energy_metrics.csv")

archivo_diario_existe <- file.exists(archivo_diario)
archivo_horario_existe <- file.exists(archivo_horario)

if (archivo_diario_existe) {
  datos_dia <- read.csv(
    archivo_diario,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  datos_dia$fecha <- as.Date(datos_dia$fecha)
  datos_dia$tipo_dia <- factor(
    datos_dia$tipo_dia,
    levels = c("Laboral", "Fin de semana")
  )
  datos_dia$cluster_kmeans <- factor(datos_dia$cluster_kmeans)
  datos_dia$cluster_hc <- factor(datos_dia$cluster_hc)
  datos_dia$outlier_multivariado <- as.logical(datos_dia$outlier_multivariado)
} else {
  datos_dia <- data.frame()
}

if (archivo_horario_existe) {
  datos_hora <- read.csv(
    archivo_horario,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  datos_hora$fecha <- as.Date(datos_hora$fecha)
  datos_hora$tipo_dia <- factor(
    datos_hora$tipo_dia,
    levels = c("Laboral", "Fin de semana")
  )
  datos_hora$cluster_kmeans <- factor(datos_hora$cluster_kmeans)

  if (archivo_diario_existe) {
    datos_hora$dia_semana <- datos_dia$dia_semana[
      match(datos_hora$fecha, datos_dia$fecha)
    ]

    datos_hora$dia_semana <- factor(
      datos_hora$dia_semana,
      levels = c(
        "Lunes",
        "Martes",
        "Miercoles",
        "Jueves",
        "Viernes",
        "Sabado",
        "Domingo"
      ),
      ordered = TRUE
    )
  }
} else {
  datos_hora <- data.frame()
}

# ============================================================
# 3. DEFINICIONES Y FUNCIONES AUXILIARES
# ============================================================

vars_multivariadas <- c(
  "energia_global_kwh",
  "reactiva_media",
  "voltaje_medio",
  "intensidad_media",
  "sub1_kwh",
  "sub2_kwh",
  "sub3_kwh"
)

etiquetas_vars <- c(
  energia_global_kwh = "Energía global",
  reactiva_media = "Potencia reactiva",
  voltaje_medio = "Voltaje",
  intensidad_media = "Intensidad",
  sub1_kwh = "Submedidor 1",
  sub2_kwh = "Submedidor 2",
  sub3_kwh = "Submedidor 3"
)

unidades_vars <- c(
  energia_global_kwh = "kWh/día",
  reactiva_media = "kVAR",
  voltaje_medio = "V",
  intensidad_media = "A",
  sub1_kwh = "kWh/día",
  sub2_kwh = "kWh/día",
  sub3_kwh = "kWh/día"
)

formato_num <- function(x, digitos = 2) {
  if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
    return("No disponible")
  }
  format(round(x, digitos), big.mark = ",", scientific = FALSE)
}

formato_pct <- function(x, digitos = 1) {
  if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
    return("No disponible")
  }
  paste0(round(x, digitos), "%")
}

formato_pvalue <- function(p) {
  if (length(p) == 0 || is.na(p) || is.nan(p)) {
    return("No disponible")
  }
  if (p < 0.001) {
    return("< 0.001")
  }
  format(round(p, 4), scientific = FALSE)
}

texto_lista <- function(x, todos) {
  if (is.null(x) || length(x) == 0) {
    return("Ninguno")
  }
  if (length(x) == length(todos)) {
    return("Todos")
  }
  paste(x, collapse = ", ")
}

calcular_auc <- function(real, score) {
  real <- as.numeric(real)
  n_pos <- sum(real == 1)
  n_neg <- sum(real == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(NA)
  }

  rangos <- rank(score, ties.method = "average")

  (
    sum(rangos[real == 1]) -
      n_pos * (n_pos + 1) / 2
  ) / (n_pos * n_neg)
}

calcular_metricas_clasificacion <- function(real, prob, corte = 0.50) {
  pred <- ifelse(prob >= corte, 1, 0)

  TP <- sum(pred == 1 & real == 1)
  TN <- sum(pred == 0 & real == 0)
  FP <- sum(pred == 1 & real == 0)
  FN <- sum(pred == 0 & real == 1)

  accuracy <- (TP + TN) / length(real)
  precision <- ifelse(TP + FP == 0, NA, TP / (TP + FP))
  recall <- ifelse(TP + FN == 0, NA, TP / (TP + FN))
  specificity <- ifelse(TN + FP == 0, NA, TN / (TN + FP))
  f1 <- ifelse(
    is.na(precision) || is.na(recall) || precision + recall == 0,
    NA,
    2 * precision * recall / (precision + recall)
  )

  data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    F1 = f1,
    Balanced_Accuracy = mean(c(recall, specificity), na.rm = TRUE),
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN
  )
}

calcular_vif <- function(modelo) {
  X <- model.matrix(modelo)

  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }

  resultado <- numeric(ncol(X))
  names(resultado) <- colnames(X)

  for (j in seq_len(ncol(X))) {
    y_vif <- X[, j]
    otros <- as.data.frame(X[, -j, drop = FALSE])
    ajuste_vif <- lm(y_vif ~ ., data = otros)
    r2 <- summary(ajuste_vif)$r.squared
    resultado[j] <- 1 / (1 - r2)
  }

  resultado
}

box_m_manual <- function(X, grupo) {
  X <- as.matrix(X)
  grupo <- factor(grupo)

  casos_completos <- complete.cases(X) & !is.na(grupo)
  X <- X[casos_completos, , drop = FALSE]
  grupo <- droplevels(grupo[casos_completos])

  niveles <- levels(grupo)
  g <- length(niveles)
  p <- ncol(X)
  N <- nrow(X)

  cov_grupos <- vector("list", g)
  n_grupos <- numeric(g)

  for (i in seq_along(niveles)) {
    Xi <- X[grupo == niveles[i], , drop = FALSE]
    n_grupos[i] <- nrow(Xi)
    cov_grupos[[i]] <- cov(Xi)
  }

  S_pooled <- matrix(0, nrow = p, ncol = p)

  for (i in seq_along(niveles)) {
    S_pooled <- S_pooled + (n_grupos[i] - 1) * cov_grupos[[i]]
  }

  S_pooled <- S_pooled / (N - g)

  logdet_pooled <- as.numeric(
    determinant(S_pooled, logarithm = TRUE)$modulus
  )

  logdet_grupos <- sapply(
    cov_grupos,
    function(Si) {
      as.numeric(determinant(Si, logarithm = TRUE)$modulus)
    }
  )

  M <- (N - g) * logdet_pooled -
    sum((n_grupos - 1) * logdet_grupos)

  C <- (
    (2 * p^2 + 3 * p - 1) /
      (6 * (p + 1) * (g - 1))
  ) * (
    sum(1 / (n_grupos - 1)) -
      1 / (N - g)
  )

  chi2 <- (1 - C) * M
  gl <- (g - 1) * p * (p + 1) / 2
  p_value <- pchisq(chi2, df = gl, lower.tail = FALSE)

  data.frame(
    Box_M = M,
    Chi_cuadrado = chi2,
    gl = gl,
    p_value = p_value
  )
}

prueba_mardia <- function(X) {
  X <- as.matrix(X)
  X <- X[complete.cases(X), , drop = FALSE]

  n <- nrow(X)
  p <- ncol(X)
  centro <- colMeans(X)
  S <- cov(X)
  Z <- sweep(X, 2, centro, FUN = "-")
  inv_S <- solve(S)
  G <- Z %*% inv_S %*% t(Z)

  b1p <- mean(G^3)
  dist2 <- diag(G)
  b2p <- mean(dist2^2)

  chi_asimetria <- n * b1p / 6
  gl_asimetria <- p * (p + 1) * (p + 2) / 6
  p_asimetria <- pchisq(
    chi_asimetria,
    df = gl_asimetria,
    lower.tail = FALSE
  )

  curtosis_esperada <- p * (p + 2)
  z_curtosis <- (b2p - curtosis_esperada) /
    sqrt(8 * p * (p + 2) / n)
  p_curtosis <- 2 * pnorm(abs(z_curtosis), lower.tail = FALSE)

  data.frame(
    Prueba = c("Asimetría de Mardia", "Curtosis de Mardia"),
    Coeficiente = c(b1p, b2p),
    Estadistico = c(chi_asimetria, z_curtosis),
    gl = c(gl_asimetria, NA),
    p_value = c(p_asimetria, p_curtosis)
  )
}

# ============================================================
# 4. ANALISIS OFICIAL DEL PROYECTO
#    Se calcula una vez sobre los 723 días del análisis final.
# ============================================================

analisis_disponible <- archivo_diario_existe && nrow(datos_dia) > 0

if (analisis_disponible) {

  X_dia <- datos_dia[, vars_multivariadas, drop = FALSE]
  X_scaled <- scale(X_dia)
  matriz_cor <- cor(X_dia, use = "complete.obs")

  # PCA
  pca <- prcomp(
    X_scaled,
    center = FALSE,
    scale. = FALSE
  )

  autovalores_pca <- pca$sdev^2
  prop_var_pca <- autovalores_pca / sum(autovalores_pca)
  acum_var_pca <- cumsum(prop_var_pca)

  tabla_pca <- data.frame(
    Componente = paste0("PC", seq_along(autovalores_pca)),
    Autovalor = autovalores_pca,
    Varianza_Pct = prop_var_pca * 100,
    Varianza_Acumulada_Pct = acum_var_pca * 100
  )

  scores_pca <- data.frame(
    fecha = datos_dia$fecha,
    tipo_dia = datos_dia$tipo_dia,
    cluster = datos_dia$cluster_kmeans,
    pca$x,
    check.names = FALSE
  )

  cargas_pca <- as.data.frame(pca$rotation)
  cargas_pca$Variable <- rownames(cargas_pca)
  rownames(cargas_pca) <- NULL

  # FA
  kmo_result <- KMO(matriz_cor)
  bartlett_result <- cortest.bartlett(
    matriz_cor,
    n = nrow(X_dia)
  )

  fa_result <- suppressWarnings(
    fa(
      X_scaled,
      nfactors = 2,
      rotate = "varimax",
      fm = "minres",
      scores = "regression"
    )
  )

  cargas_fa <- as.data.frame(unclass(fa_result$loadings))
  cargas_fa$Variable <- rownames(cargas_fa)
  rownames(cargas_fa) <- NULL

  msa_tabla <- data.frame(
    Variable = names(kmo_result$MSAi),
    MSA = as.numeric(kmo_result$MSAi)
  )

  # MDS
  dist_dia <- dist(X_scaled, method = "euclidean")
  mds <- cmdscale(dist_dia, k = 2, eig = TRUE)

  coord_mds <- data.frame(
    fecha = datos_dia$fecha,
    tipo_dia = datos_dia$tipo_dia,
    cluster = datos_dia$cluster_kmeans,
    MDS1 = mds$points[, 1],
    MDS2 = mds$points[, 2]
  )

  # Clustering: evaluación k
  rango_k <- 2:8

  hc <- hclust(dist_dia, method = "ward.D2")

  sil_hc <- sapply(
    rango_k,
    function(k) {
      grupos <- cutree(hc, k = k)
      sil <- silhouette(grupos, dist_dia)
      mean(sil[, 3])
    }
  )

  set.seed(2026)
  sil_kmeans <- numeric(length(rango_k))
  wss_kmeans <- numeric(length(rango_k))

  for (i in seq_along(rango_k)) {
    modelo_k <- kmeans(
      X_scaled,
      centers = rango_k[i],
      nstart = 50,
      iter.max = 100
    )

    sil_k <- silhouette(modelo_k$cluster, dist_dia)
    sil_kmeans[i] <- mean(sil_k[, 3])
    wss_kmeans[i] <- modelo_k$tot.withinss
  }

  tabla_cluster_eval <- data.frame(
    k = rango_k,
    Silhouette_Jerarquico = sil_hc,
    Silhouette_KMeans = sil_kmeans,
    WSS_KMeans = wss_kmeans
  )

  # Logística oficial
  datos_logit <- datos_dia[order(datos_dia$fecha), , drop = FALSE]
  n_total <- nrow(datos_logit)
  n_train <- floor(0.80 * n_total)

  train_logit <- datos_logit[1:n_train, , drop = FALSE]
  test_logit <- datos_logit[(n_train + 1):n_total, , drop = FALSE]

  umbral_consumo <- median(train_logit$energia_global_kwh)

  train_logit$consumo_alto <- ifelse(
    train_logit$energia_global_kwh > umbral_consumo,
    1,
    0
  )

  test_logit$consumo_alto <- ifelse(
    test_logit$energia_global_kwh > umbral_consumo,
    1,
    0
  )

  predictores_logit <- c(
    "reactiva_media",
    "voltaje_medio",
    "sub1_kwh",
    "sub2_kwh",
    "sub3_kwh"
  )

  medias_train <- sapply(
    train_logit[, predictores_logit, drop = FALSE],
    mean
  )

  sd_train <- sapply(
    train_logit[, predictores_logit, drop = FALSE],
    sd
  )

  estandarizar_con_train <- function(
      datos_nuevos,
      variables,
      medias,
      desviaciones
  ) {
    X <- as.matrix(datos_nuevos[, variables, drop = FALSE])
    X <- sweep(X, 2, medias, FUN = "-")
    X <- sweep(X, 2, desviaciones, FUN = "/")
    X <- as.data.frame(X)
    names(X) <- paste0(variables, "_z")
    X
  }

  X_train_z <- estandarizar_con_train(
    train_logit,
    predictores_logit,
    medias_train,
    sd_train
  )

  X_test_z <- estandarizar_con_train(
    test_logit,
    predictores_logit,
    medias_train,
    sd_train
  )

  train_modelo <- cbind(
    consumo_alto = train_logit$consumo_alto,
    X_train_z
  )

  test_modelo <- cbind(
    consumo_alto = test_logit$consumo_alto,
    X_test_z
  )

  formula_logit <- as.formula(
    paste(
      "consumo_alto ~",
      paste(names(X_train_z), collapse = " + ")
    )
  )

  modelo_logit <- glm(
    formula_logit,
    data = train_modelo,
    family = binomial(link = "logit")
  )

  prob_test <- predict(
    modelo_logit,
    newdata = test_modelo,
    type = "response"
  )

  real_test <- test_modelo$consumo_alto
  auc_oficial <- calcular_auc(real_test, prob_test)
  metricas_oficiales <- calcular_metricas_clasificacion(
    real_test,
    prob_test,
    corte = 0.50
  )

  # Sensibilidad del punto de corte de clasificación.
  # 0.50 se mantiene como referencia oficial.
  cortes_sensibilidad <- seq(
    0.10,
    0.90,
    by = 0.01
  )

  tabla_sensibilidad_cortes <- do.call(
    rbind,
    lapply(
      cortes_sensibilidad,
      function(corte) {
        resultado <- calcular_metricas_clasificacion(
          real_test,
          prob_test,
          corte = corte
        )
        cbind(
          Corte = corte,
          resultado
        )
      }
    )
  )

  indice_mejor_balance <- which.max(
    tabla_sensibilidad_cortes$Balanced_Accuracy
  )

  corte_balanceado_exploratorio <-
    tabla_sensibilidad_cortes$Corte[
      indice_mejor_balance
    ]

  comparacion_cortes <- do.call(
    rbind,
    lapply(
      c(0.50, 0.61, 0.74),
      function(corte) {
        resultado <- calcular_metricas_clasificacion(
          real_test,
          prob_test,
          corte = corte
        )
        cbind(
          Corte = corte,
          resultado
        )
      }
    )
  )

  comparacion_cortes$Escenario <- c(
    "Referencia del modelo",
    "Mayor Balanced Accuracy observada",
    "Escenario más restrictivo"
  )

  coef_logit <- coef(modelo_logit)
  se_logit <- summary(modelo_logit)$coefficients[, "Std. Error"]

  tabla_or <- data.frame(
    Variable = names(coef_logit),
    Beta = coef_logit,
    OR = exp(coef_logit),
    IC95_Inferior = exp(coef_logit - 1.96 * se_logit),
    IC95_Superior = exp(coef_logit + 1.96 * se_logit),
    row.names = NULL
  )

  tabla_or <- tabla_or[tabla_or$Variable != "(Intercept)", , drop = FALSE]

  # MANOVA y supuestos
  resultado_box_m <- box_m_manual(
    X_dia,
    datos_dia$tipo_dia
  )

  resultado_mardia <- prueba_mardia(X_dia)

  modelo_manova <- manova(
    cbind(
      energia_global_kwh,
      reactiva_media,
      voltaje_medio,
      intensidad_media,
      sub1_kwh,
      sub2_kwh,
      sub3_kwh
    ) ~ tipo_dia,
    data = datos_dia
  )

  pillai_full <- summary(
    modelo_manova,
    test = "Pillai"
  )$stats

  datos_sin_outliers <- datos_dia[
    !datos_dia$outlier_multivariado,
    ,
    drop = FALSE
  ]

  modelo_manova_sin_out <- manova(
    cbind(
      energia_global_kwh,
      reactiva_media,
      voltaje_medio,
      intensidad_media,
      sub1_kwh,
      sub2_kwh,
      sub3_kwh
    ) ~ tipo_dia,
    data = datos_sin_outliers
  )

  pillai_sin_out <- summary(
    modelo_manova_sin_out,
    test = "Pillai"
  )$stats

  # Regresión complementaria
  modelo_consumo <- lm(
    energia_global_kwh ~
      reactiva_media +
      voltaje_medio +
      intensidad_media +
      sub1_kwh +
      sub2_kwh +
      sub3_kwh,
    data = datos_dia
  )

  datos_reg_z <- as.data.frame(
    scale(
      datos_dia[, vars_multivariadas, drop = FALSE]
    )
  )

  modelo_consumo_z <- lm(
    energia_global_kwh ~
      reactiva_media +
      voltaje_medio +
      intensidad_media +
      sub1_kwh +
      sub2_kwh +
      sub3_kwh,
    data = datos_reg_z
  )

  beta_estandarizados <- coef(modelo_consumo_z)
}

# ============================================================
# 5. INTERFAZ
# ============================================================

ui <- dashboardPage(

  skin = "blue",

  dashboardHeader(
    title = "BI Consumo Eléctrico"
  ),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Resumen ejecutivo",
        tabName = "inicio",
        icon = icon("dashboard")
      ),
      menuItem(
        "Explorador de consumo",
        tabName = "explorador",
        icon = icon("line-chart")
      ),
      menuItem(
        "Estructura multivariada",
        tabName = "multivariado",
        icon = icon("sitemap")
      ),
      menuItem(
        "Perfiles de consumo",
        tabName = "perfiles",
        icon = icon("users")
      ),
      menuItem(
        "Clasificación",
        tabName = "clasificacion",
        icon = icon("bullseye")
      ),
      menuItem(
        "Laboral vs. fin de semana",
        tabName = "manova",
        icon = icon("calendar")
      ),
      menuItem(
        "Conclusiones y decisiones",
        tabName = "conclusiones",
        icon = icon("check-circle")
      ),
      menuItem(
        "Metodología",
        tabName = "metodologia",
        icon = icon("flask")
      )
    ),

    hr(),

    div(
      class = "sidebar-note",
      strong("Proyecto Final"),
      br(),
      "Métodos Multivariados",
      br(),
      "Grupo 4 | USAC 2026"
    )
  ),

  dashboardBody(

    tags$head(
      tags$style(
        HTML("
          body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
          .content-wrapper, .right-side { background-color: #f4f7fb; }
          .main-header .logo { font-weight: 700; }
          .main-sidebar { background-color: #152536 !important; }
          .sidebar-menu > li.active > a { border-left-color: #35a7ff !important; }
          .sidebar-note { color: #cbd6e2; padding: 0 16px 16px 16px; line-height: 1.55; }
          .box { border-radius: 12px; box-shadow: 0 2px 9px rgba(23, 43, 77, 0.07); border-top-width: 3px; }
          .small-box { border-radius: 12px; box-shadow: 0 2px 9px rgba(23, 43, 77, 0.08); }
          .hero-panel {
            background: linear-gradient(120deg, #12324a 0%, #17669a 60%, #2389c9 100%);
            color: white;
            padding: 28px 30px;
            border-radius: 15px;
            margin-bottom: 20px;
            box-shadow: 0 8px 24px rgba(23, 50, 74, 0.16);
          }
          .hero-panel h2 { margin-top: 0; font-weight: 750; }
          .hero-panel p { font-size: 16px; line-height: 1.55; max-width: 1000px; }
          .eyebrow { text-transform: uppercase; letter-spacing: 1.3px; font-size: 12px; opacity: 0.85; font-weight: 700; }
          .question-card {
            background: white;
            border-radius: 12px;
            padding: 18px;
            min-height: 155px;
            border-left: 5px solid #2389c9;
            box-shadow: 0 2px 9px rgba(23, 43, 77, 0.07);
            margin-bottom: 15px;
          }
          .question-card h4 { margin-top: 0; color: #16354d; font-weight: 700; }
          .question-card p { color: #4b5d6b; line-height: 1.48; }
          .insight-box {
            background: white;
            border-left: 6px solid #2389c9;
            padding: 17px 20px;
            border-radius: 10px;
            line-height: 1.55;
            font-size: 15px;
          }
          .success-box {
            background: #f0fbf5;
            border-left: 6px solid #28a745;
            padding: 17px 20px;
            border-radius: 10px;
            line-height: 1.55;
          }
          .warning-box {
            background: #fff8e6;
            border-left: 6px solid #f0ad4e;
            padding: 17px 20px;
            border-radius: 10px;
            line-height: 1.55;
          }
          .danger-box {
            background: #fff3f3;
            border-left: 6px solid #d9534f;
            padding: 17px 20px;
            border-radius: 10px;
            line-height: 1.55;
          }
          .method-pill {
            display: inline-block;
            background: #eaf4fb;
            color: #17669a;
            padding: 6px 10px;
            border-radius: 20px;
            margin: 3px 4px 3px 0;
            font-size: 12px;
            font-weight: 650;
          }
          .metric-inline { font-size: 24px; font-weight: 750; color: #12324a; }
          .metric-caption { color: #6c7b88; font-size: 12px; }
          .result-tag {
            display: inline-block;
            padding: 5px 9px;
            border-radius: 8px;
            background: #eef6fb;
            color: #17669a;
            font-weight: 700;
            font-size: 12px;
          }
          .nav-footer { margin: 14px 0 5px 0; text-align: right; }
          .control-panel { background: #f8fbfd; padding: 14px; border-radius: 10px; }
          .data-note { color: #6c7b88; font-size: 12px; margin-top: 7px; }
          .decision-table td { vertical-align: top !important; }
        ")
      )
    ),

    if (!archivo_diario_existe) {
      fluidRow(
        box(
          width = 12,
          status = "danger",
          solidHeader = TRUE,
          title = "Archivo de datos no encontrado",
          div(
            class = "danger-box",
            paste0(
              "No se encontró '", archivo_diario, "'. ",
              "Coloque data/daily_energy_metrics.csv dentro del proyecto."
            )
          )
        )
      )
    },

    tabItems(

      # --------------------------------------------------------
      # RESUMEN EJECUTIVO
      # --------------------------------------------------------
      tabItem(
        tabName = "inicio",

        div(
          class = "hero-panel",
          div(class = "eyebrow", "Proyecto Final | Métodos Multivariados Aplicados al Análisis de Datos"),
          h2("Household Power Consumption"),
          p(
            "El dashboard convierte el análisis estadístico del proyecto en una historia interactiva para presentación y toma de decisiones. Permite entender cuándo cambia el consumo, cómo se relacionan las variables, qué perfiles existen y qué factores ayudan a identificar días de consumo alto."
          ),
          actionButton(
            "ir_explorador",
            "Iniciar recorrido interactivo",
            icon = icon("arrow-right"),
            class = "btn-success"
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_dias", width = 2),
          valueBoxOutput("kpi_consumo", width = 2),
          valueBoxOutput("kpi_pca", width = 2),
          valueBoxOutput("kpi_clusters", width = 2),
          valueBoxOutput("kpi_auc", width = 2),
          valueBoxOutput("kpi_manova", width = 2)
        ),

        fluidRow(
          valueBoxOutput("kpi_fin_premium", width = 3),
          valueBoxOutput("kpi_hora_pico", width = 3),
          valueBoxOutput("kpi_sub3_share", width = 3),
          valueBoxOutput("kpi_cluster_ratio", width = 3)
        ),

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Las cinco preguntas del proyecto",
            fluidRow(
              column(
                width = 4,
                div(
                  class = "question-card",
                  h4("1. ¿Qué patrones existen?"),
                  p("El consumo cambia por hora, período y tipo de día. Existe autocorrelación diaria y los fines de semana mantienen niveles superiores durante varias horas."),
                  span(class = "result-tag", "Patrón temporal confirmado")
                )
              ),
              column(
                width = 4,
                div(
                  class = "question-card",
                  h4("2. ¿Cuántas dimensiones?"),
                  p("Cuatro componentes principales conservan aproximadamente 89.8% de la variabilidad de las siete variables originales."),
                  span(class = "result-tag", "4 componentes")
                )
              ),
              column(
                width = 4,
                div(
                  class = "question-card",
                  h4("3. ¿Existen perfiles?"),
                  p("K-Means resume los días en dos perfiles generales de mayor y menor consumo, aunque la separación entre ellos es moderada-baja."),
                  span(class = "result-tag", "2 perfiles generales")
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                div(
                  class = "question-card",
                  h4("4. ¿Laboral y fin de semana difieren?"),
                  p("Sí. MANOVA identifica diferencias multivariadas y los fines de semana muestran mayor energía global, intensidad y consumo en los tres submedidores."),
                  span(class = "result-tag", "Pillai significativo")
                )
              ),
              column(
                width = 6,
                div(
                  class = "question-card",
                  h4("5. ¿Qué explica el consumo global?"),
                  p("La intensidad está casi perfectamente asociada con la energía global. Entre los consumos específicos, el submedidor 3 presenta la relación más fuerte."),
                  span(class = "result-tag", "Intensidad + Submedidor 3")
                )
              )
            )
          )
        ),

        fluidRow(
          box(
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            title = "Evolución del consumo diario",
            plotlyOutput("graf_inicio_serie", height = "350px")
          ),
          box(
            width = 5,
            status = "success",
            solidHeader = TRUE,
            title = "Hallazgos clave",
            htmlOutput("texto_inicio_gerencial")
          )
        )
      ),

      # --------------------------------------------------------
      # EXPLORADOR
      # --------------------------------------------------------
      tabItem(
        tabName = "explorador",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Explorador interactivo del consumo",
            div(
              class = "control-panel",
              fluidRow(
                column(
                  width = 3,
                  dateRangeInput(
                    "filtro_fechas",
                    "Período",
                    start = if (analisis_disponible) min(datos_dia$fecha) else Sys.Date(),
                    end = if (analisis_disponible) max(datos_dia$fecha) else Sys.Date(),
                    min = if (analisis_disponible) min(datos_dia$fecha) else Sys.Date(),
                    max = if (analisis_disponible) max(datos_dia$fecha) else Sys.Date(),
                    format = "dd/mm/yyyy"
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    "filtro_tipo_dia",
                    "Tipo de día",
                    choices = c("Laboral", "Fin de semana"),
                    selected = c("Laboral", "Fin de semana")
                  )
                ),
                column(
                  width = 2,
                  checkboxGroupInput(
                    "filtro_cluster",
                    "Perfil K-Means",
                    choices = c("1", "2"),
                    selected = c("1", "2")
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    "variable_explorador",
                    "Variable a analizar",
                    choices = setNames(names(etiquetas_vars), etiquetas_vars),
                    selected = "energia_global_kwh"
                  )
                ),
                column(
                  width = 1,
                  br(),
                  actionButton(
                    "reiniciar_filtros",
                    "Reset",
                    icon = icon("refresh")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  sliderInput(
                    "filtro_energia",
                    "Rango de energía global (kWh/día)",
                    min = if (analisis_disponible) floor(min(datos_dia$energia_global_kwh)) else 0,
                    max = if (analisis_disponible) ceiling(max(datos_dia$energia_global_kwh)) else 100,
                    value = if (analisis_disponible) c(
                      floor(min(datos_dia$energia_global_kwh)),
                      ceiling(max(datos_dia$energia_global_kwh))
                    ) else c(0, 100),
                    step = 1
                  )
                ),
                column(
                  width = 6,
                  br(),
                  downloadButton(
                    "descargar_filtrado",
                    "Descargar datos filtrados",
                    class = "btn-primary"
                  )
                )
              )
            )
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_explora_n", width = 3),
          valueBoxOutput("kpi_explora_media", width = 3),
          valueBoxOutput("kpi_explora_fin", width = 3),
          valueBoxOutput("kpi_explora_outliers", width = 3)
        ),

        fluidRow(
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Serie temporal según filtros",
            plotlyOutput("graf_explora_serie", height = "390px")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Interpretación automática",
            htmlOutput("texto_explorador")
          )
        ),

        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Distribución por tipo de día",
            plotlyOutput("graf_explora_box", height = "360px")
          ),
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Relación con energía global",
            plotlyOutput("graf_explora_scatter", height = "360px")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = "Perfil horario interactivo",
            uiOutput("control_horario"),
            plotlyOutput("graf_explora_horario", height = "390px")
          )
        ),

        fluidRow(
          box(
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            title = "Huella energética semanal | Hora × día",
            plotlyOutput("graf_huella_semanal", height = "430px")
          ),
          box(
            width = 5,
            status = "success",
            solidHeader = TRUE,
            title = "Composición estimada del consumo activo diario",
            p(
              class = "data-note",
              "Consumo no submedido = energía activa global menos los tres submedidores. Es una categoría residual y no corresponde a un circuito medido directamente."
            ),
            htmlOutput("texto_composicion"),
            plotlyOutput("graf_composicion_consumo", height = "360px")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Detalle de días seleccionados",
            DTOutput("tabla_explorador")
          )
        )
      ),

      # --------------------------------------------------------
      # ESTRUCTURA MULTIVARIADA
      # --------------------------------------------------------
      tabItem(
        tabName = "multivariado",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "¿Qué información se repite y qué patrones resumen el comportamiento eléctrico?",
            p(
              "Aquí se estudia cómo se relacionan las siete variables eléctricas y cuánto puede simplificarse la información sin perder su estructura principal. PCA resume la variabilidad en pocas dimensiones; FA prueba si existen factores comunes detrás de las variables; y MDS coloca cerca los días con perfiles eléctricos similares."
            )
          )
        ),

        tabBox(
          width = 12,
          id = "tabs_multivariado",

          tabPanel(
            "PCA",
            fluidRow(
              valueBoxOutput("kpi_pca_pc1", width = 3),
              valueBoxOutput("kpi_pca_pc2", width = 3),
              valueBoxOutput("kpi_pca_4", width = 3),
              valueBoxOutput("kpi_pca_reduccion", width = 3)
            ),
            fluidRow(
              box(
                width = 6,
                title = "Varianza por componente",
                status = "primary",
                solidHeader = TRUE,
                p(
                  class = "data-note",
                  "La línea roja punteada representa la referencia de Kaiser: 14.29% de varianza por componente, equivalente a un autovalor igual a 1."
                ),
                plotlyOutput("graf_pca_varianza", height = "335px")
              ),
              box(
                width = 6,
                title = "Varianza acumulada",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("graf_pca_acumulada", height = "360px")
              )
            ),
            fluidRow(
              box(
                width = 8,
                title = "Mapa de días en el espacio PCA",
                status = "success",
                solidHeader = TRUE,
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "pca_eje_x",
                      "Eje X",
                      choices = paste0("PC", 1:4),
                      selected = "PC1"
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "pca_eje_y",
                      "Eje Y",
                      choices = paste0("PC", 1:4),
                      selected = "PC2"
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "pca_color",
                      "Colorear por",
                      choices = c(
                        "Tipo de día" = "tipo_dia",
                        "Cluster" = "cluster"
                      ),
                      selected = "tipo_dia"
                    )
                  )
                ),
                plotlyOutput("graf_pca_scores", height = "440px")
              ),
              box(
                width = 4,
                title = "Lectura del PCA",
                status = "info",
                solidHeader = TRUE,
                div(
                  class = "insight-box",
                  p("PC1 resume principalmente nivel general de consumo y demanda eléctrica."),
                  p("PC2 contrasta principalmente potencia reactiva y voltaje."),
                  p("Se retienen 4 componentes para conservar 89.8% de la información."),
                  p("PC7 explica apenas 0.003%, consistente con la redundancia entre energía e intensidad.")
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Cargas de los componentes principales",
                status = "primary",
                solidHeader = TRUE,
                DTOutput("tabla_pca_cargas")
              )
            )
          ),

          tabPanel(
            "Análisis Factorial",
            fluidRow(
              valueBoxOutput("kpi_fa_kmo", width = 3),
              valueBoxOutput("kpi_fa_bartlett", width = 3),
              valueBoxOutput("kpi_fa_factores", width = 3),
              valueBoxOutput("kpi_fa_estado", width = 3)
            ),
            fluidRow(
              box(
                width = 6,
                title = "MSA por variable",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("graf_fa_msa", height = "360px")
              ),
              box(
                width = 6,
                title = "Diagnóstico de la solución",
                status = "warning",
                solidHeader = TRUE,
                div(
                  class = "warning-box",
                  strong("La solución de 2 factores no debe interpretarse como estable."),
                  br(), br(),
                  "KMO global bajo, MSA muy bajo para potencia reactiva y voltaje, y comunalidades superiores a 1 para energía global e intensidad. Esto produce unicidades negativas y un caso ultra-Heywood."
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Cargas, comunalidades y unicidades",
                status = "primary",
                solidHeader = TRUE,
                DTOutput("tabla_fa")
              )
            )
          ),

          tabPanel(
            "MDS",
            fluidRow(
              valueBoxOutput("kpi_mds_gof", width = 4),
              valueBoxOutput("kpi_mds_dias", width = 4),
              valueBoxOutput("kpi_mds_dim", width = 4)
            ),
            fluidRow(
              box(
                width = 9,
                title = "Mapa MDS de similitud entre días",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  "mds_color",
                  "Colorear por",
                  choices = c(
                    "Tipo de día" = "tipo_dia",
                    "Cluster" = "cluster"
                  ),
                  selected = "tipo_dia"
                ),
                plotlyOutput("graf_mds", height = "470px")
              ),
              box(
                width = 3,
                title = "Cómo leerlo",
                status = "info",
                solidHeader = TRUE,
                div(
                  class = "insight-box",
                  p("Puntos cercanos representan días con perfiles eléctricos similares."),
                  p("Puntos alejados representan patrones multivariados diferentes."),
                  p("El GOF de 66.2% indica una representación útil, pero no completa, de la estructura original.")
                )
              )
            )
          ),

          tabPanel(
            "Correlaciones",
            fluidRow(
              box(
                width = 8,
                title = "Mapa de correlaciones",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("graf_correlacion", height = "530px")
              ),
              box(
                width = 4,
                title = "Hallazgo principal",
                status = "success",
                solidHeader = TRUE,
                div(
                  class = "success-box",
                  p("Energía global e intensidad media: r = 0.9994."),
                  p("Submedidor 3 y energía global: r = 0.7631."),
                  p("La primera relación evidencia redundancia estadística; la segunda identifica el subconsumo con mayor asociación marginal con el total.")
                )
              )
            )
          )
        )
      ),

      # --------------------------------------------------------
      # PERFILES DE CONSUMO
      # --------------------------------------------------------
      tabItem(
        tabName = "perfiles",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Perfiles de consumo: resultado oficial y laboratorio interactivo",
            p(
              "El resultado oficial del proyecto utiliza K-Means con k = 2. El control inferior permite explorar qué ocurre al cambiar el número de clusters sin modificar la conclusión oficial del informe."
            )
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_cluster_1", width = 3),
          valueBoxOutput("kpi_cluster_2", width = 3),
          valueBoxOutput("kpi_cluster_sil", width = 3),
          valueBoxOutput("kpi_cluster_fin", width = 3)
        ),

        fluidRow(
          box(
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            title = "Evaluación del número de clusters",
            plotlyOutput("graf_cluster_silhouette", height = "390px")
          ),
          box(
            width = 5,
            status = "success",
            solidHeader = TRUE,
            title = "Interpretación oficial",
            div(
              class = "success-box",
              p("Cluster 1: perfil de mayor consumo, 307 días, 36.26 kWh/día en promedio."),
              p("Cluster 2: perfil de menor consumo, 416 días, 19.45 kWh/día en promedio."),
              p("41.37% de los días del Cluster 1 son fines de semana, frente a 18.51% en el Cluster 2."),
              p("El Silhouette de 0.2629 indica que los perfiles son útiles descriptivamente, pero no constituyen grupos rígidos.")
            )
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Laboratorio: cambie k y observe cómo se reorganizan los días",
            sliderInput(
              "k_interactivo",
              "Número de clusters",
              min = 2,
              max = 6,
              value = 2,
              step = 1
            )
          )
        ),

        fluidRow(
          box(
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            title = "Clusters interactivos en el espacio PCA",
            plotlyOutput("graf_cluster_interactivo", height = "430px")
          ),
          box(
            width = 5,
            status = "primary",
            solidHeader = TRUE,
            title = "Perfil promedio estandarizado",
            plotlyOutput("graf_cluster_perfil", height = "430px")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = "Perfil horario de los dos clusters oficiales",
            plotlyOutput("graf_cluster_horario", height = "390px")
          )
        )
      ),

      # --------------------------------------------------------
      # CLASIFICACION
      # --------------------------------------------------------
      tabItem(
        tabName = "clasificacion",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Clasificación de días de consumo alto",
            p(
              "El modelo utiliza dos puntos de corte distintos. Primero, se define qué significa consumo alto: un día se considera alto si supera 26.71 kWh/día, valor obtenido con la mediana del conjunto de entrenamiento. Segundo, la regresión logística estima una probabilidad y el usuario decide desde qué probabilidad clasificar una observación como alta. Intensidad se excluyó como predictor por su correlación casi perfecta con energía global."
            ),
            p(
              class = "data-note",
              "El valor de 26.71 kWh/día es un umbral estadístico para este hogar y este período; no representa un límite técnico, contractual ni tarifario."
            )
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_logit_umbral", width = 3),
          valueBoxOutput("kpi_logit_accuracy", width = 3),
          valueBoxOutput("kpi_logit_f1", width = 3),
          valueBoxOutput("kpi_logit_auc", width = 3)
        ),

        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Simulador del punto de corte de probabilidad",
            sliderInput(
              "corte_logit",
              "Probabilidad mínima estimada para predecir consumo alto",
              min = 0.10,
              max = 0.90,
              value = 0.50,
              step = 0.01
            ),
            div(
              style = "margin-bottom: 10px;",
              actionButton(
                "usar_corte_050",
                "0.50 · Referencia",
                icon = icon("flag")
              ),
              actionButton(
                "usar_corte_061",
                "0.61 · Mejor balance observado",
                icon = icon("balance-scale")
              ),
              actionButton(
                "usar_corte_074",
                "0.74 · Más restrictivo",
                icon = icon("shield")
              )
            ),
            p(
              class = "data-note",
              "Este control no cambia qué significa consumo alto (26.71 kWh/día). Solo cambia desde qué probabilidad estimada el modelo emite la predicción Alto. El corte 0.50 se mantiene como referencia; 0.61 es un resultado exploratorio observado en el conjunto de prueba y no un umbral óptimo validado; 0.74 ilustra un escenario más restrictivo. El AUC no cambia porque evalúa el ordenamiento de probabilidades a través de todos los cortes."
            )
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_logit_precision_dyn", width = 3),
          valueBoxOutput("kpi_logit_recall_dyn", width = 3),
          valueBoxOutput("kpi_logit_specificity_dyn", width = 3),
          valueBoxOutput("kpi_logit_balanced_dyn", width = 3)
        ),

        fluidRow(
          box(
            width = 8,
            status = "success",
            solidHeader = TRUE,
            title = "Trade-off del punto de corte",
            plotlyOutput("graf_logit_sensibilidad", height = "380px")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Tres escenarios de decisión",
            DTOutput("tabla_cortes_logit")
          )
        ),

        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Odds Ratios del modelo",
            plotlyOutput("graf_logit_or", height = "420px")
          ),
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Matriz de confusión según corte",
            plotlyOutput("graf_logit_confusion", height = "420px")
          )
        ),

        fluidRow(
          box(
            width = 8,
            status = "success",
            solidHeader = TRUE,
            title = "Distribución de probabilidades en prueba",
            plotlyOutput("graf_logit_prob", height = "360px")
          ),
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Decisión operativa",
            htmlOutput("texto_logit_decision")
          )
        )
      ),

      # --------------------------------------------------------
      # MANOVA
      # --------------------------------------------------------
      tabItem(
        tabName = "manova",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "¿Difiere el consumo entre días laborales y fines de semana?",
            p(
              "MANOVA evalúa simultáneamente las siete variables eléctricas. Debido a la heterogeneidad de covarianzas, Pillai se utiliza como criterio principal."
            )
          )
        ),

        fluidRow(
          valueBoxOutput("kpi_manova_pillai", width = 3),
          valueBoxOutput("kpi_manova_f", width = 3),
          valueBoxOutput("kpi_boxm", width = 3),
          valueBoxOutput("kpi_manova_sens", width = 3)
        ),

        fluidRow(
          box(
            width = 5,
            status = "primary",
            solidHeader = TRUE,
            title = "Variable a comparar",
            selectInput(
              "variable_manova",
              "Seleccionar variable",
              choices = setNames(names(etiquetas_vars), etiquetas_vars),
              selected = "energia_global_kwh"
            ),
            plotlyOutput("graf_manova_box", height = "380px")
          ),
          box(
            width = 7,
            status = "success",
            solidHeader = TRUE,
            title = "Diferencias estandarizadas: fin de semana - laboral",
            plotlyOutput("graf_manova_diferencias", height = "430px")
          )
        ),

        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Medias por tipo de día",
            DTOutput("tabla_manova_medias")
          ),
          box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = "Interpretación",
            div(
              class = "insight-box",
              p("Pillai = 0.1549; evidencia de diferencias multivariadas entre laborales y fines de semana."),
              p("El voltaje es la única variable sin diferencia univariada significativa en el seguimiento."),
              p("La conclusión se mantiene al retirar los 16 outliers multivariados."),
              p("La inferencia debe leerse con cautela por no normalidad, heterogeneidad de covarianzas y autocorrelación temporal.")
            )
          )
        )
      ),

      # --------------------------------------------------------
      # CONCLUSIONES
      # --------------------------------------------------------
      tabItem(
        tabName = "conclusiones",

        div(
          class = "hero-panel",
          div(class = "eyebrow", "Cierre del proyecto"),
          h2("De los resultados estadísticos a decisiones de consumo"),
          p(
            "Esta vista resume la evidencia encontrada y traduce cada pregunta del proyecto en una conclusión y una implicación práctica, sin convertir asociaciones estadísticas en afirmaciones causales."
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = "Matriz ejecutiva de resultados",
            DTOutput("tabla_decisiones")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Oportunidades de gestión energética",
            fluidRow(
              column(
                width = 3,
                div(
                  class = "question-card",
                  h4("1. Cuándo actuar"),
                  p("Fines de semana y horas de la tarde-noche concentran niveles más altos de consumo y merecen seguimiento diferenciado."),
                  span(class = "result-tag", "Tiempo y demanda")
                )
              ),
              column(
                width = 3,
                div(
                  class = "question-card",
                  h4("2. Qué monitorear"),
                  p("El Submedidor 3 es el consumo específico más relacionado con energía global y con la probabilidad de consumo alto."),
                  span(class = "result-tag", "Submedidor 3")
                )
              ),
              column(
                width = 3,
                div(
                  class = "question-card",
                  h4("3. Cómo alertar"),
                  p("El punto de corte de probabilidad puede ajustarse al costo operativo de una falsa alerta frente al costo de no detectar un día alto."),
                  span(class = "result-tag", "0.50 como referencia")
                )
              ),
              column(
                width = 3,
                div(
                  class = "question-card",
                  h4("4. Qué información falta"),
                  p("Una parte relevante del consumo queda fuera de los tres submedidores y se identifica únicamente como consumo no submedido."),
                  span(class = "result-tag", "Mayor granularidad")
                )
              )
            )
          )
        ),

        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Recomendaciones",
            div(
              class = "success-box",
              p(strong("1."), "Ampliar futuros estudios a múltiples viviendas para evaluar generalización."),
              p(strong("2."), "Complementar el análisis con modelos de series temporales que incorporen autocorrelación."),
              p(strong("3."), "Incorporar temperatura, feriados, vacaciones, ocupación y horarios de permanencia."),
              p(strong("4."), "Profundizar en períodos de mayor demanda, especialmente fines de semana y tarde-noche."),
              p(strong("5."), "Investigar con mayor detalle los equipos vinculados al submedidor 3 antes de recomendar acciones de ahorro."),
              p(strong("6."), "Si el modelo se utiliza como alerta, definir el punto de corte según el costo relativo de falsas alarmas y consumos altos no detectados; 0.61 puede utilizarse como referencia exploratoria para una validación posterior.")
            )
          ),
          box(
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            title = "Limitaciones",
            div(
              class = "warning-box",
              p("Una única vivienda: los resultados no representan directamente a otros hogares."),
              p("Dependencia temporal: los días consecutivos no son completamente independientes."),
              p("Clasificación calendario limitada: no se distinguen feriados ni vacaciones."),
              p("Cobertura diaria ≥95%: se asume que los minutos faltantes no son sistemáticamente distintos."),
              p("Redundancia energía-intensidad: debe considerarse en reducción dimensional, MANOVA y regresión."),
              p("El corte 0.61 se identificó exploratoriamente en el conjunto de prueba; requiere validación independiente antes de utilizarse como regla operativa.")
            )
          )
        )
      ),

      # --------------------------------------------------------
      # METODOLOGIA
      # --------------------------------------------------------
      tabItem(
        tabName = "metodologia",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Contexto del proyecto",
            p(strong("Universidad de San Carlos de Guatemala")),
            p("Facultad de Ingeniería | Escuela de Estudios de Postgrado"),
            p("Maestría en Estadística Aplicada"),
            p("Métodos Multivariados aplicados al Análisis de Datos"),
            p("Maestría en Estadística Aplicada | USAC | 3er. Trimestre 2026")
          )
        ),

        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Pipeline analítico",
            div(class = "method-pill", "1. Calidad y depuración"),
            div(class = "method-pill", "2. Agregación diaria"),
            div(class = "method-pill", "3. Estandarización"),
            div(class = "method-pill", "4. Correlación"),
            div(class = "method-pill", "5. Mahalanobis"),
            div(class = "method-pill", "6. Mardia"),
            div(class = "method-pill", "7. PCA"),
            div(class = "method-pill", "8. FA"),
            div(class = "method-pill", "9. MDS"),
            div(class = "method-pill", "10. Clustering"),
            div(class = "method-pill", "11. Logística"),
            div(class = "method-pill", "12. MANOVA"),
            br(), br(),
            p("La base minuto a minuto se utiliza para patrones temporales. Los métodos multivariados principales operan a nivel diario para reducir dependencia de muy corto plazo y hacer viables los métodos de distancia.")
          ),
          box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = "Base de análisis",
            fluidRow(
              column(6, div(class = "metric-inline", "1,048,575"), div(class = "metric-caption", "registros originales")),
              column(6, div(class = "metric-inline", "1,044,506"), div(class = "metric-caption", "registros completos"))
            ),
            br(),
            fluidRow(
              column(6, div(class = "metric-inline", "728"), div(class = "metric-caption", "días construidos")),
              column(6, div(class = "metric-inline", "723"), div(class = "metric-caption", "días con cobertura ≥95%"))
            ),
            br(),
            p("Período analítico diario: 17/12/2006 al 12/12/2008."),
            p("519 días laborales y 204 fines de semana.")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Revisión de supuestos",
            DTOutput("tabla_supuestos")
          )
        ),

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Integrantes Grupo 4",
            DTOutput("tabla_integrantes")
          )
        )
      )
    )
  )
)

# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Navegación
  # ----------------------------------------------------------
  observeEvent(input$ir_explorador, {
    updateTabItems(session, "tabs", "explorador")
  })

  # ----------------------------------------------------------
  # KPIs de inicio
  # ----------------------------------------------------------
  output$kpi_dias <- renderValueBox({
    req(analisis_disponible)
    valueBox(
      nrow(datos_dia),
      "Días analizados",
      icon = icon("calendar-check"),
      color = "blue"
    )
  })

  output$kpi_consumo <- renderValueBox({
    req(analisis_disponible)
    valueBox(
      paste0(formato_num(mean(datos_dia$energia_global_kwh), 2), " kWh"),
      "Consumo diario medio",
      icon = icon("bolt"),
      color = "aqua"
    )
  })

  output$kpi_pca <- renderValueBox({
    req(analisis_disponible)
    valueBox(
      paste0(formato_num(acum_var_pca[4] * 100, 1), "%"),
      "Varianza con 4 PC",
      icon = icon("compress"),
      color = "purple"
    )
  })

  output$kpi_clusters <- renderValueBox({
    valueBox(
      "2",
      "Perfiles K-Means",
      icon = icon("users"),
      color = "teal"
    )
  })

  output$kpi_auc <- renderValueBox({
    req(analisis_disponible)
    valueBox(
      formato_num(auc_oficial, 3),
      "AUC logística",
      icon = icon("bullseye"),
      color = "green"
    )
  })

  output$kpi_manova <- renderValueBox({
    valueBox(
      "p < 0.001",
      "MANOVA laboral vs fin",
      icon = icon("balance-scale"),
      color = "yellow"
    )
  })

  output$kpi_fin_premium <- renderValueBox({
    req(analisis_disponible)

    medias_tipo <- tapply(
      datos_dia$energia_global_kwh,
      datos_dia$tipo_dia,
      mean,
      na.rm = TRUE
    )

    prima_fin <- (
      medias_tipo["Fin de semana"] /
      medias_tipo["Laboral"] -
      1
    ) * 100

    valueBox(
      formato_pct(prima_fin, 1),
      "Consumo fin de semana vs laboral",
      icon = icon("calendar-plus"),
      color = "red"
    )
  })

  output$kpi_hora_pico <- renderValueBox({
    req(archivo_horario_existe)

    suma_energia <- tapply(
      datos_hora$potencia_activa_media * datos_hora$n_minutos,
      datos_hora$hora,
      sum,
      na.rm = TRUE
    )

    suma_minutos <- tapply(
      datos_hora$n_minutos,
      datos_hora$hora,
      sum,
      na.rm = TRUE
    )

    curva <- suma_energia / suma_minutos
    hora_pico <- as.integer(names(curva)[which.max(curva)])

    valueBox(
      paste0(sprintf("%02d", hora_pico), ":00"),
      "Hora pico promedio",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$kpi_sub3_share <- renderValueBox({
    req(analisis_disponible)

    participacion <- mean(
      datos_dia$sub3_kwh,
      na.rm = TRUE
    ) /
      mean(
        datos_dia$energia_global_kwh,
        na.rm = TRUE
      ) * 100

    valueBox(
      formato_pct(participacion, 1),
      "Participación media Submedidor 3",
      icon = icon("pie-chart"),
      color = "green"
    )
  })

  output$kpi_cluster_ratio <- renderValueBox({
    req(analisis_disponible)

    medias_cluster <- tapply(
      datos_dia$energia_global_kwh,
      datos_dia$cluster_kmeans,
      mean,
      na.rm = TRUE
    )

    relacion <- max(medias_cluster) / min(medias_cluster)

    valueBox(
      paste0(formato_num(relacion, 2), "x"),
      "Consumo perfil alto vs bajo",
      icon = icon("arrows-v"),
      color = "teal"
    )
  })

  output$graf_inicio_serie <- renderPlotly({
    req(analisis_disponible)

    g <- ggplot(
      datos_dia,
      aes(
        x = fecha,
        y = energia_global_kwh,
        group = 1,
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>Energía: ", round(energia_global_kwh, 2), " kWh",
          "<br>Tipo: ", tipo_dia,
          "<br>Cluster: ", cluster_kmeans
        )
      )
    ) +
      geom_line(linewidth = 0.45, color = "#2389c9") +
      labs(
        x = "Fecha",
        y = "Energía global (kWh/día)"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$texto_inicio_gerencial <- renderUI({
    HTML(
      paste0(
        "<div class='success-box'>",
        "1. El consumo tiene <b>estructura temporal</b>; no todos los días se comportan igual.<br><br>",
        "2. Los fines de semana presentan un nivel de consumo multivariado mayor.<br><br>",
        "3. Existen dos perfiles generales: uno de mayor y otro de menor consumo, aunque se solapan.<br><br>",
        "4. La <b>intensidad</b> está casi perfectamente asociada con energía global y el <b>Submedidor 3</b> es el consumo específico con mayor relación con el total.<br><br>",
        "5. La clasificación logística distingue días de consumo alto con AUC de <b>",
        formato_num(auc_oficial, 3),
        "</b>.",
        "</div>"
      )
    )
  })

  # ----------------------------------------------------------
  # Explorador
  # ----------------------------------------------------------
  observeEvent(input$reiniciar_filtros, {
    req(analisis_disponible)

    updateDateRangeInput(
      session,
      "filtro_fechas",
      start = min(datos_dia$fecha),
      end = max(datos_dia$fecha)
    )

    updateCheckboxGroupInput(
      session,
      "filtro_tipo_dia",
      selected = c("Laboral", "Fin de semana")
    )

    updateCheckboxGroupInput(
      session,
      "filtro_cluster",
      selected = c("1", "2")
    )

    updateSliderInput(
      session,
      "filtro_energia",
      value = c(
        floor(min(datos_dia$energia_global_kwh)),
        ceiling(max(datos_dia$energia_global_kwh))
      )
    )
  })

  datos_filtrados <- reactive({
    req(analisis_disponible)
    req(input$filtro_fechas)
    req(input$filtro_tipo_dia)
    req(input$filtro_cluster)
    req(input$filtro_energia)

    df <- datos_dia %>%
      filter(
        fecha >= input$filtro_fechas[1],
        fecha <= input$filtro_fechas[2],
        tipo_dia %in% input$filtro_tipo_dia,
        as.character(cluster_kmeans) %in% input$filtro_cluster,
        energia_global_kwh >= input$filtro_energia[1],
        energia_global_kwh <= input$filtro_energia[2]
      )

    validate(
      need(nrow(df) > 0, "No hay observaciones para los filtros seleccionados.")
    )

    df
  })

  datos_hora_filtrados <- reactive({
    req(archivo_horario_existe)
    req(input$filtro_fechas)
    req(input$filtro_tipo_dia)
    req(input$filtro_cluster)

    datos_hora %>%
      filter(
        fecha >= input$filtro_fechas[1],
        fecha <= input$filtro_fechas[2],
        tipo_dia %in% input$filtro_tipo_dia,
        as.character(cluster_kmeans) %in% input$filtro_cluster
      )
  })

  output$descargar_filtrado <- downloadHandler(
    filename = function() {
      paste0("consumo_filtrado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        datos_filtrados(),
        file,
        row.names = FALSE
      )
    }
  )

  output$kpi_explora_n <- renderValueBox({
    df <- datos_filtrados()
    valueBox(
      nrow(df),
      "Días seleccionados",
      icon = icon("calendar"),
      color = "blue"
    )
  })

  output$kpi_explora_media <- renderValueBox({
    df <- datos_filtrados()
    var <- input$variable_explorador
    valueBox(
      paste0(
        formato_num(mean(df[[var]], na.rm = TRUE), 2),
        " ",
        unidades_vars[[var]]
      ),
      paste("Promedio de", etiquetas_vars[[var]]),
      icon = icon("line-chart"),
      color = "aqua"
    )
  })

  output$kpi_explora_fin <- renderValueBox({
    df <- datos_filtrados()
    pct <- mean(df$tipo_dia == "Fin de semana") * 100
    valueBox(
      formato_pct(pct, 1),
      "Fines de semana",
      icon = icon("calendar-o"),
      color = "purple"
    )
  })

  output$kpi_explora_outliers <- renderValueBox({
    df <- datos_filtrados()
    valueBox(
      sum(df$outlier_multivariado, na.rm = TRUE),
      "Outliers multivariados",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$graf_explora_serie <- renderPlotly({
    df <- datos_filtrados()
    var <- input$variable_explorador
    etiqueta <- etiquetas_vars[[var]]
    unidad <- unidades_vars[[var]]

    g <- ggplot(
      df,
      aes(
        x = fecha,
        y = .data[[var]],
        color = tipo_dia,
        group = tipo_dia,
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>", etiqueta, ": ", round(.data[[var]], 3), " ", unidad,
          "<br>Tipo: ", tipo_dia,
          "<br>Cluster: ", cluster_kmeans
        )
      )
    ) +
      geom_line(linewidth = 0.55, alpha = 0.85) +
      geom_point(size = 1.2, alpha = 0.65) +
      labs(
        x = "Fecha",
        y = paste0(etiqueta, " (", unidad, ")"),
        color = "Tipo de día"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_explora_box <- renderPlotly({
    df <- datos_filtrados()
    var <- input$variable_explorador
    etiqueta <- etiquetas_vars[[var]]

    g <- ggplot(
      df,
      aes(
        x = tipo_dia,
        y = .data[[var]],
        fill = tipo_dia,
        text = paste0(
          "Tipo: ", tipo_dia,
          "<br>", etiqueta, ": ", round(.data[[var]], 3)
        )
      )
    ) +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
      labs(
        x = "Tipo de día",
        y = etiqueta,
        fill = "Tipo de día"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(g, tooltip = "text")
  })

  output$graf_explora_scatter <- renderPlotly({
    df <- datos_filtrados()
    var <- input$variable_explorador

    if (var == "energia_global_kwh") {
      var <- "intensidad_media"
    }

    etiqueta <- etiquetas_vars[[var]]

    g <- ggplot(
      df,
      aes(
        x = .data[[var]],
        y = energia_global_kwh,
        color = cluster_kmeans,
        shape = tipo_dia,
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>", etiqueta, ": ", round(.data[[var]], 3),
          "<br>Energía: ", round(energia_global_kwh, 2), " kWh",
          "<br>Cluster: ", cluster_kmeans
        )
      )
    ) +
      geom_point(alpha = 0.72, size = 2) +
      labs(
        x = etiqueta,
        y = "Energía global (kWh/día)",
        color = "Cluster",
        shape = "Tipo de día"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$control_horario <- renderUI({
    if (!archivo_horario_existe) {
      return(
        div(
          class = "warning-box",
          "No se encontró datos_dashboard_horarios.csv. El resto del dashboard continúa disponible."
        )
      )
    }

    radioButtons(
      "metrica_horaria",
      "Métrica horaria",
      choices = c(
        "Potencia activa media (kW)" = "potencia_activa_media",
        "Intensidad media (A)" = "intensidad_media"
      ),
      selected = "potencia_activa_media",
      inline = TRUE
    )
  })

  output$graf_explora_horario <- renderPlotly({
    req(archivo_horario_existe)
    req(input$metrica_horaria)

    df <- datos_hora_filtrados()
    var <- input$metrica_horaria

    resumen <- df %>%
      group_by(tipo_dia, hora) %>%
      summarise(
        valor = weighted.mean(
          .data[[var]],
          w = n_minutos,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    y_lab <- ifelse(
      var == "potencia_activa_media",
      "Potencia activa media (kW)",
      "Intensidad media (A)"
    )

    g <- ggplot(
      resumen,
      aes(
        x = hora,
        y = valor,
        color = tipo_dia,
        group = tipo_dia,
        text = paste0(
          "Hora: ", hora, ":00",
          "<br>Tipo: ", tipo_dia,
          "<br>Valor: ", round(valor, 3)
        )
      )
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      scale_x_continuous(breaks = 0:23) +
      labs(
        x = "Hora del día",
        y = y_lab,
        color = "Tipo de día"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_huella_semanal <- renderPlotly({
    req(archivo_horario_existe)

    df <- datos_hora_filtrados()

    validate(
      need("dia_semana" %in% names(df), "No se pudo construir el día de la semana."),
      need(nrow(df) > 0, "No hay datos para los filtros seleccionados.")
    )

    resumen <- df %>%
      group_by(dia_semana, hora) %>%
      summarise(
        potencia = weighted.mean(
          potencia_activa_media,
          w = n_minutos,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    g <- ggplot(
      resumen,
      aes(
        x = hora,
        y = dia_semana,
        fill = potencia,
        text = paste0(
          "Día: ", dia_semana,
          "<br>Hora: ", sprintf("%02d:00", hora),
          "<br>Potencia media: ", round(potencia, 3), " kW"
        )
      )
    ) +
      geom_tile() +
      scale_x_continuous(breaks = 0:23) +
      labs(
        x = "Hora del día",
        y = "",
        fill = "kW"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_composicion_consumo <- renderPlotly({
    df <- datos_filtrados()

    resumen <- aggregate(
      df[
        ,
        c(
          "energia_global_kwh",
          "sub1_kwh",
          "sub2_kwh",
          "sub3_kwh"
        ),
        drop = FALSE
      ],
      by = list(
        tipo_dia = df$tipo_dia
      ),
      FUN = mean,
      na.rm = TRUE
    )

    resumen$no_submedido_kwh <- pmax(
      resumen$energia_global_kwh -
        resumen$sub1_kwh -
        resumen$sub2_kwh -
        resumen$sub3_kwh,
      0
    )

    largo <- data.frame(
      tipo_dia = rep(resumen$tipo_dia, times = 4),
      Componente = rep(
        c(
          "Cocina (Sub 1)",
          "Lavandería (Sub 2)",
          "Climatización (Sub 3)",
          "Consumo no submedido"
        ),
        each = nrow(resumen)
      ),
      kWh = c(
        resumen$sub1_kwh,
        resumen$sub2_kwh,
        resumen$sub3_kwh,
        resumen$no_submedido_kwh
      )
    )

    g <- ggplot(
      largo,
      aes(
        x = tipo_dia,
        y = kWh,
        fill = Componente,
        text = paste0(
          "Tipo de día: ", tipo_dia,
          "<br>Componente: ", Componente,
          "<br>Consumo medio: ", round(kWh, 2), " kWh/día"
        )
      )
    ) +
      geom_col() +
      labs(
        x = "",
        y = "Consumo medio (kWh/día)",
        fill = "Componente"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$texto_composicion <- renderUI({
    df <- datos_filtrados()

    total <- mean(
      df$energia_global_kwh,
      na.rm = TRUE
    )

    submedido <- mean(
      df$sub1_kwh + df$sub2_kwh + df$sub3_kwh,
      na.rm = TRUE
    )

    no_submedido <- max(
      total - submedido,
      0
    )

    pct_submedido <- ifelse(
      total > 0,
      100 * submedido / total,
      NA
    )

    pct_no_submedido <- ifelse(
      total > 0,
      100 * no_submedido / total,
      NA
    )

    HTML(
      paste0(
        "<div class='insight-box' style='margin-bottom:10px;'>",
        "En la selección actual, los tres submedidores representan aproximadamente <b>",
        formato_pct(pct_submedido, 1),
        "</b> del consumo activo medio; el <b>",
        formato_pct(pct_no_submedido, 1),
        "</b> restante se registra como consumo no submedido.",
        "</div>"
      )
    )
  })

  output$texto_explorador <- renderUI({
    df <- datos_filtrados()
    var <- input$variable_explorador

    media_total <- mean(datos_dia[[var]], na.rm = TRUE)
    media_sel <- mean(df[[var]], na.rm = TRUE)
    diferencia <- (media_sel / media_total - 1) * 100

    if (diferencia >= 5) {
      lectura <- paste0(
        "El período seleccionado presenta un nivel de ",
        etiquetas_vars[[var]],
        " aproximadamente ",
        formato_pct(abs(diferencia), 1),
        " superior al promedio general."
      )
    } else if (diferencia <= -5) {
      lectura <- paste0(
        "El período seleccionado presenta un nivel de ",
        etiquetas_vars[[var]],
        " aproximadamente ",
        formato_pct(abs(diferencia), 1),
        " inferior al promedio general."
      )
    } else {
      lectura <- paste0(
        "El nivel de ",
        etiquetas_vars[[var]],
        " se mantiene cercano al promedio general del período."
      )
    }

    cluster_tab <- prop.table(table(df$cluster_kmeans)) * 100
    cluster_dom <- names(cluster_tab)[which.max(cluster_tab)]
    pct_cluster <- max(cluster_tab)

    HTML(
      paste0(
        "<div class='insight-box'>",
        "<b>Lectura del escenario:</b><br><br>",
        lectura,
        "<br><br>",
        "El cluster más frecuente es <b>", cluster_dom, "</b>, con ",
        formato_pct(pct_cluster, 1),
        " de los días seleccionados.<br><br>",
        "Se observan <b>",
        sum(df$outlier_multivariado, na.rm = TRUE),
        "</b> días marcados como atípicos multivariados.",
        "</div>"
      )
    )
  })

  output$tabla_explorador <- renderDT({
    df <- datos_filtrados()[
      ,
      c(
        "fecha",
        "tipo_dia",
        "cluster_kmeans",
        "energia_global_kwh",
        "intensidad_media",
        "reactiva_media",
        "voltaje_medio",
        "sub1_kwh",
        "sub2_kwh",
        "sub3_kwh",
        "outlier_multivariado"
      ),
      drop = FALSE
    ]

    names(df) <- c(
      "Fecha",
      "Tipo de día",
      "Cluster",
      "Energía global",
      "Intensidad",
      "Reactiva",
      "Voltaje",
      "Sub 1",
      "Sub 2",
      "Sub 3",
      "Outlier"
    )

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # ----------------------------------------------------------
  # PCA
  # ----------------------------------------------------------
  output$kpi_pca_pc1 <- renderValueBox({
    valueBox(
      formato_pct(prop_var_pca[1] * 100, 1),
      "Varianza PC1",
      icon = icon("bar-chart"),
      color = "blue"
    )
  })

  output$kpi_pca_pc2 <- renderValueBox({
    valueBox(
      formato_pct(acum_var_pca[2] * 100, 1),
      "Acumulado PC1 + PC2",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })

  output$kpi_pca_4 <- renderValueBox({
    valueBox(
      formato_pct(acum_var_pca[4] * 100, 1),
      "Acumulado 4 PC",
      icon = icon("compress"),
      color = "green"
    )
  })

  output$kpi_pca_reduccion <- renderValueBox({
    valueBox(
      "42.86%",
      "Reducción dimensional",
      icon = icon("arrows-alt"),
      color = "purple"
    )
  })

  output$graf_pca_varianza <- renderPlotly({
    g <- ggplot(
      tabla_pca,
      aes(
        x = Componente,
        y = Varianza_Pct,
        text = paste0(
          Componente,
          "<br>Autovalor: ", round(Autovalor, 4),
          "<br>Varianza: ", round(Varianza_Pct, 2), "%"
        )
      )
    ) +
      geom_col(fill = "#2389c9") +
      geom_hline(
        yintercept = 100 / 7,
        linetype = "dotted",
        color = "#d9534f"
      ) +
      labs(
        x = "Componente",
        y = "Varianza explicada (%)",
        subtitle = "Referencia Kaiser = 14.29% (autovalor = 1)"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_pca_acumulada <- renderPlotly({
    g <- ggplot(
      tabla_pca,
      aes(
        x = Componente,
        y = Varianza_Acumulada_Pct,
        group = 1,
        text = paste0(
          Componente,
          "<br>Varianza acumulada: ",
          round(Varianza_Acumulada_Pct, 2), "%"
        )
      )
    ) +
      geom_line(linewidth = 1.1, color = "#17669a") +
      geom_point(size = 3, color = "#17669a") +
      geom_hline(
        yintercept = c(80, 90),
        linetype = "dashed",
        color = "#6c757d"
      ) +
      labs(
        x = "Componente",
        y = "Varianza acumulada (%)"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_pca_scores <- renderPlotly({
    req(input$pca_eje_x)
    req(input$pca_eje_y)
    req(input$pca_color)

    color_var <- input$pca_color

    g <- ggplot(
      scores_pca,
      aes(
        x = .data[[input$pca_eje_x]],
        y = .data[[input$pca_eje_y]],
        color = .data[[color_var]],
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>", input$pca_eje_x, ": ", round(.data[[input$pca_eje_x]], 3),
          "<br>", input$pca_eje_y, ": ", round(.data[[input$pca_eje_y]], 3),
          "<br>Tipo: ", tipo_dia,
          "<br>Cluster: ", cluster
        )
      )
    ) +
      geom_point(alpha = 0.72, size = 2) +
      labs(
        x = input$pca_eje_x,
        y = input$pca_eje_y,
        color = ifelse(color_var == "tipo_dia", "Tipo de día", "Cluster")
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$tabla_pca_cargas <- renderDT({
    df <- cargas_pca[
      ,
      c("Variable", "PC1", "PC2", "PC3", "PC4"),
      drop = FALSE
    ]

    columnas_numericas <- sapply(df, is.numeric)
    df[, columnas_numericas] <- lapply(
      df[, columnas_numericas, drop = FALSE],
      round,
      4
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 7, dom = "t", scrollX = TRUE)
    )
  })

  # ----------------------------------------------------------
  # FA
  # ----------------------------------------------------------
  output$kpi_fa_kmo <- renderValueBox({
    valueBox(
      formato_num(kmo_result$MSA, 3),
      "KMO global",
      icon = icon("check-square-o"),
      color = "yellow"
    )
  })

  output$kpi_fa_bartlett <- renderValueBox({
    valueBox(
      "p < 0.001",
      "Bartlett",
      icon = icon("calculator"),
      color = "green"
    )
  })

  output$kpi_fa_factores <- renderValueBox({
    valueBox(
      "2",
      "Factores sugeridos",
      icon = icon("columns"),
      color = "blue"
    )
  })

  output$kpi_fa_estado <- renderValueBox({
    valueBox(
      "No estable",
      "Solución factorial",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$graf_fa_msa <- renderPlotly({
    g <- ggplot(
      msa_tabla,
      aes(
        x = reorder(Variable, MSA),
        y = MSA,
        text = paste0(
          "Variable: ", Variable,
          "<br>MSA: ", round(MSA, 3)
        )
      )
    ) +
      geom_col(fill = "#2389c9") +
      geom_hline(
        yintercept = 0.70,
        linetype = "dashed",
        color = "#d9534f"
      ) +
      coord_flip() +
      labs(
        x = "Variable",
        y = "MSA"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$tabla_fa <- renderDT({
    df <- cargas_fa
    df$Comunalidad <- fa_result$communality[match(df$Variable, names(fa_result$communality))]
    df$Unicidad <- fa_result$uniquenesses[match(df$Variable, names(fa_result$uniquenesses))]
    df <- df[
      ,
      c("Variable", setdiff(names(df), "Variable")),
      drop = FALSE
    ]

    columnas_numericas <- sapply(df, is.numeric)
    df[, columnas_numericas] <- lapply(
      df[, columnas_numericas, drop = FALSE],
      round,
      4
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 7, dom = "t", scrollX = TRUE)
    )
  })

  # ----------------------------------------------------------
  # MDS y correlación
  # ----------------------------------------------------------
  output$kpi_mds_gof <- renderValueBox({
    valueBox(
      formato_pct(mds$GOF[1] * 100, 1),
      "GOF MDS",
      icon = icon("map"),
      color = "blue"
    )
  })

  output$kpi_mds_dias <- renderValueBox({
    valueBox(
      nrow(coord_mds),
      "Días representados",
      icon = icon("circle-o"),
      color = "aqua"
    )
  })

  output$kpi_mds_dim <- renderValueBox({
    valueBox(
      "2D",
      "Representación",
      icon = icon("arrows"),
      color = "purple"
    )
  })

  output$graf_mds <- renderPlotly({
    req(input$mds_color)

    color_var <- input$mds_color

    g <- ggplot(
      coord_mds,
      aes(
        x = MDS1,
        y = MDS2,
        color = .data[[color_var]],
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>MDS1: ", round(MDS1, 3),
          "<br>MDS2: ", round(MDS2, 3),
          "<br>Tipo: ", tipo_dia,
          "<br>Cluster: ", cluster
        )
      )
    ) +
      geom_point(alpha = 0.68, size = 2) +
      labs(
        x = "MDS 1",
        y = "MDS 2",
        color = ifelse(color_var == "tipo_dia", "Tipo de día", "Cluster")
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_correlacion <- renderPlotly({
    cor_long <- as.data.frame(as.table(matriz_cor))
    names(cor_long) <- c("Variable_1", "Variable_2", "Correlacion")

    g <- ggplot(
      cor_long,
      aes(
        x = Variable_1,
        y = Variable_2,
        fill = Correlacion,
        text = paste0(
          Variable_1,
          " × ",
          Variable_2,
          "<br>r = ",
          round(Correlacion, 3)
        )
      )
    ) +
      geom_tile() +
      geom_text(aes(label = round(Correlacion, 2)), size = 3) +
      scale_fill_gradient2(
        low = "#b2182b",
        mid = "white",
        high = "#2166ac",
        midpoint = 0,
        limits = c(-1, 1)
      ) +
      labs(x = "", y = "", fill = "r") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggplotly(g, tooltip = "text")
  })

  # ----------------------------------------------------------
  # Clusters
  # ----------------------------------------------------------
  perfiles_oficiales <- reactive({
    datos_dia %>%
      group_by(cluster_kmeans) %>%
      summarise(
        n = n(),
        energia = mean(energia_global_kwh),
        intensidad = mean(intensidad_media),
        sub1 = mean(sub1_kwh),
        sub2 = mean(sub2_kwh),
        sub3 = mean(sub3_kwh),
        pct_fin = mean(tipo_dia == "Fin de semana") * 100,
        .groups = "drop"
      )
  })

  output$kpi_cluster_1 <- renderValueBox({
    p <- perfiles_oficiales()
    x <- p[p$cluster_kmeans == "1", ]
    valueBox(
      paste0(formato_num(x$energia, 2), " kWh"),
      paste0("Cluster 1 | ", x$n, " días"),
      icon = icon("arrow-up"),
      color = "red"
    )
  })

  output$kpi_cluster_2 <- renderValueBox({
    p <- perfiles_oficiales()
    x <- p[p$cluster_kmeans == "2", ]
    valueBox(
      paste0(formato_num(x$energia, 2), " kWh"),
      paste0("Cluster 2 | ", x$n, " días"),
      icon = icon("arrow-down"),
      color = "green"
    )
  })

  output$kpi_cluster_sil <- renderValueBox({
    valueBox(
      formato_num(max(tabla_cluster_eval$Silhouette_KMeans), 4),
      "Silhouette K-Means",
      icon = icon("object-group"),
      color = "yellow"
    )
  })

  output$kpi_cluster_fin <- renderValueBox({
    p <- perfiles_oficiales()
    x <- p[p$cluster_kmeans == "1", ]
    valueBox(
      formato_pct(x$pct_fin, 1),
      "Fin de semana en Cluster 1",
      icon = icon("calendar-o"),
      color = "purple"
    )
  })

  output$graf_cluster_silhouette <- renderPlotly({
    df <- tabla_cluster_eval[
      ,
      c("k", "Silhouette_Jerarquico", "Silhouette_KMeans"),
      drop = FALSE
    ]

    df <- pivot_longer(
      df,
      cols = c(Silhouette_Jerarquico, Silhouette_KMeans),
      names_to = "Metodo",
      values_to = "Silhouette"
    )

    df$Metodo <- ifelse(
      df$Metodo == "Silhouette_Jerarquico",
      "Jerárquico Ward.D2",
      "K-Means"
    )

    g <- ggplot(
      df,
      aes(
        x = k,
        y = Silhouette,
        color = Metodo,
        group = Metodo,
        text = paste0(
          "Método: ", Metodo,
          "<br>k = ", k,
          "<br>Silhouette: ", round(Silhouette, 4)
        )
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      scale_x_continuous(breaks = 2:8) +
      labs(
        x = "Número de clusters",
        y = "Silhouette promedio",
        color = "Método"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  cluster_interactivo <- reactive({
    req(input$k_interactivo)
    set.seed(2026)
    modelo <- kmeans(
      X_scaled,
      centers = input$k_interactivo,
      nstart = 50,
      iter.max = 100
    )

    list(
      modelo = modelo,
      cluster = factor(modelo$cluster)
    )
  })

  output$graf_cluster_interactivo <- renderPlotly({
    ci <- cluster_interactivo()

    df <- data.frame(
      fecha = datos_dia$fecha,
      PC1 = pca$x[, 1],
      PC2 = pca$x[, 2],
      cluster = ci$cluster
    )

    g <- ggplot(
      df,
      aes(
        x = PC1,
        y = PC2,
        color = cluster,
        text = paste0(
          "Fecha: ", format(fecha, "%d/%m/%Y"),
          "<br>Cluster: ", cluster,
          "<br>PC1: ", round(PC1, 3),
          "<br>PC2: ", round(PC2, 3)
        )
      )
    ) +
      geom_point(alpha = 0.72, size = 2) +
      labs(
        x = "PC1",
        y = "PC2",
        color = "Cluster"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_cluster_perfil <- renderPlotly({
    ci <- cluster_interactivo()

    df <- as.data.frame(X_scaled)
    df$cluster <- ci$cluster

    perfil <- df %>%
      group_by(cluster) %>%
      summarise(
        across(all_of(vars_multivariadas), mean),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = all_of(vars_multivariadas),
        names_to = "Variable",
        values_to = "Media_Z"
      ) %>%
      mutate(
        Variable = etiquetas_vars[Variable]
      )

    g <- ggplot(
      perfil,
      aes(
        x = Variable,
        y = Media_Z,
        fill = cluster,
        text = paste0(
          "Cluster: ", cluster,
          "<br>Variable: ", Variable,
          "<br>Media estandarizada: ", round(Media_Z, 2)
        )
      )
    ) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "",
        y = "Media estandarizada",
        fill = "Cluster"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))

    ggplotly(g, tooltip = "text")
  })

  output$graf_cluster_horario <- renderPlotly({
    req(archivo_horario_existe)

    resumen <- datos_hora %>%
      group_by(cluster_kmeans, hora) %>%
      summarise(
        potencia = weighted.mean(
          potencia_activa_media,
          w = n_minutos,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    g <- ggplot(
      resumen,
      aes(
        x = hora,
        y = potencia,
        color = cluster_kmeans,
        group = cluster_kmeans,
        text = paste0(
          "Cluster: ", cluster_kmeans,
          "<br>Hora: ", hora, ":00",
          "<br>Potencia media: ", round(potencia, 3), " kW"
        )
      )
    ) +
      geom_line(linewidth = 1.15) +
      scale_x_continuous(breaks = 0:23) +
      labs(
        x = "Hora del día",
        y = "Potencia activa media (kW)",
        color = "Cluster"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  # ----------------------------------------------------------
  # Logística
  # ----------------------------------------------------------
  output$kpi_logit_umbral <- renderValueBox({
    valueBox(
      paste0(formato_num(umbral_consumo, 2), " kWh"),
      "Umbral de consumo alto",
      icon = icon("flag"),
      color = "blue"
    )
  })

  output$kpi_logit_accuracy <- renderValueBox({
    valueBox(
      formato_pct(metricas_oficiales$Accuracy * 100, 1),
      "Accuracy oficial",
      icon = icon("check"),
      color = "green"
    )
  })

  output$kpi_logit_f1 <- renderValueBox({
    valueBox(
      formato_num(metricas_oficiales$F1, 3),
      "F1-score",
      icon = icon("balance-scale"),
      color = "aqua"
    )
  })

  output$kpi_logit_auc <- renderValueBox({
    valueBox(
      formato_num(auc_oficial, 3),
      "AUC",
      icon = icon("bullseye"),
      color = "purple"
    )
  })

  observeEvent(input$usar_corte_050, {
    updateSliderInput(
      session,
      "corte_logit",
      value = 0.50
    )
  })

  observeEvent(input$usar_corte_061, {
    updateSliderInput(
      session,
      "corte_logit",
      value = 0.61
    )
  })

  observeEvent(input$usar_corte_074, {
    updateSliderInput(
      session,
      "corte_logit",
      value = 0.74
    )
  })

  metricas_corte <- reactive({
    calcular_metricas_clasificacion(
      real_test,
      prob_test,
      corte = input$corte_logit
    )
  })

  output$kpi_logit_precision_dyn <- renderValueBox({
    m <- metricas_corte()
    valueBox(
      formato_pct(m$Precision * 100, 1),
      "Precision",
      icon = icon("crosshairs"),
      color = "blue"
    )
  })

  output$kpi_logit_recall_dyn <- renderValueBox({
    m <- metricas_corte()
    valueBox(
      formato_pct(m$Recall * 100, 1),
      "Recall / Sensibilidad",
      icon = icon("search"),
      color = "green"
    )
  })

  output$kpi_logit_specificity_dyn <- renderValueBox({
    m <- metricas_corte()
    valueBox(
      formato_pct(m$Specificity * 100, 1),
      "Especificidad",
      icon = icon("shield"),
      color = "aqua"
    )
  })

  output$kpi_logit_balanced_dyn <- renderValueBox({
    m <- metricas_corte()
    valueBox(
      formato_pct(m$Balanced_Accuracy * 100, 1),
      "Balanced Accuracy",
      icon = icon("balance-scale"),
      color = "purple"
    )
  })

  output$graf_logit_sensibilidad <- renderPlotly({
    df <- tabla_sensibilidad_cortes[
      ,
      c(
        "Corte",
        "Precision",
        "Recall",
        "Specificity",
        "Balanced_Accuracy"
      ),
      drop = FALSE
    ]

    largo <- data.frame(
      Corte = rep(
        df$Corte,
        times = 4
      ),
      Metrica = rep(
        c(
          "Precision",
          "Recall",
          "Specificity",
          "Balanced Accuracy"
        ),
        each = nrow(df)
      ),
      Valor = c(
        df$Precision,
        df$Recall,
        df$Specificity,
        df$Balanced_Accuracy
      )
    )

    g <- ggplot(
      largo,
      aes(
        x = Corte,
        y = Valor,
        color = Metrica,
        group = Metrica,
        text = paste0(
          "Corte: ", sprintf("%.2f", Corte),
          "<br>Métrica: ", Metrica,
          "<br>Valor: ", round(Valor * 100, 1), "%"
        )
      )
    ) +
      geom_line(linewidth = 1) +
      geom_vline(
        xintercept = c(0.50, 0.61, 0.74),
        linetype = c("solid", "dashed", "dotted"),
        linewidth = 0.8,
        color = c("#17669a", "#28a745", "#d9534f")
      ) +
      geom_vline(
        xintercept = input$corte_logit,
        linetype = "longdash",
        linewidth = 1.1,
        color = "#f39c12"
      ) +
      scale_y_continuous(
        labels = percent_format(
          accuracy = 1
        ),
        limits = c(0, 1)
      ) +
      labs(
        x = "Punto de corte de probabilidad",
        y = "Desempeño",
        color = "Métrica"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$tabla_cortes_logit <- renderDT({
    df <- comparacion_cortes[
      ,
      c(
        "Escenario",
        "Corte",
        "Precision",
        "Recall",
        "Specificity",
        "Balanced_Accuracy"
      ),
      drop = FALSE
    ]

    df$Corte <- sprintf(
      "%.2f",
      df$Corte
    )

    df$Precision <- paste0(
      round(
        df$Precision * 100,
        1
      ),
      "%"
    )

    df$Recall <- paste0(
      round(
        df$Recall * 100,
        1
      ),
      "%"
    )

    df$Specificity <- paste0(
      round(
        df$Specificity * 100,
        1
      ),
      "%"
    )

    df$Balanced_Accuracy <- paste0(
      round(
        df$Balanced_Accuracy * 100,
        1
      ),
      "%"
    )

    names(df) <- c(
      "Escenario",
      "Corte",
      "Precision",
      "Recall",
      "Especificidad",
      "Balanced Accuracy"
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    )
  })

  output$graf_logit_or <- renderPlotly({
    df <- tabla_or

    etiquetas_or <- c(
      reactiva_media_z = "Potencia reactiva",
      voltaje_medio_z = "Voltaje",
      sub1_kwh_z = "Submedidor 1",
      sub2_kwh_z = "Submedidor 2",
      sub3_kwh_z = "Submedidor 3"
    )

    df$Variable <- unname(
      etiquetas_or[df$Variable]
    )

    g <- ggplot(
      df,
      aes(
        x = reorder(Variable, OR),
        y = OR,
        text = paste0(
          "Variable: ", Variable,
          "<br>OR: ", round(OR, 3),
          "<br>IC95%: [", round(IC95_Inferior, 3), ", ", round(IC95_Superior, 3), "]"
        )
      )
    ) +
      geom_col(fill = "#2389c9", width = 0.65) +
      geom_errorbar(
        aes(
          ymin = IC95_Inferior,
          ymax = IC95_Superior
        ),
        width = 0.18
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "#d9534f") +
      coord_flip() +
      labs(
        x = "",
        y = "Odds Ratio"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_logit_confusion <- renderPlotly({
    m <- metricas_corte()

    df <- data.frame(
      Prediccion = factor(
        c("No alto", "Alto", "No alto", "Alto"),
        levels = c("Alto", "No alto")
      ),
      Real = factor(
        c("No alto", "No alto", "Alto", "Alto"),
        levels = c("No alto", "Alto")
      ),
      Conteo = c(m$TN, m$FP, m$FN, m$TP)
    )

    g <- ggplot(
      df,
      aes(
        x = Real,
        y = Prediccion,
        fill = Conteo,
        text = paste0(
          "Real: ", Real,
          "<br>Predicción: ", Prediccion,
          "<br>Días: ", Conteo
        )
      )
    ) +
      geom_tile() +
      geom_text(aes(label = Conteo), size = 7, color = "white") +
      scale_fill_gradient(low = "#8ec7e8", high = "#12324a") +
      labs(
        x = "Clase real",
        y = "Predicción",
        fill = "Días"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$graf_logit_prob <- renderPlotly({
    df <- data.frame(
      Probabilidad = prob_test,
      Real = factor(
        real_test,
        levels = c(0, 1),
        labels = c("No alto", "Alto")
      )
    )

    g <- ggplot(
      df,
      aes(
        x = Probabilidad,
        fill = Real,
        text = paste0(
          "Clase real: ", Real,
          "<br>Probabilidad: ", round(Probabilidad, 3)
        )
      )
    ) +
      geom_histogram(
        position = "identity",
        alpha = 0.55,
        bins = 25
      ) +
      geom_vline(
        xintercept = input$corte_logit,
        linetype = "dashed",
        linewidth = 1
      ) +
      labs(
        x = "Probabilidad estimada de consumo alto",
        y = "Frecuencia",
        fill = "Clase real"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$texto_logit_decision <- renderUI({
    m <- metricas_corte()

    if (m$Recall > m$Specificity + 0.05) {
      lectura <- "El corte actual favorece detectar más días de consumo alto, a costa de generar más falsos positivos."
    } else if (m$Specificity > m$Recall + 0.05) {
      lectura <- "El corte actual favorece descartar correctamente días de menor consumo, a costa de perder algunos días realmente altos."
    } else {
      lectura <- "El corte actual mantiene un equilibrio relativamente parecido entre sensibilidad y especificidad."
    }

    nota_corte <- if (
      abs(input$corte_logit - 0.50) < 0.005
    ) {
      "Este es el punto de corte de referencia del modelo."
    } else if (
      abs(input$corte_logit - 0.61) < 0.005
    ) {
      "Este valor presentó la mayor Balanced Accuracy observada en el análisis exploratorio del conjunto de prueba; requiere validación independiente."
    } else if (
      abs(input$corte_logit - 0.74) < 0.005
    ) {
      "Este escenario es más restrictivo: reduce falsas alertas, pero deja más consumos altos sin detectar."
    } else {
      "Este punto permite explorar el intercambio entre detección y falsas alertas."
    }

    HTML(
      paste0(
        "<div class='insight-box'>",
        "<b>Con corte = ", round(input$corte_logit, 2), ":</b><br><br>",
        lectura,
        "<br><br>",
        "<b>", nota_corte, "</b><br><br>",
        "El AUC permanece en <b>", formato_num(auc_oficial, 3), "</b>, porque mide la capacidad de discriminación del modelo a través de todos los posibles cortes.<br><br>",
        "Entre los predictores retenidos, <b>Submedidor 3</b> presenta la mayor asociación positiva con consumo alto.",
        "</div>"
      )
    )
  })

  # ----------------------------------------------------------
  # MANOVA
  # ----------------------------------------------------------
  output$kpi_manova_pillai <- renderValueBox({
    pillai <- pillai_full["tipo_dia", "Pillai"]
    valueBox(
      formato_num(pillai, 4),
      "Pillai",
      icon = icon("calculator"),
      color = "blue"
    )
  })

  output$kpi_manova_f <- renderValueBox({
    f_aprox <- pillai_full["tipo_dia", "approx F"]
    valueBox(
      formato_num(f_aprox, 3),
      "F aproximada",
      icon = icon("bar-chart"),
      color = "green"
    )
  })

  output$kpi_boxm <- renderValueBox({
    valueBox(
      "p < 0.001",
      "Box M: heterogéneo",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$kpi_manova_sens <- renderValueBox({
    p_sens <- pillai_sin_out["tipo_dia", "Pillai"]
    valueBox(
      formato_num(p_sens, 4),
      "Pillai sin outliers",
      icon = icon("check-circle"),
      color = "purple"
    )
  })

  output$graf_manova_box <- renderPlotly({
    var <- input$variable_manova
    etiqueta <- etiquetas_vars[[var]]

    g <- ggplot(
      datos_dia,
      aes(
        x = tipo_dia,
        y = .data[[var]],
        fill = tipo_dia,
        text = paste0(
          "Tipo: ", tipo_dia,
          "<br>", etiqueta, ": ", round(.data[[var]], 3)
        )
      )
    ) +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
      labs(
        x = "Tipo de día",
        y = etiqueta,
        fill = "Tipo de día"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(g, tooltip = "text")
  })

  output$graf_manova_diferencias <- renderPlotly({
    medias <- datos_dia %>%
      group_by(tipo_dia) %>%
      summarise(
        across(all_of(vars_multivariadas), mean),
        .groups = "drop"
      )

    desv <- sapply(datos_dia[, vars_multivariadas, drop = FALSE], sd)

    laboral <- as.numeric(
      unlist(medias[medias$tipo_dia == "Laboral", vars_multivariadas])
    )
    fin <- as.numeric(
      unlist(medias[medias$tipo_dia == "Fin de semana", vars_multivariadas])
    )

    df <- data.frame(
      Variable = unname(etiquetas_vars[vars_multivariadas]),
      Diferencia_Z = (fin - laboral) / desv
    )

    g <- ggplot(
      df,
      aes(
        x = reorder(Variable, Diferencia_Z),
        y = Diferencia_Z,
        text = paste0(
          "Variable: ", Variable,
          "<br>Diferencia estandarizada: ", round(Diferencia_Z, 3)
        )
      )
    ) +
      geom_col(fill = "#2389c9") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(
        x = "",
        y = "Diferencia en desviaciones estándar"
      ) +
      theme_minimal()

    ggplotly(g, tooltip = "text")
  })

  output$tabla_manova_medias <- renderDT({
    df <- datos_dia %>%
      group_by(tipo_dia) %>%
      summarise(
        across(all_of(vars_multivariadas), mean),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    names(df) <- c(
      "Tipo de día",
      "Energía global",
      "Reactiva",
      "Voltaje",
      "Intensidad",
      "Sub 1",
      "Sub 2",
      "Sub 3"
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })

  # ----------------------------------------------------------
  # Conclusiones
  # ----------------------------------------------------------
  output$tabla_decisiones <- renderDT({
    df <- data.frame(
      Pregunta = c(
        "1. Patrones a lo largo del tiempo",
        "2. Dimensiones principales",
        "3. Perfiles de consumo",
        "4. Laboral vs. fin de semana",
        "5. Variables asociadas al consumo global"
      ),
      Evidencia = c(
        "Perfil horario + ACF + Ljung-Box",
        "PCA: 4 componentes = 89.8%",
        "K-Means: k=2; Silhouette=0.2629",
        "Pillai=0.1549; p<0.001",
        "r energía-intensidad=0.9994; r Sub3=0.7631"
      ),
      Conclusion = c(
        "El consumo cambia por hora, período y tipo de día y conserva dependencia temporal.",
        "Cuatro dimensiones resumen casi 90% de la variabilidad original.",
        "Hay dos perfiles generales de mayor y menor consumo, pero con solapamiento.",
        "Existe diferencia multivariada; fines de semana muestran mayor consumo en varias dimensiones.",
        "Intensidad domina la relación con consumo total; Submedidor 3 es el subconsumo más asociado."
      ),
      Implicacion = c(
        "Gestionar por franjas y períodos, no solo con un promedio global.",
        "Es posible simplificar monitoreo sin conservar las siete variables como dimensiones independientes.",
        "Usar los perfiles para segmentar días de operación, no como categorías rígidas.",
        "Diseñar seguimiento diferenciado para fines de semana y horas de mayor demanda.",
        "Priorizar investigación del Submedidor 3 y evitar duplicar indicadores energía-intensidad."
      ),
      stringsAsFactors = FALSE
    )

    datatable(
      df,
      rownames = FALSE,
      escape = TRUE,
      class = "cell-border stripe decision-table",
      options = list(
        pageLength = 5,
        dom = "t",
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  # ----------------------------------------------------------
  # Metodología
  # ----------------------------------------------------------
  output$tabla_supuestos <- renderDT({
    mardia_p <- resultado_mardia$p_value

    df <- data.frame(
      Supuesto = c(
        "Normalidad multivariada",
        "Independencia temporal",
        "Homogeneidad de covarianzas",
        "Adecuación para FA",
        "Sensibilidad MANOVA a outliers"
      ),
      Resultado = c(
        "No se cumple",
        "No se cumple",
        "No se cumple",
        "Baja / inestable",
        "Conclusión se mantiene"
      ),
      Evidencia = c(
        paste0("Mardia p < 0.001; asimetría=", round(resultado_mardia$Coeficiente[1], 2)),
        "Ljung-Box lag 7 y 14: p < 0.001",
        paste0("Box M: p ", formato_pvalue(resultado_box_m$p_value)),
        paste0("KMO=", round(kmo_result$MSA, 3), "; caso ultra-Heywood"),
        paste0("Pillai sin outliers=", round(pillai_sin_out["tipo_dia", "Pillai"], 4))
      ),
      Tratamiento = c(
        "Interpretación prudente + QQ/Mahalanobis",
        "Se reconoce como limitación y se sugiere modelación temporal",
        "Pillai como estadístico principal",
        "No se nombran ni interpretan factores latentes",
        "Se reporta análisis de sensibilidad"
      ),
      stringsAsFactors = FALSE
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom = "t",
        scrollX = TRUE
      )
    ) %>%
      formatStyle(
        "Resultado",
        backgroundColor = styleEqual(
          c(
            "No se cumple",
            "Baja / inestable",
            "Conclusión se mantiene"
          ),
          c(
            "#f8d7da",
            "#fff3cd",
            "#d4edda"
          )
        )
      )
  })

  output$tabla_integrantes <- renderDT({
    df <- data.frame(
      Autor = "Raúl Antonio Ruano Katz",
      Contexto = "Versión de portafolio de un proyecto académico grupal",
      check.names = FALSE
    )

    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 1)
    )
  })
}

# ============================================================
# 7. EJECUTAR APP
# ============================================================

shinyApp(ui = ui, server = server)
