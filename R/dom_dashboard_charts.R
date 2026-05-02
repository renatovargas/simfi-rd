#' Plotting and summary helpers (extracted from app-legacy.R for the live scenario app).
#' Do not source app-legacy.R from here.

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(RColorBrewer)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

zap_labelled_vec <- function(x) {
  if (inherits(x, "labelled") || inherits(x, "haven_labelled")) {
    if (requireNamespace("haven", quietly = TRUE)) {
      return(haven::zap_labels(x))
    }
    return(unclass(x))
  }
  x
}

zap_labelled_df <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }
  for (nm in names(df)) {
    df[[nm]] <- zap_labelled_vec(df[[nm]])
  }
  df
}

as_chr_safe <- function(x) {
  x <- zap_labelled_vec(x)
  if (is.factor(x)) {
    return(as.character(x))
  }
  as.character(x)
}

as_int_safe <- function(x) {
  x <- zap_labelled_vec(x)
  suppressWarnings(as.integer(x))
}

MACRO_REGION_NAMES <- c(
  `1` = "Este",
  `2` = "Norte",
  `3` = "Ozama",
  `4` = "Sur"
)

#' Etiquetas de eje (misma convención que el panel de referencia).
labels_inc_dom <- function() {
  list(
    decinc = as.character(1:10),
    estrinc = paste("Estrato", 1:4),
    urbinc = c("Rural", "Urbano"),
    sexinc = c("Jefe (categoría 1)", "Jefe (categoría 2)"),
    catinc = paste("Tipo hogar", 1:4),
    depinc = sprintf(
      "%s (macroregión %d)",
      unname(MACRO_REGION_NAMES[as.character(seq_len(4))]),
      seq_len(4)
    )
  )
}

comb01_plotly_native <- function(
  data,
  dec,
  tit1,
  tit2_plot_lab,
  indice,
  default_bar_color = "#636EFA",
  pre_reforma_bar_color = "#FECB52",
  xaxis_label_wrap_width = 18,
  value_label_size_px = 11,
  base_font_size = 11,
  column_tooltips = NULL
) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Datos no válidos.", x = 0.5))
    )
  }
  if (is.null(dim(data)) || nrow(data) == 0 || ncol(data) == 0) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Sin datos.", x = 0.5))
    )
  }
  if (indice < 1 || indice > nrow(data)) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Índice de fila fuera de rango.", x = 0.5))
    )
  }
  if (is.null(colnames(data))) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Faltan nombres de columnas.", x = 0.5))
    )
  }

  plot_data_df <- data.frame(
    Categoria = colnames(data),
    Valor = as.numeric(data[indice, ]),
    stringsAsFactors = FALSE
  )
  plot_data_df$Categoria <- factor(
    plot_data_df$Categoria,
    levels = colnames(data)
  )

  tips_raw <- if (!is.null(column_tooltips) && length(column_tooltips)) {
    unname(column_tooltips[as.character(plot_data_df$Categoria)])
  } else {
    rep(NA_character_, nrow(plot_data_df))
  }
  tips_raw[is.na(tips_raw)] <- ""
  comma_accuracy <- if (dec == 0) 1 else 1 / (10^dec)
  plot_data_df$hover_custom <- paste0(
    "<b>",
    as.character(plot_data_df$Categoria),
    "</b>",
    ifelse(nzchar(tips_raw), paste0("<br>", tips_raw), ""),
    "<br>Valor: ",
    scales::comma(plot_data_df$Valor, accuracy = comma_accuracy)
  )

  first_col <- colnames(data)[1]
  is_base_col <- first_col %in% c("Pre-reforma", "Línea base")
  bar_colors <- ifelse(
    as.character(plot_data_df$Categoria) == first_col & is_base_col,
    pre_reforma_bar_color,
    default_bar_color
  )

  max_data_val <- max(plot_data_df$Valor, na.rm = TRUE)
  min_data_val <- min(plot_data_df$Valor, na.rm = TRUE)
  pad <- 0.12
  maxypos <- if (is.finite(max_data_val)) max_data_val * (1 + pad) else 1
  minypos <- if (is.finite(min_data_val) && min_data_val < 0) {
    min_data_val * (1 + pad)
  } else {
    0
  }
  if (!is.finite(maxypos)) {
    maxypos <- 1
  }
  if (maxypos <= minypos) {
    maxypos <- minypos + 1
  }

  x_axis_labels <- levels(plot_data_df$Categoria)
  wrapped_x_labels <- if (xaxis_label_wrap_width > 0) {
    sapply(
      x_axis_labels,
      function(lab) {
        gsub(
          paste0("(.{", xaxis_label_wrap_width, "})(?!$)"),
          "\\1<br>",
          lab,
          perl = TRUE
        )
      },
      USE.NAMES = FALSE
    )
  } else {
    x_axis_labels
  }

  plot_ly(
    data = plot_data_df,
    x = ~Categoria,
    y = ~Valor,
    customdata = ~hover_custom,
    type = "bar",
    marker = list(color = bar_colors),
    text = ~ scales::comma(Valor, accuracy = comma_accuracy),
    textposition = "outside",
    textfont = list(size = value_label_size_px, color = "black"),
    hovertemplate = "%{customdata}<extra></extra>"
  ) %>%
    layout(
      title = list(
        text = tit1,
        x = 0.5,
        font = list(size = base_font_size + 2)
      ),
      xaxis = list(
        title = "",
        ticktext = wrapped_x_labels,
        tickvals = x_axis_labels,
        tickfont = list(size = base_font_size),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = tit2_plot_lab,
          font = list(size = base_font_size + 1)
        ),
        tickformat = paste0(",.", dec, "f"),
        range = c(minypos, maxypos),
        automargin = TRUE
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 20, b = 90, t = 50),
      font = list(family = "system-ui, sans-serif", size = base_font_size)
    )
}

graficar_incidencia_plotly <- function(
  matriz_data,
  category_labels,
  scenario_names,
  plot_main_title = "",
  y_axis_label_str = "Porcentaje del ingreso disponible",
  color_palette = NULL,
  font_size_base = 12,
  hover_decimal_places = 2,
  scenario_tooltips = NULL,
  y_as_percent = TRUE
) {
  if (
    is.null(matriz_data) || NCOL(matriz_data) == 0 || NROW(matriz_data) == 0
  ) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Sin datos de incidencia.", x = 0.5))
    )
  }

  df <- as.data.frame(matriz_data)
  if (length(scenario_names) == ncol(df)) {
    colnames(df) <- scenario_names
  } else {
    colnames(df) <- paste0("Esc.", seq_len(ncol(df)))
  }

  n <- nrow(df)
  if (length(category_labels) != n) {
    category_labels <- paste("Categoría", seq_len(n))
  }

  df <- df %>%
    mutate(cat_axis_labels = factor(category_labels, levels = category_labels))

  final_scenario_names <- colnames(df)[colnames(df) != "cat_axis_labels"]

  df_long <- df %>%
    pivot_longer(
      cols = all_of(final_scenario_names),
      names_to = "Escenario",
      values_to = "Valor"
    ) %>%
    mutate(Escenario = factor(Escenario, levels = final_scenario_names))

  tip_vec <- if (!is.null(scenario_tooltips) && length(scenario_tooltips)) {
    tv <- unname(scenario_tooltips[as.character(df_long$Escenario)])
    tv[is.na(tv)] <- ""
    tv
  } else {
    rep("", nrow(df_long))
  }
  val_fmt <- if (y_as_percent) {
    sprintf("%.*f%%", hover_decimal_places, df_long$Valor)
  } else {
    sprintf("%.*f", hover_decimal_places, df_long$Valor)
  }
  df_long$hover_custom <- paste0(
    "<b>",
    df_long$Escenario,
    "</b>",
    ifelse(nzchar(tip_vec), paste0("<br>", tip_vec), ""),
    "<br>Categoría: ",
    df_long$cat_axis_labels,
    "<br>Valor: ",
    val_fmt
  )

  if (is.null(color_palette)) {
    color_palette <- brewer.pal(max(3, length(final_scenario_names)), "Set1")
    color_palette <- color_palette[seq_along(final_scenario_names)]
  }

  y_min <- min(0, min(df_long$Valor, na.rm = TRUE))
  y_max <- max(0, max(df_long$Valor, na.rm = TRUE))
  pad <- (y_max - y_min) * 0.12
  if (!is.finite(pad) || pad == 0) {
    pad <- 0.5
  }

  x_ticks <- levels(df_long$cat_axis_labels)
  wrapped <- sapply(
    x_ticks,
    function(lab) {
      gsub("(.{14})(?!$)", "\\1<br>", lab, perl = TRUE)
    },
    USE.NAMES = FALSE
  )

  plot_ly(
    data = df_long,
    x = ~cat_axis_labels,
    y = ~Valor,
    color = ~Escenario,
    colors = color_palette,
    type = "bar",
    customdata = ~hover_custom,
    hovertemplate = "%{customdata}<extra></extra>"
  ) %>%
    layout(
      title = list(
        text = plot_main_title,
        x = 0.5,
        font = list(size = font_size_base + 3)
      ),
      xaxis = list(
        title = "",
        ticktext = wrapped,
        tickvals = x_ticks,
        categoryorder = "array",
        categoryarray = x_ticks,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = y_axis_label_str),
        ticksuffix = if (y_as_percent) "%" else "",
        range = c(y_min - pad * 0.3, y_max + pad),
        zeroline = TRUE
      ),
      barmode = "dodge",
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.35,
        font = list(size = font_size_base)
      ),
      margin = list(b = 140, t = 60),
      font = list(family = "system-ui, sans-serif", size = font_size_base)
    )
}

pick_comp_pc_col <- function(nm, scen_idx) {
  hit <- grep(sprintf("^comp_%d_pc$", scen_idx), nm, value = TRUE)[1]
  if (!is.na(hit)) {
    return(hit)
  }
  grep(paste0("^comp", scen_idx, "_pc$"), nm, value = TRUE)[1]
}

scenario_index_from_key <- function(scen_key) {
  as.integer(sub("^escenario_", "", scen_key))
}

nitx_colname_for_scenario <- function(scen_idx) {
  paste0("nitx", scen_idx, "_pc")
}

dom_list_is_pipeline_layout <- function(dom_list) {
  pg <- dom_list[[1]]$pov_gral
  !is.null(colnames(pg)) && "Base" %in% colnames(pg)
}

fiscal_post_measures_mat <- function(dom_list, scen_table_hdr_named) {
  scen_nums <- vapply(names(dom_list), scenario_index_from_key, integer(1))
  vals <- mapply(
    function(res, k) {
      df <- as.data.frame(res$decsum)
      r <- df[nrow(df), , drop = TRUE]
      nm <- names(r)
      gv <- function(stem) {
        cc <- grep(sprintf("^%s%d_pc$", stem, k), nm, value = TRUE)[1]
        if (is.na(cc)) {
          return(NA_real_)
        }
        suppressWarnings(as.numeric(r[[cc]]))
      }
      cc_comp <- pick_comp_pc_col(nm, k)
      comp_v <- if (is.na(cc_comp)) {
        NA_real_
      } else {
        suppressWarnings(as.numeric(r[[cc_comp]]))
      }
      c(
        gv("dtx_isr"),
        gv("itx_itb"),
        gv("sub_ele"),
        gv("neto"),
        comp_v,
        gv("nitx")
      )
    },
    dom_list,
    scen_nums,
    SIMPLIFY = TRUE
  )
  mat <- as.matrix(vals)
  if (ncol(mat) == 1) {
    colnames(mat) <- names(dom_list)
  }
  rownames(mat) <- c(
    "ISR posreforma (% PIB)",
    "ITBIS posreforma (% PIB)",
    "Subsidio eléctrico posreforma (% PIB)",
    "Neto fiscal previo a compensación (% PIB)",
    "Compensación (% PIB)",
    "Incidencia neta fiscal (% PIB)"
  )
  colnames(mat) <- unname(scen_table_hdr_named[colnames(mat)])
  mat
}

kakwani_summary_matrices <- function(dom_list, post_headers_chr) {
  lab_cols <- c("Pre-reforma", post_headers_chr)
  n <- length(dom_list)
  rows <- c("ISR", "ITBIS", "Subsidio eléctrico", "Compensación")
  ic_mat <- matrix(NA_real_, nrow = 4L, ncol = n + 1L)
  kw_mat <- matrix(NA_real_, nrow = 4L, ncol = n + 1L)
  pre <- dom_list[[1]]$kakwani
  ic_mat[, 1L] <- suppressWarnings(as.numeric(pre[, "ic_Base"]))
  kw_mat[, 1L] <- suppressWarnings(as.numeric(pre[, "kakwani_Base"]))
  for (i in seq_len(n)) {
    k <- scenario_index_from_key(names(dom_list)[i])
    kk <- dom_list[[i]]$kakwani
    ic_col <- grep(paste0("^ic_Esc", k, "$"), colnames(kk), value = TRUE)[1]
    kw_col <- grep(paste0("^kakwani_Esc", k, "$"), colnames(kk), value = TRUE)[
      1
    ]
    if (!is.na(ic_col)) {
      ic_mat[, i + 1L] <- suppressWarnings(as.numeric(kk[, ic_col]))
    }
    if (!is.na(kw_col)) {
      kw_mat[, i + 1L] <- suppressWarnings(as.numeric(kk[, kw_col]))
    }
  }
  rownames(ic_mat) <- rownames(kw_mat) <- rows
  colnames(ic_mat) <- colnames(kw_mat) <- lab_cols
  list(kak_ic = ic_mat, kak_kw = kw_mat)
}

plotly_compensacion_bars <- function(
  dom_list,
  scenario_labels,
  scenario_tooltips = NULL
) {
  comp_vals <- mapply(
    function(res, k) {
      df <- as.data.frame(res$decsum)
      r <- df[nrow(df), , drop = TRUE]
      comp_col <- if (dom_list_is_pipeline_layout(dom_list)) {
        pick_comp_pc_col(names(r), k)
      } else {
        grep("^comp\\d+_pc$", names(r), value = TRUE)[1]
      }
      if (is.na(comp_col)) {
        return(NA_real_)
      }
      v <- suppressWarnings(as.numeric(r[[comp_col]]))
      if (length(v) != 1) NA_real_ else v
    },
    dom_list,
    vapply(names(dom_list), scenario_index_from_key, integer(1)),
    SIMPLIFY = TRUE
  )
  plot_data <- data.frame(
    Escenario = unname(scenario_labels),
    Compensacion_PIB = comp_vals,
    stringsAsFactors = FALSE
  )
  tips <- if (!is.null(scenario_tooltips) && length(scenario_tooltips)) {
    tv <- unname(scenario_tooltips[as.character(plot_data$Escenario)])
    tv[is.na(tv)] <- ""
    tv
  } else {
    rep("", nrow(plot_data))
  }
  plot_data$hover_custom <- paste0(
    "<b>",
    plot_data$Escenario,
    "</b>",
    ifelse(nzchar(tips), paste0("<br>", tips), ""),
    "<br>",
    sprintf("%.3f%% PIB", plot_data$Compensacion_PIB)
  )
  plot_ly(
    plot_data,
    x = ~Escenario,
    y = ~Compensacion_PIB,
    customdata = ~hover_custom,
    type = "bar",
    marker = list(color = "#00CC96"),
    hovertemplate = "%{customdata}<extra></extra>"
  ) %>%
    layout(
      title = list(text = "Compensación total (fila Total, % PIB)", x = 0.5),
      yaxis = list(title = "% PIB", rangemode = "tozero"),
      xaxis = list(title = "", tickangle = 25),
      font = list(family = "system-ui, sans-serif", size = 12)
    )
}

plotly_npov_horizontal <- function(npov_mat, column_tooltips = NULL) {
  if (is.null(npov_mat) || !nrow(npov_mat) || !ncol(npov_mat)) {
    return(
      plot_ly() %>%
        layout(title = list(text = "Sin datos de nuevos pobres.", x = 0.5))
    )
  }

  scen_order <- colnames(npov_mat)
  df_wide <- as.data.frame(npov_mat, optional = TRUE)
  df_wide$Indicador <- rownames(npov_mat)

  ind_order <- rownames(npov_mat)

  plot_df <- df_wide %>%
    pivot_longer(-Indicador, names_to = "Escenario", values_to = "Valor") %>%
    mutate(
      Escenario = factor(Escenario, levels = scen_order),
      Indicador = factor(Indicador, levels = ind_order),
      Valor = as.numeric(Valor)
    )

  scen_tips <- if (!is.null(column_tooltips) && length(column_tooltips)) {
    tv <- unname(column_tooltips[as.character(plot_df$Escenario)])
    tv[is.na(tv)] <- ""
    tv
  } else {
    rep("", nrow(plot_df))
  }
  plot_df$hover_custom <- paste0(
    "<b>",
    plot_df$Escenario,
    "</b>",
    ifelse(nzchar(scen_tips), paste0("<br>", scen_tips), ""),
    "<br>",
    plot_df$Indicador,
    "<br>",
    sprintf("%s personas", scales::comma(plot_df$Valor, accuracy = 1))
  )

  n_ind <- length(ind_order)
  cols <- RColorBrewer::brewer.pal(max(3, n_ind), "Set2")[seq_len(n_ind)]

  plot_ly(
    data = plot_df,
    x = ~Valor,
    y = ~Escenario,
    color = ~Indicador,
    colors = cols,
    type = "bar",
    orientation = "h",
    customdata = ~hover_custom,
    hovertemplate = "%{customdata}<extra></extra>"
  ) %>%
    layout(
      title = list(
        text = "Nuevos pobres: variación respecto a pre-reforma por escenario",
        x = 0.5,
        font = list(size = 14)
      ),
      barmode = "group",
      xaxis = list(
        title = "Personas",
        tickformat = ",.0f",
        gridcolor = "#e9ecef",
        zeroline = TRUE,
        zerolinewidth = 1.5,
        zerolinecolor = "#495057"
      ),
      yaxis = list(
        title = "",
        autorange = "reversed"
      ),
      legend = list(
        title = list(text = "Indicador"),
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.22,
        font = list(size = 11)
      ),
      margin = list(l = 10, r = 20, t = 55, b = 90, pad = 4),
      font = list(family = "system-ui, sans-serif", size = 12),
      bargap = 0.18,
      bargroupgap = 0.08
    )
}

# --- DOM → matrices resumen (lógica alineada al .qmd de Guatemala) ------------
# RDS recientes (script05): matrices con columnas Base, yd_k, … y filas con
# nombres (rate, headcount, gap_lcuhh). RDS antiguos: índices numéricos 8×4.

procesar_fila_dom <- function(
  dom_list,
  idx_pre_col,
  idx_post_col,
  post_display = NULL
) {
  if (dom_list_is_pipeline_layout(dom_list)) {
    scen_keys <- names(dom_list)
    n_scen <- length(scen_keys)
    scen_nums <- vapply(scen_keys, scenario_index_from_key, integer(1))

    pov_block <- function(res, col_nm) {
      c(
        res$pov_gral["rate", col_nm],
        res$pov_ext["rate", col_nm],
        res$pov_int["rate", col_nm]
      )
    }
    npov_block <- function(res, col_nm) {
      c(
        res$pov_gral["headcount", col_nm],
        res$pov_ext["headcount", col_nm],
        res$pov_int["headcount", col_nm]
      )
    }
    povb_block <- function(res, col_nm) {
      c(
        res$pov_gral["gap_lcuhh", col_nm],
        res$pov_ext["gap_lcuhh", col_nm],
        res$pov_int["gap_lcuhh", col_nm]
      )
    }

    pre_col <- "Base"
    pre_povr <- pov_block(dom_list[[1]], pre_col)
    pre_npov <- npov_block(dom_list[[1]], pre_col)
    pre_povb <- povb_block(dom_list[[1]], pre_col)

    post_list <- lapply(seq_len(n_scen), function(i) {
      cc <- paste0("yd_", scen_nums[i])
      list(
        povr = pov_block(dom_list[[i]], cc),
        npov = npov_block(dom_list[[i]], cc),
        povb = povb_block(dom_list[[i]], cc)
      )
    })

    povr <- do.call(cbind, c(list(pre_povr), lapply(post_list, `[[`, "povr")))
    npov <- do.call(cbind, c(list(pre_npov), lapply(post_list, `[[`, "npov")))
    povb <- do.call(cbind, c(list(pre_povb), lapply(post_list, `[[`, "povb")))

    ineq_rows <- c("Gini", "dGini", "Palma", "Theil")
    pre_ineq <- dom_list[[1]]$ineq[ineq_rows, pre_col, drop = FALSE]
    post_ineq <- lapply(seq_len(n_scen), function(i) {
      cc <- paste0("yd_", scen_nums[i])
      dom_list[[i]]$ineq[ineq_rows, cc, drop = FALSE]
    })
    desr <- do.call(cbind, c(list(pre_ineq), post_ineq))
    desr <- as.matrix(desr)

    lab_cols <- if (!is.null(post_display)) {
      c("Pre-reforma", post_display)
    } else {
      c("Pre-reforma", sprintf("Escenario %d", seq_len(n_scen)))
    }
    colnames(povr) <- colnames(npov) <- colnames(povb) <- colnames(
      desr
    ) <- lab_cols
    povr <- as.matrix(povr)
    npov <- as.matrix(npov)
    povb <- as.matrix(povb)

    rownames(povr) <- c(
      "Pobreza general (%)",
      "Pobreza extrema (%)",
      "Pobreza línea int. LPIM (%)"
    )
    rownames(npov) <- c(
      "Nuevos pobres (general)",
      "Nuevos pobres (extrema)",
      "Nuevos pobres (línea int.)"
    )
    rownames(povb) <- c(
      "Brecha de pobreza promedio entre hogares pobres (pesos mensuales): general",
      "Brecha de pobreza promedio entre hogares pobres (pesos mensuales): extrema",
      "Brecha de pobreza promedio entre hogares pobres (pesos mensuales): LPIM"
    )
    rownames(desr) <- c(
      "Coeficiente de Gini",
      "Cambio en Gini (respecto a línea base, mismo concepto de ingreso)",
      "Índice de Palma",
      "Índice de Theil"
    )

    return(list(povr = povr, npov = npov, povb = povb, desr = desr))
  }

  scen_names <- names(dom_list)
  n_scen <- length(scen_names)

  extract_block <- function(
    res,
    col_idx,
    pov_row_p,
    pov_row_n,
    pov_row_b,
    ineq_row
  ) {
    list(
      povr = c(
        res$pov_gral[pov_row_p, col_idx],
        res$pov_ext[pov_row_p, col_idx],
        res$pov_int[pov_row_p, col_idx]
      ),
      npov = c(
        res$pov_gral[pov_row_n, col_idx],
        res$pov_ext[pov_row_n, col_idx],
        res$pov_int[pov_row_n, col_idx]
      ),
      povb = c(
        res$pov_gral[pov_row_b, col_idx],
        res$pov_ext[pov_row_b, col_idx],
        res$pov_int[pov_row_b, col_idx]
      ),
      desr = res$ineq[ineq_row, col_idx]
    )
  }

  pre <- extract_block(dom_list[[1]], idx_pre_col, 1L, 3L, 6L, 1L)

  post_list <- lapply(seq_len(n_scen), function(i) {
    extract_block(dom_list[[i]], idx_post_col, 1L, 3L, 6L, 1L)
  })

  povr <- do.call(cbind, c(list(pre$povr), lapply(post_list, `[[`, "povr")))
  npov <- do.call(cbind, c(list(pre$npov), lapply(post_list, `[[`, "npov")))
  povb <- do.call(cbind, c(list(pre$povb), lapply(post_list, `[[`, "povb")))
  desr <- do.call(cbind, c(list(pre$desr), lapply(post_list, `[[`, "desr")))

  post_labs <- post_display %||% sprintf("Escenario %d", seq_len(n_scen))
  lab_cols <- c("Pre-reforma", post_labs)
  colnames(povr) <- colnames(npov) <- colnames(povb) <- lab_cols
  colnames(desr) <- lab_cols
  povr <- as.matrix(povr)
  npov <- as.matrix(npov)
  povb <- as.matrix(povb)
  desr <- matrix(as.numeric(desr), nrow = 1)
  colnames(desr) <- lab_cols

  rownames(povr) <- c(
    "Pobreza general (%)",
    "Pobreza extrema (%)",
    "Pobreza línea int. LPIM (%)"
  )
  rownames(npov) <- c(
    "Nuevos pobres (general)",
    "Nuevos pobres (extrema)",
    "Nuevos pobres (línea int.)"
  )
  rownames(povb) <- c(
    "Brecha: pobreza general (DOP/mes/hogar pobre)",
    "Brecha: pobreza extrema (DOP/mes/hogar pobre)",
    "Brecha: línea int. (DOP/mes/hogar pobre)"
  )
  rownames(desr) <- "Coeficiente de Gini"

  list(povr = povr, npov = npov, povb = povb, desr = desr)
}
incidencia_columna <- function(
  dom_list,
  tabla,
  col_idx = 10L,
  mult = -1,
  scenario_colnames = NULL
) {
  if (dom_list_is_pipeline_layout(dom_list)) {
    scen_keys <- names(dom_list)
    scen_nums <- vapply(scen_keys, scenario_index_from_key, integer(1))
    mats <- mapply(
      function(res, k) {
        mat <- as.data.frame(res[[tabla]])
        j <- match(nitx_colname_for_scenario(k), names(mat))
        if (is.na(j)) {
          return(NULL)
        }
        nr <- nrow(mat) - 1L
        if (nr < 1) {
          return(NULL)
        }
        as.matrix(mat[seq_len(nr), j, drop = FALSE]) * mult
      },
      dom_list,
      scen_nums,
      SIMPLIFY = FALSE
    )
    mats <- Filter(Negate(is.null), mats)
    if (!length(mats)) {
      return(NULL)
    }
    out <- do.call(cbind, mats)
    cn <- scenario_colnames
    if (is.null(cn) || length(cn) != ncol(out)) {
      cn <- sprintf("Escenario %d", seq_len(ncol(out)))
    }
    colnames(out) <- cn
    return(out)
  }

  mats <- lapply(dom_list, function(res) {
    mat <- as.data.frame(res[[tabla]])
    if (ncol(mat) < col_idx) {
      return(NULL)
    }
    nr <- nrow(mat) - 1L
    if (nr < 1) {
      return(NULL)
    }
    as.matrix(mat[seq_len(nr), col_idx, drop = FALSE]) * mult
  })
  mats <- Filter(Negate(is.null), mats)
  if (!length(mats)) {
    return(NULL)
  }
  out <- do.call(cbind, mats)
  cn <- scenario_colnames
  if (is.null(cn) || length(cn) != ncol(out)) {
    cn <- sprintf("Escenario %d", seq_len(ncol(out)))
  }
  colnames(out) <- cn
  out
}

region_label <- function(code) {
  x <- as_chr_safe(code)
  ifelse(
    x == "Total",
    "Total (país)",
    sprintf(
      "%s (macroregión %s)",
      MACRO_REGION_NAMES[x] %||% paste("Región", x),
      x
    )
  )
}

#' @param escenario_headers Named vector: `names(dom_list)` → etiqueta corta.
tabla_macro_region <- function(dom_list, escenario_headers = NULL) {
  bind_rows(lapply(seq_along(dom_list), function(i) {
    df <- zap_labelled_df(as.data.frame(dom_list[[i]]$depinc))
    df <- df %>%
      mutate(
        `Clave región` = as_chr_safe(.data$macro_region),
        `Nombre región` = vapply(
          as_chr_safe(.data$macro_region),
          function(k) {
            as.character(MACRO_REGION_NAMES[k] %||% "")
          },
          character(1)
        ),
        `Etiqueta región` = region_label(.data$macro_region)
      )
    sk <- names(dom_list)[i]
    df$Escenario <- as.character(escenario_headers[sk] %||% sk)
    df
  })) %>%
    select(
      Escenario,
      `Clave región`,
      `Nombre región`,
      `Etiqueta región`,
      dplyr::ends_with("_pc")
    )
}
