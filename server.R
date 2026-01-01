library(shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Chargement global ----
all_contvals <- get(load("all_contvals_2026.Rdata"))

K_progs <- list(
  "ns2:RK_PS_psmHerbicide" = "SP non-recours herbicides",
  "ns2:RK_PS_soilCoverage"= "SP couv. sol",
  "ns2:RK_PS_psmArableFarming" ="SP non-recours PPh grandes cultures",
  "ns2:RK_PS_fbdInsectStrips" = "Bandes semées org. utiles",
  "ns2:RK_PS_soilMildTreatment" = "SP tech. cult. préserv. sol",
  "ns2:RK_PS_psmVegetablesBerries" = "SP non-recours PPh petits fruits et cult. maraich.",
  "ns2:RK_PS_psmPerennialCulture" = "SP non-recours PPh cult. pérennes",
  "ns2:RK_PS_psmBioAids" = "SP intrants bio.",
  "ns1:RK_PS_BIO_OA" = "SP agriculture bio.",
  "ns1:RK_PS_BIO_SPK" ="SP agriculture bio.",
  "ns1:RK_PS_BIO_UEF" ="SP agriculture bio.",
  "ns1:RK_EK_EIWEISS" = "cultures particulières",
  "ns1:RK_EK_OEL" ="cultures particulières",
  "ns1:RK_EK_SAATGUT1"= "cultures particulières",
  "ns1:RK_EK_SAATGUT2"= "cultures particulières",
  "ns1:RK_EK_SOJA" = "cultures particulières",
  "ns1:RK_EK_ZUCKERRUEBEN" = "cultures particulières",
  "ns1:RK_IN_INSITU" = "in situ",
  
  "ns1:RK_VS_BAS_DG"   = "sécurité approvisionnement",
  "ns1:RK_VS_DK"       = "sécurité approvisionnement",
  "ns1:RK_VS_BAS_UEB"  = "sécurité approvisionnement",
  "ns1:RK_VS_OA"       = "sécurité approvisionnement",
  
  "ns1:RK_OB"          = "PC paysage ouvert",
  "ns1:KL_HAN"         = "PC surf. en pente",
  "ns1:KL_REB"         = "PC surf. viti. en pente",
  
  "BD_BCE" = "Bande culturale extensive",
  "Q1" = "Biodiv. Q1",
  "Q2" = "Biodiv. Q1",
  "R1" = "Réseau Q1",
  "R2" = "Réseau Q2"
)

shinyServer(function(input, output, session) {
  
  # --- Remplir la liste cc ---
  observe({
    cc_choices <- sort(names(all_contvals))
    default_cc <- if ("504" %in% cc_choices) "504" else cc_choices[1]
    updateSelectInput(session, "cc", choices = cc_choices, selected = default_cc)
  })
  
  # --- Labels dynamiques des pentes selon cc ---
  is_701_717 <- reactive({
    cc <- trimws(input$cc %||% "")
    cc %in% c("701", "717")
  })
  
  output$pente1_ui <- renderUI({
    lab <- if (is_701_717()) "Surface pente 35–50 (m²)" else "Surface pente 18–35 (m²)"
    numericInput("pente1", lab, value = 50, min = 0, step = 1)
  })
  output$pente2_ui <- renderUI({
    lab <- if (is_701_717()) "Surface pente 50–100 (m²)" else "Surface pente 35–50 (m²)"
    numericInput("pente2", lab, value = 50, min = 0, step = 1)
  })
  output$pente3_ui <- renderUI({
    lab <- if (is_701_717()) "Terrasses (m²)" else "Surface pente > 50 (m²)"
    numericInput("pente3", lab, value = 50, min = 0, step = 1)
  })
  
  # --- KPI ---
  output$kpi_surfexp <- renderText({
    s <- (input$pente1 %||% 0) + (input$pente2 %||% 0) + (input$pente3 %||% 0)
    format(s, big.mark = "'", scientific = FALSE)
  })
  output$kpi_cc   <- renderText({ input$cc %||% "" })
  output$kpi_zone <- renderText({ input$zone %||% "" })
  
  # Meta affichée pour le résultat courant
  output$current_meta <- renderText({
    cc <- trimws(input$cc %||% "")
    zone <- trimws(input$zone %||% "")
    p1 <- as.numeric(input$pente1 %||% 0)
    p2 <- as.numeric(input$pente2 %||% 0)
    p3 <- as.numeric(input$pente3 %||% 0)
    surfexp <- p1 + p2 + p3
    paste0("cc=", cc, " | zone=", zone, " | pentes=", p1, " / ", p2, " / ", p3, " | total=", surfexp, " m²")
  })
  
  # --- Calcul au clic ---
  calc <- eventReactive(input$go, {
    
    cc     <- trimws(input$cc %||% "")
    zone   <- trimws(input$zone %||% "")
    pente1 <- as.numeric(input$pente1 %||% 0)
    pente2 <- as.numeric(input$pente2 %||% 0)
    pente3 <- as.numeric(input$pente3 %||% 0)
    
    # validations "bloquantes" (on ne peut pas calculer sans ça)
    validate(
      need(nzchar(cc), "Veuillez sélectionner un code culture (cc)."),
      need(cc %in% names(all_contvals), paste0("cc inconnu : ", cc, " (absent de all_contvals).")),
      need(nzchar(zone), "Veuillez sélectionner une zone.")
    )
    
    surfexp  <- pente1 + pente2 + pente3
    cc_conts <- all_contvals[[cc]]
    
    out_lines <- character(0)   # plus pratique que concaténer une string
    warn_lines <- character(0)
    
    for (icont in names(cc_conts)) {
      
      # programme inconnu dans K_progs => on n'arrête pas tout, on signale
      if (!icont %in% names(K_progs)) {
        warn_lines <- c(warn_lines, paste0("⚠ Programme non référencé dans K_progs : ", icont))
        next
      }
      
      icont_lab <- K_progs[[icont]]
      sub_conts <- cc_conts[[icont]]
      
      # Cas 1 : valeur unique
      if (length(sub_conts) == 1) {
        contrib <- suppressWarnings(as.numeric(sub_conts))
        if (is.na(contrib)) {
          warn_lines <- c(warn_lines, paste0("⚠ ", icont_lab, " : contribution non numérique"))
          next
        }
        
        itot <- contrib * surfexp / 10000
        itot <- round(itot, 2)
        out_lines <- c(out_lines, paste0(icont_lab, " : ", contrib, ".- x ", surfexp, "m2 = ", itot, ".-"))
        next
      }
      
      # Cas 2 : par zone
      if (all(names(sub_conts) %in% c(31, 41, 51:54, 99))) {
        
        # si zone absente : on écrit un message pour CE programme, mais on continue
        if (!zone %in% names(sub_conts)) {
          warn_lines <- c(warn_lines, paste0("⚠ ", icont_lab, " : Zone ", zone, " absente"))
          next
        }
        
        contrib <- suppressWarnings(as.numeric(sub_conts[[zone]]))
        if (is.na(contrib)) {
          warn_lines <- c(warn_lines, paste0("⚠ ", icont_lab, " : contribution non numérique pour la zone ", zone))
          next
        }
        
        itot <- contrib * surfexp / 10000
        itot <- round(itot, 2)
        out_lines <- c(out_lines, paste0(icont_lab, " : ", contrib, ".-/ha x ", surfexp, "m2 = ", itot, ".-"))
        next
      }
      
      # Cas 3 : pentes KL_HAN / KL_REB
      if (!(icont == "ns1:KL_HAN" || icont == "ns1:KL_REB")) {
        warn_lines <- c(warn_lines, paste0("⚠ ", icont_lab, " : structure inattendue (ni constant, ni zones, ni pentes)"))
        next
      }
      
      contrib1 <- suppressWarnings(as.numeric(sub_conts[[1]]))
      contrib2 <- suppressWarnings(as.numeric(sub_conts[[2]]))
      contrib3 <- suppressWarnings(as.numeric(sub_conts[[3]]))
      
      if (any(is.na(c(contrib1, contrib2, contrib3)))) {
        warn_lines <- c(warn_lines, paste0("⚠ ", icont_lab, " : contributions non numériques (pentes)"))
        next
      }
      
      itot <- contrib1 * pente1/10000 + contrib2 * pente2/10000 + contrib3 * pente3/10000
      itot <- round(itot, 2)
      out_lines <- c(out_lines, paste0(
        icont_lab, " : ", contrib1, ".-/ha x ", pente1, "m2 + ",  contrib2, ".-/ha x ", pente2, "m2", 
          " + ", contrib3 , ".-/ha x ", pente3, "m2 = ", itot, ".-"
      ))
    }
    
    # Assemblage final : résultats + (si présent) section avertissements
    out_txt <- paste(out_lines, collapse = "\n")
    
    if (length(warn_lines) > 0) {
      out_txt <- paste0(
        out_txt,
        if (nzchar(out_txt)) "\n\n" else "",
        "— Avertissements —\n",
        paste(warn_lines, collapse = "\n")
      )
    }
    
    out_txt
  }, ignoreInit = TRUE)
  
  output$out_txt <- renderText({
    calc()
  })
  
  # --- Comparer : snapshot du résultat courant vers la carte du bas ---
  prev <- reactiveValues(meta = NULL, txt = NULL)
  
  observeEvent(input$compare, {
    # Il faut avoir calculé au moins une fois
    req(calc())
    
    cc <- trimws(input$cc %||% "")
    zone <- trimws(input$zone %||% "")
    p1 <- as.numeric(input$pente1 %||% 0)
    p2 <- as.numeric(input$pente2 %||% 0)
    p3 <- as.numeric(input$pente3 %||% 0)
    surfexp <- p1 + p2 + p3
    
    prev$meta <- paste0("Référence : cc=", cc, " | zone=", zone,
                        " | pentes=", p1, " / ", p2, " / ", p3,
                        " | total=", surfexp, " m²")
    prev$txt <- calc()
  }, ignoreInit = TRUE)
  
  output$prev_block <- renderUI({
    if (is.null(prev$txt)) {
      return(tags$div(
        class = "meta",
        "Aucune référence enregistrée. Cliquez sur « Calculer » puis « Comparer »."
      ))
    }
    
    tagList(
      tags$p(class = "meta", prev$meta),
      tags$pre(prev$txt)
    )
  })
  
})
