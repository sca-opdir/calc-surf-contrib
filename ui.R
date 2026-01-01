library(shiny)

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Calcul surf. contrib. — aperçu"),
    tags$style(HTML("
      :root{
        --bg1:#f6f9ff; --bg2:#ffffff; --card:#ffffff;
        --text:#17223b; --muted:#5b6785; --border:rgba(20,35,70,.10);
        --shadow: 0 12px 28px rgba(16,24,40,.10); --radius:18px;
        --accent:#2fb7a5; --accent2:#4c86ff;
      }
      body{
        background:
          radial-gradient(900px 380px at 20% 0%, rgba(47,183,165,.18), transparent 62%),
          radial-gradient(820px 360px at 88% 10%, rgba(76,134,255,.16), transparent 58%),
          linear-gradient(180deg, var(--bg1), var(--bg2) 55%);
        color: var(--text);
        font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
      }
      .topbar{
        background: linear-gradient(90deg, rgba(47,183,165,.14), rgba(76,134,255,.12));
        border: 1px solid var(--border); border-radius: var(--radius);
        padding: 16px 18px; margin: 18px 0 14px 0; box-shadow: var(--shadow);
      }
      .topbar h1{ margin:0; font-size:20px; font-weight:800; }
      .topbar .sub{ margin-top:6px; color:var(--muted); font-size:13px; }

      .card{
        background: var(--card); border:1px solid var(--border);
        border-radius: var(--radius); box-shadow: var(--shadow);
        padding: 16px; margin-bottom: 16px;
      }
      .card-title{
        display:flex; align-items:center; justify-content:space-between;
        margin-bottom: 10px; font-weight: 800;
      }
      .badge{
        display:inline-flex; align-items:center; gap:8px;
        padding: 5px 10px; border-radius: 999px; border: 1px solid var(--border);
        color: var(--muted); font-size: 12px; background: rgba(255,255,255,.65);
      }
      .dot{ width:10px; height:10px; border-radius:50%;
        background: linear-gradient(90deg, var(--accent), var(--accent2));
      }

      label{ color: var(--muted); font-weight: 700; }
      .form-control{
        background:#fbfcff; border:1px solid var(--border);
        color: var(--text); border-radius: 12px;
      }
      .form-control:focus{
        border-color: rgba(76,134,255,.45);
        box-shadow: 0 0 0 3px rgba(76,134,255,.14);
      }

      .btn-primary{
        background: linear-gradient(90deg, var(--accent), var(--accent2));
        border:none; border-radius:14px; font-weight:900;
        padding: 10px 14px; box-shadow: 0 10px 20px rgba(76,134,255,.18);
      }
      .btn-default{
        border-radius:14px;
        font-weight:800;
        padding: 10px 14px;
        border: 1px solid var(--border);
        background: #ffffff;
      }
      .btn-row{ display:flex; gap:10px; flex-wrap:wrap; margin-top: 6px; }

/* --- Fix états focus/active des boutons --- */
.btn-primary,
.btn-primary:hover,
.btn-primary:focus,
.btn-primary:active,
.btn-primary:focus:active,
.btn-primary.active {
  background: linear-gradient(90deg, var(--accent), var(--accent2)) !important;
  border: none !important;
  color: #fff !important;
  outline: none !important;
}

.btn-primary:focus {
  box-shadow: 0 0 0 3px rgba(76,134,255,.18) !important;
}

/* Bouton Comparer (btn-default) stable aussi */
.btn-default,
.btn-default:hover,
.btn-default:focus,
.btn-default:active,
.btn-default:focus:active,
.btn-default.active {
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  outline: none !important;
}

.btn-default:focus {
  box-shadow: 0 0 0 3px rgba(47,183,165,.12) !important;
}


      .kpi{ display:flex; gap:12px; flex-wrap:wrap; margin-top: 12px; }
      .kpi .box{
        background: linear-gradient(180deg, rgba(47,183,165,.06), rgba(76,134,255,.05));
        border: 1px solid var(--border); border-radius: 14px; padding: 10px 12px;
        min-width: 140px;
      }
      .kpi .box .v{ font-weight: 900; font-size: 16px; }
      .kpi .box .t{ color: var(--muted); font-size: 12px; margin-top: 2px; }

      pre{
        background:#fbfcff; border:1px solid var(--border);
        border-radius:14px; padding:14px; margin:0;
        white-space: pre-wrap; word-break: break-word;
      }
      .meta{
        color: var(--muted);
        font-size: 12px;
        margin: 0 0 10px 0;
      }
      .footnote{ color: var(--muted); font-size: 12px; margin-top: 10px; }
    "))
  ),
  
  fluidRow(
    column(
      12,
      div(
        class = "topbar",
        tags$h1("Calcul surf. contrib. — comparer deux scénarios"),
        div(class = "sub",
            "1) Calculer → 2) Comparer pour garder une référence → changer cc/zone/pentes → recalculer.")
      )
    )
  ),
  
  fluidRow(
    column(
      4,
      div(
        class = "card",
        div(class="card-title",
            tags$span("Paramètres"),
            tags$span(class="badge", tags$span(class="dot"), "Entrées")
        ),
        
        selectInput("cc", "Code culture (cc)", choices = character(0)),
        selectInput("zone", "Zone",
                    choices = c("31","41","51","52","53","54"),
                    selected = "41"),
        
        uiOutput("pente1_ui"),
        uiOutput("pente2_ui"),
        uiOutput("pente3_ui"),
        
        div(class="btn-row",
            actionButton("go", "Calculer", class = "btn-primary"),
            actionButton("compare", "Comparer", class = "btn-default")
        ),
        
        div(class="kpi",
            div(class="box",
                div(class="v", textOutput("kpi_surfexp", inline = TRUE)),
                div(class="t", "Surface totale (m²)")
            ),
            div(class="box",
                div(class="v", textOutput("kpi_cc", inline = TRUE)),
                div(class="t", "cc")
            ),
            div(class="box",
                div(class="v", textOutput("kpi_zone", inline = TRUE)),
                div(class="t", "zone")
            )
        ),
        
        div(class="footnote",
            "Comparer copie le résultat courant dans la carte “Référence” (en dessous).")
      )
    ),
    
    column(
      8,
      
      # Résultat courant
      div(
        class = "card",
        div(class="card-title",
            tags$span("Résultat courant"),
            tags$span(class="badge", tags$span(class="dot"), "actuel")
        ),
        p(class="meta", textOutput("current_meta")),
        verbatimTextOutput("out_txt")
      ),
      
      # Référence (snapshot)
      div(
        class = "card",
        div(class="card-title",
            tags$span("Référence (dernière comparaison)"),
            tags$span(class="badge", tags$span(class="dot"), "copie")
        ),
        uiOutput("prev_block")
      )
    )
  )
)
