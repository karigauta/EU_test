library(shiny)
library(ggplot2)
library(scales)
library(markdown)
# ══════════════════════════════════════════════════════════════════
# INTEGRATED CAP + TARIFF IMPACT MODEL FOR ICELAND
# Rekon / Kári Gautason — March 2026
#
# Unified single-page model:
#   Tariff removal → price drops → supply response (elasticities)
#   → reduced production → adjusted CAP support calculations
#
# Key references:
#   Kola, Hofreither & Rabinowicz (2000) — Nordic accession impacts
#   Niemi (2003) — Static welfare effects on Finnish agriculture
#   OECD PSE Iceland (2025) — PSE 47%, prices 60% above border
#   EU Evaluation of Nordic Aid Schemes (2007)
#   Hagstofa Íslands, LbhÍ Rit 179 (2025)
# ══════════════════════════════════════════════════════════════════

# ── Rekon Brand Palette ──────────────────────────────────────────
col_green    <- "#4A5C36"   # Highland Green
col_cloud    <- "#F3F4F6"   # Cloud White (backgrounds)
col_grey     <- "#6B7280"   # Mountain Grey
col_brown    <- "#57534E"   # Soil Brown
col_red      <- "#A12B26"   # Signal Red
col_p1       <- "#4A90E2"   # Sky Blue — Pillar 1 / EU
col_anc      <- "#E2B14A"   # Lichen Gold — ANC
col_gap      <- "#A12B26"   # Signal Red — Gap / Loss
col_accent   <- "#57534E"   # Soil Brown — totals/neutral
col_basalt   <- "#5D6D7E"   # Basalt Grey
col_moss     <- "#789A5B"   # Moss Green
col_consumer <- "#2E86AB"   # Consumer benefit blue
col_before   <- "#D1D5DB"   # Light grey for "before" bars

# ── CPI weights (VIS01306, Hagstofa Íslands, desember 2025) ─────
# Source: Vogir fyrir undirvisitölur neysluverðs, COICOP 2018
# Weights are out of 10,000 total CPI weight; food total = 1,386
CPI_W_DAIRY    <- 231   # 01141-01147, 01149 (mjólk, ostur, jógúrt — án eggs)
CPI_W_EGGS     <- 22    # 01148 Egg
CPI_W_LAMB     <- 59    # 011223 Geita-, lamba- og kindakjöt
CPI_W_BEEF     <- 54    # 011221 Nautgripakjöt
CPI_W_POULTRY  <- 48    # 011224 Alifuglakjöt
CPI_W_VEG      <- 122   # 0117 Grænmeti og hnýði
CPI_W_FOOD     <- 1386  # 011 Matur (food only, excluding beverages)

APP_VERSION <- "v1.4"

# ── Value chain splits (non-farmer portion, proportions summing to 1) ──────
# Used in farm_share_pie renderPlot. Defined globally to avoid re-allocation
# on every slider interaction. Sources: AHDB 2024, USDA ERS, OECD Iceland 2025.
CHAIN_SPLITS <- list(
  "Mjólk"     = c("Vinnsla (Mjólkursamsalan)" = 0.42, "Pökkun" = 0.13, "Smásala" = 0.45),
  "Lambakjöt" = c("Slátrun (SS/HB)" = 0.38, "Dreifing" = 0.12, "Smásala" = 0.50),
  "Nautakjöt" = c("Slátrun" = 0.32, "Dreifing" = 0.18, "Smásala" = 0.50),
  "Alifuglar" = c("Vinnsla" = 0.38, "Pökkun" = 0.12, "Smásala" = 0.50),
  "Egg"       = c("Pökkun" = 0.28, "Dreifing" = 0.12, "Smásala" = 0.60),
  "Grænmeti"  = c("Vinnsla/Pökkun" = 0.22, "Dreifing" = 0.18, "Smásala" = 0.60)
)

# ── Shared theme helper ──────────────────────────────────────────
rekon_theme <- function(base_size = 13) {
  theme_minimal(base_family = "Montserrat", base_size = base_size) %+replace%
    theme(
      plot.title = element_text(face = "bold", colour = col_brown, size = 16),
      plot.subtitle = element_text(colour = col_grey, size = 11, family = "Lora"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(colour = col_grey),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )
}

rekon_theme_flip <- function(base_size = 12) {
  rekon_theme(base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = 11, colour = col_grey)
    )
}

# ── CSS ──────────────────────────────────────────────────────────
app_css <- sprintf("
  body {
    font-family: 'Lora', Georgia, serif;
    background: %s;
    color: %s;
    font-size: 13px;
  }
  h1, h2, h3, h4, h5, .metric-label, .metric-value, .metric-sub,
  .brand-bar, .brand-title, .brand-sub, th,
  .nav-tabs > li > a {
    font-family: 'Montserrat', Arial, sans-serif;
  }
  h3 { color: %s; font-weight: 700; }
  h4 { color: %s; font-weight: 600; margin-top: 20px; margin-bottom: 10px; }
  .well { background: #fff; border: 1px solid #E5E7EB; border-radius: 8px; }

  /* Brand header */
  .brand-bar {
    background: %s; color: #fff;
    padding: 18px 28px; border-radius: 10px; margin-bottom: 22px;
    display: flex; align-items: center; justify-content: space-between;
  }
  .brand-title { font-size: 22px; font-weight: 700; letter-spacing: 0.5px; }
  .brand-title span { font-weight: 400; opacity: 0.85; font-size: 15px; margin-left: 10px; }
  .brand-sub { font-size: 12px; opacity: 0.8; font-weight: 400; }

  /* Tabs */
  .nav-tabs { border-bottom: 2px solid %s; margin-bottom: 18px; }
  .nav-tabs > li > a {
    color: %s; font-weight: 600; font-size: 13px;
    border: none; border-bottom: 3px solid transparent;
    padding: 10px 18px; margin-bottom: -2px;
  }
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover {
    color: %s; border-bottom: 3px solid %s;
    background: transparent;
  }
  .nav-tabs > li > a:hover { color: %s; background: transparent; }

  /* Metric cards */
  .metric-box {
    background: #fff; border-radius: 8px; padding: 16px 18px; margin-bottom: 12px;
    border: 1px solid #E5E7EB; border-left: 4px solid %s;
    box-shadow: 0 1px 3px rgba(0,0,0,0.04);
  }
  .metric-box.eu    { border-left-color: %s; }
  .metric-box.cap   { border-left-color: %s; }
  .metric-box.gap   { border-left-color: %s; }
  .metric-box.rate  { border-left-color: %s; }
  .metric-box.loss  { border-left-color: %s; }
  .metric-box.gain  { border-left-color: %s; }
  .metric-box.farm  { border-left-color: %s; }
  .metric-label {
    font-size: 11px; color: %s; text-transform: uppercase;
    letter-spacing: 0.5px; font-weight: 600; margin-bottom: 4px;
  }
  .metric-value { font-size: 26px; font-weight: 700; color: %s; }
  .metric-sub { font-size: 11px; color: %s; font-weight: 600; margin-top: 2px; }

  .section-divider { border: none; border-top: 2px solid %s; margin: 24px 0; }
  .source-note {
    font-family: 'Lora', Georgia, serif;
    font-size: 11px; color: %s; line-height: 1.6; margin-top: 10px;
  }
  .help-block { font-family: 'Lora', Georgia, serif; font-size: 11px; color: %s; }
  .sidebar-section {
    font-family: 'Montserrat', Arial, sans-serif;
    font-size: 13px; font-weight: 600; color: %s;
    padding: 6px 0 4px 0; margin-top: 16px;
    border-bottom: 2px solid %s; margin-bottom: 10px;
  }
  .context-box {
    background: #fff; border: 1px solid #E5E7EB; border-left: 4px solid %s;
    border-radius: 6px; padding: 14px 18px; margin: 14px 0;
    font-size: 12px; line-height: 1.6;
  }
  .context-box strong { color: %s; }

  /* Navbar styling */
  .navbar { background: #57534E !important; border: none; }
  .navbar-brand { color: #fff !important; font-family: Montserrat; font-weight: 700; }
  .navbar-nav > li > a { color: rgba(255,255,255,0.85) !important; font-family: Montserrat; font-weight: 600; font-size: 13px; }
  .navbar-nav > li.active > a { color: #fff !important; border-bottom: 3px solid #4A90E2; background: transparent !important; }
  .navbar-nav > li > a:hover { color: #fff !important; background: rgba(255,255,255,0.1) !important; }
  .source-badge { display:inline-block; padding:2px 8px; border-radius:3px; font-size:10px; font-weight:600; font-family:Montserrat,sans-serif; color:#fff; margin-left:8px; vertical-align:middle; }

",
  col_cloud, col_brown,                         # body
  col_brown, col_brown,                          # h3, h4
  col_accent,                                     # brand bar
  col_p1, col_grey, col_brown, col_p1, col_brown, # tabs
  col_grey,                                        # metric default
  col_p1, col_green, col_red, col_anc,            # metric variants
  col_red, col_moss, col_basalt,                   # loss, gain, farm
  col_grey, col_brown, col_red,                    # label, value, sub
  col_cloud, col_grey, col_grey,                   # divider, source, help
  col_brown, col_p1,                               # sidebar section
  col_basalt, col_brown                            # context box
)

# ══════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════
source_note_text <- p(class = "source-note",
  "Höfundur: Rekon / Kári Gautason. Heimildir: LbhÍ Rit 179 (2025), ",
  "Úttekt AMS/HÍ á aðildarviðræðum (2014), ",
  "EU Reg. 2021/2115, Wageningen Economic Research, CAP Reform blog, ",
  "Hagstofa Íslands, OECD Agricultural Policy Monitoring (2025), ",
  "Kola, Hofreither & Rabinowicz (2000), ",
  "Niemi (2003) — Static Welfare Effects.")

ui <- navbarPage(
  title = div(class = "brand-title", "rekon", span("| CAP stuðningsætlun"), span(APP_VERSION, style = "font-size:10px;font-weight:400;color:#9CA3AF;margin-left:8px;vertical-align:middle;")),
  id = "main_nav",
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&family=Lora:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(app_css))
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 1 — Niðurstöður (Results) — Full-width, all sections A–G
  # ══════════════════════════════════════════════════════════════
  tabPanel("📊 Niðurstöður", value = "results",
    fluidPage(
      # ── Key Metrics — two rows ──
      fluidRow(
        column(3, div(class = "metric-box loss",
                      div(class = "metric-label", "Tekjutap bænda"),
                      uiOutput("metric_loss"))),
        column(3, div(class = "metric-box cap",
                      div(class = "metric-label", "CAP leiðrétt"),
                      uiOutput("metric_cap_adj"))),
        column(3, div(class = "metric-box gap",
                      div(class = "metric-label", "Nettó staða"),
                      uiOutput("metric_net"))),
        column(3, div(class = "metric-box gain",
                      div(class = "metric-label", "Sparnaður neytenda"),
                      uiOutput("metric_consumer")))
      ),
      fluidRow(
        column(4, div(class = "metric-box eu",
                      div(class = "metric-label", "Núverandi kerfi (fastur pottur)"),
                      uiOutput("metric_fixed_pot"))),
        column(4, div(class = "metric-box farm",
                      div(class = "metric-label", "Stuðningur á bú — núverandi kerfi"),
                      uiOutput("metric_per_farm_current"))),
        column(4, div(class = "metric-box rate",
                      div(class = "metric-label", "Tapaður púði"),
                      uiOutput("metric_cushion_lost")))
      ),

      hr(class = "section-divider"),

      # ── Section A: Verðáhrif ──
      h4("A. Verðáhrif — Áhrif tollaafnáms á framleiðendaverð",
         tags$span(class = "source-badge", style = paste0("background:", col_anc), "Hagfræði")),
      plotOutput("price_impact_chart", height = "380px"),

      hr(class = "section-divider"),

      # ── Section B: Framleiðsluvið brögð ──
      h4("B. Framleiðsluvið brögð — Samdráttur vegna verðlækkunar",
         tags$span(class = "source-badge", style = paste0("background:", col_anc), "Hagfræði")),
      div(class = "context-box",
        HTML("<strong>Keðjuáhrif in:</strong> Verðlækkun → framboðsteygni → samdráttur í framleiðslu.
        Td. ef mjólkurverð lækkar um 30% og teygni er 0.4, þá dregst framleiðsla saman um 12%.")
      ),
      plotOutput("production_response_chart", height = "400px"),

      hr(class = "section-divider"),

      # ── Section C: System Comparison ──
      h4("C. Kerfismunur — Fastur pottur (núverandi) vs. per-unit (CAP)",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Staða + Hagfræði")),
      div(class = "context-box",
        HTML("<strong>Lykilmunur:</strong> Búvörusamningar eru <em>fastur pottur</em> — þegar bú hætta,
        skiptist sama upphæð á færri einingar og stuðningur á bú <em>hækkar</em>.
        CAP er <em>per-unit</em> kerfi — þegar framleiðsla minnkar, lækka greiðslur í hlutfalli.
        Þbetta þyðir að yfirfærsla í CAP kostar ekki bara tollverniðna, heldur einnig
        þennan innbyggðan púða núverandi kerfis.")
      ),
      plotOutput("system_comparison_chart", height = "420px"),
      uiOutput("system_comparison_table"),

      hr(class = "section-divider"),

      # ── Section D: CAP stuðningur ──
      h4("D. CAP stuðningur — Leiðrétt fyrir framleiðslusamdrætti",
         tags$span(class = "source-badge", style = paste0("background:", col_p1), "ESB Samningur")),
      plotOutput("cap_comparison_chart", height = "420px"),
      uiOutput("payer_bar_html"),

      hr(class = "section-divider"),

      # ── Section E: Heildarjafnvægi ──
      h4("E. Heildarjafnvægi — Tekjutap vs. CAP vs. Neytendur",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      fluidRow(
        column(6, plotOutput("balance_chart", height = "340px")),
        column(6, plotOutput("farm_projection", height = "340px"))
      ),
      fluidRow(
        column(12, plotOutput("consumer_breakdown_chart", height = "260px"))
      ),

      hr(class = "section-divider"),

      # ── Section F: Winners/losers ──
      h4("F. Ávinningur og kostnaður — Hver hagnast?",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      uiOutput("winners_table"),

      hr(class = "section-divider"),

      # ── Section G: Detail breakdown ──
      h4("G. Sundurliðun — Grunnlína vs. Leiðrétt",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      uiOutput("detail_table_html"),

      p(class = "source-note",
        "Athugasemd: Þetta líkan er samþætt. Tollaafnám leiðir til verðlækkunar, ",
        "sem dregur úr framleiðslu (eftir framboðsteygni), ",
        "sem aftur lækkar CAP greiðslur. Auk þ ess er tekið tillit til kerfismunarins: ",
        "Búvörusamningar eru fastur pottur (samdrátt = meiri stuðningur á bú), ",
        "en CAP er per-unit kerfi (samdráttur = lægri heildargreiðslur). ",
        "Þessi falinn kostnaður er sýndur í kafla C. ",
        "Raunveruleg áhrif fara eftir aðlögunartíma, ",
        "sérákvæðum í aðildarsamningi og viðbrögðum framleiðenda. ",
        "Finnland fékk 5 ára aðlögunartímabil.")
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 2 — ESB Samningur (Negotiation)
  # ══════════════════════════════════════════════════════════════
  tabPanel("🔧 ESB Samningur", value = "negotiation",
    sidebarLayout(
      sidebarPanel(
        width = 4,

        div(class = "context-box",
          HTML("<strong>ESB samningsf orsendur:</strong> Stilltu CAP greiðsluhlutföll til að kanna
          hvað Ísland gæti fengið í samningum. Forsendurnar hér endurspegla möguleg útkomu —
          núverandi EU bil á greiðslum og sambærilegar niðurstöður Finnlands og Svíþjóðar 1995.")
        ),

        div(class = "sidebar-section", "CAP greiðsluhlutföll"),
        sliderInput("p1_rate", "Stoð 1: Greiðsla á hektara (€/ha)",
                    min = 100, max = 400, value = 200, step = 10, pre = "€"),
        helpText("EU lágmark: €200 (2023), €215 (2027). EU meðaltal: ~€243."),
        sliderInput("milk_rate", "Mjólkurgreiðsla (€/lítra)",
                    min = 0.04, max = 0.15, value = 0.09, step = 0.01, pre = "€"),
        helpText("Finnland: €0.07 (suður) – €0.11 (Lappland)"),
        sliderInput("ewe_rate", "Greiðsla á á (€/kind)",
                    min = 10, max = 40, value = 25, step = 1, pre = "€"),
        helpText("ESB hámark í tengdum greiðslum: €28/kind."),
        sliderInput("cattle_rate", "Greiðsla á grip (€/grip)",
                    min = 100, max = 400, value = 200, step = 10, pre = "€"),
        helpText("Finnland: €150–350 eftir svæði og tegund. Meðaltal ~€200."),
        sliderInput("anc_rate", "ANC greiðsla á hektara (€/ha)",
                    min = 100, max = 450, value = 250, step = 10, pre = "€"),
        helpText("Hámark norðan 62°: €450/ha. Finnland meðaltal: ~€217/ha."),
        sliderInput("eu_cofinance", "ESB-hlutfall af ANC (%)",
                    min = 30, max = 75, value = 55, step = 5, post = "%"),
        helpText("Fer eftir þ róunarstigi svæðis. Háþroðað ríki: ~40%. Jaðarsvæði: ~55–75%."),

        actionButton("go_results_2", "→ Sjá allar niðurstöður",
                     class = "btn btn-primary", style = "width:100%;margin-top:16px;"),

        hr(),
        source_note_text
      ),

      mainPanel(
        width = 8,

        h4("Bein forsýn — CAP greiðslur"),
        p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
          "Uppfærist í rauntíma við breytingar"),

        fluidRow(
          column(3, uiOutput("mini_cap_baseline")),
          column(3, uiOutput("mini_cap_adjusted")),
          column(3, uiOutput("mini_net")),
          column(3, uiOutput("mini_eu_vs_is"))
        ),

        plotOutput("mini_cap_chart", height = "380px"),

        uiOutput("mini_payer_bar")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 3 — Núverandi staða (Status)
  # ══════════════════════════════════════════════════════════════
  tabPanel("📋 Núverandi staða", value = "status",
    fluidPage(
      h4("Grunnforsendur og framleiðsla í dag"),
      p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
        "Stilltu grunnforsendur og framleiðslutölur til að aðlaga líkanið að raunverulegri stöðu."),

      fluidRow(
        column(6,
          wellPanel(
            style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
            h4("Grunnforsendur", style = "margin-top:0;"),
            sliderInput("eligible_ha", "Styrkhæft land (ha)",
                        min = 80000, max = 400000, value = 200000, step = 10000,
                        pre = "", post = " ha"),
            helpText("120k = ræktað land. 200k = ræktað + viaðhaldiað tún. Source: LbhÍ 2025, FAO."),
            sliderInput("current_support_isk", "Núverandi stuðningur (ma.kr.)",
                        min = 14, max = 30, value = 18, step = 0.5,
                        pre = "", post = " ma.kr."),
            helpText("Búvörusamningar."),
            numericInput("eur_isk", "Gengi EUR/ISK", value = 143, min = 100, max = 200, step = 1),
            sliderInput("farms_now", "Fjöldi búa í dag",
                        min = 1500, max = 4000, value = 2500, step = 100)
          )
        ),
        column(6,
          wellPanel(
            style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
            h4("Framleiðsla í dag", style = "margin-top:0;"),
            sliderInput("milk_litres", "Mjólkurframleiðsla (M lítr.)",
                        min = 120, max = 170, value = 145, step = 5, post = "M L"),
            sliderInput("ewes", "Ær (fjöldi)",
                        min = 200000, max = 500000, value = 400000, step = 10000),
            sliderInput("cattle", "Nautgripir (fjöldi)",
                        min = 20000, max = 50000, value = 31000, step = 1000),
            helpText("Framleiðsluverðmæti á núverandi innlendu verði:"),
            sliderInput("dairy_val", "Mjólkurvörur (ma.kr.)",
                        min = 10, max = 30, value = 19, step = 0.5, post = " ma.kr."),
            sliderInput("lamb_val", "Kindakjöt (ma.kr.)",
                        min = 3, max = 15, value = 7, step = 0.5, post = " ma.kr."),
            sliderInput("beef_val", "Nautakjöt (ma.kr.)",
                        min = 1, max = 8, value = 3, step = 0.5, post = " ma.kr."),
            sliderInput("egg_val", "Egg og alifuglar (ma.kr.)",
                        min = 0.5, max = 5, value = 2, step = 0.5, post = " ma.kr."),
            sliderInput("veg_val", "Grænmeti og gróðurhús (ma.kr.)",
                        min = 1, max = 8, value = 4, step = 0.5, post = " ma.kr.")
          )
        )
      ),

      actionButton("go_results_3", "→ Sjá niðurstöður",
                   class = "btn btn-primary", style = "margin-top:12px;"),

      hr(),
      source_note_text
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 4 — Forsendur [Advanced]
  # ══════════════════════════════════════════════════════════════
  tabPanel("⚙️ Forsendur [Advanced]", value = "assumptions",
    fluidPage(
      h4("Hagfræðilegar forsendur — Advanced stillingar"),
      p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
        "Stilltu tollaáhrif, framboðsviðbrögð og adrar hagfræðilegar forsendur."),

      # WellPanel 1: Tollaáhrif
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Tollaáhrif — Verðlækkun", style = "margin-top:0;"),
        div(class = "context-box",
          HTML("<strong>Byggt á reynslu Finnlands 1995:</strong> Þegar Finnland gekk til liðs við ESB,
          lækkuuðu framleiðendaverð verulega. Mjólk: −28–32%, nautakjöt: −38–43%,
          egg: −65–68%. Ísland ætti á að fara samanþerðar.")
        ),
        fluidRow(
          column(6,
            sliderInput("dairy_drop", "Mjólk — verðlækkun",
                        min = 0, max = 70, value = 30, step = 1, post = "%"),
            helpText("Finnland: −28% til −32%."),
            sliderInput("lamb_drop", "Kindakjöt — verðlækkun",
                        min = 0, max = 70, value = 45, step = 1, post = "%"),
            helpText("0% ef útflutningur gleypir alla umframframleiðslu (Kristófersson 2011). 45% = versta tilvik: innanlandsmarkaður brotnar saman vegna ódýrra alifugla."),
            sliderInput("beef_drop", "Nautakjöt — verðlækkun",
                        min = 0, max = 70, value = 30, step = 1, post = "%"),
            helpText("Kristófersson & Bjarnadóttir (2011): ~8% verðmunur við ESB. Finnland: −30%. ANR 2020: 35–42% vegin tollvernd. Miðgildi: 30%.")
          ),
          column(6,
            sliderInput("egg_drop", "Egg/alifuglar — verðlækkun",
                        min = 0, max = 70, value = 45, step = 1, post = "%"),
            helpText("Kristófersson & Bjarnadóttir (2011): egg −44%, alifuglar ~−50%. Finnland egg: −65–68% (hámark). Miðgildi: 45%."),
            sliderInput("veg_drop", "Grænmeti — verðlækkun",
                        min = 0, max = 70, value = 20, step = 1, post = "%"),
            helpText("EES-bókun 3 þýðir að flest grænmeti er þegar tollfrítt. Gróðurhúsarækt (tómatar, agúrkur) er jarðhitaknúin og á litla samkeppni. Lágt sjálfgefið gildi (12%) byggist á skýrslu ANR 2020.")
          )
        )
      ),

      # WellPanel 2: Framboðsviðbrögð
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Framboðsviðbrögð", style = "margin-top:0;"),
        div(class = "context-box",
          HTML("<strong>Framboðsteygni:</strong> H versu mikið framleiðsla lækkar þegar verð lækkar.
          Teygni 0.4 þýðir að 10% verðlækkun leiðir til 4% framleiðslulækkunar.
          Byggð á Kola et al. (2000) og reynslu Norðurlanda.")
        ),
        fluidRow(
          column(6,
            sliderInput("elast_dairy", "Mjólkurteygni",
                        min = 0.1, max = 0.8, value = 0.4, step = 0.05),
            helpText("10% verðlækkun → 4% framleiðslulækkun (við 0.4)"),
            sliderInput("elast_sheep", "Sauðfjárteygni",
                        min = 0.1, max = 1.0, value = 0.5, step = 0.05)
          ),
          column(6,
            sliderInput("elast_beef", "Nautgripateygni",
                        min = 0.1, max = 0.8, value = 0.4, step = 0.05),
            sliderInput("land_abandon", "Landyfirgefning (%)",
                        min = 5, max = 40, value = 15, step = 1, post = "%"),
            helpText("Hlutfall lands sem fellur úr notkun vegna samdráttar."),
            helpText("Finnland: mjólk 0.3–0.5, sauðfé 0.4–0.7. Byggð á Kola et al. (2000)")
          )
        )
      ),

      # WellPanel 3: Neytendur og bústöðvun
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Neytendur og bústöðvun", style = "margin-top:0;"),
        fluidRow(
          column(6,
            sliderInput("food_spend", "Matarútgjöld heimila (ma.kr./ár)",
                        min = 180, max = 400, value = 270, step = 10, post = " ma.kr."),
            helpText("Hagstofa THJ02105: 233 ma.kr. (2020 fastaverð). Við 2026 verðlag ~270–290 ma.kr. Heildarneysla á mat og drykkjarvörum."),
            div(class = "context-box",
              HTML("<strong>CPI-vegið mat:</strong> Sparnaður neytenda tekur nú tillit til bæði verðteygni og víxlteygni (cross-elasticity) eftir aðild. Heimildir byggja á Nordic accession modelum þar sem fuglakjöt er sterkur staðgengill fyrir naut/lamb. Vogir úr VIS01306 (Hagstofa 2025).")
            ),
            div(class = "sidebar-section", "Hlutfall framleiðandaverðs í smásöluverði"),
            helpText("Hversu stór hluti smásöluverðsins endurspeglar framleiðandaverðið? Hærra hlutfall þýðir að verðlækkun hjá framleiðanda skilar sér meira til neytenda."),
            sliderInput("farm_share_dairy", "Mjólk & ostur — framleiðandahlutfall",
                        min = 25, max = 60, value = 42, step = 1, post = "%"),
            helpText("AHDB UK: mjólk 43%. Íslenskt samvinnukerfi (Mjólkursamsalan) þrýstir hlutfallinu upp. Miðgildi: 42%. (AHDB 2025, USDA ERS)"),
            sliderInput("farm_share_lamb", "Lambakjöt — framleiðandahlutfall",
                        min = 35, max = 68, value = 52, step = 1, post = "%"),
            helpText("UK/Írland: 47–50%. Íslenskt samvinnuslátrun (SS/HB) styttir keðjuna — +5pp. Miðgildi: 52%. (AHDB Lamb Market Outlook 2024)"),
            sliderInput("farm_share_beef", "Nautakjöt — framleiðandahlutfall",
                        min = 30, max = 62, value = 48, step = 1, post = "%"),
            helpText("UK AHDB: 48% (£4.80/£10 kg). USDA ERS: 45–50%. Miðgildi: 48%. (AHDB Beef Market Update 2023)"),
            sliderInput("farm_share_poultry", "Alifuglar — framleiðandahlutfall",
                        min = 22, max = 52, value = 38, step = 1, post = "%"),
            helpText("Lægra en rautt kjöt vegna samþættrar framleiðslu (samningabúskapur). OECD Iceland 2025: há markaðsverðsstuðningur. Miðgildi: 38%."),
            sliderInput("farm_share_eggs", "Egg — framleiðandahlutfall",
                        min = 32, max = 62, value = 47, step = 1, post = "%"),
            helpText("UK: 36–44% (sveiflur milli ára). Stutt keðja á Íslandi þrýstir hlutfallinu upp. Miðgildi: 47%. (FarmingUK 2024, USDA ERS)"),
            sliderInput("farm_share_veg", "Grænmeti — framleiðandahlutfall",
                        min = 15, max = 48, value = 30, step = 1, post = "%"),
            helpText("USDA ERS: 20–29% (meðaltal 16 grænmetistegunda). Íslenskt gróðurhúsagrænmeti hefur styttri keðju — +5pp. Miðgildi: 30%. (USDA ERS Amber Waves 2018)")
          ),
          column(6,
            sliderInput("farm_exit_10y", "Bústöðvun á 10 árum (%)",
                        min = 10, max = 60, value = 35, step = 1, post = "%"),
            helpText("Finnland: −38% á 17 árum. Mjólkurbú: −82% á 24 árum."),
            hr(style = "margin:16px 0 10px;"),
            div(class = "sidebar-section", "Framleiðandahlutfall — yfirlit"),
            helpText("Hversu stór hluti smásöluverðsins fer til framleiðanda í hverjum flokki."),
            plotOutput("farm_share_pie", height = "420px"),
            hr(style = "margin:16px 0 10px;"),
            div(class = "sidebar-section", "Neytendaverðsbreyting — útreikningur"),
            helpText("Reiknað sem: framleiðsludrop × framleiðandahlutfall = neytendadrop."),
            uiOutput("consumer_drops_summary")
          )
        )
      ),

      actionButton("go_results_4", "→ Sjá niðurstöður",
                   class = "btn btn-primary", style = "margin-top:12px;"),

      hr(),
      source_note_text
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 5 — Heimildir (Sources)
  # ══════════════════════════════════════════════════════════════
  tabPanel("📚 Heimildir", value = "sources",
    fluidPage(
      div(style = "max-width:860px; margin:0 auto; padding:24px 12px;",
        includeMarkdown("sources.md")
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Shared helpers ────────────────────────────────────────────
  compute_cap <- function(ha, milk, ewes, cattle) {
    p1         <- ha     * input$p1_rate     / 1e6
    milk_pay   <- milk   * 1e6 * input$milk_rate / 1e6
    sheep_pay  <- ewes   * input$ewe_rate    / 1e6
    cattle_pay <- cattle * input$cattle_rate / 1e6
    nordic     <- milk_pay + sheep_pay + cattle_pay
    anc_total  <- ha * input$anc_rate / 1e6
    anc_eu     <- anc_total * input$eu_cofinance / 100
    anc_is     <- anc_total * (1 - input$eu_cofinance / 100)
    total      <- p1 + nordic + anc_total
    list(
      p1 = p1, milk = milk_pay, sheep = sheep_pay, cattle = cattle_pay,
      nordic    = nordic,
      anc_total = anc_total, anc_eu = anc_eu, anc_is = anc_is,
      total     = total,
      eu_pays      = p1 + anc_eu,
      iceland_pays = nordic + anc_is
    )
  }

  badge <- function(label, color) {
    sprintf('<span style="display:inline-block;padding:3px 12px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:600;font-family:Montserrat,sans-serif;">%s</span>', color, label)
  }

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: BASELINE — current production numbers
  # ────────────────────────────────────────────────────────────────
  baseline <- reactive({
    list(
      ha = input$eligible_ha,
      milk = input$milk_litres,    # M litres
      ewes = input$ewes,
      cattle = input$cattle,
      fx = input$eur_isk,
      farms = input$farms_now,
      current_eur = input$current_support_isk * 1000 / input$eur_isk,
      current_isk = input$current_support_isk  # ma.kr.
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CURRENT SYSTEM (fixed-pot) — what happens if
  # production declines UNDER the current Búvörusamningar regime
  #
  # Key insight: the total budget pot is FIXED (set by political
  # agreement). When farms exit and production shrinks, the same
  # pot is divided among fewer units → per-unit support RISES.
  # This is the opposite of CAP where per-unit rates are fixed
  # and total payments shrink with production.
  # ────────────────────────────────────────────────────────────────
  current_system <- reactive({
    b  <- baseline()
    s  <- shock()
    fe <- farm_exit()

    # The fixed pot stays the same regardless of production changes
    fixed_pot_eur <- b$current_eur  # €M — doesn't change

    # Per-unit support today (before any shock)
    # Approximate: split current support proportionally by sector value
    total_prod_val <- input$dairy_val + input$lamb_val + input$beef_val +
                      input$egg_val + input$veg_val
    dairy_share <- input$dairy_val / total_prod_val
    lamb_share  <- input$lamb_val / total_prod_val
    beef_share  <- input$beef_val / total_prod_val

    # Per-unit support today
    support_per_litre_now <- (fixed_pot_eur * dairy_share) / (b$milk)       # €M per M litres = €/litre
    support_per_ewe_now   <- (fixed_pot_eur * lamb_share * 1e6) / b$ewes    # €/ewe
    support_per_head_now  <- (fixed_pot_eur * beef_share * 1e6) / b$cattle  # €/head
    support_per_farm_now  <- fixed_pot_eur * 1e6 / b$farms                  # €/farm

    # Per-unit support AFTER production decline (same pot, fewer units)
    support_per_litre_after <- (fixed_pot_eur * dairy_share) / (s$new_milk)
    support_per_ewe_after   <- (fixed_pot_eur * lamb_share * 1e6) / s$new_ewes
    support_per_head_after  <- (fixed_pot_eur * beef_share * 1e6) / s$new_cattle
    farms_after <- fe$farms_after
    support_per_farm_after  <- fixed_pot_eur * 1e6 / farms_after

    # The "cushion effect" — how much more each surviving unit gets
    dairy_cushion_pct <- (support_per_litre_after / support_per_litre_now - 1) * 100
    sheep_cushion_pct <- (support_per_ewe_after / support_per_ewe_now - 1) * 100
    beef_cushion_pct  <- (support_per_head_after / support_per_head_now - 1) * 100
    farm_cushion_pct  <- (support_per_farm_after / support_per_farm_now - 1) * 100

    list(
      fixed_pot_eur = fixed_pot_eur,
      # Per-unit before
      per_litre_now = support_per_litre_now,
      per_ewe_now   = support_per_ewe_now,
      per_head_now  = support_per_head_now,
      per_farm_now  = support_per_farm_now,
      # Per-unit after (same pot, fewer units)
      per_litre_after = support_per_litre_after,
      per_ewe_after   = support_per_ewe_after,
      per_head_after  = support_per_head_after,
      per_farm_after  = support_per_farm_after,
      # Cushion %
      dairy_cushion = dairy_cushion_pct,
      sheep_cushion = sheep_cushion_pct,
      beef_cushion  = beef_cushion_pct,
      farm_cushion  = farm_cushion_pct,
      # Shares
      dairy_share = dairy_share,
      lamb_share = lamb_share,
      beef_share = beef_share
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: SHOCK — price drops + supply response → adjusted production
  # ────────────────────────────────────────────────────────────────
  shock <- reactive({
    b <- baseline()

    # Production change percentages
    dairy_pct_change <- input$dairy_drop * input$elast_dairy   # e.g., 30 * 0.4 = 12%
    sheep_pct_change <- input$lamb_drop  * input$elast_sheep
    beef_pct_change  <- input$beef_drop  * input$elast_beef
    land_pct_change  <- input$land_abandon

    # Adjusted production
    new_milk   <- b$milk   * (1 - dairy_pct_change / 100)
    new_ewes   <- b$ewes   * (1 - sheep_pct_change / 100)
    new_cattle <- b$cattle * (1 - beef_pct_change / 100)
    new_ha     <- b$ha     * (1 - land_pct_change / 100)

    list(
      dairy_pct_change = dairy_pct_change,
      sheep_pct_change = sheep_pct_change,
      beef_pct_change  = beef_pct_change,
      land_pct_change  = land_pct_change,
      new_milk   = new_milk,
      new_ewes   = new_ewes,
      new_cattle = new_cattle,
      new_ha     = new_ha
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: REVENUE SHOCK — sector price drops → revenue losses
  # Depends on: *_val, *_drop, eur_isk
  # ────────────────────────────────────────────────────────────────
  revenue_shock <- reactive({
    fx        <- input$eur_isk
    value_isk <- c(input$dairy_val, input$lamb_val, input$beef_val,
                   input$egg_val,   input$veg_val)
    drop_pct  <- c(input$dairy_drop, input$lamb_drop, input$beef_drop,
                   input$egg_drop,   input$veg_drop)
    value_eur <- value_isk * 1000 / fx
    loss_eur  <- value_eur * drop_pct / 100
    after_eur <- value_eur - loss_eur
    list(
      sectors = data.frame(
        sector    = c("Mjólkurvörur","Kindakjöt","Nautakjöt","Egg/alifuglar","Grænmeti"),
        value_isk = value_isk,
        drop_pct  = drop_pct,
        value_eur = value_eur,
        loss_eur  = loss_eur,
        after_eur = after_eur
      ),
      total_loss_eur = sum(loss_eur),
      total_before   = sum(value_eur),
      fx             = fx
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CONSUMER SAVINGS & DEMAND SHIFT (UPDATED WITH ELASTICITIES)
  # ────────────────────────────────────────────────────────────────
  consumer_savings <- reactive({
    fx      <- input$eur_isk

    # 1. Baseline spends based on static CPI weights
    base_spends  <- input$food_spend *
               c(CPI_W_DAIRY, CPI_W_LAMB, CPI_W_BEEF,
                 CPI_W_POULTRY, CPI_W_EGGS, CPI_W_VEG) / CPI_W_FOOD

    # 2. Retail Price Drops (expressed as negative decimals for the matrix)
    drops_p <- c(input$dairy_drop, input$lamb_drop, input$beef_drop,
                 input$egg_drop, input$egg_drop, input$veg_drop)
    shares  <- c(input$farm_share_dairy, input$farm_share_lamb, input$farm_share_beef,
                 input$farm_share_poultry, input$farm_share_eggs, input$farm_share_veg)

    # Calculate retail price % change
    cons_drops_pct <- drops_p * shares / 100 
    retail_price_change_dec <- -(cons_drops_pct / 100) 

    # 3. Define the Nordic/Icelandic Cross-Elasticity Matrix
    # Rows: Quantity Demanded (Dairy, Lamb, Beef, Poultry, Eggs, Veg)
    # Cols: Price Change of   (Dairy, Lamb, Beef, Poultry, Eggs, Veg)
    demand_elasticities <- matrix(c(
      -0.30,  0.00,  0.00,  0.00,  0.00,  0.00,  # Dairy (Isolated)
       0.00, -0.60, -0.10,  0.40,  0.00,  0.00,  # Lamb (Loses to Poultry, weak complement to Beef)
       0.00, -0.10, -0.65,  0.45,  0.00,  0.00,  # Beef (Loses to Poultry, weak complement to Lamb)
       0.00,  0.20,  0.25, -1.10,  0.00,  0.00,  # Poultry (Highly elastic, gains from red meat)
       0.00,  0.00,  0.00,  0.00, -0.40,  0.00,  # Eggs (Isolated)
       0.00,  0.00,  0.00,  0.00,  0.00, -0.30   # Veg (Isolated)
    ), nrow = 6, byrow = TRUE)

    # 4. Calculate Demand Shift (Quantity % Change)
    # Matrix multiplication: Elasticity Matrix %*% Price Change Vector
    quantity_change_dec <- as.numeric(demand_elasticities %*% retail_price_change_dec)

    # 5. Calculate New Spending and Savings
    # New spend = Base Spend * (1 + price_change) * (1 + quantity_change)
    new_spends <- base_spends * (1 + retail_price_change_dec) * (1 + quantity_change_dec)

    # Savings is the difference between what they would have spent vs what they now spend
    savings <- base_spends - new_spends

    list(
      consumer_breakdown = data.frame(
        sector     = c("Mjólk & ostur","Lambakjöt","Nautakjöt","Alifuglar","Egg","Grænmeti"),
        spend      = base_spends,
        prod_drop  = drops_p,
        farm_share = shares,
        drop       = cons_drops_pct,
        qty_shift  = quantity_change_dec * 100,
        savings    = savings,
        group      = c("dairy","meat","meat","meat","eggs","veg")
      ),
      consumer_save_isk = sum(savings),
      consumer_save_eur = sum(savings) * 1000 / fx
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: FARM EXIT — trajectory projection
  # Depends on: farms_now, farm_exit_10y ONLY
  # ────────────────────────────────────────────────────────────────
  farm_exit <- reactive({
    farms_after <- round(input$farms_now * (1 - input$farm_exit_10y / 100))
    annual_exit <- 1 - (1 - input$farm_exit_10y / 100)^(1/10)
    years <- 0:15
    list(
      farms_after     = farms_after,
      farms_lost      = input$farms_now - farms_after,
      farm_trajectory = data.frame(
        year  = years,
        farms = round(input$farms_now * (1 - annual_exit)^years)
      )
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CAP BASELINE — CAP on original (pre-shock) numbers
  # ────────────────────────────────────────────────────────────────
  cap_baseline <- reactive({
    b <- baseline()
    compute_cap(b$ha, b$milk, b$ewes, b$cattle)
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CAP ADJUSTED — CAP on post-shock production numbers
  # ────────────────────────────────────────────────────────────────
  cap_adjusted <- reactive({
    s <- shock()
    compute_cap(s$new_ha, s$new_milk, s$new_ewes, s$new_cattle)
  })

  # ────────────────────────────────────────────────────────────────
  # METRIC BOXES
  # ────────────────────────────────────────────────────────────────

  output$metric_loss <- renderUI({
    rs <- revenue_shock()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("−€%.0fM", rs$total_loss_eur)),
      div(class = "metric-sub", sprintf("%.0f%% af framleiðsluvermi", rs$total_loss_eur / rs$total_before * 100))
    )
  })

  output$metric_cap_adj <- renderUI({
    ca <- cap_adjusted()
    cb <- cap_baseline()
    diff_pct <- (ca$total - cb$total) / cb$total * 100
    tagList(
      div(class = "metric-value", style = paste0("color:", col_green),
          sprintf("€%.0fM", ca$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f%% lægra en grunnlína (€%.0fM)", abs(diff_pct), cb$total))
    )
  })

  output$metric_net <- renderUI({
    ca <- cap_adjusted()
    rs <- revenue_shock()
    b <- baseline()
    # Farmers lose: (1) revenue from price drops + (2) current Búvörusamningar
    # Farmers gain: CAP payments
    total_farmer_loss <- rs$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    col <- if (net >= 0) col_green else col_red
    sign <- if (net >= 0) "+" else "−"
    tagList(
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%s€%.0fM", sign, abs(net))),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("CAP €%.0fM vs. tap €%.0fM + núv. stuðn. €%.0fM",
                  ca$total, rs$total_loss_eur, b$current_eur))
    )
  })

  output$metric_consumer <- renderUI({
    con <- consumer_savings()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_moss),
          sprintf("+€%.0fM", con$consumer_save_eur)),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("%.0f ma.kr./ár lægra matarverð", con$consumer_save_isk))
    )
  })

  # ── Fixed-pot metrics ──

  output$metric_fixed_pot <- renderUI({
    b <- baseline()
    div(
      div(class = "metric-value", sprintf("€%.0fM", b$current_eur)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          "Fastur — breytist ekki þegar bú hætta")
    )
  })

  output$metric_per_farm_current <- renderUI({
    cs <- current_system()
    tagList(
      div(class = "metric-value", sprintf("€%s", format(round(cs$per_farm_now), big.mark = "."))),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("→ €%s eftir bústöðvun (+%.0f%%)",
                  format(round(cs$per_farm_after), big.mark = "."),
                  cs$farm_cushion))
    )
  })

  output$metric_cushion_lost <- renderUI({
    cs <- current_system()
    ca <- cap_adjusted()
    b <- baseline()
    fe <- farm_exit()
    # Under current system, surviving farms keep the full pot
    # Under CAP, surviving farms get reduced total (production-linked)
    # The difference is the "cushion lost"
    cap_per_farm <- ca$total * 1e6 / fe$farms_after
    cushion_lost_per_farm <- cs$per_farm_after - cap_per_farm
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("−€%s/bú", format(round(cushion_lost_per_farm), big.mark = "."))),
      div(class = "metric-sub",
          sprintf("Núv. kerfi: €%s/bú vs CAP: €%s/bú",
                  format(round(cs$per_farm_after), big.mark = "."),
                  format(round(cap_per_farm), big.mark = ".")))
    )
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION A: Price Impact Chart
  # ────────────────────────────────────────────────────────────────

  output$price_impact_chart <- renderPlot({
    rs <- revenue_shock()
    s <- rs$sectors

    df <- data.frame(
      sector = factor(rep(s$sector, 2), levels = rev(s$sector)),
      type = rep(c("Fyrir", "Eftir"), each = nrow(s)),
      value = c(s$value_eur, s$after_eur)
    )
    df$type <- factor(df$type, levels = c("Fyrir", "Eftir"))

    ann <- data.frame(
      sector = factor(s$sector, levels = rev(s$sector)),
      y_pos = s$value_eur + 2,
      label = sprintf("−%.0f%%", s$drop_pct)
    )

    ggplot(df, aes(x = sector, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = ann, aes(x = sector, y = y_pos, label = label),
                inherit.aes = FALSE, hjust = -0.1, size = 3.5,
                colour = col_red, fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = c("Fyrir" = col_before, "Eftir" = col_red),
                        labels = c("Núverandi verð", "Eftir tollaafnám")) +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(title = "Verðáhrif á framleiðendur eftir geirum",
           subtitle = sprintf("Heildar tekjutap: €%.0fM  |  Grátt = núverandi  |  Rauðt = eftir ESB-aðild",
                              rs$total_loss_eur),
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme_flip() +
      theme(
        legend.position = "top",
        legend.text     = element_text(size = 10)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # SECTION B: Production Response Chart
  # ────────────────────────────────────────────────────────────────

  output$production_response_chart <- renderPlot({
    b <- baseline()
    s <- shock()

    categories <- c("Mjólk\n(M lítrar)", "Ær\n(þús.)", "Nautgripir\n(þús.)", "Land\n(þús. ha)")
    before_vals <- c(b$milk, b$ewes / 1000, b$cattle / 1000, b$ha / 1000)
    after_vals  <- c(s$new_milk, s$new_ewes / 1000, s$new_cattle / 1000, s$new_ha / 1000)
    pct_changes <- c(s$dairy_pct_change, s$sheep_pct_change, s$beef_pct_change, s$land_pct_change)

    df <- data.frame(
      category = factor(rep(categories, 2), levels = rev(categories)),
      type = factor(rep(c("Fyrir", "Eftir"), each = 4), levels = c("Fyrir", "Eftir")),
      value = c(before_vals, after_vals)
    )

    ann <- data.frame(
      category = factor(categories, levels = rev(categories)),
      y_pos = before_vals * 1.02,
      label = sprintf("−%.0f%%", pct_changes)
    )

    ggplot(df, aes(x = category, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = ann, aes(x = category, y = y_pos, label = label),
                inherit.aes = FALSE, hjust = -0.15, size = 4,
                colour = col_red, fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = c("Fyrir" = col_before, "Eftir" = col_green),
                        labels = c("Grunnlína", "Eftir aðlögun")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(title = "Framleiðsluvið brögð — Samdráttur vegna verðlækkunar",
           subtitle = sprintf("Mjólk: −%.0f%%  |  Sauðfé: −%.0f%%  |  Nautgr.: −%.0f%%  |  Land: −%.0f%%",
                              s$dairy_pct_change, s$sheep_pct_change, s$beef_pct_change, s$land_pct_change),
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme_flip() +
      theme(
        legend.position = "top",
        legend.text     = element_text(size = 10)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # SECTION C: System Comparison — Fixed pot vs CAP per-unit
  # ────────────────────────────────────────────────────────────────

  output$system_comparison_chart <- renderPlot({
    b <- baseline()
    cs <- current_system()
    ca <- cap_adjusted()

    # Compare per-farm support under both systems at different exit levels
    exit_levels <- seq(0, 50, by = 5)  # 0% to 50% farm exit
    farms_at_level <- b$farms * (1 - exit_levels / 100)

    # Current system: fixed pot / fewer farms
    current_per_farm <- (b$current_eur * 1e6) / farms_at_level

    # CAP system: total CAP shrinks as production drops (proportionally)
    # Approximate: production decline is proportional to farm exit
    # (not exact, but reasonable approximation for this comparison)
    s <- shock()
    prod_decline_at_exit <- exit_levels * 0.7  # farms exiting ≈ 70% production impact
    cap_total_at_level <- ca$total * (1 - prod_decline_at_exit / 100 * 0.3)  # partial effect
    # More precise: use the actual adjusted CAP at current exit level
    # and scale linearly from cap_baseline to cap_adjusted
    cb <- cap_baseline()
    actual_exit_pct <- input$farm_exit_10y
    cap_at_level <- cb$total + (ca$total - cb$total) * (exit_levels / actual_exit_pct)
    cap_at_level <- pmin(cap_at_level, cb$total)  # can't exceed baseline
    cap_at_level <- pmax(cap_at_level, ca$total * 0.5)  # floor
    cap_per_farm <- (cap_at_level * 1e6) / farms_at_level

    df <- data.frame(
      exit = rep(exit_levels, 2),
      system = factor(rep(c("Núverandi kerfi\n(fastur pottur)", "CAP\n(per-unit)"), each = length(exit_levels)),
                      levels = c("Núverandi kerfi\n(fastur pottur)", "CAP\n(per-unit)")),
      per_farm = c(current_per_farm, cap_per_farm) / 1000  # €thousands
    )

    # Mark the actual expected exit point
    actual_idx <- which.min(abs(exit_levels - actual_exit_pct))

    ggplot(df, aes(x = exit, y = per_farm, colour = system, linewidth = system)) +
      geom_line() +
      geom_point(data = df[df$exit == exit_levels[actual_idx], ],
                 size = 4, shape = 21, fill = "white", stroke = 1.5) +
      annotate("text", x = exit_levels[actual_idx] + 2,
               y = current_per_farm[actual_idx] / 1000 + 2,
               label = sprintf("Spá búst.: %.0f%%", actual_exit_pct),
               hjust = 0, size = 3.3, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_ribbon(data = data.frame(
                    exit = exit_levels,
                    ymin = cap_per_farm / 1000,
                    ymax = current_per_farm / 1000),
                  aes(x = exit, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = col_red, alpha = 0.08) +
      annotate("text",
               x = mean(exit_levels), y = mean(c(current_per_farm[6], cap_per_farm[6])) / 1000,
               label = "Tapaður púði", colour = col_red, size = 3.5,
               fontface = "italic", family = "Lora") +
      scale_colour_manual(values = c(col_p1, col_red)) +
      scale_linewidth_manual(values = c(1.5, 1.5)) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         breaks = seq(0, 50, 10)) +
      scale_y_continuous(labels = function(x) paste0("€", x, "k")) +
      labs(title = "Stuðningur á bú — Núverandi kerfi vs. CAP",
           subtitle = "Núverandi kerfi: fastur pottur styrkist við bústöðvun  |  CAP: per-unit greiðslur dragast saman",
           x = "Bústöðvun (%)", y = "Stuðningur á bú (€þús.)",
           colour = NULL, linewidth = NULL) +
      rekon_theme() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.3)
      )
  }, res = 110, bg = "transparent")

  output$system_comparison_table <- renderUI({
    cs <- current_system()
    ca <- cap_adjusted()
    b <- baseline()
    fe <- farm_exit()

    cap_per_farm <- ca$total * 1e6 / fe$farms_after

    HTML(sprintf('
      <div style="margin:18px 0; font-family:Montserrat,sans-serif;">
        <table style="width:100%%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">
          <thead><tr style="border-bottom:2px solid %s;background:%s;">
            <th style="padding:8px 14px;text-align:left;font-size:12px;color:%s;text-transform:uppercase;">Mælikvarði</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">Núv. kerfi (í dag)</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">Núv. kerfi (eftir búst.)</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">CAP (leiðrétt)</th>
          </tr></thead>
          <tbody>
            <tr style="background:%s;">
              <td style="padding:8px 14px;font-weight:600;">Heildarpottur (€M)</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">€%.0fM</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">€%.0fM</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">€%.0fM</td>
            </tr>
            <tr>
              <td style="padding:8px 14px;font-weight:600;">Fjöldi búa</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
            </tr>
            <tr style="background:%s;">
              <td style="padding:8px 14px;font-weight:600;">Stuðningur á bú (€)</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;">€%s</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">€%s</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">€%s</td>
            </tr>
            <tr>
              <td style="padding:8px 14px;font-weight:600;">Breyting á bú</td>
              <td style="padding:8px 14px;text-align:right;">—</td>
              <td style="padding:8px 14px;text-align:right;color:%s;font-weight:700;">+%.0f%%</td>
              <td style="padding:8px 14px;text-align:right;color:%s;font-weight:700;">%.0f%%</td>
            </tr>
            <tr style="border-top:2px solid %s;background:%s;">
              <td style="padding:10px 14px;font-weight:700;color:%s;">Tapaður púði á bú</td>
              <td style="padding:10px 14px;" colspan="2"></td>
              <td style="padding:10px 14px;text-align:right;font-weight:700;color:%s;font-size:15px;">−€%s/bú</td>
            </tr>
          </tbody>
        </table>
      </div>',
      col_brown, col_cloud, col_grey, col_grey, col_grey, col_grey,
      col_cloud,
      col_p1, b$current_eur,
      col_p1, b$current_eur,  # same pot after exit!
      col_red, ca$total,
      format(b$farms, big.mark = "."),
      format(fe$farms_after, big.mark = "."),
      format(fe$farms_after, big.mark = "."),
      col_cloud,
      format(round(cs$per_farm_now), big.mark = "."),
      col_moss, format(round(cs$per_farm_after), big.mark = "."),
      col_red, format(round(cap_per_farm), big.mark = "."),
      col_moss, cs$farm_cushion,
      col_red, (cap_per_farm / cs$per_farm_now - 1) * 100,
      col_red, col_cloud,
      col_red,
      col_red, format(round(cs$per_farm_after - cap_per_farm), big.mark = ".")
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION D: CAP Comparison Chart (Baseline vs Adjusted waterfall)
  # ────────────────────────────────────────────────────────────────

  output$cap_comparison_chart <- renderPlot({
    cb <- cap_baseline()
    ca <- cap_adjusted()
    b <- baseline()

    labels <- c("Stoð 1\nGrunnl.", "Stoð 1\nLeiðr.",
                "Art. 142\nGrunnl.", "Art. 142\nLeiðr.",
                "ANC\nGrunnl.", "ANC\nLeiðr.",
                "Samtals\nGrunnl.", "Samtals\nLeiðr.")

    values <- c(cb$p1, ca$p1,
                cb$nordic, ca$nordic,
                cb$anc_total, ca$anc_total,
                cb$total, ca$total)

    fills <- c(col_before, col_p1,
               col_before, col_green,
               col_before, col_anc,
               col_before, col_accent)

    df <- data.frame(
      label = factor(labels, levels = labels),
      value = values,
      fill = fills
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = b$current_eur, linetype = "dashed", colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 8.4, y = b$current_eur + 2,
               label = sprintf("Núverandi: €%.0fM", b$current_eur),
               hjust = 1, size = 3.2, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_text(aes(label = sprintf("€%.0fM", value)),
                vjust = -0.5, size = 3.2, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = "CAP stuðningur — Grunnlína vs. Leiðrétt",
           subtitle = sprintf("Grunnlína: €%.0fM  |  Leiðrétt (eftir framleiðslusamdrátt): €%.0fM  |  Munur: −€%.0fM",
                              cb$total, ca$total, cb$total - ca$total),
           x = NULL, y = NULL) +
      rekon_theme() +
      theme(axis.text.x = element_text(size = 9, lineheight = 1.1))
  }, res = 110, bg = "transparent")

  # ── Payer bar (based on adjusted numbers) ──

  output$payer_bar_html <- renderUI({
    ca <- cap_adjusted()
    total <- ca$eu_pays + ca$iceland_pays
    eu_pct <- round(ca$eu_pays / total * 100)
    is_pct <- 100 - eu_pct
    HTML(sprintf('
      <div style="margin:18px 0 8px 0; font-family:Montserrat,sans-serif;">
        <div style="font-size:13px;font-weight:600;color:%s;margin-bottom:8px;">
          Hver borgar? (leiðrétt) &mdash; ESB: €%.0fM (%s%%) &nbsp;|&nbsp; Ísland: €%.0fM (%s%%)
        </div>
        <div style="display:flex;height:32px;border-radius:6px;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,0.08);">
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            ESB €%.0fM
          </div>
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            Ísland €%.0fM
          </div>
        </div>
      </div>',
      col_brown,
      ca$eu_pays, eu_pct, ca$iceland_pays, is_pct,
      eu_pct, col_p1, ca$eu_pays,
      is_pct, col_accent, ca$iceland_pays
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION E: Balance chart + Farm projection
  # ────────────────────────────────────────────────────────────────

  output$balance_chart <- renderPlot({
    rs  <- revenue_shock()
    con <- consumer_savings()
    ca <- cap_adjusted()
    b <- baseline()

    # Total farmer loss = revenue loss from tariffs + loss of current support
    total_farmer_loss <- rs$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss

    df <- data.frame(
      label = factor(c("Tekjutap\n(verðlækkun)", "Tapaður\nstuðn. pottur",
                       "CAP\nleiðrétt", "Nettó staða\nbænda",
                       "Sparnaður\nneytenda"),
                     levels = c("Tekjutap\n(verðlækkun)", "Tapaður\nstuðn. pottur",
                                "CAP\nleiðrétt", "Nettó staða\nbænda",
                                "Sparnaður\nneytenda")),
      value = c(-rs$total_loss_eur, -b$current_eur, ca$total, net, con$consumer_save_eur),
      fill  = c(col_red, col_anc, col_green,
                if (net >= 0) col_moss else col_gap, col_consumer)
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.55) +
      geom_hline(yintercept = 0, colour = col_brown, linewidth = 0.5) +
      geom_text(aes(label = sprintf("€%.0fM", value),
                    vjust = ifelse(value >= 0, -0.5, 1.5)),
                size = 3.8, fontface = "bold", colour = col_brown, family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"),
                         expand = expansion(mult = c(0.15, 0.15))) +
      labs(title = "Heildarjafnvægi bænda",
           subtitle = sprintf("Tap: €%.0fM (verð) + €%.0fM (stuðn.) = €%.0fM  |  CAP: €%.0fM  |  Nettó: %s€%.0fM",
                              rs$total_loss_eur, b$current_eur, total_farmer_loss,
                              ca$total,
                              if (net >= 0) "+" else "−", abs(net)),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        axis.text.x = element_text(size = 9, lineheight = 1.1)
      )
  }, res = 110, bg = "transparent")

  output$farm_projection <- renderPlot({
    fe <- farm_exit()
    df <- fe$farm_trajectory

    ggplot(df, aes(x = year, y = farms)) +
      geom_area(fill = col_red, alpha = 0.12) +
      geom_line(colour = col_red, linewidth = 1.2) +
      geom_point(data = df[df$year %in% c(0, 5, 10, 15), ],
                 colour = col_red, size = 3) +
      geom_text(data = df[df$year %in% c(0, 5, 10, 15), ],
                aes(label = format(farms, big.mark = ".")),
                vjust = -1.2, size = 3.5, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_x_continuous(breaks = c(0, 5, 10, 15),
                         labels = c("Dagur 1", "Ár 5", "Ár 10", "Ár 15")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = "."),
                         expand = expansion(mult = c(0.05, 0.15))) +
      labs(title = "Spá: fjöldi búa eftir aðild",
           subtitle = sprintf("Frá %s í %s á 10 árum  |  Byggð á reynslu Finnlands",
                              format(input$farms_now, big.mark = "."),
                              format(fe$farms_after, big.mark = ".")),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.3)
      )
  }, res = 110, bg = "transparent")

  output$consumer_breakdown_chart <- renderPlot({
    con <- consumer_savings()
    b   <- con$consumer_breakdown

    # Order: dairy at top, then meat subtypes grouped, then eggs, veg at bottom
    level_order <- rev(c("Mjólk & ostur", "Lambakjöt", "Nautakjöt", "Alifuglar", "Egg", "Grænmeti"))
    b$sector <- factor(b$sector, levels = level_order)

    fill_vals <- c(
      "Mjólk & ostur" = col_p1,
      "Lambakjöt"     = "#4A5C36",   # Highland Green (dark)
      "Nautakjöt"     = "#6B8B4A",   # mid green
      "Alifuglar"     = "#8FAF6A",   # light green
      "Egg"           = col_anc,
      "Grænmeti"      = col_moss
    )

    ggplot(b, aes(x = sector, y = savings, fill = sector)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = sprintf("%.1f ma.kr.  (Verð: -%.1f%% | Magn: %+.1f%%)",
                                    savings, drop, qty_shift)), 
                hjust = -0.05, size = 3.2, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = fill_vals) +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.55))) +
      coord_flip() +
      labs(title = "Sparnaður neytenda eftir flokkum",
           subtitle = sprintf("Samtals %.1f ma.kr./ár  |  Tekið tillit til verðteygni og víxlteygni (Magnbreyting sýnd)",
                              sum(b$savings)),
           x = NULL, y = "Ma.kr./ár", fill = NULL) +
      rekon_theme_flip(base_size = 12) +
      theme(legend.position = "none")
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # Farm-gate price share: value chain breakdown (Tab 4 — Advanced)
  # One donut per product showing producer share (dynamic) +
  # fixed value-chain components (slaughter, processing, retail, etc.)
  # Sources: AHDB 2024, USDA ERS, OECD Iceland 2025
  # ────────────────────────────────────────────────────────────────
  output$farm_share_pie <- renderPlot({
    farm_shares <- c(
      input$farm_share_dairy, input$farm_share_lamb, input$farm_share_beef,
      input$farm_share_poultry, input$farm_share_eggs, input$farm_share_veg
    )
    products <- c("Mjólk", "Lambakjöt", "Nautakjöt", "Alifuglar", "Egg", "Grænmeti")

    # Build long data frame
    rows <- list()
    for (i in seq_along(products)) {
      prod <- products[i]
      fs   <- farm_shares[i]
      rem  <- 100 - fs
      rows[[length(rows) + 1]] <- data.frame(
        product   = prod,
        component = "Framleiðandi",
        value     = fs
      )
      for (comp in names(CHAIN_SPLITS[[prod]])) {
        rows[[length(rows) + 1]] <- data.frame(
          product   = prod,
          component = comp,
          value     = rem * CHAIN_SPLITS[[prod]][[comp]]
        )
      }
    }
    df <- do.call(rbind, rows)
    df$product <- factor(df$product, levels = products)

    # Component colour palette — producer always green, chain steps grey shades
    comp_cols <- c(
      "Framleiðandi"              = col_green,
      "Vinnsla (Mjólkursamsalan)" = "#8FA8C8",
      "Vinnsla"                   = "#8FA8C8",
      "Vinnsla/Pökkun"            = "#8FA8C8",
      "Slátrun (SS/HB)"           = "#9AA5AF",
      "Slátrun"                   = "#9AA5AF",
      "Pökkun"                    = "#C0C8D0",
      "Dreifing"                  = "#D4D8DC",
      "Smásala"                   = "#EAECEE"
    )

    ggplot(df, aes(x = 2, y = value, fill = component)) +
      geom_col(width = 1, colour = "white", linewidth = 0.35) +
      geom_text(
        aes(label = ifelse(value >= 7, sprintf("%.0f%%", value), "")),
        position = position_stack(vjust = 0.5),
        size = 2.6, colour = "white", fontface = "bold", family = "Montserrat"
      ) +
      scale_fill_manual(values = comp_cols) +
      coord_polar(theta = "y", start = 0) +
      xlim(0.5, 2.5) +
      facet_wrap(~product, nrow = 2) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_void() +
      theme(
        legend.position   = "bottom",
        legend.text       = element_text(size = 8, family = "Montserrat", colour = col_grey),
        legend.key.size   = unit(0.55, "lines"),
        strip.text        = element_text(face = "bold", size = 10, colour = col_brown,
                                         family = "Montserrat", margin = margin(b = 4)),
        plot.background   = element_rect(fill = "transparent", colour = NA)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # Consumer drops summary table (Tab 4 — Advanced)
  # ────────────────────────────────────────────────────────────────
  output$consumer_drops_summary <- renderUI({
    rows <- list(
      list("Mjólk", input$dairy_drop, input$farm_share_dairy),
      list("Lambakjöt", input$lamb_drop, input$farm_share_lamb),
      list("Nautakjöt", input$beef_drop, input$farm_share_beef),
      list("Alifuglar", input$egg_drop, input$farm_share_poultry),
      list("Egg", input$egg_drop, input$farm_share_eggs),
      list("Grænmeti", input$veg_drop, input$farm_share_veg)
    )
    row_html <- paste(sapply(rows, function(r) {
      prod  <- r[[2]]
      share <- r[[3]]
      cons  <- round(prod * share / 100, 1)
      col   <- if (cons >= 15) col_red else if (cons >= 8) col_anc else col_green
      sprintf(
        '<tr style="border-bottom:1px solid #F3F4F6;">
           <td style="padding:5px 8px;font-size:12px;">%s</td>
           <td style="padding:5px 8px;text-align:right;font-size:12px;font-family:Montserrat;">%d%%</td>
           <td style="padding:5px 8px;text-align:right;font-size:12px;font-family:Montserrat;">%d%%</td>
           <td style="padding:5px 8px;text-align:right;font-weight:700;font-size:12px;font-family:Montserrat;color:%s;">%.1f%%</td>
         </tr>',
        r[[1]], prod, share, col, cons
      )
    }), collapse = "")
    HTML(sprintf(
      '<table style="width:100%%;border-collapse:collapse;margin-top:6px;">
         <thead>
           <tr style="background:#F3F4F6;">
             <th style="padding:5px 8px;text-align:left;font-size:11px;color:%s;">Vara</th>
             <th style="padding:5px 8px;text-align:right;font-size:11px;color:%s;">Framl.drop</th>
             <th style="padding:5px 8px;text-align:right;font-size:11px;color:%s;">Hlutfall</th>
             <th style="padding:5px 8px;text-align:right;font-size:11px;color:%s;">Neyt.drop</th>
           </tr>
         </thead>
         <tbody>%s</tbody>
       </table>',
      col_grey, col_grey, col_grey, col_grey, row_html
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION F: Winners/losers table
  # ────────────────────────────────────────────────────────────────

  output$winners_table <- renderUI({
    rs  <- revenue_shock()
    con <- consumer_savings()
    fe  <- farm_exit()
    ca <- cap_adjusted()
    b <- baseline()

    # Total farmer loss = revenue loss + lost Búvörusamningar
    total_farmer_loss <- rs$total_loss_eur + b$current_eur
    net_farmer <- ca$total - total_farmer_loss

    rows <- sprintf('
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Framleiðendur (bændur)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">−€%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">+€%.0fM</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">%s€%.0fM</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-style:italic;color:%s;">
          &nbsp;&nbsp;Þar af: tekjutap (verðlækkun)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">−€%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;"></td>
        <td style="padding:10px 14px;"></td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-style:italic;color:%s;">
          &nbsp;&nbsp;Þar af: tapaður Búvörusm. pottur</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">−€%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;"></td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Neytendur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">+€%.0fM</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-weight:600;">Ríkissjóður (tolltek.)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">−tolltekjur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Dreifbýli / byggðar</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">−%s bú</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">ANC stuðn.</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">—</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>',
      col_cloud,
      # Farmer total row
      col_red, total_farmer_loss,
      ca$total,
      if (net_farmer >= 0) col_green else col_red,
      if (net_farmer >= 0) "+" else "−",
      abs(net_farmer),
      if (net_farmer >= 0) badge("Jafnvægi", col_moss) else badge("Tap", col_red),
      # Sub-row: revenue loss from tariffs
      col_grey,
      col_red, rs$total_loss_eur,
      # Sub-row: lost Búvörusamningar pot
      col_grey,
      col_anc, b$current_eur,
      badge("Fastur pottur tapadur", col_anc),
      # Consumer row
      col_cloud,
      col_moss, con$consumer_save_eur, badge("Ávinningur", col_moss),
      # Treasury row
      col_red,
      badge("Hlutlægt/Tap", col_basalt),
      # Rural row
      col_cloud,
      format(fe$farms_lost, big.mark = "."),
      badge("Áhættulegt", col_red)
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      '<th style="padding:8px 14px;text-align:left;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Aðili</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Tekjutap</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">CAP bætur</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Nettó</th>',
      '<th style="padding:8px 14px;text-align:center;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Mat</th>',
      '</tr></thead><tbody>',
      rows,
      '</tbody></table>'
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION G: Detail breakdown table — Baseline vs Adjusted
  # ────────────────────────────────────────────────────────────────

  output$detail_table_html <- renderUI({
    b <- baseline()
    s <- shock()
    cb <- cap_baseline()
    ca <- cap_adjusted()

    badge_eu <- badge("ESB", col_p1)
    badge_is <- badge("Ísland", col_accent)
    badge_both <- badge("Samfjármögnun", col_anc)

    make_row <- function(label, base_val, adj_val, payer_badge, is_header) {
      bg <- if (is_header) sprintf(' style="background:%s;font-weight:600;"', col_cloud) else ""
      sprintf("<tr%s><td style='padding:7px 12px;font-family:Lora,serif;'>%s</td><td style='padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;font-weight:600;'>%s</td><td style='padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;font-weight:600;'>%s</td><td style='padding:7px 12px;text-align:center;'>%s</td></tr>",
              bg, label, base_val, adj_val, payer_badge)
    }

    rows <- paste0(
      make_row("Stoð 1: Beingreiðslur",
               sprintf("€%.1fM", cb$p1), sprintf("€%.1fM", ca$p1), badge_eu, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;%s ha × €%s/ha (grunnl.)",
                       format(b$ha, big.mark = "."), input$p1_rate),
               "", "", "", FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;%s ha × €%s/ha (leiðr.)",
                       format(round(s$new_ha), big.mark = "."), input$p1_rate),
               "", "", "", FALSE),
      make_row("", "", "", "", FALSE),
      make_row("Art. 142: Norðurslóðastuðningur",
               sprintf("€%.1fM", cb$nordic), sprintf("€%.1fM", ca$nordic), badge_is, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Mjólk: %.0fM L → %.0fM L",
                       b$milk, s$new_milk),
               sprintf("€%.1fM", cb$milk), sprintf("€%.1fM", ca$milk), badge_is, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Sauðfé: %s ær → %s",
                       format(b$ewes, big.mark = "."), format(round(s$new_ewes), big.mark = ".")),
               sprintf("€%.1fM", cb$sheep), sprintf("€%.1fM", ca$sheep), badge_is, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Nautgripir: %s → %s",
                       format(b$cattle, big.mark = "."), format(round(s$new_cattle), big.mark = ".")),
               sprintf("€%.1fM", cb$cattle), sprintf("€%.1fM", ca$cattle), badge_is, FALSE),
      make_row("", "", "", "", FALSE),
      make_row("Stoð 2: Harðbýlisgreiðslur (ANC)",
               sprintf("€%.1fM", cb$anc_total), sprintf("€%.1fM", ca$anc_total), badge_both, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Þar af ESB (%s%%)", input$eu_cofinance),
               sprintf("€%.1fM", cb$anc_eu), sprintf("€%.1fM", ca$anc_eu), badge_eu, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Þar af Ísland (%s%%)", 100 - input$eu_cofinance),
               sprintf("€%.1fM", cb$anc_is), sprintf("€%.1fM", ca$anc_is), badge_is, FALSE)
    )

    # Summary rows
    rs <- revenue_shock()
    total_farmer_loss <- rs$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    net_color <- if (net >= 0) col_green else col_red
    net_sign <- if (net >= 0) sprintf("+€%.1fM", net) else sprintf("−€%.1fM", abs(net))

    summary_rows <- sprintf('
      <tr style="border-top:2px solid %s;background:%s;font-weight:700;">
        <td style="padding:10px 12px;font-family:Montserrat,sans-serif;">SAMTALS CAP (nýtt kerfi)</td>
        <td style="padding:10px 12px;text-align:right;font-family:Montserrat,sans-serif;">€%.1fM</td>
        <td style="padding:10px 12px;text-align:right;font-family:Montserrat,sans-serif;">€%.1fM</td>
        <td style="padding:10px 12px;text-align:center;">%s &nbsp; %s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:7px 12px;font-family:Lora,serif;">Tapað núv. stuðn. (Búvörusm.)</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">−€%.1fM</td>
        <td style="padding:7px 12px;text-align:center;">%s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:7px 12px;font-family:Lora,serif;">Tekjutap (verðlækkun)</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">−€%.1fM</td>
        <td></td>
      </tr>
      <tr style="font-weight:600;background:%s;">
        <td style="padding:7px 12px;font-family:Montserrat,sans-serif;">HEILDARTAP BÆNDA</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">−€%.1fM</td>
        <td></td>
      </tr>
      <tr style="border-top:2px solid %s;font-weight:700;">
        <td style="padding:10px 12px;color:%s;font-family:Montserrat,sans-serif;">NETTÓ STAÐA (CAP − heildartap)</td>
        <td style="padding:10px 12px;text-align:right;color:%s;font-family:Montserrat,sans-serif;font-size:15px;" colspan="2">%s</td>
        <td></td>
      </tr>',
      col_brown, col_cloud,
      cb$total, ca$total,
      badge_eu, badge_is,
      col_anc, b$current_eur, badge_is,
      col_red, rs$total_loss_eur,
      col_cloud,
      col_red, total_farmer_loss,
      net_color, net_color, net_color, net_sign
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      '<th style="padding:8px 12px;text-align:left;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Liður</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Grunnlína €M</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Leiðrétt €M</th>',
      '<th style="padding:8px 12px;text-align:center;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Greiðandi</th>',
      '</tr></thead><tbody>',
      rows,
      summary_rows,
      '</tbody></table>'
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # NAVIGATION BUTTONS
  # ────────────────────────────────────────────────────────────────

  observeEvent(input$go_results_2, {
    updateNavbarPage(session, "main_nav", selected = "results")
  })

  observeEvent(input$go_results_3, {
    updateNavbarPage(session, "main_nav", selected = "results")
  })

  observeEvent(input$go_results_4, {
    updateNavbarPage(session, "main_nav", selected = "results")
  })

  # ────────────────────────────────────────────────────────────────
  # MINI OUTPUTS FOR TAB 2
  # ────────────────────────────────────────────────────────────────

  output$mini_cap_baseline <- renderUI({
    cb <- cap_baseline()
    div(class = "metric-box cap",
      div(class = "metric-label", "CAP Grunnlína"),
      div(class = "metric-value", style = paste0("color:", col_p1), sprintf("€%.0fM", cb$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey), "Fyrir framleiðslusamdrátt")
    )
  })

  output$mini_cap_adjusted <- renderUI({
    ca <- cap_adjusted()
    cb <- cap_baseline()
    diff_pct <- (ca$total - cb$total) / cb$total * 100
    div(class = "metric-box cap",
      div(class = "metric-label", "CAP Leiðrétt"),
      div(class = "metric-value", style = paste0("color:", col_green), sprintf("€%.0fM", ca$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f%% breyting", diff_pct))
    )
  })

  output$mini_net <- renderUI({
    ca <- cap_adjusted()
    rs <- revenue_shock()
    b  <- baseline()
    total_farmer_loss <- rs$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    col <- if (net >= 0) col_green else col_red
    sign <- if (net >= 0) "+" else "−"
    div(class = "metric-box gap",
      div(class = "metric-label", "Nettó staða"),
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%s€%.0fM", sign, abs(net))),
      div(class = "metric-sub", style = paste0("color:", col_grey), "CAP vs. heildartap")
    )
  })

  output$mini_eu_vs_is <- renderUI({
    ca <- cap_adjusted()
    total <- ca$eu_pays + ca$iceland_pays
    eu_pct <- round(ca$eu_pays / total * 100)
    div(class = "metric-box eu",
      div(class = "metric-label", "ESB hlutfall"),
      div(class = "metric-value", style = paste0("color:", col_p1), sprintf("%s%%", eu_pct)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("ESB: €%.0fM | Ís: €%.0fM", ca$eu_pays, ca$iceland_pays))
    )
  })

  output$mini_payer_bar <- renderUI({
    ca <- cap_adjusted()
    total <- ca$eu_pays + ca$iceland_pays
    eu_pct <- round(ca$eu_pays / total * 100)
    is_pct <- 100 - eu_pct
    HTML(sprintf('
      <div style="margin:18px 0 8px 0; font-family:Montserrat,sans-serif;">
        <div style="font-size:13px;font-weight:600;color:%s;margin-bottom:8px;">
          Hver borgar? (leiðrétt) &mdash; ESB: €%.0fM (%s%%) &nbsp;|&nbsp; Ísland: €%.0fM (%s%%)
        </div>
        <div style="display:flex;height:32px;border-radius:6px;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,0.08);">
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            ESB €%.0fM
          </div>
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            Ísland €%.0fM
          </div>
        </div>
      </div>',
      col_brown,
      ca$eu_pays, eu_pct, ca$iceland_pays, is_pct,
      eu_pct, col_p1, ca$eu_pays,
      is_pct, col_accent, ca$iceland_pays
    ))
  })

  output$mini_cap_chart <- renderPlot({
    cb <- cap_baseline()
    ca <- cap_adjusted()
    b <- baseline()

    labels <- c("Stoð 1\nGrunnl.", "Stoð 1\nLeiðr.",
                "Art. 142\nGrunnl.", "Art. 142\nLeiðr.",
                "ANC\nGrunnl.", "ANC\nLeiðr.",
                "Samtals\nGrunnl.", "Samtals\nLeiðr.")

    values <- c(cb$p1, ca$p1,
                cb$nordic, ca$nordic,
                cb$anc_total, ca$anc_total,
                cb$total, ca$total)

    fills <- c(col_before, col_p1,
               col_before, col_green,
               col_before, col_anc,
               col_before, col_accent)

    df <- data.frame(
      label = factor(labels, levels = labels),
      value = values,
      fill = fills
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = b$current_eur, linetype = "dashed", colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 8.4, y = b$current_eur + 2,
               label = sprintf("Núverandi: €%.0fM", b$current_eur),
               hjust = 1, size = 2.8, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_text(aes(label = sprintf("€%.0fM", value)),
                vjust = -0.5, size = 2.8, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = "CAP stuðningur — Grunnlína vs. Leiðrétt",
           subtitle = sprintf("Grunnlína: €%.0fM  |  Leiðrétt: €%.0fM  |  Munur: −€%.0fM",
                              cb$total, ca$total, cb$total - ca$total),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 11) +
      theme(axis.text.x = element_text(size = 8, lineheight = 1.1))
  }, res = 110, bg = "transparent")

}

shinyApp(ui, server)