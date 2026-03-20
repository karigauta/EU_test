library(shiny)
library(ggplot2)
library(scales)
# ══════════════════════════════════════════════════════════════════
# INTEGRATED CAP + TARIFF IMPACT MODEL FOR ICELAND
# Rekon / Kári Gautason — March 2026
#
# A unified economic model:
#   Baseline production → Price shock (tariff removal) →
#   Supply response (elasticity) → Adjusted production →
#   CAP payments on adjusted base → Net impact
#
# Key references:
#   Kola, Hofreither & Rabinowicz (2000) — Nordic accession impacts
#   Niemi (2003) — Static welfare effects on Finnish agriculture
#   OECD PSE Iceland (2025) — PSE 47%, prices 60% above border
#   EU Evaluation of Nordic Aid Schemes (2007)
#   Hagstofa Íslands, LbhÍ Rit 179 (2025)
#   Jongeneel & Tonini (2008) — EU milk supply elasticities
#   Revell & Oglethorpe (2003) — UK sheep supply response
# ══════════════════════════════════════════════════════════════════

# ── Rekon Brand Palette ──────────────────────────────────────────
col_sky      <- "#4A90E2"
col_green    <- "#4A5C36"
col_cloud    <- "#F3F4F6"
col_grey     <- "#6B7280"
col_brown    <- "#57534E"
col_red      <- "#A12B26"
col_p1       <- "#4A90E2"
col_nordic   <- "#4A5C36"
col_anc      <- "#E2B14A"
col_gap      <- "#A12B26"
col_accent   <- "#57534E"
col_basalt   <- "#5D6D7E"
col_moss     <- "#789A5B"
col_consumer <- "#2E86AB"
col_before   <- "#D1D5DB"

rekon_theme <- function(base_size = 13) {
  theme_minimal(base_family = "Montserrat", base_size = base_size) %+replace%
    theme(
      plot.title = element_text(face = "bold", colour = col_brown, size = 16, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = col_grey, size = 11, family = "Lora", margin = margin(b = 12)),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )
}

# ── CSS ──────────────────────────────────────────────────────────
app_css <- sprintf("
  body { font-family: 'Lora', Georgia, serif; background: %s; color: %s; font-size: 13px; }
  h1,h2,h3,h4,h5,.metric-label,.metric-value,.metric-sub,
  .brand-bar,.brand-title,.brand-sub,th,.nav-tabs>li>a,
  .section-title { font-family: 'Montserrat', Arial, sans-serif; }
  h3 { color: %s; font-weight: 700; }
  h4 { color: %s; font-weight: 600; margin-top: 20px; margin-bottom: 10px; }
  .well { background: #fff; border: 1px solid #E5E7EB; border-radius: 8px; }

  .brand-bar {
    background: %s; color: #fff; padding: 18px 28px;
    border-radius: 10px; margin-bottom: 22px;
    display: flex; align-items: center; justify-content: space-between;
  }
  .brand-title { font-size: 22px; font-weight: 700; letter-spacing: 0.5px; }
  .brand-title span { font-weight: 400; opacity: 0.85; font-size: 15px; margin-left: 10px; }
  .brand-sub { font-size: 12px; opacity: 0.8; }

  /* Tabs */
  .nav-tabs { border-bottom: 2px solid %s; margin-bottom: 18px; }
  .nav-tabs>li>a {
    color: %s; font-weight: 600; font-size: 13px;
    border: none; border-bottom: 3px solid transparent;
    padding: 10px 18px; margin-bottom: -2px;
  }
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover {
    color: %s; border-bottom: 3px solid %s; background: transparent;
  }

  .metric-box {
    background: #fff; border-radius: 8px; padding: 16px 18px; margin-bottom: 12px;
    border: 1px solid #E5E7EB; border-left: 4px solid %s;
    box-shadow: 0 1px 3px rgba(0,0,0,0.04);
  }
  .metric-box.sky   { border-left-color: %s; }
  .metric-box.green { border-left-color: %s; }
  .metric-box.red   { border-left-color: %s; }
  .metric-box.gold  { border-left-color: %s; }
  .metric-box.moss  { border-left-color: %s; }
  .metric-box.basalt{ border-left-color: %s; }
  .metric-label {
    font-size: 11px; color: %s; text-transform: uppercase;
    letter-spacing: 0.5px; font-weight: 600; margin-bottom: 4px;
  }
  .metric-value { font-size: 26px; font-weight: 700; color: %s; }
  .metric-sub { font-size: 11px; font-weight: 600; margin-top: 2px; }

  .section-divider { border: none; border-top: 2px solid %s; margin: 24px 0; }
  .source-note { font-family: 'Lora', serif; font-size: 11px; color: %s; line-height: 1.6; margin-top: 10px; }
  .help-block { font-family: 'Lora', serif; font-size: 11px; color: %s; }
  .sidebar-section {
    font-family: 'Montserrat', sans-serif; font-size: 13px; font-weight: 600;
    color: %s; padding: 6px 0 4px 0; margin-top: 16px;
    border-bottom: 2px solid %s; margin-bottom: 10px;
  }
  .context-box {
    background: #fff; border: 1px solid #E5E7EB; border-left: 4px solid %s;
    border-radius: 6px; padding: 14px 18px; margin: 14px 0;
    font-size: 12px; line-height: 1.6;
  }
  .context-box strong { color: %s; }
  .flow-arrow {
    text-align: center; font-size: 24px; color: %s;
    margin: 6px 0; font-family: Montserrat, sans-serif;
  }
  .section-title {
    font-size: 15px; font-weight: 700; color: %s;
    border-left: 4px solid %s; padding-left: 12px;
    margin: 24px 0 14px 0;
  }
",
  col_cloud, col_brown,
  col_brown, col_brown,
  col_accent,
  col_p1, col_grey, col_brown, col_p1,
  col_grey,
  col_p1, col_green, col_red, col_anc, col_moss, col_basalt,
  col_grey, col_brown,
  col_cloud, col_grey, col_grey,
  col_brown, col_p1,
  col_basalt, col_brown,
  col_p1,
  col_brown, col_p1
)

# ══════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&family=Lora:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(app_css))
  ),

  div(class = "brand-bar",
    div(
      div(class = "brand-title", "rekon", span("| Landbúnaðarlíkan ESB-aðildar")),
      div(class = "brand-sub", "Samþætt líkan: tollaáhrif \u2192 framleiðslubreyting \u2192 CAP-stuðningur \u2192 nettóstaða")
    ),
    div(style = "text-align:right;",
      div(style = "font-size:11px; opacity:0.7;", "Kári Gautason — mars 2026"),
      div(style = "font-size:11px; opacity:0.7;", "Grounded Analysis & Strategic Insight")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      div(class = "context-box",
        HTML(paste0(
          "<strong>Samþætt líkan</strong><br>",
          "Þetta líkan tengir saman tollaáhrif og CAP-stuðning. ",
          "Verðlækkun \u2192 framleiðsluviðbrögð (framboðsteygni) \u2192 ",
          "nýr fjöldi búfjár og mjólkurframleiðsla \u2192 ",
          "CAP-greiðslur reiknaðar á leiðréttum grunni.<br><br>",
          "<em>OECD PSE Íslands: 47%. Verðlag 60% yfir heimsmarkaði.</em>"
        ))
      ),

      # ═══════════════════════════════════════
      # SECTION 1: BASELINE
      # ═══════════════════════════════════════
      div(class = "sidebar-section", "\u2460 Grunnstaða Íslands í dag"),

      numericInput("eur_isk", "Gengi EUR/ISK", value = 143, min = 100, max = 200, step = 1),

      sliderInput("base_milk_litres", "Mjólkurframleiðsla (M lítr.)",
                  min = 120, max = 170, value = 145, step = 5, post = " M L"),
      sliderInput("base_ewes", "Ær (fjöldi)",
                  min = 200000, max = 500000, value = 400000, step = 10000),
      sliderInput("base_cattle", "Nautgripir (fjöldi)",
                  min = 20000, max = 50000, value = 31000, step = 1000),
      sliderInput("base_egg_val", "Egg/alifuglar — verðmæti (ma.kr.)",
                  min = 0.5, max = 5, value = 2, step = 0.5, post = " ma.kr."),
      sliderInput("base_veg_val", "Grænmeti/gróðurhús — verðmæti (ma.kr.)",
                  min = 1, max = 8, value = 4, step = 0.5, post = " ma.kr."),

      sliderInput("eligible_ha", "Styrkhæft land (ha)",
                  min = 80000, max = 400000, value = 200000, step = 10000, post = " ha"),
      helpText("120k ræktað + 80k viðhaldið tún = 200k. Meira ef afréttir."),

      sliderInput("current_support_isk", "Núverandi stuðningur samtals (ma.kr.)",
                  min = 14, max = 30, value = 18, step = 0.5, post = " ma.kr."),
      helpText("Búvörusamningar."),

      sliderInput("base_farms", "Fjöldi búa í dag",
                  min = 1500, max = 4000, value = 2500, step = 100),

      hr(),

      # ═══════════════════════════════════════
      # SECTION 2: PRICE SHOCK
      # ═══════════════════════════════════════
      div(class = "sidebar-section", "\u2461 Verðáhrif tollaafnáms (% lækkun)"),
      helpText("Byggð á reynslu Finnlands 1995. Hægt að aðlaga."),

      sliderInput("drop_dairy", "Mjólk/mjólkurvörur",
                  min = 10, max = 50, value = 30, step = 1, post = "%"),
      helpText("Finnland: \u221228% til \u221232%."),

      sliderInput("drop_lamb", "Kindakjöt",
                  min = 15, max = 60, value = 45, step = 1, post = "%"),
      helpText("Finnland kjöt: \u221238–43%. Lamb sennilega hærra (einangrað)."),

      sliderInput("drop_beef", "Nautakjöt",
                  min = 15, max = 55, value = 40, step = 1, post = "%"),
      helpText("Finnland: \u221238% til \u221243%."),

      sliderInput("drop_egg", "Egg og alifuglar",
                  min = 20, max = 70, value = 55, step = 1, post = "%"),
      helpText("Finnland: \u221265–68%. Ísland líklega lægra v/ flutnkostn."),

      sliderInput("drop_veg", "Grænmeti/gróðurhús",
                  min = 5, max = 40, value = 20, step = 1, post = "%"),
      helpText("Gróðurhús m/ jarðvarma: samkeppnishæf. Minni áhrif."),

      hr(),

      # ═══════════════════════════════════════
      # SECTION 3: SUPPLY ELASTICITIES
      # ═══════════════════════════════════════
      div(class = "sidebar-section", "\u2462 Framboðsteygni (supply elasticity)"),
      helpText("Hversu mikið framleiðsla minnkar þegar verð lækkar. 0.5 = 10% verðlækkun → 5% framleiðsluminnkun."),

      sliderInput("elas_dairy", "Mjólk (langtíma)",
                  min = 0.1, max = 0.8, value = 0.4, step = 0.05),
      helpText("Jongeneel & Tonini (2008): 0.3–0.5. Mjólk er tiltölulega óteygin."),

      sliderInput("elas_sheep", "Sauðfé (langtíma)",
                  min = 0.2, max = 1.0, value = 0.6, step = 0.05),
      helpText("Revell & Oglethorpe (2003): 0.4–0.7. Auðvelt að skala niður."),

      sliderInput("elas_cattle", "Nautgripir (langtíma)",
                  min = 0.1, max = 0.8, value = 0.4, step = 0.05),
      helpText("Tengist mjólkurfrágangi. Svipaðar tölur og mjólk."),

      sliderInput("elas_egg", "Egg/alifuglar",
                  min = 0.2, max = 1.0, value = 0.7, step = 0.05),
      helpText("Tiltölulega hár teygni (lítill fastur kostnaður)."),

      sliderInput("elas_veg", "Grænmeti/gróðurhús",
                  min = 0.1, max = 0.6, value = 0.3, step = 0.05),
      helpText("Jarðvarmi gefur varanlegri samkeppnisforskot."),

      hr(),

      # ═══════════════════════════════════════
      # SECTION 4: CAP RATES
      # ═══════════════════════════════════════
      div(class = "sidebar-section", "\u2463 CAP-greiðslur (á einingu)"),

      sliderInput("p1_rate", "Stoð 1: Beingreiðslur (€/ha)",
                  min = 100, max = 400, value = 200, step = 10, pre = "€"),
      helpText("EU lágmark: €200 (2023), €215 (2027). Meðaltal: ~€243."),

      sliderInput("milk_rate", "Art. 142: Mjólkurgreiðsla (€/lítra)",
                  min = 0.04, max = 0.15, value = 0.09, step = 0.01, pre = "€"),
      helpText("Finnland: €0.07 (suður) – €0.11 (Lappland)."),

      sliderInput("ewe_rate", "Art. 142: Greiðsla á á (€/kind)",
                  min = 10, max = 40, value = 25, step = 1, pre = "€"),
      helpText("ESB hámark: €28/kind."),

      sliderInput("cattle_rate", "Art. 142: Greiðsla á grip (€/grip)",
                  min = 100, max = 400, value = 200, step = 10, pre = "€"),
      helpText("Finnland: €150–350. Meðaltal ~€200."),

      sliderInput("anc_rate", "ANC greiðsla á hektara (€/ha)",
                  min = 100, max = 450, value = 250, step = 10, pre = "€"),
      helpText("Hámark norðan 62°: €450/ha. Finnl. meðaltal: ~€217."),

      sliderInput("eu_cofinance", "ESB-hlutfall af ANC (%)",
                  min = 30, max = 75, value = 55, step = 5, post = "%"),

      hr(),

      # ═══════════════════════════════════════
      # SECTION 5: STRUCTURAL / CONSUMER
      # ═══════════════════════════════════════
      div(class = "sidebar-section", "\u2464 Neytendur og bygg\u00f0ir"),

      sliderInput("land_abandon_factor", "Landnotkun sem helst (%)",
                  min = 50, max = 100, value = 80, step = 5, post = "%"),
      helpText("Hluti lands helst í notkun þrátt fyrir fækkun búfjár (ANC hvetur)."),

      sliderInput("food_spend", "Matarútgjöld heimila (ma.kr./ár)",
                  min = 100, max = 250, value = 170, step = 10, post = " ma.kr."),
      sliderInput("consumer_price_drop", "Áætluð matvælaverðlækkun (%)",
                  min = 5, max = 30, value = 15, step = 1, post = "%"),
      helpText("Finnland: \u221211%. Ísland líklega meira (48% yfir ESB-meðaltali)."),

      sliderInput("transition_years", "Aðlögunartímabil (ár)",
                  min = 3, max = 15, value = 10, step = 1, post = " ár"),
      helpText("Finnland fékk 5 ár. Smærri ríki fá oft lengri aðlögun."),

      hr(),
      p(class = "source-note",
        "Höfundur: Rekon / Kári Gautason. Heimildir: OECD Agricultural Policy Monitoring (2025), ",
        "Kola, Hofreither & Rabinowicz (2000), Niemi (2003), ",
        "Jongeneel & Tonini (2008), Revell & Oglethorpe (2003), ",
        "EU Evaluation of Nordic Aid (2007), Eurostat (2024), ",
        "Hagstofa Íslands, LbhÍ Rit 179 (2025).")
    ),

    # ═══════════════════════════════════════════════════════════════
    # MAIN PANEL
    # ═══════════════════════════════════════════════════════════════
    mainPanel(
      width = 8,

      # ── HEADLINE METRICS ──
      fluidRow(
        column(3, div(class = "metric-box sky",
          div(class = "metric-label", "Núverandi stuðningur"),
          uiOutput("m_current"))),
        column(3, div(class = "metric-box red",
          div(class = "metric-label", "Tekjutap (verðáhrif)"),
          uiOutput("m_revenue_loss"))),
        column(3, div(class = "metric-box green",
          div(class = "metric-label", "CAP stuðningur (nýr)"),
          uiOutput("m_cap_adjusted"))),
        column(3, div(class = "metric-box basalt",
          div(class = "metric-label", "Nettóstaða"),
          uiOutput("m_net")))
      ),

      # ── SECONDARY METRICS ──
      fluidRow(
        column(3, div(class = "metric-box moss",
          div(class = "metric-label", "Sparnaður neytenda"),
          uiOutput("m_consumer"))),
        column(3, div(class = "metric-box gold",
          div(class = "metric-label", "Leiðrétt land (ha)"),
          uiOutput("m_adj_ha"))),
        column(3, div(class = "metric-box basalt",
          div(class = "metric-label", "Bú eftir aðlögun"),
          uiOutput("m_farms"))),
        column(3, div(class = "metric-box sky",
          div(class = "metric-label", "Hver borgar CAP"),
          uiOutput("m_payer")))
      ),

      # ── MODEL FLOW DIAGRAM ──
      div(class = "flow-arrow", HTML(
        "\u2460 Grunnstaða &nbsp;\u2192&nbsp; \u2461 Verðsokk &nbsp;\u2192&nbsp; \u2462 Framleiðslubreyting &nbsp;\u2192&nbsp; \u2463 CAP á nýjum grunni &nbsp;\u2192&nbsp; \u2464 Nettó"
      )),

      hr(class = "section-divider"),

      # ════════════════════════════════════════
      # SECTION A: PRODUCTION ADJUSTMENT
      # ════════════════════════════════════════
      div(class = "section-title", "\u2461\u2192\u2462 Framleiðsluviðbrögð: Verðsokk og framboðsteygni"),

      fluidRow(
        column(6, plotOutput("chart_production_shift", height = "400px")),
        column(6, plotOutput("chart_livestock_before_after", height = "400px"))
      ),

      hr(class = "section-divider"),

      # ════════════════════════════════════════
      # SECTION B: CAP ON ADJUSTED BASE
      # ════════════════════════════════════════
      div(class = "section-title", "\u2463 CAP-stuðningur á leiðréttum framleiðslugrunni"),

      plotOutput("chart_cap_waterfall", height = "400px"),

      # ── Who pays bar ──
      uiOutput("payer_bar_html"),

      hr(class = "section-divider"),

      # ════════════════════════════════════════
      # SECTION C: NET IMPACT
      # ════════════════════════════════════════
      div(class = "section-title", "\u2464 Heildarmynd: Tap, stuðningur og nettóstaða"),

      fluidRow(
        column(6, plotOutput("chart_net_balance", height = "380px")),
        column(6, plotOutput("chart_farm_trajectory", height = "380px"))
      ),

      hr(class = "section-divider"),

      # ════════════════════════════════════════
      # SECTION D: DETAILED TABLE
      # ════════════════════════════════════════
      div(class = "section-title", "Sundurliðun — Samanburður fyrir og eftir aðild"),

      uiOutput("detail_table_html"),

      hr(class = "section-divider"),

      # ════════════════════════════════════════
      # SECTION E: WINNERS / LOSERS
      # ════════════════════════════════════════
      div(class = "section-title", "Hver hagnast? Hver tapar?"),

      uiOutput("winners_table_html"),

      p(class = "source-note",
        "Athugasemd: Líkanið er einfaldað og byggir á stöðukomfræði (comparative statics). ",
        "Raunveruleg áhrif fara eftir aðlögunartíma, sérákvæðum í samningi, ",
        "og viðbrögðum framleiðenda. Framboðsteygni miðast við langtímaviðbrögð (5–10 ár). ",
        "Neytendaáhrif geta verið hraðari. ",
        "Finnland fékk 5 ára aðlögunartímabil og sérstakan stuðningspakka.")
    )
  )
)

# ══════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ────────────────────────────────────────────────────────────────
  # CORE MODEL: Everything flows from here
  # ────────────────────────────────────────────────────────────────

  model <- reactive({
    fx <- input$eur_isk
    current_support_eur <- input$current_support_isk * 1000 / fx  # €M

    # ── BASELINE (before accession) ──
    # Estimate sector revenue at current protected ISK prices
    # Milk: ~19 ma.kr based on 145M L at ~130 ISK/L
    milk_price_isk_per_L <- 131   # avg farm-gate ISK/L (Hagstofa)
    dairy_val_isk <- input$base_milk_litres * milk_price_isk_per_L / 1000  # ma.kr.

    lamb_price_isk_per_kg <- 1100  # ~ISK/kg carcass weight
    avg_carcass_kg <- 18
    lamb_val_isk <- input$base_ewes * avg_carcass_kg * lamb_price_isk_per_kg / 1e9  # ma.kr. (not all ewes slaughtered, ~60% lambs)
    # Simpler: use ~17 ISK per ewe as total revenue proxy
    lamb_val_isk <- input$base_ewes * 17500 / 1e6  # ma.kr.

    beef_price_isk_per_kg <- 1400
    avg_beef_yield_kg <- 280
    # ~10% of cattle herd slaughtered/year, plus dairy bull calves
    beef_val_isk <- input$base_cattle * 0.35 * avg_beef_yield_kg * beef_price_isk_per_kg / 1e9 * 1000 # ma.kr.
    beef_val_isk <- input$base_cattle * 100000 / 1e6  # simplified: ~100k ISK revenue per head

    egg_val_isk <- input$base_egg_val
    veg_val_isk <- input$base_veg_val

    sectors <- data.frame(
      sector = c("Mjólk", "Kindakjöt", "Nautakjöt", "Egg/alifuglar", "Grænmeti"),
      sector_short = c("Mjólk", "Lamb", "Naut", "Egg", "Græn"),
      val_isk = c(dairy_val_isk, lamb_val_isk, beef_val_isk, egg_val_isk, veg_val_isk),
      drop_pct = c(input$drop_dairy, input$drop_lamb, input$drop_beef,
                   input$drop_egg, input$drop_veg),
      elasticity = c(input$elas_dairy, input$elas_sheep, input$elas_cattle,
                     input$elas_egg, input$elas_veg),
      stringsAsFactors = FALSE
    )

    sectors$val_eur <- sectors$val_isk * 1000 / fx

    # ── SUPPLY RESPONSE ──
    # Production change = -drop% × elasticity
    # E.g., 30% price drop × 0.4 elasticity = 12% production decline
    sectors$prod_change_pct <- -sectors$drop_pct * sectors$elasticity / 100
    sectors$prod_multiplier <- 1 + sectors$prod_change_pct

    # Revenue after = volume × (1 + prod_change) × price × (1 - drop)
    sectors$revenue_after_eur <- sectors$val_eur * sectors$prod_multiplier * (1 - sectors$drop_pct / 100)
    sectors$revenue_loss_eur  <- sectors$val_eur - sectors$revenue_after_eur

    total_before_eur <- sum(sectors$val_eur)
    total_after_eur  <- sum(sectors$revenue_after_eur)
    total_loss_eur   <- sum(sectors$revenue_loss_eur)

    # ── ADJUSTED PRODUCTION VOLUMES ──
    adj_milk   <- input$base_milk_litres * (1 + sectors$prod_change_pct[1])
    adj_ewes   <- round(input$base_ewes * (1 + sectors$prod_change_pct[2]))
    adj_cattle <- round(input$base_cattle * (1 + sectors$prod_change_pct[3]))

    # Land: partially abandons, but ANC incentivizes keeping land
    livestock_decline_avg <- mean(c(
      sectors$prod_change_pct[1],
      sectors$prod_change_pct[2],
      sectors$prod_change_pct[3]
    ))
    land_retention <- input$land_abandon_factor / 100
    adj_ha <- round(input$eligible_ha * (1 + livestock_decline_avg * (1 - land_retention)))

    # ── CAP PAYMENTS ON ADJUSTED BASE ──
    # Pillar 1: based on adjusted land
    p1 <- adj_ha * input$p1_rate / 1e6

    # Nordic Aid (Art. 142): based on adjusted production
    nordic_milk  <- adj_milk * 1e6 * input$milk_rate / 1e6
    nordic_sheep <- adj_ewes * input$ewe_rate / 1e6
    nordic_cows  <- adj_cattle * input$cattle_rate / 1e6
    nordic_total <- nordic_milk + nordic_sheep + nordic_cows

    # ANC: based on adjusted land
    anc_total <- adj_ha * input$anc_rate / 1e6
    anc_eu    <- anc_total * input$eu_cofinance / 100
    anc_is    <- anc_total * (1 - input$eu_cofinance / 100)

    cap_total <- p1 + nordic_total + anc_total

    # Who pays
    eu_pays      <- p1 + anc_eu
    iceland_pays <- nordic_total + anc_is

    # ── WHAT IF NO DECLINE (for comparison) ──
    p1_max         <- input$eligible_ha * input$p1_rate / 1e6
    nordic_max     <- (input$base_milk_litres * 1e6 * input$milk_rate +
                       input$base_ewes * input$ewe_rate +
                       input$base_cattle * input$cattle_rate) / 1e6
    anc_max        <- input$eligible_ha * input$anc_rate / 1e6
    cap_max        <- p1_max + nordic_max + anc_max

    # ── NET IMPACT ──
    net_farmer <- cap_total - total_loss_eur
    cap_covers_pct <- cap_total / total_loss_eur * 100

    # ── CONSUMER SAVINGS ──
    consumer_save_isk <- input$food_spend * input$consumer_price_drop / 100
    consumer_save_eur <- consumer_save_isk * 1000 / fx

    # ── FARM TRAJECTORY ──
    # Farm exits driven by: revenue loss not covered by CAP
    # Use weighted sector decline as proxy for farm exit
    farm_exit_rate <- -livestock_decline_avg  # base annual-ish
    # Scale: if CAP covers a lot, fewer farms exit
    coverage_dampener <- min(1, max(0.3, 1 - cap_covers_pct / 200))
    effective_annual_exit <- farm_exit_rate * coverage_dampener
    # Clamp to reasonable range
    effective_annual_exit <- max(0.01, min(0.08, effective_annual_exit))

    years <- 0:15
    farm_trajectory <- data.frame(
      year = years,
      farms = round(input$base_farms * (1 - effective_annual_exit)^years)
    )
    farms_after <- farm_trajectory$farms[farm_trajectory$year == input$transition_years]

    list(
      fx = fx,
      current_support_eur = current_support_eur,
      sectors = sectors,
      total_before_eur = total_before_eur,
      total_after_eur = total_after_eur,
      total_loss_eur = total_loss_eur,
      adj_milk = adj_milk, adj_ewes = adj_ewes, adj_cattle = adj_cattle,
      adj_ha = adj_ha,
      p1 = p1, nordic_milk = nordic_milk, nordic_sheep = nordic_sheep,
      nordic_cows = nordic_cows, nordic_total = nordic_total,
      anc_total = anc_total, anc_eu = anc_eu, anc_is = anc_is,
      cap_total = cap_total, cap_max = cap_max,
      eu_pays = eu_pays, iceland_pays = iceland_pays,
      net_farmer = net_farmer, cap_covers_pct = cap_covers_pct,
      consumer_save_eur = consumer_save_eur, consumer_save_isk = consumer_save_isk,
      farm_trajectory = farm_trajectory,
      farms_after = farms_after,
      effective_annual_exit = effective_annual_exit
    )
  })

  # ────────────────────────────────────────────────────────────────
  # METRIC CARDS
  # ────────────────────────────────────────────────────────────────

  output$m_current <- renderUI({
    m <- model()
    tagList(
      div(class = "metric-value", sprintf("\u20ac%.0fM", m$current_support_eur)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f ma.kr.", input$current_support_isk))
    )
  })

  output$m_revenue_loss <- renderUI({
    m <- model()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("\u2212\u20ac%.0fM", m$total_loss_eur)),
      div(class = "metric-sub", style = paste0("color:", col_red),
          sprintf("%.0f%% af framleiðsluverðmæti", m$total_loss_eur / m$total_before_eur * 100))
    )
  })

  output$m_cap_adjusted <- renderUI({
    m <- model()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_green),
          sprintf("\u20ac%.0fM", m$cap_total)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("(hefði verið \u20ac%.0fM án samdráttar)", m$cap_max))
    )
  })

  output$m_net <- renderUI({
    m <- model()
    col <- if (m$net_farmer >= 0) col_green else col_red
    sign <- if (m$net_farmer >= 0) "+" else "\u2212"
    tagList(
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%s\u20ac%.0fM", sign, abs(m$net_farmer))),
      div(class = "metric-sub", style = paste0("color:", col),
          sprintf("CAP dekkar %.0f%% tapsins", m$cap_covers_pct))
    )
  })

  output$m_consumer <- renderUI({
    m <- model()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_moss),
          sprintf("+\u20ac%.0fM", m$consumer_save_eur)),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("%.0f ma.kr./ár lægra matvælaverð", m$consumer_save_isk))
    )
  })

  output$m_adj_ha <- renderUI({
    m <- model()
    change_pct <- (m$adj_ha - input$eligible_ha) / input$eligible_ha * 100
    tagList(
      div(class = "metric-value", format(m$adj_ha, big.mark = ".")),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f%% af upphaflegu (%s ha)", 100 + change_pct, format(input$eligible_ha, big.mark = ".")))
    )
  })

  output$m_farms <- renderUI({
    m <- model()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          format(m$farms_after, big.mark = ".")),
      div(class = "metric-sub", style = paste0("color:", col_red),
          sprintf("\u2212%s bú á %s árum (%.1f%%/ár)",
                  format(input$base_farms - m$farms_after, big.mark = "."),
                  input$transition_years,
                  m$effective_annual_exit * 100))
    )
  })

  output$m_payer <- renderUI({
    m <- model()
    eu_pct <- round(m$eu_pays / m$cap_total * 100)
    tagList(
      div(class = "metric-value", sprintf("%.0f/%.0f", eu_pct, 100 - eu_pct)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("ESB \u20ac%.0fM / Ísland \u20ac%.0fM", m$eu_pays, m$iceland_pays))
    )
  })

  # ────────────────────────────────────────────────────────────────
  # CHART: Production shift (before/after by sector)
  # ────────────────────────────────────────────────────────────────

  output$chart_production_shift <- renderPlot({
    m <- model()
    s <- m$sectors

    df <- data.frame(
      sector = factor(rep(s$sector, 2), levels = rev(s$sector)),
      type = factor(rep(c("Fyrir aðild", "Eftir aðild"), each = nrow(s)),
                    levels = c("Fyrir aðild", "Eftir aðild")),
      value = c(s$val_eur, s$revenue_after_eur)
    )

    ann <- data.frame(
      sector = factor(s$sector, levels = rev(s$sector)),
      y_pos = s$val_eur,
      label = sprintf("\u2212%.0f%%\nverð", s$drop_pct),
      label2 = sprintf("\u2212%.0f%%\nmagn", abs(s$prod_change_pct * 100))
    )

    ggplot(df, aes(x = sector, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = ann,
                aes(x = sector, y = y_pos * 1.02, label = label),
                inherit.aes = FALSE, hjust = -0.1, size = 2.8,
                colour = col_red, fontface = "bold", family = "Montserrat",
                lineheight = 0.9) +
      scale_fill_manual(values = c("Fyrir aðild" = col_before, "Eftir aðild" = col_red)) +
      scale_y_continuous(labels = function(x) paste0("\u20ac", round(x), "M"),
                         expand = expansion(mult = c(0, 0.25))) +
      coord_flip() +
      labs(title = "Tekjuáhrif á framleiðendur",
           subtitle = sprintf("Heildar tekjutap: \u20ac%.0fM (\u2212%.0f%% af framleiðsluverðmæti)",
                              m$total_loss_eur, m$total_loss_eur / m$total_before_eur * 100),
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme() +
      theme(
        legend.position = "top", legend.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E5E7EB"),
        axis.text.y = element_text(size = 11)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # CHART: Livestock before/after
  # ────────────────────────────────────────────────────────────────

  output$chart_livestock_before_after <- renderPlot({
    m <- model()

    df <- data.frame(
      item = factor(rep(c("Mjólk (M L)", "Ær (þús.)", "Nautgr. (þús.)"), 2),
                    levels = rev(c("Mjólk (M L)", "Ær (þús.)", "Nautgr. (þús.)"))),
      type = factor(rep(c("Fyrir", "Eftir"), each = 3), levels = c("Fyrir", "Eftir")),
      value = c(
        input$base_milk_litres, input$base_ewes / 1000, input$base_cattle / 1000,
        m$adj_milk, m$adj_ewes / 1000, m$adj_cattle / 1000
      )
    )

    changes <- data.frame(
      item = factor(c("Mjólk (M L)", "Ær (þús.)", "Nautgr. (þús.)"),
                    levels = rev(c("Mjólk (M L)", "Ær (þús.)", "Nautgr. (þús.)"))),
      before = c(input$base_milk_litres, input$base_ewes / 1000, input$base_cattle / 1000),
      after = c(m$adj_milk, m$adj_ewes / 1000, m$adj_cattle / 1000)
    )
    changes$pct <- (changes$after - changes$before) / changes$before * 100

    ggplot(df, aes(x = item, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = changes,
                aes(x = item, y = before * 1.02,
                    label = sprintf("%.0f%%", pct)),
                inherit.aes = FALSE, hjust = -0.15, size = 3.5,
                colour = col_red, fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = c("Fyrir" = col_basalt, "Eftir" = col_anc)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(title = "Framleiðslumagn: fyrir og eftir",
           subtitle = "Leiðrétt f. framboðsteygni (langtíma viðbrögð)",
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme() +
      theme(
        legend.position = "top", legend.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E5E7EB"),
        axis.text.y = element_text(size = 11)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # CHART: CAP waterfall (on adjusted base)
  # ────────────────────────────────────────────────────────────────

  output$chart_cap_waterfall <- renderPlot({
    m <- model()

    labels <- c("Stoð 1\nBeingreiðslur",
                "Art. 142\nMjólk",
                "Art. 142\nSauðfé",
                "Art. 142\nNautgripir",
                "ANC\nHarðbýli",
                "CAP\nSamtals")
    vals <- c(m$p1, m$nordic_milk, m$nordic_sheep, m$nordic_cows, m$anc_total, m$cap_total)
    fills <- c(col_p1, col_nordic, col_nordic, col_nordic, col_anc, col_accent)

    # Waterfall: stack first 5, last is total
    cumvals <- cumsum(vals[1:5])
    ymins <- c(0, cumvals[1:4], 0)
    ymaxs <- c(cumvals, m$cap_total)

    df <- data.frame(
      label = factor(labels, levels = labels),
      ymin = ymins, ymax = ymaxs, fill = fills
    )

    ggplot(df, aes(x = label)) +
      geom_rect(aes(xmin = as.numeric(label) - 0.35, xmax = as.numeric(label) + 0.35,
                    ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
      geom_hline(yintercept = m$current_support_eur, linetype = "dashed",
                 colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 6.4, y = m$current_support_eur,
               label = sprintf("Núverandi: \u20ac%.0fM", m$current_support_eur),
               hjust = 1, vjust = -0.7, size = 3.3, colour = col_brown,
               fontface = "bold", family = "Montserrat") +
      geom_hline(yintercept = m$total_loss_eur, linetype = "dotted",
                 colour = col_red, linewidth = 0.6) +
      annotate("text", x = 6.4, y = m$total_loss_eur,
               label = sprintf("Tekjutap: \u20ac%.0fM", m$total_loss_eur),
               hjust = 1, vjust = 1.5, size = 3.3, colour = col_red,
               fontface = "bold", family = "Montserrat") +
      geom_text(aes(y = (ymin + ymax) / 2, label = sprintf("\u20ac%.0fM", ymax - ymin)),
                size = 3.5, colour = "white", fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(title = "CAP-stuðningur á leiðréttum framleiðslugrunni",
           subtitle = sprintf(
             "Samtals \u20ac%.0fM  |  Leiðrétt land: %s ha  |  Mjólk: %.0fM L  |  Ær: %s  |  Naut: %s",
             m$cap_total,
             format(m$adj_ha, big.mark = "."),
             m$adj_milk,
             format(m$adj_ewes, big.mark = "."),
             format(m$adj_cattle, big.mark = ".")
           ),
           x = NULL, y = NULL) +
      rekon_theme() +
      theme(axis.text.x = element_text(size = 9, lineheight = 1.1))
  }, res = 110, bg = "transparent")

  # ── Payer bar ──

  output$payer_bar_html <- renderUI({
    m <- model()
    total <- m$eu_pays + m$iceland_pays
    eu_pct <- round(m$eu_pays / total * 100)
    is_pct <- 100 - eu_pct
    HTML(sprintf('
      <div style="margin:18px 0 8px 0;font-family:Montserrat,sans-serif;">
        <div style="font-size:13px;font-weight:600;color:%s;margin-bottom:8px;">
          Hver borgar? &mdash; ESB: \u20ac%.0fM (%s%%) &nbsp;|&nbsp; Ísland: \u20ac%.0fM (%s%%)
        </div>
        <div style="display:flex;height:32px;border-radius:6px;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,0.08);">
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            ESB \u20ac%.0fM</div>
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            Ísland \u20ac%.0fM</div>
        </div>
      </div>',
      col_brown,
      m$eu_pays, eu_pct, m$iceland_pays, is_pct,
      eu_pct, col_p1, m$eu_pays,
      is_pct, col_accent, m$iceland_pays
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # CHART: Net balance
  # ────────────────────────────────────────────────────────────────

  output$chart_net_balance <- renderPlot({
    m <- model()

    df <- data.frame(
      label = factor(c("Tekjutap\nframleiðenda", "CAP\nstuðningur",
                       "Nettó\nstaða bænda", "Sparnaður\nneytenda"),
                     levels = c("Tekjutap\nframleiðenda", "CAP\nstuðningur",
                                "Nettó\nstaða bænda", "Sparnaður\nneytenda")),
      value = c(-m$total_loss_eur, m$cap_total, m$net_farmer, m$consumer_save_eur),
      fill = c(col_red, col_green,
               if (m$net_farmer >= 0) col_moss else col_gap,
               col_consumer)
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.55) +
      geom_hline(yintercept = 0, colour = col_brown, linewidth = 0.6) +
      geom_text(aes(label = sprintf("%s\u20ac%.0fM", ifelse(value >= 0, "+", "\u2212"), abs(value)),
                    vjust = ifelse(value >= 0, -0.5, 1.5)),
                size = 4, fontface = "bold", colour = col_brown, family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0.15, 0.15))) +
      labs(title = "Heildaráhrif á hagsmunaaðila",
           subtitle = sprintf("Bændur nettó: %s\u20ac%.0fM  |  Neytendur: +\u20ac%.0fM/ár",
                              ifelse(m$net_farmer >= 0, "+", "\u2212"),
                              abs(m$net_farmer), m$consumer_save_eur),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, lineheight = 1.1),
        panel.grid.major.x = element_blank()
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # CHART: Farm trajectory
  # ────────────────────────────────────────────────────────────────

  output$chart_farm_trajectory <- renderPlot({
    m <- model()
    df <- m$farm_trajectory

    milestones <- df[df$year %in% c(0, 5, 10, 15), ]

    ggplot(df, aes(x = year, y = farms)) +
      geom_area(fill = col_red, alpha = 0.10) +
      geom_line(colour = col_red, linewidth = 1.2) +
      geom_point(data = milestones, colour = col_red, size = 3) +
      geom_text(data = milestones,
                aes(label = format(farms, big.mark = ".")),
                vjust = -1.2, size = 3.5, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      geom_vline(xintercept = input$transition_years, linetype = "dashed",
                 colour = col_basalt, linewidth = 0.5) +
      annotate("text", x = input$transition_years, y = max(df$farms) * 0.95,
               label = sprintf("Aðlögun: %s ár", input$transition_years),
               hjust = -0.1, size = 3, colour = col_basalt, family = "Montserrat") +
      scale_x_continuous(breaks = c(0, 5, 10, 15),
                         labels = c("Dagur 1", "Ár 5", "Ár 10", "Ár 15")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = "."),
                         expand = expansion(mult = c(0.05, 0.15))) +
      labs(title = "Spá: fjöldi búa eftir aðild",
           subtitle = sprintf("Fækkunarhraði: %.1f%%/ár  |  Háð framboðsteygni og CAP-dekkun",
                              m$effective_annual_exit * 100),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(size = 14),
        panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.3)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # DETAIL TABLE
  # ────────────────────────────────────────────────────────────────

  output$detail_table_html <- renderUI({
    m <- model()
    s <- m$sectors

    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:3px 10px;border-radius:4px;background:%s;color:#fff;font-size:10px;font-weight:600;font-family:Montserrat,sans-serif;">%s</span>', color, label)
    }
    badge_eu <- badge("ESB", col_p1)
    badge_is <- badge("Ísland", col_accent)
    badge_both <- badge("Samfjármögnun", col_anc)

    td <- function(x, align = "left", bold = FALSE, color = NULL) {
      style <- sprintf("padding:8px 12px;text-align:%s;font-family:%s;%s%s",
                       align,
                       if (align == "right") "Montserrat,sans-serif" else "Lora,serif",
                       if (bold) "font-weight:700;" else "",
                       if (!is.null(color)) paste0("color:", color, ";") else "")
      sprintf("<td style='%s'>%s</td>", style, x)
    }
    tr <- function(..., bg = NULL) {
      style <- if (!is.null(bg)) sprintf(' style="background:%s;"', bg) else ""
      paste0("<tr", style, ">", paste0(...), "</tr>")
    }

    rows <- paste0(
      # ── BASELINE vs ADJUSTED ──
      tr(td("GRUNNSTAÐA FYRIR AÐILD", bold = TRUE), td(""), td(""), td(""), td(""), bg = col_cloud),
      tr(td("&nbsp;&nbsp;Mjólk"),
         td(sprintf("%.0f M L", input$base_milk_litres), "right"),
         td(sprintf("\u20ac%.1fM", s$val_eur[1]), "right"),
         td(""), td("")),
      tr(td("&nbsp;&nbsp;Kindakjöt"),
         td(sprintf("%s ær", format(input$base_ewes, big.mark = ".")), "right"),
         td(sprintf("\u20ac%.1fM", s$val_eur[2]), "right"),
         td(""), td("")),
      tr(td("&nbsp;&nbsp;Nautakjöt"),
         td(sprintf("%s gripir", format(input$base_cattle, big.mark = ".")), "right"),
         td(sprintf("\u20ac%.1fM", s$val_eur[3]), "right"),
         td(""), td("")),
      tr(td("&nbsp;&nbsp;Egg/alifuglar"), td(""), td(sprintf("\u20ac%.1fM", s$val_eur[4]), "right"), td(""), td("")),
      tr(td("&nbsp;&nbsp;Grænmeti"), td(""), td(sprintf("\u20ac%.1fM", s$val_eur[5]), "right"), td(""), td("")),
      tr(td("&nbsp;&nbsp;Samtals framleiðsla", bold = TRUE), td(""),
         td(sprintf("\u20ac%.0fM", m$total_before_eur), "right", bold = TRUE), td(""), td(""),
         bg = col_cloud),

      # ── PRICE SHOCK + SUPPLY RESPONSE ──
      tr(td("EFTIR AÐILD (leiðrétt)", bold = TRUE), td("Magn"), td("Tekjur"), td("Breyting"), td(""), bg = col_cloud),
      tr(td("&nbsp;&nbsp;Mjólk"),
         td(sprintf("%.0f M L", m$adj_milk), "right"),
         td(sprintf("\u20ac%.1fM", s$revenue_after_eur[1]), "right"),
         td(sprintf("\u2212%.0f%%", s$drop_pct[1] + abs(s$prod_change_pct[1]) * 100), "right", color = col_red),
         td("")),
      tr(td("&nbsp;&nbsp;Kindakjöt"),
         td(sprintf("%s ær", format(m$adj_ewes, big.mark = ".")), "right"),
         td(sprintf("\u20ac%.1fM", s$revenue_after_eur[2]), "right"),
         td(sprintf("\u2212%.0f%%", s$drop_pct[2] + abs(s$prod_change_pct[2]) * 100), "right", color = col_red),
         td("")),
      tr(td("&nbsp;&nbsp;Nautakjöt"),
         td(sprintf("%s gripir", format(m$adj_cattle, big.mark = ".")), "right"),
         td(sprintf("\u20ac%.1fM", s$revenue_after_eur[3]), "right"),
         td(sprintf("\u2212%.0f%%", s$drop_pct[3] + abs(s$prod_change_pct[3]) * 100), "right", color = col_red),
         td("")),
      tr(td("&nbsp;&nbsp;Egg/alifuglar"), td(""),
         td(sprintf("\u20ac%.1fM", s$revenue_after_eur[4]), "right"),
         td(sprintf("\u2212%.0f%%", s$drop_pct[4] + abs(s$prod_change_pct[4]) * 100), "right", color = col_red),
         td("")),
      tr(td("&nbsp;&nbsp;Grænmeti"), td(""),
         td(sprintf("\u20ac%.1fM", s$revenue_after_eur[5]), "right"),
         td(sprintf("\u2212%.0f%%", s$drop_pct[5] + abs(s$prod_change_pct[5]) * 100), "right", color = col_red),
         td("")),
      tr(td("&nbsp;&nbsp;Tekjutap samtals", bold = TRUE), td(""),
         td(sprintf("\u2212\u20ac%.0fM", m$total_loss_eur), "right", bold = TRUE, color = col_red),
         td(""), td(""),
         bg = col_cloud),

      # ── CAP ON ADJUSTED BASE ──
      tr(td("CAP-STUÐNINGUR (leiðréttur)", bold = TRUE), td("Grunnur"), td("\u20acM"), td(""), td("Greiðandi"), bg = col_cloud),
      tr(td("&nbsp;&nbsp;Stoð 1: Beingreiðslur"),
         td(sprintf("%s ha \u00d7 \u20ac%s", format(m$adj_ha, big.mark = "."), input$p1_rate), "right"),
         td(sprintf("\u20ac%.1fM", m$p1), "right"),
         td(""), td(badge_eu, "center")),
      tr(td("&nbsp;&nbsp;Art. 142: Mjólk"),
         td(sprintf("%.0fM L \u00d7 \u20ac%.2f", m$adj_milk, input$milk_rate), "right"),
         td(sprintf("\u20ac%.1fM", m$nordic_milk), "right"),
         td(""), td(badge_is, "center")),
      tr(td("&nbsp;&nbsp;Art. 142: Sauðfé"),
         td(sprintf("%s \u00d7 \u20ac%s", format(m$adj_ewes, big.mark = "."), input$ewe_rate), "right"),
         td(sprintf("\u20ac%.1fM", m$nordic_sheep), "right"),
         td(""), td(badge_is, "center")),
      tr(td("&nbsp;&nbsp;Art. 142: Nautgripir"),
         td(sprintf("%s \u00d7 \u20ac%s", format(m$adj_cattle, big.mark = "."), input$cattle_rate), "right"),
         td(sprintf("\u20ac%.1fM", m$nordic_cows), "right"),
         td(""), td(badge_is, "center")),
      tr(td("&nbsp;&nbsp;ANC Harðbýli"),
         td(sprintf("%s ha \u00d7 \u20ac%s", format(m$adj_ha, big.mark = "."), input$anc_rate), "right"),
         td(sprintf("\u20ac%.1fM", m$anc_total), "right"),
         td(""), td(badge_both, "center")),
      tr(td("CAP SAMTALS", bold = TRUE), td(""),
         td(sprintf("\u20ac%.0fM", m$cap_total), "right", bold = TRUE, color = col_green),
         td(""), td(paste(badge_eu, badge_is), "center"),
         bg = col_cloud),

      # ── NET ──
      tr(td("NETTÓSTAÐA", bold = TRUE), td(""), td(""), td(""), td(""), bg = col_cloud),
      tr(td("&nbsp;&nbsp;Tekjutap"), td(""), td(sprintf("\u2212\u20ac%.0fM", m$total_loss_eur), "right", color = col_red), td(""), td("")),
      tr(td("&nbsp;&nbsp;CAP-stuðningur"), td(""), td(sprintf("+\u20ac%.0fM", m$cap_total), "right", color = col_green), td(""), td("")),
      tr(td("&nbsp;&nbsp;NETTÓ BÆNDUR", bold = TRUE), td(""),
         td(sprintf("%s\u20ac%.0fM", if (m$net_farmer >= 0) "+" else "\u2212", abs(m$net_farmer)), "right", bold = TRUE,
            color = if (m$net_farmer >= 0) col_green else col_red),
         td(sprintf("%.0f%% dekking", m$cap_covers_pct), "right"), td(""),
         bg = col_cloud)
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      sprintf('<th style="padding:8px 12px;text-align:left;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Liður</th>', col_grey),
      sprintf('<th style="padding:8px 12px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Magn/grunnur</th>', col_grey),
      sprintf('<th style="padding:8px 12px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">\u20acM/ári</th>', col_grey),
      sprintf('<th style="padding:8px 12px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Breyting</th>', col_grey),
      sprintf('<th style="padding:8px 12px;text-align:center;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Greiðandi</th>', col_grey),
      '</tr></thead><tbody>',
      rows,
      '</tbody></table>'
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # WINNERS / LOSERS TABLE
  # ────────────────────────────────────────────────────────────────

  output$winners_table_html <- renderUI({
    m <- model()

    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:4px 14px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:700;font-family:Montserrat;">%s</span>', color, label)
    }

    farmer_verdict <- if (m$cap_covers_pct >= 90) badge("Nánast jafnvægi", col_moss)
                      else if (m$cap_covers_pct >= 60) badge("Verulegt tap", col_anc)
                      else badge("Alvarlegt tap", col_red)

    td <- function(x, align = "left", bold = FALSE, color = NULL) {
      style <- sprintf("padding:10px 14px;text-align:%s;font-family:%s;%s%s",
                       align,
                       if (bold || align == "right") "Montserrat,sans-serif" else "Lora,serif",
                       if (bold) "font-weight:700;" else "",
                       if (!is.null(color)) paste0("color:", color, ";") else "")
      sprintf("<td style='%s'>%s</td>", style, x)
    }

    rows <- paste0(
      sprintf('<tr style="background:%s;">', col_cloud),
      td("Framleiðendur (bændur)", bold = TRUE),
      td(sprintf("\u2212\u20ac%.0fM tekjur", m$total_loss_eur), "right", color = col_red),
      td(sprintf("+\u20ac%.0fM CAP", m$cap_total), "right", color = col_green),
      td(sprintf("%s\u20ac%.0fM", if (m$net_farmer >= 0) "+" else "\u2212", abs(m$net_farmer)), "right",
         bold = TRUE, color = if (m$net_farmer >= 0) col_green else col_red),
      td(farmer_verdict, "center"),
      "</tr>",

      "<tr>",
      td("Neytendur (heimilin)", bold = TRUE),
      td("Matvælaverð lækkar", "right"),
      td(sprintf("+\u20ac%.0fM/ár", m$consumer_save_eur), "right", color = col_moss),
      td(sprintf("%.0f ma.kr. sparnaður", m$consumer_save_isk), "right", color = col_moss),
      td(badge("Ávinningur", col_moss), "center"),
      "</tr>",

      sprintf('<tr style="background:%s;">', col_cloud),
      td("Ríkissjóður", bold = TRUE),
      td("Tapar tolltekjum", "right", color = col_red),
      td("Sparar stuðning", "right", color = col_moss),
      td("Óvíst", "right"),
      td(badge("Hlutlaust", col_basalt), "center"),
      "</tr>",

      "<tr>",
      td("Dreifbýli / byggðir", bold = TRUE),
      td(sprintf("\u2212%s bú (%s ár)",
                 format(input$base_farms - m$farms_after, big.mark = "."),
                 input$transition_years), "right", color = col_red),
      td("ANC verndar land", "right"),
      td(sprintf("%.0f bú eftir", m$farms_after), "right"),
      td(badge("Áhættulegt", col_red), "center"),
      "</tr>"
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      sprintf('<th style="padding:8px 14px;text-align:left;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Aðili</th>', col_grey),
      sprintf('<th style="padding:8px 14px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Tap</th>', col_grey),
      sprintf('<th style="padding:8px 14px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Ávinningur</th>', col_grey),
      sprintf('<th style="padding:8px 14px;text-align:right;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Nettó</th>', col_grey),
      sprintf('<th style="padding:8px 14px;text-align:center;font-size:11px;color:%s;text-transform:uppercase;font-family:Montserrat;">Mat</th>', col_grey),
      '</tr></thead><tbody>',
      rows,
      '</tbody></table>'
    ))
  })
}

shinyApp(ui, server)
