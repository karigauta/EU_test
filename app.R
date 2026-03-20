library(shiny)
library(ggplot2)
library(scales)
# ══════════════════════════════════════════════════════════════════
# CAP SUPPORT ESTIMATION MODEL FOR ICELAND
# Rekon / Kári Gautason — March 2026
#
# Tab 1: CAP Support — three payment channels
# Tab 2: Tariff Impact — modelling effects of removing tariff
#         protection, calibrated on Finland/Sweden 1995 accession
#
# Key references:
#   Kola, Hofreither & Rabinowicz (2000) — Nordic accession impacts
#   Niemi (2003) — Static welfare effects on Finnish agriculture
#   OECD PSE Iceland (2025) — PSE 47%, prices 60% above border
#   EU Evaluation of Nordic Aid Schemes (2007)
#   Hagstofa Íslands, LbhÍ Rit 179 (2025)
# ══════════════════════════════════════════════════════════════════

# ── Rekon Brand Palette ──────────────────────────────────────────
col_sky      <- "#4A90E2"   # Sky Blue (primary brand color)
col_green    <- "#4A5C36"   # Highland Green
col_cloud    <- "#F3F4F6"   # Cloud White (backgrounds)
col_grey     <- "#6B7280"   # Mountain Grey
col_brown    <- "#57534E"   # Soil Brown
col_red      <- "#A12B26"   # Signal Red
col_p1       <- "#4A90E2"   # Sky Blue — Pillar 1 / EU
col_nordic   <- "#4A5C36"   # Highland Green — Nordic Aid
col_anc      <- "#E2B14A"   # Lichen Gold — ANC
col_gap      <- "#A12B26"   # Signal Red — Gap / Loss
col_accent   <- "#57534E"   # Soil Brown — totals/neutral
col_basalt   <- "#5D6D7E"   # Basalt Grey
col_moss     <- "#789A5B"   # Moss Green
col_consumer <- "#2E86AB"   # Consumer benefit blue
col_before   <- "#D1D5DB"   # Light grey for "before" bars

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
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&family=Lora:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(app_css))
  ),

  # ── Brand header ──
  div(class = "brand-bar",
    div(
      div(class = "brand-title", "rekon", span("| CAP stuðningsáætlun")),
      div(class = "brand-sub", "Áætlun um stuðning ESB við íslenskan landbúnað")
    ),
    div(style = "text-align:right;",
      div(style = "font-size:11px; opacity:0.7;", "Kári Gautason — mars 2026"),
      div(style = "font-size:11px; opacity:0.7;", "Grounded Analysis & Strategic Insight")
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TABS
  # ══════════════════════════════════════════════════════════════
  tabsetPanel(
    id = "main_tabs",

    # ════════════════════════════════════════════════════════════
    # TAB 1: CAP SUPPORT
    # ════════════════════════════════════════════════════════════
    tabPanel("Greiðsluleiðir CAP",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          div(class = "sidebar-section", "Grunnforsendur"),
          sliderInput("eligible_ha", "Styrkhæft land (ha)",
                      min = 80000, max = 400000, value = 200000, step = 10000,
                      pre = "", post = " ha"),
          helpText("120k = ræktað land. 200k = ræktað + viðhaldið tún. Source: LbhÍ 2025, FAO."),
          sliderInput("current_support_isk", "Núverandi stuðningur (ma.kr.)",
                      min = 14, max = 30, value = 18, step = 0.5,
                      pre = "", post = " ma.kr."),
          helpText("Búvörusamningar."),
          numericInput("eur_isk", "Gengi EUR/ISK", value = 143, min = 100, max = 200, step = 1),
          hr(),
          div(class = "sidebar-section", "Stoð 1: Beingreiðslur (Pillar 1)"),
          sliderInput("p1_rate", "Greiðsla á hektara (€/ha)",
                      min = 100, max = 400, value = 200, step = 10, pre = "€"),
          helpText("EU lágmark: €200 (2023), €215 (2027). EU meðaltal: ~€243. Nýju ríkin: €130–180."),
          hr(),
          div(class = "sidebar-section", "Norðurslóðastuðningur (Art. 142)"),
          sliderInput("milk_litres", "Mjólkurframleiðsla (M lítr.)",
                      min = 120, max = 170, value = 145, step = 5, post = "M L"),
          sliderInput("milk_rate", "Mjólkurgreiðsla (€/lítra)",
                      min = 0.04, max = 0.15, value = 0.09, step = 0.01, pre = "€"),
          helpText("Finnland: €0.07 (suður) – €0.11 (Lappland)"),
          sliderInput("ewes", "Ær (fjöldi)",
                      min = 200000, max = 500000, value = 400000, step = 10000),
          sliderInput("ewe_rate", "Greiðsla á á (€/kind)",
                      min = 10, max = 40, value = 25, step = 1, pre = "€"),
          helpText("ESB hámark í tengdum greiðslum: €28/kind."),
          sliderInput("cattle", "Nautgripir (fjöldi)",
                      min = 20000, max = 50000, value = 31000, step = 1000),
          sliderInput("cattle_rate", "Greiðsla á grip (€/grip)",
                      min = 100, max = 400, value = 200, step = 10, pre = "€"),
          helpText("Finnland: €150–350 eftir svæði og tegund. Meðaltal ~€200."),
          hr(),
          div(class = "sidebar-section", "Harðbýlisgreiðslur (ANC, Pillar 2)"),
          sliderInput("anc_rate", "ANC greiðsla á hektara (€/ha)",
                      min = 100, max = 450, value = 250, step = 10, pre = "€"),
          helpText("Hámark norðan 62°: €450/ha. Finnland meðaltal: ~€217/ha."),
          sliderInput("eu_cofinance", "ESB-hlutfall af ANC (%)",
                      min = 30, max = 75, value = 55, step = 5, post = "%"),
          helpText("Fer eftir þróunarstigi svæðis. Háþróuð ríki: ~40%. Jaðarsvæði: ~55–75%."),
          hr(),
          p(class = "source-note",
            "Höfundur: Rekon / Kári Gautason. Heimildir: LbhÍ Rit 179 (2025), ",
            "Úttekt AMS/HÍ á aðildarviðræðum (2014), ",
            "EU Reg. 2021/2115, Wageningen Economic Research, CAP Reform blog, Hagstofa Íslands.")
        ),

        mainPanel(
          width = 8,
          fluidRow(
            column(3, div(class = "metric-box eu",
                          div(class = "metric-label", "Núverandi stuðningur"),
                          uiOutput("metric_current"))),
            column(3, div(class = "metric-box cap",
                          div(class = "metric-label", "CAP heildaráætlun"),
                          uiOutput("metric_cap_total"))),
            column(3, div(class = "metric-box gap",
                          div(class = "metric-label", "Munur (gap)"),
                          uiOutput("metric_gap"))),
            column(3, div(class = "metric-box rate",
                          div(class = "metric-label", "Nauðsynlegur €/ha"),
                          uiOutput("metric_needed_rate")))
          ),
          uiOutput("payer_bar_html"),
          hr(class = "section-divider"),
          plotOutput("waterfall_chart", height = "420px"),
          hr(class = "section-divider"),
          fluidRow(column(12, plotOutput("component_chart", height = "320px"))),
          hr(class = "section-divider"),
          h4("Sundurliðun"),
          uiOutput("detail_table_html"),
          p(class = "source-note",
            "Athugasemd: Allar tölur eru áætlaðar á grundvelli bestu þekkingar og fordæma frá Finnlandi. ",
            "Raunverulegar upphæðir fara eftir samningaviðræðum. ",
            "Norðurslóðastuðningur (Art. 142) er greiddur af Íslandi, ekki ESB. ",
            "ANC-greiðslur eru samfjármagnaðar.")
        )
      )
    ),

    # ════════════════════════════════════════════════════════════
    # TAB 2: TARIFF IMPACT
    # ════════════════════════════════════════════════════════════
    tabPanel("Áhrif tollaafnáms",
      sidebarLayout(
        sidebarPanel(
          width = 4,

          div(class = "context-box",
            HTML("<strong>Hvað gerist þegar tollar falla?</strong><br>
            Ísland hefur næsthæstu landbúnaðartolla í OECD (OECD PSE: 47%).
            Verðlag til bænda er ~60% yfir heimsmarkaðsverði.
            Hér er hægt að skoða áhrifin ef Ísland gengi í ESB og tækju upp
            sameiginlega landbúnaðarstefnuna (CAP), byggð á reynslu Finnlands
            og Svíþjóðar 1995.")
          ),

          div(class = "sidebar-section", "Framleiðsluverðmæti Íslands (ma.kr.)"),
          helpText("Áætlað heildarframleiðsluverðmæti á núverandi innlendu verði."),

          sliderInput("t_dairy_val", "Mjólkurvörur",
                      min = 10, max = 30, value = 19, step = 0.5, post = " ma.kr."),
          sliderInput("t_lamb_val", "Kindakjöt",
                      min = 3, max = 15, value = 7, step = 0.5, post = " ma.kr."),
          sliderInput("t_beef_val", "Nautakjöt",
                      min = 1, max = 8, value = 3, step = 0.5, post = " ma.kr."),
          sliderInput("t_egg_val", "Egg og alifuglar",
                      min = 0.5, max = 5, value = 2, step = 0.5, post = " ma.kr."),
          sliderInput("t_veg_val", "Grænmeti og gróðurhús",
                      min = 1, max = 8, value = 4, step = 0.5, post = " ma.kr."),

          hr(),
          div(class = "sidebar-section", "Verðlækkun til framleiðenda (%)"),
          helpText("Byggð á reynslu Finnlands 1995. Hægt að aðlaga."),

          sliderInput("t_dairy_drop", "Mjólk — verðlækkun",
                      min = 10, max = 50, value = 30, step = 1, post = "%"),
          helpText("Finnland: −28% til −32%."),
          sliderInput("t_lamb_drop", "Kindakjöt — verðlækkun",
                      min = 15, max = 60, value = 45, step = 1, post = "%"),
          helpText("Finnland nautakjöt: −38% til −43%. Lambakjöt líklega hærra."),
          sliderInput("t_beef_drop", "Nautakjöt — verðlækkun",
                      min = 15, max = 55, value = 40, step = 1, post = "%"),
          sliderInput("t_egg_drop", "Egg/alifuglar — verðlækkun",
                      min = 20, max = 70, value = 55, step = 1, post = "%"),
          helpText("Finnland egg: −65% til −68%. Ísland líkl. lægra v/ flutnkostn."),
          sliderInput("t_veg_drop", "Grænmeti — verðlækkun",
                      min = 5, max = 40, value = 20, step = 1, post = "%"),
          helpText("Hluti ræktar í gróðurhúsum m/ jarðvarma. Minni samkeppnisáhrif."),

          hr(),
          div(class = "sidebar-section", "Neytendaáhrif og búskapur"),

          sliderInput("t_food_spend", "Matarútgjöld heimila (ma.kr./ár)",
                      min = 100, max = 250, value = 170, step = 10, post = " ma.kr."),
          helpText("Hagstofa: ~170 ma.kr. Matvælaverð á Íslandi 48% yfir ESB-meðaltali."),

          sliderInput("t_consumer_drop", "Áætluð matvælaverðlækkun (%)",
                      min = 5, max = 30, value = 15, step = 1, post = "%"),
          helpText("Finnland: −11%. Ísland líklega meira (stærri tollgat)."),

          sliderInput("t_farms_now", "Fjöldi búa í dag",
                      min = 1500, max = 4000, value = 2500, step = 100),
          sliderInput("t_farm_exit_10y", "Bústöðvun á 10 árum (%)",
                      min = 10, max = 60, value = 35, step = 1, post = "%"),
          helpText("Finnland: −38% á 17 árum. Mjólkurbú: −82% á 24 árum."),

          numericInput("t_eur_isk", "Gengi EUR/ISK", value = 143, min = 100, max = 200, step = 1),

          hr(),
          p(class = "source-note",
            "Heimildir: OECD Agricultural Policy Monitoring (2025), ",
            "Kola, Hofreither & Rabinowicz (2000), ",
            "Niemi (2003) — Static Welfare Effects, ",
            "EU Evaluation of Nordic Aid Schemes (2007), ",
            "Eurostat Comparative Price Levels (2024), ",
            "Hagstofa Íslands, USDA Iceland Tariff Review.")
        ),

        mainPanel(
          width = 8,

          # ── Headline metrics ──
          fluidRow(
            column(3, div(class = "metric-box loss",
                          div(class = "metric-label", "Tekjutap bænda"),
                          uiOutput("t_metric_loss"))),
            column(3, div(class = "metric-box gain",
                          div(class = "metric-label", "Sparnaður neytenda"),
                          uiOutput("t_metric_consumer"))),
            column(3, div(class = "metric-box farm",
                          div(class = "metric-label", "Bú sem hætta"),
                          uiOutput("t_metric_farms"))),
            column(3, div(class = "metric-box eu",
                          div(class = "metric-label", "Tekjutap sem % af CAP"),
                          uiOutput("t_metric_cap_cover")))
          ),

          hr(class = "section-divider"),

          # ── Before/after price impact by sector ──
          plotOutput("t_price_impact_chart", height = "380px"),

          hr(class = "section-divider"),

          # ── Revenue loss waterfall ──
          plotOutput("t_revenue_waterfall", height = "380px"),

          hr(class = "section-divider"),

          # ── Balance: loss vs. CAP compensation ──
          fluidRow(
            column(6, plotOutput("t_balance_chart", height = "340px")),
            column(6, plotOutput("t_farm_projection", height = "340px"))
          ),

          hr(class = "section-divider"),

          # ── Winners and losers summary ──
          h4("Ávinningur og kostnaður — Hver hagnast?"),
          uiOutput("t_winners_table"),

          p(class = "source-note",
            "Athugasemd: Þetta líkan er einfaldað. Raunveruleg áhrif fara eftir aðlögunartíma, ",
            "sérákvæðum í aðildarsamningi og viðbrögðum framleiðenda. ",
            "Finnland fékk 5 ára aðlögunartímabil. ",
            "Verðlækkanir miðast við afnám tollaverndar og samkeppni á innri markaðnum.")
        )
      )
    )
  ) # end tabsetPanel
)

# ══════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ────────────────────────────────────────────────────────────────
  # TAB 1: CAP SUPPORT — REACTIVE CALCULATIONS
  # ────────────────────────────────────────────────────────────────

  calcs <- reactive({
    current_eur <- input$current_support_isk * 1000 / input$eur_isk
    p1 <- input$eligible_ha * input$p1_rate / 1e6
    milk  <- input$milk_litres * 1e6 * input$milk_rate / 1e6
    sheep <- input$ewes * input$ewe_rate / 1e6
    cows  <- input$cattle * input$cattle_rate / 1e6
    nordic <- milk + sheep + cows
    anc_total  <- input$eligible_ha * input$anc_rate / 1e6
    anc_eu     <- anc_total * input$eu_cofinance / 100
    anc_is     <- anc_total * (1 - input$eu_cofinance / 100)
    total <- p1 + nordic + anc_total
    gap   <- current_eur - total
    eu_pays      <- p1 + anc_eu
    iceland_pays <- nordic + anc_is
    needed_rate <- current_eur * 1e6 / input$eligible_ha
    list(
      current_eur = current_eur,
      p1 = p1, milk = milk, sheep = sheep, cows = cows,
      nordic = nordic,
      anc_total = anc_total, anc_eu = anc_eu, anc_is = anc_is,
      total = total, gap = gap,
      eu_pays = eu_pays, iceland_pays = iceland_pays,
      needed_rate = needed_rate
    )
  })

  # ── Tab 1: Metric boxes ──

  output$metric_current <- renderUI({
    c <- calcs()
    div(class = "metric-value", sprintf("\u20ac%.0fM", c$current_eur))
  })

  output$metric_cap_total <- renderUI({
    c <- calcs()
    col <- if (c$total >= c$current_eur) col_green else col_red
    div(class = "metric-value", style = paste0("color:", col), sprintf("\u20ac%.0fM", c$total))
  })

  output$metric_gap <- renderUI({
    c <- calcs()
    if (c$gap > 0) {
      tagList(
        div(class = "metric-value", style = paste0("color:", col_red), sprintf("\u2212\u20ac%.0fM", c$gap)),
        div(class = "metric-sub", sprintf("%.0f%% vantar", c$gap / c$current_eur * 100))
      )
    } else {
      div(class = "metric-value", style = paste0("color:", col_green), sprintf("+\u20ac%.0fM", abs(c$gap)))
    }
  })

  output$metric_needed_rate <- renderUI({
    c <- calcs()
    div(
      div(class = "metric-value", sprintf("\u20ac%.0f/ha", c$needed_rate)),
      div(class = "metric-sub", style = paste0("color:", col_grey), "til a\u00f0 jafna n\u00faverandi")
    )
  })

  # ── Tab 1: Payer stacked bar ──

  output$payer_bar_html <- renderUI({
    c <- calcs()
    total <- c$eu_pays + c$iceland_pays
    eu_pct <- round(c$eu_pays / total * 100)
    is_pct <- 100 - eu_pct
    HTML(sprintf('
      <div style="margin:18px 0 8px 0; font-family:Montserrat,sans-serif;">
        <div style="font-size:13px;font-weight:600;color:%s;margin-bottom:8px;">
          Hver borgar? &mdash; ESB: \u20ac%.0fM (%s%%) &nbsp;|&nbsp; \u00cdsland: \u20ac%.0fM (%s%%)
        </div>
        <div style="display:flex;height:32px;border-radius:6px;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,0.08);">
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            ESB \u20ac%.0fM
          </div>
          <div style="width:%s%%;background:%s;display:flex;align-items:center;justify-content:center;color:#fff;font-size:12px;font-weight:600;">
            \u00cdsland \u20ac%.0fM
          </div>
        </div>
      </div>',
      col_brown,
      c$eu_pays, eu_pct, c$iceland_pays, is_pct,
      eu_pct, col_p1, c$eu_pays,
      is_pct, col_accent, c$iceland_pays
    ))
  })

  # ── Tab 1: Waterfall chart ──

  output$waterfall_chart <- renderPlot({
    c <- calcs()
    df <- data.frame(
      label = factor(c("Sto\u00f0 1\nBeingrei\u00f0slur", "Art. 142\nNor\u00f0ursl\u00f3\u00f0ir",
                       "Sto\u00f0 2\nHar\u00f0b\u00fdli (ANC)", "CAP samtals", "Munur (gap)"),
                     levels = c("Sto\u00f0 1\nBeingrei\u00f0slur", "Art. 142\nNor\u00f0ursl\u00f3\u00f0ir",
                                "Sto\u00f0 2\nHar\u00f0b\u00fdli (ANC)", "CAP samtals", "Munur (gap)")),
      fill  = c(col_p1, col_nordic, col_anc, col_accent, col_gap),
      ymin  = c(0, c$p1, c$p1 + c$nordic, 0, c$total),
      ymax  = c(c$p1, c$p1 + c$nordic, c$total, c$total, c$total + max(c$gap, 0))
    )
    ggplot(df, aes(x = label)) +
      geom_rect(aes(xmin = as.numeric(label) - 0.35, xmax = as.numeric(label) + 0.35,
                    ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
      geom_hline(yintercept = c$current_eur, linetype = "dashed", colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 5.4, y = c$current_eur + 3,
               label = sprintf("N\u00faverandi: \u20ac%.0fM", c$current_eur),
               hjust = 1, size = 3.5, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_text(aes(y = (ymin + ymax) / 2, label = sprintf("\u20ac%.0fM", ymax - ymin)),
                size = 3.8, colour = "white", fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"), expand = expansion(mult = c(0, 0.08))) +
      coord_cartesian(ylim = c(0, max(c$current_eur, c$total + c$gap) * 1.12)) +
      labs(title = "CAP stu\u00f0nings\u00e1\u00e6tlun fyrir \u00cdsland",
           subtitle = sprintf("Samtals \u20ac%.0fM vs. n\u00faverandi \u20ac%.0fM  |  Eligible area: %s ha  |  EUR/ISK: %s",
                              c$total, c$current_eur, format(input$eligible_ha, big.mark = "."), input$eur_isk),
           x = NULL, y = NULL) +
      rekon_theme() +
      theme(axis.text.x = element_text(size = 10, lineheight = 1.1))
  }, res = 110, bg = "transparent")

  # ── Tab 1: Component chart ──

  output$component_chart <- renderPlot({
    c <- calcs()
    df <- data.frame(
      component = factor(
        c("Beingrei\u00f0slur (Sto\u00f0 1)", "Mj\u00f3lk", "Sau\u00f0f\u00e9", "Nautgripir", "ANC (Sto\u00f0 2)"),
        levels = rev(c("Beingrei\u00f0slur (Sto\u00f0 1)", "Mj\u00f3lk", "Sau\u00f0f\u00e9", "Nautgripir", "ANC (Sto\u00f0 2)"))
      ),
      value = c(c$p1, c$milk, c$sheep, c$cows, c$anc_total),
      fill = c(col_p1, col_nordic, col_nordic, col_nordic, col_anc)
    )
    ggplot(df, aes(x = component, y = value, fill = fill)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = sprintf("\u20ac%.1fM", value)), hjust = -0.15, size = 3.8,
                colour = col_brown, fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"), expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(title = "Sundurli\u00f0un eftir grei\u00f0slulei\u00f0um",
           subtitle = "Bl\u00e1tt = Sto\u00f0 1 (ESB)  |  Gr\u00e6nt = Art. 142 (\u00cdsland)  |  Gult = ANC (samf.)",
           x = NULL, y = NULL) +
      rekon_theme() +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11)
      )
  }, res = 110, bg = "transparent")

  # ── Tab 1: Detail table ──

  output$detail_table_html <- renderUI({
    c <- calcs()
    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:3px 12px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:600;font-family:Montserrat,sans-serif;">%s</span>', color, label)
    }
    badge_eu <- badge("ESB", col_p1)
    badge_is <- badge("\u00cdsland", col_accent)
    badge_both <- badge("Samfj\u00e1rm\u00f6gnun", col_anc)

    rows <- list(
      list("Sto\u00f0 1: Beingrei\u00f0slur", sprintf("\u20ac%.1fM", c$p1), badge_eu, TRUE),
      list(sprintf("&nbsp;&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha", format(input$eligible_ha, big.mark = "."), input$p1_rate), "", "", FALSE),
      list("", "", "", FALSE),
      list("Art. 142: Nor\u00f0ursl\u00f3\u00f0astu\u00f0ningur", sprintf("\u20ac%.1fM", c$nordic), badge_is, TRUE),
      list(sprintf("&nbsp;&nbsp;&nbsp;Mj\u00f3lk: %.0fM L \u00d7 \u20ac%.2f/L", input$milk_litres, input$milk_rate), sprintf("\u20ac%.1fM", c$milk), badge_is, FALSE),
      list(sprintf("&nbsp;&nbsp;&nbsp;Sau\u00f0f\u00e9: %s \u00e6r \u00d7 \u20ac%s/kind", format(input$ewes, big.mark = "."), input$ewe_rate), sprintf("\u20ac%.1fM", c$sheep), badge_is, FALSE),
      list(sprintf("&nbsp;&nbsp;&nbsp;Nautgripir: %s \u00d7 \u20ac%s/grip", format(input$cattle, big.mark = "."), input$cattle_rate), sprintf("\u20ac%.1fM", c$cows), badge_is, FALSE),
      list("", "", "", FALSE),
      list("Sto\u00f0 2: Har\u00f0b\u00fdlisgrei\u00f0slur (ANC)", sprintf("\u20ac%.1fM", c$anc_total), badge_both, TRUE),
      list(sprintf("&nbsp;&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha", format(input$eligible_ha, big.mark = "."), input$anc_rate), "", "", FALSE),
      list(sprintf("&nbsp;&nbsp;&nbsp;\u00dear af ESB (%s%%)", input$eu_cofinance), sprintf("\u20ac%.1fM", c$anc_eu), badge_eu, FALSE),
      list(sprintf("&nbsp;&nbsp;&nbsp;\u00dear af \u00cdsland (%s%%)", 100 - input$eu_cofinance), sprintf("\u20ac%.1fM", c$anc_is), badge_is, FALSE)
    )

    row_html <- sapply(rows, function(r) {
      bg <- if (r[[4]]) sprintf(' style="background:%s;font-weight:600;"', col_cloud) else ""
      sprintf("<tr%s><td style='padding:7px 12px;font-family:Lora,serif;'>%s</td><td style='padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;font-weight:600;'>%s</td><td style='padding:7px 12px;text-align:center;'>%s</td></tr>",
              bg, r[[1]], r[[2]], r[[3]])
    })

    gap_color <- if (c$gap > 0) col_red else col_green
    gap_sign <- if (c$gap > 0) sprintf("\u2212\u20ac%.1fM", c$gap) else sprintf("+\u20ac%.1fM", abs(c$gap))

    summary_rows <- sprintf('
      <tr style="border-top:2px solid %s;background:%s;font-weight:700;">
        <td style="padding:10px 12px;font-family:Montserrat,sans-serif;">SAMTALS CAP</td>
        <td style="padding:10px 12px;text-align:right;font-family:Montserrat,sans-serif;">\u20ac%.1fM</td>
        <td style="padding:10px 12px;text-align:center;">%s &nbsp; %s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:7px 12px;font-family:Lora,serif;">N\u00faverandi stu\u00f0ningur</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;">\u20ac%.1fM</td>
        <td style="padding:7px 12px;text-align:center;">%s</td>
      </tr>
      <tr style="border-top:2px solid %s;font-weight:700;">
        <td style="padding:10px 12px;color:%s;font-family:Montserrat,sans-serif;">MUNUR</td>
        <td style="padding:10px 12px;text-align:right;color:%s;font-family:Montserrat,sans-serif;font-size:15px;">%s</td>
        <td></td>
      </tr>',
      col_brown, col_cloud,
      c$total, badge_eu, badge_is,
      c$current_eur, badge_is,
      gap_color, gap_color, gap_color, gap_sign
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      '<th style="padding:8px 12px;text-align:left;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Li\u00f0ur</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">\u20acM/\u00e1ri</th>',
      '<th style="padding:8px 12px;text-align:center;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Grei\u00f0andi</th>',
      '</tr></thead><tbody>',
      paste(row_html, collapse = ""),
      summary_rows,
      '</tbody></table>'
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # TAB 2: TARIFF IMPACT — REACTIVE CALCULATIONS
  # ────────────────────────────────────────────────────────────────

  tariff <- reactive({
    # Sector data: value (ISK bn), drop (%), convert to EUR M
    fx <- input$t_eur_isk
    sectors <- data.frame(
      sector = c("Mj\u00f3lkurv\u00f6rur", "Kindakj\u00f6t", "Nautakj\u00f6t", "Egg/alifuglar", "Gr\u00e6nmeti"),
      value_isk = c(input$t_dairy_val, input$t_lamb_val, input$t_beef_val,
                    input$t_egg_val, input$t_veg_val),
      drop_pct = c(input$t_dairy_drop, input$t_lamb_drop, input$t_beef_drop,
                   input$t_egg_drop, input$t_veg_drop),
      stringsAsFactors = FALSE
    )
    sectors$value_eur <- sectors$value_isk * 1000 / fx  # €M
    sectors$loss_eur  <- sectors$value_eur * sectors$drop_pct / 100
    sectors$after_eur <- sectors$value_eur - sectors$loss_eur

    total_loss_eur <- sum(sectors$loss_eur)
    total_before   <- sum(sectors$value_eur)
    total_after    <- sum(sectors$after_eur)

    # Consumer savings
    consumer_save_isk <- input$t_food_spend * input$t_consumer_drop / 100
    consumer_save_eur <- consumer_save_isk * 1000 / fx

    # Farm projection (exponential decline over 10 years)
    farms_after <- round(input$t_farms_now * (1 - input$t_farm_exit_10y / 100))
    farms_lost  <- input$t_farms_now - farms_after
    annual_exit <- 1 - (1 - input$t_farm_exit_10y / 100)^(1/10)

    # Farm trajectory: year 0 to 15
    years <- 0:15
    farm_trajectory <- data.frame(
      year = years,
      farms = round(input$t_farms_now * (1 - annual_exit)^years)
    )

    list(
      sectors = sectors,
      total_loss_eur = total_loss_eur,
      total_before = total_before,
      total_after = total_after,
      consumer_save_eur = consumer_save_eur,
      consumer_save_isk = consumer_save_isk,
      farms_after = farms_after,
      farms_lost = farms_lost,
      farm_trajectory = farm_trajectory,
      fx = fx
    )
  })

  # ── Tab 2: Metrics ──

  output$t_metric_loss <- renderUI({
    t <- tariff()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("\u2212\u20ac%.0fM", t$total_loss_eur)),
      div(class = "metric-sub", sprintf("%.0f%% af framlei\u00f0sluvermi", t$total_loss_eur / t$total_before * 100))
    )
  })

  output$t_metric_consumer <- renderUI({
    t <- tariff()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_moss),
          sprintf("+\u20ac%.0fM", t$consumer_save_eur)),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("%.0f ma.kr./\u00e1r l\u00e6gra matarver\u00f0", t$consumer_save_isk))
    )
  })

  output$t_metric_farms <- renderUI({
    t <- tariff()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("\u2212%s", format(t$farms_lost, big.mark = "."))),
      div(class = "metric-sub", sprintf("b\u00fa \u00e1 10 \u00e1rum (%s%% f\u00e6kkun)", input$t_farm_exit_10y))
    )
  })

  output$t_metric_cap_cover <- renderUI({
    t <- tariff()
    c <- calcs()
    cover_pct <- c$total / t$total_loss_eur * 100
    col <- if (cover_pct >= 100) col_green else col_red
    tagList(
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%.0f%%", cover_pct)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("CAP \u20ac%.0fM vs. tap \u20ac%.0fM", c$total, t$total_loss_eur))
    )
  })

  # ── Tab 2: Price impact chart (before/after paired bars) ──

  output$t_price_impact_chart <- renderPlot({
    t <- tariff()
    s <- t$sectors

    df <- data.frame(
      sector = factor(rep(s$sector, 2), levels = rev(s$sector)),
      type = rep(c("Fyrir", "Eftir"), each = nrow(s)),
      value = c(s$value_eur, s$after_eur),
      stringsAsFactors = FALSE
    )
    df$type <- factor(df$type, levels = c("Fyrir", "Eftir"))

    # Loss annotations
    ann <- data.frame(
      sector = factor(s$sector, levels = rev(s$sector)),
      y_pos = s$value_eur + 2,
      label = sprintf("\u2212%.0f%%", s$drop_pct)
    )

    ggplot(df, aes(x = sector, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = ann, aes(x = sector, y = y_pos, label = label),
                inherit.aes = FALSE, hjust = -0.1, size = 3.5,
                colour = col_red, fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = c("Fyrir" = col_before, "Eftir" = col_red),
                        labels = c("N\u00faverandi ver\u00f0", "Eftir tollaafn\u00e1m")) +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(title = "Ver\u00f0\u00e1hrif \u00e1 framlei\u00f0endur eftir geirum",
           subtitle = sprintf("Heildar tekjutap: \u20ac%.0fM  |  Gr\u00e1tt = n\u00faverandi  |  Rau\u00f0t = eftir ESB-a\u00f0ild",
                              t$total_loss_eur),
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11)
      )
  }, res = 110, bg = "transparent")

  # ── Tab 2: Revenue loss waterfall ──

  output$t_revenue_waterfall <- renderPlot({
    t <- tariff()
    s <- t$sectors

    labels <- c(s$sector, "Samtals tap")
    values <- c(s$loss_eur, t$total_loss_eur)
    fills  <- c(rep(col_red, nrow(s)), col_accent)

    # Waterfall positions
    ymin <- c(rep(0, nrow(s)), 0)
    ymax <- values

    df <- data.frame(
      label = factor(labels, levels = labels),
      ymin = ymin, ymax = ymax, fill = fills
    )

    ggplot(df, aes(x = label)) +
      geom_rect(aes(xmin = as.numeric(label) - 0.35, xmax = as.numeric(label) + 0.35,
                    ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
      geom_text(aes(y = (ymin + ymax) / 2, label = sprintf("\u20ac%.0fM", ymax - ymin)),
                size = 3.8, colour = "white", fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Tekjutap framlei\u00f0enda eftir greinum",
           subtitle = "Framlei\u00f0sluver\u00f0m\u00e6ti sem tapast vi\u00f0 ver\u00f0l\u00e6kkun",
           x = NULL, y = NULL) +
      rekon_theme() +
      theme(axis.text.x = element_text(size = 9, lineheight = 1.1, angle = 15, hjust = 1))
  }, res = 110, bg = "transparent")

  # ── Tab 2: Balance chart (loss vs CAP compensation) ──

  output$t_balance_chart <- renderPlot({
    t <- tariff()
    c <- calcs()

    df <- data.frame(
      label = factor(c("Tekjutap\nbænda", "CAP\nstu\u00f0ningur", "Nett\u00f3 sta\u00f0a"),
                     levels = c("Tekjutap\nbænda", "CAP\nstu\u00f0ningur", "Nett\u00f3 sta\u00f0a")),
      value = c(t$total_loss_eur, c$total, c$total - t$total_loss_eur),
      fill  = c(col_red, col_green, if (c$total >= t$total_loss_eur) col_moss else col_gap)
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.55) +
      geom_hline(yintercept = 0, colour = col_brown, linewidth = 0.5) +
      geom_text(aes(label = sprintf("\u20ac%.0fM", value),
                    vjust = ifelse(value >= 0, -0.5, 1.5)),
                size = 4, fontface = "bold", colour = col_brown, family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0.15, 0.15))) +
      labs(title = "B\u00e6tur CAP upp tapi\u00f0?",
           subtitle = sprintf("CAP dekkar %.0f%% af tekjutapinu", c$total / t$total_loss_eur * 100),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        axis.text.x = element_text(size = 10, lineheight = 1.1)
      )
  }, res = 110, bg = "transparent")

  # ── Tab 2: Farm projection chart ──

  output$t_farm_projection <- renderPlot({
    t <- tariff()
    df <- t$farm_trajectory

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
                         labels = c("Dagur 1", "\u00c1r 5", "\u00c1r 10", "\u00c1r 15")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = "."),
                         expand = expansion(mult = c(0.05, 0.15))) +
      labs(title = "Sp\u00e1: fj\u00f6ldi b\u00faa eftir a\u00f0ild",
           subtitle = sprintf("Fr\u00e1 %s \u00ed %s \u00e1 10 \u00e1rum  |  Bygg\u00f0 \u00e1 reynslu Finnlands",
                              format(input$t_farms_now, big.mark = "."),
                              format(t$farms_after, big.mark = ".")),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.3)
      )
  }, res = 110, bg = "transparent")

  # ── Tab 2: Winners/losers table ──

  output$t_winners_table <- renderUI({
    t <- tariff()
    c <- calcs()
    fx <- t$fx

    net_farmer <- c$total - t$total_loss_eur

    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:3px 12px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:600;font-family:Montserrat,sans-serif;">%s</span>', color, label)
    }

    rows <- sprintf('
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Framlei\u00f0endur (b\u00e6ndur)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">\u2212\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">+\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">%s\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-weight:600;">Neytendur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">+\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">R\u00edkissj\u00f3\u00f0ur (tolltek.)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">\u2212tolltekjur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-weight:600;">Dreifbýli / byggðar</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2212%s b\u00fa</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">ANC stu\u00f0n.</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>',
      col_cloud,
      col_red, t$total_loss_eur,
      c$total,
      if (net_farmer >= 0) col_green else col_red,
      if (net_farmer >= 0) "+" else "\u2212",
      abs(net_farmer),
      if (net_farmer >= 0) badge("Jafnv\u00e6gi", col_moss) else badge("Tap", col_red),
      col_moss, t$consumer_save_eur, badge("\u00c1vinningur", col_moss),
      col_cloud,
      col_red,
      badge("Hlutlaust/Tap", col_basalt),
      format(t$farms_lost, big.mark = "."),
      badge("\u00c1h\u00e6ttulegt", col_red)
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      '<th style="padding:8px 14px;text-align:left;font-size:12px;color:', col_grey, ';text-transform:uppercase;">A\u00f0ili</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Tekjutap</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">CAP b\u00e6tur</th>',
      '<th style="padding:8px 14px;text-align:right;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Nett\u00f3</th>',
      '<th style="padding:8px 14px;text-align:center;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Mat</th>',
      '</tr></thead><tbody>',
      rows,
      '</tbody></table>'
    ))
  })
}

shinyApp(ui, server)
