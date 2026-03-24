library(shiny)
library(ggplot2)
library(scales)
# ══════════════════════════════════════════════════════════════════
# INTEGRATED CAP + TARIFF IMPACT MODEL FOR ICELAND
# Rekon / Kári Gautason — March 2026
# TEST TO SEE IF IT WORKS
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
  "Niemi (2003) \u2014 Static Welfare Effects.")

ui <- navbarPage(
  title = div(class = "brand-title", "rekon", span("| CAP stu\u00f0nings\u00e6tlun")),
  id = "main_nav",
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&family=Lora:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(app_css))
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 1 — Niðurstöður (Results) — Full-width, all sections A–G
  # ══════════════════════════════════════════════════════════════
  tabPanel("\U0001f4ca Ni\u00f0urst\u00f6\u00f0ur", value = "results",
    fluidPage(
      # ── Key Metrics — two rows ──
      fluidRow(
        column(3, div(class = "metric-box loss",
                      div(class = "metric-label", "Tekjutap b\u00e6nda"),
                      uiOutput("metric_loss"))),
        column(3, div(class = "metric-box cap",
                      div(class = "metric-label", "CAP lei\u00f0r\u00e9tt"),
                      uiOutput("metric_cap_adj"))),
        column(3, div(class = "metric-box gap",
                      div(class = "metric-label", "Nett\u00f3 sta\u00f0a"),
                      uiOutput("metric_net"))),
        column(3, div(class = "metric-box gain",
                      div(class = "metric-label", "Sparna\u00f0ur neytenda"),
                      uiOutput("metric_consumer")))
      ),
      fluidRow(
        column(4, div(class = "metric-box eu",
                      div(class = "metric-label", "N\u00faverandi kerfi (fastur pottur)"),
                      uiOutput("metric_fixed_pot"))),
        column(4, div(class = "metric-box farm",
                      div(class = "metric-label", "Stu\u00f0ningur \u00e1 b\u00fa \u2014 n\u00faverandi kerfi"),
                      uiOutput("metric_per_farm_current"))),
        column(4, div(class = "metric-box rate",
                      div(class = "metric-label", "Tapa\u00f0ur p\u00fa\u00f0i"),
                      uiOutput("metric_cushion_lost")))
      ),

      hr(class = "section-divider"),

      # ── Section A: Verðáhrif ──
      h4("A. Ver\u00f0\u00e1hrif \u2014 \u00c1hrif tollaafn\u00e1ms \u00e1 framlei\u00f0endaver\u00f0",
         tags$span(class = "source-badge", style = paste0("background:", col_anc), "Hagfr\u00e6\u00f0i")),
      plotOutput("price_impact_chart", height = "380px"),

      hr(class = "section-divider"),

      # ── Section B: Framleiðsluviðbrögð ──
      h4("B. Framlei\u00f0sluvið br\u00f6g\u00f0 \u2014 Samdr\u00e1ttur vegna ver\u00f0l\u00e6kkunar",
         tags$span(class = "source-badge", style = paste0("background:", col_anc), "Hagfr\u00e6\u00f0i")),
      div(class = "context-box",
        HTML("<strong>Keðjuáhrif in:</strong> Ver\u00f0l\u00e6kkun \u2192 frambo\u00f0steygni \u2192 samdr\u00e1ttur \u00ed framlei\u00f0slu.
        Td. ef mj\u00f3lkurver\u00f0 l\u00e6kkar um 30% og teygni er 0.4, \u00fe\u00e1 dregst framlei\u00f0sla saman um 12%.")
      ),
      plotOutput("production_response_chart", height = "400px"),

      hr(class = "section-divider"),

      # ── Section C: System Comparison ──
      h4("C. Kerfismunur \u2014 Fastur pottur (n\u00faverandi) vs. per-unit (CAP)",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Sta\u00f0a + Hagfr\u00e6\u00f0i")),
      div(class = "context-box",
        HTML("<strong>Lykilmunur:</strong> B\u00favörusamningar eru <em>fastur pottur</em> \u2014 \u00feegar b\u00fa h\u00e6tta,
        skiptist sama upph\u00e6\u00f0 \u00e1 f\u00e6rri einingar og stu\u00f0ningur \u00e1 b\u00fa <em>h\u00e6kkar</em>.
        CAP er <em>per-unit</em> kerfi \u2014 \u00feegar framlei\u00f0sla minnkar, l\u00e6kka grei\u00f0slur \u00ed hlutfalli.
        \u00debetta \u00fey\u00f0ir a\u00f0 yfirfærsla \u00ed CAP kostar ekki bara tollverni\u00f0na, heldur einnig
        \u00feennan innbygg\u00f0an p\u00fa\u00f0a n\u00faverandi kerfis.")
      ),
      plotOutput("system_comparison_chart", height = "420px"),
      uiOutput("system_comparison_table"),

      hr(class = "section-divider"),

      # ── Section D: CAP stuðningur ──
      h4("D. CAP stu\u00f0ningur \u2014 Lei\u00f0r\u00e9tt fyrir framlei\u00f0slusamdr\u00e6tti",
         tags$span(class = "source-badge", style = paste0("background:", col_p1), "ESB Samningur")),
      plotOutput("cap_comparison_chart", height = "420px"),
      uiOutput("payer_bar_html"),

      hr(class = "section-divider"),

      # ── Section E: Heildarjafnvægi ──
      h4("E. Heildarjafnv\u00e6gi \u2014 Tekjutap vs. CAP vs. Neytendur",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      fluidRow(
        column(6, plotOutput("balance_chart", height = "340px")),
        column(6, plotOutput("farm_projection", height = "340px"))
      ),

      hr(class = "section-divider"),

      # ── Section F: Winners/losers ──
      h4("F. \u00c1vinningur og kostna\u00f0ur \u2014 Hver hagnast?",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      uiOutput("winners_table"),

      hr(class = "section-divider"),

      # ── Section G: Detail breakdown ──
      h4("G. Sundurli\u00f0un \u2014 Grunnl\u00edna vs. Lei\u00f0r\u00e9tt",
         tags$span(class = "source-badge", style = paste0("background:", col_accent), "Allt")),
      uiOutput("detail_table_html"),

      p(class = "source-note",
        "Athugasemd: \u00dEetta l\u00edkan er samþætt. Tollaafn\u00e1m leiðir til ver\u00f0l\u00e6kkunar, ",
        "sem dregur \u00far framlei\u00f0slu (eftir frambo\u00f0steygni), ",
        "sem aftur l\u00e6kkar CAP grei\u00f0slur. Auk \u00fe ess er tekið tillit til kerfismunarins: ",
        "B\u00favörusamningar eru fastur pottur (samdr\u00e1tt = meiri stu\u00f0ningur \u00e1 b\u00fa), ",
        "en CAP er per-unit kerfi (samdr\u00e1ttur = l\u00e6gri heildargreiðslur). ",
        "\u00dEessi falinn kostna\u00f0ur er s\u00fdndur \u00ed kafla C. ",
        "Raunveruleg \u00e1hrif fara eftir a\u00f0l\u00f6gunart\u00edma, ",
        "s\u00e9r\u00e1kvæðum \u00ed a\u00f0ildarsamningi og vi\u00f0br\u00f6gðum framlei\u00f0enda. ",
        "Finnland fékk 5 ára a\u00f0l\u00f6gunart\u00edmabil.")
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 2 — ESB Samningur (Negotiation)
  # ══════════════════════════════════════════════════════════════
  tabPanel("\U0001f527 ESB Samningur", value = "negotiation",
    sidebarLayout(
      sidebarPanel(
        width = 4,

        div(class = "context-box",
          HTML("<strong>ESB samningsf orsendur:</strong> Stilltu CAP grei\u00f0sluhlutf\u00f6ll til a\u00f0 kanna
          hva\u00f0 Ísland g\u00e6ti fengið \u00ed samningum. Forsendurnar h\u00e9r endurspegla m\u00f6guleg útkomu \u2014
          n\u00faverandi EU bil \u00e1 grei\u00f0slum og sambærilegar ni\u00f0urst\u00f6\u00f0ur Finnlands og Svíþjóðar 1995.")
        ),

        div(class = "sidebar-section", "CAP grei\u00f0sluhlutf\u00f6ll"),
        sliderInput("p1_rate", "Sto\u00f0 1: Grei\u00f0sla \u00e1 hektara (\u20ac/ha)",
                    min = 100, max = 400, value = 200, step = 10, pre = "\u20ac"),
        helpText("EU l\u00e1gmark: \u20ac200 (2023), \u20ac215 (2027). EU me\u00f0altal: ~\u20ac243."),
        sliderInput("milk_rate", "Mj\u00f3lkurgrei\u00f0sla (\u20ac/l\u00edtra)",
                    min = 0.04, max = 0.15, value = 0.09, step = 0.01, pre = "\u20ac"),
        helpText("Finnland: \u20ac0.07 (su\u00f0ur) \u2013 \u20ac0.11 (Lappland)"),
        sliderInput("ewe_rate", "Grei\u00f0sla \u00e1 á (\u20ac/kind)",
                    min = 10, max = 40, value = 25, step = 1, pre = "\u20ac"),
        helpText("ESB h\u00e1mark \u00ed tengdum grei\u00f0slum: \u20ac28/kind."),
        sliderInput("cattle_rate", "Grei\u00f0sla \u00e1 grip (\u20ac/grip)",
                    min = 100, max = 400, value = 200, step = 10, pre = "\u20ac"),
        helpText("Finnland: \u20ac150\u2013350 eftir sv\u00e6\u00f0i og tegund. Me\u00f0altal ~\u20ac200."),
        sliderInput("anc_rate", "ANC grei\u00f0sla \u00e1 hektara (\u20ac/ha)",
                    min = 100, max = 450, value = 250, step = 10, pre = "\u20ac"),
        helpText("H\u00e1mark nor\u00f0an 62\u00b0: \u20ac450/ha. Finnland me\u00f0altal: ~\u20ac217/ha."),
        sliderInput("eu_cofinance", "ESB-hlutfall af ANC (%)",
                    min = 30, max = 75, value = 55, step = 5, post = "%"),
        helpText("Fer eftir \u00fe r\u00f3unarstigi sv\u00e6\u00f0is. H\u00e1\u00fero\u00f0a\u00f0 r\u00edki: ~40%. Ja\u00f0arsv\u00e6\u00f0i: ~55\u201375%."),

        actionButton("go_results_2", "\u2192 Sj\u00e1 allar ni\u00f0urst\u00f6\u00f0ur",
                     class = "btn btn-primary", style = "width:100%;margin-top:16px;"),

        hr(),
        source_note_text
      ),

      mainPanel(
        width = 8,

        h4("Bein fors\u00fdn \u2014 CAP grei\u00f0slur"),
        p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
          "Uppf\u00e6rist \u00ed raunt\u00edma vi\u00f0 breytingar"),

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
  tabPanel("\U0001f4cb N\u00faverandi sta\u00f0a", value = "status",
    fluidPage(
      h4("Grunnforsendur og framlei\u00f0sla \u00ed dag"),
      p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
        "Stilltu grunnforsendur og framlei\u00f0slut\u00f6lur til a\u00f0 a\u00f0laga l\u00edkani\u00f0 a\u00f0 raunverulegri st\u00f6\u00f0u."),

      fluidRow(
        column(6,
          wellPanel(
            style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
            h4("Grunnforsendur", style = "margin-top:0;"),
            sliderInput("eligible_ha", "Styrkhæft land (ha)",
                        min = 80000, max = 400000, value = 200000, step = 10000,
                        pre = "", post = " ha"),
            helpText("120k = r\u00e6kta\u00f0 land. 200k = r\u00e6kta\u00f0 + via\u00f0haldia\u00f0 t\u00fan. Source: LbhÍ 2025, FAO."),
            sliderInput("current_support_isk", "N\u00faverandi stu\u00f0ningur (ma.kr.)",
                        min = 14, max = 30, value = 18, step = 0.5,
                        pre = "", post = " ma.kr."),
            helpText("B\u00favörusamningar."),
            numericInput("eur_isk", "Gengi EUR/ISK", value = 143, min = 100, max = 200, step = 1),
            sliderInput("farms_now", "Fj\u00f6ldi b\u00faa \u00ed dag",
                        min = 1500, max = 4000, value = 2500, step = 100)
          )
        ),
        column(6,
          wellPanel(
            style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
            h4("Framlei\u00f0sla \u00ed dag", style = "margin-top:0;"),
            sliderInput("milk_litres", "Mj\u00f3lkurframlei\u00f0sla (M l\u00edtr.)",
                        min = 120, max = 170, value = 145, step = 5, post = "M L"),
            sliderInput("ewes", "\u00c6r (fj\u00f6ldi)",
                        min = 200000, max = 500000, value = 400000, step = 10000),
            sliderInput("cattle", "Nautgripir (fj\u00f6ldi)",
                        min = 20000, max = 50000, value = 31000, step = 1000),
            helpText("Framlei\u00f0sluver\u00f0mæti \u00e1 n\u00faverandi innlendu ver\u00f0i:"),
            sliderInput("dairy_val", "Mj\u00f3lkurvörur (ma.kr.)",
                        min = 10, max = 30, value = 19, step = 0.5, post = " ma.kr."),
            sliderInput("lamb_val", "Kindakjöt (ma.kr.)",
                        min = 3, max = 15, value = 7, step = 0.5, post = " ma.kr."),
            sliderInput("beef_val", "Nautakjöt (ma.kr.)",
                        min = 1, max = 8, value = 3, step = 0.5, post = " ma.kr."),
            sliderInput("egg_val", "Egg og alifuglar (ma.kr.)",
                        min = 0.5, max = 5, value = 2, step = 0.5, post = " ma.kr."),
            sliderInput("veg_val", "Gr\u00e6nmeti og gr\u00f3\u00f0urh\u00fas (ma.kr.)",
                        min = 1, max = 8, value = 4, step = 0.5, post = " ma.kr.")
          )
        )
      ),

      actionButton("go_results_3", "\u2192 Sj\u00e1 ni\u00f0urst\u00f6\u00f0ur",
                   class = "btn btn-primary", style = "margin-top:12px;"),

      hr(),
      source_note_text
    )
  ),

  # ══════════════════════════════════════════════════════════════
  # TAB 4 — Forsendur [Advanced]
  # ══════════════════════════════════════════════════════════════
  tabPanel("\u2699\ufe0f Forsendur [Advanced]", value = "assumptions",
    fluidPage(
      h4("Hagfr\u00e6\u00f0ilegar forsendur \u2014 Advanced stillingar"),
      p(style = "color:#6B7280; font-size:12px; margin-top:-8px; margin-bottom:16px;",
        "Stilltu tollaáhrif, frambo\u00f0sviðbrög\u00f0 og adrar hagfr\u00e6\u00f0ilegar forsendur."),

      # WellPanel 1: Tollaáhrif
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Tollaáhrif \u2014 Ver\u00f0l\u00e6kkun", style = "margin-top:0;"),
        div(class = "context-box",
          HTML("<strong>Byggt \u00e1 reynslu Finnlands 1995:</strong> \u00dEegar Finnland gekk til li\u00f0s vi\u00f0 ESB,
          l\u00e6kkuu\u00f0u framlei\u00f0endaver\u00f0 verulega. Mj\u00f3lk: \u221228\u201332%, nautakjöt: \u221238\u201343%,
          egg: \u221265\u201368%. \u00cdsland \u00e6tti \u00e1 a\u00f0 fara saman\u00feer\u00f0ar.")
        ),
        fluidRow(
          column(6,
            sliderInput("dairy_drop", "Mj\u00f3lk \u2014 ver\u00f0l\u00e6kkun",
                        min = 10, max = 50, value = 30, step = 1, post = "%"),
            helpText("Finnland: \u221228% til \u221232%."),
            sliderInput("lamb_drop", "Kindakjöt \u2014 ver\u00f0l\u00e6kkun",
                        min = 15, max = 60, value = 45, step = 1, post = "%"),
            helpText("Finnland nautakjöt: \u221238% til \u221243%. Lambakjöt líklega hærra."),
            sliderInput("beef_drop", "Nautakjöt \u2014 ver\u00f0l\u00e6kkun",
                        min = 15, max = 55, value = 40, step = 1, post = "%")
          ),
          column(6,
            sliderInput("egg_drop", "Egg/alifuglar \u2014 ver\u00f0l\u00e6kkun",
                        min = 20, max = 70, value = 55, step = 1, post = "%"),
            helpText("Finnland egg: \u221265% til \u221268%. \u00cdsland l\u00edkl. l\u00e6gra v/ flutnkostn."),
            sliderInput("veg_drop", "Gr\u00e6nmeti \u2014 ver\u00f0l\u00e6kkun",
                        min = 5, max = 40, value = 20, step = 1, post = "%"),
            helpText("Hluti r\u00e6ktar \u00ed gr\u00f3\u00f0urh\u00fasum m/ jar\u00f0varma. Minni samkeppnisáhrif.")
          )
        )
      ),

      # WellPanel 2: Framboðsviðbrögð
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Frambo\u00f0svi\u00f0br\u00f6g\u00f0", style = "margin-top:0;"),
        div(class = "context-box",
          HTML("<strong>Frambo\u00f0steygni:</strong> H versu mikið framlei\u00f0sla l\u00e6kkar \u00feegar ver\u00f0 l\u00e6kkar.
          Teygni 0.4 \u00feý\u00f0ir a\u00f0 10% ver\u00f0l\u00e6kkun lei\u00f0ir til 4% framlei\u00f0slul\u00e6kkunar.
          Bygg\u00f0 \u00e1 Kola et al. (2000) og reynslu Nor\u00f0urlanda.")
        ),
        fluidRow(
          column(6,
            sliderInput("elast_dairy", "Mj\u00f3lkurteygni",
                        min = 0.1, max = 0.8, value = 0.4, step = 0.05),
            helpText("10% ver\u00f0l\u00e6kkun \u2192 4% framlei\u00f0slul\u00e6kkun (vi\u00f0 0.4)"),
            sliderInput("elast_sheep", "Sau\u00f0fj\u00e1rteygni",
                        min = 0.1, max = 1.0, value = 0.5, step = 0.05)
          ),
          column(6,
            sliderInput("elast_beef", "Nautgripateygni",
                        min = 0.1, max = 0.8, value = 0.4, step = 0.05),
            sliderInput("land_abandon", "Landyfirgefning (%)",
                        min = 5, max = 40, value = 15, step = 1, post = "%"),
            helpText("Hlutfall lands sem fellur \u00far notkun vegna samdr\u00e1ttar."),
            helpText("Finnland: mj\u00f3lk 0.3\u20130.5, sau\u00f0fé 0.4\u20130.7. Bygg\u00f0 \u00e1 Kola et al. (2000)")
          )
        )
      ),

      # WellPanel 3: Neytendur og bústöðvun
      wellPanel(
        style = "background:#fff; border:1px solid #E5E7EB; border-radius:8px; padding:20px;",
        h4("Neytendur og b\u00fast\u00f6\u00f0vun", style = "margin-top:0;"),
        fluidRow(
          column(6,
            sliderInput("food_spend", "Matarútgjöld heimila (ma.kr./\u00e1r)",
                        min = 100, max = 250, value = 170, step = 10, post = " ma.kr."),
            helpText("Hagstofa: ~170 ma.kr. Matvælaverð \u00e1 \u00cdsland i 48% yfir ESB-me\u00f0altali."),
            sliderInput("consumer_drop", "\u00c1ætlu\u00f0 matvælaverðl\u00e6kkun (%)",
                        min = 5, max = 30, value = 15, step = 1, post = "%"),
            helpText("Finnland: \u221211%. \u00cdsland l\u00edklega meira (st\u00e6rri tollgat).")
          ),
          column(6,
            sliderInput("farm_exit_10y", "B\u00fast\u00f6\u00f0vun \u00e1 10 \u00e1rum (%)",
                        min = 10, max = 60, value = 35, step = 1, post = "%"),
            helpText("Finnland: \u221238% \u00e1 17 \u00e1rum. Mj\u00f3lkurb\u00fa: \u221282% \u00e1 24 \u00e1rum.")
          )
        )
      ),

      actionButton("go_results_4", "\u2192 Sj\u00e1 ni\u00f0urst\u00f6\u00f0ur",
                   class = "btn btn-primary", style = "margin-top:12px;"),

      hr(),
      source_note_text
    )
  )
)

# ══════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

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
    b <- baseline()
    s <- shock()
    t <- tariff_loss()

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
    farms_after <- t$farms_after
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
  # REACTIVE: TARIFF LOSS — revenue losses from price drops
  # ────────────────────────────────────────────────────────────────
  tariff_loss <- reactive({
    fx <- input$eur_isk
    sectors <- data.frame(
      sector = c("Mj\u00f3lkurvörur", "Kindakjöt", "Nautakjöt", "Egg/alifuglar", "Gr\u00e6nmeti"),
      value_isk = c(input$dairy_val, input$lamb_val, input$beef_val,
                    input$egg_val, input$veg_val),
      drop_pct = c(input$dairy_drop, input$lamb_drop, input$beef_drop,
                   input$egg_drop, input$veg_drop),
      stringsAsFactors = FALSE
    )
    sectors$value_eur <- sectors$value_isk * 1000 / fx
    sectors$loss_eur  <- sectors$value_eur * sectors$drop_pct / 100
    sectors$after_eur <- sectors$value_eur - sectors$loss_eur

    total_loss_eur <- sum(sectors$loss_eur)
    total_before   <- sum(sectors$value_eur)
    total_after    <- sum(sectors$after_eur)

    # Consumer savings
    consumer_save_isk <- input$food_spend * input$consumer_drop / 100
    consumer_save_eur <- consumer_save_isk * 1000 / fx

    # Farm projection
    farms_after <- round(input$farms_now * (1 - input$farm_exit_10y / 100))
    farms_lost  <- input$farms_now - farms_after
    annual_exit <- 1 - (1 - input$farm_exit_10y / 100)^(1/10)
    years <- 0:15
    farm_trajectory <- data.frame(
      year = years,
      farms = round(input$farms_now * (1 - annual_exit)^years)
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

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CAP BASELINE — CAP on original (pre-shock) numbers
  # ────────────────────────────────────────────────────────────────
  cap_baseline <- reactive({
    b <- baseline()
    p1       <- b$ha * input$p1_rate / 1e6
    milk_pay <- b$milk * 1e6 * input$milk_rate / 1e6
    sheep_pay <- b$ewes * input$ewe_rate / 1e6
    cattle_pay <- b$cattle * input$cattle_rate / 1e6
    nordic   <- milk_pay + sheep_pay + cattle_pay
    anc_total <- b$ha * input$anc_rate / 1e6
    anc_eu    <- anc_total * input$eu_cofinance / 100
    anc_is    <- anc_total * (1 - input$eu_cofinance / 100)
    total    <- p1 + nordic + anc_total
    eu_pays      <- p1 + anc_eu
    iceland_pays <- nordic + anc_is

    list(
      p1 = p1, milk = milk_pay, sheep = sheep_pay, cattle = cattle_pay,
      nordic = nordic,
      anc_total = anc_total, anc_eu = anc_eu, anc_is = anc_is,
      total = total,
      eu_pays = eu_pays, iceland_pays = iceland_pays
    )
  })

  # ────────────────────────────────────────────────────────────────
  # REACTIVE: CAP ADJUSTED — CAP on post-shock production numbers
  # ────────────────────────────────────────────────────────────────
  cap_adjusted <- reactive({
    s <- shock()
    p1         <- s$new_ha * input$p1_rate / 1e6
    milk_pay   <- s$new_milk * 1e6 * input$milk_rate / 1e6
    sheep_pay  <- s$new_ewes * input$ewe_rate / 1e6
    cattle_pay <- s$new_cattle * input$cattle_rate / 1e6
    nordic     <- milk_pay + sheep_pay + cattle_pay
    anc_total  <- s$new_ha * input$anc_rate / 1e6
    anc_eu     <- anc_total * input$eu_cofinance / 100
    anc_is     <- anc_total * (1 - input$eu_cofinance / 100)
    total      <- p1 + nordic + anc_total
    eu_pays      <- p1 + anc_eu
    iceland_pays <- nordic + anc_is

    list(
      p1 = p1, milk = milk_pay, sheep = sheep_pay, cattle = cattle_pay,
      nordic = nordic,
      anc_total = anc_total, anc_eu = anc_eu, anc_is = anc_is,
      total = total,
      eu_pays = eu_pays, iceland_pays = iceland_pays
    )
  })

  # ────────────────────────────────────────────────────────────────
  # METRIC BOXES
  # ────────────────────────────────────────────────────────────────

  output$metric_loss <- renderUI({
    t <- tariff_loss()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("\u2212\u20ac%.0fM", t$total_loss_eur)),
      div(class = "metric-sub", sprintf("%.0f%% af framlei\u00f0sluvermi", t$total_loss_eur / t$total_before * 100))
    )
  })

  output$metric_cap_adj <- renderUI({
    ca <- cap_adjusted()
    cb <- cap_baseline()
    diff_pct <- (ca$total - cb$total) / cb$total * 100
    tagList(
      div(class = "metric-value", style = paste0("color:", col_green),
          sprintf("\u20ac%.0fM", ca$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f%% l\u00e6gra en grunnl\u00edna (\u20ac%.0fM)", abs(diff_pct), cb$total))
    )
  })

  output$metric_net <- renderUI({
    ca <- cap_adjusted()
    t <- tariff_loss()
    b <- baseline()
    # Farmers lose: (1) revenue from price drops + (2) current Búvörusamningar
    # Farmers gain: CAP payments
    total_farmer_loss <- t$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    col <- if (net >= 0) col_green else col_red
    sign <- if (net >= 0) "+" else "\u2212"
    tagList(
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%s\u20ac%.0fM", sign, abs(net))),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("CAP \u20ac%.0fM vs. tap \u20ac%.0fM + n\u00fav. stu\u00f0n. \u20ac%.0fM",
                  ca$total, t$total_loss_eur, b$current_eur))
    )
  })

  output$metric_consumer <- renderUI({
    t <- tariff_loss()
    tagList(
      div(class = "metric-value", style = paste0("color:", col_moss),
          sprintf("+\u20ac%.0fM", t$consumer_save_eur)),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("%.0f ma.kr./\u00e1r l\u00e6gra matarver\u00f0", t$consumer_save_isk))
    )
  })

  # ── Fixed-pot metrics ──

  output$metric_fixed_pot <- renderUI({
    b <- baseline()
    div(
      div(class = "metric-value", sprintf("\u20ac%.0fM", b$current_eur)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          "Fastur \u2014 breytist ekki \u00feegar b\u00fa h\u00e6tta")
    )
  })

  output$metric_per_farm_current <- renderUI({
    cs <- current_system()
    tagList(
      div(class = "metric-value", sprintf("\u20ac%s", format(round(cs$per_farm_now), big.mark = "."))),
      div(class = "metric-sub", style = paste0("color:", col_moss),
          sprintf("\u2192 \u20ac%s eftir b\u00fast\u00f6\u00f0vun (+%.0f%%)",
                  format(round(cs$per_farm_after), big.mark = "."),
                  cs$farm_cushion))
    )
  })

  output$metric_cushion_lost <- renderUI({
    cs <- current_system()
    ca <- cap_adjusted()
    b <- baseline()
    t <- tariff_loss()
    # Under current system, surviving farms keep the full pot
    # Under CAP, surviving farms get reduced total (production-linked)
    # The difference is the "cushion lost"
    cap_per_farm <- ca$total * 1e6 / t$farms_after
    cushion_lost_per_farm <- cs$per_farm_after - cap_per_farm
    tagList(
      div(class = "metric-value", style = paste0("color:", col_red),
          sprintf("\u2212\u20ac%s/b\u00fa", format(round(cushion_lost_per_farm), big.mark = "."))),
      div(class = "metric-sub",
          sprintf("N\u00fav. kerfi: \u20ac%s/b\u00fa vs CAP: \u20ac%s/b\u00fa",
                  format(round(cs$per_farm_after), big.mark = "."),
                  format(round(cap_per_farm), big.mark = ".")))
    )
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION A: Price Impact Chart
  # ────────────────────────────────────────────────────────────────

  output$price_impact_chart <- renderPlot({
    t <- tariff_loss()
    s <- t$sectors

    df <- data.frame(
      sector = factor(rep(s$sector, 2), levels = rev(s$sector)),
      type = rep(c("Fyrir", "Eftir"), each = nrow(s)),
      value = c(s$value_eur, s$after_eur),
      stringsAsFactors = FALSE
    )
    df$type <- factor(df$type, levels = c("Fyrir", "Eftir"))

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

  # ────────────────────────────────────────────────────────────────
  # SECTION B: Production Response Chart
  # ────────────────────────────────────────────────────────────────

  output$production_response_chart <- renderPlot({
    b <- baseline()
    s <- shock()

    categories <- c("Mj\u00f3lk\n(M l\u00edtrar)", "\u00c6r\n(\u00fe\u00fas.)", "Nautgripir\n(\u00fe\u00fas.)", "Land\n(\u00fe\u00fas. ha)")
    before_vals <- c(b$milk, b$ewes / 1000, b$cattle / 1000, b$ha / 1000)
    after_vals  <- c(s$new_milk, s$new_ewes / 1000, s$new_cattle / 1000, s$new_ha / 1000)
    pct_changes <- c(s$dairy_pct_change, s$sheep_pct_change, s$beef_pct_change, s$land_pct_change)

    df <- data.frame(
      category = factor(rep(categories, 2), levels = rev(categories)),
      type = factor(rep(c("Fyrir", "Eftir"), each = 4), levels = c("Fyrir", "Eftir")),
      value = c(before_vals, after_vals),
      stringsAsFactors = FALSE
    )

    ann <- data.frame(
      category = factor(categories, levels = rev(categories)),
      y_pos = before_vals * 1.02,
      label = sprintf("\u2212%.0f%%", pct_changes)
    )

    ggplot(df, aes(x = category, y = value, fill = type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(data = ann, aes(x = category, y = y_pos, label = label),
                inherit.aes = FALSE, hjust = -0.15, size = 4,
                colour = col_red, fontface = "bold", family = "Montserrat") +
      scale_fill_manual(values = c("Fyrir" = col_before, "Eftir" = col_green),
                        labels = c("Grunnl\u00edna", "Eftir a\u00f0l\u00f6gun")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(title = "Framlei\u00f0sluvið br\u00f6g\u00f0 \u2014 Samdr\u00e1ttur vegna ver\u00f0l\u00e6kkunar",
           subtitle = sprintf("Mj\u00f3lk: \u2212%.0f%%  |  Sau\u00f0f\u00e9: \u2212%.0f%%  |  Nautgr.: \u2212%.0f%%  |  Land: \u2212%.0f%%",
                              s$dairy_pct_change, s$sheep_pct_change, s$beef_pct_change, s$land_pct_change),
           x = NULL, y = NULL, fill = NULL) +
      rekon_theme() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # SECTION C: System Comparison — Fixed pot vs CAP per-unit
  # ────────────────────────────────────────────────────────────────

  output$system_comparison_chart <- renderPlot({
    b <- baseline()
    cs <- current_system()
    ca <- cap_adjusted()
    t <- tariff_loss()

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
      system = factor(rep(c("N\u00faverandi kerfi\n(fastur pottur)", "CAP\n(per-unit)"), each = length(exit_levels)),
                      levels = c("N\u00faverandi kerfi\n(fastur pottur)", "CAP\n(per-unit)")),
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
               label = sprintf("Sp\u00e1 b\u00fast.: %.0f%%", actual_exit_pct),
               hjust = 0, size = 3.3, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_ribbon(data = data.frame(
                    exit = exit_levels,
                    ymin = cap_per_farm / 1000,
                    ymax = current_per_farm / 1000),
                  aes(x = exit, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = col_red, alpha = 0.08) +
      annotate("text",
               x = mean(exit_levels), y = mean(c(current_per_farm[6], cap_per_farm[6])) / 1000,
               label = "Tapa\u00f0ur p\u00fa\u00f0i", colour = col_red, size = 3.5,
               fontface = "italic", family = "Lora") +
      scale_colour_manual(values = c(col_p1, col_red)) +
      scale_linewidth_manual(values = c(1.5, 1.5)) +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         breaks = seq(0, 50, 10)) +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "k")) +
      labs(title = "Stu\u00f0ningur \u00e1 b\u00fa \u2014 N\u00faverandi kerfi vs. CAP",
           subtitle = "N\u00faverandi kerfi: fastur pottur styrkist vi\u00f0 b\u00fast\u00f6\u00f0vun  |  CAP: per-unit grei\u00f0slur dragast saman",
           x = "B\u00fast\u00f6\u00f0vun (%)", y = "Stu\u00f0ningur \u00e1 b\u00fa (\u20ac\u00fe\u00fas.)",
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
    t <- tariff_loss()

    cap_per_farm <- ca$total * 1e6 / t$farms_after

    HTML(sprintf('
      <div style="margin:18px 0; font-family:Montserrat,sans-serif;">
        <table style="width:100%%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">
          <thead><tr style="border-bottom:2px solid %s;background:%s;">
            <th style="padding:8px 14px;text-align:left;font-size:12px;color:%s;text-transform:uppercase;">M\u00e6likvar\u00f0i</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">N\u00fav. kerfi (\u00ed dag)</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">N\u00fav. kerfi (eftir b\u00fast.)</th>
            <th style="padding:8px 14px;text-align:right;font-size:12px;color:%s;text-transform:uppercase;">CAP (lei\u00f0r\u00e9tt)</th>
          </tr></thead>
          <tbody>
            <tr style="background:%s;">
              <td style="padding:8px 14px;font-weight:600;">Heildarpottur (\u20acM)</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">\u20ac%.0fM</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">\u20ac%.0fM</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">\u20ac%.0fM</td>
            </tr>
            <tr>
              <td style="padding:8px 14px;font-weight:600;">Fj\u00f6ldi b\u00faa</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
              <td style="padding:8px 14px;text-align:right;">%s</td>
            </tr>
            <tr style="background:%s;">
              <td style="padding:8px 14px;font-weight:600;">Stu\u00f0ningur \u00e1 b\u00fa (\u20ac)</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;">\u20ac%s</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">\u20ac%s</td>
              <td style="padding:8px 14px;text-align:right;font-weight:700;color:%s;">\u20ac%s</td>
            </tr>
            <tr>
              <td style="padding:8px 14px;font-weight:600;">Breyting \u00e1 b\u00fa</td>
              <td style="padding:8px 14px;text-align:right;">\u2014</td>
              <td style="padding:8px 14px;text-align:right;color:%s;font-weight:700;">+%.0f%%</td>
              <td style="padding:8px 14px;text-align:right;color:%s;font-weight:700;">%.0f%%</td>
            </tr>
            <tr style="border-top:2px solid %s;background:%s;">
              <td style="padding:10px 14px;font-weight:700;color:%s;">Tapa\u00f0ur p\u00fa\u00f0i \u00e1 b\u00fa</td>
              <td style="padding:10px 14px;" colspan="2"></td>
              <td style="padding:10px 14px;text-align:right;font-weight:700;color:%s;font-size:15px;">\u2212\u20ac%s/b\u00fa</td>
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
      format(t$farms_after, big.mark = "."),
      format(t$farms_after, big.mark = "."),
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

    labels <- c("Sto\u00f0 1\nGrunnl.", "Sto\u00f0 1\nLei\u00f0r.",
                "Art. 142\nGrunnl.", "Art. 142\nLei\u00f0r.",
                "ANC\nGrunnl.", "ANC\nLei\u00f0r.",
                "Samtals\nGrunnl.", "Samtals\nLei\u00f0r.")

    values <- c(cb$p1, ca$p1,
                cb$nordic, ca$nordic,
                cb$anc_total, ca$anc_total,
                cb$total, ca$total)

    fills <- c(col_before, col_p1,
               col_before, col_nordic,
               col_before, col_anc,
               col_before, col_accent)

    df <- data.frame(
      label = factor(labels, levels = labels),
      value = values,
      fill = fills,
      stringsAsFactors = FALSE
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = b$current_eur, linetype = "dashed", colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 8.4, y = b$current_eur + 2,
               label = sprintf("N\u00faverandi: \u20ac%.0fM", b$current_eur),
               hjust = 1, size = 3.2, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_text(aes(label = sprintf("\u20ac%.0fM", value)),
                vjust = -0.5, size = 3.2, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = "CAP stu\u00f0ningur \u2014 Grunnl\u00edna vs. Lei\u00f0r\u00e9tt",
           subtitle = sprintf("Grunnl\u00edna: \u20ac%.0fM  |  Lei\u00f0r\u00e9tt (eftir framlei\u00f0slusamdr\u00e1tt): \u20ac%.0fM  |  Munur: \u2212\u20ac%.0fM",
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
          Hver borgar? (lei\u00f0r\u00e9tt) &mdash; ESB: \u20ac%.0fM (%s%%) &nbsp;|&nbsp; \u00cdsland: \u20ac%.0fM (%s%%)
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
      ca$eu_pays, eu_pct, ca$iceland_pays, is_pct,
      eu_pct, col_p1, ca$eu_pays,
      is_pct, col_accent, ca$iceland_pays
    ))
  })

  # ────────────────────────────────────────────────────────────────
  # SECTION E: Balance chart + Farm projection
  # ────────────────────────────────────────────────────────────────

  output$balance_chart <- renderPlot({
    t <- tariff_loss()
    ca <- cap_adjusted()
    b <- baseline()

    # Total farmer loss = revenue loss from tariffs + loss of current support
    total_farmer_loss <- t$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss

    df <- data.frame(
      label = factor(c("Tekjutap\n(ver\u00f0l\u00e6kkun)", "Tapa\u00f0ur\nstu\u00f0n. pottur",
                       "CAP\nlei\u00f0r\u00e9tt", "Nett\u00f3 sta\u00f0a\nb\u00e6nda",
                       "Sparna\u00f0ur\nneytenda"),
                     levels = c("Tekjutap\n(ver\u00f0l\u00e6kkun)", "Tapa\u00f0ur\nstu\u00f0n. pottur",
                                "CAP\nlei\u00f0r\u00e9tt", "Nett\u00f3 sta\u00f0a\nb\u00e6nda",
                                "Sparna\u00f0ur\nneytenda")),
      value = c(-t$total_loss_eur, -b$current_eur, ca$total, net, t$consumer_save_eur),
      fill  = c(col_red, col_anc, col_green,
                if (net >= 0) col_moss else col_gap, col_consumer)
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.55) +
      geom_hline(yintercept = 0, colour = col_brown, linewidth = 0.5) +
      geom_text(aes(label = sprintf("\u20ac%.0fM", value),
                    vjust = ifelse(value >= 0, -0.5, 1.5)),
                size = 3.8, fontface = "bold", colour = col_brown, family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0.15, 0.15))) +
      labs(title = "Heildarjafnv\u00e6gi b\u00e6nda",
           subtitle = sprintf("Tap: \u20ac%.0fM (ver\u00f0) + \u20ac%.0fM (stu\u00f0n.) = \u20ac%.0fM  |  CAP: \u20ac%.0fM  |  Nett\u00f3: %s\u20ac%.0fM",
                              t$total_loss_eur, b$current_eur, total_farmer_loss,
                              ca$total,
                              if (net >= 0) "+" else "\u2212", abs(net)),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        axis.text.x = element_text(size = 9, lineheight = 1.1)
      )
  }, res = 110, bg = "transparent")

  output$farm_projection <- renderPlot({
    t <- tariff_loss()
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
                              format(input$farms_now, big.mark = "."),
                              format(t$farms_after, big.mark = ".")),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.3)
      )
  }, res = 110, bg = "transparent")

  # ────────────────────────────────────────────────────────────────
  # SECTION F: Winners/losers table
  # ────────────────────────────────────────────────────────────────

  output$winners_table <- renderUI({
    t <- tariff_loss()
    ca <- cap_adjusted()
    b <- baseline()

    # Total farmer loss = revenue loss + lost Búvörusamningar
    total_farmer_loss <- t$total_loss_eur + b$current_eur
    net_farmer <- ca$total - total_farmer_loss

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
        <td style="padding:10px 14px;font-style:italic;color:%s;">
          &nbsp;&nbsp;\u00dear af: tekjutap (ver\u00f0l\u00e6kkun)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">\u2212\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;"></td>
        <td style="padding:10px 14px;"></td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-style:italic;color:%s;">
          &nbsp;&nbsp;\u00dear af: tapa\u00f0ur B\u00favörusm. pottur</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">\u2212\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;"></td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Neytendur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-weight:700;font-family:Montserrat;">+\u20ac%.0fM</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr>
        <td style="padding:10px 14px;font-weight:600;">R\u00edkissj\u00f3\u00f0ur (tolltek.)</td>
        <td style="padding:10px 14px;text-align:right;color:%s;font-family:Montserrat;">\u2212tolltekjur</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>
      <tr style="background:%s;">
        <td style="padding:10px 14px;font-weight:600;">Dreifb\u00fdli / bygg\u00f0ar</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2212%s b\u00fa</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">ANC stu\u00f0n.</td>
        <td style="padding:10px 14px;text-align:right;font-family:Montserrat;">\u2014</td>
        <td style="padding:10px 14px;text-align:center;">%s</td>
      </tr>',
      col_cloud,
      # Farmer total row
      col_red, total_farmer_loss,
      ca$total,
      if (net_farmer >= 0) col_green else col_red,
      if (net_farmer >= 0) "+" else "\u2212",
      abs(net_farmer),
      if (net_farmer >= 0) badge("Jafnv\u00e6gi", col_moss) else badge("Tap", col_red),
      # Sub-row: revenue loss from tariffs
      col_grey,
      col_red, t$total_loss_eur,
      # Sub-row: lost Búvörusamningar pot
      col_grey,
      col_anc, b$current_eur,
      badge("Fastur pottur tapadur", col_anc),
      # Consumer row
      col_cloud,
      col_moss, t$consumer_save_eur, badge("\u00c1vinningur", col_moss),
      # Treasury row
      col_red,
      badge("Hlutlægt/Tap", col_basalt),
      # Rural row
      col_cloud,
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

  # ────────────────────────────────────────────────────────────────
  # SECTION G: Detail breakdown table — Baseline vs Adjusted
  # ────────────────────────────────────────────────────────────────

  output$detail_table_html <- renderUI({
    b <- baseline()
    s <- shock()
    cb <- cap_baseline()
    ca <- cap_adjusted()

    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:3px 12px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:600;font-family:Montserrat,sans-serif;">%s</span>', color, label)
    }
    badge_eu <- badge("ESB", col_p1)
    badge_is <- badge("\u00cdsland", col_accent)
    badge_both <- badge("Samfj\u00e1rm\u00f6gnun", col_anc)

    make_row <- function(label, base_val, adj_val, payer_badge, is_header) {
      bg <- if (is_header) sprintf(' style="background:%s;font-weight:600;"', col_cloud) else ""
      sprintf("<tr%s><td style='padding:7px 12px;font-family:Lora,serif;'>%s</td><td style='padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;font-weight:600;'>%s</td><td style='padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;font-weight:600;'>%s</td><td style='padding:7px 12px;text-align:center;'>%s</td></tr>",
              bg, label, base_val, adj_val, payer_badge)
    }

    rows <- paste0(
      make_row("Sto\u00f0 1: Beingrei\u00f0slur",
               sprintf("\u20ac%.1fM", cb$p1), sprintf("\u20ac%.1fM", ca$p1), badge_eu, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha (grunnl.)",
                       format(b$ha, big.mark = "."), input$p1_rate),
               "", "", "", FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha (lei\u00f0r.)",
                       format(round(s$new_ha), big.mark = "."), input$p1_rate),
               "", "", "", FALSE),
      make_row("", "", "", "", FALSE),
      make_row("Art. 142: Nor\u00f0ursl\u00f3\u00f0astu\u00f0ningur",
               sprintf("\u20ac%.1fM", cb$nordic), sprintf("\u20ac%.1fM", ca$nordic), badge_is, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Mj\u00f3lk: %.0fM L \u2192 %.0fM L",
                       b$milk, s$new_milk),
               sprintf("\u20ac%.1fM", cb$milk), sprintf("\u20ac%.1fM", ca$milk), badge_is, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Sau\u00f0f\u00e9: %s \u00e6r \u2192 %s",
                       format(b$ewes, big.mark = "."), format(round(s$new_ewes), big.mark = ".")),
               sprintf("\u20ac%.1fM", cb$sheep), sprintf("\u20ac%.1fM", ca$sheep), badge_is, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;Nautgripir: %s \u2192 %s",
                       format(b$cattle, big.mark = "."), format(round(s$new_cattle), big.mark = ".")),
               sprintf("\u20ac%.1fM", cb$cattle), sprintf("\u20ac%.1fM", ca$cattle), badge_is, FALSE),
      make_row("", "", "", "", FALSE),
      make_row("Sto\u00f0 2: Har\u00f0b\u00fdlisgrei\u00f0slur (ANC)",
               sprintf("\u20ac%.1fM", cb$anc_total), sprintf("\u20ac%.1fM", ca$anc_total), badge_both, TRUE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;\u00dear af ESB (%s%%)", input$eu_cofinance),
               sprintf("\u20ac%.1fM", cb$anc_eu), sprintf("\u20ac%.1fM", ca$anc_eu), badge_eu, FALSE),
      make_row(sprintf("&nbsp;&nbsp;&nbsp;\u00dear af \u00cdsland (%s%%)", 100 - input$eu_cofinance),
               sprintf("\u20ac%.1fM", cb$anc_is), sprintf("\u20ac%.1fM", ca$anc_is), badge_is, FALSE)
    )

    # Summary rows
    t <- tariff_loss()
    total_farmer_loss <- t$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    net_color <- if (net >= 0) col_green else col_red
    net_sign <- if (net >= 0) sprintf("+\u20ac%.1fM", net) else sprintf("\u2212\u20ac%.1fM", abs(net))

    summary_rows <- sprintf('
      <tr style="border-top:2px solid %s;background:%s;font-weight:700;">
        <td style="padding:10px 12px;font-family:Montserrat,sans-serif;">SAMTALS CAP (n\u00fdtt kerfi)</td>
        <td style="padding:10px 12px;text-align:right;font-family:Montserrat,sans-serif;">\u20ac%.1fM</td>
        <td style="padding:10px 12px;text-align:right;font-family:Montserrat,sans-serif;">\u20ac%.1fM</td>
        <td style="padding:10px 12px;text-align:center;">%s &nbsp; %s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:7px 12px;font-family:Lora,serif;">Tapa\u00f0 n\u00fav. stu\u00f0n. (B\u00favörusm.)</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">\u2212\u20ac%.1fM</td>
        <td style="padding:7px 12px;text-align:center;">%s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:7px 12px;font-family:Lora,serif;">Tekjutap (ver\u00f0l\u00e6kkun)</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">\u2212\u20ac%.1fM</td>
        <td></td>
      </tr>
      <tr style="font-weight:600;background:%s;">
        <td style="padding:7px 12px;font-family:Montserrat,sans-serif;">HEILDARTAP B\u00c6NDA</td>
        <td style="padding:7px 12px;text-align:right;font-family:Montserrat,sans-serif;color:%s;" colspan="2">\u2212\u20ac%.1fM</td>
        <td></td>
      </tr>
      <tr style="border-top:2px solid %s;font-weight:700;">
        <td style="padding:10px 12px;color:%s;font-family:Montserrat,sans-serif;">NETT\u00d3 STA\u00d0A (CAP \u2212 heildartap)</td>
        <td style="padding:10px 12px;text-align:right;color:%s;font-family:Montserrat,sans-serif;font-size:15px;" colspan="2">%s</td>
        <td></td>
      </tr>',
      col_brown, col_cloud,
      cb$total, ca$total,
      badge_eu, badge_is,
      col_anc, b$current_eur, badge_is,
      col_red, t$total_loss_eur,
      col_cloud,
      col_red, total_farmer_loss,
      net_color, net_color, net_color, net_sign
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;border:1px solid #E5E7EB;border-radius:8px;overflow:hidden;">',
      '<thead><tr style="border-bottom:2px solid ', col_brown, ';background:', col_cloud, ';">',
      '<th style="padding:8px 12px;text-align:left;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Li\u00f0ur</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Grunnl\u00edna \u20acM</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Lei\u00f0r\u00e9tt \u20acM</th>',
      '<th style="padding:8px 12px;text-align:center;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;">Grei\u00f0andi</th>',
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
      div(class = "metric-label", "CAP Grunnl\u00edna"),
      div(class = "metric-value", style = paste0("color:", col_p1), sprintf("\u20ac%.0fM", cb$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey), "Fyrir framlei\u00f0slusamdr\u00e1tt")
    )
  })

  output$mini_cap_adjusted <- renderUI({
    ca <- cap_adjusted()
    cb <- cap_baseline()
    diff_pct <- (ca$total - cb$total) / cb$total * 100
    div(class = "metric-box cap",
      div(class = "metric-label", "CAP Lei\u00f0r\u00e9tt"),
      div(class = "metric-value", style = paste0("color:", col_green), sprintf("\u20ac%.0fM", ca$total)),
      div(class = "metric-sub", style = paste0("color:", col_grey),
          sprintf("%.0f%% breyting", diff_pct))
    )
  })

  output$mini_net <- renderUI({
    ca <- cap_adjusted()
    t <- tariff_loss()
    b <- baseline()
    total_farmer_loss <- t$total_loss_eur + b$current_eur
    net <- ca$total - total_farmer_loss
    col <- if (net >= 0) col_green else col_red
    sign <- if (net >= 0) "+" else "\u2212"
    div(class = "metric-box gap",
      div(class = "metric-label", "Nett\u00f3 sta\u00f0a"),
      div(class = "metric-value", style = paste0("color:", col),
          sprintf("%s\u20ac%.0fM", sign, abs(net))),
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
          sprintf("ESB: \u20ac%.0fM | \u00cds: \u20ac%.0fM", ca$eu_pays, ca$iceland_pays))
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
          Hver borgar? (lei\u00f0r\u00e9tt) &mdash; ESB: \u20ac%.0fM (%s%%) &nbsp;|&nbsp; \u00cdsland: \u20ac%.0fM (%s%%)
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
      ca$eu_pays, eu_pct, ca$iceland_pays, is_pct,
      eu_pct, col_p1, ca$eu_pays,
      is_pct, col_accent, ca$iceland_pays
    ))
  })

  output$mini_cap_chart <- renderPlot({
    cb <- cap_baseline()
    ca <- cap_adjusted()
    b <- baseline()

    labels <- c("Sto\u00f0 1\nGrunnl.", "Sto\u00f0 1\nLei\u00f0r.",
                "Art. 142\nGrunnl.", "Art. 142\nLei\u00f0r.",
                "ANC\nGrunnl.", "ANC\nLei\u00f0r.",
                "Samtals\nGrunnl.", "Samtals\nLei\u00f0r.")

    values <- c(cb$p1, ca$p1,
                cb$nordic, ca$nordic,
                cb$anc_total, ca$anc_total,
                cb$total, ca$total)

    fills <- c(col_before, col_p1,
               col_before, col_nordic,
               col_before, col_anc,
               col_before, col_accent)

    df <- data.frame(
      label = factor(labels, levels = labels),
      value = values,
      fill = fills,
      stringsAsFactors = FALSE
    )

    ggplot(df, aes(x = label, y = value, fill = fill)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = b$current_eur, linetype = "dashed", colour = col_brown, linewidth = 0.7) +
      annotate("text", x = 8.4, y = b$current_eur + 2,
               label = sprintf("N\u00faverandi: \u20ac%.0fM", b$current_eur),
               hjust = 1, size = 2.8, colour = col_brown, fontface = "bold", family = "Montserrat") +
      geom_text(aes(label = sprintf("\u20ac%.0fM", value)),
                vjust = -0.5, size = 2.8, colour = col_brown,
                fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = "CAP stu\u00f0ningur \u2014 Grunnl\u00edna vs. Lei\u00f0r\u00e9tt",
           subtitle = sprintf("Grunnl\u00edna: \u20ac%.0fM  |  Lei\u00f0r\u00e9tt: \u20ac%.0fM  |  Munur: \u2212\u20ac%.0fM",
                              cb$total, ca$total, cb$total - ca$total),
           x = NULL, y = NULL) +
      rekon_theme(base_size = 11) +
      theme(axis.text.x = element_text(size = 8, lineheight = 1.1))
  }, res = 110, bg = "transparent")

}

shinyApp(ui, server)
