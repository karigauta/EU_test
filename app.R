library(shiny)
library(ggplot2)
library(scales)
# ══════════════════════════════════════════════════════════════════
# CAP SUPPORT ESTIMATION MODEL FOR ICELAND
# Rekon / Kári Gautason — March 2026
#
# Three channels:
#   1. Pillar 1 Core Direct Payments (EU-funded)
#   2. Nordic Aid / Article 142 (nationally funded, production-linked)
#   3. ANC — Areas with Natural Constraints, Pillar 2 (co-financed)
# ══════════════════════════════════════════════════════════════════

# ── Rekon Brand Palette ──────────────────────────────────────────
# Primary
col_sky      <- "#4A90E2"   # Sky Blue (primary brand color)
col_green    <- "#4A5C36"   # Highland Green
col_cloud    <- "#F3F4F6"   # Cloud White (backgrounds)

# Secondary
col_grey     <- "#6B7280"   # Mountain Grey
col_brown    <- "#57534E"   # Soil Brown

# Accent
col_red      <- "#A12B26"   # Signal Red

# Data Visualization (in order)
col_p1       <- "#4A90E2"   # Sky Blue — Pillar 1
col_nordic   <- "#4A5C36"   # Highland Green — Nordic Aid
col_anc      <- "#E2B14A"   # Lichen Gold — ANC
col_gap      <- "#A12B26"   # Signal Red — Gap
col_accent   <- "#57534E"   # Soil Brown — totals/neutral
col_basalt   <- "#5D6D7E"   # Basalt Grey
col_moss     <- "#789A5B"   # Moss Green

# ── UI ───────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    # Google Fonts: Montserrat (headings) + Lora (body)
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&family=Lora:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(sprintf("
      body {
        font-family: 'Lora', Georgia, serif;
        background: %s;
        color: %s;
        font-size: 13px;
      }
      h1, h2, h3, h4, h5, .metric-label, .metric-value, .metric-sub,
      .brand-bar, .brand-title, .brand-sub, th {
        font-family: 'Montserrat', Arial, sans-serif;
      }
      h3 { color: %s; font-weight: 700; }
      h4 { color: %s; font-weight: 600; margin-top: 20px; margin-bottom: 10px; }
      .well { background: #fff; border: 1px solid #E5E7EB; border-radius: 8px; }

      /* ── Brand header bar ── */
      .brand-bar {
        background: %s;
        color: #fff;
        padding: 18px 28px;
        border-radius: 10px;
        margin-bottom: 22px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .brand-title {
        font-size: 22px;
        font-weight: 700;
        letter-spacing: 0.5px;
      }
      .brand-title span { font-weight: 400; opacity: 0.85; font-size: 15px; margin-left: 10px; }
      .brand-sub {
        font-size: 12px;
        opacity: 0.8;
        font-weight: 400;
      }

      /* ── Metric cards ── */
      .metric-box {
        background: #fff;
        border-radius: 8px;
        padding: 16px 18px;
        margin-bottom: 12px;
        border: 1px solid #E5E7EB;
        border-left: 4px solid %s;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .metric-box.eu    { border-left-color: %s; }
      .metric-box.cap   { border-left-color: %s; }
      .metric-box.gap   { border-left-color: %s; }
      .metric-box.rate  { border-left-color: %s; }
      .metric-label {
        font-size: 11px;
        color: %s;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
        margin-bottom: 4px;
      }
      .metric-value { font-size: 26px; font-weight: 700; color: %s; }
      .metric-sub { font-size: 11px; color: %s; font-weight: 600; margin-top: 2px; }

      /* ── Section dividers ── */
      .section-divider {
        border: none;
        border-top: 2px solid %s;
        margin: 24px 0;
      }

      /* ── Source notes ── */
      .source-note {
        font-family: 'Lora', Georgia, serif;
        font-size: 11px;
        color: %s;
        line-height: 1.6;
        margin-top: 10px;
      }

      /* ── Help text ── */
      .help-block {
        font-family: 'Lora', Georgia, serif;
        font-size: 11px;
        color: %s;
      }

      /* ── Sidebar section headers ── */
      .sidebar-section {
        font-family: 'Montserrat', Arial, sans-serif;
        font-size: 13px;
        font-weight: 600;
        color: %s;
        padding: 6px 0 4px 0;
        margin-top: 16px;
        border-bottom: 2px solid %s;
        margin-bottom: 10px;
      }
    ",
    col_cloud, col_brown,          # body bg, text
    col_brown, col_brown,          # h3, h4
    col_accent,                     # brand bar bg
    col_grey,                       # default metric border
    col_p1, col_green, col_red, col_anc,  # metric borders
    col_grey,                       # metric label
    col_brown,                      # metric value
    col_red,                        # metric sub
    col_cloud,                      # section divider
    col_grey,                       # source note
    col_grey,                       # help text
    col_brown, col_p1              # sidebar section
    )))
  ),

  # ── Brand header ──
  div(class = "brand-bar",
    div(
      div(class = "brand-title", "rekon", span("| CAP stuðningsáætlun")),
      div(class = "brand-sub", "Áætlun um stuðning ESB við íslenskan landbúnað — þrjár greiðsluleiðir")
    ),
    div(style = "text-align:right;",
      div(style = "font-size:11px; opacity:0.7;", "Kári Gautason — mars 2026"),
      div(style = "font-size:11px; opacity:0.7;", "Grounded Analysis & Strategic Insight")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # ── Iceland baseline ──
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

      # ── Pillar 1 ──
      div(class = "sidebar-section", "Stoð 1: Beingreiðslur (Pillar 1)"),
      sliderInput("p1_rate", "Greiðsla á hektara (€/ha)",
                  min = 100, max = 400, value = 200, step = 10,
                  pre = "€"),
      helpText("EU lágmark: €200 (2023), €215 (2027). EU meðaltal: ~€243. Nýju ríkin: €130–180."),

      hr(),

      # ── Nordic Aid ──
      div(class = "sidebar-section", "Norðurslóðastuðningur (Art. 142)"),

      sliderInput("milk_litres", "Mjólkurframleiðsla (M lítr.)",
                  min = 120, max = 170, value = 145, step = 5,
                  post = "M L"),
      sliderInput("milk_rate", "Mjólkurgreiðsla (€/lítra)",
                  min = 0.04, max = 0.15, value = 0.09, step = 0.01,
                  pre = "€"),
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

      # ── ANC ──
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
        "Úttekt AMS/HÍ á aðildarviðræðum (2014), Uttekt á stöðu aðildarviðræðna (2014), ",
        "EU Reg. 2021/2115, Wageningen Economic Research, CAP Reform blog, Hagstofa Íslands.")
    ),

    mainPanel(
      width = 8,

      # ── Summary metrics ──
      fluidRow(
        column(3, div(class = "metric-box eu",
                      div(class = "metric-label", "Núverandi stuðningur"),
                      uiOutput("metric_current")
        )),
        column(3, div(class = "metric-box cap",
                      div(class = "metric-label", "CAP heildaráætlun"),
                      uiOutput("metric_cap_total")
        )),
        column(3, div(class = "metric-box gap",
                      div(class = "metric-label", "Munur (gap)"),
                      uiOutput("metric_gap")
        )),
        column(3, div(class = "metric-box rate",
                      div(class = "metric-label", "Nauðsynlegur €/ha"),
                      uiOutput("metric_needed_rate")
        ))
      ),

      # ── Who pays: stacked bar ──
      uiOutput("payer_bar_html"),

      hr(class = "section-divider"),

      # ── Main chart ──
      plotOutput("waterfall_chart", height = "420px"),

      hr(class = "section-divider"),

      # ── Breakdown ──
      fluidRow(
        column(12, plotOutput("component_chart", height = "320px"))
      ),

      hr(class = "section-divider"),

      # ── Detail table ──
      h4("Sundurliðun"),
      uiOutput("detail_table_html"),

      p(class = "source-note",
        "Athugasemd: Allar tölur eru áætlaðar á grundvelli bestu þekkingar og fordæma frá Finnlandi. ",
        "Raunverulegar upphæðir fara eftir samningaviðræðum. ",
        "Norðurslóðastuðningur (Art. 142) er greiddur af Íslandi, ekki ESB. ",
        "ANC-greiðslur eru samfjármagnaðar.")
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive calculations ──

  calcs <- reactive({
    current_eur <- input$current_support_isk * 1000 / input$eur_isk  # M EUR

    # Pillar 1
    p1 <- input$eligible_ha * input$p1_rate / 1e6

    # Nordic Aid
    milk  <- input$milk_litres * 1e6 * input$milk_rate / 1e6
    sheep <- input$ewes * input$ewe_rate / 1e6
    cows  <- input$cattle * input$cattle_rate / 1e6
    nordic <- milk + sheep + cows

    # ANC
    anc_total  <- input$eligible_ha * input$anc_rate / 1e6
    anc_eu     <- anc_total * input$eu_cofinance / 100
    anc_is     <- anc_total * (1 - input$eu_cofinance / 100)

    total <- p1 + nordic + anc_total
    gap   <- current_eur - total

    # Who pays what
    eu_pays      <- p1 + anc_eu
    iceland_pays <- nordic + anc_is

    # Needed rate
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

  # ── Metric boxes ──

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

  # ── Payer stacked bar (infographic style) ──

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

  # ── Waterfall chart ──

  output$waterfall_chart <- renderPlot({
    c <- calcs()

    df <- data.frame(
      label = factor(c("Sto\u00f0 1\nBeingrei\u00f0slur", "Art. 142\nNor\u00f0ursl\u00f3\u00f0ir", "Sto\u00f0 2\nHar\u00f0b\u00fdli (ANC)",
                       "CAP samtals", "Munur (gap)"),
                     levels = c("Sto\u00f0 1\nBeingrei\u00f0slur", "Art. 142\nNor\u00f0ursl\u00f3\u00f0ir", "Sto\u00f0 2\nHar\u00f0b\u00fdli (ANC)",
                                "CAP samtals", "Munur (gap)")),
      value = c(c$p1, c$nordic, c$anc_total, c$total, max(c$gap, 0)),
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
               hjust = 1, size = 3.5, colour = col_brown, fontface = "bold",
               family = "Montserrat") +
      geom_text(aes(y = (ymin + ymax) / 2, label = sprintf("\u20ac%.0fM", ymax - ymin)),
                size = 3.8, colour = "white", fontface = "bold", family = "Montserrat") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("\u20ac", x, "M"), expand = expansion(mult = c(0, 0.08))) +
      coord_cartesian(ylim = c(0, max(c$current_eur, c$total + c$gap) * 1.12)) +
      labs(title = "CAP stu\u00f0nings\u00e1\u00e6tlun fyrir \u00cdsland",
           subtitle = sprintf("Samtals \u20ac%.0fM vs. n\u00faverandi \u20ac%.0fM  |  Eligible area: %s ha  |  EUR/ISK: %s",
                              c$total, c$current_eur, format(input$eligible_ha, big.mark = "."), input$eur_isk),
           x = NULL, y = NULL) +
      theme_minimal(base_family = "Montserrat", base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 16),
        plot.subtitle = element_text(colour = col_grey, size = 11, family = "Lora"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, lineheight = 1.1),
        axis.text.y = element_text(colour = col_grey),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
      )
  }, res = 110, bg = "transparent")

  # ── Component breakdown chart (horizontal, grouped by channel) ──

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
           subtitle = sprintf("Sky Blue = Sto\u00f0 1 (ESB)  |  Gr\u00e6nt = Art. 142 (\u00cdsland)  |  Gult = ANC (samf.)"),
           x = NULL, y = NULL) +
      theme_minimal(base_family = "Montserrat", base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = col_brown, size = 14),
        plot.subtitle = element_text(colour = col_grey, size = 10, family = "Lora"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(colour = col_grey),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
      )
  }, res = 110, bg = "transparent")

  # ── Detail table (color-coded) ──

  output$detail_table_html <- renderUI({
    c <- calcs()

    # Helper: color-coded payer badge
    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:3px 12px;border-radius:4px;background:%s;color:#fff;font-size:11px;font-weight:600;font-family:Montserrat,sans-serif;letter-spacing:0.3px;">%s</span>', color, label)
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
      '<th style="padding:8px 12px;text-align:left;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;letter-spacing:0.5px;">Li\u00f0ur</th>',
      '<th style="padding:8px 12px;text-align:right;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;letter-spacing:0.5px;">\u20acM/\u00e1ri</th>',
      '<th style="padding:8px 12px;text-align:center;font-family:Montserrat,sans-serif;font-size:12px;color:', col_grey, ';text-transform:uppercase;letter-spacing:0.5px;">Grei\u00f0andi</th>',
      '</tr></thead><tbody>',
      paste(row_html, collapse = ""),
      summary_rows,
      '</tbody></table>'
    ))
  })
}

shinyApp(ui, server)
