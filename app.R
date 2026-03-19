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
col_p1     <- "#3266ad"
col_nordic <- "#639922"
col_anc    <- "#EF9F27"
col_gap    <- "#E24B4A"
col_accent <- "#2E4057"
# ── UI ───────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: Arial, Helvetica, sans-serif; background: #FAFBFC; }
    .sidebar { background: #fff; border-right: 1px solid #E5E7EB; }
    h3 { color: #2E4057; font-weight: 700; }
    h4 { color: #2E4057; font-weight: 600; margin-top: 18px; }
    .well { background: #fff; border: 1px solid #E5E7EB; }
    .metric-box { background: #F2F4F7; border-radius: 8px; padding: 14px 16px; margin-bottom: 10px; }
    .metric-label { font-size: 12px; color: #666; }
    .metric-value { font-size: 22px; font-weight: 600; color: #2E4057; }
    .metric-sub { font-size: 12px; color: #B83030; font-weight: 600; }
    .source-note { font-size: 11px; color: #999; line-height: 1.5; margin-top: 8px; }
  "))),

  titlePanel(
    div(
      h3("Áætlun um stuðning ESB við íslenskan landbúnað", style = "margin-bottom: 2px;"),
      p("CAP support estimation model — three channels", style = "color: #666; font-size: 13px; margin-top: 0;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # ── Iceland baseline ──
      h4("Ísland: grunnforsendur"),

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
      h4("Stoð 1: Beingreiðslur (Pillar 1)"),
      sliderInput("p1_rate", "Greiðsla á hektara (€/ha)",
                  min = 100, max = 400, value = 200, step = 10,
                  pre = "€"),
      helpText("EU lágmark: €200 (2023), €215 (2027). EU meðaltal: ~€243. Nýju ríkin: €130–180."),

      hr(),

      # ── Nordic Aid ──
      h4("Stoð 2: Norðurslóðastuðningur (Art. 142)"),

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
      h4("Stoð 3: Harðbýlisgreiðslur (ANC, Pillar 2)"),
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
        column(3, div(class = "metric-box",
                      div(class = "metric-label", "Núverandi stuðningur"),
                      uiOutput("metric_current")
        )),
        column(3, div(class = "metric-box",
                      div(class = "metric-label", "CAP heildaráætlun"),
                      uiOutput("metric_cap_total")
        )),
        column(3, div(class = "metric-box",
                      div(class = "metric-label", "Munur (gap)"),
                      uiOutput("metric_gap")
        )),
        column(3, div(class = "metric-box",
                      div(class = "metric-label", "Nauðsynlegur €/ha"),
                      uiOutput("metric_needed_rate")
        ))
      ),

      hr(),

      # ── Main chart ──
      plotOutput("waterfall_chart", height = "420px"),

      hr(),

      # ── Breakdown ──
      fluidRow(
        column(6, plotOutput("funding_source_chart", height = "320px")),
        column(6, plotOutput("component_chart", height = "320px"))
      ),

      hr(),

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
    current_eur <- input$current_support_isk * 1000 / input$eur_isk  # €M

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
    div(class = "metric-value", sprintf("€%.0fM", c$current_eur))
  })

  output$metric_cap_total <- renderUI({
    c <- calcs()
    col <- if (c$total >= c$current_eur) col_nordic else col_gap
    div(class = "metric-value", style = paste0("color:", col), sprintf("€%.0fM", c$total))
  })

  output$metric_gap <- renderUI({
    c <- calcs()
    if (c$gap > 0) {
      tagList(
        div(class = "metric-value", style = paste0("color:", col_gap), sprintf("−€%.0fM", c$gap)),
        div(class = "metric-sub", sprintf("%.0f%% vantar", c$gap / c$current_eur * 100))
      )
    } else {
      div(class = "metric-value", style = paste0("color:", col_nordic), sprintf("+€%.0fM", abs(c$gap)))
    }
  })

  output$metric_needed_rate <- renderUI({
    c <- calcs()
    div(
      div(class = "metric-value", sprintf("€%.0f/ha", c$needed_rate)),
      div(class = "metric-sub", "til að jafna núverandi")
    )
  })

  # ── Waterfall chart ──

  output$waterfall_chart <- renderPlot({
    c <- calcs()

    df <- data.frame(
      label = factor(c("Stoð 1\nBeingreiðslur", "Art. 142\nNorðurslóðir", "Stoð 2\nHarðbýli (ANC)",
                       "CAP samtals", "Munur (gap)"),
                     levels = c("Stoð 1\nBeingreiðslur", "Art. 142\nNorðurslóðir", "Stoð 2\nHarðbýli (ANC)",
                                "CAP samtals", "Munur (gap)")),
      value = c(c$p1, c$nordic, c$anc_total, c$total, max(c$gap, 0)),
      fill  = c(col_p1, col_nordic, col_anc, col_accent, col_gap),
      ymin  = c(0, c$p1, c$p1 + c$nordic, 0, c$total),
      ymax  = c(c$p1, c$p1 + c$nordic, c$total, c$total, c$total + max(c$gap, 0))
    )

    ggplot(df, aes(x = label)) +
      geom_rect(aes(xmin = as.numeric(label) - 0.35, xmax = as.numeric(label) + 0.35,
                    ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
      geom_hline(yintercept = c$current_eur, linetype = "dashed", colour = "#333", linewidth = 0.7) +
      annotate("text", x = 5.4, y = c$current_eur + 3,
               label = sprintf("Núverandi: €%.0fM", c$current_eur),
               hjust = 1, size = 3.5, colour = "#333", fontface = "bold") +
      geom_text(aes(y = (ymin + ymax) / 2, label = sprintf("€%.0fM", ymax - ymin)),
                size = 3.8, colour = "white", fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"), expand = expansion(mult = c(0, 0.08))) +
      coord_cartesian(ylim = c(0, max(c$current_eur, c$total + c$gap) * 1.12)) +
      labs(title = "CAP stuðningsáætlun fyrir Ísland",
           subtitle = sprintf("Samtals €%.0fM vs. núverandi €%.0fM  |  Eligible area: %s ha  |  EUR/ISK: %s",
                              c$total, c$current_eur, format(input$eligible_ha, big.mark = "."), input$eur_isk),
           x = NULL, y = NULL) +
      theme_minimal(base_family = "Arial", base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = col_accent, size = 16),
        plot.subtitle = element_text(colour = "#666", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, lineheight = 1.1)
      )
  }, res = 110)

  # ── Funding source chart ──

  output$funding_source_chart <- renderPlot({
    c <- calcs()

    df <- data.frame(
      source = factor(c("ESB greiðir", "Ísland greiðir"), levels = c("ESB greiðir", "Ísland greiðir")),
      value = c(c$eu_pays, c$iceland_pays),
      fill = c(col_p1, col_accent)
    )

    ggplot(df, aes(x = source, y = value, fill = fill)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = sprintf("€%.0fM", value)), vjust = -0.5, size = 4, fontface = "bold", colour = col_accent) +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"), expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Hver borgar?",
           subtitle = sprintf("ESB: €%.0fM  |  Ísland: €%.0fM", c$eu_pays, c$iceland_pays),
           x = NULL, y = NULL) +
      theme_minimal(base_family = "Arial", base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_accent, size = 14),
        plot.subtitle = element_text(colour = "#666", size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
  }, res = 110)

  # ── Component breakdown chart ──

  output$component_chart <- renderPlot({
    c <- calcs()

    df <- data.frame(
      component = factor(c("Mjólk", "Sauðfé", "Nautgripir", "Beingreiðslur\n(Stoð 1)", "ANC\n(Stoð 2)"),
                         levels = c("Beingreiðslur\n(Stoð 1)", "Mjólk", "Sauðfé", "Nautgripir", "ANC\n(Stoð 2)")),
      value = c(c$milk, c$sheep, c$cows, c$p1, c$anc_total),
      fill = c(rep(col_nordic, 3), col_p1, col_anc)
    )

    ggplot(df, aes(x = component, y = value, fill = fill)) +
      geom_col(width = 0.55) +
      geom_text(aes(label = sprintf("€%.1fM", value)), vjust = -0.5, size = 3.5, colour = col_accent) +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) paste0("€", x, "M"), expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Sundurliðun eftir greiðsluleiðum",
           x = NULL, y = NULL) +
      theme_minimal(base_family = "Arial", base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", colour = col_accent, size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, lineheight = 1.1)
      )
  }, res = 110)

  # ── Detail table (color-coded) ──

  output$detail_table_html <- renderUI({
    c <- calcs()

    # Helper: color-coded payer badge
    badge <- function(label, color) {
      sprintf('<span style="display:inline-block;padding:2px 10px;border-radius:4px;background:%s;color:#fff;font-size:12px;font-weight:600;">%s</span>', color, label)
    }
    badge_eu <- badge("ESB", col_p1)
    badge_is <- badge("\u00cdsland", col_accent)
    badge_both <- badge("Samfj\u00e1rm\u00f6gnun", col_anc)

    rows <- list(
      # Pillar 1
      list("Sto\u00f0 1: Beingrei\u00f0slur", sprintf("\u20ac%.1fM", c$p1), badge_eu, TRUE),
      list(sprintf("&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha", format(input$eligible_ha, big.mark = "."), input$p1_rate), "", "", FALSE),
      list("", "", "", FALSE),
      # Nordic Aid
      list("Art. 142: Nor\u00f0ursl\u00f3\u00f0astu\u00f0ningur", sprintf("\u20ac%.1fM", c$nordic), badge_is, TRUE),
      list(sprintf("&nbsp;&nbsp;Mj\u00f3lk: %.0fM L \u00d7 \u20ac%.2f/L", input$milk_litres, input$milk_rate), sprintf("\u20ac%.1fM", c$milk), badge_is, FALSE),
      list(sprintf("&nbsp;&nbsp;Sau\u00f0f\u00e9: %s \u00e6r \u00d7 \u20ac%s/kind", format(input$ewes, big.mark = "."), input$ewe_rate), sprintf("\u20ac%.1fM", c$sheep), badge_is, FALSE),
      list(sprintf("&nbsp;&nbsp;Nautgripir: %s \u00d7 \u20ac%s/grip", format(input$cattle, big.mark = "."), input$cattle_rate), sprintf("\u20ac%.1fM", c$cows), badge_is, FALSE),
      list("", "", "", FALSE),
      # ANC
      list("Sto\u00f0 2: Har\u00f0b\u00fdlisgrei\u00f0slur (ANC)", sprintf("\u20ac%.1fM", c$anc_total), badge_both, TRUE),
      list(sprintf("&nbsp;&nbsp;%s ha \u00d7 \u20ac%s/ha", format(input$eligible_ha, big.mark = "."), input$anc_rate), "", "", FALSE),
      list(sprintf("&nbsp;&nbsp;\u00dear af ESB (%s%%)", input$eu_cofinance), sprintf("\u20ac%.1fM", c$anc_eu), badge_eu, FALSE),
      list(sprintf("&nbsp;&nbsp;\u00dear af \u00cdsland (%s%%)", 100 - input$eu_cofinance), sprintf("\u20ac%.1fM", c$anc_is), badge_is, FALSE)
    )

    # Build table rows
    row_html <- sapply(rows, function(r) {
      bg <- if (r[[4]]) ' style="background:#F2F4F7;font-weight:600;"' else ""
      sprintf("<tr%s><td style='padding:6px 10px;'>%s</td><td style='padding:6px 10px;text-align:right;'>%s</td><td style='padding:6px 10px;text-align:center;'>%s</td></tr>",
              bg, r[[1]], r[[2]], r[[3]])
    })

    # Summary rows
    gap_color <- if (c$gap > 0) col_gap else col_nordic
    gap_sign <- if (c$gap > 0) sprintf("\u2212\u20ac%.1fM", c$gap) else sprintf("+\u20ac%.1fM", abs(c$gap))

    summary_rows <- sprintf('
      <tr style="border-top:2px solid #333;background:#F2F4F7;font-weight:700;">
        <td style="padding:8px 10px;">SAMTALS CAP</td>
        <td style="padding:8px 10px;text-align:right;">\u20ac%.1fM</td>
        <td style="padding:8px 10px;text-align:center;">%s &nbsp; %s</td>
      </tr>
      <tr style="font-weight:600;">
        <td style="padding:6px 10px;">N\u00faverandi stu\u00f0ningur</td>
        <td style="padding:6px 10px;text-align:right;">\u20ac%.1fM</td>
        <td style="padding:6px 10px;text-align:center;">%s</td>
      </tr>
      <tr style="border-top:2px solid %s;font-weight:700;">
        <td style="padding:8px 10px;color:%s;">MUNUR</td>
        <td style="padding:8px 10px;text-align:right;color:%s;">%s</td>
        <td></td>
      </tr>',
      c$total, badge_eu, badge_is,
      c$current_eur, badge_is,
      gap_color, gap_color, gap_color, gap_sign
    )

    HTML(paste0(
      '<table style="width:100%;border-collapse:collapse;font-size:13px;">',
      '<thead><tr style="border-bottom:2px solid #333;">',
      '<th style="padding:6px 10px;text-align:left;">Li\u00f0ur</th>',
      '<th style="padding:6px 10px;text-align:right;">\u20acM/\u00e1ri</th>',
      '<th style="padding:6px 10px;text-align:center;">Grei\u00f0andi</th>',
      '</tr></thead><tbody>',
      paste(row_html, collapse = ""),
      summary_rows,
      '</tbody></table>'
    ))
  })
}
shinyApp(ui, server)
