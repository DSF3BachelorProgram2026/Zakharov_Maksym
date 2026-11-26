# Shiny-застосунок: динаміка населення України
# Запуск: shiny::runApp() (у цій папці) або Rscript -e "shiny::runApp('.')"

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(DBI)
  library(RSQLite)
  library(forecast)
})

db_path <- "data/ukraine_population.sqlite"
if (!file.exists(db_path)) {
  stop("Немає бази даних. Спочатку виконайте: Rscript update_data.R")
}

con <- dbConnect(SQLite(), db_path)
onStop(function() dbDisconnect(con))

country <- dbReadTable(con, "country_stats") |> arrange(year)
regions <- dbReadTable(con, "regions") |> arrange(name)
region_pop <- dbReadTable(con, "region_population") |>
  left_join(regions, by = "region_id") |>
  arrange(name, year)
life <- dbReadTable(con, "life_expectancy") |>
  pivot_wider(names_from = sex, values_from = value) |>
  arrange(year)
sources <- dbReadTable(con, "sources")

default_regions <- region_pop |>
  filter(year == 2014) |>
  slice_max(population_thousand, n = 6) |>
  pull(name)

series_choices <- c(
  "Населення, тис. осіб"            = "population_thousand",
  "Народжуваність, осіб"           = "births",
  "Смертність, осіб"               = "deaths",
  "Природний приріст, осіб"        = "natural_increase",
  "Тривалість життя — чоловіки"    = "male",
  "Тривалість життя — жінки"       = "female"
)

series_value <- function(key) {
  if (key %in% names(country)) {
    tibble(year = country$year, value = country[[key]])
  } else {
    tibble(year = life$year, value = life[[key]])
  }
}

plot_layout <- function(p, title, y_title) {
  p |> layout(
    title = list(text = title, x = 0.02, font = list(size = 16)),
    xaxis = list(title = "Рік", rangeslider = list(visible = FALSE)),
    yaxis = list(title = y_title, rangemode = "tozero"),
    hovermode = "x unified",
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 60, b = 60)
  )
}

ui <- page_navbar(
  title = "Динаміка населення України",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  nav_panel(
    "Населення",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Чисельність населення, 1950–2021"),
        plotlyOutput("plot_population", height = "460px")
      ),
      card(
        card_header("Дані за роками"),
        DTOutput("table_country")
      )
    )
  ),

  nav_panel(
    "Народжуваність і смертність",
    card(
      card_header("Живонароджені та померлі"),
      plotlyOutput("plot_birth_death", height = "440px")
    ),
    card(
      card_header("Природний приріст (народжуваність мінус смертність)"),
      plotlyOutput("plot_natural", height = "300px")
    )
  ),

  nav_panel(
    "Тривалість життя",
    card(
      card_header("Очікувана тривалість життя при народженні за статтю"),
      plotlyOutput("plot_life", height = "460px")
    ),
    card(
      card_header("Різниця між жінками та чоловіками"),
      DTOutput("table_life")
    )
  ),

  nav_panel(
    "Регіони",
    card(
      card_header("Населення регіонів, 1926–2014 (тис. осіб)"),
      selectizeInput(
        "region_sel", "Оберіть регіони",
        choices = regions$name,
        selected = default_regions,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      plotlyOutput("plot_regions", height = "480px")
    ),
    card(
      card_header("Зміна населення регіонів"),
      DTOutput("table_regions")
    )
  ),

  nav_panel(
    "Прогноз",
    card(
      card_header("Прогноз за часовим рядом"),
      layout_columns(
        col_widths = c(5, 4, 3),
        selectInput("fc_series", "Показник", choices = series_choices),
        sliderInput("fc_range", "Період навчання моделі",
                    min = 1950, max = 2021, value = c(1990, 2021),
                    step = 1, sep = ""),
        sliderInput("fc_h", "Горизонт, років", min = 1, max = 20, value = 10)
      ),
      plotlyOutput("plot_forecast", height = "480px"),
      HTML("<p class='text-muted small'>Прогноз є статистичною екстраполяцією тренда (ARIMA).
            Він не враховує воєнні події, міграцію та зміни адміністративних меж,
            тому після 2021 року його слід трактувати обережно.</p>")
    ),
    card(
      card_header("Числові значення прогнозу"),
      DTOutput("table_forecast")
    )
  ),

  nav_panel(
    "Про дані",
    card(
      card_header("Джерела даних"),
      tableOutput("table_sources")
    ),
    card(
      card_header("Схема реляційної бази SQLite"),
      tags$pre(
"country_stats(year PK, population_thousand, births, deaths, natural_increase,
              birth_rate, death_rate, natural_rate, fertility, source_id FK)
regions(region_id PK, name UNIQUE)
region_population(region_id FK, year, population_thousand, source_id FK,
                  PK(region_id, year))
life_expectancy(year, sex, value, source_id FK, PK(year, sex))
sources(source_id PK, title, url, scraped_at)"
      ),
      p(class = "text-muted",
        "Дані отримано web-scraping-ом таблиць Вікіпедії, приведено до tidy-форми
         та завантажено до SQLite. Оновлення: Rscript update_data.R.")
    )
  )
)

server <- function(input, output, session) {

  # --- Населення -------------------------------------------------------------

  output$plot_population <- renderPlotly({
    plot_ly(country, x = ~year, y = ~population_thousand,
            type = "scatter", mode = "lines+markers",
            name = "Населення",
            line = list(color = "#1f77b4", width = 3),
            marker = list(size = 4),
            hovertemplate = "%{x}: %{y:,.0f} тис. осіб<extra></extra>") |>
      plot_layout("Чисельність населення України", "Тис. осіб") |>
      config(displaylogo = FALSE)
  })

  output$table_country <- renderDT({
    country |>
      transmute(Рік = year,
                `Населення, тис.` = round(population_thousand, 1),
                `Народжуваність, осіб` = births,
                `Смертність, осіб` = deaths,
                `Природний приріст, осіб` = natural_increase,
                `Народжуваність, ‰` = birth_rate,
                `Смертність, ‰` = death_rate) |>
      datatable(options = list(pageLength = 12, scrollX = TRUE, dom = "ftip"),
                rownames = FALSE) |>
      formatRound(c("Народжуваність, ‰", "Смертність, ‰"), 1)
  })

  # --- Народжуваність і смертність ------------------------------------------

  output$plot_birth_death <- renderPlotly({
    plot_ly(country, x = ~year) |>
      add_lines(y = ~births, name = "Народжуваність",
                line = list(color = "#2ca02c", width = 3)) |>
      add_lines(y = ~deaths, name = "Смертність",
                line = list(color = "#d62728", width = 3)) |>
      plot_layout("Народжуваність і смертність", "Осіб") |>
      config(displaylogo = FALSE)
  })

  output$plot_natural <- renderPlotly({
    country |>
      mutate(sign = if_else(natural_increase >= 0, "Приріст", "Спад")) |>
      plot_ly(x = ~year, y = ~natural_increase, color = ~sign,
              colors = c("Приріст" = "#2ca02c", "Спад" = "#d62728"),
              type = "bar", hovertemplate = "%{x}: %{y:,.0f} осіб<extra></extra>") |>
      plot_layout("Природний приріст населення", "Осіб") |>
      layout(barmode = "relative", showlegend = FALSE) |>
      config(displaylogo = FALSE)
  })

  # --- Тривалість життя -----------------------------------------------------

  output$plot_life <- renderPlotly({
    plot_ly(life, x = ~year) |>
      add_lines(y = ~male, name = "Чоловіки",
                line = list(color = "#1f77b4", width = 3)) |>
      add_lines(y = ~female, name = "Жінки",
                line = list(color = "#e377c2", width = 3)) |>
      plot_layout("Очікувана тривалість життя", "Років") |>
      config(displaylogo = FALSE)
  })

  output$table_life <- renderDT({
    life |>
      mutate(gap = female - male) |>
      transmute(Рік = year,
                `Чоловіки, років` = round(male, 2),
                `Жінки, років` = round(female, 2),
                `Різниця, років` = round(gap, 2)) |>
      datatable(options = list(pageLength = 10, dom = "ftip"), rownames = FALSE)
  })

  # --- Регіони --------------------------------------------------------------

  output$plot_regions <- renderPlotly({
    req(input$region_sel)
    d <- region_pop |> filter(name %in% input$region_sel)
    validate(need(nrow(d) > 0, "Оберіть хоча б один регіон"))

    p <- plot_ly()
    for (r in unique(d$name)) {
      dr <- filter(d, name == r)
      p <- add_lines(p, data = dr, x = ~year, y = ~population_thousand,
                     name = r, mode = "lines+markers",
                     line = list(width = 2), marker = list(size = 5),
                     hovertemplate = paste0(r, ": %{y:,.0f} тис.<extra></extra>"))
    }
    p |>
      plot_layout("Населення регіонів", "Тис. осіб") |>
      config(displaylogo = FALSE)
  })

  output$table_regions <- renderDT({
    wide <- region_pop |>
      select(name, year, population_thousand) |>
      pivot_wider(names_from = year, values_from = population_thousand) |>
      mutate(
        `Зміна 1989→2014, %` = round((`2014` / `1989` - 1) * 100, 1)
      ) |>
      arrange(desc(`2014`))

    names(wide)[1] <- "Регіон"
    wide |>
      datatable(options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"),
                rownames = FALSE) |>
      formatRound(setdiff(names(wide), c("Регіон", "Зміна 1989→2014, %")), 0)
  })

  # --- Прогноз --------------------------------------------------------------

  forecast_fit <- reactive({
    d <- series_value(input$fc_series) |>
      filter(year >= input$fc_range[1], year <= input$fc_range[2], !is.na(value))
    validate(need(nrow(d) >= 10, "Для прогнозу потрібно щонайменше 10 спостережень"))

    fit <- auto.arima(ts(d$value, start = min(d$year), frequency = 1))
    f <- forecast(fit, h = input$fc_h, level = c(80, 95))
    list(history = d, fit = fit, forecast = f)
  })

  output$plot_forecast <- renderPlotly({
    x <- forecast_fit()
    hist <- x$history
    f <- x$forecast
    fc <- tibble(
      year = max(hist$year) + seq_along(f$mean),
      mean = as.numeric(f$mean),
      lo80 = as.numeric(f$lower[, 1]), hi80 = as.numeric(f$upper[, 1]),
      lo95 = as.numeric(f$lower[, 2]), hi95 = as.numeric(f$upper[, 2])
    )

    plot_ly() |>
      add_ribbons(data = fc, x = ~year, ymin = ~lo95, ymax = ~hi95,
                  name = "95% інтервал",
                  fillcolor = "rgba(31,119,180,0.14)",
                  line = list(width = 0)) |>
      add_ribbons(data = fc, x = ~year, ymin = ~lo80, ymax = ~hi80,
                  name = "80% інтервал",
                  fillcolor = "rgba(31,119,180,0.25)",
                  line = list(width = 0)) |>
      add_lines(data = hist, x = ~year, y = ~value,
                name = "Факт", line = list(color = "#1f77b4", width = 3)) |>
      add_lines(data = fc, x = ~year, y = ~mean,
                name = "Прогноз", line = list(color = "#d62728", width = 3, dash = "dash")) |>
      add_segments(x = max(hist$year), xend = max(hist$year),
                   y = min(hist$value), yend = max(hist$value),
                   line = list(color = "grey", dash = "dot"),
                   showlegend = FALSE, hoverinfo = "skip") |>
      plot_layout(
        paste0("Прогноз: ", names(series_choices)[series_choices == input$fc_series]),
        ""
      ) |>
      layout(yaxis = list(title = "", rangemode = "normal"),
             annotations = list(
               x = max(hist$year), y = max(hist$value),
               text = "кінець фактичних даних", showarrow = TRUE,
               arrowhead = 2, ax = 40, ay = -40, font = list(size = 11)
             )) |>
      config(displaylogo = FALSE)
  })

  output$table_forecast <- renderDT({
    x <- forecast_fit()
    f <- x$forecast
    start <- max(x$history$year) + 1
    tibble(
      Рік = start + seq_along(f$mean) - 1,
      Прогноз = as.numeric(f$mean),
      `Нижня межа 80%` = as.numeric(f$lower[, 1]),
      `Верхня межа 80%` = as.numeric(f$upper[, 1]),
      `Нижня межа 95%` = as.numeric(f$lower[, 2]),
      `Верхня межа 95%` = as.numeric(f$upper[, 2])
    ) |>
      datatable(options = list(pageLength = 10, dom = "ftip", rownames = FALSE)) |>
      formatRound(2:6, 1)
  })

  # --- Про дані -------------------------------------------------------------

  output$table_sources <- renderTable({
    sources |> select(Джерело = title, URL = url, `Зібрано` = scraped_at)
  }, sanitize.text.function = identity)
}

shinyApp(ui, server)
