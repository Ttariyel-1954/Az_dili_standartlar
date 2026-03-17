# ============================================================
# app.R — Azərbaycan Dili Standartları Dashboard
# R Shiny ilə interaktiv idarəetmə paneli
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(httr)
library(jsonlite)
library(plotly)

# PostgreSQL — yalniz movcud olduqda yukle
DB_AVAILABLE <- tryCatch({
  library(DBI)
  library(RPostgres)
  TRUE
}, error = function(e) FALSE)

# --- .env faylından konfiqurasiya oxumaq ---
env_file <- file.path(getwd(), ".env")
if (file.exists(env_file)) {
  env_lines <- readLines(env_file, warn = FALSE)
  for (line in env_lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "#")) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      key <- trimws(parts[1])
      val <- trimws(paste(parts[-1], collapse = "="))
      do.call(Sys.setenv, setNames(list(val), key))
    }
  }
  cat("   .env faylı yükləndi\n")
}

# --- KONFİQURASİYA ---
DB_CONFIG <- list(
  dbname   = Sys.getenv("DB_NAME", "az_dili_standartlar"),
  host     = Sys.getenv("DB_HOST", "localhost"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  user     = Sys.getenv("DB_USER", "postgres"),
  password = Sys.getenv("DB_PASSWORD", "postgres")
)

CLAUDE_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY", "")
CLAUDE_MODEL <- Sys.getenv("DEFAULT_AI_MODEL", "claude-sonnet-4-20250514")
CLAUDE_ENDPOINT <- "https://api.anthropic.com/v1/messages"

# --- Bazaya qoşulma funksiyası ---
get_con <- function() {
  if (!DB_AVAILABLE) return(NULL)
  tryCatch(
    dbConnect(RPostgres::Postgres(),
              dbname = DB_CONFIG$dbname, host = DB_CONFIG$host,
              port = DB_CONFIG$port, user = DB_CONFIG$user,
              password = DB_CONFIG$password),
    error = function(e) { DB_AVAILABLE <<- FALSE; NULL }
  )
}

# --- CSV fallback (Binder ucun) ---
CSV_MOVCUD <- NULL
csv_path <- file.path(getwd(), "data", "movcud_standartlar_backup.csv")
if (file.exists(csv_path)) {
  CSV_MOVCUD <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  cat("   CSV yuklendi:", nrow(CSV_MOVCUD), "setir\n")
}

# DB ve ya CSV-den oxu
db_query <- function(query_fn, csv_fallback_fn) {
  con <- get_con()
  if (!is.null(con)) {
    on.exit(dbDisconnect(con))
    tryCatch(query_fn(con), error = function(e) csv_fallback_fn())
  } else {
    csv_fallback_fn()
  }
}

# --- CEFR xəritəsi ---
cefr_map <- c("1"="A1","2"="A1","3"="A2","4"="A2","5"="B1",
              "6"="B1","7"="B1","8"="B2","9"="B2","10"="C1","11"="C1")

# --- %||% operatoru ---
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(as.character(a)) > 0) a else b

# --- Claude API (token izləmə ilə) ---
call_claude <- function(prompt, api_key = NULL) {
  key <- if (!is.null(api_key) && nchar(api_key) >= 10) api_key else CLAUDE_API_KEY
  if (nchar(key) < 10) return(list(success = FALSE, error = "API key tapilmadi! .env faylini yoxlayin.",
                                    time_sec = 0, input_tokens = 0, output_tokens = 0))
  t0 <- proc.time()["elapsed"]
  tryCatch({
    resp <- POST(CLAUDE_ENDPOINT,
      add_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01", `content-type` = "application/json"),
      body = toJSON(list(
        model = CLAUDE_MODEL,
        max_tokens = 16384L,
        messages = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE),
      encode = "raw", timeout(300))
    elapsed <- round(as.numeric(proc.time()["elapsed"] - t0), 1)
    res <- content(resp, "parsed", encoding = "UTF-8")
    inp_tok <- as.integer(res$usage$input_tokens %||% 0)
    out_tok <- as.integer(res$usage$output_tokens %||% 0)
    if (resp$status_code == 200) {
      txt <- if (length(res$content) > 0) res$content[[1]]$text %||% "" else ""
      list(success = TRUE, text = txt, time_sec = elapsed, input_tokens = inp_tok, output_tokens = out_tok)
    } else {
      err_msg <- if (!is.null(res$error)) res$error$message %||% paste("HTTP", resp$status_code) else paste("HTTP", resp$status_code)
      list(success = FALSE, error = err_msg, time_sec = elapsed, input_tokens = inp_tok, output_tokens = out_tok)
    }
  }, error = function(e) {
    elapsed <- round(as.numeric(proc.time()["elapsed"] - t0), 1)
    list(success = FALSE, error = e$message, time_sec = elapsed, input_tokens = 0, output_tokens = 0)
  })
}

# --- Statistika paneli (token/xərc) ---
make_stats_bar <- function(time_sec, input_tokens, output_tokens) {
  total <- input_tokens + output_tokens
  cost <- round((input_tokens * 3 + output_tokens * 15) / 1e6, 4)
  paste0(
    '<div style="background:linear-gradient(135deg,#0f172a,#1e293b);color:#e2e8f0;padding:20px 28px;border-radius:14px;margin-top:24px;box-shadow:0 4px 20px rgba(0,0,0,0.2);">',
    '<div style="font-size:1.5em;font-weight:700;margin-bottom:14px;color:#fbbf24;">Generasiya Statistikasi</div>',
    '<div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px;">',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #2E7D32;"><div style="font-size:0.98em;color:#94a3b8;">Vaxt</div><div style="font-size:1.6em;font-weight:700;color:#66BB6A;">', sprintf("%.1f", time_sec), ' san</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #22c55e;"><div style="font-size:0.98em;color:#94a3b8;">Giris token</div><div style="font-size:1.6em;font-weight:700;color:#4ade80;">', formatC(input_tokens, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #f59e0b;"><div style="font-size:0.98em;color:#94a3b8;">Cixis token</div><div style="font-size:1.6em;font-weight:700;color:#fbbf24;">', formatC(output_tokens, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #ef4444;"><div style="font-size:0.98em;color:#94a3b8;">Cemi token</div><div style="font-size:1.6em;font-weight:700;color:#f87171;">', formatC(total, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #a78bfa;"><div style="font-size:0.98em;color:#94a3b8;">Texmini qiymet</div><div style="font-size:1.6em;font-weight:700;color:#c4b5fd;">$', sprintf("%.4f", cost), '</div></div>',
    '</div></div>')
}

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://flagcdn.com/w40/az.png", height = "20px", style = "margin-right:8px;"),
      "Azerbaycan Dili Standartlari"
    ),
    titleWidth = 350
  ),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Ana Sehife", tabName = "home", icon = icon("home")),
      menuItem("Standartlar", tabName = "standards", icon = icon("book")),
      menuItem("Muqayise", tabName = "compare", icon = icon("columns")),
      menuItem("Beynelxalq", tabName = "international", icon = icon("globe")),
      menuItem("Statistika", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Claude AI Tehlil", tabName = "ai_analysis", icon = icon("robot")),
      menuItem("HTML Export", tabName = "export", icon = icon("download"))
    ),
    hr(),
    div(style = "padding: 15px;",
      selectInput("sinif_sec", "Sinif secin:",
                  choices = setNames(1:11, paste0(1:11, "-ci sinif")),
                  selected = 1),
      selectInput("mezmun_sec", "Mezmun xetti:",
                  choices = c("Hamisi" = "all", "Dinleme ve danisma" = "Dinl\u0259m\u0259 v\u0259 dan\u0131\u015fma",
                              "Oxu" = "Oxu", "Yazi" = "Yaz\u0131", "Dil qaydalari" = "Dil qaydalar\u0131"),
                  selected = "all"),
      selectInput("deyisiklik_sec", "Deyisiklik novu:",
                  choices = c("Hamisi" = "all", "Movcud" = "movcud", "Yenilenib" = "yenilenib",
                              "Yeni" = "yeni", "Silinib" = "silinib"),
                  selected = "all"),
      hr(),
      div(style = "text-align:center; color:#7f8c8d; font-size:12px;",
          "CEFR Seviyyesi:",
          textOutput("cefr_display", inline = TRUE))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML('
        @import url("https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300;400;500;600;700&display=swap");
        body, .content-wrapper, .main-sidebar { font-family: "Noto Sans", sans-serif; font-size: 16px; }
        .content-wrapper { background-color: #f0f4f8; }
        .info-box .info-box-number { font-size: 32px; }
        .box { border-top: 3px solid #1976D2; border-radius: 8px; }
        .box-title { font-size: 18px !important; }
        .skin-blue .main-header .logo { background-color: #0D47A1; font-weight: 600; font-size: 18px; }
        .skin-blue .main-header .navbar { background-color: #1565C0; }
        .skin-blue .main-sidebar { background-color: #0D47A1; font-size: 16px; }
        .skin-blue .sidebar-menu > li > a { font-size: 16px; }
        .skin-blue .sidebar-menu > li.active > a { background-color: #1565C0; border-left-color: #64B5F6; }
        .dt-row-yenilenib { background-color: #E8F5E9 !important; }
        .dt-row-yeni { background-color: #FFF3E0 !important; }
        .dt-row-silinib { background-color: #FFEBEE !important; text-decoration: line-through; }
        .small-box { border-radius: 10px; }
        .small-box h3 { font-size: 2.2em; }
        .small-box p { font-size: 16px; }
        .stat-card {
          background: white; border-radius: 12px; padding: 20px; margin: 10px 0;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-left: 4px solid #1976D2;
        }
        .stat-card h4 { color: #0D47A1; margin-bottom: 10px; }
        .stat-card .number { font-size: 2.9em; font-weight: 700; color: #1565C0; }
        /* DataTable font */
        table.dataTable { font-size: 15px !important; }
        table.dataTable th { font-size: 16px !important; }
        .dataTables_info, .dataTables_length, .dataTables_filter { font-size: 15px !important; }
        /* Label badges */
        .label { font-size: 15px !important; }
        /* Select inputs */
        .selectize-input, .selectize-dropdown { font-size: 15px !important; }
        /* Buttons */
        .btn-lg { font-size: 18px !important; }
        /* AI token/timer stilleri */
        .token-display{display:inline-flex;align-items:center;gap:8px;font-size:1.32em;font-weight:700;padding:10px 18px;border-radius:10px;margin-top:15px}
        .token-waiting{background:#fef3c7;color:#92400e;border:1px solid #fde68a}
        .token-done{background:#dcfce7;color:#166534;border:1px solid #86efac}
        .token-error{background:#fef2f2;color:#991b1b;border:1px solid #fca5a5}
        .live-timer-panel{background:linear-gradient(135deg,#0f172a,#1e293b);border:2px solid #22c55e;border-radius:16px;padding:32px 40px;margin:20px 0;text-align:center;box-shadow:0 4px 24px rgba(46,125,50,.15)}
        .live-timer-panel .t-status{font-size:1.32em;color:#94a3b8;margin-bottom:10px}
        .live-timer-panel .t-clock{font-family:"JetBrains Mono",monospace;font-size:3.7em;font-weight:700;color:#66BB6A;letter-spacing:.06em;margin:8px 0}
        .live-timer-panel .t-start{font-size:1.09em;color:#64748b;margin-bottom:14px}
        .live-timer-panel .t-details{display:flex;justify-content:center;gap:16px;flex-wrap:wrap}
        .live-timer-panel .t-item{background:rgba(255,255,255,.06);padding:10px 20px;border-radius:10px;font-size:1.09em;color:#cbd5e1}
        .pdot{display:inline-block;width:12px;height:12px;background:#22c55e;border-radius:50%;margin-right:8px;animation:pdot 1s infinite}
        @keyframes pdot{0%,100%{opacity:1}50%{opacity:.3}}
        .ai-output{font-size:18px;line-height:1.85;padding:24px;background:white;border-radius:12px;border:1px solid #e2e8f0;margin-top:16px}
        .ai-output h1{color:#0D47A1;margin-top:24px;font-size:26px}
        .ai-output h2{color:#0D47A1;margin-top:22px;font-size:23px}
        .ai-output h3{color:#0D47A1;margin-top:20px;font-size:20px}
        .ai-output ul,.ai-output ol{margin-left:24px;font-size:18px}
        .ai-output li{margin-bottom:6px}
        .ai-output table{width:100%;border-collapse:collapse;margin:18px 0;font-size:17px}
        .ai-output th{background:#1976D2;color:white;padding:12px;text-align:left;font-size:17px}
        .ai-output td{padding:10px 12px;border-bottom:1px solid #e2e8f0}
        .ai-output tr:nth-child(even){background:#f8fafc}
        .ai-output p{font-size:18px;margin-bottom:12px}
      ')),
      # JavaScript: canli taymer ve token gostericisi
      tags$script(HTML('
        var _aiTimerInterval = null;
        var _aiTimerStart = null;

        Shiny.addCustomMessageHandler("ai_timer_start", function(m) {
          _aiTimerStart = new Date();
          if (_aiTimerInterval) clearInterval(_aiTimerInterval);
          var e = document.getElementById(m.target);
          if (!e) return;
          var st = _aiTimerStart.toLocaleTimeString("az-AZ", {hour:"2-digit",minute:"2-digit",second:"2-digit"});
          e.innerHTML = "<div class=\\"live-timer-panel\\">" +
            "<div class=\\"t-status\\"><span class=\\"pdot\\"></span>" + m.status + "</div>" +
            "<div class=\\"t-clock\\" id=\\"_clk_" + m.target + "\\">00:00</div>" +
            "<div class=\\"t-start\\">Baslama: " + st + "</div>" +
            "<div class=\\"t-details\\">" +
              "<div class=\\"t-item\\">" + m.info1 + "</div>" +
              "<div class=\\"t-item\\">" + m.info2 + "</div>" +
              "<div class=\\"t-item\\">Claude AI</div>" +
            "</div></div>";
          _aiTimerInterval = setInterval(function() {
            var d = Math.floor((new Date() - _aiTimerStart) / 1000);
            var mm = Math.floor(d / 60), ss = d % 60;
            var t = (mm < 10 ? "0" : "") + mm + ":" + (ss < 10 ? "0" : "") + ss;
            var c = document.getElementById("_clk_" + m.target);
            if (c) c.textContent = t;
          }, 250);
        });

        Shiny.addCustomMessageHandler("ai_timer_stop", function(m) {
          if (_aiTimerInterval) { clearInterval(_aiTimerInterval); _aiTimerInterval = null; }
          var e = document.getElementById(m.target);
          if (!e) return;
          var cl = m.ok ? "" : "border-color:#ef4444";
          var co = m.ok ? "#66BB6A" : "#ef4444";
          var lb = m.ok ? "Tamamlandi" : "Xeta bas verdi";
          e.innerHTML = "<div class=\\"live-timer-panel\\" style=\\"" + cl + "\\">" +
            "<div class=\\"t-status\\" style=\\"color:" + co + "\\">" + lb + "</div>" +
            "<div class=\\"t-clock\\" style=\\"color:" + co + "\\">" + m.elapsed + " san</div>" +
            "<div class=\\"t-details\\">" +
              "<div class=\\"t-item\\">Giris: " + m.inp + "</div>" +
              "<div class=\\"t-item\\">Cixis: " + m.out + "</div>" +
              "<div class=\\"t-item\\">" + m.cost + "</div>" +
            "</div></div>";
        });
      '))
    ),

    tabItems(
      # ========== ANA SEHIFE ==========
      tabItem(tabName = "home",
        fluidRow(
          valueBoxOutput("vb_umumi", width = 3),
          valueBoxOutput("vb_movcud", width = 3),
          valueBoxOutput("vb_yenilenib", width = 3),
          valueBoxOutput("vb_yeni", width = 3)
        ),
        fluidRow(
          box(title = "Sinifler uzre Standart Sayi", width = 8,
              status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_sinifler", height = "400px")),
          box(title = "Layihe Haqqinda", width = 4, status = "info", solidHeader = TRUE,
              div(style = "font-size: 17px; line-height: 1.85;",
                tags$p("Bu dashboard Azerbaycan dili standartlarinin beynelxalq telebere uygun yenilanmasini idare edir."),
                tags$hr(),
                tags$p(tags$strong("Beynelxalq cerceveler:")),
                tags$ul(
                  tags$li("PISA - Oxu savadliligi"),
                  tags$li("PIRLS - Oxu bacarqlari"),
                  tags$li("CEFR - Avropa Dil Cercevesi"),
                  tags$li("Bloom Taksonomiyasi")
                ),
                tags$p(tags$strong("Aparici olkeler:")),
                tags$p("Finlandiya, Sinqapur, Estoniya"),
                tags$p("Yaponiya, Kanada, Irlandiya")
              ))
        )
      ),

      # ========== STANDARTLAR ==========
      tabItem(tabName = "standards",
        fluidRow(
          box(title = "Yenilenmistandartlar", width = 12, status = "primary", solidHeader = TRUE,
              div(style = "margin-bottom: 15px;",
                tags$span(class = "label label-default", style = "background:#90CAF9; font-size:13px;", "Movcud"),
                tags$span(class = "label label-success", style = "font-size:13px;", "Yenilenib"),
                tags$span(class = "label label-warning", style = "font-size:13px;", "Yeni"),
                tags$span(class = "label label-danger", style = "font-size:13px;", "Silinib")
              ),
              DTOutput("dt_standartlar"))
        )
      ),

      # ========== MUQAYISE ==========
      tabItem(tabName = "compare",
        fluidRow(
          box(title = "Kohne Standartlar", width = 6, status = "warning", solidHeader = TRUE,
              DTOutput("dt_kohne")),
          box(title = "Yeni Standartlar", width = 6, status = "success", solidHeader = TRUE,
              DTOutput("dt_yeni"))
        )
      ),

      # ========== BEYNELXALQ ==========
      tabItem(tabName = "international",
        fluidRow(
          box(title = "PISA/PIRLS/CEFR Uygunlugu", width = 7, status = "primary", solidHeader = TRUE,
              DTOutput("dt_beynelxalq")),
          box(title = "Olke Standartlari", width = 5, status = "info", solidHeader = TRUE,
              selectInput("olke_sec", "Olke secin:",
                          choices = c("Hamisi", "Finlandiya", "Sinqapur", "Estoniya",
                                      "Yaponiya", "Kanada", "Irlandiya")),
              DTOutput("dt_olkeler"))
        ),
        fluidRow(
          box(title = "Bloom Taksonomiyasi Paylanmasi", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_bloom", height = "350px"))
        )
      ),

      # ========== STATISTIKA ==========
      tabItem(tabName = "stats",
        fluidRow(
          box(title = "Deyisiklik Novleri uzre Paylanma", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_deyisiklik", height = "400px")),
          box(title = "Mezmun Xetleri uzre", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_mezmun", height = "400px"))
        ),
        fluidRow(
          box(title = "CEFR Seviyyeleri uzre", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("chart_cefr", height = "350px")),
          box(title = "Bloom Seviyyeleri uzre", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("chart_bloom_all", height = "350px"))
        )
      ),

      # ========== AI TEHLIL ==========
      tabItem(tabName = "ai_analysis",
        fluidRow(
          box(title = "Claude AI ile Standart Tehlili", width = 12, status = "primary", solidHeader = TRUE,
              div(style = "margin-bottom: 15px;",
                  tags$p("Secilmis sinif ucun Claude AI standartlari ehtrafli tehlil edecek ve elave tovsiyeler verecek.")),
              actionButton("btn_analyze", "Tehlil et", class = "btn-primary btn-lg",
                           style = "margin-bottom: 20px;", icon = icon("robot")),
              hr(),
              # Canli taymer paneli
              div(id = "ai_timer_panel"),
              # Token gostericisi
              uiOutput("ai_token_display"),
              # AI neticesi
              uiOutput("ai_result"))
        )
      ),

      # ========== EXPORT ==========
      tabItem(tabName = "export",
        fluidRow(
          box(title = "HTML Hesabat Yukle", width = 6, status = "primary", solidHeader = TRUE,
              tags$p("Secilmis sinif ucun HTML hesabati yukleyin:"),
              downloadButton("download_html", "HTML Yukle", class = "btn-primary btn-lg")),
          box(title = "CSV Yukle", width = 6, status = "info", solidHeader = TRUE,
              tags$p("Butun standartlari CSV formatinda yukleyin:"),
              downloadButton("download_csv", "CSV Yukle", class = "btn-info btn-lg"))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # --- Reaktiv melumat ---
  data_yenilenmi <- reactive({
    db_query(
      function(con) {
        query <- sprintf("SELECT * FROM yenilenmi_standartlar WHERE sinif = %s", input$sinif_sec)
        if (input$mezmun_sec != "all") query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", input$mezmun_sec))
        if (input$deyisiklik_sec != "all") query <- paste0(query, sprintf(" AND deyisiklik_novu = '%s'", input$deyisiklik_sec))
        dbGetQuery(con, paste0(query, " ORDER BY mezmun_xetti, standart_kodu"))
      },
      function() data.frame()
    )
  })

  data_movcud <- reactive({
    db_query(
      function(con) {
        dbGetQuery(con, sprintf("SELECT * FROM movcud_standartlar WHERE sinif = %s ORDER BY mezmun_xetti, standart_kodu",
                                input$sinif_sec))
      },
      function() {
        if (!is.null(CSV_MOVCUD)) {
          df <- CSV_MOVCUD[CSV_MOVCUD$sinif == as.integer(input$sinif_sec), ]
          df[order(df$mezmun_xetti, df$standart_kodu), ]
        } else data.frame()
      }
    )
  })

  all_stats <- reactive({
    db_query(
      function(con) {
        df <- dbGetQuery(con, "
          SELECT sinif,
            COUNT(*) as umumi,
            COUNT(*) FILTER (WHERE deyisiklik_novu = 'movcud') as movcud,
            COUNT(*) FILTER (WHERE deyisiklik_novu = 'yenilenib') as yenilenib,
            COUNT(*) FILTER (WHERE deyisiklik_novu = 'yeni') as yeni,
            COUNT(*) FILTER (WHERE deyisiklik_novu = 'silinib') as silinib
          FROM yenilenmi_standartlar GROUP BY sinif ORDER BY sinif")
        if (nrow(df) == 0) {
          df <- dbGetQuery(con, "
            SELECT sinif, COUNT(*) as umumi, COUNT(*) as movcud,
                   0 as yenilenib, 0 as yeni, 0 as silinib
            FROM movcud_standartlar GROUP BY sinif ORDER BY sinif")
        }
        df
      },
      function() {
        if (!is.null(CSV_MOVCUD)) {
          agg <- aggregate(id ~ sinif, data = CSV_MOVCUD, FUN = length)
          names(agg) <- c("sinif", "umumi")
          agg$movcud <- agg$umumi
          agg$yenilenib <- 0; agg$yeni <- 0; agg$silinib <- 0
          agg[order(agg$sinif), ]
        } else data.frame(sinif=integer(), umumi=integer(), movcud=integer(),
                          yenilenib=integer(), yeni=integer(), silinib=integer())
      }
    )
  })

  # --- CEFR ---
  output$cefr_display <- renderText({
    cefr_map[input$sinif_sec]
  })

  # --- VALUE BOXES ---
  output$vb_umumi <- renderValueBox({
    st <- all_stats()
    valueBox(if(nrow(st)>0) sum(st$umumi) else 0, "Umumi Standart", icon = icon("book"), color = "blue")
  })
  output$vb_movcud <- renderValueBox({
    st <- all_stats()
    valueBox(if(nrow(st)>0) sum(st$movcud) else 0, "Movcud", icon = icon("check"), color = "light-blue")
  })
  output$vb_yenilenib <- renderValueBox({
    st <- all_stats()
    valueBox(if(nrow(st)>0) sum(st$yenilenib) else 0, "Yenilenib", icon = icon("edit"), color = "green")
  })
  output$vb_yeni <- renderValueBox({
    st <- all_stats()
    valueBox(if(nrow(st)>0) sum(st$yeni) else 0, "Yeni Elave", icon = icon("plus"), color = "orange")
  })

  # --- QRAFIKLER ---
  output$chart_sinifler <- renderPlotly({
    st <- all_stats()
    if (nrow(st) == 0) return(plotly_empty())

    plot_ly(st, x = ~paste0(sinif, "-ci sinif"), type = "bar") %>%
      add_bars(y = ~movcud, name = "Movcud", marker = list(color = "#90CAF9")) %>%
      add_bars(y = ~yenilenib, name = "Yenilenib", marker = list(color = "#4CAF50")) %>%
      add_bars(y = ~yeni, name = "Yeni", marker = list(color = "#FF9800")) %>%
      layout(barmode = "stack",
             xaxis = list(title = ""), yaxis = list(title = "Standart sayi"),
             legend = list(orientation = "h", y = -0.15),
             font = list(family = "Noto Sans"))
  })

  output$chart_deyisiklik <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty())

    counts <- table(df$deyisiklik_novu)
    colors <- c("movcud" = "#90CAF9", "yenilenib" = "#4CAF50", "yeni" = "#FF9800", "silinib" = "#F44336")

    plot_ly(labels = names(counts), values = as.numeric(counts), type = "pie",
            marker = list(colors = colors[names(counts)]),
            textinfo = "label+percent") %>%
      layout(font = list(family = "Noto Sans"))
  })

  output$chart_mezmun <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty())

    counts <- as.data.frame(table(df$mezmun_xetti))
    plot_ly(counts, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#1976D2")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })

  output$chart_bloom <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty())
    df <- df[!is.na(df$bloom_seviyyesi) & nchar(df$bloom_seviyyesi) > 0, ]
    if (nrow(df) == 0) return(plotly_empty())

    bloom_order <- c("Xatirlama", "Anlama", "Tetbiq", "Analiz", "Qiymetlendirme", "Yaratma")
    counts <- as.data.frame(table(factor(df$bloom_seviyyesi, levels = bloom_order)))

    bloom_colors <- c("#E3F2FD", "#BBDEFB", "#90CAF9", "#42A5F5", "#1976D2", "#0D47A1")
    plot_ly(counts, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = bloom_colors)) %>%
      layout(xaxis = list(title = "", categoryorder = "array", categoryarray = bloom_order),
             yaxis = list(title = "Say"), font = list(family = "Noto Sans"))
  })

  output$chart_cefr <- renderPlotly({
    df <- db_query(
      function(con) dbGetQuery(con, "
        SELECT cefr_seviyyesi, COUNT(*) as say
        FROM yenilenmi_standartlar
        WHERE cefr_seviyyesi IS NOT NULL AND cefr_seviyyesi != ''
        GROUP BY cefr_seviyyesi ORDER BY cefr_seviyyesi"),
      function() data.frame()
    )
    if (nrow(df) == 0) return(plotly_empty())
    plot_ly(df, x = ~cefr_seviyyesi, y = ~say, type = "bar",
            marker = list(color = c("#BBDEFB","#90CAF9","#42A5F5","#1976D2","#0D47A1"))) %>%
      layout(xaxis = list(title = "CEFR"), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })

  output$chart_bloom_all <- renderPlotly({
    df <- db_query(
      function(con) dbGetQuery(con, "
        SELECT bloom_seviyyesi, COUNT(*) as say
        FROM yenilenmi_standartlar
        WHERE bloom_seviyyesi IS NOT NULL AND bloom_seviyyesi != ''
        GROUP BY bloom_seviyyesi"),
      function() {
        if (!is.null(CSV_MOVCUD) && "bloom_seviyyesi" %in% names(CSV_MOVCUD)) {
          bdf <- CSV_MOVCUD[!is.na(CSV_MOVCUD$bloom_seviyyesi) & nchar(CSV_MOVCUD$bloom_seviyyesi) > 0, ]
          if (nrow(bdf) > 0) {
            agg <- as.data.frame(table(bdf$bloom_seviyyesi)); names(agg) <- c("bloom_seviyyesi","say"); agg
          } else data.frame()
        } else data.frame()
      }
    )
    if (nrow(df) == 0) return(plotly_empty())
    plot_ly(df, labels = ~bloom_seviyyesi, values = ~say, type = "pie",
            textinfo = "label+percent") %>%
      layout(font = list(family = "Noto Sans"))
  })

  # --- CEDVELLER ---
  output$dt_standartlar <- renderDT({
    df <- data_yenilenmi()
    if (nrow(df) == 0) {
      # yenilenmi bosdursa, movcud-u goster
      df <- data_movcud()
      if (nrow(df) == 0) return(datatable(data.frame(Mesaj = "Melumat yoxdur")))
      show_cols <- intersect(c("standart_kodu", "standart_metni", "alt_standart_metni", "mezmun_xetti",
                                "bloom_seviyyesi"), names(df))
      return(datatable(df[, show_cols], options = list(pageLength = 20), rownames = FALSE))
    }

    show_cols <- c("standart_kodu", "standart_metni", "alt_standart_metni",
                   "mezmun_xetti", "deyisiklik_novu", "bloom_seviyyesi", "cefr_seviyyesi",
                   "esaslandirma")
    show_cols <- intersect(show_cols, names(df))

    datatable(df[, show_cols],
              options = list(pageLength = 20, language = list(url = "")),
              rownames = FALSE,
              colnames = c("Kod", "Standart", "Alt standart", "Mezmun xetti",
                           "Deyisiklik", "Bloom", "CEFR", "Esaslandirma")[1:length(show_cols)]) %>%
      formatStyle("deyisiklik_novu",
                  backgroundColor = styleEqual(
                    c("movcud", "yenilenib", "yeni", "silinib"),
                    c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#FFEBEE")))
  })

  output$dt_kohne <- renderDT({
    df <- data_movcud()
    if (nrow(df) == 0) return(datatable(data.frame(Mesaj = "Melumat yoxdur")))

    show_cols <- intersect(c("standart_kodu", "standart_metni", "alt_standart_metni", "mezmun_xetti"), names(df))
    datatable(df[, show_cols], options = list(pageLength = 15), rownames = FALSE)
  })

  output$dt_yeni <- renderDT({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(datatable(data.frame(Mesaj = "Melumat yoxdur")))

    show_cols <- intersect(c("standart_kodu", "standart_metni", "alt_standart_metni",
                             "deyisiklik_novu", "esaslandirma"), names(df))
    datatable(df[, show_cols], options = list(pageLength = 15), rownames = FALSE) %>%
      formatStyle("deyisiklik_novu",
                  backgroundColor = styleEqual(
                    c("movcud", "yenilenib", "yeni", "silinib"),
                    c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#FFEBEE")))
  })

  output$dt_beynelxalq <- renderDT({
    sinif <- as.integer(input$sinif_sec)
    df <- db_query(
      function(con) {
        dbGetQuery(con, sprintf("
          SELECT cerceve_adi, kateqoriya, alt_kateqoriya, tesvir
          FROM beynelxalq_cerceveler
          WHERE %d BETWEEN SPLIT_PART(sinif_araligi, '-', 1)::int
                      AND SPLIT_PART(sinif_araligi, '-', 2)::int
          ORDER BY cerceve_adi", sinif))
      },
      function() data.frame()
    )
    if (nrow(df) == 0) return(datatable(data.frame(Mesaj = "Melumat yoxdur")))
    datatable(df, options = list(pageLength = 15), rownames = FALSE,
              colnames = c("Cerceve", "Kateqoriya", "Alt kateqoriya", "Tesvir"))
  })

  output$dt_olkeler <- renderDT({
    df <- db_query(
      function(con) {
        query <- "SELECT olke, mezmun_xetti, standart_metni, xususi_yanasma FROM olke_standartlari"
        if (input$olke_sec != "Hamisi") query <- paste0(query, sprintf(" WHERE olke = '%s'", input$olke_sec))
        dbGetQuery(con, query)
      },
      function() data.frame()
    )
    if (nrow(df) == 0) return(datatable(data.frame(Mesaj = "Melumat yoxdur")))
    datatable(df, options = list(pageLength = 10), rownames = FALSE,
              colnames = c("Olke", "Mezmun", "Standart", "Xususi yanasma"))
  })

  # --- AI TEHLIL (canli taymer + token izleme + xerc) ---
  observeEvent(input$btn_analyze, {
    sinif <- input$sinif_sec

    # Movcud standartlari gotir
    df <- db_query(
      function(con) {
        dbGetQuery(con, sprintf("SELECT standart_kodu, standart_metni, alt_standart_kodu, alt_standart_metni,
                                        mezmun_xetti, pisa_seviyyesi, pirls_kateqoriya, bloom_seviyyesi
                                 FROM movcud_standartlar WHERE sinif = %s
                                 ORDER BY mezmun_xetti, standart_kodu", sinif))
      },
      function() {
        if (!is.null(CSV_MOVCUD)) {
          d <- CSV_MOVCUD[CSV_MOVCUD$sinif == as.integer(sinif), ]
          cols <- intersect(c("standart_kodu","standart_metni","alt_standart_kodu","alt_standart_metni",
                              "mezmun_xetti","pisa_seviyyesi","pirls_kateqoriya","bloom_seviyyesi"), names(d))
          d[order(d$mezmun_xetti, d$standart_kodu), cols, drop=FALSE]
        } else data.frame()
      }
    )

    if (nrow(df) == 0) {
      output$ai_result <- renderUI(tags$div(style="padding:30px;color:#dc2626;",
        tags$h3("Melumat yoxdur"), tags$p("Bu sinif ucun standart tapilmadi.")))
      return()
    }

    # Taymer baslat
    session$sendCustomMessage("ai_timer_start", list(
      target = "ai_timer_panel",
      status = "Claude AI tehlil edir...",
      info1 = paste0(sinif, "-ci sinif"),
      info2 = paste0(nrow(df), " standart")
    ))

    output$ai_token_display <- renderUI(
      tags$div(class = "token-display token-waiting", icon("hourglass-half"), " AI isleyir...")
    )
    output$ai_result <- renderUI(NULL)

    # Prompt
    prompt <- sprintf(
      'Sen Azerbaycan dili kurikulumu ekspertisin.
%s-ci sinif ucun asagidaki movcud standartlari tehlil et.

Standartlar (JSON):
%s

Azerbaycan dilinde etrafli tehlil yaz:
1. Bu sinif ucun umumi qiymetlendirme
2. PISA/PIRLS telebherine uygunluq derecesi
3. Bloom taksonomiyasi balansi
4. Hansi saheler gucludur, hansilari zeifdir
5. Yenilenme teklifleri - hansi standartlar yenilenmeli, hansilari saxlanmali
6. Yeni elave olunmali standart teklifleri
7. CEFR %s seviyyesine uygunlug

Cavabi HTML formatinda (sade HTML tagleri ile) ver.',
      sinif,
      toJSON(df, pretty = TRUE, auto_unbox = TRUE),
      cefr_map[as.character(sinif)])

    # API cagiris (async)
    session$onFlushed(function() {
      res <- call_claude(prompt)

      if (res$success) {
        # Taymer dayandır - ugurlu
        session$sendCustomMessage("ai_timer_stop", list(
          target = "ai_timer_panel", ok = TRUE,
          elapsed = sprintf("%.1f", res$time_sec),
          inp = formatC(res$input_tokens, format = "d", big.mark = ","),
          out = formatC(res$output_tokens, format = "d", big.mark = ","),
          cost = sprintf("$%.4f", (res$input_tokens * 3 + res$output_tokens * 15) / 1e6)
        ))

        # Token gostericisi
        total_tok <- res$input_tokens + res$output_tokens
        output$ai_token_display <- renderUI(
          tags$div(class = "token-display token-done", icon("check-circle"),
            sprintf(" %.1f san | %s token | $%.4f",
                    res$time_sec,
                    formatC(total_tok, format = "d", big.mark = ","),
                    (res$input_tokens * 3 + res$output_tokens * 15) / 1e6))
        )

        # Neticeni html_reports qovluguna yaz
        stats_html <- make_stats_bar(res$time_sec, res$input_tokens, res$output_tokens)
        report_dir <- file.path(getwd(), "html_reports")
        if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
        report_file <- file.path(report_dir, sprintf("sinif_%s_tehlil.html", sinif))
        report_content <- paste0(
          '<!DOCTYPE html>\n<html lang="az">\n<head>\n<meta charset="UTF-8">\n',
          '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
          '<title>Sinif ', sinif, ' - AI Tehlil Neticesi</title>\n',
          '<style>\n',
          'body{font-family:"Noto Sans","Segoe UI",sans-serif;font-size:18px;line-height:1.85;max-width:1100px;margin:0 auto;padding:40px;background:#f8fafc;color:#1e293b}',
          '\nh1{color:#0D47A1;font-size:28px;border-bottom:3px solid #1976D2;padding-bottom:12px}',
          '\nh2{color:#1565C0;font-size:24px;margin-top:28px}',
          '\nh3{color:#1976D2;font-size:21px;margin-top:22px}',
          '\np{margin-bottom:12px}',
          '\nul,ol{margin-left:24px}li{margin-bottom:6px}',
          '\ntable{width:100%;border-collapse:collapse;margin:18px 0;font-size:17px}',
          '\nth{background:#1976D2;color:white;padding:12px;text-align:left}',
          '\ntd{padding:10px 12px;border-bottom:1px solid #e2e8f0}',
          '\ntr:nth-child(even){background:#f1f5f9}',
          '\n.header{background:linear-gradient(135deg,#0D47A1,#1976D2);color:white;padding:30px;border-radius:14px;margin-bottom:30px}',
          '\n.header h1{color:white;border:none;margin:0}',
          '\n.header p{color:#bbdefb;margin:8px 0 0}',
          '\n.stats{background:linear-gradient(135deg,#0f172a,#1e293b);color:#e2e8f0;padding:20px 28px;border-radius:14px;margin-top:30px}',
          '\n.stats .title{font-size:1.3em;font-weight:700;color:#fbbf24;margin-bottom:14px}',
          '\n.stats .grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:12px}',
          '\n.stats .item{background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px}',
          '\n.stats .item .label{font-size:0.9em;color:#94a3b8}',
          '\n.stats .item .value{font-size:1.5em;font-weight:700}',
          '\n.footer{text-align:center;margin-top:30px;color:#94a3b8;font-size:14px}',
          '\n</style>\n</head>\n<body>\n',
          '<div class="header"><h1>Sinif ', sinif, ' - Azerbaycan Dili Standartlari Tehlili</h1>',
          '<p>CEFR: ', cefr_map[as.character(sinif)], ' | Model: ', CLAUDE_MODEL, ' | Tarix: ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</p></div>\n',
          res$text, '\n',
          '<div class="stats"><div class="title">Generasiya Statistikasi</div><div class="grid">',
          '<div class="item"><div class="label">Vaxt</div><div class="value" style="color:#66BB6A;">', sprintf("%.1f", res$time_sec), ' san</div></div>',
          '<div class="item"><div class="label">Giris token</div><div class="value" style="color:#4ade80;">', formatC(res$input_tokens, format="d", big.mark=","), '</div></div>',
          '<div class="item"><div class="label">Cixis token</div><div class="value" style="color:#fbbf24;">', formatC(res$output_tokens, format="d", big.mark=","), '</div></div>',
          '<div class="item"><div class="label">Cemi token</div><div class="value" style="color:#f87171;">', formatC(res$input_tokens + res$output_tokens, format="d", big.mark=","), '</div></div>',
          '<div class="item"><div class="label">Texmini qiymet</div><div class="value" style="color:#c4b5fd;">$', sprintf("%.4f", (res$input_tokens * 3 + res$output_tokens * 15) / 1e6), '</div></div>',
          '</div></div>\n',
          '<div class="footer">Claude AI ile generasiya olunub | ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</div>\n',
          '</body>\n</html>')
        writeLines(report_content, report_file, useBytes = FALSE)
        cat(sprintf("   HTML yazildi: %s\n", report_file))

        # Neticeni goster + statistika paneli + fayl mesaji
        output$ai_result <- renderUI(tagList(
          tags$div(class = "ai-output", HTML(res$text)),
          HTML(stats_html),
          tags$div(style = "margin-top:16px;padding:12px 20px;background:#dcfce7;border:1px solid #86efac;border-radius:10px;font-size:16px;color:#166534;",
            icon("file-code"), sprintf(" Netice yazildi: html_reports/sinif_%s_tehlil.html", sinif))
        ))

      } else {
        # Xeta
        session$sendCustomMessage("ai_timer_stop", list(
          target = "ai_timer_panel", ok = FALSE,
          elapsed = sprintf("%.1f", res$time_sec),
          inp = "0", out = "0", cost = "$0"
        ))

        output$ai_token_display <- renderUI(
          tags$div(class = "token-display token-error", icon("times-circle"),
                   sprintf(" Xeta (%.1f san)", res$time_sec))
        )
        output$ai_result <- renderUI(tags$div(style = "padding:30px;color:#dc2626;",
          tags$h3("Xeta bas verdi"), tags$p(res$error)))
      }
    }, once = TRUE)
  })

  # --- EXPORT ---
  output$download_html <- downloadHandler(
    filename = function() { sprintf("sinif_%s_standartlar.html", input$sinif_sec) },
    content = function(file) {
      html_path <- sprintf("html_reports/sinif_%s_standartlar.html", input$sinif_sec)
      if (file.exists(html_path)) {
        file.copy(html_path, file)
      } else {
        writeLines("<h1>HTML hesabat hele yaradilmayib!</h1><p>03_generate_html.R skriptini ise salin.</p>", file)
      }
    },
    contentType = "text/html"
  )

  output$download_csv <- downloadHandler(
    filename = function() { sprintf("standartlar_sinif_%s.csv", input$sinif_sec) },
    content = function(file) {
      df <- data_yenilenmi()
      if (nrow(df) == 0) df <- data_movcud()
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# --- ISE SALMAQ ---
# Binder ucun PORT ve host deyisenleri
port <- as.integer(Sys.getenv("PORT", "4567"))
options(shiny.port = port, shiny.host = "0.0.0.0")
shinyApp(ui, server)
