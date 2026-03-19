# ============================================================
# app.R — Azərbaycan Dili Standartları Dashboard
# R Shiny ilə interaktiv idarəetmə paneli
# ============================================================

library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(httr)
library(jsonlite)
library(plotly)

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
cat("   API KEY:", if (nchar(CLAUDE_API_KEY) >= 10) paste0("mövcuddur (", nchar(CLAUDE_API_KEY), " simvol)") else "TAPILMADI", "\n")

# --- Bazaya qoşulma funksiyası (CSV fallback ilə) ---
USE_CSV <- FALSE
tryCatch({
  .test_con <- dbConnect(RPostgres::Postgres(),
                         dbname = DB_CONFIG$dbname, host = DB_CONFIG$host,
                         port = DB_CONFIG$port, user = DB_CONFIG$user,
                         password = DB_CONFIG$password)
  dbDisconnect(.test_con)
  cat("   PostgreSQL: bağlantı uğurlu\n")
}, error = function(e) {
  USE_CSV <<- TRUE
  cat("   PostgreSQL yoxdur, CSV rejimində işləyir\n")
})

get_con <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = DB_CONFIG$dbname, host = DB_CONFIG$host,
            port = DB_CONFIG$port, user = DB_CONFIG$user,
            password = DB_CONFIG$password)
}

# CSV-dən data oxuma funksiyaları
csv_movcud <- function() {
  f <- file.path(getwd(), "data", "movcud_standartlar_backup.csv")
  if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  else data.frame()
}

csv_yenilenmi <- function() {
  f <- file.path(getwd(), "data", "yenilenmi_standartlar_backup.csv")
  if (file.exists(f)) {
    df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    if (nrow(df) == 0) return(data.frame())
    df
  } else data.frame()
}

# --- CEFR xəritəsi ---
cefr_map <- c("1"="A1","2"="A1","3"="A2","4"="A2","5"="B1",
              "6"="B1","7"="B1","8"="B2","9"="B2","10"="C1","11"="C1")

# --- Claude API (token izləmə ilə) ---
CLAUDE_MODEL <- "claude-sonnet-4-20250514"
CLAUDE_ENDPOINT <- "https://api.anthropic.com/v1/messages"

call_claude <- function(prompt) {
  if (nchar(CLAUDE_API_KEY) < 10) return(list(success = FALSE, error = "API açarı təyin edilməyib!",
                                               time_sec = 0, input_tokens = 0, output_tokens = 0))
  t0 <- proc.time()["elapsed"]
  tryCatch({
    resp <- POST(CLAUDE_ENDPOINT,
      add_headers(`x-api-key` = CLAUDE_API_KEY, `anthropic-version` = "2023-06-01", `content-type` = "application/json"),
      body = toJSON(list(
        model = CLAUDE_MODEL,
        max_tokens = 16384L,
        messages = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE),
      encode = "raw", timeout(600))
    elapsed <- round(as.numeric(proc.time()["elapsed"] - t0), 1)
    res <- content(resp, "parsed", encoding = "UTF-8")
    inp_tok <- as.integer(if (!is.null(res$usage$input_tokens)) res$usage$input_tokens else 0)
    out_tok <- as.integer(if (!is.null(res$usage$output_tokens)) res$usage$output_tokens else 0)
    if (resp$status_code == 200) {
      txt <- if (length(res$content) > 0 && !is.null(res$content[[1]]$text)) res$content[[1]]$text else ""
      list(success = TRUE, text = txt, time_sec = elapsed, input_tokens = inp_tok, output_tokens = out_tok)
    } else {
      err_msg <- if (!is.null(res$error)) res$error$message else paste("HTTP", resp$status_code)
      list(success = FALSE, error = err_msg, time_sec = elapsed, input_tokens = inp_tok, output_tokens = out_tok)
    }
  }, error = function(e) {
    elapsed <- round(as.numeric(proc.time()["elapsed"] - t0), 1)
    list(success = FALSE, error = e$message, time_sec = elapsed, input_tokens = 0, output_tokens = 0)
  })
}

# --- JSON çıxarma (```json ... ``` təmizləmə) ---
extract_json_array <- function(txt) {
  # ```json ... ``` blokundan təmizlə
  txt <- gsub("```json\\s*", "", txt)
  txt <- gsub("```\\s*$", "", txt)
  txt <- gsub("```", "", txt)
  # [ ... ] arasını tap
  m <- regmatches(txt, regexpr("\\[[\\s\\S]*\\]", txt, perl = TRUE))
  if (length(m) == 0) stop("JSON massivi tapılmadı")
  m[1]
}

# --- Statistika paneli (token/xərc) ---
make_stats_bar <- function(time_sec, input_tokens, output_tokens) {
  total <- input_tokens + output_tokens
  cost <- round((input_tokens * 3 + output_tokens * 15) / 1e6, 4)
  paste0(
    '<div style="background:linear-gradient(135deg,#0f172a,#1e293b);color:#e2e8f0;padding:20px 28px;border-radius:14px;margin-top:24px;box-shadow:0 4px 20px rgba(0,0,0,0.2);">',
    '<div style="font-size:1.5em;font-weight:700;margin-bottom:14px;color:#fbbf24;">Generasiya Statistikası</div>',
    '<div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px;">',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #2E7D32;"><div style="font-size:0.98em;color:#94a3b8;">Vaxt</div><div style="font-size:1.6em;font-weight:700;color:#66BB6A;">', sprintf("%.1f", time_sec), ' san</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #22c55e;"><div style="font-size:0.98em;color:#94a3b8;">Giriş token</div><div style="font-size:1.6em;font-weight:700;color:#4ade80;">', formatC(input_tokens, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #f59e0b;"><div style="font-size:0.98em;color:#94a3b8;">Çıxış token</div><div style="font-size:1.6em;font-weight:700;color:#fbbf24;">', formatC(output_tokens, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #ef4444;"><div style="font-size:0.98em;color:#94a3b8;">Cəmi token</div><div style="font-size:1.6em;font-weight:700;color:#f87171;">', formatC(total, format = "d", big.mark = ","), '</div></div>',
    '<div style="background:rgba(255,255,255,0.06);padding:14px 18px;border-radius:10px;border-left:3px solid #a78bfa;"><div style="font-size:0.98em;color:#94a3b8;">Təxmini qiymət</div><div style="font-size:1.6em;font-weight:700;color:#c4b5fd;">$', sprintf("%.4f", cost), '</div></div>',
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
      "Azərbaycan Dili Standartları"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Ana Səhifə", tabName = "home", icon = icon("home")),
      menuItem("Standartlar", tabName = "standards", icon = icon("book")),
      menuItem("Müqayisə", tabName = "compare", icon = icon("columns")),
      menuItem("Beynəlxalq", tabName = "international", icon = icon("globe")),
      menuItem("Statistika", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Claude AI Təhlil", tabName = "ai_analysis", icon = icon("robot")),
      menuItem("Yekun Standartlar", tabName = "yekun", icon = icon("check-double")),
      menuItem("Yeni Təkliflər", tabName = "yeni_teklifler_tab", icon = icon("lightbulb")),
      menuItem("HTML Export", tabName = "export", icon = icon("download"))
    ),
    hr(),
    div(style = "padding: 15px;",
      selectInput("sinif_sec", "🎓 Sinif seçin:",
                  choices = setNames(1:11, paste0(1:11, "-ci sinif")),
                  selected = 1),
      selectInput("mezmun_sec", "📌 Məzmun xətti:",
                  choices = c("Hamısı" = "all", "Dinləmə", "Danışma", "Oxu", "Yazı", "Dil qaydaları"),
                  selected = "all"),
      selectInput("deyisiklik_sec", "🔄 Dəyişiklik növü:",
                  choices = c("Hamısı" = "all", "Mövcud" = "movcud", "Yenilənib" = "yenilenib",
                              "Yeni" = "yeni", "Silinib" = "silinib"),
                  selected = "all"),
      hr(),
      div(style = "text-align:center; color:#7f8c8d; font-size:12px;",
          "CEFR Səviyyəsi:",
          textOutput("cefr_display", inline = TRUE))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      @import url("https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300;400;500;600;700&display=swap");
      body, .content-wrapper, .main-sidebar { font-family: "Noto Sans", sans-serif; }
      .content-wrapper { background-color: #f0f4f8; }
      .info-box .info-box-number { font-size: 28px; }
      .box { border-top: 3px solid #1976D2; border-radius: 8px; overflow-x: auto; }
      .skin-blue .main-header .logo { background-color: #0D47A1; font-weight: 600; }
      .skin-blue .main-header .navbar { background-color: #1565C0; }
      .skin-blue .main-sidebar { background-color: #0D47A1; }
      .skin-blue .sidebar-menu > li.active > a { background-color: #1565C0; border-left-color: #64B5F6; }
      .small-box { border-radius: 10px; }
      
      /* Cədvəl düzəlişləri */
      .dataTables_wrapper { overflow-x: auto; }
      table.dataTable { font-size: 14px; width: 100% !important; }
      table.dataTable td { 
        white-space: normal !important; 
        word-wrap: break-word;
        max-width: 350px;
        padding: 8px 10px !important;
        vertical-align: top;
      }
      table.dataTable th { 
        white-space: nowrap; 
        padding: 10px !important;
        font-size: 13px;
      }
      
      /* Boş mesaj stili */
      .bos-mesaj {
        text-align: center; padding: 40px 20px;
        color: #90A4AE; font-size: 16px;
      }
      .bos-mesaj .icon { font-size: 40px; margin-bottom: 10px; }
      
      /* ===== MÜQAYİSƏ SPLIT PANEL ===== */
      .compare-container {
        display: flex;
        gap: 0;
        height: calc(100vh - 180px);
        min-height: 500px;
      }
      .compare-panel {
        flex: 1;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 15px;
        border: 1px solid #CFD8DC;
        background: #fff;
        border-radius: 8px;
      }
      .compare-panel-left {
        border-right: none;
        border-radius: 8px 0 0 8px;
        resize: horizontal;
        min-width: 300px;
        max-width: 70%;
      }
      .compare-panel-right {
        border-radius: 0 8px 8px 0;
        min-width: 300px;
      }
      .compare-divider {
        width: 8px;
        background: linear-gradient(180deg, #1976D2, #42A5F5);
        cursor: col-resize;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
        border-radius: 4px;
        position: relative;
      }
      .compare-divider::after {
        content: "⋮";
        color: white;
        font-size: 18px;
        font-weight: bold;
      }
      .compare-header {
        font-size: 16px;
        font-weight: 600;
        padding: 10px 15px;
        margin: -15px -15px 15px -15px;
        border-bottom: 2px solid #E0E0E0;
      }
      .compare-header-left {
        background: linear-gradient(90deg, #FFF8E1, #FFF);
        color: #E65100;
      }
      .compare-header-right {
        background: linear-gradient(90deg, #E8F5E9, #FFF);
        color: #2E7D32;
      }
      
      /* Standart kartları müqayisədə */
      .cmp-card {
        border: 1px solid #E0E0E0;
        border-radius: 8px;
        margin-bottom: 12px;
        padding: 12px 15px;
        font-size: 14px;
        line-height: 1.7;
        transition: box-shadow 0.2s;
      }
      .cmp-card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
      .cmp-card-left {
        border-left: 4px solid #FF9800;
        background: #FFFDE7;
      }
      .cmp-card-right-movcud {
        border-left: 4px solid #90CAF9;
        background: #FAFAFA;
      }
      .cmp-card-right-yenilenib {
        border-left: 4px solid #4CAF50;
        background: #F1F8E9;
      }
      .cmp-card-right-yeni {
        border-left: 4px solid #FF9800;
        background: #FFF3E0;
      }
      .cmp-card-right-silinib {
        border-left: 4px solid #F44336;
        background: #FFEBEE;
        text-decoration: line-through;
        opacity: 0.6;
      }
      .cmp-kod {
        font-family: "Courier New", monospace;
        font-weight: 700;
        color: #0D47A1;
        font-size: 13px;
        margin-bottom: 5px;
      }
      .cmp-mezmun {
        display: inline-block;
        font-size: 11px;
        background: #E3F2FD;
        color: #1565C0;
        padding: 2px 8px;
        border-radius: 10px;
        margin-left: 8px;
      }
      .cmp-badge {
        display: inline-block;
        font-size: 11px;
        padding: 2px 8px;
        border-radius: 10px;
        margin-left: 5px;
      }
      .cmp-badge-movcud { background: #E3F2FD; color: #1565C0; }
      .cmp-badge-yenilenib { background: #C8E6C9; color: #2E7D32; }
      .cmp-badge-yeni { background: #FFE0B2; color: #E65100; }
      .cmp-badge-silinib { background: #FFCDD2; color: #C62828; }
      
      /* ===== DİFF RENKLEME ===== */
      .diff-added {
        background-color: #C8E6C9;
        color: #1B5E20;
        padding: 1px 3px;
        border-radius: 3px;
        font-weight: 500;
      }
      .diff-same { color: #424242; }

      /* AI taymer/token stilləri */
      .token-display{display:inline-flex;align-items:center;gap:8px;font-size:1.2em;font-weight:700;padding:10px 18px;border-radius:10px;margin-top:15px}
      .token-waiting{background:#fef3c7;color:#92400e;border:1px solid #fde68a}
      .token-done{background:#dcfce7;color:#166534;border:1px solid #86efac}
      .token-error{background:#fef2f2;color:#991b1b;border:1px solid #fca5a5}
      .live-timer-panel{background:linear-gradient(135deg,#0f172a,#1e293b);border:2px solid #22c55e;border-radius:16px;padding:32px 40px;margin:20px 0;text-align:center;box-shadow:0 4px 24px rgba(46,125,50,.15)}
      .live-timer-panel .t-status{font-size:1.3em;color:#94a3b8;margin-bottom:10px}
      .live-timer-panel .t-clock{font-family:"JetBrains Mono",monospace;font-size:3.5em;font-weight:700;color:#66BB6A;letter-spacing:.06em;margin:8px 0}
      .live-timer-panel .t-start{font-size:1em;color:#64748b;margin-bottom:14px}
      .live-timer-panel .t-details{display:flex;justify-content:center;gap:16px;flex-wrap:wrap}
      .live-timer-panel .t-item{background:rgba(255,255,255,.06);padding:10px 20px;border-radius:10px;font-size:1em;color:#cbd5e1}
      .pdot{display:inline-block;width:12px;height:12px;background:#22c55e;border-radius:50%;margin-right:8px;animation:pdot 1s infinite}
      @keyframes pdot{0%,100%{opacity:1}50%{opacity:.3}}
      .ai-output{font-size:16px;line-height:1.85;padding:24px;background:white;border-radius:12px;border:1px solid #e2e8f0;margin-top:16px}
      .ai-output h1{color:#0D47A1;margin-top:24px;font-size:24px}
      .ai-output h2{color:#0D47A1;margin-top:22px;font-size:21px}
      .ai-output h3{color:#0D47A1;margin-top:20px;font-size:19px}
      .ai-output table{width:100%;border-collapse:collapse;margin:18px 0;font-size:15px}
      .ai-output th{background:#1976D2;color:white;padding:10px;text-align:left}
      .ai-output td{padding:8px 10px;border-bottom:1px solid #e2e8f0}
      .ai-output tr:nth-child(even){background:#f8fafc}
    '))),

    # JavaScript: canlı taymer
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
          "<div class=\\"t-start\\">Başlama: " + st + "</div>" +
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
        var lb = m.ok ? "Tamamlandı" : "Xəta baş verdi";
        e.innerHTML = "<div class=\\"live-timer-panel\\" style=\\"" + cl + "\\">" +
          "<div class=\\"t-status\\" style=\\"color:" + co + "\\">" + lb + "</div>" +
          "<div class=\\"t-clock\\" style=\\"color:" + co + "\\">" + m.elapsed + " san</div>" +
          "<div class=\\"t-details\\">" +
            "<div class=\\"t-item\\">Giriş: " + m.inp + "</div>" +
            "<div class=\\"t-item\\">Çıxış: " + m.out + "</div>" +
            "<div class=\\"t-item\\">" + m.cost + "</div>" +
          "</div></div>";
      });
    ')),

    tabItems(
      # ========== ANA SƏHİFƏ ==========
      tabItem(tabName = "home",
        fluidRow(
          valueBoxOutput("vb_umumi", width = 3),
          valueBoxOutput("vb_movcud", width = 3),
          valueBoxOutput("vb_yenilenib", width = 3),
          valueBoxOutput("vb_yeni", width = 3)
        ),
        fluidRow(
          box(title = "📊 Siniflər üzrə Standart Sayı", width = 8, 
              status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_sinifler", height = "400px")),
          box(title = "ℹ️ Layihə Haqqında", width = 4, status = "info", solidHeader = TRUE,
              div(style = "font-size: 15px; line-height: 1.8;",
                tags$p("Bu dashboard Azərbaycan dili standartlarının beynəlxalq tələblərə uyğun yenilənməsini idarə edir."),
                tags$hr(),
                tags$p(tags$strong("Beynəlxalq çərçəvələr:")),
                tags$ul(
                  tags$li("PISA — Oxu savadlılığı"),
                  tags$li("PIRLS — Oxu bacarıqları"),
                  tags$li("CEFR — Avropa Dil Çərçəvəsi"),
                  tags$li("Bloom Taksonomiyası")
                ),
                tags$p(tags$strong("Aparıcı ölkələr:")),
                tags$p("🇫🇮 Finlandiya • 🇸🇬 Sinqapur • 🇪🇪 Estoniya"),
                tags$p("🇯🇵 Yaponiya • 🇨🇦 Kanada • 🇮🇪 İrlandiya")
              ))
        )
      ),
      
      # ========== STANDARTLAR ==========
      tabItem(tabName = "standards",
        fluidRow(
          box(title = "📚 Standartlar", width = 12, status = "primary", solidHeader = TRUE,
              div(style = "margin-bottom: 15px; display:flex; gap:8px; flex-wrap:wrap;",
                actionButton("filter_all", "Hamısı", class = "btn btn-default",
                             style = "font-size:14px; font-weight:600; border:2px solid #1976D2; color:#1976D2;"),
                actionButton("filter_movcud", "Mövcud", class = "btn btn-default",
                             style = "font-size:14px; font-weight:600; background:#E3F2FD; border:2px solid #90CAF9; color:#0D47A1;"),
                actionButton("filter_yenilenib", "Yenilənib", class = "btn btn-default",
                             style = "font-size:14px; font-weight:600; background:#E8F5E9; border:2px solid #4CAF50; color:#2E7D32;"),
                actionButton("filter_yeni", "Yeni", class = "btn btn-default",
                             style = "font-size:14px; font-weight:600; background:#FFF3E0; border:2px solid #FF9800; color:#E65100;"),
                actionButton("filter_silinib", "Silinib", class = "btn btn-default",
                             style = "font-size:14px; font-weight:600; background:#FFEBEE; border:2px solid #F44336; color:#C62828;")
              ),
              uiOutput("ui_standartlar"))
        )
      ),
      
      # ========== MÜQAYİSƏ ==========
      tabItem(tabName = "compare",
        fluidRow(
          column(12,
            div(style="padding:10px 15px;background:#E3F2FD;border-radius:8px;margin-bottom:15px;",
              tags$span(style="font-weight:600;color:#0D47A1;", "Müqayisə: "),
              tags$span(style="display:inline-block;width:18px;height:18px;background:#FFFFFF;border:1px solid #ccc;border-radius:3px;margin:0 4px;vertical-align:middle;"), " Dəyişməmiş  ",
              tags$span(style="display:inline-block;width:18px;height:18px;background:#FFF8E1;border:1px solid #FF9800;border-radius:3px;margin:0 4px;vertical-align:middle;"), " Yenilənib  ",
              tags$span(style="display:inline-block;width:18px;height:18px;background:#FFEBEE;border:1px solid #F44336;border-radius:3px;margin:0 4px;vertical-align:middle;"), " Silinib  ",
              tags$span(style="display:inline-block;width:18px;height:18px;background:#E8F5E9;border:1px solid #4CAF50;border-radius:3px;margin:0 4px;vertical-align:middle;"), " Yeni əlavə"
            )
          )
        ),
        fluidRow(
          box(title = NULL, width = 12, solidHeader = FALSE,
            uiOutput("ui_compare_table")
          )
        )
      ),
      
      # ========== BEYNƏLXALQ ==========
      tabItem(tabName = "international",
        fluidRow(
          box(title = "🌐 PISA/PIRLS/CEFR Uyğunluğu", width = 12, status = "primary", solidHeader = TRUE,
              DTOutput("dt_beynelxalq"))
        ),
        fluidRow(
          box(title = "🏳️ Ölkə Standartları", width = 12, status = "info", solidHeader = TRUE,
              div(style = "margin-bottom: 15px;",
                selectInput("olke_sec", "Ölkə seçin:", width = "300px",
                            choices = c("Hamısı", "Finlandiya", "Sinqapur", "Estoniya", 
                                        "Yaponiya", "Kanada", "İrlandiya"))
              ),
              DTOutput("dt_olkeler"))
        ),
        fluidRow(
          box(title = "📊 Bloom Taksonomiyası Paylanması", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_bloom", height = "350px"))
        )
      ),
      
      # ========== STATİSTİKA ==========
      tabItem(tabName = "stats",
        fluidRow(
          box(title = "📈 Dəyişiklik Növləri üzrə Paylanma", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_deyisiklik", height = "400px")),
          box(title = "📊 Məzmun Xətləri üzrə", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("chart_mezmun", height = "400px"))
        ),
        fluidRow(
          box(title = "🎯 CEFR Səviyyələri üzrə", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("chart_cefr", height = "350px")),
          box(title = "📚 Bloom Səviyyələri üzrə", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("chart_bloom_all", height = "350px"))
        )
      ),
      
      # ========== AI TƏHLİL ==========
      tabItem(tabName = "ai_analysis",
        # --- ADDIM 1: TƏHLİL ---
        fluidRow(
          box(title = "📋 Addım 1: Standartların Təhlili", width = 12, status = "primary", solidHeader = TRUE,
              tags$p(style="font-size:15px; color:#334155; margin-bottom:15px;",
                "Seçilmiş sinifin mövcud standartları PISA, PIRLS, CEFR, Bloom və 6 ölkə standartları əsasında təhlil ediləcək.",
                tags$br(), "Hər standarta status veriləcək: ✅ Qalsın, ✏️ Yenilənsin, ❌ Silinsin + Yeni standart təklifləri."),
              actionButton("btn_step1", "🔍 Addım 1: Təhlil et", class = "btn-primary btn-lg",
                           style = "font-size:16px; margin-bottom:15px;"),
              div(id = "ai_timer_panel"),
              uiOutput("ai_token_display"),
              uiOutput("ai_result"))
        ),
        # --- ADDIM 2: SEÇİM VƏ YENİLƏMƏ ---
        fluidRow(
          box(title = "✏️ Addım 2: Standartları Seçin və Yeniləyin", width = 12, status = "success", solidHeader = TRUE,
              tags$p(style="font-size:15px; color:#334155; margin-bottom:15px;",
                "Hər standart üçün status seçin: Saxla, Yenilə, Sil. Sonra AI yalnız 'Yenilə' seçilmişləri yenidən yazacaq."),
              div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:15px;",
                actionButton("btn_step2_load", "📥 Standartları yüklə", class = "btn-info btn-lg",
                             style = "font-size:15px;"),
                actionButton("btn_step2_ai", "🤖 Seçilmişləri AI ilə yenilə", class = "btn-success btn-lg",
                             style = "font-size:15px; font-weight:600;"),
                actionButton("btn_step2_add_new", "➕ AI ilə yeni standart təklif et", class = "btn-warning btn-lg",
                             style = "font-size:15px;")
              ),
              div(id = "ai_timer_panel2"),
              uiOutput("step2_status"),
              DTOutput("dt_step2_review"))
        ),
        # --- ADDIM 3: YADDA SAXLA ---
        fluidRow(
          box(title = "💾 Addım 3: Bazaya Yaz", width = 12, status = "warning", solidHeader = TRUE,
              tags$p(style="font-size:15px; color:#334155; margin-bottom:15px;",
                "Yuxarıdakı cədvəli nəzərdən keçirdikdən sonra dəyişiklikləri bazaya yazın.",
                tags$br(), "Yazdıqdan sonra 'Standartlar' tab-ında filtr düymələri ilə görə bilərsiniz."),
              actionButton("btn_step3", "💾 Addım 3: Bazaya yaz", class = "btn-warning btn-lg",
                           style = "font-size:16px; margin-bottom:15px;"),
              uiOutput("step3_status"))
        )
      ),
      

      # ========== YEKUN STANDARTLAR ==========
      tabItem(tabName = "yekun",
        fluidRow(
          box(title = NULL, width = 12, background = "blue",
              tags$h3(style="margin:0;color:white;", "Yekun Standartlar"),
              tags$p(style="color:#e3f2fd;margin:5px 0 0 0;",
                "3 addımlı təhlildən sonra hər sinif üçün yekun standartlar burada göstərilir."))
        ),
        fluidRow(
          column(6,
            selectInput("yekun_sinif", "Sinif seçin:",
                        choices = setNames(1:11, paste0("Yekun_Standartlar-", 1:11)),
                        selected = 1, width = "100%")),
          column(3,
            selectInput("yekun_status_filter", "Status filtri:",
                        choices = c("Hamısı" = "all", "Dəyişməmiş" = "movcud",
                                    "Yenilənmiş" = "yenilenib",
                                    "Yeni əlavə" = "yeni",
                                    "Silinmiş" = "silinib"),
                        selected = "all", width = "100%")),
          column(3, style = "padding-top:25px;",
            downloadButton("download_yekun_csv", "CSV ixrac", class = "btn-success",
                           style = "margin-right:8px;"),
            downloadButton("download_yekun_excel", "Excel ixrac", class = "btn-primary"))
        ),
        fluidRow(
          valueBoxOutput("vb_yekun_umumi", width = 4),
          valueBoxOutput("vb_yekun_movcud", width = 3),
          valueBoxOutput("vb_yekun_yenilenib", width = 3),
          valueBoxOutput("vb_yekun_silinib", width = 2)
        ),
        fluidRow(
          box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
              DTOutput("dt_yekun"),
              tags$br(),
              uiOutput("yekun_info"))
        )
      ),

      # ========== YENİ TƏKLİFLƏR ==========
      tabItem(tabName = "yeni_teklifler_tab",
        fluidRow(
          box(title = NULL, width = 12, background = "orange",
              tags$h3(style="margin:0;color:white;", "AI Tərəfindən Təklif Olunan Yeni Standartlar"),
              tags$p(style="color:#fff3e0;margin:5px 0 0 0;",
                "Beynəlxalq təhlil nəticəsində AI-nin təklif etdiyi yeni standartlar. Qəbul etdikləriniz yekun standartlara əlavə olunacaq."))
        ),
        fluidRow(
          column(4,
            selectInput("teklif_sinif", "Sinif seçin:",
                        choices = setNames(1:11, paste0(1:11, "-ci sinif")),
                        selected = 1, width = "100%")),
          column(4,
            selectInput("teklif_status_filter", "Status filtri:",
                        choices = c("Hamısı" = "all", "Təklif" = "teklif",
                                    "Qəbul edilib" = "qebul", "Rədd edilib" = "redd"),
                        selected = "all", width = "100%")),
          column(4, style = "padding-top:25px;",
            actionButton("btn_teklif_qebul_sec", "Seçilmişləri Yekuna Əlavə Et",
                         class = "btn-success btn-lg", icon = icon("check"),
                         style = "font-weight:600;"))
        ),
        fluidRow(
          valueBoxOutput("vb_teklif_cemi", width = 3),
          valueBoxOutput("vb_teklif_gozleyen", width = 3),
          valueBoxOutput("vb_teklif_qebul", width = 3),
          valueBoxOutput("vb_teklif_redd", width = 3)
        ),
        fluidRow(
          box(title = NULL, width = 12, status = "warning", solidHeader = FALSE,
              DTOutput("dt_teklifler"),
              tags$br(),
              uiOutput("teklif_status_msg"))
        )
      ),

      # ========== EXPORT ==========
      tabItem(tabName = "export",
        fluidRow(
          box(title = "📥 HTML Hesabat Yüklə", width = 6, status = "primary", solidHeader = TRUE,
              tags$p("Seçilmiş sinif üçün nəfis HTML hesabatı yükləyin:"),
              downloadButton("download_html", "⬇️ HTML Yüklə", class = "btn-primary btn-lg")),
          box(title = "📊 CSV Yüklə", width = 6, status = "info", solidHeader = TRUE,
              tags$p("Bütün standartları CSV formatında yükləyin:"),
              downloadButton("download_csv", "⬇️ CSV Yüklə", class = "btn-info btn-lg"))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # --- Filtr düymələri ---
  observeEvent(input$filter_all, { updateSelectInput(session, "deyisiklik_sec", selected = "all") })
  observeEvent(input$filter_movcud, { updateSelectInput(session, "deyisiklik_sec", selected = "movcud") })
  observeEvent(input$filter_yenilenib, { updateSelectInput(session, "deyisiklik_sec", selected = "yenilenib") })
  observeEvent(input$filter_yeni, { updateSelectInput(session, "deyisiklik_sec", selected = "yeni") })
  observeEvent(input$filter_silinib, { updateSelectInput(session, "deyisiklik_sec", selected = "silinib") })

  # --- Reaktiv məlumat (PostgreSQL + CSV fallback) ---
  data_yenilenmi <- reactive({
    if (USE_CSV) {
      df <- csv_yenilenmi()
      if (nrow(df) == 0) return(df)
      df <- df[df$sinif == as.integer(input$sinif_sec), , drop=FALSE]
      if (input$mezmun_sec != "all") df <- df[df$mezmun_xetti == input$mezmun_sec, , drop=FALSE]
      if (input$deyisiklik_sec != "all") df <- df[df$deyisiklik_novu == input$deyisiklik_sec, , drop=FALSE]
      df <- df[order(df$mezmun_xetti, df$standart_kodu), ]
      return(df)
    }
    con <- get_con()
    on.exit(dbDisconnect(con))
    query <- sprintf("SELECT * FROM yenilenmi_standartlar WHERE sinif = %s", input$sinif_sec)
    if (input$mezmun_sec != "all") {
      query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", input$mezmun_sec))
    }
    if (input$deyisiklik_sec != "all") {
      query <- paste0(query, sprintf(" AND deyisiklik_novu = '%s'", input$deyisiklik_sec))
    }
    query <- paste0(query, " ORDER BY mezmun_xetti, standart_kodu")
    dbGetQuery(con, query)
  })

  data_movcud <- reactive({
    if (USE_CSV) {
      df <- csv_movcud()
      if (nrow(df) == 0) return(df)
      df <- df[df$sinif == as.integer(input$sinif_sec), , drop=FALSE]
      return(df[order(df$mezmun_xetti, df$standart_kodu), ])
    }
    con <- get_con()
    on.exit(dbDisconnect(con))
    query <- sprintf("SELECT * FROM movcud_standartlar WHERE sinif = %s ORDER BY mezmun_xetti, standart_kodu",
                     input$sinif_sec)
    dbGetQuery(con, query)
  })

  all_stats <- reactive({
    if (USE_CSV) {
      df_m <- csv_movcud()
      df_y <- csv_yenilenmi()
      if (nrow(df_m) == 0) return(data.frame(sinif=integer(), umumi=integer(), movcud=integer(),
                                              yenilenib=integer(), yeni=integer(), silinib=integer()))
      agg_m <- aggregate(id ~ sinif, data = df_m, FUN = length)
      names(agg_m) <- c("sinif", "movcud_say")
      df <- agg_m
      df$umumi <- df$movcud_say
      df$movcud <- df$movcud_say
      df$yenilenib <- 0L; df$yeni <- 0L; df$silinib <- 0L
      if (nrow(df_y) > 0) {
        for (s in unique(df_y$sinif)) {
          idx <- which(df$sinif == s)
          if (length(idx) > 0) {
            sub <- df_y[df_y$sinif == s, , drop=FALSE]
            df$yenilenib[idx] <- as.integer(sum(sub$deyisiklik_novu == "yenilenib", na.rm=TRUE))
            df$yeni[idx] <- as.integer(sum(sub$deyisiklik_novu == "yeni", na.rm=TRUE))
            df$silinib[idx] <- as.integer(sum(sub$deyisiklik_novu == "silinib", na.rm=TRUE))
            df$umumi[idx] <- df$movcud_say[idx] + df$yeni[idx]
          }
        }
      }
      return(df[, c("sinif", "umumi", "movcud", "yenilenib", "yeni", "silinib")])
    }

    con <- get_con()
    on.exit(dbDisconnect(con))

    # Mövcud standartlar (əsas say)
    df_m <- dbGetQuery(con, "
      SELECT sinif, COUNT(*) as movcud_say
      FROM movcud_standartlar GROUP BY sinif ORDER BY sinif")

    if (nrow(df_m) == 0) return(data.frame(sinif=integer(), umumi=integer(), movcud=integer(),
                                            yenilenib=integer(), yeni=integer(), silinib=integer()))

    # Yekun standartlardan statistika al (əsas mənbə)
    df_yek <- dbGetQuery(con, "
      SELECT sinif,
        COUNT(*) as umumi,
        COUNT(*) FILTER (WHERE status = 'movcud') as movcud,
        COUNT(*) FILTER (WHERE status = 'yenilenib') as yenilenib,
        COUNT(*) FILTER (WHERE status = 'yeni') as yeni,
        COUNT(*) FILTER (WHERE status = 'silinib') as silinib
      FROM yekun_standartlar GROUP BY sinif ORDER BY sinif")

    df <- df_m
    df$umumi <- df$movcud_say
    df$movcud <- df$movcud_say
    df$yenilenib <- 0L
    df$yeni <- 0L
    df$silinib <- 0L

    # Yekun varsa, həmin siniflərin dəyərlərini üstünə yaz
    if (nrow(df_yek) > 0) {
      for (i in 1:nrow(df_yek)) {
        idx <- which(df$sinif == df_yek$sinif[i])
        if (length(idx) > 0) {
          df$umumi[idx] <- as.integer(df_yek$umumi[i])
          df$movcud[idx] <- as.integer(df_yek$movcud[i])
          df$yenilenib[idx] <- as.integer(df_yek$yenilenib[i])
          df$yeni[idx] <- as.integer(df_yek$yeni[i])
          df$silinib[idx] <- as.integer(df_yek$silinib[i])
        }
      }
    }

    df[, c("sinif", "umumi", "movcud", "yenilenib", "yeni", "silinib")]
  })
  
  # --- CEFR ---
  output$cefr_display <- renderText({
    cefr_map[input$sinif_sec]
  })
  
  # --- VALUE BOXES ---
  output$vb_umumi <- renderValueBox({
    st <- all_stats()
    val <- if (nrow(st) > 0) sum(st$umumi) else 0
    valueBox(val, "Ümumi Standart", icon = icon("book"), color = "blue")
  })
  output$vb_movcud <- renderValueBox({
    st <- all_stats()
    val <- if (nrow(st) > 0) sum(st$movcud) else 0
    valueBox(val, "Mövcud", icon = icon("check"), color = "light-blue")
  })
  output$vb_yenilenib <- renderValueBox({
    st <- all_stats()
    val <- if (nrow(st) > 0) as.integer(sum(st$yenilenib)) else 0L
    valueBox(val, "Yenilənib", icon = icon("edit"), color = "green")
  })
  output$vb_yeni <- renderValueBox({
    st <- all_stats()
    val <- if (nrow(st) > 0) as.integer(sum(st$yeni)) else 0L
    valueBox(val, "Yeni Əlavə", icon = icon("plus"), color = "orange")
  })
  
  # --- QRAFİKLƏR ---
  output$chart_sinifler <- renderPlotly({
    st <- all_stats()
    if (nrow(st) == 0) return(plotly_empty(type = "bar"))
    
    plot_ly(st, x = ~paste0(sinif, "-ci sinif"), type = "bar") %>%
      add_bars(y = ~movcud, name = "Mövcud", marker = list(color = "#90CAF9")) %>%
      add_bars(y = ~yenilenib, name = "Yenilənib", marker = list(color = "#4CAF50")) %>%
      add_bars(y = ~yeni, name = "Yeni", marker = list(color = "#FF9800")) %>%
      layout(barmode = "stack", 
             xaxis = list(title = ""), yaxis = list(title = "Standart sayı"),
             legend = list(orientation = "h", y = -0.15),
             font = list(family = "Noto Sans"))
  })
  
  # --- Statistika üçün yekun datası (bütün siniflər) ---
  data_yekun_all <- reactive({
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
        if (!file.exists(f)) return(data.frame())
        read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbGetQuery(con, "SELECT * FROM yekun_standartlar ORDER BY sinif, mezmun_xetti, standart_kodu")
      }
    }, error = function(e) data.frame())
  })

  output$chart_deyisiklik <- renderPlotly({
    df <- data_yekun_all()
    if (nrow(df) == 0 || !"status" %in% names(df)) return(plotly_empty(type = "bar"))

    status_labels <- c("movcud" = "Dəyişməmiş", "yenilenib" = "Yenilənmiş", "yeni" = "Yeni əlavə", "silinib" = "Silinmiş")
    counts <- table(df$status)
    labels <- sapply(names(counts), function(s) if (s %in% names(status_labels)) status_labels[s] else s)
    colors <- c("movcud" = "#90CAF9", "yenilenib" = "#4CAF50", "yeni" = "#FF9800", "silinib" = "#F44336")

    plot_ly(labels = labels, values = as.numeric(counts), type = "pie",
            marker = list(colors = colors[names(counts)]),
            textinfo = "label+percent") %>%
      layout(font = list(family = "Noto Sans"))
  })

  output$chart_mezmun <- renderPlotly({
    df <- data_yekun_all()
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))

    counts <- as.data.frame(table(df$mezmun_xetti))
    plot_ly(counts, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#1976D2")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })

  output$chart_bloom <- renderPlotly({
    df <- data_yekun_all()
    if (nrow(df) == 0 || !"bloom_seviyyesi" %in% names(df)) return(plotly_empty(type = "bar"))
    df <- df[!is.na(df$bloom_seviyyesi) & nchar(df$bloom_seviyyesi) > 0, ]
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))

    bloom_order <- c("Xatırlama", "Anlama", "Tətbiq", "Analiz", "Qiymətləndirmə", "Yaratma")
    counts <- as.data.frame(table(factor(df$bloom_seviyyesi, levels = bloom_order)))

    bloom_colors <- c("#E3F2FD", "#BBDEFB", "#90CAF9", "#42A5F5", "#1976D2", "#0D47A1")
    plot_ly(counts, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = bloom_colors)) %>%
      layout(xaxis = list(title = "", categoryorder = "array", categoryarray = bloom_order),
             yaxis = list(title = "Say"), font = list(family = "Noto Sans"))
  })

  output$chart_cefr <- renderPlotly({
    df <- data_yekun_all()
    if (nrow(df) == 0 || !"cefr_seviyyesi" %in% names(df)) return(plotly_empty(type = "bar"))
    df <- df[!is.na(df$cefr_seviyyesi) & df$cefr_seviyyesi != "", ]
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))

    counts <- as.data.frame(table(df$cefr_seviyyesi), stringsAsFactors = FALSE)
    names(counts) <- c("cefr_seviyyesi", "say")

    plot_ly(counts, x = ~cefr_seviyyesi, y = ~say, type = "bar",
            marker = list(color = c("#BBDEFB","#90CAF9","#42A5F5","#1976D2","#0D47A1"))) %>%
      layout(xaxis = list(title = "CEFR"), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })

  output$chart_bloom_all <- renderPlotly({
    df <- data_yekun_all()
    if (nrow(df) == 0 || !"bloom_seviyyesi" %in% names(df)) return(plotly_empty(type = "bar"))
    df <- df[!is.na(df$bloom_seviyyesi) & df$bloom_seviyyesi != "", ]
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))

    counts <- as.data.frame(table(df$bloom_seviyyesi), stringsAsFactors = FALSE)
    names(counts) <- c("bloom_seviyyesi", "say")

    plot_ly(counts, labels = ~bloom_seviyyesi, values = ~say, type = "pie",
            textinfo = "label+percent") %>%
      layout(font = list(family = "Noto Sans"))
  })
  
  # --- CƏDVƏLLƏR ---
  
  # Standartlar pəncərəsi — şərti göstərmə
  output$ui_standartlar <- renderUI({
    df <- data_yenilenmi()
    if (nrow(df) == 0) {
      # "Hamisi" ve ya "Movcud" secilibse - movcud standartlari goster
      if (input$deyisiklik_sec %in% c("all", "movcud")) {
        return(DTOutput("dt_standartlar_movcud"))
      }
      filtr_adi <- switch(input$deyisiklik_sec,
        "yenilenib" = "yenilənmiş", "yeni" = "yeni", "silinib" = "silinmiş", "")
      return(div(class = "bos-mesaj",
        div(class = "icon", "📭"),
        tags$p(sprintf("%s standart yoxdur. Əvvəlcə 'Claude AI Təhlil' bölməsindən təhlil aparın.", filtr_adi))
      ))
    }
    DTOutput("dt_standartlar")
  })

  output$dt_standartlar_movcud <- renderDT({
    df <- data_movcud()
    if (nrow(df) == 0) return(NULL)
    show_cols <- intersect(c("standart_kodu", "standart_metni", "alt_standart_metni",
                              "mezmun_xetti", "bloom_seviyyesi"), names(df))
    df_show <- df[, show_cols, drop = FALSE]
    nice_names <- c("standart_kodu"="Kod", "standart_metni"="Standart",
                    "alt_standart_metni"="Alt standart", "mezmun_xetti"="Məzmun xətti",
                    "bloom_seviyyesi"="Bloom")
    names(df_show) <- nice_names[show_cols]
    datatable(df_show,
              options = list(pageLength = 20, scrollX = TRUE, autoWidth = FALSE),
              rownames = FALSE)
  })
  
  output$dt_standartlar <- renderDT({
    df <- data_yenilenmi()
    req(nrow(df) > 0)

    show_cols <- c("standart_kodu", "standart_metni", "alt_standart_metni",
                   "mezmun_xetti", "deyisiklik_novu", "bloom_seviyyesi", "cefr_seviyyesi",
                   "esaslandirma")
    show_cols <- intersect(show_cols, names(df))

    df_show <- df[, show_cols, drop=FALSE]
    names(df_show) <- c("Kod", "Standart", "Alt standart", "Məzmun xətti",
                         "Dəyişiklik", "Bloom", "CEFR", "Əsaslandırma")[seq_along(show_cols)]

    datatable(df_show,
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                autoWidth = FALSE,
                columnDefs = list(
                  list(width = '60px', targets = 0),
                  list(width = '250px', targets = 1),
                  list(width = '200px', targets = 2),
                  list(width = '90px', targets = 3),
                  list(width = '80px', targets = 4),
                  list(width = '80px', targets = 5),
                  list(width = '50px', targets = 6),
                  list(width = '200px', targets = 7)
                )
              ),
              rownames = FALSE) %>%
      formatStyle("Dəyişiklik",
                  backgroundColor = styleEqual(
                    c("movcud", "yenilenib", "yeni", "silinib"),
                    c("#E3F2FD", "#E8F5E9", "#FFF3E0", "#FFEBEE")))
  })
  
  # --- Müqayisə üçün yekun datası ---
  data_compare <- reactive({
    sinif <- as.integer(input$sinif_sec)
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
        if (!file.exists(f)) return(data.frame())
        df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        if (nrow(df) > 0) df <- df[df$sinif == sinif, , drop=FALSE]
        df
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbGetQuery(con, sprintf(
          "SELECT * FROM yekun_standartlar WHERE sinif = %d ORDER BY
           CASE status WHEN 'movcud' THEN 1 WHEN 'yenilenib' THEN 1 WHEN 'silinib' THEN 1 WHEN 'yeni' THEN 2 ELSE 3 END,
           mezmun_xetti, standart_kodu", sinif))
      }
    }, error = function(e) data.frame())
  })

  # --- Diff highlight funksiyası ---
  highlight_diff <- function(old_text, new_text) {
    if (is.na(old_text) || is.na(new_text) || nchar(old_text) == 0) return(new_text)
    old_words <- strsplit(old_text, "\\s+")[[1]]
    new_words <- strsplit(new_text, "\\s+")[[1]]
    result <- c()
    for (i in seq_along(new_words)) {
      w <- new_words[i]
      if (i <= length(old_words) && w == old_words[i]) {
        result <- c(result, w)
      } else {
        result <- c(result, sprintf('<span style="background:#C8E6C9;font-weight:600;padding:1px 3px;border-radius:3px;">%s</span>', w))
      }
    }
    paste(result, collapse = " ")
  }

  # Müqayisə — tək panel, hər standart bir sətir: sol köhnə, sağ yeni
  output$ui_compare_table <- renderUI({
    df <- data_compare()
    if (nrow(df) == 0) {
      return(tags$div(style="padding:40px;text-align:center;color:#9E9E9E;font-size:18px;",
        "Bu sinif üçün yekun standart yoxdur. Əvvəlcə AI Təhlil bölməsindən 3 addımı tamamlayın."))
    }

    cards <- lapply(1:nrow(df), function(i) {
      row <- df[i, ]
      kod <- if (!is.na(row$standart_kodu)) row$standart_kodu else ""
      alt_kod <- if (!is.na(row$alt_standart_kodu)) row$alt_standart_kodu else ""
      mez <- if (!is.na(row$mezmun_xetti)) row$mezmun_xetti else ""
      st <- if ("status" %in% names(row) && !is.na(row$status)) row$status else "movcud"

      kohne_metn <- if ("standart_metni" %in% names(row) && !is.na(row$standart_metni)) row$standart_metni else ""
      kohne_alt <- if ("alt_standart_metni" %in% names(row) && !is.na(row$alt_standart_metni)) row$alt_standart_metni else ""
      yeni_metn <- if ("yeni_standart_metni" %in% names(row) && !is.na(row$yeni_standart_metni)) row$yeni_standart_metni else ""
      yeni_alt <- if ("yeni_alt_standart_metni" %in% names(row) && !is.na(row$yeni_alt_standart_metni)) row$yeni_alt_standart_metni else ""

      # Status badge
      badge <- switch(st,
        "movcud" = '<span style="background:#90CAF9;color:#fff;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:600;">Dəyişməmiş</span>',
        "yenilenib" = '<span style="background:#4CAF50;color:#fff;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:600;">Yenilənib</span>',
        "silinib" = '<span style="background:#F44336;color:#fff;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:600;">Silinib</span>',
        "yeni" = '<span style="background:#FF9800;color:#fff;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:600;">Yeni əlavə</span>',
        "")

      # Fon rəngi
      bg <- switch(st, "movcud"="#FFFFFF", "yenilenib"="#FFF8E1", "silinib"="#FFEBEE", "yeni"="#E8F5E9", "#FFFFFF")
      border_color <- switch(st, "movcud"="#E0E0E0", "yenilenib"="#FF9800", "silinib"="#F44336", "yeni"="#4CAF50", "#E0E0E0")

      if (st == "movcud") {
        # Dəyişməmiş — kompakt, bir sətir
        tags$div(style=sprintf("display:flex;align-items:flex-start;padding:8px 12px;border-bottom:1px solid #EEEEEE;background:%s;", bg),
          tags$div(style="width:70px;flex-shrink:0;font-weight:600;color:#1976D2;font-size:13px;padding-top:2px;", alt_kod),
          tags$div(style="width:90px;flex-shrink:0;", tags$span(style="background:#E3F2FD;color:#1565C0;padding:2px 6px;border-radius:4px;font-size:11px;", mez)),
          tags$div(style="flex:1;color:#757575;font-size:13px;", kohne_alt),
          tags$div(style="width:90px;flex-shrink:0;text-align:right;", HTML(badge))
        )
      } else if (st == "yenilenib") {
        # Yenilənib — köhnə və yeni yan-yana
        yeni_alt_html <- yeni_alt
        if (nchar(kohne_alt) > 0 && nchar(yeni_alt) > 0 && kohne_alt != yeni_alt) {
          yeni_alt_html <- highlight_diff(kohne_alt, yeni_alt)
        }
        tags$div(style=sprintf("padding:10px 12px;border:2px solid %s;border-radius:8px;margin:6px 0;background:%s;", border_color, bg),
          tags$div(style="display:flex;align-items:center;margin-bottom:8px;",
            tags$span(style="font-weight:700;color:#1976D2;margin-right:10px;", alt_kod),
            tags$span(style="background:#E3F2FD;color:#1565C0;padding:2px 6px;border-radius:4px;font-size:11px;margin-right:10px;", mez),
            HTML(badge)
          ),
          tags$div(style="display:flex;gap:16px;",
            tags$div(style="flex:1;padding:8px 12px;background:#FFF;border-radius:6px;border:1px solid #E0E0E0;",
              tags$div(style="font-size:11px;color:#E65100;font-weight:600;margin-bottom:4px;", "KÖHNƏ:"),
              tags$p(style="margin:0;color:#616161;font-size:13px;", kohne_alt)
            ),
            tags$div(style="flex:0 0 24px;display:flex;align-items:center;justify-content:center;color:#4CAF50;font-size:20px;font-weight:bold;", HTML("&rarr;")),
            tags$div(style="flex:1;padding:8px 12px;background:#E8F5E9;border-radius:6px;border:1px solid #A5D6A7;",
              tags$div(style="font-size:11px;color:#2E7D32;font-weight:600;margin-bottom:4px;", "YENİ:"),
              tags$p(style="margin:0;font-size:13px;", HTML(yeni_alt_html))
            )
          )
        )
      } else if (st == "silinib") {
        tags$div(style=sprintf("padding:10px 12px;border:2px solid %s;border-radius:8px;margin:6px 0;background:%s;", border_color, bg),
          tags$div(style="display:flex;align-items:center;",
            tags$span(style="font-weight:700;color:#1976D2;margin-right:10px;", alt_kod),
            tags$span(style="background:#FFCDD2;color:#C62828;padding:2px 6px;border-radius:4px;font-size:11px;margin-right:10px;", mez),
            HTML(badge)
          ),
          tags$p(style="margin:6px 0 0 0;color:#C62828;text-decoration:line-through;font-size:13px;", kohne_alt)
        )
      } else if (st == "yeni") {
        tags$div(style=sprintf("padding:10px 12px;border:2px solid %s;border-radius:8px;margin:6px 0;background:%s;", border_color, bg),
          tags$div(style="display:flex;align-items:center;margin-bottom:4px;",
            tags$span(style="font-weight:700;color:#1976D2;margin-right:10px;", alt_kod),
            tags$span(style="background:#C8E6C9;color:#2E7D32;padding:2px 6px;border-radius:4px;font-size:11px;margin-right:10px;", mez),
            HTML(badge)
          ),
          tags$p(style="margin:0;color:#2E7D32;font-weight:600;font-size:13px;", yeni_metn),
          if (nchar(yeni_alt) > 0) tags$p(style="margin:4px 0 0 10px;font-size:13px;color:#388E3C;", tags$em(yeni_alt))
        )
      }
    })
    do.call(tagList, cards)
  })
  
  output$dt_beynelxalq <- renderDT({
    tryCatch({
      if (USE_CSV) return(NULL)
      con <- get_con(); on.exit(dbDisconnect(con))
      sinif <- as.integer(input$sinif_sec)
      df <- dbGetQuery(con, sprintf("
        SELECT cerceve_adi, kateqoriya, alt_kateqoriya, tesvir
        FROM beynelxalq_cerceveler
        WHERE %d BETWEEN SPLIT_PART(sinif_araligi, '-', 1)::int
                    AND SPLIT_PART(sinif_araligi, '-', 2)::int
        ORDER BY cerceve_adi", sinif))
      if (nrow(df) == 0) return(NULL)
      datatable(df, options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE,
                  columnDefs = list(
                    list(width = '80px', targets = 0),
                    list(width = '120px', targets = 1),
                    list(width = '180px', targets = 2),
                    list(width = '250px', targets = 3)
                  )),
                rownames = FALSE,
                colnames = c("Çərçəvə", "Kateqoriya", "Alt kateqoriya", "Təsvir"))
    }, error = function(e) NULL)
  })

  output$dt_olkeler <- renderDT({
    tryCatch({
      if (USE_CSV) return(NULL)
      con <- get_con(); on.exit(dbDisconnect(con))
      query <- "SELECT olke, mezmun_xetti, standart_metni, xususi_yanaşma FROM olke_standartlari"
      if (input$olke_sec != "Hamısı") {
        query <- paste0(query, sprintf(" WHERE olke = '%s'", input$olke_sec))
      }
      df <- dbGetQuery(con, query)
      if (nrow(df) == 0) return(NULL)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE,
                  columnDefs = list(
                    list(width = '90px', targets = 0),
                    list(width = '90px', targets = 1),
                    list(width = '250px', targets = 2),
                    list(width = '200px', targets = 3)
                  )),
                rownames = FALSE,
                colnames = c("Ölkə", "Məzmun", "Standart", "Xüsusi yanaşma"))
    }, error = function(e) NULL)
  })
  
  # ============================================================
  # ADDIM 1: TƏHLİL — JSON ilə hər standarta status ver + yeni təkliflər
  # ============================================================
  observeEvent(input$btn_step1, {
    sinif <- input$sinif_sec
    mezmun <- input$mezmun_sec

    if (USE_CSV) {
      df <- csv_movcud()
      if (nrow(df) > 0) {
        df <- df[df$sinif == as.integer(sinif), , drop=FALSE]
        if (mezmun != "all") df <- df[df$mezmun_xetti == mezmun, , drop=FALSE]
        df <- df[order(df$mezmun_xetti, df$standart_kodu), ]
      }
    } else {
      con <- get_con(); on.exit(dbDisconnect(con))
      query <- sprintf("SELECT * FROM movcud_standartlar WHERE sinif = %s", sinif)
      if (mezmun != "all") query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", mezmun))
      query <- paste0(query, " ORDER BY mezmun_xetti, standart_kodu")
      df <- dbGetQuery(con, query)
    }

    if (nrow(df) == 0) {
      output$ai_result <- renderUI(tags$div(style="padding:30px;color:#dc2626;",
        tags$h3("Məlumat yoxdur"), tags$p("Bu sinif/məzmun xətti üçün standart tapılmadı.")))
      return()
    }

    mezmun_label <- if (mezmun == "all") "bütün məzmun xətləri" else mezmun
    cefr_sev <- cefr_map[as.character(sinif)]
    std_cols <- intersect(c("standart_kodu","standart_metni","alt_standart_kodu","alt_standart_metni",
                            "mezmun_xetti","bloom_seviyyesi"), names(df))

    session$sendCustomMessage("ai_timer_start", list(
      target = "ai_timer_panel",
      status = "Addım 1: Claude AI təhlil edir...",
      info1 = paste0(sinif, "-ci sinif"),
      info2 = paste0(nrow(df), " standart (", mezmun_label, ")")
    ))
    output$ai_token_display <- renderUI(tags$div(class = "token-display token-waiting", "⏳ Addım 1 icra olunur..."))
    output$ai_result <- renderUI(NULL)

    std_json <- toJSON(df[, std_cols, drop=FALSE], pretty = TRUE, auto_unbox = TRUE)
    prompt <- paste0(
'Sən Azərbaycan dili kurikulumu üzrə beynəlxalq ekspertisən. Sənin vəzifən ', sinif, '-ci sinif, "', mezmun_label, '" üçün mövcud standartları dərindən təhlil etmək və BOŞLUQLARI müəyyən edib yeni standartlar təklif etməkdir.

Bu sinif üçün CEFR səviyyəsi: ', cefr_sev, '

═══════════════════════════════════════════
MÖVCud STANDARTLAR:
═══════════════════════════════════════════
', std_json, '

═══════════════════════════════════════════
TƏHLİL METODOLOGİYASI (hər standart üçün tətbiq et):
═══════════════════════════════════════════

1. PISA OXU SAVADLIlIĞI ÇƏRÇƏVƏSİ ilə müqayisə:
   - Məlumat əldə etmə və axtarış (Accessing and retrieving)
   - İnteqrasiya və interpretasiya (Integrating and interpreting)
   - Əks etdirmə və qiymətləndirmə (Reflecting and evaluating)
   - PISA 2025 yenilikləri: rəqəmsal oxu, çoxsaylı mənbələr, kritik qiymətləndirmə
   → Hansı PISA bacarıqları mövcud standartlarda ƏKSİK qalır?

2. PIRLS OXU BACARIQ ÇƏRÇƏVƏSİ ilə müqayisə:
   - Aydın ifadə olunmuş məlumatın tapılması
   - Birbaşa nəticə çıxarma
   - İnterpretasiya və ideyaların inteqrasiyası
   - Məzmun və formanın qiymətləndrilməsi
   - PIRLS 2026: rəqəmsal mətnlər, onlayn oxu strategiyaları
   → Hansı PIRLS tələbləri mövcud standartlarda yoxdur?

3. BLOOM TAKSONOMİYASI balansı:
   - Xatırlama → Anlama → Tətbiq → Analiz → Qiymətləndirmə → Yaratma
   → Mövcud standartlarda Bloom balansı necədir? Yuxarı səviyyələr (Analiz, Qiymətləndirmə, Yaratma) kifayət qədərmi?

4. 6 APARICI ÖLKƏ ilə müqayisə — HƏR BİRİ üçün BOŞLUQ TƏHLİLİ:

   a) FİNLANDİYA (Finnish mother tongue):
      - Fənlərarası oxu, çoxsaylı mətn növləri, media savadlılığı
      - Çoxmodal mətn anlama (şəkil+mətn+qrafik)
      → Finlandiya standartlarında olub bizdə OLMAYAN nədir?

   b) SİNQAPUR (STELLAR proqramı):
      - Kommunikativ yanaşma, kritik düşüncə, real həyat konteksti
      - Sistemli dil öyrənmə strategiyaları, özünüqiymətləndirmə
      → Sinqapur modelindən nə öyrənə bilərik?

   c) ESTONİYA (Eston dili kurikulumu):
      - Rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya
      - İnformasiya mühitində naviqasiya
      → Estoniya yanaşmasında hansı güclü tərəflər var?

   d) YAPONİYA (Kokugo):
      - Dərin oxu, estetik duyum, yaradıcı ifadə
      - Mətn strukturunu anlamaq, yazıçı niyyətini müəyyən etmək
      → Yaponiya standartlarından hansı elementlər əksikdir?

   e) KANADA - ONTARİO (Language Arts):
      - Diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər
      - Əməkdaşlıq yolu ilə öyrənmə, tənqidi savadlılıq
      → Ontario modelindən hansı boşluqlar görünür?

   f) İRLANDİYA (Gaeilge/English):
      - İkidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma
      - Multimodal mətnlərlə işləmə, dinləmə strategiyaları
      → İrlandiya təcrübəsindən nə götürülməlidir?

5. MÜASİR TƏHSİL TENDENSİYALARI — BOŞLUQ TƏHLİLİ:
   - Rəqəmsal savadlılıq (digital literacy): onlayn mətnləri oxumaq, rəqəmsal mühitdə naviqasiya
   - Media savadlılığı: reklam, xəbər, sosial media mətnlərini kritik qiymətləndirmə
   - İnteqrativ təhsil: fənlərarası əlaqə, real həyat tətbiqi
   - Kritik düşüncə: mənbə etibarlılığı, fakt vs fikir ayrımı, arqumentasiya
   - Çoxsaylı mənbələrdən məlumat sintezi
   - Əməkdaşlıq və kommunikasiya bacarıqları
   - Özünüidarəetmə və metakognitiv strategiyalar
   → Bu sahələrdən hansıları mövcud standartlarda ƏKSİKDİR?

═══════════════════════════════════════════
YENİ STANDART TƏKLİFLƏRİ — MÜTLƏQDİR!
═══════════════════════════════════════════
Yuxarıdakı təhlilin nəticəsində müəyyən etdiyin BOŞLUQLARA əsasən,
HƏR MƏZMUN XƏTTİ üçün ən azı 1-2 YENİ standart təklif et (cəmi minimum 3-4 yeni standart).

Hər yeni standart üçün GENIŞ ƏSASLANDIRMA yaz:
- Bu standart HANSI BOŞLUĞU doldurur?
- HANSl ÖLKƏ/ÖLKƏLƏR-in standartlarından ilhamlanıb?
- PISA/PIRLS/CEFR-in HANSl TƏLƏBİNƏ cavab verir?
- BLOOM-un hansı səviyyəsini gücləndirir?
- Bu yaş qrupu üçün NİYƏ vacibdir?
- Praktiki TƏTBİQ nümunəsi (1-2 cümlə)

Yeni standart kodları mövcud kodlardan SONRA gəlməlidir.
Məsələn: mövcud sonuncu 1.1.4 isə, yeni: 1.1.5, 1.1.6...

═══════════════════════════════════════════
CAVAB FORMATI — YALNIZ JSON
═══════════════════════════════════════════
{
  "umumi_qiymetlendirme": "Mövcud standartların ümumi güclü və zəif tərəfləri. PISA/PIRLS/CEFR/Bloom balansı. Əsas boşluqların xülasəsi. 6 ölkə ilə müqayisənin nəticəsi (5-8 cümlə, ətraflı).",
  "beynelxalq_muqayise": {
    "pisa_bosluqlari": "PISA çərçəvəsində aşkar edilən boşluqlar",
    "pirls_bosluqlari": "PIRLS çərçəvəsində aşkar edilən boşluqlar",
    "cefr_uygunlugu": "CEFR ', cefr_sev, ' səviyyəsinə uyğunluq qiymətləndirməsi",
    "bloom_balansi": "Bloom taksonomiyası üzrə balans təhlili",
    "olke_muqayisesi": "6 ölkə ilə müqayisənin xülasəsi — hər ölkədən əsas fərq"
  },
  "standartlar": [
    {
      "standart_kodu": "1.1.1",
      "alt_standart_kodu": "1.1.1.1",
      "standart_metni": "Standart mətni",
      "alt_standart_metni": "Alt standart mətni",
      "mezmun_xetti": "Dinləmə və danışma",
      "bloom_seviyyesi": "Anlama",
      "status": "qalsin",
      "esaslandirma": "Ətraflı əsaslandırma — hansı beynəlxalq çərçəvəyə uyğundur, hansı ölkələrdə oxşar standart var (3-5 cümlə)"
    }
  ],
  "yeni_teklifler": [
    {
      "standart_kodu": "1.1.5",
      "alt_standart_kodu": "1.1.5.1",
      "standart_metni": "Yeni standart mətni",
      "alt_standart_metni": "Yeni alt standart mətni",
      "mezmun_xetti": "Oxu",
      "bloom_seviyyesi": "Analiz",
      "cetinlik": "orta",
      "esaslandirma": "GENİŞ ƏSASLANDIRMA: 1) Hansı boşluğu doldurur 2) Hansı ölkə/ölkələrin standartlarından ilhamlanıb 3) PISA/PIRLS/CEFR-in hansı tələbinə cavab verir 4) Bloom-un hansı səviyyəsini gücləndirir 5) Bu yaş qrupu üçün niyə vacibdir 6) Praktiki tətbiq nümunəsi (minimum 5-6 cümlə)"
    }
  ]
}

status dəyərləri: "qalsin", "yenilensin", "silinsin"
cetinlik dəyərləri: "asan", "orta", "cetin"
Bloom: Xatırlama, Anlama, Tətbiq, Analiz, Qiymətləndirmə, Yaratma
Məzmun xətləri: Dinləmə və danışma, Oxu, Yazı, Dil qaydaları

KRİTİK QAYDALAR:
1. "yeni_teklifler" massivi BOŞ OLMAMALIDIR! Minimum 3-4 yeni standart təklif et.
2. Hər yeni standart üçün minimum 5-6 cümlə əsaslandırma yaz.
3. Yeni standartlar rəqəmsal savadlılıq, media savadlılığı, kritik düşüncə, çoxsaylı mənbələrdən sintez, inteqrativ bacarıqlar kimi müasir tələbləri əhatə etməlidir.
4. Əsaslandırmada KONKRET ölkə adı və çərçəvə adı göstər.
5. YALNIZ JSON formatında cavab ver, başqa heç nə yazma.

═══════════════════════════════════════════
HƏR STANDART ÜÇÜN KONKRET YOXLAMA KRİTERİYALARI:
═══════════════════════════════════════════
Hər standartı aşağıdakı konkret kriteriyalara görə yoxla. Əgər hər hansı kriteriya pozulursa, status "yenilensin" olmalıdır:

A) BLOOM TAKSONOMİYASI YOXLAMASI:
   - Bu sinif səviyyəsi üçün Bloom balansı düzgündürmü?
   - 1-2-ci siniflər: əsasən Xatırlama, Anlama, az Tətbiq olmalı. Amma YALNIZ "adlandırır", "sadalayır" kimi passiv fellər varsa → "yenilensin" (aktiv fell əlavə et: "fərqləndirir", "qruplaşdırır")
   - 3-4-cü siniflər: Anlama, Tətbiq ağırlıqlı, az Analiz olmalı
   - 5-7-ci siniflər: Tətbiq, Analiz ağırlıqlı olmalı
   - 8-9-cu siniflər: Analiz, Qiymətləndirmə ağırlıqlı olmalı
   - 10-11-ci siniflər: Qiymətləndirmə, Yaratma ağırlıqlı olmalı
   - Əgər standartın feli Bloom səviyyəsinə uyğun gəlmirsə → "yenilensin"

B) CEFR UYĞUNLUQ YOXLAMASI:
   - A1 (1-2 sinif): sadə cümlələr, tanış mövzular, əsas ehtiyaclar
   - A2 (3-4 sinif): gündəlik ifadələr, sadə təsvirlər, qısa mətnlər
   - B1 (5-7 sinif): əsas fikirləri anlama, şəxsi fikir bildirmə, əlaqəli mətn yaratma
   - B2 (8-9 sinif): mürəkkəb mətnlərin əsas fikirlərini anlama, səlis ünsiyyət, ətraflı mətn
   - C1 (10-11 sinif): uzun mürəkkəb mətnlər, akademik dil, dəqiq ifadə
   - Standart öz CEFR səviyyəsinə uyğun deyilsə (çox sadə və ya çox çətindirsə) → "yenilensin"

C) PISA/PIRLS BOŞLUQLARI:
   - Rəqəmsal mətnlərlə iş — əgər standartda yoxdursa və bu sinif üçün lazımdırsa (3+ sinif) → "yenilensin"
   - Çoxsaylı mənbələrdən məlumat sintezi — əgər yoxdursa (5+ sinif) → "yenilensin"
   - Mənbənin etibarlılığını qiymətləndirmə — əgər yoxdursa (7+ sinif) → "yenilensin"
   - Fakt və fikir ayrımı — əgər yoxdursa (5+ sinif) → "yenilensin"
   - Müəllifin məqsədini/mövqeyini müəyyənləşdirmə — yoxdursa (4+ sinif) → "yenilensin"

D) APARICI ÖLKƏ MÜQAYİSƏSİ BOŞLUQLARI:
   - Finlandiya: media savadlılığı, çoxmodal mətnlər (şəkil+mətn) — yoxdursa yenilə
   - Sinqapur: özünüqiymətləndirmə, real həyat konteksti — yoxdursa yenilə
   - Estoniya: rəqəmsal mühitdə naviqasiya — yoxdursa yenilə
   - Yaponiya: mətn strukturunu anlamaq, yazıçı niyyəti — yoxdursa yenilə
   - Kanada: inklüziv yanaşma, əməkdaşlıq ilə öyrənmə — yoxdursa yenilə
   - İrlandiya: dinləmə strategiyaları, multimodal mətnlər — yoxdursa yenilə

E) MÜASİR TƏLƏBLƏRİN YOXLAMASI:
   - Standartda yalnız passiv bacarıq varsa (dinləyir, oxuyur) və aktiv/yaradıcı komponent yoxdursa → "yenilensin"
   - Standart çox ümumidirsə (dəqiq ölçülə bilən nəticə yoxdursa) → "yenilensin"
   - Standart yalnız bilik səviyyəsindədirsə və tətbiq komponenti yoxdursa → "yenilensin"

DİQQƏT: "yenilensin" statusu verdikdə standartın yeni mətni FƏRQLI olmalıdır — köhnə ilə eyni yazmaq QADAĞANDIR! Yeni mətn konkret olaraq hansı boşluğu doldurduğunu əks etdirməlidir.')

    session$onFlushed(function() {
      res <- call_claude(prompt)

      if (res$success) {
        tryCatch({
          # JSON parse
          json_text <- res$text
          json_text <- gsub("```json\\s*", "", json_text)
          json_text <- gsub("```\\s*$", "", json_text)
          json_text <- gsub("```", "", json_text)
          # { ... } arasını tap
          m <- regmatches(json_text, regexpr("\\{[\\s\\S]*\\}", json_text, perl = TRUE))
          if (length(m) == 0) stop("JSON tapılmadı")
          parsed <- fromJSON(m[1], simplifyDataFrame = TRUE)

          # Nəticəni saxla (onFlushed callback-dan isolate lazımdır)
          isolate(step1_results(parsed))

          # --- Gözəl HTML render ---
          umumi <- if (!is.null(parsed$umumi_qiymetlendirme)) parsed$umumi_qiymetlendirme else ""
          std_df <- parsed$standartlar
          yeni_df <- parsed$yeni_teklifler

          # Rəng xəritəsi
          status_renk <- c("qalsin" = "#4CAF50", "yenilensin" = "#FF9800", "silinsin" = "#F44336")
          status_adi <- c("qalsin" = "QALSIN", "yenilensin" = "YENİLƏNSİN", "silinsin" = "SİLİNSİN")

          # Standart kartları
          cards_html <- ""
          if (is.data.frame(std_df) && nrow(std_df) > 0) {
            n_q <- sum(std_df$status == "qalsin", na.rm=TRUE)
            n_y <- sum(std_df$status == "yenilensin", na.rm=TRUE)
            n_s <- sum(std_df$status == "silinsin", na.rm=TRUE)

            cards_html <- paste0(
              '<div style="display:flex;gap:12px;margin:16px 0;flex-wrap:wrap;">',
              '<div style="padding:10px 20px;background:#E8F5E9;border-radius:8px;font-weight:600;color:#2E7D32;">QALSIN: ', n_q, '</div>',
              '<div style="padding:10px 20px;background:#FFF3E0;border-radius:8px;font-weight:600;color:#E65100;">YENİLƏNSİN: ', n_y, '</div>',
              '<div style="padding:10px 20px;background:#FFEBEE;border-radius:8px;font-weight:600;color:#C62828;">SİLİNSİN: ', n_s, '</div>',
              '</div>')

            for (i in 1:nrow(std_df)) {
              r <- std_df[i, ]
              st <- if (!is.null(r$status)) r$status else "qalsin"
              renk <- if (st %in% names(status_renk)) status_renk[[st]] else "#1976D2"
              ad <- if (st %in% names(status_adi)) status_adi[[st]] else st
              kod <- if (!is.null(r$standart_kodu)) r$standart_kodu else ""
              metn <- if (!is.null(r$standart_metni)) r$standart_metni else ""
              esas <- if (!is.null(r$esaslandirma)) r$esaslandirma else ""
              bloom <- if (!is.null(r$bloom_seviyyesi)) r$bloom_seviyyesi else ""

              cards_html <- paste0(cards_html,
                '<div style="border:2px solid ', renk, ';border-radius:12px;padding:16px;margin:10px 0;background:#FAFAFA;">',
                '<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;">',
                '<span style="font-weight:700;color:#0D47A1;font-size:16px;">', kod, '</span>',
                '<div><span style="padding:3px 10px;border-radius:12px;font-size:12px;background:#E3F2FD;color:#1565C0;margin-right:6px;">', bloom, '</span>',
                '<span style="padding:4px 14px;border-radius:20px;font-weight:600;font-size:13px;background:', renk, ';color:white;">', ad, '</span></div>',
                '</div>',
                '<div style="font-size:15px;color:#333;margin-bottom:8px;">', metn, '</div>',
                '<div style="font-size:14px;color:#555;padding:10px;background:#F5F5F5;border-radius:8px;border-left:4px solid ', renk, ';">',
                '<b>Əsaslandırma:</b> ', esas, '</div>',
                '</div>')
            }
          }

          # Yeni təkliflər
          yeni_html <- ""
          if (is.data.frame(yeni_df) && nrow(yeni_df) > 0) {
            yeni_html <- paste0('<h2 style="color:#1565C0;margin-top:28px;border-bottom:2px solid #42A5F5;padding-bottom:8px;">',
                                'Yeni Standart Təklifləri (', nrow(yeni_df), ' ədəd)</h2>')
            for (i in 1:nrow(yeni_df)) {
              r <- yeni_df[i, ]
              kod <- if (!is.null(r$standart_kodu)) r$standart_kodu else ""
              metn <- if (!is.null(r$standart_metni)) r$standart_metni else ""
              esas <- if (!is.null(r$esaslandirma)) r$esaslandirma else ""
              bloom <- if (!is.null(r$bloom_seviyyesi)) r$bloom_seviyyesi else ""
              mez <- if (!is.null(r$mezmun_xetti)) r$mezmun_xetti else ""
              cet <- if (!is.null(r$cetinlik)) r$cetinlik else ""

              yeni_html <- paste0(yeni_html,
                '<div style="border:2px solid #2196F3;border-radius:12px;padding:16px;margin:10px 0;background:#E3F2FD;">',
                '<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;">',
                '<span style="font-weight:700;color:#0D47A1;font-size:16px;">YENİ: ', kod, '</span>',
                '<div>',
                '<span style="padding:3px 10px;border-radius:12px;font-size:12px;background:#BBDEFB;color:#0D47A1;margin-right:4px;">', mez, '</span>',
                '<span style="padding:3px 10px;border-radius:12px;font-size:12px;background:#BBDEFB;color:#0D47A1;margin-right:4px;">', bloom, '</span>',
                '<span style="padding:3px 10px;border-radius:12px;font-size:12px;background:#BBDEFB;color:#0D47A1;margin-right:4px;">', cet, '</span>',
                '<span style="padding:4px 14px;border-radius:20px;font-weight:600;font-size:13px;background:#2196F3;color:white;">YENİ</span>',
                '</div></div>',
                '<div style="font-size:15px;color:#333;margin-bottom:8px;">', metn, '</div>',
                '<div style="font-size:14px;color:#555;padding:10px;background:#F5F5F5;border-radius:8px;border-left:4px solid #2196F3;">',
                '<b>Əsaslandırma:</b> ', esas, '</div>',
                '</div>')
            }
          }

          # Beynəlxalq müqayisə paneli
          muqayise_html <- ""
          bm <- parsed$beynelxalq_muqayise
          if (!is.null(bm) && is.list(bm)) {
            muqayise_html <- paste0(
              '<div style="margin:20px 0;padding:20px;background:#EDE7F6;border-radius:12px;border-left:4px solid #7E57C2;">',
              '<h3 style="color:#4527A0;margin-top:0;">Beynəlxalq Müqayisə Təhlili</h3>',
              if (!is.null(bm$pisa_bosluqlari)) paste0('<p><b>PISA boşluqları:</b> ', bm$pisa_bosluqlari, '</p>') else '',
              if (!is.null(bm$pirls_bosluqlari)) paste0('<p><b>PIRLS boşluqları:</b> ', bm$pirls_bosluqlari, '</p>') else '',
              if (!is.null(bm$cefr_uygunlugu)) paste0('<p><b>CEFR uyğunluğu:</b> ', bm$cefr_uygunlugu, '</p>') else '',
              if (!is.null(bm$bloom_balansi)) paste0('<p><b>Bloom balansı:</b> ', bm$bloom_balansi, '</p>') else '',
              if (!is.null(bm$olke_muqayisesi)) paste0('<p><b>6 ölkə müqayisəsi:</b> ', bm$olke_muqayisesi, '</p>') else '',
              '</div>')
          }

          full_html <- paste0(
            '<div style="font-size:16px;line-height:1.7;padding:16px;background:#F8F9FA;border-radius:10px;border-left:4px solid #1976D2;margin-bottom:20px;">',
            umumi, '</div>', muqayise_html, cards_html, yeni_html)

          # Yeni təklifləri ayrıca cədvələ yaz (yeni_elave_teklifleri)
          if (is.data.frame(yeni_df) && nrow(yeni_df) > 0) {
            tryCatch({
              if (USE_CSV) {
                csv_teklif_path <- file.path(getwd(), "data", "yeni_elave_teklifleri_backup.csv")
                df_old_t <- if (file.exists(csv_teklif_path)) {
                  read.csv(csv_teklif_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
                } else data.frame()
                # Eyni sinif + mezmun üçün köhnə təklifləri sil
                if (nrow(df_old_t) > 0) {
                  if (mezmun != "all") {
                    df_old_t <- df_old_t[!(df_old_t$sinif == as.integer(sinif) & df_old_t$mezmun_xetti %in% yeni_df$mezmun_xetti), , drop=FALSE]
                  } else {
                    df_old_t <- df_old_t[df_old_t$sinif != as.integer(sinif), , drop=FALSE]
                  }
                }
                for (i in 1:nrow(yeni_df)) {
                  r <- yeni_df[i, ]
                  new_t <- data.frame(
                    id = if (nrow(df_old_t) > 0) max(df_old_t$id, na.rm=TRUE) + i else i,
                    sinif = as.integer(sinif),
                    mezmun_xetti = if (!is.null(r$mezmun_xetti)) as.character(r$mezmun_xetti) else "",
                    standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
                    standart_metni = if (!is.null(r$standart_metni)) as.character(r$standart_metni) else "",
                    alt_standart_kodu = if (!is.null(r$alt_standart_kodu)) as.character(r$alt_standart_kodu) else "",
                    alt_standart_metni = if (!is.null(r$alt_standart_metni)) as.character(r$alt_standart_metni) else "",
                    bloom_seviyyesi = if (!is.null(r$bloom_seviyyesi)) as.character(r$bloom_seviyyesi) else "",
                    cetinlik = if (!is.null(r$cetinlik)) as.character(r$cetinlik) else "",
                    cefr_seviyyesi = cefr_sev,
                    esaslandirma = if (!is.null(r$esaslandirma)) as.character(r$esaslandirma) else "",
                    pisa_elaqesi = "", pirls_elaqesi = "", olke_istinad = "",
                    status = "teklif",
                    yaradilma_tarixi = as.character(Sys.time()),
                    stringsAsFactors = FALSE)
                  if (nrow(df_old_t) > 0) {
                    common <- intersect(names(df_old_t), names(new_t))
                    df_old_t <- rbind(df_old_t[, common, drop=FALSE], new_t[, common, drop=FALSE])
                  } else df_old_t <- new_t
                }
                write.csv(df_old_t, csv_teklif_path, row.names = FALSE, fileEncoding = "UTF-8")
              } else {
                con_t <- get_con()
                # Eyni sinif üçün köhnə təklifləri sil
                if (mezmun != "all") {
                  mezmun_list <- unique(yeni_df$mezmun_xetti)
                  for (mx in mezmun_list) {
                    dbExecute(con_t, sprintf("DELETE FROM yeni_elave_teklifleri WHERE sinif = %d AND mezmun_xetti = '%s'",
                      as.integer(sinif), gsub("'", "''", mx)))
                  }
                } else {
                  dbExecute(con_t, sprintf("DELETE FROM yeni_elave_teklifleri WHERE sinif = %d", as.integer(sinif)))
                }
                for (i in 1:nrow(yeni_df)) {
                  r <- yeni_df[i, ]
                  dbExecute(con_t, sprintf(
                    "INSERT INTO yeni_elave_teklifleri (sinif, mezmun_xetti, standart_kodu, standart_metni,
                     alt_standart_kodu, alt_standart_metni, bloom_seviyyesi, cetinlik, cefr_seviyyesi,
                     esaslandirma, status)
                     VALUES (%d, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', 'teklif')",
                    as.integer(sinif),
                    gsub("'", "''", if (!is.null(r$mezmun_xetti)) as.character(r$mezmun_xetti) else ""),
                    gsub("'", "''", if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else ""),
                    gsub("'", "''", if (!is.null(r$standart_metni)) as.character(r$standart_metni) else ""),
                    gsub("'", "''", if (!is.null(r$alt_standart_kodu)) as.character(r$alt_standart_kodu) else ""),
                    gsub("'", "''", if (!is.null(r$alt_standart_metni)) as.character(r$alt_standart_metni) else ""),
                    gsub("'", "''", if (!is.null(r$bloom_seviyyesi)) as.character(r$bloom_seviyyesi) else ""),
                    gsub("'", "''", if (!is.null(r$cetinlik)) as.character(r$cetinlik) else ""),
                    cefr_sev,
                    gsub("'", "''", if (!is.null(r$esaslandirma)) as.character(r$esaslandirma) else "")
                  ))
                }
                dbDisconnect(con_t)
              }
              cat(sprintf("Yeni təkliflər yazıldı: sinif %s, %d təklif\n", sinif, nrow(yeni_df)))
            }, error = function(e) {
              cat(sprintf("Yeni təkliflər yazılarkən xəta: %s\n", e$message))
            })
          }

          session$sendCustomMessage("ai_timer_stop", list(
            target = "ai_timer_panel", ok = TRUE,
            elapsed = sprintf("%.1f", res$time_sec),
            inp = formatC(res$input_tokens, format = "d", big.mark = ","),
            out = formatC(res$output_tokens, format = "d", big.mark = ","),
            cost = sprintf("$%.4f", (res$input_tokens * 3 + res$output_tokens * 15) / 1e6)
          ))

          total_tok <- res$input_tokens + res$output_tokens
          output$ai_token_display <- renderUI(
            tags$div(class = "token-display token-done",
              sprintf("✅ Addım 1 tamamlandı | %.1f san | %s token | $%.4f",
                      res$time_sec, formatC(total_tok, format="d", big.mark=","),
                      (res$input_tokens * 3 + res$output_tokens * 15) / 1e6))
          )

          # HTML hesabatı yaz
          stats_html <- make_stats_bar(res$time_sec, res$input_tokens, res$output_tokens)
          report_dir <- file.path(getwd(), "html_reports")
          if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
          mezmun_suffix <- if (mezmun == "all") "hamisi" else gsub("[^a-zA-Z0-9]", "_", tolower(mezmun))
          report_file <- file.path(report_dir, sprintf("sinif_%s_%s_tehlil.html", sinif, mezmun_suffix))
          report_content <- paste0(
            '<!DOCTYPE html>\n<html lang="az">\n<head>\n<meta charset="UTF-8">\n',
            '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
            '<title>Sinif ', sinif, ' - Təhlil</title>\n',
            '<style>body{font-family:"Noto Sans",sans-serif;font-size:17px;line-height:1.85;max-width:1100px;margin:0 auto;padding:40px;background:#f8fafc;color:#1e293b}',
            '.header{background:linear-gradient(135deg,#0D47A1,#1976D2);color:white;padding:30px;border-radius:14px;margin-bottom:30px}',
            '.header h1{color:white;border:none;margin:0}.header p{color:#bbdefb;margin:8px 0 0}',
            '</style>\n</head>\n<body>\n',
            '<div class="header"><h1>Sinif ', sinif, ' — Təhlil (', mezmun_label, ')</h1>',
            '<p>CEFR: ', cefr_sev, ' | ', CLAUDE_MODEL, ' | ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</p></div>\n',
            full_html, '\n', stats_html, '\n</body>\n</html>')
          writeLines(report_content, report_file, useBytes = FALSE)

          n_yeni_saved <- if (is.data.frame(yeni_df)) nrow(yeni_df) else 0
          output$ai_result <- renderUI(tagList(
            tags$div(class = "ai-output", HTML(full_html)),
            HTML(stats_html),
            tags$div(style = "margin-top:16px;padding:12px 20px;background:#dcfce7;border:1px solid #86efac;border-radius:10px;font-size:15px;color:#166534;",
              sprintf("📄 Təhlil yazıldı: html_reports/sinif_%s_%s_tehlil.html", sinif, mezmun_suffix)),
            if (n_yeni_saved > 0) tags$div(style = "margin-top:12px;padding:14px 20px;background:#EDE7F6;border:2px solid #7E57C2;border-radius:10px;font-size:16px;color:#4527A0;font-weight:600;",
              sprintf("💡 %d yeni standart təklifi 'Yeni Təkliflər' tab-ına yazıldı. Orada qəbul/rədd edə bilərsiniz.", n_yeni_saved)) else NULL,
            tags$div(style = "margin-top:12px;padding:14px 20px;background:#FFF3E0;border:2px solid #FF9800;border-radius:10px;font-size:16px;color:#E65100;font-weight:600;",
              "👇 Addım 2: Dəyişiklik tələb edən standartlar yüklənəcək. 'Standartları yüklə' basın.")
          ))

        }, error = function(e) {
          # JSON parse uğursuz — raw text göstər
          session$sendCustomMessage("ai_timer_stop", list(
            target = "ai_timer_panel", ok = TRUE,
            elapsed = sprintf("%.1f", res$time_sec),
            inp = formatC(res$input_tokens, format="d", big.mark=","),
            out = formatC(res$output_tokens, format="d", big.mark=","),
            cost = sprintf("$%.4f", (res$input_tokens*3 + res$output_tokens*15)/1e6)))
          output$ai_token_display <- renderUI(tags$div(class="token-display token-done",
            sprintf("✅ %.1f san | JSON parse xətası — raw nəticə göstərilir", res$time_sec)))
          output$ai_result <- renderUI(tags$div(class = "ai-output", HTML(res$text)))
        })

      } else {
        session$sendCustomMessage("ai_timer_stop", list(
          target = "ai_timer_panel", ok = FALSE,
          elapsed = sprintf("%.1f", res$time_sec), inp = "0", out = "0", cost = "$0"))
        output$ai_token_display <- renderUI(tags$div(class = "token-display token-error",
          sprintf("❌ Xəta (%.1f san)", res$time_sec)))
        output$ai_result <- renderUI(tags$div(style = "padding:30px;color:#dc2626;",
          tags$h3("Xəta baş verdi"), tags$p(res$error)))
      }
    }, once = TRUE)
  })

  # ============================================================
  # ADDIM 2: İNTERAKTİV SEÇİM — Standartları yüklə, status seç, AI yeniləsin
  # ============================================================

  # 2a: Addım 1 nəticəsindən yalnız dəyişiklik tələb edən standartları yüklə
  observeEvent(input$btn_step2_load, {
    sinif <- input$sinif_sec
    cefr_sev <- cefr_map[as.character(sinif)]
    s1 <- step1_results()

    if (is.null(s1)) {
      output$step2_status <- renderUI(tags$div(style="padding:20px;color:#dc2626;font-weight:600;",
        "❌ Əvvəlcə Addım 1-i icra edin! Təhlil nəticəsi yoxdur."))
      return()
    }

    rows <- list()
    rid <- 1

    # Yenilənməsi təklif olunan standartlar
    if (is.data.frame(s1$standartlar) && nrow(s1$standartlar) > 0) {
      for (i in 1:nrow(s1$standartlar)) {
        r <- s1$standartlar[i, ]
        st <- if (!is.null(r$status)) r$status else "qalsin"
        if (st %in% c("yenilensin", "silinsin")) {
          nov <- if (st == "yenilensin") "yenilensin" else "silinsin"
          rows[[rid]] <- data.frame(
            id = rid,
            sinif = as.integer(sinif),
            mezmun_xetti = if (!is.null(r$mezmun_xetti)) as.character(r$mezmun_xetti) else "",
            standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
            standart_metni = if (!is.null(r$standart_metni)) as.character(r$standart_metni) else "",
            alt_standart_kodu = if (!is.null(r$alt_standart_kodu)) as.character(r$alt_standart_kodu) else if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
            alt_standart_metni = if (!is.null(r$alt_standart_metni)) as.character(r$alt_standart_metni) else "",
            deyisiklik_novu = nov,
            esaslandirma = if (!is.null(r$esaslandirma)) as.character(r$esaslandirma) else "",
            bloom_seviyyesi = if (!is.null(r$bloom_seviyyesi)) as.character(r$bloom_seviyyesi) else "",
            cefr_seviyyesi = cefr_sev,
            cetinlik = "",
            stringsAsFactors = FALSE)
          rid <- rid + 1
        }
      }
    }

    # Yeni standart təklifləri artıq ayrıca "Yeni Təkliflər" tab-ında idarə olunur
    # Step 1 bitdikdə avtomatik yeni_elave_teklifleri cədvəlinə yazılır
    n_yeni_teklif <- if (is.data.frame(s1$yeni_teklifler)) nrow(s1$yeni_teklifler) else 0

    if (length(rows) == 0) {
      output$step2_status <- renderUI(tags$div(style="padding:20px;color:#166534;font-weight:600;background:#dcfce7;border-radius:10px;",
        "✅ Bütün standartlar yaxşıdır! Dəyişiklik tələb edən standart yoxdur."))
      return()
    }

    df_all <- do.call(rbind, rows)
    step2_data(df_all)

    n_yen <- sum(df_all$deyisiklik_novu == "yenilensin", na.rm=TRUE)
    n_sil <- sum(df_all$deyisiklik_novu == "silinsin", na.rm=TRUE)
    output$step2_status <- renderUI(
      tags$div(style="padding:14px 20px;background:#FFF3E0;border:2px solid #FF9800;border-radius:10px;font-size:15px;color:#E65100;",
        tags$b(sprintf("📋 Dəyişiklik tələb edən: %d standart", nrow(df_all))),
        tags$br(),
        sprintf("Yenilənsin: %d | Silinsin: %d", n_yen, n_sil),
        if (n_yeni_teklif > 0) sprintf(" | 💡 %d yeni təklif 'Yeni Təkliflər' tab-ında", n_yeni_teklif) else "",
        tags$br(), tags$br(),
        "Hər sətirdəki düymələrlə razılaşın və ya dəyişin. Sonra ",
        tags$b("'AI ilə yenilə'"), " basın — AI yalnız 'Yenilə' seçilmişləri yenidən yazacaq.")
    )
  })

  # 2b: Addım 2 cədvəli — interaktiv düymələrlə
  output$dt_step2_review <- renderDT({
    df <- step2_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mesaj = "📥 'Standartları yüklə' düyməsini basın"),
                       rownames = FALSE, options = list(dom = 't')))
    }

    # Əməliyyat düymələri — "yeni" standartlar üçün Qəbul/Rədd, mövcudlar üçün Saxla/Yenilə/Sil
    df$emeliyyat <- sapply(1:nrow(df), function(i) {
      if (df$deyisiklik_novu[i] %in% c("yeni", "qebul", "redd")) {
        # Yeni standart təklifi — Qəbul et / Rədd et
        paste0(
          '<div style="display:flex;gap:3px;flex-wrap:nowrap;">',
          '<button class="btn btn-xs" style="background:#E8F5E9;border:1px solid #4CAF50;color:#2E7D32;font-weight:600;" ',
            'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'qebul\'}, {priority:\'event\'})">Qəbul et</button>',
          '<button class="btn btn-xs" style="background:#FFEBEE;border:1px solid #F44336;color:#C62828;font-weight:600;" ',
            'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'redd\'}, {priority:\'event\'})">Rədd et</button>',
          '</div>')
      } else {
        # Mövcud standart — Saxla/Yenilə/Sil
        paste0(
          '<div style="display:flex;gap:3px;flex-wrap:nowrap;">',
          '<button class="btn btn-xs" style="background:#E3F2FD;border:1px solid #90CAF9;color:#0D47A1;font-weight:600;" ',
            'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'movcud\'}, {priority:\'event\'})">Saxla</button>',
          '<button class="btn btn-xs" style="background:#E8F5E9;border:1px solid #4CAF50;color:#2E7D32;font-weight:600;" ',
            'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'yenilensin\'}, {priority:\'event\'})">Yenilə</button>',
          '<button class="btn btn-xs" style="background:#FFEBEE;border:1px solid #F44336;color:#C62828;font-weight:600;" ',
            'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'silinsin\'}, {priority:\'event\'})">Sil</button>',
          '</div>')
      }
    })

    show_cols <- c("standart_kodu","alt_standart_kodu","standart_metni","alt_standart_metni","mezmun_xetti",
                   "deyisiklik_novu","bloom_seviyyesi","cetinlik","esaslandirma","emeliyyat")
    show_cols <- intersect(show_cols, names(df))
    df_show <- df[, show_cols, drop=FALSE]
    col_labels <- c("standart_kodu"="Std. Kod","alt_standart_kodu"="Alt Kod","standart_metni"="Standart",
                    "alt_standart_metni"="Alt standart","mezmun_xetti"="Məzmun",
                    "deyisiklik_novu"="Status","bloom_seviyyesi"="Bloom","cetinlik"="Çətinlik",
                    "esaslandirma"="Əsaslandırma","emeliyyat"="Əməliyyat")
    names(df_show) <- col_labels[show_cols]

    datatable(df_show,
              escape = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE,
                columnDefs = list(
                  list(width = '50px', targets = 0),
                  list(width = '55px', targets = 1),
                  list(width = '180px', targets = 2),
                  list(width = '180px', targets = 3),
                  list(width = '70px', targets = 4),
                  list(width = '65px', targets = 5),
                  list(width = '70px', targets = 6),
                  list(width = '50px', targets = 7),
                  list(width = '150px', targets = 8),
                  list(width = '120px', targets = 9)
                )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("movcud", "yenilensin", "yenilenib", "yeni", "silinsin", "silinib", "qebul", "redd"),
                    c("#E3F2FD", "#FFF3E0", "#E8F5E9", "#E3F2FD", "#FFEBEE", "#FFCDD2", "#C8E6C9", "#FFCDD2")),
                  fontWeight = "bold")
  })

  # 2c: Status düymələri handler
  observeEvent(input$step2_action, {
    df <- step2_data()
    row <- input$step2_action$row
    action <- input$step2_action$action
    if (row >= 1 && row <= nrow(df)) {
      df[row, "deyisiklik_novu"] <- action
      step2_data(df)

      # Statistika göstər
      n_sax <- sum(df$deyisiklik_novu == "movcud", na.rm=TRUE)
      n_yen <- sum(df$deyisiklik_novu == "yenilensin", na.rm=TRUE)
      n_sil <- sum(df$deyisiklik_novu == "silinsin", na.rm=TRUE)
      output$step2_status <- renderUI(
        tags$div(style="padding:12px 20px;background:#F5F5F5;border:2px solid #CFD8DC;border-radius:10px;font-size:15px;",
          sprintf("Saxla: %d | Yenilə: %d | Sil: %d — Seçimi bitirdikdən sonra 'AI ilə yenilə' basın.",
                  n_sax, n_yen, n_sil))
      )
    }
  })

  # 2d: AI yalnız "yenilensin" seçilmiş standartları yeniləyir
  observeEvent(input$btn_step2_ai, {
    df <- step2_data()
    if (nrow(df) == 0) {
      showNotification("Əvvəlcə standartları yükləyin!", type = "error")
      return()
    }

    df_to_update <- df[df$deyisiklik_novu == "yenilensin", , drop=FALSE]
    if (nrow(df_to_update) == 0) {
      showNotification("Heç bir standart 'Yenilənsin' statusunda deyil! Əvvəlcə düymələrlə seçin.", type = "warning")
      return()
    }

    sinif <- input$sinif_sec
    cefr_sev <- cefr_map[as.character(sinif)]

    session$sendCustomMessage("ai_timer_start", list(
      target = "ai_timer_panel2",
      status = "AI seçilmiş standartları yeniləyir...",
      info1 = paste0(sinif, "-ci sinif"),
      info2 = paste0(nrow(df_to_update), " standart yenilənəcək")
    ))

    output$step2_status <- renderUI(
      tags$div(class = "token-display token-waiting",
        sprintf("⏳ %d standart AI tərəfindən yenilənir...", nrow(df_to_update)))
    )

    session$onFlushed(function() {
      prompt <- sprintf(
'Sən Azərbaycan dili kurikulumu ekspertisən.
%s-ci sinif üçün aşağıdakı standartlar YENİLƏNMƏLİDİR.
PISA, PIRLS, CEFR (%s), Bloom taksonomiyası və Finlandiya, Sinqapur, Estoniya, Yaponiya, Kanada, İrlandiya standartları əsasında hər birinin yeni, təkmilləşdirilmiş mətnini yaz.

Yenilənəcək standartlar:
%s

CAVABI YALNIZ JSON formatında ver:
[
  {
    "standart_kodu": "...",
    "standart_metni": "YENİ TƏKMİLLƏŞDİRİLMİŞ MƏTN",
    "alt_standart_kodu": "...",
    "alt_standart_metni": "YENİ ALT STANDART MƏTNİ",
    "mezmun_xetti": "...",
    "esaslandirma": "Niyə dəyişdirildi — hansı beynəlxalq çərçəvəyə əsasən",
    "bloom_seviyyesi": "...",
    "cefr_seviyyesi": "%s"
  }
]

Bloom: Xatırlama, Anlama, Tətbiq, Analiz, Qiymətləndirmə, Yaratma
YALNIZ JSON, başqa heç nə yazma.',
        sinif, cefr_sev,
        toJSON(df_to_update[, c("standart_kodu","standart_metni","alt_standart_metni","mezmun_xetti","bloom_seviyyesi"), drop=FALSE],
               pretty = TRUE, auto_unbox = TRUE),
        cefr_sev)

      res <- call_claude(prompt)

      if (res$success) {
        tryCatch({
          json_text <- extract_json_array(res$text)
          parsed <- fromJSON(json_text, simplifyDataFrame = TRUE)

          if (is.data.frame(parsed) && nrow(parsed) > 0) {
            # Yenilənmiş mətnləri əsas data-ya yaz
            df_current <- isolate(step2_data())
            for (i in 1:nrow(parsed)) {
              kod <- parsed$standart_kodu[i]
              idx <- which(df_current$standart_kodu == kod & df_current$deyisiklik_novu == "yenilensin")
              if (length(idx) > 0) {
                idx <- idx[1]
                df_current[idx, "standart_metni"] <- parsed$standart_metni[i]
                if ("alt_standart_metni" %in% names(parsed)) df_current[idx, "alt_standart_metni"] <- parsed$alt_standart_metni[i]
                if ("esaslandirma" %in% names(parsed)) df_current[idx, "esaslandirma"] <- parsed$esaslandirma[i]
                if ("bloom_seviyyesi" %in% names(parsed)) df_current[idx, "bloom_seviyyesi"] <- parsed$bloom_seviyyesi[i]
                df_current[idx, "deyisiklik_novu"] <- "yenilenib"  # yenilensin → yenilenib
              }
            }
            isolate(step2_data(df_current))

            session$sendCustomMessage("ai_timer_stop", list(
              target = "ai_timer_panel2", ok = TRUE,
              elapsed = sprintf("%.1f", res$time_sec),
              inp = formatC(res$input_tokens, format = "d", big.mark = ","),
              out = formatC(res$output_tokens, format = "d", big.mark = ","),
              cost = sprintf("$%.4f", (res$input_tokens * 3 + res$output_tokens * 15) / 1e6)
            ))

            output$step2_status <- renderUI(
              tags$div(style="padding:12px 20px;background:#dcfce7;border:2px solid #4CAF50;border-radius:10px;font-size:15px;color:#166534;font-weight:600;",
                sprintf("✅ %d standart yeniləndi! Cədvəldə yeni mətnlər göstərilir. Addım 3 ilə bazaya yazın.", nrow(parsed)))
            )
          } else { stop("JSON boşdur") }
        }, error = function(e) {
          session$sendCustomMessage("ai_timer_stop", list(
            target = "ai_timer_panel2", ok = FALSE,
            elapsed = sprintf("%.1f", res$time_sec),
            inp = formatC(res$input_tokens, format="d", big.mark=","),
            out = formatC(res$output_tokens, format="d", big.mark=","),
            cost = sprintf("$%.4f", (res$input_tokens*3 + res$output_tokens*15)/1e6)
          ))
          output$step2_status <- renderUI(
            tags$div(class="token-display token-error", paste("❌ JSON xətası:", e$message)))
        })
      } else {
        session$sendCustomMessage("ai_timer_stop", list(
          target = "ai_timer_panel2", ok = FALSE,
          elapsed = sprintf("%.1f", res$time_sec), inp = "0", out = "0", cost = "$0"))
        output$step2_status <- renderUI(
          tags$div(class="token-display token-error", paste("❌ API xətası:", res$error)))
      }
    }, once = TRUE)
  })

  # 2e: AI ilə yeni standart təklif et
  observeEvent(input$btn_step2_add_new, {
    df <- step2_data()
    sinif <- input$sinif_sec
    mezmun <- input$mezmun_sec
    cefr_sev <- cefr_map[as.character(sinif)]
    mezmun_label <- if (mezmun == "all") "bütün məzmun xətləri" else mezmun

    # Mövcud standartları kontekst kimi ver
    if (USE_CSV) {
      df_existing <- csv_movcud()
      if (nrow(df_existing) > 0) {
        df_existing <- df_existing[df_existing$sinif == as.integer(sinif), , drop=FALSE]
        if (mezmun != "all") df_existing <- df_existing[df_existing$mezmun_xetti == mezmun, , drop=FALSE]
        df_existing <- df_existing[, c("standart_kodu","standart_metni","mezmun_xetti"), drop=FALSE]
      }
    } else {
      con <- get_con(); on.exit(dbDisconnect(con))
      query <- sprintf("SELECT standart_kodu, standart_metni, mezmun_xetti FROM movcud_standartlar WHERE sinif = %s", sinif)
      if (mezmun != "all") query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", mezmun))
      df_existing <- dbGetQuery(con, query)
    }

    session$sendCustomMessage("ai_timer_start", list(
      target = "ai_timer_panel2",
      status = "AI yeni standartlar təklif edir...",
      info1 = paste0(sinif, "-ci sinif"),
      info2 = mezmun_label
    ))

    output$step2_status <- renderUI(
      tags$div(class = "token-display token-waiting", "⏳ AI yeni standartlar hazırlayır..."))

    session$onFlushed(function() {
      prompt <- sprintf(
'Sən Azərbaycan dili kurikulumu ekspertisən.
%s-ci sinif, "%s" üçün mövcud standartlar bunlardır:
%s

Bu standartlarda PISA, PIRLS, CEFR (%s) və Bloom taksonomiyası tələblərinə görə BOŞLUQLAR var.
Yalnız ƏKSİK olan, əlavə edilməli YENİ standartlar təklif et. Mövcudları təkrarlama.

CAVABI YALNIZ JSON formatında ver:
[
  {
    "standart_kodu": "YENİ KOD (məs: 1.1.5)",
    "standart_metni": "Yeni standartın mətni",
    "alt_standart_kodu": "...",
    "alt_standart_metni": "Alt standart mətni",
    "mezmun_xetti": "%s",
    "esaslandirma": "Niyə lazımdır — hansı boşluğu doldurur",
    "bloom_seviyyesi": "...",
    "cefr_seviyyesi": "%s"
  }
]
YALNIZ JSON, başqa heç nə.',
        sinif, mezmun_label,
        toJSON(df_existing, pretty=TRUE, auto_unbox=TRUE),
        cefr_sev,
        if (mezmun == "all") "uyğun məzmun xətti" else mezmun,
        cefr_sev)

      res <- call_claude(prompt)

      if (res$success) {
        tryCatch({
          json_text <- extract_json_array(res$text)
          parsed <- fromJSON(json_text, simplifyDataFrame = TRUE)

          if (is.data.frame(parsed) && nrow(parsed) > 0) {
            parsed$sinif <- as.integer(sinif)
            parsed$deyisiklik_novu <- "yeni"
            if (!"cefr_seviyyesi" %in% names(parsed)) parsed$cefr_seviyyesi <- cefr_sev
            if (!"esaslandirma" %in% names(parsed)) parsed$esaslandirma <- ""
            if (!"alt_standart_kodu" %in% names(parsed)) parsed$alt_standart_kodu <- parsed$standart_kodu

            # Əsas cədvələ əlavə et
            df_current <- isolate(step2_data())
            required_cols <- c("id","sinif","mezmun_xetti","standart_kodu","standart_metni",
                               "alt_standart_kodu","alt_standart_metni","deyisiklik_novu",
                               "esaslandirma","bloom_seviyyesi","cefr_seviyyesi")
            for (col in required_cols) {
              if (!col %in% names(parsed)) parsed[[col]] <- ""
            }
            max_id <- if (nrow(df_current) > 0) max(df_current$id, na.rm=TRUE) else 0
            parsed$id <- seq(max_id + 1, max_id + nrow(parsed))
            df_new <- rbind(df_current, parsed[, required_cols])
            isolate(step2_data(df_new))

            session$sendCustomMessage("ai_timer_stop", list(
              target = "ai_timer_panel2", ok = TRUE,
              elapsed = sprintf("%.1f", res$time_sec),
              inp = formatC(res$input_tokens, format="d", big.mark=","),
              out = formatC(res$output_tokens, format="d", big.mark=","),
              cost = sprintf("$%.4f", (res$input_tokens*3 + res$output_tokens*15)/1e6)
            ))
            output$step2_status <- renderUI(
              tags$div(style="padding:12px 20px;background:#FFF3E0;border:2px solid #FF9800;border-radius:10px;font-size:15px;color:#E65100;font-weight:600;",
                sprintf("✅ %d yeni standart əlavə olundu! Cədvəldə 'yeni' statusu ilə görünür.", nrow(parsed)))
            )
          } else { stop("JSON boşdur") }
        }, error = function(e) {
          session$sendCustomMessage("ai_timer_stop", list(
            target="ai_timer_panel2", ok=FALSE, elapsed=sprintf("%.1f",res$time_sec),
            inp=formatC(res$input_tokens,format="d",big.mark=","),
            out=formatC(res$output_tokens,format="d",big.mark=","),
            cost=sprintf("$%.4f",(res$input_tokens*3+res$output_tokens*15)/1e6)))
          output$step2_status <- renderUI(
            tags$div(class="token-display token-error", paste("❌ JSON xətası:", e$message)))
        })
      } else {
        session$sendCustomMessage("ai_timer_stop", list(
          target="ai_timer_panel2", ok=FALSE, elapsed=sprintf("%.1f",res$time_sec),
          inp="0", out="0", cost="$0"))
        output$step2_status <- renderUI(
          tags$div(class="token-display token-error", paste("❌ API xətası:", res$error)))
      }
    }, once = TRUE)
  })

  # ============================================================
  # ADDIM 3: BAZAYA YAZ — Seçilmiş standartları PostgreSQL-ə yaz
  # ============================================================
  observeEvent(input$btn_step3, {
    df_changes <- step2_data()
    s1 <- step1_results()

    if (nrow(df_changes) == 0) {
      output$step3_status <- renderUI(
        tags$div(style="padding:20px;color:#dc2626;font-weight:600;",
          "❌ Əvvəlcə Addım 2-ni icra edin! Bazaya yazmaq üçün məlumat yoxdur."))
      return()
    }

    sinif <- as.integer(input$sinif_sec)
    mezmun <- input$mezmun_sec
    cefr_sev <- cefr_map[as.character(sinif)]

    tryCatch({
      if (!USE_CSV) {
        con <- get_con()
        on.exit(dbDisconnect(con))
      }

      # Yalnız seçilmiş məzmun xətti üçün sil (yeni təklifləri QORUYURUQ)
      if (!USE_CSV) {
        if (mezmun != "all") {
          dbExecute(con, sprintf("DELETE FROM yenilenmi_standartlar WHERE sinif = %d AND mezmun_xetti = '%s' AND deyisiklik_novu != 'yeni'",
                                 sinif, gsub("'", "''", mezmun)))
        } else {
          dbExecute(con, sprintf("DELETE FROM yenilenmi_standartlar WHERE sinif = %d AND deyisiklik_novu != 'yeni'", sinif))
        }
      }

      # Yazılacaq sətirləri yığ: qalsin + dəyişənlər
      all_rows <- list()

      # 1) Addım 1-dən "qalsin" olanları əlavə et (dəyişməyənlər)
      if (!is.null(s1) && is.data.frame(s1$standartlar)) {
        for (i in 1:nrow(s1$standartlar)) {
          r <- s1$standartlar[i, ]
          st <- if (!is.null(r$status)) r$status else "qalsin"
          if (st == "qalsin") {
            all_rows[[length(all_rows)+1]] <- data.frame(
              sinif = sinif,
              mezmun_xetti = if (!is.null(r$mezmun_xetti)) as.character(r$mezmun_xetti) else "",
              standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
              standart_metni = if (!is.null(r$standart_metni)) as.character(r$standart_metni) else "",
              alt_standart_kodu = if (!is.null(r$alt_standart_kodu)) as.character(r$alt_standart_kodu) else if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
              alt_standart_metni = if (!is.null(r$alt_standart_metni)) as.character(r$alt_standart_metni) else "",
              deyisiklik_novu = "movcud",
              esaslandirma = if (!is.null(r$esaslandirma)) as.character(r$esaslandirma) else "",
              bloom_seviyyesi = if (!is.null(r$bloom_seviyyesi)) as.character(r$bloom_seviyyesi) else "",
              cefr_seviyyesi = cefr_sev,
              stringsAsFactors = FALSE)
          }
        }
      }

      # 2) Addım 2-dən dəyişənləri əlavə et (istifadəçi "ləğv" etməyibsə)
      for (i in 1:nrow(df_changes)) {
        row <- df_changes[i, ]
        # "movcud" statusuna dəyişdirilibsə — artıq yuxarıda qalsin kimi yazılıb, skip
        if (row$deyisiklik_novu == "movcud") next
        all_rows[[length(all_rows)+1]] <- data.frame(
          sinif = sinif,
          mezmun_xetti = as.character(row$mezmun_xetti),
          standart_kodu = as.character(row$standart_kodu),
          standart_metni = as.character(row$standart_metni),
          alt_standart_kodu = as.character(row$alt_standart_kodu),
          alt_standart_metni = as.character(row$alt_standart_metni),
          deyisiklik_novu = as.character(row$deyisiklik_novu),
          esaslandirma = as.character(row$esaslandirma),
          bloom_seviyyesi = as.character(row$bloom_seviyyesi),
          cefr_seviyyesi = as.character(row$cefr_seviyyesi),
          stringsAsFactors = FALSE)
      }

      if (length(all_rows) == 0) {
        output$step3_status <- renderUI(tags$div(style="padding:20px;color:#dc2626;", "Yazmaq üçün məlumat yoxdur!"))
        return()
      }

      df_write <- do.call(rbind, all_rows)

      # Təklif statuslarını son statusa çevir: yenilensin → yenilenib, silinsin → silinib
      df_write$deyisiklik_novu[df_write$deyisiklik_novu == "yenilensin"] <- "yenilenib"
      df_write$deyisiklik_novu[df_write$deyisiklik_novu == "silinsin"] <- "silinib"

      # Bazaya / CSV-yə yaz
      if (USE_CSV) {
        # CSV rejimində — mövcud CSV-ni oxu, köhnə datanı sil, yenisini əlavə et
        csv_path <- file.path(getwd(), "data", "yenilenmi_standartlar_backup.csv")
        df_old <- csv_yenilenmi()
        if (nrow(df_old) > 0) {
          if (mezmun != "all") {
            df_old <- df_old[!(df_old$sinif == sinif & df_old$mezmun_xetti == mezmun), , drop=FALSE]
          } else {
            df_old <- df_old[df_old$sinif != sinif, , drop=FALSE]
          }
        }
        # df_write sütunlarını uyğunlaşdır
        write_cols <- c("sinif","mezmun_xetti","standart_kodu","standart_metni",
                        "alt_standart_kodu","alt_standart_metni","deyisiklik_novu",
                        "esaslandirma","bloom_seviyyesi","cefr_seviyyesi")
        df_write_csv <- df_write[, intersect(write_cols, names(df_write)), drop=FALSE]
        if (nrow(df_old) > 0) {
          # Eyni sütunları saxla
          common_cols <- intersect(names(df_old), names(df_write_csv))
          df_combined <- rbind(df_old[, common_cols, drop=FALSE], df_write_csv[, common_cols, drop=FALSE])
        } else {
          df_combined <- df_write_csv
        }
        write.csv(df_combined, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        for (i in 1:nrow(df_write)) {
          row <- df_write[i, ]
          dbExecute(con, sprintf(
            "INSERT INTO yenilenmi_standartlar (sinif, mezmun_xetti, standart_kodu, standart_metni,
             alt_standart_kodu, alt_standart_metni, deyisiklik_novu, esaslandirma, bloom_seviyyesi, cefr_seviyyesi)
             VALUES (%d, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
            sinif,
            gsub("'", "''", as.character(row$mezmun_xetti)),
            gsub("'", "''", as.character(row$standart_kodu)),
            gsub("'", "''", as.character(row$standart_metni)),
            gsub("'", "''", as.character(row$alt_standart_kodu)),
            gsub("'", "''", as.character(row$alt_standart_metni)),
            gsub("'", "''", as.character(row$deyisiklik_novu)),
            gsub("'", "''", as.character(row$esaslandirma)),
            gsub("'", "''", as.character(row$bloom_seviyyesi)),
            gsub("'", "''", as.character(row$cefr_seviyyesi))
          ))
        }
      }

      n_movcud <- sum(df_write$deyisiklik_novu == "movcud", na.rm=TRUE)
      n_yenilenib <- sum(df_write$deyisiklik_novu == "yenilenib", na.rm=TRUE)
      n_yeni <- sum(df_write$deyisiklik_novu == "yeni", na.rm=TRUE)
      n_silinib <- sum(df_write$deyisiklik_novu == "silinib", na.rm=TRUE)

      save_target <- if (USE_CSV) "CSV faylına" else "PostgreSQL bazasına"
      output$step3_status <- renderUI(
        tags$div(style="padding:20px;background:#dcfce7;border:2px solid #4CAF50;border-radius:12px;",
          tags$h4(style="color:#166534;margin:0 0 10px 0;", sprintf("✅ %s uğurla yazıldı!", save_target)),
          tags$p(style="font-size:16px;color:#166534;margin:0;",
            sprintf("Sinif %d: Mövcud: %d, Yenilənib: %d, Silinib: %d — Cəmi: %d",
                    sinif, n_movcud, n_yenilenib, n_silinib, nrow(df_write))),
          tags$p(style="font-size:14px;color:#166534;margin:10px 0 0 0;",
            "📚 'Yekun Standartlar' tab-ında bütün standartları görə bilərsiniz.",
            tags$br(),
            "💡 Yeni standartları 'Yeni Təkliflər' tab-ından qəbul edib yekuna əlavə edə bilərsiniz.",
            tags$br(),
            "📊 'Müqayisə' tab-ında köhnə və yeni standartları yan-yana müqayisə edə bilərsiniz.")
        )
      )
      showNotification(sprintf("✅ Sinif %d: %d standart %s yazıldı!", sinif, nrow(df_write), save_target), type = "message", duration = 10)

      # ============================================================
      # YEKUN STANDARTLAR — SQL müqayisə ilə avtomatik qurulur
      # ============================================================
      # movcud_standartlar (orijinal) vs yenilenmi_standartlar (yenilənmiş) müqayisə
      # Köhnə mətn + Yeni mətn yan-yana, status avtomatik təyin olunur
      # Tam yeni standartlar yekuna QATILMIR (ayrıca bazada saxlanılır)
      # ============================================================
      tryCatch({
        if (USE_CSV) {
          # CSV rejimində — manual müqayisə
          df_base <- csv_movcud()
          if (nrow(df_base) > 0) df_base <- df_base[df_base$sinif == sinif, , drop=FALSE]
          df_yen <- csv_yenilenmi()
          if (nrow(df_yen) > 0) df_yen <- df_yen[df_yen$sinif == sinif & df_yen$deyisiklik_novu != "yeni", , drop=FALSE]

          if (nrow(df_base) > 0) {
            df_yekun <- data.frame(
              sinif = df_base$sinif, mezmun_xetti = df_base$mezmun_xetti,
              standart_kodu = df_base$standart_kodu, alt_standart_kodu = df_base$alt_standart_kodu,
              standart_metni = df_base$standart_metni, alt_standart_metni = df_base$alt_standart_metni,
              yeni_standart_metni = df_base$standart_metni, yeni_alt_standart_metni = df_base$alt_standart_metni,
              bloom_seviyyesi = if ("bloom_seviyyesi" %in% names(df_base)) df_base$bloom_seviyyesi else "",
              cefr_seviyyesi = cefr_sev, esaslandirma = "", beynelxalq_istinad = "",
              status = "movcud", stringsAsFactors = FALSE)
            # Yenilenmi ilə müqayisə
            if (nrow(df_yen) > 0) {
              for (i in 1:nrow(df_yen)) {
                row <- df_yen[i, ]
                ak <- as.character(row$alt_standart_kodu)
                idx <- which(df_yekun$alt_standart_kodu == ak)
                if (length(idx) > 0) {
                  idx <- idx[1]
                  new_text <- as.character(row$alt_standart_metni)
                  old_text <- df_yekun$alt_standart_metni[idx]
                  if (row$deyisiklik_novu == "silinib") {
                    df_yekun$status[idx] <- "silinib"
                    df_yekun$yeni_standart_metni[idx] <- ""
                    df_yekun$yeni_alt_standart_metni[idx] <- ""
                  } else if (!is.na(new_text) && new_text != old_text) {
                    df_yekun$yeni_standart_metni[idx] <- as.character(row$standart_metni)
                    df_yekun$yeni_alt_standart_metni[idx] <- new_text
                    df_yekun$status[idx] <- "yenilenib"
                  }
                  df_yekun$bloom_seviyyesi[idx] <- if ("bloom_seviyyesi" %in% names(row)) as.character(row$bloom_seviyyesi) else df_yekun$bloom_seviyyesi[idx]
                  df_yekun$esaslandirma[idx] <- if ("esaslandirma" %in% names(row) && nchar(as.character(row$esaslandirma)) > 0) as.character(row$esaslandirma) else df_yekun$esaslandirma[idx]
                }
              }
            }
            csv_yekun_path <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
            df_old <- if (file.exists(csv_yekun_path)) read.csv(csv_yekun_path, stringsAsFactors=FALSE, fileEncoding="UTF-8") else data.frame()
            # Digər sinifləri saxla + eyni sinifin "yeni" statuslu standartlarını qoru
            if (nrow(df_old) > 0) {
              df_yeni_qorunan <- df_old[df_old$sinif == sinif & !is.na(df_old$status) & df_old$status == "yeni", , drop=FALSE]
              df_old <- df_old[df_old$sinif != sinif, , drop=FALSE]
              if (nrow(df_yeni_qorunan) > 0) df_old <- rbind(df_old, df_yeni_qorunan)
            }
            df_yekun$id <- seq_len(nrow(df_yekun))
            if (nrow(df_old) > 0) {
              common <- intersect(names(df_old), names(df_yekun))
              df_combined <- rbind(df_old[, common, drop=FALSE], df_yekun[, common, drop=FALSE])
            } else df_combined <- df_yekun
            write.csv(df_combined, csv_yekun_path, row.names = FALSE, fileEncoding = "UTF-8")
          }
        } else {
          # PostgreSQL — SQL ilə müqayisə edib yekunu yenidən qur
          # Əvvəlcə qəbul edilmiş yeni standartları qoru (status = 'yeni')
          dbExecute(con, sprintf("DELETE FROM yekun_standartlar WHERE sinif = %d AND status != 'yeni'", sinif))
          dbExecute(con, sprintf("
            INSERT INTO yekun_standartlar (sinif, mezmun_xetti, standart_kodu, alt_standart_kodu,
              standart_metni, alt_standart_metni, yeni_standart_metni, yeni_alt_standart_metni,
              bloom_seviyyesi, cefr_seviyyesi, esaslandirma, beynelxalq_istinad, status)
            SELECT
              m.sinif, m.mezmun_xetti, m.standart_kodu, m.alt_standart_kodu,
              m.standart_metni, m.alt_standart_metni,
              COALESCE(y.standart_metni, m.standart_metni),
              COALESCE(y.alt_standart_metni, m.alt_standart_metni),
              COALESCE(y.bloom_seviyyesi, m.bloom_seviyyesi, ''),
              '%s',
              COALESCE(y.esaslandirma, ''),
              '',
              CASE
                WHEN y.deyisiklik_novu = 'silinib' THEN 'silinib'
                WHEN y.alt_standart_metni IS NOT NULL AND y.alt_standart_metni != m.alt_standart_metni THEN 'yenilenib'
                ELSE 'movcud'
              END
            FROM movcud_standartlar m
            LEFT JOIN yenilenmi_standartlar y
              ON m.alt_standart_kodu = y.alt_standart_kodu
              AND m.sinif = y.sinif
              AND y.deyisiklik_novu != 'yeni'
            WHERE m.sinif = %d
            ORDER BY m.mezmun_xetti, m.standart_kodu", cefr_sev, sinif))
        }

        # Statistika
        if (USE_CSV) {
          n_yek <- nrow(df_yekun)
          n_yek_movcud <- sum(df_yekun$status == "movcud", na.rm=TRUE)
          n_yek_yenilenib <- sum(df_yekun$status == "yenilenib", na.rm=TRUE)
          n_yek_silinib <- sum(df_yekun$status == "silinib", na.rm=TRUE)
        } else {
          yek_stats <- dbGetQuery(con, sprintf("SELECT status, COUNT(*) as cnt FROM yekun_standartlar WHERE sinif = %d GROUP BY status", sinif))
          n_yek <- sum(yek_stats$cnt)
          n_yek_movcud <- sum(yek_stats$cnt[yek_stats$status == "movcud"])
          n_yek_yenilenib <- sum(yek_stats$cnt[yek_stats$status == "yenilenib"])
          n_yek_silinib <- sum(yek_stats$cnt[yek_stats$status == "silinib"])
        }
        cat(sprintf("Yekun quruldu: mövcud=%d, yenilənib=%d, silinib=%d, CƏMİ=%d\n",
            n_yek_movcud, n_yek_yenilenib, n_yek_silinib, n_yek))
      }, error = function(e_yekun) cat("Yekun yazma xətası:", e_yekun$message, "\n"))

      # CSV backup-ları da yenilə (yalnız PostgreSQL rejimində)
      if (!USE_CSV) tryCatch({
        df_all_m <- dbGetQuery(con, "SELECT * FROM movcud_standartlar ORDER BY sinif, mezmun_xetti, standart_kodu")
        df_all_y <- dbGetQuery(con, "SELECT * FROM yenilenmi_standartlar ORDER BY sinif, mezmun_xetti, standart_kodu")
        write.csv(df_all_m, file.path(getwd(), "data", "movcud_standartlar_backup.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        write.csv(df_all_y, file.path(getwd(), "data", "yenilenmi_standartlar_backup.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        cat("CSV backup yeniləndi\n")
      }, error = function(e2) cat("CSV backup xətası:", e2$message, "\n"))

    }, error = function(e) {
      output$step3_status <- renderUI(
        tags$div(style="padding:20px;color:#dc2626;font-weight:600;",
          paste("❌ Bazaya yazma xətası:", e$message)))
      showNotification(paste("❌ Xəta:", e$message), type = "error")
    })
  })
  
  # --- Addım 1 və 2 üçün reaktiv data ---
  step1_results <- reactiveVal(NULL)  # Addım 1 JSON nəticəsi
  step2_data <- reactiveVal(data.frame())

  # ============================================================
  # YEKUN STANDARTLAR TAB
  # ============================================================

  # Yekun data reactive
  data_yekun <- reactive({
    sinif <- as.integer(input$yekun_sinif)
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
        if (!file.exists(f)) return(data.frame())
        df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        if (nrow(df) == 0) return(df)
        df <- df[df$sinif == sinif, , drop=FALSE]
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        df <- dbGetQuery(con, sprintf(
          "SELECT * FROM yekun_standartlar WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu", sinif))
      }
      # Status filtri
      if (!is.null(input$yekun_status_filter) && input$yekun_status_filter != "all") {
        df <- df[df$status == input$yekun_status_filter, , drop=FALSE]
      }
      df
    }, error = function(e) data.frame())
  })

  # Value boxes
  output$vb_yekun_umumi <- renderValueBox({
    sinif <- as.integer(input$yekun_sinif)
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
        if (!file.exists(f)) return(valueBox(0L, "Ümumi", icon = icon("list"), color = "blue"))
        df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        df <- df[df$sinif == sinif, , drop=FALSE]
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        df <- dbGetQuery(con, sprintf("SELECT status FROM yekun_standartlar WHERE sinif = %d", sinif))
      }
      valueBox(as.integer(nrow(df)), "Ümumi", icon = icon("list"), color = "blue")
    }, error = function(e) valueBox(0L, "Ümumi", icon = icon("list"), color = "blue"))
  })

  output$vb_yekun_movcud <- renderValueBox({
    df <- data_yekun(); n <- if (nrow(df) > 0) as.integer(sum(df$status == "movcud", na.rm=TRUE)) else 0L
    valueBox(n, "Dəyişməmiş", icon = icon("check"), color = "light-blue")
  })
  output$vb_yekun_yenilenib <- renderValueBox({
    df <- data_yekun(); n <- if (nrow(df) > 0) as.integer(sum(df$status == "yenilenib", na.rm=TRUE)) else 0L
    valueBox(n, "Yenilənmiş", icon = icon("edit"), color = "green")
  })
  output$vb_yekun_yeni <- renderValueBox({
    df <- data_yekun(); n <- if (nrow(df) > 0) as.integer(sum(df$status == "yeni", na.rm=TRUE)) else 0L
    valueBox(n, "Yeni əlavə", icon = icon("plus"), color = "orange")
  })
  output$vb_yekun_silinib <- renderValueBox({
    df <- data_yekun(); n <- if (nrow(df) > 0) as.integer(sum(df$status == "silinib", na.rm=TRUE)) else 0L
    valueBox(n, "Silinmiş", icon = icon("trash"), color = "red")
  })

  # Yekun cədvəl — Köhnə və Yeni YAN-YANA, bütün atributlar
  output$dt_yekun <- renderDT({
    df <- data_yekun()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mesaj = "Bu sinif üçün yekun standart yoxdur. Əvvəlcə 'Claude AI Təhlil' bölməsindən 3 addımı tamamlayın."),
                       rownames = FALSE, options = list(dom = 't')))
    }

    # Status adlarını Azərbaycan dilinə çevir
    status_adi <- c("movcud" = "Dəyişməmiş", "yenilenib" = "Yenilənmiş", "silinib" = "Silinmiş", "yeni" = "Yeni əlavə")
    df$status_adi <- sapply(df$status, function(s) {
      if (s %in% names(status_adi)) status_adi[s] else s
    })

    # Yeni sütunlar mövcud deyilsə əlavə et
    if (!"yeni_standart_metni" %in% names(df)) df$yeni_standart_metni <- df$standart_metni
    if (!"yeni_alt_standart_metni" %in% names(df)) df$yeni_alt_standart_metni <- df$alt_standart_metni

    show_cols <- c("standart_kodu","alt_standart_kodu","mezmun_xetti",
                   "standart_metni","yeni_standart_metni",
                   "alt_standart_metni","yeni_alt_standart_metni",
                   "bloom_seviyyesi","cefr_seviyyesi","esaslandirma","status_adi")
    show_cols <- intersect(show_cols, names(df))
    df_show <- df[, show_cols, drop=FALSE]
    col_labels <- c("standart_kodu"="Std Kod","alt_standart_kodu"="Alt Kod","mezmun_xetti"="Məzmun xətti",
                    "standart_metni"="Mövcud standart","yeni_standart_metni"="Yenilənmiş standart",
                    "alt_standart_metni"="Mövcud alt standart","yeni_alt_standart_metni"="Yenilənmiş alt standart",
                    "bloom_seviyyesi"="Bloom","cefr_seviyyesi"="CEFR",
                    "esaslandirma"="Əsaslandırma","status_adi"="Status")
    names(df_show) <- col_labels[show_cols]

    datatable(df_show,
              options = list(pageLength = 50, scrollX = TRUE, autoWidth = FALSE,
                columnDefs = list(
                  list(width = '50px', targets = 0),   # Std Kod
                  list(width = '55px', targets = 1),   # Alt Kod
                  list(width = '75px', targets = 2),   # Məzmun
                  list(width = '180px', targets = 3),  # Mövcud standart
                  list(width = '180px', targets = 4),  # Yenilənmiş standart
                  list(width = '180px', targets = 5),  # Mövcud alt standart
                  list(width = '180px', targets = 6),  # Yenilənmiş alt standart
                  list(width = '65px', targets = 7),   # Bloom
                  list(width = '40px', targets = 8),   # CEFR
                  list(width = '160px', targets = 9),  # Əsaslandırma
                  list(width = '80px', targets = 10)   # Status
                )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("Dəyişməmiş", "Yenilənmiş", "Silinmiş", "Yeni əlavə"),
                    c("#E3F2FD", "#E8F5E9", "#FFEBEE", "#FFF3E0")),
                  color = styleEqual(
                    c("Dəyişməmiş", "Yenilənmiş", "Silinmiş", "Yeni əlavə"),
                    c("#0D47A1", "#2E7D32", "#C62828", "#E65100")),
                  fontWeight = "bold")
  })

  # Info mesajı
  output$yekun_info <- renderUI({
    df <- data_yekun()
    if (nrow(df) == 0) return(NULL)
    sinif <- input$yekun_sinif
    n_movcud <- sum(df$status == "movcud", na.rm=TRUE)
    n_yenilenib <- sum(df$status == "yenilenib", na.rm=TRUE)
    n_yeni <- sum(df$status == "yeni", na.rm=TRUE)
    n_silinib <- sum(df$status == "silinib", na.rm=TRUE)
    tags$div(style="padding:12px 18px;background:#E3F2FD;border:1px solid #90CAF9;border-radius:8px;font-size:14px;color:#0D47A1;",
      sprintf("Yekun_Standartlar-%s: Cəmi %d standart — Dəyişməmiş: %d, Yenilənmiş: %d, Yeni əlavə: %d, Silinmiş: %d",
              sinif, nrow(df), n_movcud, n_yenilenib, n_yeni, n_silinib),
      if (n_yeni > 0) tags$br() else NULL,
      if (n_yeni > 0) tags$span(style="color:#E65100;font-weight:600;",
        sprintf("🟠 %d yeni standart 'Yeni Təkliflər' tab-ından qəbul edilərək əlavə olunub.", n_yeni)) else NULL)
  })

  # Yekun CSV ixrac
  output$download_yekun_csv <- downloadHandler(
    filename = function() { sprintf("Yekun_Standartlar_%s.csv", input$yekun_sinif) },
    content = function(file) {
      df <- data_yekun()
      if (nrow(df) > 0) {
        status_adi <- c("movcud" = "Dəyişməmiş", "yenilenib" = "Yenilənmiş",
                        "yeni" = "Yeni əlavə", "silinib" = "Silinmiş")
        df$status_adi <- sapply(df$status, function(s) if (s %in% names(status_adi)) status_adi[s] else s)
        export_cols <- c("sinif","mezmun_xetti","standart_kodu","alt_standart_kodu",
                         "standart_metni","yeni_standart_metni",
                         "alt_standart_metni","yeni_alt_standart_metni",
                         "bloom_seviyyesi","cefr_seviyyesi","esaslandirma","status_adi")
        export_cols <- intersect(export_cols, names(df))
        df <- df[, export_cols, drop=FALSE]
        names(df)[names(df) == "status_adi"] <- "Status"
      }
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # Yekun Excel ixrac
  output$download_yekun_excel <- downloadHandler(
    filename = function() { sprintf("Yekun_Standartlar_%s.xlsx", input$yekun_sinif) },
    content = function(file) {
      df <- data_yekun()
      if (nrow(df) > 0) {
        status_adi <- c("movcud" = "Dəyişməmiş", "yenilenib" = "Yenilənmiş",
                        "yeni" = "Yeni əlavə", "silinib" = "Silinmiş")
        df$status_adi <- sapply(df$status, function(s) if (s %in% names(status_adi)) status_adi[s] else s)
        export_cols <- c("sinif","mezmun_xetti","standart_kodu","alt_standart_kodu",
                         "standart_metni","yeni_standart_metni",
                         "alt_standart_metni","yeni_alt_standart_metni",
                         "bloom_seviyyesi","cefr_seviyyesi","esaslandirma","status_adi")
        export_cols <- intersect(export_cols, names(df))
        df <- df[, export_cols, drop=FALSE]
        names(df)[names(df) == "status_adi"] <- "Status"
      }
      # writexl paketi ilə
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(df, file)
      } else {
        # Fallback: CSV kimi yaz
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )

  # --- EXPORT ---
  output$download_html <- downloadHandler(
    filename = function() { sprintf("Yekun_Standartlar_Sinif_%s.html", input$sinif_sec) },
    content = function(file) {
      sinif <- as.integer(input$sinif_sec)
      cefr_sev <- cefr_map[as.character(sinif)]
      # Yekun datanı al
      tryCatch({
        if (USE_CSV) {
          f <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
          df <- if (file.exists(f)) read.csv(f, stringsAsFactors=FALSE, fileEncoding="UTF-8") else data.frame()
          if (nrow(df) > 0) df <- df[df$sinif == sinif, , drop=FALSE]
        } else {
          con <- get_con(); on.exit(dbDisconnect(con))
          df <- dbGetQuery(con, sprintf("SELECT * FROM yekun_standartlar WHERE sinif = %d ORDER BY
            CASE status WHEN 'movcud' THEN 1 WHEN 'yenilenib' THEN 2 WHEN 'silinib' THEN 3 WHEN 'yeni' THEN 4 END,
            mezmun_xetti, standart_kodu", sinif))
        }
      }, error = function(e) { df <<- data.frame() })

      n_movcud <- sum(df$status == "movcud", na.rm=TRUE)
      n_yenilenib <- sum(df$status == "yenilenib", na.rm=TRUE)
      n_yeni <- sum(df$status == "yeni", na.rm=TRUE)
      n_silinib <- sum(df$status == "silinib", na.rm=TRUE)
      tarix <- format(Sys.time(), "%d.%m.%Y %H:%M")

      # HTML sətirləri yarat
      rows_html <- ""
      if (nrow(df) > 0) {
        for (i in 1:nrow(df)) {
          r <- df[i, ]
          st <- if (!is.na(r$status)) r$status else "movcud"
          bg <- switch(st, "movcud"="#FFFFFF", "yenilenib"="#FFF8E1", "silinib"="#FFEBEE", "yeni"="#E8F5E9", "#FFFFFF")
          st_label <- switch(st, "movcud"="Dəyişməmiş", "yenilenib"="Yenilənmiş", "silinib"="Silinmiş", "yeni"="Yeni əlavə", st)
          st_color <- switch(st, "movcud"="#1565C0", "yenilenib"="#E65100", "silinib"="#C62828", "yeni"="#2E7D32", "#333")

          kohne_alt <- if ("alt_standart_metni" %in% names(r) && !is.na(r$alt_standart_metni)) r$alt_standart_metni else ""
          yeni_alt <- if ("yeni_alt_standart_metni" %in% names(r) && !is.na(r$yeni_alt_standart_metni)) r$yeni_alt_standart_metni else ""
          bloom <- if ("bloom_seviyyesi" %in% names(r) && !is.na(r$bloom_seviyyesi)) r$bloom_seviyyesi else ""
          esas <- if ("esaslandirma" %in% names(r) && !is.na(r$esaslandirma)) r$esaslandirma else ""

          rows_html <- paste0(rows_html, sprintf(
            '<tr style="background:%s;">
              <td style="font-weight:600;">%s</td>
              <td>%s</td>
              <td>%s</td>
              <td>%s</td>
              <td>%s</td>
              <td style="color:%s;font-weight:600;">%s</td>
              <td>%s</td>
            </tr>\n',
            bg,
            if (!is.na(r$alt_standart_kodu)) r$alt_standart_kodu else "",
            if (!is.na(r$mezmun_xetti)) r$mezmun_xetti else "",
            kohne_alt, yeni_alt, bloom, st_color, st_label, esas
          ))
        }
      }

      html <- sprintf('<!DOCTYPE html>
<html lang="az">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Yekun Standartlar — %d-ci sinif</title>
<style>
  * { margin:0; padding:0; box-sizing:border-box; }
  body { font-family: "Segoe UI", "Noto Sans", Arial, sans-serif; background:#f5f5f5; color:#333; }
  .header { background:linear-gradient(135deg, #1565C0, #0D47A1); color:#fff; padding:30px 40px; }
  .header h1 { font-size:28px; margin-bottom:8px; }
  .header p { font-size:14px; opacity:0.9; }
  .stats { display:flex; gap:16px; padding:20px 40px; background:#fff; border-bottom:2px solid #E3F2FD; }
  .stat-box { flex:1; text-align:center; padding:16px; border-radius:10px; }
  .stat-box .num { font-size:32px; font-weight:700; }
  .stat-box .label { font-size:12px; margin-top:4px; }
  .stat-movcud { background:#E3F2FD; } .stat-movcud .num { color:#1565C0; }
  .stat-yenilenib { background:#FFF8E1; } .stat-yenilenib .num { color:#E65100; }
  .stat-yeni { background:#E8F5E9; } .stat-yeni .num { color:#2E7D32; }
  .stat-silinib { background:#FFEBEE; } .stat-silinib .num { color:#C62828; }
  .content { padding:20px 40px; }
  table { width:100%%; border-collapse:collapse; background:#fff; border-radius:8px; overflow:hidden; box-shadow:0 2px 8px rgba(0,0,0,0.1); }
  th { background:#1565C0; color:#fff; padding:12px 10px; font-size:13px; text-align:left; }
  td { padding:10px; font-size:13px; border-bottom:1px solid #eee; vertical-align:top; }
  tr:hover { background:#F5F5F5 !important; }
  .footer { text-align:center; padding:20px; color:#999; font-size:12px; margin-top:30px; }
  @media print { body { background:#fff; } .header { background:#1565C0 !important; -webkit-print-color-adjust:exact; print-color-adjust:exact; } }
</style>
</head>
<body>
<div class="header">
  <h1>Yekun Standartlar — %d-ci sinif</h1>
  <p>CEFR səviyyəsi: %s | Tarix: %s | Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi</p>
</div>
<div class="stats">
  <div class="stat-box stat-movcud"><div class="num">%d</div><div class="label">Dəyişməmiş</div></div>
  <div class="stat-box stat-yenilenib"><div class="num">%d</div><div class="label">Yenilənmiş</div></div>
  <div class="stat-box stat-yeni"><div class="num">%d</div><div class="label">Yeni əlavə</div></div>
  <div class="stat-box stat-silinib"><div class="num">%d</div><div class="label">Silinmiş</div></div>
</div>
<div class="content">
<table>
<thead>
  <tr>
    <th style="width:70px;">Kod</th>
    <th style="width:100px;">Məzmun xətti</th>
    <th>Mövcud alt standart</th>
    <th>Yenilənmiş alt standart</th>
    <th style="width:80px;">Bloom</th>
    <th style="width:90px;">Status</th>
    <th>Əsaslandırma</th>
  </tr>
</thead>
<tbody>
%s
</tbody>
</table>
</div>
<div class="footer">
  Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi | Hesabat tarixi: %s
</div>
</body>
</html>',
        sinif, sinif, cefr_sev, tarix, n_movcud, n_yenilenib, n_yeni, n_silinib, rows_html, tarix)

      writeLines(html, file, useBytes = TRUE)

      # html_reports qovluğuna da saxla
      report_dir <- file.path(getwd(), "html_reports")
      if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
      report_path <- file.path(report_dir, sprintf("sinif_%d_yekun.html", sinif))
      writeLines(html, report_path, useBytes = TRUE)
    },
    contentType = "text/html"
  )

  output$download_csv <- downloadHandler(
    filename = function() { sprintf("standartlar_sinif_%s.csv", input$sinif_sec) },
    content = function(file) {
      # Yekun datanı CSV olaraq ixrac et
      sinif <- as.integer(input$sinif_sec)
      tryCatch({
        if (USE_CSV) {
          f2 <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
          df <- if (file.exists(f2)) read.csv(f2, stringsAsFactors=FALSE, fileEncoding="UTF-8") else data.frame()
          if (nrow(df) > 0) df <- df[df$sinif == sinif, , drop=FALSE]
        } else {
          con <- get_con(); on.exit(dbDisconnect(con))
          df <- dbGetQuery(con, sprintf("SELECT * FROM yekun_standartlar WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu", sinif))
        }
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }, error = function(e) write.csv(data.frame(xeta = e$message), file, row.names = FALSE))
    }
  )

  # ============================================================
  # YENİ TƏKLİFLƏR TAB — Server
  # ============================================================

  # Təklif data reactive
  data_teklifler <- reactive({
    input$btn_teklif_qebul_sec  # refresh trigger
    sinif <- as.integer(input$teklif_sinif)
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yeni_elave_teklifleri_backup.csv")
        if (!file.exists(f)) return(data.frame())
        df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        if (nrow(df) == 0) return(df)
        df <- df[df$sinif == sinif, , drop=FALSE]
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        df <- dbGetQuery(con, sprintf(
          "SELECT * FROM yeni_elave_teklifleri WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu", sinif))
      }
      if (!is.null(input$teklif_status_filter) && input$teklif_status_filter != "all") {
        df <- df[df$status == input$teklif_status_filter, , drop=FALSE]
      }
      df
    }, error = function(e) data.frame())
  })

  # Value boxes
  output$vb_teklif_cemi <- renderValueBox({
    sinif <- as.integer(input$teklif_sinif)
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yeni_elave_teklifleri_backup.csv")
        if (!file.exists(f)) return(valueBox(0L, "Cəmi təklif", icon = icon("lightbulb"), color = "orange"))
        df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        df <- df[df$sinif == sinif, , drop=FALSE]
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        df <- dbGetQuery(con, sprintf("SELECT status FROM yeni_elave_teklifleri WHERE sinif = %d", sinif))
      }
      valueBox(as.integer(nrow(df)), "Cəmi təklif", icon = icon("lightbulb"), color = "orange")
    }, error = function(e) valueBox(0L, "Cəmi təklif", icon = icon("lightbulb"), color = "orange"))
  })

  output$vb_teklif_gozleyen <- renderValueBox({
    df <- data_teklifler()
    n <- if (nrow(df) > 0) as.integer(sum(df$status == "teklif", na.rm=TRUE)) else 0L
    valueBox(n, "Gözləyən", icon = icon("clock"), color = "yellow")
  })

  output$vb_teklif_qebul <- renderValueBox({
    df <- data_teklifler()
    n <- if (nrow(df) > 0) as.integer(sum(df$status == "qebul", na.rm=TRUE)) else 0L
    valueBox(n, "Qəbul edilib", icon = icon("check"), color = "green")
  })

  output$vb_teklif_redd <- renderValueBox({
    df <- data_teklifler()
    n <- if (nrow(df) > 0) as.integer(sum(df$status == "redd", na.rm=TRUE)) else 0L
    valueBox(n, "Rədd edilib", icon = icon("times"), color = "red")
  })

  # Təkliflər cədvəli — Qəbul/Rədd düymələri ilə
  output$dt_teklifler <- renderDT({
    df <- data_teklifler()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mesaj = "Bu sinif üçün yeni standart təklifi yoxdur. Əvvəlcə 'Claude AI Təhlil' bölməsindən Addım 1-i icra edin."),
                       rownames = FALSE, options = list(dom = 't')))
    }

    # Status adları
    status_adi <- c("teklif" = "Təklif", "qebul" = "Qəbul edilib", "redd" = "Rədd edilib")
    df$status_adi <- sapply(df$status, function(s) if (s %in% names(status_adi)) status_adi[s] else s)

    # Əməliyyat düymələri
    df$emeliyyat <- sapply(1:nrow(df), function(i) {
      row_id <- df$id[i]
      paste0(
        '<div style="display:flex;gap:4px;flex-wrap:nowrap;">',
        '<button class="btn btn-xs" style="background:#E8F5E9;border:1px solid #4CAF50;color:#2E7D32;font-weight:600;" ',
          'onclick="Shiny.setInputValue(\'teklif_action\', {id:', row_id, ', action:\'qebul\'}, {priority:\'event\'})">Qəbul et</button>',
        '<button class="btn btn-xs" style="background:#FFEBEE;border:1px solid #F44336;color:#C62828;font-weight:600;" ',
          'onclick="Shiny.setInputValue(\'teklif_action\', {id:', row_id, ', action:\'redd\'}, {priority:\'event\'})">Rədd et</button>',
        '</div>')
    })

    show_cols <- c("standart_kodu","alt_standart_kodu","standart_metni","alt_standart_metni",
                   "mezmun_xetti","bloom_seviyyesi","cetinlik","esaslandirma","status_adi","emeliyyat")
    show_cols <- intersect(show_cols, names(df))
    df_show <- df[, show_cols, drop=FALSE]
    col_labels <- c("standart_kodu"="Std. Kod","alt_standart_kodu"="Alt Kod",
                    "standart_metni"="Standart","alt_standart_metni"="Alt standart",
                    "mezmun_xetti"="Məzmun","bloom_seviyyesi"="Bloom","cetinlik"="Çətinlik",
                    "esaslandirma"="Əsaslandırma","status_adi"="Status","emeliyyat"="Əməliyyat")
    names(df_show) <- col_labels[show_cols]

    datatable(df_show,
              escape = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                columnDefs = list(
                  list(width = '50px', targets = 0),
                  list(width = '55px', targets = 1),
                  list(width = '200px', targets = 2),
                  list(width = '200px', targets = 3),
                  list(width = '80px', targets = 4),
                  list(width = '250px', targets = 7),
                  list(width = '120px', targets = 9)
                )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("Təklif", "Qəbul edilib", "Rədd edilib"),
                    c("#FFF3E0", "#C8E6C9", "#FFCDD2")),
                  fontWeight = "bold")
  })

  # Qəbul/Rədd düyməsi handler
  observeEvent(input$teklif_action, {
    row_id <- input$teklif_action$id
    action <- input$teklif_action$action
    tryCatch({
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yeni_elave_teklifleri_backup.csv")
        if (file.exists(f)) {
          df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
          idx <- which(df$id == row_id)
          if (length(idx) > 0) {
            df$status[idx[1]] <- action
            write.csv(df, f, row.names = FALSE, fileEncoding = "UTF-8")
          }
        }
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbExecute(con, sprintf("UPDATE yeni_elave_teklifleri SET status = '%s' WHERE id = %d", action, row_id))
      }
      showNotification(
        if (action == "qebul") "Təklif qəbul edildi!" else "Təklif rədd edildi.",
        type = if (action == "qebul") "message" else "warning", duration = 3)
    }, error = function(e) {
      showNotification(paste("Xəta:", e$message), type = "error")
    })
  })

  # Seçilmişləri yekuna əlavə et
  observeEvent(input$btn_teklif_qebul_sec, {
    sinif <- as.integer(input$teklif_sinif)
    cefr_sev <- cefr_map[as.character(sinif)]
    tryCatch({
      # Qəbul edilmiş təklifləri al
      if (USE_CSV) {
        f <- file.path(getwd(), "data", "yeni_elave_teklifleri_backup.csv")
        if (!file.exists(f)) { showNotification("Təklif yoxdur!", type = "warning"); return() }
        df_qebul <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        df_qebul <- df_qebul[df_qebul$sinif == sinif & df_qebul$status == "qebul", , drop=FALSE]
      } else {
        con <- get_con(); on.exit(dbDisconnect(con))
        df_qebul <- dbGetQuery(con, sprintf(
          "SELECT * FROM yeni_elave_teklifleri WHERE sinif = %d AND status = 'qebul'", sinif))
      }

      if (nrow(df_qebul) == 0) {
        showNotification("Qəbul edilmiş təklif yoxdur! Əvvəlcə təklifləri 'Qəbul et' düyməsi ilə seçin.", type = "warning")
        return()
      }

      # Yekun standartlara əlavə et — yeni standartlar cədvəlin sonuna
      # Köhnə sütunlar boş, yeni sütunlarda standart mətni
      n_added <- 0
      if (USE_CSV) {
        yekun_path <- file.path(getwd(), "data", "yekun_standartlar_backup.csv")
        df_yekun <- if (file.exists(yekun_path)) {
          read.csv(yekun_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        } else data.frame()

        for (i in 1:nrow(df_qebul)) {
          row <- df_qebul[i, ]
          ak <- as.character(row$alt_standart_kodu)
          if (nrow(df_yekun) > 0 && ak %in% df_yekun$alt_standart_kodu[df_yekun$sinif == sinif]) next
          new_row <- data.frame(
            sinif = sinif, mezmun_xetti = as.character(row$mezmun_xetti),
            standart_kodu = as.character(row$standart_kodu),
            alt_standart_kodu = ak,
            standart_metni = "",
            alt_standart_metni = "",
            yeni_standart_metni = as.character(row$standart_metni),
            yeni_alt_standart_metni = as.character(row$alt_standart_metni),
            bloom_seviyyesi = as.character(row$bloom_seviyyesi), cefr_seviyyesi = cefr_sev,
            esaslandirma = as.character(row$esaslandirma), beynelxalq_istinad = "",
            status = "yeni", stringsAsFactors = FALSE)
          if (nrow(df_yekun) > 0) {
            common <- intersect(names(df_yekun), names(new_row))
            df_yekun <- rbind(df_yekun[, common, drop=FALSE], new_row[, common, drop=FALSE])
          } else df_yekun <- new_row
          n_added <- n_added + 1
        }
        if (n_added > 0) write.csv(df_yekun, yekun_path, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        for (i in 1:nrow(df_qebul)) {
          row <- df_qebul[i, ]
          ak <- as.character(row$alt_standart_kodu)
          existing <- dbGetQuery(con, sprintf(
            "SELECT id FROM yekun_standartlar WHERE sinif = %d AND alt_standart_kodu = '%s'",
            sinif, gsub("'", "''", ak)))
          if (nrow(existing) > 0) next
          dbExecute(con, sprintf(
            "INSERT INTO yekun_standartlar (sinif, mezmun_xetti, standart_kodu, alt_standart_kodu,
             standart_metni, alt_standart_metni, yeni_standart_metni, yeni_alt_standart_metni,
             bloom_seviyyesi, cefr_seviyyesi, esaslandirma, beynelxalq_istinad, status)
             VALUES (%d, $t$%s$t$, $t$%s$t$, $t$%s$t$, '', '', $t$%s$t$, $t$%s$t$, $t$%s$t$, $t$%s$t$, $t$%s$t$, '', 'yeni')",
            sinif,
            as.character(row$mezmun_xetti),
            as.character(row$standart_kodu),
            ak,
            as.character(row$standart_metni),
            as.character(row$alt_standart_metni),
            as.character(row$bloom_seviyyesi),
            cefr_sev,
            as.character(row$esaslandirma)
          ))
          n_added <- n_added + 1
        }
      }

      output$teklif_status_msg <- renderUI(
        tags$div(style="padding:16px;background:#dcfce7;border:2px solid #4CAF50;border-radius:10px;font-size:16px;color:#166534;font-weight:600;",
          sprintf("✅ %d yeni standart yekun standartlara əlavə olundu! 'Yekun Standartlar' tab-ına baxın.", n_added))
      )
      showNotification(sprintf("✅ %d standart yekuna əlavə olundu!", n_added), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Xəta:", e$message), type = "error")
      output$teklif_status_msg <- renderUI(
        tags$div(style="padding:16px;background:#FFEBEE;border:2px solid #F44336;border-radius:10px;color:#C62828;",
          paste("❌ Xəta:", e$message)))
    })
  })
}

# --- İŞƏ SALMAQ ---
shinyApp(ui, server)
