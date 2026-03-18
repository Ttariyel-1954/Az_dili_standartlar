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

# --- Bazaya qoşulma funksiyası ---
get_con <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = DB_CONFIG$dbname, host = DB_CONFIG$host,
            port = DB_CONFIG$port, user = DB_CONFIG$user, 
            password = DB_CONFIG$password)
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
      encode = "raw", timeout(300))
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
        div(class = "compare-container",
          # Sol panel — Köhnə
          div(class = "compare-panel compare-panel-left",
            div(class = "compare-header compare-header-left", "📋 Mövcud (Köhnə) Standartlar"),
            uiOutput("ui_compare_left")
          ),
          # Divider — sürüşdürmə
          div(class = "compare-divider", id = "compare-divider"),
          # Sağ panel — Yeni
          div(class = "compare-panel compare-panel-right",
            div(class = "compare-header compare-header-right", "✨ Yenilənmiş Standartlar"),
            uiOutput("ui_compare_right")
          )
        ),
        # Sürüşdürmə JS
        tags$script(HTML('
          $(function(){
            var divider = document.getElementById("compare-divider");
            if(!divider) return;
            var container = divider.parentElement;
            var leftPanel = container.querySelector(".compare-panel-left");
            var isDragging = false;
            
            divider.addEventListener("mousedown", function(e){
              isDragging = true;
              document.body.style.cursor = "col-resize";
              document.body.style.userSelect = "none";
              e.preventDefault();
            });
            document.addEventListener("mousemove", function(e){
              if(!isDragging) return;
              var rect = container.getBoundingClientRect();
              var pct = ((e.clientX - rect.left) / rect.width) * 100;
              pct = Math.max(25, Math.min(75, pct));
              leftPanel.style.flex = "0 0 " + pct + "%";
            });
            document.addEventListener("mouseup", function(){
              if(isDragging){
                isDragging = false;
                document.body.style.cursor = "";
                document.body.style.userSelect = "";
              }
            });
          });
        '))
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

  # --- Reaktiv məlumat ---
  data_yenilenmi <- reactive({
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
    con <- get_con()
    on.exit(dbDisconnect(con))
    query <- sprintf("SELECT * FROM movcud_standartlar WHERE sinif = %s ORDER BY mezmun_xetti, standart_kodu", 
                     input$sinif_sec)
    dbGetQuery(con, query)
  })
  
  all_stats <- reactive({
    con <- get_con()
    on.exit(dbDisconnect(con))

    # Mövcud standartlar (əsas say)
    df_m <- dbGetQuery(con, "
      SELECT sinif, COUNT(*) as movcud_say
      FROM movcud_standartlar GROUP BY sinif ORDER BY sinif")

    # Yenilənmiş standartlar (əgər varsa)
    df_y <- dbGetQuery(con, "
      SELECT sinif,
        COUNT(*) FILTER (WHERE deyisiklik_novu = 'movcud') as movcud,
        COUNT(*) FILTER (WHERE deyisiklik_novu = 'yenilenib') as yenilenib,
        COUNT(*) FILTER (WHERE deyisiklik_novu = 'yeni') as yeni,
        COUNT(*) FILTER (WHERE deyisiklik_novu = 'silinib') as silinib
      FROM yenilenmi_standartlar GROUP BY sinif ORDER BY sinif")

    # Birləşdir
    if (nrow(df_m) == 0) return(data.frame(sinif=integer(), umumi=integer(), movcud=integer(),
                                            yenilenib=integer(), yeni=integer(), silinib=integer()))

    df <- df_m
    df$umumi <- df$movcud_say
    df$movcud <- df$movcud_say
    df$yenilenib <- 0L
    df$yeni <- 0L
    df$silinib <- 0L

    # Yenilenmi varsa, həmin siniflərin dəyərlərini üstünə yaz
    if (nrow(df_y) > 0) {
      for (i in 1:nrow(df_y)) {
        idx <- which(df$sinif == df_y$sinif[i])
        if (length(idx) > 0) {
          df$yenilenib[idx] <- df_y$yenilenib[i]
          df$yeni[idx] <- df_y$yeni[i]
          df$silinib[idx] <- df_y$silinib[i]
          df$umumi[idx] <- df$movcud_say[idx] + df_y$yeni[i]
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
  
  output$chart_deyisiklik <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))
    
    counts <- table(df$deyisiklik_novu)
    colors <- c("movcud" = "#90CAF9", "yenilenib" = "#4CAF50", "yeni" = "#FF9800", "silinib" = "#F44336")
    
    plot_ly(labels = names(counts), values = as.numeric(counts), type = "pie",
            marker = list(colors = colors[names(counts)]),
            textinfo = "label+percent") %>%
      layout(font = list(family = "Noto Sans"))
  })
  
  output$chart_mezmun <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))
    
    counts <- as.data.frame(table(df$mezmun_xetti))
    plot_ly(counts, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = "#1976D2")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })
  
  output$chart_bloom <- renderPlotly({
    df <- data_yenilenmi()
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))
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
    con <- get_con(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT cefr_seviyyesi, COUNT(*) as say 
      FROM yenilenmi_standartlar 
      WHERE cefr_seviyyesi IS NOT NULL AND cefr_seviyyesi != ''
      GROUP BY cefr_seviyyesi ORDER BY cefr_seviyyesi")
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))
    
    plot_ly(df, x = ~cefr_seviyyesi, y = ~say, type = "bar",
            marker = list(color = c("#BBDEFB","#90CAF9","#42A5F5","#1976D2","#0D47A1"))) %>%
      layout(xaxis = list(title = "CEFR"), yaxis = list(title = "Say"),
             font = list(family = "Noto Sans"))
  })
  
  output$chart_bloom_all <- renderPlotly({
    con <- get_con(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT bloom_seviyyesi, COUNT(*) as say 
      FROM yenilenmi_standartlar 
      WHERE bloom_seviyyesi IS NOT NULL AND bloom_seviyyesi != ''
      GROUP BY bloom_seviyyesi")
    if (nrow(df) == 0) return(plotly_empty(type = "bar"))
    
    plot_ly(df, labels = ~bloom_seviyyesi, values = ~say, type = "pie",
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
  
  # Müqayisə — Köhnə standartlar (sol panel)
  output$ui_compare_left <- renderUI({
    df <- data_movcud()
    if (nrow(df) == 0) {
      return(div(class = "bos-mesaj",
        div(class = "icon", "📭"),
        tags$p("Bu sinif üçün mövcud standart yoxdur.")
      ))
    }
    
    cards <- lapply(1:nrow(df), function(i) {
      row <- df[i, ]
      kod <- if (!is.na(row$standart_kodu)) row$standart_kodu else ""
      metn <- if (!is.na(row$standart_metni)) row$standart_metni else ""
      alt <- if ("alt_standart_metni" %in% names(row) && !is.na(row$alt_standart_metni)) row$alt_standart_metni else ""
      mez <- if (!is.na(row$mezmun_xetti)) row$mezmun_xetti else ""
      
      div(class = "cmp-card cmp-card-left",
        div(tags$span(class = "cmp-kod", kod),
            tags$span(class = "cmp-mezmun", mez)),
        tags$p(style = "margin: 6px 0 0 0;", metn),
        if (nchar(alt) > 0) tags$p(style = "margin: 4px 0 0 10px; font-size: 13px; color: #757575;", 
                                    tags$em(alt))
      )
    })
    do.call(tagList, cards)
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
        result <- c(result, sprintf('<span class="diff-same">%s</span>', w))
      } else {
        result <- c(result, sprintf('<span class="diff-added">%s</span>', w))
      }
    }
    paste(result, collapse = " ")
  }
  
  # Müqayisə — Yenilənmiş standartlar (sağ panel) — diff ilə
  output$ui_compare_right <- renderUI({
    df_yeni <- data_yenilenmi()
    df_kohne <- data_movcud()
    
    if (nrow(df_yeni) == 0) {
      return(div(class = "bos-mesaj",
        div(class = "icon", "📭"),
        tags$p("Bu sinif üçün yenilənmiş standart yoxdur.")
      ))
    }
    
    # Köhnə standartları kod üzrə lüğətə çevirmək
    kohne_map <- list()
    if (nrow(df_kohne) > 0) {
      for (i in 1:nrow(df_kohne)) {
        k <- df_kohne$standart_kodu[i]
        if (!is.na(k)) kohne_map[[k]] <- df_kohne$standart_metni[i]
      }
    }
    
    cards <- lapply(1:nrow(df_yeni), function(i) {
      row <- df_yeni[i, ]
      kod <- if (!is.na(row$standart_kodu)) row$standart_kodu else ""
      metn <- if (!is.na(row$standart_metni)) row$standart_metni else ""
      alt <- if ("alt_standart_metni" %in% names(row) && !is.na(row$alt_standart_metni)) row$alt_standart_metni else ""
      mez <- if ("mezmun_xetti" %in% names(row) && !is.na(row$mezmun_xetti)) row$mezmun_xetti else ""
      nov <- if (!is.na(row$deyisiklik_novu)) row$deyisiklik_novu else "movcud"
      
      # Badge
      badge_text <- switch(nov,
        "movcud" = "Mövcud", "yenilenib" = "Yenilənib ✏️",
        "yeni" = "Yeni ✨", "silinib" = "Silinib 🗑️", "")
      badge_class <- paste0("cmp-badge cmp-badge-", nov)
      card_class <- paste0("cmp-card cmp-card-right-", nov)
      
      # Diff highlight — yenilənibsə köhnə ilə müqayisə
      metn_html <- metn
      if (nov == "yenilenib") {
        kohne_metn <- NULL
        if ("kohne_metni" %in% names(row) && !is.na(row$kohne_metni) && nchar(row$kohne_metni) > 0) {
          kohne_metn <- row$kohne_metni
        } else if (kod %in% names(kohne_map)) {
          kohne_metn <- kohne_map[[kod]]
        }
        if (!is.null(kohne_metn)) {
          metn_html <- highlight_diff(kohne_metn, metn)
        }
      }
      
      div(class = card_class,
        div(tags$span(class = "cmp-kod", kod),
            tags$span(class = "cmp-mezmun", mez),
            tags$span(class = badge_class, badge_text)),
        tags$p(style = "margin: 6px 0 0 0;", HTML(metn_html)),
        if (nchar(alt) > 0) tags$p(style = "margin: 4px 0 0 10px; font-size: 13px; color: #555;",
                                    tags$em(alt))
      )
    })
    do.call(tagList, cards)
  })
  
  output$dt_beynelxalq <- renderDT({
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
  })
  
  output$dt_olkeler <- renderDT({
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
  })
  
  # ============================================================
  # ADDIM 1: TƏHLİL — JSON ilə hər standarta status ver + yeni təkliflər
  # ============================================================
  observeEvent(input$btn_step1, {
    sinif <- input$sinif_sec
    mezmun <- input$mezmun_sec

    con <- get_con(); on.exit(dbDisconnect(con))
    query <- sprintf("SELECT * FROM movcud_standartlar WHERE sinif = %s", sinif)
    if (mezmun != "all") query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", mezmun))
    query <- paste0(query, " ORDER BY mezmun_xetti, standart_kodu")
    df <- dbGetQuery(con, query)

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

    prompt <- sprintf(
'Sən Azərbaycan dili kurikulumu ekspertisən.
%s-ci sinif, "%s" üçün aşağıdakı mövcud standartları təhlil et.

Mövcud standartlar:
%s

TAPŞIRIQ: CAVABI YALNIZ JSON formatında ver. Heç bir izah, HTML və ya başqa mətn yazma.

JSON strukturu:
{
  "umumi_qiymetlendirme": "3-4 cümlə ilə ümumi qiymətləndirmə (PISA/PIRLS/CEFR/Bloom balansı)",
  "standartlar": [
    {
      "standart_kodu": "1.1.1",
      "standart_metni": "Mövcud standart mətni (olduğu kimi)",
      "alt_standart_metni": "Mövcud alt standart mətni",
      "mezmun_xetti": "Dinləmə və danışma",
      "bloom_seviyyesi": "Anlama",
      "status": "qalsin",
      "esaslandirma": "Niyə bu qərar verildi — beynəlxalq istinad"
    }
  ],
  "yeni_teklifler": [
    {
      "standart_kodu": "1.1.5",
      "standart_metni": "Təklif olunan yeni standart mətni",
      "alt_standart_metni": "Alt standart mətni",
      "mezmun_xetti": "Oxu",
      "bloom_seviyyesi": "Analiz",
      "cetinlik": "orta",
      "esaslandirma": "Hansı boşluğu doldurur — PISA/PIRLS/CEFR istinadı"
    }
  ]
}

status dəyərləri: "qalsin", "yenilensin", "silinsin"
cetinlik dəyərləri: "asan", "orta", "cetin"
Bloom: Xatırlama, Anlama, Tətbiq, Analiz, Qiymətləndirmə, Yaratma
Məzmun xətləri: Dinləmə və danışma, Oxu, Yazı, Dil qaydaları
CEFR: %s

Beynəlxalq müqayisə: Finlandiya, Sinqapur, Estoniya, Yaponiya, Kanada (Ontario), İrlandiya
YALNIZ JSON, başqa heç nə yazma.',
      sinif, mezmun_label,
      toJSON(df[, std_cols, drop=FALSE], pretty = TRUE, auto_unbox = TRUE),
      cefr_sev)

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

          full_html <- paste0(
            '<div style="font-size:16px;line-height:1.7;padding:16px;background:#F8F9FA;border-radius:10px;border-left:4px solid #1976D2;margin-bottom:20px;">',
            umumi, '</div>', cards_html, yeni_html)

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

          output$ai_result <- renderUI(tagList(
            tags$div(class = "ai-output", HTML(full_html)),
            HTML(stats_html),
            tags$div(style = "margin-top:16px;padding:12px 20px;background:#dcfce7;border:1px solid #86efac;border-radius:10px;font-size:15px;color:#166534;",
              sprintf("📄 Təhlil yazıldı: html_reports/sinif_%s_%s_tehlil.html", sinif, mezmun_suffix)),
            tags$div(style = "margin-top:12px;padding:14px 20px;background:#FFF3E0;border:2px solid #FF9800;border-radius:10px;font-size:16px;color:#E65100;font-weight:600;",
              "👇 Addım 2: Yalnız dəyişiklik tələb edən standartlar avtomatik yüklənəcək. 'Standartları yüklə' basın.")
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
            alt_standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
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

    # Yeni standart təklifləri
    if (is.data.frame(s1$yeni_teklifler) && nrow(s1$yeni_teklifler) > 0) {
      for (i in 1:nrow(s1$yeni_teklifler)) {
        r <- s1$yeni_teklifler[i, ]
        rows[[rid]] <- data.frame(
          id = rid,
          sinif = as.integer(sinif),
          mezmun_xetti = if (!is.null(r$mezmun_xetti)) as.character(r$mezmun_xetti) else "",
          standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
          standart_metni = if (!is.null(r$standart_metni)) as.character(r$standart_metni) else "",
          alt_standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
          alt_standart_metni = if (!is.null(r$alt_standart_metni)) as.character(r$alt_standart_metni) else "",
          deyisiklik_novu = "yeni",
          esaslandirma = if (!is.null(r$esaslandirma)) as.character(r$esaslandirma) else "",
          bloom_seviyyesi = if (!is.null(r$bloom_seviyyesi)) as.character(r$bloom_seviyyesi) else "",
          cefr_seviyyesi = cefr_sev,
          cetinlik = if (!is.null(r$cetinlik)) as.character(r$cetinlik) else "",
          stringsAsFactors = FALSE)
        rid <- rid + 1
      }
    }

    if (length(rows) == 0) {
      output$step2_status <- renderUI(tags$div(style="padding:20px;color:#166534;font-weight:600;background:#dcfce7;border-radius:10px;",
        "✅ Bütün standartlar yaxşıdır! Dəyişiklik tələb edən standart yoxdur."))
      return()
    }

    df_all <- do.call(rbind, rows)
    step2_data(df_all)

    n_yen <- sum(df_all$deyisiklik_novu == "yenilensin", na.rm=TRUE)
    n_sil <- sum(df_all$deyisiklik_novu == "silinsin", na.rm=TRUE)
    n_yeni <- sum(df_all$deyisiklik_novu == "yeni", na.rm=TRUE)
    output$step2_status <- renderUI(
      tags$div(style="padding:14px 20px;background:#FFF3E0;border:2px solid #FF9800;border-radius:10px;font-size:15px;color:#E65100;",
        tags$b(sprintf("📋 Dəyişiklik tələb edən: %d standart", nrow(df_all))),
        tags$br(),
        sprintf("Yenilənsin: %d | Silinsin: %d | Yeni təklif: %d", n_yen, n_sil, n_yeni),
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

    # Əməliyyat düymələri
    df$emeliyyat <- sapply(1:nrow(df), function(i) {
      paste0(
        '<div style="display:flex;gap:3px;flex-wrap:nowrap;">',
        '<button class="btn btn-xs" style="background:#E3F2FD;border:1px solid #90CAF9;color:#0D47A1;font-weight:600;" ',
          'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'movcud\'}, {priority:\'event\'})">Saxla</button>',
        '<button class="btn btn-xs" style="background:#E8F5E9;border:1px solid #4CAF50;color:#2E7D32;font-weight:600;" ',
          'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'yenilensin\'}, {priority:\'event\'})">Yenilə</button>',
        '<button class="btn btn-xs" style="background:#FFEBEE;border:1px solid #F44336;color:#C62828;font-weight:600;" ',
          'onclick="Shiny.setInputValue(\'step2_action\', {row:', i, ', action:\'silinsin\'}, {priority:\'event\'})">Sil</button>',
        '</div>')
    })

    show_cols <- c("standart_kodu","standart_metni","mezmun_xetti",
                   "deyisiklik_novu","bloom_seviyyesi","cetinlik","esaslandirma","emeliyyat")
    show_cols <- intersect(show_cols, names(df))
    df_show <- df[, show_cols, drop=FALSE]
    col_labels <- c("standart_kodu"="Kod","standart_metni"="Standart","mezmun_xetti"="Məzmun",
                    "deyisiklik_novu"="Status","bloom_seviyyesi"="Bloom","cetinlik"="Çətinlik",
                    "esaslandirma"="Əsaslandırma","emeliyyat"="Əməliyyat")
    names(df_show) <- col_labels[show_cols]

    datatable(df_show,
              escape = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE,
                columnDefs = list(
                  list(width = '55px', targets = 0),
                  list(width = '200px', targets = 1),
                  list(width = '80px', targets = 2),
                  list(width = '70px', targets = 3),
                  list(width = '80px', targets = 4),
                  list(width = '60px', targets = 5),
                  list(width = '170px', targets = 6),
                  list(width = '130px', targets = 7)
                )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("movcud", "yenilensin", "yenilenib", "yeni", "silinsin", "silinib"),
                    c("#E3F2FD", "#FFF3E0", "#E8F5E9", "#E3F2FD", "#FFEBEE", "#FFCDD2")),
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
      n_yeni <- sum(df$deyisiklik_novu == "yeni", na.rm=TRUE)
      n_sil <- sum(df$deyisiklik_novu == "silinsin", na.rm=TRUE)
      output$step2_status <- renderUI(
        tags$div(style="padding:12px 20px;background:#F5F5F5;border:2px solid #CFD8DC;border-radius:10px;font-size:15px;",
          sprintf("Saxla: %d | Yenilə: %d | Yeni: %d | Sil: %d — Seçimi bitirdikdən sonra 'AI ilə yenilə' basın.",
                  n_sax, n_yen, n_yeni, n_sil))
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
    con <- get_con(); on.exit(dbDisconnect(con))
    query <- sprintf("SELECT standart_kodu, standart_metni, mezmun_xetti FROM movcud_standartlar WHERE sinif = %s", sinif)
    if (mezmun != "all") query <- paste0(query, sprintf(" AND mezmun_xetti = '%s'", mezmun))
    df_existing <- dbGetQuery(con, query)

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
      con <- get_con()
      on.exit(dbDisconnect(con))

      # Yalnız seçilmiş məzmun xətti üçün sil
      if (mezmun != "all") {
        dbExecute(con, sprintf("DELETE FROM yenilenmi_standartlar WHERE sinif = %d AND mezmun_xetti = '%s'",
                               sinif, gsub("'", "''", mezmun)))
      } else {
        dbExecute(con, sprintf("DELETE FROM yenilenmi_standartlar WHERE sinif = %d", sinif))
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
              alt_standart_kodu = if (!is.null(r$standart_kodu)) as.character(r$standart_kodu) else "",
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

      # Bazaya yaz
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

      n_movcud <- sum(df_write$deyisiklik_novu == "movcud", na.rm=TRUE)
      n_yenilenib <- sum(df_write$deyisiklik_novu == "yenilenib", na.rm=TRUE)
      n_yeni <- sum(df_write$deyisiklik_novu == "yeni", na.rm=TRUE)
      n_silinib <- sum(df_write$deyisiklik_novu == "silinib", na.rm=TRUE)

      output$step3_status <- renderUI(
        tags$div(style="padding:20px;background:#dcfce7;border:2px solid #4CAF50;border-radius:12px;",
          tags$h4(style="color:#166534;margin:0 0 10px 0;", "✅ PostgreSQL bazasına uğurla yazıldı!"),
          tags$p(style="font-size:16px;color:#166534;margin:0;",
            sprintf("Sinif %d: Cəmi %d standart yazıldı — Mövcud: %d, Yenilənib: %d, Yeni: %d, Silinib: %d",
                    sinif, nrow(df_write), n_movcud, n_yenilenib, n_yeni, n_silinib)),
          tags$p(style="font-size:14px;color:#166534;margin:10px 0 0 0;",
            "📚 'Standartlar' tab-ında filtr düymələri ilə nəticələri görə bilərsiniz.",
            tags$br(),
            "📊 'Müqayisə' tab-ında köhnə və yeni standartları yan-yana müqayisə edə bilərsiniz.")
        )
      )
      showNotification(sprintf("✅ Sinif %d: %d standart PostgreSQL-ə yazıldı!", sinif, nrow(df_write)), type = "message", duration = 10)

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

  # --- EXPORT ---
  output$download_html <- downloadHandler(
    filename = function() { sprintf("sinif_%s_standartlar.html", input$sinif_sec) },
    content = function(file) {
      html_path <- sprintf("html_reports/sinif_%s_standartlar.html", input$sinif_sec)
      if (file.exists(html_path)) {
        file.copy(html_path, file)
      } else {
        writeLines("<h1>HTML hesabat hələ yaradılmayıb!</h1><p>03_generate_html.R skriptini işə salın.</p>", file)
      }
    },
    contentType = "text/html"
  )
  
  output$download_csv <- downloadHandler(
    filename = function() { sprintf("standartlar_sinif_%s.csv", input$sinif_sec) },
    content = function(file) {
      write.csv(data_yenilenmi(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# --- İŞƏ SALMAQ ---
shinyApp(ui, server)
