# ============================================================
# 03_generate_html.R
# Hər sinif üçün nəfis HTML5 hesabat generatoru
# ============================================================

library(DBI)
library(RPostgres)
library(jsonlite)

cat("=== HTML5 Hesabat Generatoru ===\n\n")

# --- Bazaya qoşulma ---
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "az_dili_standartlar", host = "localhost", port = 5432,
  user = "postgres", password = "postgres"
)

# --- HTML şablonu ---
generate_html <- function(sinif_no, con) {
  cat(sprintf("Sinif %d üçün HTML yaradılır...\n", sinif_no))
  
  # Mövcud standartları oxumaq
  movcud <- dbGetQuery(con, sprintf(
    "SELECT * FROM movcud_standartlar WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu", sinif_no))
  
  # Yenilənmiş standartları oxumaq
  yenilenmi <- dbGetQuery(con, sprintf(
    "SELECT * FROM yenilenmi_standartlar WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu", sinif_no))
  
  # Statistika
  stats <- list(
    movcud    = sum(yenilenmi$deyisiklik_novu == "movcud", na.rm = TRUE),
    yenilenib = sum(yenilenmi$deyisiklik_novu == "yenilenib", na.rm = TRUE),
    yeni      = sum(yenilenmi$deyisiklik_novu == "yeni", na.rm = TRUE),
    silinib   = sum(yenilenmi$deyisiklik_novu == "silinib", na.rm = TRUE)
  )
  
  # CEFR səviyyəsi
  cefr_map <- c("1"="A1","2"="A1","3"="A2","4"="A2","5"="B1","6"="B1","7"="B1","8"="B2","9"="B2","10"="C1","11"="C1")
  cefr <- cefr_map[as.character(sinif_no)]
  
  # Məzmun xətləri
  mezmun_xetleri <- unique(yenilenmi$mezmun_xetti)
  if (length(mezmun_xetleri) == 0) mezmun_xetleri <- c("Dinləmə", "Danışma", "Oxu", "Yazı", "Dil qaydaları")
  
  # --- Standart kartları HTML ---
  generate_cards <- function(mezmun) {
    df <- yenilenmi[yenilenmi$mezmun_xetti == mezmun, ]
    if (nrow(df) == 0) return("")
    
    cards <- ""
    for (i in 1:nrow(df)) {
      row <- df[i, ]
      
      # Rəng və status
      css_class <- switch(row$deyisiklik_novu,
        "movcud"    = "card-movcud",
        "yenilenib" = "card-yenilenib",
        "yeni"      = "card-yeni",
        "silinib"   = "card-silinib",
        "card-movcud"
      )
      
      badge_text <- switch(row$deyisiklik_novu,
        "movcud"    = "Mövcud",
        "yenilenib" = "Yenilənib ✏️",
        "yeni"      = "Yeni ✨",
        "silinib"   = "Silinib 🗑️",
        "Mövcud"
      )
      
      # Bloom badge
      bloom_badge <- if (!is.na(row$bloom_seviyyesi) && nchar(row$bloom_seviyyesi) > 0) {
        sprintf('<span class="badge badge-bloom">🎯 %s</span>', row$bloom_seviyyesi)
      } else ""
      
      # CEFR badge
      cefr_badge <- if (!is.na(row$cefr_seviyyesi) && nchar(row$cefr_seviyyesi) > 0) {
        sprintf('<span class="badge badge-cefr">🌐 %s</span>', row$cefr_seviyyesi)
      } else ""
      
      # Beynəlxalq istinad
      istinad_html <- if (!is.na(row$beynelxalq_istinad) && nchar(row$beynelxalq_istinad) > 0) {
        sprintf('<div class="istinad"><strong>Beynəlxalq istinad:</strong> %s</div>', row$beynelxalq_istinad)
      } else ""
      
      # Əsaslandırma
      esas_html <- if (!is.na(row$esaslandirma) && nchar(row$esaslandirma) > 0) {
        sprintf('<div class="esaslandirma"><strong>Əsaslandırma:</strong> %s</div>', row$esaslandirma)
      } else ""
      
      # Köhnə mətn (yenilenib üçün)
      kohne_html <- if (row$deyisiklik_novu == "yenilenib" && !is.na(row$kohne_metni) && nchar(row$kohne_metni) > 0) {
        sprintf('<div class="kohne-metn"><strong>Əvvəlki:</strong> <del>%s</del></div>', row$kohne_metni)
      } else ""
      
      # PISA/PIRLS
      pisa_html <- if (!is.na(row$pisa_elaqesi) && nchar(row$pisa_elaqesi) > 0) {
        sprintf('<span class="badge badge-pisa">📊 PISA: %s</span>', row$pisa_elaqesi)
      } else ""
      
      pirls_html <- if (!is.na(row$pirls_elaqesi) && nchar(row$pirls_elaqesi) > 0) {
        sprintf('<span class="badge badge-pirls">📖 PIRLS: %s</span>', row$pirls_elaqesi)
      } else ""
      
      # Alt standart
      alt_html <- if (!is.na(row$alt_standart_metni) && nchar(row$alt_standart_metni) > 0) {
        sprintf('<div class="alt-standart"><strong>%s</strong> %s</div>', 
                ifelse(!is.na(row$alt_standart_kodu), row$alt_standart_kodu, ""), 
                row$alt_standart_metni)
      } else ""
      
      card <- sprintf('
      <div class="standard-card %s">
        <div class="card-header">
          <span class="standart-kod">%s</span>
          <span class="badge badge-status">%s</span>
          %s %s
        </div>
        <div class="card-body">
          %s
          <p class="standart-metn">%s</p>
          %s
          <div class="badges-row">%s %s</div>
          %s
          %s
        </div>
      </div>', 
      css_class, 
      ifelse(!is.na(row$standart_kodu), row$standart_kodu, ""),
      badge_text,
      bloom_badge, cefr_badge,
      kohne_html,
      ifelse(!is.na(row$standart_metni), row$standart_metni, ""),
      alt_html,
      pisa_html, pirls_html,
      istinad_html,
      esas_html)
      
      cards <- paste0(cards, card)
    }
    return(cards)
  }
  
  # Məzmun xətləri bölmələri
  sections_html <- ""
  mezmun_icons <- c(
    "Dinləmə" = "👂", "Danışma" = "🗣️", "Oxu" = "📖", 
    "Yazı" = "✍️", "Dil qaydaları" = "📝"
  )
  
  for (mezmun in mezmun_xetleri) {
    icon <- ifelse(mezmun %in% names(mezmun_icons), mezmun_icons[mezmun], "📌")
    cards <- generate_cards(mezmun)
    if (nchar(cards) > 0) {
      sections_html <- paste0(sections_html, sprintf('
      <section class="mezmun-section">
        <h2 class="mezmun-title">%s %s</h2>
        <div class="cards-grid">%s</div>
      </section>', icon, mezmun, cards))
    }
  }
  
  # --- TAM HTML ---
  html <- sprintf('<!DOCTYPE html>
<html lang="az">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Sinif %d — Azərbaycan Dili Standartları</title>
    <style>
        @import url("https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300;400;500;600;700&display=swap");
        
        :root {
            --blue-900: #0D47A1;
            --blue-800: #1565C0;
            --blue-700: #1976D2;
            --blue-600: #1E88E5;
            --blue-500: #2196F3;
            --blue-400: #42A5F5;
            --blue-300: #64B5F6;
            --blue-200: #90CAF9;
            --blue-100: #BBDEFB;
            --blue-50:  #E3F2FD;
            --green-bg: #E8F5E9;
            --green-border: #4CAF50;
            --green-text: #2E7D32;
            --orange-bg: #FFF3E0;
            --orange-border: #FF9800;
            --orange-text: #E65100;
            --red-bg: #FFEBEE;
            --red-border: #F44336;
            --red-text: #C62828;
            --gray-50: #FAFAFA;
            --gray-100: #F5F5F5;
            --gray-200: #EEEEEE;
            --gray-600: #757575;
            --gray-800: #424242;
            --shadow: 0 2px 12px rgba(13, 71, 161, 0.08);
            --shadow-hover: 0 4px 20px rgba(13, 71, 161, 0.15);
        }
        
        * { margin: 0; padding: 0; box-sizing: border-box; }
        
        body {
            font-family: "Noto Sans", "Segoe UI", sans-serif;
            background: linear-gradient(135deg, var(--blue-50) 0%%, #fff 50%%, var(--blue-50) 100%%);
            color: var(--gray-800);
            line-height: 1.7;
            font-size: 17px;
        }
        
        .container { max-width: 1100px; margin: 0 auto; padding: 20px 30px; }
        
        /* HEADER */
        .hero {
            background: linear-gradient(135deg, var(--blue-900) 0%%, var(--blue-700) 50%%, var(--blue-500) 100%%);
            color: white;
            padding: 50px 0;
            text-align: center;
            position: relative;
            overflow: hidden;
        }
        .hero::before {
            content: "";
            position: absolute;
            top: -50%%; left: -50%%;
            width: 200%%; height: 200%%;
            background: radial-gradient(circle, rgba(255,255,255,0.05) 0%%, transparent 70%%);
            animation: pulse 6s ease-in-out infinite;
        }
        @keyframes pulse { 0%%,100%% { transform: scale(1); } 50%% { transform: scale(1.05); } }
        
        .hero h1 {
            font-size: 2.4em;
            font-weight: 700;
            margin-bottom: 10px;
            position: relative;
            text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        .hero .subtitle {
            font-size: 1.2em;
            font-weight: 300;
            opacity: 0.9;
            position: relative;
        }
        .hero .cefr-tag {
            display: inline-block;
            background: rgba(255,255,255,0.2);
            padding: 6px 20px;
            border-radius: 20px;
            margin-top: 15px;
            font-weight: 500;
            position: relative;
        }
        
        /* STATİSTİKA */
        .stats-bar {
            display: flex;
            justify-content: center;
            gap: 20px;
            padding: 25px 0;
            flex-wrap: wrap;
        }
        .stat-item {
            background: white;
            padding: 15px 25px;
            border-radius: 12px;
            box-shadow: var(--shadow);
            text-align: center;
            min-width: 130px;
            transition: transform 0.2s;
        }
        .stat-item:hover { transform: translateY(-2px); box-shadow: var(--shadow-hover); }
        .stat-number { font-size: 2em; font-weight: 700; color: var(--blue-700); }
        .stat-label { font-size: 0.85em; color: var(--gray-600); margin-top: 4px; }
        .stat-item.yenilenib .stat-number { color: var(--green-text); }
        .stat-item.yeni .stat-number { color: var(--orange-text); }
        .stat-item.silinib .stat-number { color: var(--red-text); }
        
        /* LEGEND */
        .legend {
            display: flex;
            justify-content: center;
            gap: 20px;
            padding: 15px;
            background: white;
            border-radius: 12px;
            box-shadow: var(--shadow);
            margin: 20px 0 30px;
            flex-wrap: wrap;
        }
        .legend-item { display: flex; align-items: center; gap: 8px; font-size: 0.9em; }
        .legend-dot {
            width: 14px; height: 14px; border-radius: 4px;
        }
        .legend-dot.movcud { background: var(--blue-100); border: 2px solid var(--blue-400); }
        .legend-dot.yenilenib { background: var(--green-bg); border: 2px solid var(--green-border); }
        .legend-dot.yeni { background: var(--orange-bg); border: 2px solid var(--orange-border); }
        .legend-dot.silinib { background: var(--red-bg); border: 2px solid var(--red-border); }
        
        /* BÖLMƏLƏR */
        .mezmun-section { margin-bottom: 40px; }
        .mezmun-title {
            font-size: 1.5em;
            font-weight: 600;
            color: var(--blue-800);
            padding: 12px 20px;
            background: linear-gradient(90deg, var(--blue-50), transparent);
            border-left: 4px solid var(--blue-600);
            border-radius: 0 8px 8px 0;
            margin-bottom: 20px;
        }
        
        /* KARTLAR */
        .cards-grid { display: flex; flex-direction: column; gap: 16px; }
        
        .standard-card {
            background: white;
            border-radius: 12px;
            box-shadow: var(--shadow);
            overflow: hidden;
            transition: all 0.2s ease;
            border-left: 5px solid var(--blue-400);
        }
        .standard-card:hover { box-shadow: var(--shadow-hover); transform: translateX(3px); }
        
        .card-movcud { border-left-color: var(--blue-400); }
        .card-yenilenib { 
            border-left-color: var(--green-border); 
            background: linear-gradient(to right, var(--green-bg), white 30%%);
        }
        .card-yeni { 
            border-left-color: var(--orange-border); 
            background: linear-gradient(to right, var(--orange-bg), white 30%%);
        }
        .card-silinib { 
            border-left-color: var(--red-border); 
            background: linear-gradient(to right, var(--red-bg), white 30%%);
            opacity: 0.7;
        }
        .card-silinib .standart-metn { text-decoration: line-through; color: var(--red-text); }
        
        .card-header {
            padding: 14px 20px;
            display: flex;
            align-items: center;
            gap: 10px;
            flex-wrap: wrap;
            border-bottom: 1px solid var(--gray-200);
        }
        .standart-kod {
            font-weight: 700;
            font-size: 1.05em;
            color: var(--blue-800);
            font-family: "Courier New", monospace;
        }
        
        .card-body { padding: 16px 20px; }
        .standart-metn { font-size: 1.05em; line-height: 1.8; margin-bottom: 10px; }
        
        .alt-standart {
            padding: 10px 15px;
            background: var(--gray-50);
            border-radius: 8px;
            margin: 8px 0;
            border-left: 3px solid var(--blue-200);
            font-size: 0.95em;
        }
        
        .kohne-metn {
            padding: 10px 15px;
            background: var(--red-bg);
            border-radius: 8px;
            margin-bottom: 10px;
            font-size: 0.9em;
            color: var(--red-text);
        }
        .kohne-metn del { opacity: 0.7; }
        
        /* BADGES */
        .badge {
            display: inline-block;
            padding: 3px 10px;
            border-radius: 20px;
            font-size: 0.78em;
            font-weight: 500;
        }
        .badge-status {
            color: white;
        }
        .card-movcud .badge-status { background: var(--blue-500); }
        .card-yenilenib .badge-status { background: var(--green-border); }
        .card-yeni .badge-status { background: var(--orange-border); color: white; }
        .card-silinib .badge-status { background: var(--red-border); }
        
        .badge-bloom { background: #EDE7F6; color: #4527A0; }
        .badge-cefr { background: #E0F2F1; color: #00695C; }
        .badge-pisa { background: #FFF8E1; color: #F57F17; }
        .badge-pirls { background: #E8EAF6; color: #283593; }
        
        .badges-row { display: flex; gap: 8px; flex-wrap: wrap; margin: 8px 0; }
        
        .istinad, .esaslandirma {
            font-size: 0.88em;
            color: var(--gray-600);
            padding: 8px 12px;
            background: var(--gray-50);
            border-radius: 6px;
            margin-top: 8px;
        }
        
        /* FOOTER */
        .footer {
            text-align: center;
            padding: 30px;
            color: var(--gray-600);
            font-size: 0.85em;
            border-top: 1px solid var(--gray-200);
            margin-top: 40px;
        }
        
        /* ÇAP */
        @media print {
            .hero { break-after: avoid; }
            .standard-card { break-inside: avoid; box-shadow: none; border: 1px solid #ddd; }
            body { font-size: 14px; }
        }
        
        /* MOBİL */
        @media (max-width: 768px) {
            .container { padding: 10px 15px; }
            .hero h1 { font-size: 1.6em; }
            .stats-bar { gap: 10px; }
            .stat-item { min-width: 100px; padding: 10px 15px; }
            .legend { flex-direction: column; align-items: center; }
        }
    </style>
</head>
<body>
    <header class="hero">
        <div class="container">
            <h1>📚 %d-ci Sinif — Azərbaycan Dili Standartları</h1>
            <p class="subtitle">Beynəlxalq standartlara əsaslanan yenilənmiş kurikulum</p>
            <span class="cefr-tag">CEFR Səviyyəsi: %s</span>
        </div>
    </header>
    
    <main class="container">
        <!-- STATİSTİKA -->
        <div class="stats-bar">
            <div class="stat-item">
                <div class="stat-number">%d</div>
                <div class="stat-label">Mövcud (dəyişməz)</div>
            </div>
            <div class="stat-item yenilenib">
                <div class="stat-number">%d</div>
                <div class="stat-label">Yenilənib</div>
            </div>
            <div class="stat-item yeni">
                <div class="stat-number">%d</div>
                <div class="stat-label">Yeni əlavə</div>
            </div>
            <div class="stat-item silinib">
                <div class="stat-number">%d</div>
                <div class="stat-label">Silinib</div>
            </div>
        </div>
        
        <!-- LEGEND -->
        <div class="legend">
            <div class="legend-item"><div class="legend-dot movcud"></div> Mövcud standart</div>
            <div class="legend-item"><div class="legend-dot yenilenib"></div> Yenilənmiş standart</div>
            <div class="legend-item"><div class="legend-dot yeni"></div> Yeni əlavə</div>
            <div class="legend-item"><div class="legend-dot silinib"></div> Silinmiş</div>
        </div>
        
        <!-- STANDARTLAR -->
        %s
    </main>
    
    <footer class="footer">
        <p>🇦🇿 Azərbaycan Respublikası Təhsil İnstitutu (ARTİ) — %s</p>
        <p>PISA • PIRLS • CEFR • Bloom Taksonomiyası | Finlandiya • Sinqapur • Estoniya • Yaponiya • Kanada • İrlandiya</p>
    </footer>
</body>
</html>',
  sinif_no, cefr,
  stats$movcud, stats$yenilenib, stats$yeni, stats$silinib,
  sections_html,
  format(Sys.Date(), "%%Y"))
  
  # Faylı yazmaq
  file_path <- sprintf("html_reports/sinif_%d_standartlar.html", sinif_no)
  writeLines(html, file_path, useBytes = TRUE)
  cat(sprintf("   ✅ %s yaradıldı\n", file_path))
  return(file_path)
}

# --- Bütün sinifləri yaratmaq ---
cat("HTML hesabatları yaradılır...\n\n")
dir.create("html_reports", showWarnings = FALSE)

for (sinif in 1:11) {
  generate_html(sinif, con)
}

# --- İNDEKS SƏHİFƏSİ ---
cat("\nİndeks səhifəsi yaradılır...\n")

# Ümumi statistika
total_stats <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as umumi,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'movcud') as movcud,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yenilenib') as yenilenib,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yeni') as yeni,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'silinib') as silinib
  FROM yenilenmi_standartlar
")

grade_links <- ""
for (s in 1:11) {
  grade_links <- paste0(grade_links, sprintf(
    '    <a href="sinif_%d_standartlar.html" class="grade-card">
        <span class="grade-num">%d</span>
        <span class="grade-label">-ci sinif</span>
    </a>\n', s, s))
}

index_html <- sprintf('<!DOCTYPE html>
<html lang="az">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Azərbaycan Dili Standartları — Ana Səhifə</title>
    <style>
        @import url("https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300;400;600;700&display=swap");
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: "Noto Sans", sans-serif;
            background: linear-gradient(135deg, #E3F2FD 0%%, #fff 50%%, #E3F2FD 100%%);
            min-height: 100vh;
        }
        .hero {
            background: linear-gradient(135deg, #0D47A1, #1976D2, #2196F3);
            color: white; text-align: center; padding: 60px 20px;
        }
        .hero h1 { font-size: 2.5em; margin-bottom: 10px; }
        .hero p { font-size: 1.1em; opacity: 0.9; }
        .container { max-width: 900px; margin: 0 auto; padding: 40px 20px; }
        .stats-row {
            display: flex; justify-content: center; gap: 20px;
            margin: -30px auto 40px; flex-wrap: wrap;
        }
        .stat-box {
            background: white; padding: 20px 30px; border-radius: 12px;
            box-shadow: 0 4px 15px rgba(0,0,0,0.1); text-align: center;
        }
        .stat-box .num { font-size: 2em; font-weight: 700; color: #1565C0; }
        .stat-box .lbl { font-size: 0.85em; color: #757575; }
        .grades-grid {
            display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr));
            gap: 15px; margin-top: 20px;
        }
        .grade-card {
            display: flex; flex-direction: column; align-items: center;
            padding: 25px; background: white; border-radius: 16px;
            box-shadow: 0 2px 12px rgba(13,71,161,0.1);
            text-decoration: none; color: #0D47A1;
            transition: all 0.3s ease; border: 2px solid transparent;
        }
        .grade-card:hover {
            transform: translateY(-5px); border-color: #2196F3;
            box-shadow: 0 8px 25px rgba(33,150,243,0.25);
        }
        .grade-num { font-size: 2.5em; font-weight: 700; }
        .grade-label { font-size: 0.9em; color: #757575; }
        .footer { text-align: center; padding: 30px; color: #757575; font-size: 0.85em; margin-top: 40px; }
    </style>
</head>
<body>
    <div class="hero">
        <h1>🇦🇿 Azərbaycan Dili Standartları</h1>
        <p>1-11 siniflərin yenilənmiş kurikulum standartları</p>
        <p style="margin-top:10px;opacity:0.8">PISA • PIRLS • CEFR • Bloom | Finlandiya • Sinqapur • Estoniya • Yaponiya • Kanada • İrlandiya</p>
    </div>
    <div class="stats-row">
        <div class="stat-box"><div class="num">%d</div><div class="lbl">Ümumi standart</div></div>
        <div class="stat-box"><div class="num" style="color:#4CAF50">%d</div><div class="lbl">Yenilənib</div></div>
        <div class="stat-box"><div class="num" style="color:#FF9800">%d</div><div class="lbl">Yeni əlavə</div></div>
    </div>
    <div class="container">
        <h2 style="text-align:center;color:#0D47A1;margin-bottom:20px;font-size:1.4em;">Sinif seçin</h2>
        <div class="grades-grid">
%s
        </div>
    </div>
    <div class="footer">ARTİ — %s | Beynəlxalq standartlara əsaslanan yenilənmə</div>
</body>
</html>', 
  total_stats$umumi, total_stats$yenilenib, total_stats$yeni,
  grade_links, format(Sys.Date(), "%%Y"))

writeLines(index_html, "html_reports/index.html", useBytes = TRUE)
cat("   ✅ html_reports/index.html yaradıldı\n")

dbDisconnect(con)
cat("\n=== Bütün HTML hesabatlar yaradıldı! ===\n")
cat("Açmaq üçün: open html_reports/index.html\n")
