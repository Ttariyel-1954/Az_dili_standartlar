# ============================================================
# 02_update_standards.R
# Claude API ilə standartların yenilənməsi
# ============================================================

library(DBI)
library(RPostgres)
library(httr)
library(jsonlite)

cat("=== Standartların Claude API ilə Yenilənməsi ===\n\n")

# --- KONFİQURASİYA ---
CLAUDE_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(CLAUDE_API_KEY) == 0) {
  CLAUDE_API_KEY <- readline("Claude API açarınızı daxil edin: ")
}

DB_NAME     <- "az_dili_standartlar"
DB_HOST     <- "localhost"
DB_PORT     <- 5432
DB_USER     <- "postgres"
DB_PASSWORD <- "postgres"

# --- Bazaya qoşulma ---
cat("1. Bazaya qoşulma...\n")
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = DB_NAME, host = DB_HOST, port = DB_PORT,
  user = DB_USER, password = DB_PASSWORD
)

# --- Claude API funksiyası ---
call_claude <- function(prompt, max_tokens = 8000) {
  response <- POST(
    url = "https://api.anthropic.com/v1/messages",
    add_headers(
      "x-api-key"         = CLAUDE_API_KEY,
      "anthropic-version"  = "2023-06-01",
      "content-type"       = "application/json"
    ),
    body = toJSON(list(
      model      = "claude-sonnet-4-20250514",
      max_tokens = max_tokens,
      messages   = list(
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  if (status_code(response) != 200) {
    cat("API XƏTA:", status_code(response), "\n")
    cat(content(response, "text"), "\n")
    return(NULL)
  }
  
  result <- content(response, "parsed")
  return(result$content[[1]]$text)
}

# --- Hər sinif üçün yeniləmə ---
update_grade <- function(sinif_no, con) {
  cat(sprintf("\n========== SİNİF %d ==========\n", sinif_no))
  
  # Mövcud standartları oxumaq
  query <- sprintf(
    "SELECT * FROM movcud_standartlar WHERE sinif = %d ORDER BY mezmun_xetti, standart_kodu",
    sinif_no
  )
  standartlar <- dbGetQuery(con, query)
  
  if (nrow(standartlar) == 0) {
    cat("   ⚠️  Bu sinif üçün mövcud standart yoxdur!\n")
    return(NULL)
  }
  
  cat(sprintf("   %d mövcud standart tapıldı\n", nrow(standartlar)))
  
  # Beynəlxalq çərçəvələri oxumaq
  cerceveler <- dbGetQuery(con, sprintf(
    "SELECT * FROM beynelxalq_cerceveler WHERE 
     sinif_araligi LIKE '%%-%d%%' OR sinif_araligi LIKE '%d-%%' 
     OR '%d' BETWEEN SPLIT_PART(sinif_araligi, '-', 1)::int 
     AND SPLIT_PART(sinif_araligi, '-', 2)::int",
    sinif_no, sinif_no, sinif_no
  ))
  
  # Ölkə standartlarını oxumaq
  olkeler <- dbGetQuery(con, sprintf(
    "SELECT * FROM olke_standartlari WHERE 
     sinif_araligi LIKE '%%-%d%%' OR sinif_araligi LIKE '%d-%%'",
    sinif_no, sinif_no
  ))
  
  # Standartları JSON formatına çevirmək
  standartlar_json <- toJSON(standartlar, pretty = TRUE, auto_unbox = TRUE)
  cerceveler_json <- toJSON(cerceveler, pretty = TRUE, auto_unbox = TRUE)
  olkeler_json <- toJSON(olkeler, pretty = TRUE, auto_unbox = TRUE)
  
  # CEFR səviyyəsini müəyyən etmək
  cefr_map <- list(
    "1" = "A1", "2" = "A1", "3" = "A2", "4" = "A2",
    "5" = "B1", "6" = "B1", "7" = "B1", "8" = "B2", "9" = "B2",
    "10" = "C1", "11" = "C1"
  )
  cefr_level <- cefr_map[[as.character(sinif_no)]]
  
  # Claude API sorğusu
  prompt <- sprintf('
Sən Azərbaycan dili kurikulumu üzrə beynəlxalq səviyyədə tanınmış ekspertisən.
Azərbaycan Respublikasında %d-ci sinif üçün Azərbaycan dili fənninin standartlarını yeniləməlisən.

MÖVCUDLİKDƏ OLAN STANDARTLAR:
%s

BEYNƏLXALQ ÇƏRÇƏVƏLƏR (PISA, PIRLS, CEFR, Bloom):
%s

APARICI ÖLKƏLƏR STANDARTLARI (Finlandiya, Sinqapur, Estoniya, Yaponiya, Kanada, İrlandiya):
%s

Bu sinif üçün CEFR səviyyəsi: %s

TAPŞIRİQ:
1. Hər mövcud standartı PISA, PIRLS, CEFR, Bloom taksonomiyası prizmasından qiymətləndir
2. Finlandiya, Sinqapur, Estoniya, Yaponiya, Kanada (Ontario), İrlandiya standartları ilə müqayisə et
3. Hər standart üçün qərar ver: saxlanmalı (movcud), yenilənməli (yenilenib), yoxsa silinməli (silinib)
4. Yeniləmə varsa, yeni mətni yaz + köhnə mətni saxla
5. ÇOX VACİB: Əlavə olunmalı tamamilə YENİ standartları təklif et (yeni)
6. Yeni standartlar 21-ci əsr bacarıqlarını əks etdirməlidir: rəqəmsal savadlılıq, kritik düşüncə, media savadlılığı, yaradıcılıq

MƏZMUN XƏTLƏRİ: Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları

Cavabı YALNIZ aşağıdakı JSON formatında ver, başqa heç nə yazma:
[
  {
    "standart_kodu": "1.1.1",
    "standart_metni": "Standart mətni",
    "alt_standart_kodu": "1.1.1.1",
    "alt_standart_metni": "Alt standart mətni",
    "mezmun_xetti": "Oxu",
    "deyisiklik_novu": "movcud|yenilenib|yeni|silinib",
    "kohne_metni": "Əvvəlki mətn (yalnız yenilenib üçün)",
    "esaslandirma": "Niyə bu dəyişiklik edildi",
    "beynelxalq_istinad": "PISA/PIRLS/CEFR/ölkə adı",
    "bloom_seviyyesi": "Xatırlama|Anlama|Tətbiq|Analiz|Qiymətləndirmə|Yaratma",
    "cefr_seviyyesi": "%s",
    "pisa_elaqesi": "PISA ilə əlaqəsi",
    "pirls_elaqesi": "PIRLS ilə əlaqəsi",
    "olke_istinadi": "Hansı ölkənin standartından ilhamlanıb"
  }
]', sinif_no, standartlar_json, cerceveler_json, olkeler_json, cefr_level, cefr_level)

  cat("   Claude API sorğusu göndərilir...\n")
  response_text <- call_claude(prompt)
  
  if (is.null(response_text)) {
    cat("   ❌ API cavab vermədi!\n")
    return(NULL)
  }
  
  # JSON-u parse etmək
  cat("   Cavab parse edilir...\n")
  
  # JSON-u təmizləmək
  json_text <- response_text
  # Əgər ```json ilə başlayırsa, təmizləyək
  json_text <- gsub("```json\\s*", "", json_text)
  json_text <- gsub("```\\s*$", "", json_text)
  json_text <- trimws(json_text)
  
  tryCatch({
    results <- fromJSON(json_text)
    
    if (is.data.frame(results) && nrow(results) > 0) {
      results$sinif <- sinif_no
      
      # Lazımi sütunları əlavə etmək
      required_cols <- c("sinif", "mezmun_xetti", "standart_kodu", "standart_metni",
                         "alt_standart_kodu", "alt_standart_metni", "deyisiklik_novu",
                         "kohne_metni", "esaslandirma", "beynelxalq_istinad",
                         "bloom_seviyyesi", "pisa_elaqesi", "pirls_elaqesi",
                         "cefr_seviyyesi", "olke_istinadi")
      
      for (col in required_cols) {
        if (!(col %in% names(results))) {
          results[[col]] <- NA
        }
      }
      
      # Yalnız lazımi sütunları saxlamaq
      results <- results[, required_cols]
      
      # Bazaya yazmaq
      dbWriteTable(con, "yenilenmi_standartlar", results, append = TRUE, row.names = FALSE)
      
      # Statistika
      cat(sprintf("   ✅ %d standart yazıldı:\n", nrow(results)))
      cat(sprintf("      Mövcud (dəyişməz): %d\n", sum(results$deyisiklik_novu == "movcud", na.rm = TRUE)))
      cat(sprintf("      Yenilənmiş:        %d\n", sum(results$deyisiklik_novu == "yenilenib", na.rm = TRUE)))
      cat(sprintf("      Yeni əlavə:        %d\n", sum(results$deyisiklik_novu == "yeni", na.rm = TRUE)))
      cat(sprintf("      Silinmiş:          %d\n", sum(results$deyisiklik_novu == "silinib", na.rm = TRUE)))
      
      return(results)
    } else {
      cat("   ⚠️  Boş nəticə!\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("   ❌ JSON parse xətası:", e$message, "\n")
    cat("   İlk 500 simvol:", substr(json_text, 1, 500), "\n")
    # Cavabı faylda saxlayaq debug üçün
    writeLines(response_text, sprintf("data/debug_sinif_%d.json", sinif_no))
    return(NULL)
  })
}

# --- ƏSAS PROSES ---
cat("2. Bütün siniflərin yenilənməsi başlayır...\n")

# Əvvəlcə köhnə yeniləmələri silək
dbExecute(con, "TRUNCATE TABLE yenilenmi_standartlar RESTART IDENTITY CASCADE")
cat("   Köhnə yeniləmələr silindi\n")

all_results <- list()
for (sinif in 1:11) {
  result <- update_grade(sinif, con)
  all_results[[sinif]] <- result
  
  # API rate limit üçün gözləmə
  Sys.sleep(3)
}

# --- YEKUNİ STATİSTİKA ---
cat("\n\n========== YEKUNİ STATİSTİKA ==========\n")
stats <- dbGetQuery(con, "
  SELECT 
    sinif,
    COUNT(*) as umumi,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'movcud') as movcud,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yenilenib') as yenilenib,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yeni') as yeni,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'silinib') as silinib
  FROM yenilenmi_standartlar
  GROUP BY sinif
  ORDER BY sinif
")

if (nrow(stats) > 0) {
  for (i in 1:nrow(stats)) {
    cat(sprintf("Sinif %2d: Ümumi=%d | Mövcud=%d | Yenilib=%d | Yeni=%d | Silinib=%d\n",
                stats$sinif[i], stats$umumi[i], stats$movcud[i], 
                stats$yenilenib[i], stats$yeni[i], stats$silinib[i]))
  }
  
  cat(sprintf("\nCƏMİ: %d standart işlənib\n", sum(stats$umumi)))
}

# CSV yedəkləmə
all_updated <- dbGetQuery(con, "SELECT * FROM yenilenmi_standartlar ORDER BY sinif, mezmun_xetti, standart_kodu")
write.csv(all_updated, "data/yenilenmi_standartlar_backup.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nYedək: data/yenilenmi_standartlar_backup.csv\n")

dbDisconnect(con)
cat("\n=== Standartların yenilənməsi tamamlandı! ===\n")
cat("Növbəti addım: Rscript scripts/03_generate_html.R\n")
