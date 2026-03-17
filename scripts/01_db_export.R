# ============================================================
# 01_db_export.R
# Az_agent bazasından mövcud standartları yeni bazaya köçürmə
# ============================================================

library(DBI)
library(RPostgres)

cat("=== Az_agent bazasından standartların köçürülməsi ===\n\n")

# --- 1. Az_agent bazasına qoşulma ---
cat("1. Az_agent bazasına qoşulma...\n")
con_source <- tryCatch({
  dbConnect(
    RPostgres::Postgres(),
    dbname   = "az_agent",      # <-- Az_agent baza adınızı dəyişin
    host     = "localhost",
    port     = 5432,
    user     = "postgres",
    password = "postgres"       # <-- Parolunuzu dəyişin
  )
}, error = function(e) {
  cat("XƏTA: Az_agent bazasına qoşulmaq mümkün olmadı!\n")
  cat("Baza adını, host, port, user və password yoxlayın.\n")
  cat("Xəta mesajı:", e$message, "\n")
  stop(e)
})
cat("   ✅ Az_agent bazasına qoşuldu\n")

# --- 2. Mövcud cədvəlləri yoxlamaq ---
cat("\n2. Cədvəlləri yoxlama...\n")
tables <- dbListTables(con_source)
cat("   Tapılan cədvəllər:", paste(tables, collapse = ", "), "\n")

# Standartlar cədvəlini tapmaq (adı fərqli ola bilər)
possible_names <- c("az_dili_standartlar", "standartlar", "standards", 
                     "kurikulum", "curriculum", "az_language_standards")
found_table <- intersect(tolower(tables), possible_names)

if (length(found_table) == 0) {
  cat("\n   ⚠️  Standart cədvəl adı tapılmadı. Mövcud cədvəllər:\n")
  for (t in tables) {
    cols <- dbListFields(con_source, t)
    cat(sprintf("   - %s: [%s]\n", t, paste(cols, collapse = ", ")))
  }
  cat("\n   Aşağıdakı dəyişəni cədvəlinizin adına uyğun dəyişin:\n")
  source_table <- "az_dili_standartlar"  # <-- Bunu dəyişin
} else {
  source_table <- found_table[1]
}

cat("   İstifadə olunan cədvəl:", source_table, "\n")

# --- 3. Məlumatları oxumaq ---
cat("\n3. Məlumatlar oxunur...\n")
query <- sprintf("SELECT * FROM %s ORDER BY sinif, standart_kodu", source_table)
data_source <- tryCatch({
  dbGetQuery(con_source, query)
}, error = function(e) {
  cat("XƏTA: Məlumatları oxumaq mümkün olmadı!\n")
  cat("Sorğu:", query, "\n")
  cat("Xəta:", e$message, "\n")
  
  # Alternativ — sütun adlarını yoxlayıb uyğunlaşdırmaq
  cat("\nCədvəl sütunları:\n")
  cols <- dbListFields(con_source, source_table)
  cat(paste(cols, collapse = ", "), "\n")
  stop(e)
})

cat(sprintf("   ✅ %d sətir oxundu\n", nrow(data_source)))
cat("   Sütunlar:", paste(names(data_source), collapse = ", "), "\n")

# Siniflər üzrə statistika
cat("\n   Siniflər üzrə bölgü:\n")
for (s in sort(unique(data_source$sinif))) {
  n <- sum(data_source$sinif == s)
  cat(sprintf("   Sinif %2d: %d standart\n", s, n))
}

# --- 4. CSV faylına yedəkləmə ---
cat("\n4. CSV yedəkləmə...\n")
csv_path <- "data/movcud_standartlar_backup.csv"
write.csv(data_source, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("   ✅ Yedəkləndi: %s\n", csv_path))

# --- 5. Yeni bazaya qoşulma ---
cat("\n5. Yeni az_dili_standartlar bazasına qoşulma...\n")

# Əvvəlcə bazanın mövcudluğunu yoxlayaq
con_default <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "postgres",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# Baza yoxdursa, yaradaq
db_exists <- dbGetQuery(con_default, 
  "SELECT 1 FROM pg_database WHERE datname = 'az_dili_standartlar'")
if (nrow(db_exists) == 0) {
  dbExecute(con_default, "CREATE DATABASE az_dili_standartlar")
  cat("   ✅ az_dili_standartlar bazası yaradıldı\n")
} else {
  cat("   ℹ️  az_dili_standartlar bazası artıq mövcuddur\n")
}
dbDisconnect(con_default)

# Yeni bazaya qoşulmaq
con_target <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "az_dili_standartlar",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# --- 6. Cədvəli yaratmaq və məlumatları köçürmək ---
cat("\n6. Məlumatlar köçürülür...\n")

# Əvvəlcə SQL sxemini icra edək
sql_file <- "sql/create_tables.sql"
if (file.exists(sql_file)) {
  sql_content <- readLines(sql_file, encoding = "UTF-8")
  sql_content <- paste(sql_content, collapse = "\n")
  
  # \\c əmrini çıxaraq (R-dən işləmir)
  sql_content <- gsub("\\\\c az_dili_standartlar;", "", sql_content)
  
  # Hər SQL əmrini ayrıca icra edək
  statements <- unlist(strsplit(sql_content, ";"))
  for (stmt in statements) {
    stmt <- trimws(stmt)
    if (nchar(stmt) > 5 && !grepl("^--", stmt) && !grepl("^GRANT", stmt)) {
      tryCatch({
        dbExecute(con_target, paste0(stmt, ";"))
      }, error = function(e) {
        # Artıq mövcud olanları keçək
        if (!grepl("already exists", e$message)) {
          cat("   ⚠️  SQL xəbərdarlıq:", substr(e$message, 1, 80), "\n")
        }
      })
    }
  }
  cat("   ✅ SQL sxem icra olundu\n")
}

# Sütun adlarını uyğunlaşdırmaq
# (Az_agent bazasında sütun adları fərqli ola bilər)
target_cols <- c("sinif", "mezmun_xetti", "standart_kodu", "standart_metni", 
                 "alt_standart_kodu", "alt_standart_metni")

# Məlumatları uyğunlaşdırmaq
df_import <- data.frame(
  sinif            = data_source$sinif,
  mezmun_xetti     = if ("mezmun_xetti" %in% names(data_source)) data_source$mezmun_xetti 
                      else if ("content_line" %in% names(data_source)) data_source$content_line
                      else "Ümumi",
  standart_kodu    = data_source$standart_kodu,
  standart_metni   = if ("standart_metni" %in% names(data_source)) data_source$standart_metni
                      else if ("standart_adi" %in% names(data_source)) data_source$standart_adi
                      else data_source$standard_text,
  alt_standart_kodu = if ("alt_standart_kodu" %in% names(data_source)) data_source$alt_standart_kodu
                       else NA,
  alt_standart_metni = if ("alt_standart_metni" %in% names(data_source)) data_source$alt_standart_metni
                        else if ("alt_standart_adi" %in% names(data_source)) data_source$alt_standart_adi
                        else NA,
  stringsAsFactors = FALSE
)

# Köhnə məlumatları silmək
dbExecute(con_target, "TRUNCATE TABLE movcud_standartlar RESTART IDENTITY CASCADE")

# Yeni məlumatları yazmaq
dbWriteTable(con_target, "movcud_standartlar", df_import, append = TRUE, row.names = FALSE)
cat(sprintf("   ✅ %d sətir 'movcud_standartlar' cədvəlinə köçürüldü\n", nrow(df_import)))

# --- 7. Yoxlama ---
cat("\n7. Yoxlama...\n")
check <- dbGetQuery(con_target, "SELECT sinif, COUNT(*) as say FROM movcud_standartlar GROUP BY sinif ORDER BY sinif")
cat("   Yeni bazadakı məlumatlar:\n")
for (i in 1:nrow(check)) {
  cat(sprintf("   Sinif %2d: %d standart\n", check$sinif[i], check$say[i]))
}

# --- 8. Bağlamaq ---
dbDisconnect(con_source)
dbDisconnect(con_target)

cat("\n=== Köçürmə uğurla tamamlandı! ===\n")
cat("Yedək fayl: data/movcud_standartlar_backup.csv\n")
cat("Növbəti addım: Rscript scripts/02_update_standards.R\n")
