#!/bin/bash
# ============================================================
# setup.sh — Az_dili_standartlar layihəsinin quraşdırılması
# Mac terminalda icra edin: bash setup.sh
# ============================================================

set -e
echo "🇦🇿 === Azərbaycan Dili Standartları Layihəsi — Quraşdırma ==="
echo ""

# --- 1. Lazımi R paketlərini yükləmək ---
echo "📦 1. R paketlərinin quraşdırılması..."
Rscript -e '
pkgs <- c("shiny", "shinydashboard", "DBI", "RPostgres", "DT", "httr", "jsonlite", "plotly")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new_pkgs) > 0) {
  cat("Yüklənir:", paste(new_pkgs, collapse=", "), "\n")
  install.packages(new_pkgs, repos="https://cran.r-project.org")
} else {
  cat("Bütün paketlər artıq quraşdırılıb ✅\n")
}
'
echo ""

# --- 2. PostgreSQL yoxlaması ---
echo "🐘 2. PostgreSQL yoxlaması..."
if command -v psql &> /dev/null; then
    echo "   PostgreSQL tapıldı ✅"
    PSQL_VERSION=$(psql --version)
    echo "   Versiya: $PSQL_VERSION"
else
    echo "   ❌ PostgreSQL tapılmadı!"
    echo "   Quraşdırmaq üçün: brew install postgresql@16"
    echo "   Sonra: brew services start postgresql@16"
    exit 1
fi

# PostgreSQL servisini yoxlamaq
if pg_isready &> /dev/null; then
    echo "   PostgreSQL servisi işləyir ✅"
else
    echo "   ⚠️  PostgreSQL servisi işləmir. Başladılır..."
    brew services start postgresql@16 2>/dev/null || brew services start postgresql 2>/dev/null
    sleep 2
fi
echo ""

# --- 3. Bazanı yaratmaq ---
echo "🗄️  3. Baza yaradılır..."
psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = 'az_dili_standartlar'" | grep -q 1 || \
    psql -U postgres -c "CREATE DATABASE az_dili_standartlar;"
echo "   az_dili_standartlar bazası hazırdır ✅"
echo ""

# --- 4. Cədvəlləri yaratmaq ---
echo "📋 4. Cədvəllər yaradılır..."
psql -U postgres -d az_dili_standartlar -f sql/create_tables.sql 2>/dev/null
echo "   Cədvəllər yaradıldı ✅"
echo ""

# --- 5. Qovluq strukturu ---
echo "📁 5. Qovluq strukturu..."
mkdir -p data html_reports www scripts sql
echo "   Qovluqlar hazırdır ✅"
echo ""

# --- 6. ANTHROPIC_API_KEY ---
echo "🔑 6. Claude API açarı..."
if [ -z "$ANTHROPIC_API_KEY" ]; then
    echo "   ⚠️  ANTHROPIC_API_KEY təyin edilməyib!"
    echo "   Əlavə etmək üçün:"
    echo "   echo 'export ANTHROPIC_API_KEY=\"sk-ant-...\"' >> ~/.zshrc"
    echo "   source ~/.zshrc"
else
    echo "   API açarı tapıldı ✅"
fi
echo ""

echo "=========================================="
echo "✅ Quraşdırma tamamlandı!"
echo ""
echo "NÖVBƏTİ ADDIMLAR:"
echo "  1. Standartları köçürmək:   Rscript scripts/01_db_export.R"
echo "  2. AI ilə yeniləmək:        Rscript scripts/02_update_standards.R"
echo "  3. HTML yaratmaq:           Rscript scripts/03_generate_html.R"
echo "  4. Dashboard işə salmaq:    Rscript -e \"shiny::runApp('app.R', port=4567)\""
echo "  5. HTML açmaq:              open html_reports/index.html"
echo "=========================================="
