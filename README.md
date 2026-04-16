# Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/HEAD?urlpath=shiny/app.R/)
[![R](https://img.shields.io/badge/R-4.2+-blue.svg)](https://www.r-project.org/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-14+-336791.svg)](https://www.postgresql.org/)
[![Claude](https://img.shields.io/badge/AI-Claude%20Sonnet%204-purple.svg)](https://www.anthropic.com/)

**Azərbaycan Respublikasında 1-11-ci siniflər üzrə Azərbaycan dili fənninin mövcud 442 alt standartını beynəlxalq tələblərə uyğun təhlil edən, yeniləyən və təkmilləşdirən süni intellekt əsaslı R Shiny dashboard.**

---

## Mündəricat

- [Layihənin Məqsədi](#layihənin-məqsədi)
- [Beynəlxalq Çərçəvələr](#beynəlxalq-çərçəvələr)
- [3 Addımlı AI İş Axını](#3-addımlı-ai-iş-axını)
- [Dashboard Tabları](#dashboard-tabları)
- [Mövcud Standartlar Bazası](#mövcud-standartlar-bazası)
- [Texniki Stek](#texniki-stek)
- [Verilənlər Bazası Strukturu](#verilənlər-bazası-strukturu)
- [Layihə Strukturu](#layihə-strukturu)
- [Quraşdırma](#quraşdırma)
- [CSV Backup-dan Bərpa Etmə](#csv-backup-dan-bərpa-etmə)
- [İstifadə Qaydası](#istifadə-qaydası)
- [Təhlükəsizlik](#təhlükəsizlik)
- [Onlayn Platformalar](#onlayn-platformalar)
- [Layihə Rəhbəri](#layihə-rəhbəri)

---

## Layihənin Məqsədi

Bu layihə Azərbaycan dili fənn kurikulumunun mövcud standartlarını müasir beynəlxalq tələblərə uyğunlaşdırmaq üçün yaradılıb. Əsas məqsədlər:

- **Mövcud standartların təhlili** — 1-11-ci siniflərdə 442 alt standartın hər birini beynəlxalq çərçəvələr əsasında qiymətləndirmək
- **Zəif standartların yenilənməsi** — müasir tələblərə cavab verməyən standartları Claude AI vasitəsilə yenidən yazmaq
- **Yeni standartların təklifi** — beynəlxalq praktikada olan, lakin Azərbaycan kurikulumunda əksini tapmayan standartları əlavə etmək
- **Şəffaf əsaslandırma** — hər dəyişikliyin beynəlxalq istinad ilə əsaslandırılması
- **Müqayisəli təhlil** — köhnə və yeni standartların yan-yana müqayisəsi

---

## Beynəlxalq Çərçəvələr

### PISA — Oxu Savadlılığı (Beynəlxalq Şagird Qiymətləndirmə Proqramı)
- **Məlumat əldə etmə və axtarış** — mətndə konkret informasiyanın tapılması
- **İnteqrasiya və interpretasiya** — mətnin mənasını başa düşmə, nəticə çıxarma
- **Əks etdirmə və qiymətləndirmə** — mətn haqqında tənqidi fikir yürütmə
- **PISA 2025 yenilikləri:** rəqəmsal oxu, çoxsaylı mənbələrdən istifadə, kritik qiymətləndirmə
- **6 səviyyə:** 1b → 1a → 2 → 3 → 4 → 5 → 6 (ən yüksək)

### PIRLS — Beynəlxalq Oxu Savadlılığı Tədqiqatı
- Aydın ifadə olunmuş məlumatın tapılması
- Birbaşa nəticə çıxarma
- İnterpretasiya və ideyaların inteqrasiyası
- Məzmun və formanın qiymətləndirilməsi
- **PIRLS 2026:** rəqəmsal mətnlər, onlayn oxu strategiyaları

### CEFR — Avropa Dil Çərçəvəsi (A1-C1)

| Səviyyə | Siniflər | Təsvir |
|---------|----------|--------|
| A1 | 1-2-ci sinif | İbtidai istifadəçi — əsas ifadələr, sadə cümlələr |
| A2 | 3-4-cü sinif | İbtidai istifadəçi — gündəlik mövzularda ünsiyyət |
| B1 | 5-7-ci sinif | Müstəqil istifadəçi — əsas fikirləri başa düşmə |
| B2 | 8-9-cu sinif | Müstəqil istifadəçi — mürəkkəb mətnlərin başa düşülməsi |
| C1 | 10-11-ci sinif | Təcrübəli istifadəçi — geniş, mürəkkəb mətnlərin dərki |

### Bloom Taksonomiyası (Yenilənmiş)
1. **Xatırlama** — faktları, anlayışları yadda saxlama
2. **Anlama** — mənasını izah etmə, nümunə gətirmə
3. **Tətbiq etmə** — öyrənilmişi yeni vəziyyətdə istifadə
4. **Təhlil etmə** — hissələrə ayırma, əlaqələri müəyyən etmə
5. **Qiymətləndirmə** — mühakimə yürütmə, fikir bildirmə
6. **Yaratma** — yeni məhsul ortaya qoyma, sintez etmə

### Aparıcı 6 Ölkənin Təcrübəsi

| Ölkə | Əsas yanaşma |
|------|--------------|
| **Finlandiya** | Fənlərarası oxu, çoxsaylı mətn növləri, media savadlılığı, müstəqil düşüncə |
| **Sinqapur** | STELLAR proqramı, kommunikativ dil öyrənməsi, kritik düşüncə inkişafı |
| **Estoniya** | Rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya, texnoloji inteqrasiya |
| **Yaponiya** | Dərin oxu strategiyası, estetik duyum, yaradıcı ifadə bacarığı |
| **Kanada (Ontario)** | Diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər |
| **İrlandiya** | İkidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma |

---

## 3 Addımlı AI İş Axını

### Addım 1: AI Təhlili
1. İstifadəçi **sinif** (1-11) və **məzmun xətti** (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları) seçir
2. Claude AI seçilmiş standartları PISA, PIRLS, CEFR, Bloom və 6 ölkə təcrübəsi əsasında təhlil edir
3. Hər standart üçün status təyin olunur:
   - 🟢 **QALSIN** — standart beynəlxalq tələblərə uyğundur
   - 🟠 **YENİLƏNSİN** — standart təkmilləşdirmə tələb edir
   - 🔴 **SİLİNSİN** — standart artıq lazım deyil
   - 🔵 **YENİ TƏKLİF** — AI-nin təklif etdiyi yeni standartlar
4. HTML hesabat avtomatik `html_reports/` qovluğuna yazılır

### Addım 2: İnteraktiv Seçim və Yenidən Yazma
1. Yalnız dəyişiklik tələb edən standartlar yüklənir
2. İstifadəçi hər standart üçün son qərar verir: **Saxla / Yenilə / Sil**
3. **"AI ilə yenilə"** düyməsi ilə Claude seçilmiş standartların yeni mətnini yazır

### Addım 3: Bazaya Saxlama
- Bütün nəticələr PostgreSQL bazasına yazılır
- Statuslar avtomatik yekun dəyərlərinə çevrilir

---

## Dashboard Tabları

| Tab | Məzmun |
|-----|--------|
| **1. Ana Səhifə** | Value box-lar, ümumi statistika, diaqram |
| **2. Mövcud Standartlar** | Orijinal 442 standartın cədvəli |
| **3. Müqayisə** | Köhnə vs yeni standartların yan-yana müqayisəsi |
| **4. Yekun Standartlar** | Tam yekun cədvəl + rəngli statuslar |
| **5. Beynəlxalq** | PISA/PIRLS/CEFR + 6 ölkə standartları |
| **6. Statistika** | Plotly ilə interaktiv diaqramlar |
| **7. Claude AI Təhlil** | 3 addımlı iş axını + taymer + token istifadəsi |
| **8. Export** | HTML, CSV və Word (DOCX) ixracı |

---

## Mövcud Standartlar Bazası

| Sinif | Alt standart sayı | CEFR səviyyəsi |
|-------|-------------------|----------------|
| 1-ci sinif | 37 | A1 |
| 2-ci sinif | 39 | A1 |
| 3-cü sinif | 40 | A2 |
| 4-cü sinif | 44 | A2 |
| 5-ci sinif | 43 | B1 |
| 6-cı sinif | 44 | B1 |
| 7-ci sinif | 40 | B1 |
| 8-ci sinif | 40 | B2 |
| 9-cu sinif | 43 | B2 |
| 10-cu sinif | 37 | C1 |
| 11-ci sinif | 35 | C1 |
| **Yekun** | **442** | **A1 — C1** |

**Məzmun xətləri:** Dinləmə və danışma, Oxu, Yazı, Dil qaydaları

---

## Texniki Stek

| Komponent | Texnologiya |
|-----------|-------------|
| **Proqramlaşdırma dili** | R (>= 4.2) |
| **Verilənlər bazası** | PostgreSQL (`az_dili_standartlar`) |
| **Veb interfeys** | R Shiny + shinydashboard |
| **Süni intellekt** | Claude API (Anthropic) — Claude Sonnet 4 |
| **İnteraktiv cədvəl** | DT (DataTables) |
| **Vizuallaşdırma** | Plotly (interaktiv diaqramlar) |
| **Word ixracı** | officer + flextable paketləri |
| **HTTP sorğular** | httr paketi |
| **JSON emal** | jsonlite paketi |
| **Port** | 4567 |

---

## Verilənlər Bazası Strukturu

Baza adı: `az_dili_standartlar`

### 5 Cədvəl + 1 View

#### 1. `movcud_standartlar` (442 sətir)
Azərbaycan dili kurikulumunun rəsmi standartları.

**Əsas sütunlar:** `id`, `sinif`, `mezmun_xetti`, `standart_kodu`, `standart_metni`, `alt_standart_kodu`, `alt_standart_metni`, `pisa_seviyyesi`, `pirls_kateqoriya`, `bloom_seviyyesi`, `kod`

#### 2. `yenilenmi_standartlar` (100 sətir)
AI tərəfindən təhlil olunmuş və yenilənmiş standartlar.

**Əsas sütunlar:** `deyisiklik_novu` (movcud/yenilenib/yeni/silinib), `kohne_metni`, `esaslandirma`, `beynelxalq_istinad`, `cefr_seviyyesi`, `olke_istinadi`, `versiya`, `status`

#### 3. `yekun_standartlar`
AI təhlilindən sonra final yekun cədvəl.

**Əsas sütunlar:** `sinif`, `kod`, `mezmun_sahesi`, `bloom_taksonomiyasi`, `cetinlik_seviyyesi`, `status` (deyismeyib/yenilenib/yeni/silinib), `standart_metni`, `xarakteristika`, `kohne_standart_metni`

#### 4. `beynelxalq_cerceveler`
PISA, PIRLS, CEFR kateqoriyaları və təsvirləri.

#### 5. `olke_standartlari`
6 aparıcı ölkənin dil təhsili standartları.

#### 6. `deyisiklik_jurnali`
AI təhlili və dəyişikliklərin audit tarixçəsi.

---

## Layihə Strukturu

```
Az_dili_standartlar/
├── app.R                       # Əsas R Shiny dashboard (UI + server)
├── .env                        # API açarları və baza konfiqurasiyası (git-ə daxil deyil)
├── .Renviron                   # R environment dəyişənləri
├── .gitignore                  # Git ignore qaydaları
├── CLAUDE.md                   # Layihə təlimatları (texniki sənəd)
├── README.md                   # Bu fayl
├── TELIMAT.html                # İstifadəçi təlimatı (HTML)
├── install.R                   # R paketlərinin quraşdırılması
├── runtime.txt                 # R versiyası (Binder üçün)
├── apt.txt                     # Sistem paketləri (Binder üçün)
├── setup.sh                    # İlkin quraşdırma skripti
├── data/
│   ├── movcud_standartlar_backup.csv     # 442 standart (CSV yedək)
│   ├── yenilenmi_standartlar_backup.csv  # 100 yenilənmiş standart
│   └── yekun_standartlar_backup.csv      # Yekun nəticə yedəyi
├── html_reports/               # AI təhlil nəticələri (HTML)
├── word_reports/               # Yekun standartlar (Word/DOCX)
├── Son_Exsel/                  # Yekun standartlar (Excel)
├── scripts/
│   ├── 01_db_export.R          # Az_agent bazasından standart köçürmə
│   ├── 02_update_standards.R   # Claude API ilə standart yeniləmə
│   └── 03_generate_html.R      # HTML hesabat generasiyası
└── sql/
    └── create_tables.sql       # PostgreSQL baza cədvəlləri sxemi
```

---

## Quraşdırma

### Tələblər
- **R** >= 4.2
- **PostgreSQL** >= 14
- **Anthropic API açarı** (Claude API)
- R paketləri: `shiny`, `shinydashboard`, `DBI`, `RPostgres`, `DT`, `httr`, `jsonlite`, `plotly`, `officer`, `flextable`

### Yeni quraşdırma (sıfırdan)

```bash
# 1. Repo-nu klonlayın
git clone https://github.com/Ttariyel-1954/Az_dili_standartlar.git
cd Az_dili_standartlar

# 2. R paketlərini quraşdırın
Rscript install.R

# 3. .env faylını yaradın
cat > .env << 'EOF'
DB_HOST=localhost
DB_PORT=5432
DB_NAME=az_dili_standartlar
DB_USER=sizin_user_adiniz
DB_PASSWORD=
ANTHROPIC_API_KEY=sk-ant-api03-SİZİN_AÇARINIZ
DEFAULT_AI_MODEL=claude-sonnet-4-20250514
EOF

# 4. PostgreSQL bazasını yaradın
createdb az_dili_standartlar
psql -d az_dili_standartlar -f sql/create_tables.sql

# 5. Dashboard-u işə salın
Rscript -e "shiny::runApp('app.R', port=4567, host='0.0.0.0')"
```

Brauzer: **http://localhost:4567**

---

## CSV Backup-dan Bərpa Etmə

Əgər lokal baza itibsə və yalnız CSV yedəklər qalıbsa, bərpa prosesi:

### Addım 1: Bazanı və cədvəlləri yarat

```bash
cd ~/projects/standards/Az_dili_standartlar

# Bazanı yarat
createdb az_dili_standartlar

# Cədvəlləri yarat
psql -d az_dili_standartlar -f sql/create_tables.sql
```

### Addım 2: `movcud_standartlar` cədvəlini hazırlamaq

Əlavə sütunlar əlavə et (CSV-dəki 4 əlavə sütun üçün):

```sql
ALTER TABLE movcud_standartlar
  ADD COLUMN pisa_seviyyesi VARCHAR(10),
  ADD COLUMN pirls_kateqoriya VARCHAR(100),
  ADD COLUMN bloom_seviyyesi VARCHAR(50),
  ADD COLUMN kod VARCHAR(50);
```

### Addım 3: CSV-ləri yüklə

```bash
# 442 mövcud standart
psql -d az_dili_standartlar -c "\copy movcud_standartlar FROM 'data/movcud_standartlar_backup.csv' WITH (FORMAT csv, HEADER true);"

# 100 yenilənmiş standart
psql -d az_dili_standartlar -c "\copy yenilenmi_standartlar FROM 'data/yenilenmi_standartlar_backup.csv' WITH (FORMAT csv, HEADER true, NULL 'NA');"
```

### Addım 4: `yekun_standartlar` cədvəlini yarat

App bu cədvəli **view** kimi yox, **cədvəl** kimi istifadə edir (INSERT əməliyyatı aparır):

```sql
CREATE TABLE yekun_standartlar (
  id SERIAL PRIMARY KEY,
  sinif INTEGER NOT NULL,
  kod VARCHAR(50),
  mezmun_sahesi VARCHAR(100),
  bloom_taksonomiyasi VARCHAR(50),
  cetinlik_seviyyesi VARCHAR(20),
  status VARCHAR(30),
  standart_metni TEXT,
  xarakteristika TEXT,
  kohne_standart_metni TEXT,
  yaradilma_tarixi TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_yekun_sinif ON yekun_standartlar(sinif);
CREATE INDEX idx_yekun_status ON yekun_standartlar(status);
```

### Addım 5: `yekun_standartlar`-ı doldur

```bash
psql -d az_dili_standartlar -c "\copy yekun_standartlar FROM 'data/yekun_standartlar_backup.csv' WITH (FORMAT csv, HEADER true, NULL 'NA');"
```

### Addım 6: Yoxla

```bash
psql -d az_dili_standartlar -c "
  SELECT 'movcud' AS cedvel, COUNT(*) FROM movcud_standartlar UNION ALL
  SELECT 'yenilenmi', COUNT(*) FROM yenilenmi_standartlar UNION ALL
  SELECT 'yekun', COUNT(*) FROM yekun_standartlar;
"
```

Gözlənilən nəticə: 442 / 100 / (yekun sayı).

---

## İstifadə Qaydası

1. Dashboard-u açın → sol tərəfdə menyudan **"Claude AI Təhlil"** tabını seçin
2. **Sinif** (1-11) və **Məzmun xətti** (Dinləmə, Oxu, Yazı, və s.) seçin
3. **"Addım 1: Təhlil et"** düyməsini basın — Claude AI mövcud standartları təhlil edəcək
4. Nəticələri nəzərdən keçirin — hər standart üçün rəngli kart görünəcək
5. **"Addım 2: Yüklə"** düyməsini basın — dəyişiklik tələb edən standartlar interaktiv cədvəldə görünəcək
6. Hər standart üçün son qərarınızı verin (Saxla / Yenilə / Sil)
7. **"AI ilə yenilə"** düyməsini basın — Claude seçilmiş standartların yeni mətnini yazacaq
8. **"Addım 3: Bazaya yaz"** düyməsini basın — nəticələr PostgreSQL bazasına yazılacaq
9. **Export** tabından HTML, CSV və ya Word formatında hesabat yükləyin

---

## Təhlükəsizlik

### API açarları
- **Heç vaxt** `.env` və ya `.Renviron` fayllarını git-ə commit etməyin
- `.gitignore` faylı bu faylları avtomatik istisna edir
- API açarı sızırsa, dərhal [console.anthropic.com](https://console.anthropic.com) saytından **revoke** edin və yenisini yaradın

### Baza şifrələri
- PostgreSQL şifrəsini güclü saxlayın
- Lokal development-də `trust` autentifikasiya məqbuldur, production-da `md5` və ya `scram-sha-256` istifadə edin

### Backup
- CSV backup faylları (`data/` qovluğunda) **dəyərlidir** — onları git-də saxlayın
- Hər AI təhlilindən sonra CSV ixrac edin: `Export` → CSV

---

## Onlayn Platformalar

| Platforma | Keçid |
|-----------|-------|
| **GitHub** | [github.com/Ttariyel-1954/Az_dili_standartlar](https://github.com/Ttariyel-1954/Az_dili_standartlar) |
| **ShinyApps.io** | [t01061954.shinyapps.io/Az_dili_standartlar](https://t01061954.shinyapps.io/Az_dili_standartlar/) |
| **Binder** | [mybinder.org](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/HEAD?urlpath=shiny/app.R/) |

---

## Nəticə

Bu layihə Azərbaycan dilinin tədrisində kurikulum standartlarının beynəlxalq səviyyədə qiymətləndirilməsi və təkmilləşdirilməsi üçün güclü alətdir. Claude AI-nin analitik gücünü R Shiny-nin interaktiv interfeysi ilə birləşdirərək, mütəxəssislərə standartların yenilənməsi prosesində əhəmiyyətli dərəcədə yardım göstərir. Hər bir dəyişiklik beynəlxalq istinadlarla əsaslandırıldığı üçün nəticələr şəffaf və etibarlıdır.

---

## Layihə Rəhbəri

**Talıbov Tariyel İsmayıl oğlu**
Riyaziyyat üzrə fəlsəfə doktoru
Azərbaycan Respublikası Təhsil İnstitutunun direktor müavini

**ARTI — 2026**

---

## Lisenziya

Bu layihə təhsil məqsədli istifadə üçün nəzərdə tutulub.
Azərbaycan Respublikası Elm və Təhsil Nazirliyi — ARTI
