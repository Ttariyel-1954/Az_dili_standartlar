# Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/HEAD?urlpath=shiny/app.R/)

**Azərbaycan Respublikasında 1-11-ci siniflər üzrə Azərbaycan dili fənninin mövcud standart və alt standartlarını beynəlxalq tələblərə uyğun təhlil edən, yeniləyən və təkmilləşdirən süni intellekt əsaslı R Shiny dashboard.**

---

## Layihənin Məqsədi

Bu layihə Azərbaycan dili fənn kurikulumunun mövcud standartlarını müasir beynəlxalq tələblərə uyğunlaşdırmaq üçün yaradılıb. Layihənin əsas məqsədləri:

- **Mövcud standartların təhlili** — 1-11-ci siniflərdə 442 alt standartın hər birini beynəlxalq çərçəvələr əsasında qiymətləndirmək
- **Zəif standartların yenilənməsi** — müasir tələblərə cavab verməyən standartları Claude AI vasitəsilə yenidən yazmaq
- **Yeni standartların təklifi** — beynəlxalq praktikada olan, lakin Azərbaycan kurikulumunda əksini tapmayan standartları əlavə etmək
- **Şəffaf əsaslandırma** — hər dəyişikliyin beynəlxalq istinad ilə əsaslandırılması
- **Müqayisəli təhlil** — köhnə və yeni standartların yan-yana müqayisəsi

---

## Beynəlxalq Çərçəvələr

Layihədə aşağıdakı beynəlxalq standart və çərçəvələrdən istifadə olunur:

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
Hər standart Bloom taksonomiyasının müvafiq səviyyəsi ilə etiketlənir:
1. **Xatırlama** — faktları, anlayışları yadda saxlama
2. **Anlama** — mənasını izah etmə, nümunə gətirmə
3. **Tətbiq etmə** — öyrənilmişi yeni vəziyyətdə istifadə
4. **Təhlil etmə** — hissələrə ayırma, əlaqələri müəyyən etmə
5. **Qiymətləndirmə** — mühakimə yürütmə, fikir bildirmə
6. **Yaratma** — yeni məhsul ortaya qoyma, sintez etmə

### Aparıcı 6 Ölkənin Təcrübəsi

Azərbaycan dili standartları dünya ölkələrinin ən yaxşı təcrübələri ilə müqayisə olunur:

| Ölkə | Əsas yanaşma |
|------|-------------|
| **Finlandiya** | Fənlərarası oxu, çoxsaylı mətn növləri, media savadlılığı, müstəqil düşüncə |
| **Sinqapur** | STELLAR proqramı, kommunikativ dil öyrənməsi, kritik düşüncə inkişafı |
| **Estoniya** | Rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya, texnoloji inteqrasiya |
| **Yaponiya** | Dərin oxu strategiyası, estetik duyum, yaradıcı ifadə bacarığı |
| **Kanada (Ontario)** | Diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər |
| **İrlandiya** | İkidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma |

---

## 3 Addımlı İş Axını (AI Təhlil)

Layihənin ürəyi olan Claude AI Təhlil tabı 3 addımlı interaktiv iş axını ilə işləyir:

### Addım 1: AI Təhlili
1. İstifadəçi **sinif** (1-11) və **məzmun xətti** (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları) seçir
2. Claude AI seçilmiş standartları PISA, PIRLS, CEFR, Bloom və 6 ölkə təcrübəsi əsasında təhlil edir
3. Hər standart üçün status təyin olunur:
   - 🟢 **QALSIN** — standart beynəlxalq tələblərə uyğundur, dəyişiklik tələb etmir
   - 🟠 **YENİLƏNSİN** — standart təkmilləşdirmə tələb edir
   - 🔴 **SİLİNSİN** — standart artıq lazım deyil (nadir hallarda)
   - 🔵 **YENİ TƏKLİF** — AI-nin təklif etdiyi yeni standartlar
4. Nəticə rəngli kartlarla ekranda göstərilir
5. HTML hesabat avtomatik `html_reports/` qovluğuna yazılır

### Addım 2: İnteraktiv Seçim və Yenidən Yazma
1. Yalnız dəyişiklik tələb edən standartlar yüklənir (qalacaqlar gizlədilir)
2. İstifadəçi hər standart üçün son qərar verir:
   - **Saxla** — dəyişiklikdən imtina (status: mövcud)
   - **Yenilə** — AI yenidən yazsın (status: yenilənsin)
   - **Sil** — standart silinsin (status: silinsin)
3. **"AI ilə yenilə"** düyməsi basıldıqda Claude yalnız seçilmiş standartların yeni mətnini yazır
4. Yenilənmiş standartlar yaşıl rənglə vurğulanır

### Addım 3: Bazaya Saxlama
- Bütün nəticələr PostgreSQL bazasına yazılır
- Statuslar avtomatik yekun dəyərlərinə çevrilir:
  - `yenilensin` → `yenilenib`
  - `silinsin` → `silinib`
  - `qalsin` → `movcud`
- Əvvəlki qeydlər silinir, yerinə yeniləri yazılır

---

## Dashboard Tabları

### 1. Ana Səhifə
- Seçilmiş sinif üzrə statistik göstəricilər (value box)
- Ümumi standart sayı, yenilənmiş, yeni əlavə, silinmiş sayları
- Bütün siniflər üzrə yekun diaqram (Plotly stack bar chart)

### 2. Mövcud Standartlar
- Orijinal kurikulum standartlarının tam cədvəli (442 ədəd)
- Standart kodu, məzmun sahəsi, standart mətni
- Sinif və məzmun sahəsi üzrə filtr

### 3. Müqayisə
- Köhnə və yeni standartların yan-yana müqayisəsi
- Yalnız dəyişiklik olan standartlar göstərilir
- Rəngli vurğulama: yaşıl (yenilənib), narıncı (yeni), qırmızı (silinib)

### 4. Yekun Standartlar
- Tam yekun cədvəl: kod, məzmun sahəsi, status, standart, əsaslandırma
- Rəngli sətir fonu ilə statusların ayrılması
- CSV və Excel formatında ixrac imkanı

### 5. Beynəlxalq
- PISA, PIRLS, CEFR uyğunluq cədvəli
- 6 aparıcı ölkənin standartları ilə müqayisə
- Bloom taksonomiyası paylanması

### 6. Statistika
- Dəyişiklik növləri üzrə dairəvi diaqram
- Məzmun xətləri üzrə sütun diaqramı
- CEFR və Bloom səviyyələri üzrə qrafiklər
- Plotly ilə interaktiv vizuallaşdırma

### 7. Claude AI Təhlil
- 3 addımlı iş axını (yuxarıda ətraflı təsvir olunub)
- Canlı taymer, token istifadəsi izləmə, xərc hesabı
- Hər addımda vizual geri əlaqə

### 8. Export (İxrac)
- **HTML hesabat** — nəfis rəngli cədvəl, çap üçün hazır
- **CSV data** — elektron cədvəl proqramları üçün
- **Word (DOCX)** — redaktə oluna bilən, landscape formatda, rəngli cədvəl

---

## Mövcud Standartlar Bazası

1-11-ci siniflər üzrə cəmi **442 alt standart** mövcuddur:

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
| **Verilənlər bazası** | PostgreSQL (`az_muellim_db`) + CSV yedək |
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

### movcud_standartlar
Azərbaycan dili kurikulumunun rəsmi standartları (442 ədəd):
- `kod` — Standart kodu (məs: Az_dili_I_1.1.1)
- `sinif` — Sinif nömrəsi (1-11)
- `mezmun_xetti` — Məzmun sahəsi (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları)
- `standart_metni` — Əsas standart mətni
- `alt_standart_metni` — Alt standart mətni

### yenilenmi_standartlar
AI tərəfindən təhlil olunmuş və yenilənmiş standartlar:
- `kod` — Standart kodu
- `mezmun_sahesi` — Məzmun sahəsi
- `status` — `movcud` / `yenilenib` / `yeni` / `silinib`
- `standart_metni` — Yekun standart mətni
- `xarakteristika` — Əsaslandırma (beynəlxalq istinad ilə)
- `kohne_standart_metni` — Köhnə mətn (müqayisə üçün)
- `bloom_taksonomiyasi` — Bloom səviyyəsi
- `pisa_elaqesi` — PISA uyğunluğu
- `pirls_elaqesi` — PIRLS uyğunluğu
- `cefr_seviyyesi` — CEFR səviyyəsi
- `cetinlik_seviyyesi` — Çətinlik dərəcəsi

---

## Layihə Strukturu

```
Az_dili_standartlar/
├── app.R                       # Əsas R Shiny dashboard (UI + server)
├── .env                        # API açarları və baza konfiqurasiyası (git-ə daxil deyil)
├── .gitignore                  # Git ignore qaydaları
├── CLAUDE.md                   # Layihə təlimatları (texniki sənəd)
├── README.md                   # Bu fayl
├── install.R                   # R paketlərinin quraşdırılması
├── runtime.txt                 # R versiyası (Binder üçün)
├── apt.txt                     # Sistem paketləri (Binder üçün)
├── setup.sh                    # İlkin quraşdırma skripti
├── data/
│   ├── movcud_standartlar_backup.csv     # 442 standart (CSV yedək)
│   └── yekun_standartlar_backup.csv      # Yekun nəticə yedəyi
├── html_reports/                # AI təhlil nəticələri (HTML hesabatlar)
│   └── sinif_N_mezmun_xetti.html
├── word_reports/                # Yekun standartlar (Word/DOCX)
├── Son_Exsel/                   # Yekun standartlar (Excel formatda)
├── scripts/
│   ├── 01_db_export.R          # Az_agent bazasından standart köçürmə
│   ├── 02_update_standards.R   # Claude API ilə standart yeniləmə (batch)
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

### Quraşdırma Addımları

```bash
# 1. Repo-nu klonlayın
git clone https://github.com/Ttariyel-1954/Az_dili_standartlar.git
cd Az_dili_standartlar

# 2. R paketlərini quraşdırın
Rscript install.R

# 3. .env faylını yaradın və ANTHROPIC_API_KEY daxil edin
cat > .env << 'EOF'
ANTHROPIC_API_KEY=sk-ant-api03-SİZİN_AÇARINIZ
DB_HOST=localhost
DB_PORT=5432
DB_NAME=az_muellim_db
DB_USER=postgres
DB_PASSWORD=sizin_şifrəniz
DEFAULT_AI_MODEL=claude-sonnet-4-20250514
EOF

# 4. PostgreSQL bazasını yaradın
createdb az_muellim_db
psql -U postgres -d az_muellim_db -f sql/create_tables.sql

# 5. Mövcud standartları bazaya import edin
Rscript scripts/01_db_export.R

# 6. Dashboard-u işə salın
Rscript -e "shiny::runApp('app.R', port=4567, host='0.0.0.0')"
```

Brauzer: **http://localhost:4567**

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

## Onlayn Platformalar

| Platforma | Keçid |
|-----------|-------|
| **GitHub** | [github.com/Ttariyel-1954/Az_dili_standartlar](https://github.com/Ttariyel-1954/Az_dili_standartlar) |
| **ShinyApps.io** | [t01061954.shinyapps.io/Az_dili_standartlar](https://t01061954.shinyapps.io/Az_dili_standartlar/) |
| **Binder** | [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/HEAD?urlpath=shiny/app.R/) |

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
