# Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/main?urlpath=shiny)

Bu layihə Azərbaycan Respublikasında 1-11-ci siniflər üzrə **Azərbaycan dili** fənninin mövcud standart və alt standartlarını beynəlxalq tələblərə uyğun yeniləmək, təkmilləşdirmək və yeni standartlar əlavə etmək üçün nəzərdə tutulub.

---

## Layihənin Məqsədi

Azərbaycan dili kurikulumunun mövcud standartları aşağıdakı beynəlxalq çərçəvələr əsasında təhlil edilir, müqayisə olunur və yenilənir:

- **PISA** — Oxu savadlılığı (məlumat əldə etmə, interpretasiya, qiymətləndirmə)
- **PIRLS** — Oxu bacarıqları (faktoloji anlama, nəticə çıxarma, inteqrasiya)
- **CEFR** — Avropa Dil Çərçəvəsi (A1-C1 səviyyələri)
- **Bloom Taksonomiyası** — Xatırlama → Anlama → Tətbiq → Analiz → Qiymətləndirmə → Yaratma

Həmçinin **6 aparıcı ölkənin** ana dili standartları ilə müqayisə aparılır:

| Ölkə | Xüsusi yanaşma |
|------|---------------|
| Finlandiya | Fənlərarası oxu, çoxsaylı mətnlər, media savadlılığı |
| Sinqapur | STELLAR proqramı, kommunikativ yanaşma, kritik düşüncə |
| Estoniya | Rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya |
| Yaponiya | Dərin oxu, estetik duyum, yaradıcı ifadə |
| Kanada (Ontario) | Diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər |
| İrlandiya | İkidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma |

---

## Mövcud Standartlar Bazası

Layihədə 1-11-ci siniflər üzrə **442 alt standart** mövcuddur:

| Sinif | Standart | Alt standart |
|-------|----------|-------------|
| 1-ci sinif | 14 | 37 |
| 2-ci sinif | 14 | 39 |
| 3-cü sinif | 14 | 40 |
| 4-cü sinif | 14 | 44 |
| 5-ci sinif | 13 | 43 |
| 6-cı sinif | 13 | 44 |
| 7-ci sinif | 13 | 40 |
| 8-ci sinif | 13 | 40 |
| 9-cu sinif | 13 | 43 |
| 10-cu sinif | 13 | 37 |
| 11-ci sinif | 13 | 35 |
| **YEKUN** | **147** | **442** |

**Məzmun xətləri:**
- Dinləmə və danışma (105 alt standart)
- Oxu (120 alt standart)
- Yazı (70 alt standart)
- Dil qaydaları (147 alt standart)

**CEFR uyğunluğu:**
- A1-A2 — 1-4-cü siniflər
- B1-B2 — 5-9-cu siniflər
- C1 — 10-11-ci siniflər

---

## Texniki Stek

| Komponent | Texnologiya |
|-----------|-------------|
| Verilənlər bazası | PostgreSQL |
| Backend/Analiz | R, Claude API |
| Dashboard | R Shiny (shinydashboard) |
| Hesabatlar | HTML5 (statik) |
| AI Model | Claude Sonnet 4 |
| Vizuallaşdırma | Plotly, DT |
| Hosting | Binder (onlayn demo) |

---

## Dashboard Funksiyaları

### Ana Səhifə
- Ümumi standart sayı, mövcud, yenilənmiş və yeni standartların statistikası
- Siniflər üzrə interaktiv bar chart (Plotly)

### Standartlar
- Seçilmiş sinif və məzmun xətti üzrə cədvəl
- Rəng kodlaması: mövcud (mavi), yenilənib (yaşıl), yeni (narıncı), silinib (qırmızı)

### Müqayisə
- Köhnə və yeni standartların yan-yana müqayisəsi

### Beynəlxalq
- PISA/PIRLS/CEFR uyğunluq cədvəli
- Ölkə standartları (6 ölkə üzrə filtrlə)
- Bloom taksonomiyası paylanması

### Statistika
- Dəyişiklik növləri üzrə pie chart
- Məzmun xətləri üzrə bar chart
- CEFR və Bloom səviyyələri üzrə qrafiklər

### Claude AI Təhlil
- Seçilmiş sinif üçün Claude AI ilə ətraflı standart təhlili
- **Canlı taymer** — AI işləyərkən yaşıl nöqtə ilə animasiyalı geri sayma
- **Token izləmə** — Giriş/Çıxış tokenləri və cəmi
- **Xərc hesabı** — Təxmini qiymət ($3/1M giriş + $15/1M çıxış)
- **Statistika paneli** — vaxt, token, xərc grid formatında
- Nəticə avtomatik `html_reports/` qovluğuna yazılır

### HTML Export
- Seçilmiş sinif üçün HTML hesabat yükləmə
- CSV formatında data export

---

## Quraşdırma

### Tələblər
- R (>= 4.2)
- PostgreSQL (>= 14)
- R paketləri: shiny, shinydashboard, DBI, RPostgres, DT, httr, jsonlite, plotly

### Addım 1: Repo-nu klonlayın
```bash
git clone https://github.com/Ttariyel-1954/Az_dili_standartlar.git
cd Az_dili_standartlar
```

### Addım 2: .env faylını yaradın
```bash
cp .env.example .env
# .env faylında ANTHROPIC_API_KEY dəyərini daxil edin
```

### Addım 3: PostgreSQL bazasını yaradın
```bash
psql -U postgres -f sql/create_tables.sql
```

### Addım 4: Standartları bazaya import edin
```bash
Rscript scripts/01_db_export.R
```

### Addım 5: Standartları yeniləyin (Claude API ilə)
```bash
Rscript scripts/02_update_standards.R
```

### Addım 6: HTML hesabatları yaradın
```bash
Rscript scripts/03_generate_html.R
```

### Addım 7: Dashboard-u işə salın
```bash
Rscript -e "shiny::runApp('app.R', port=4567)"
```

Brauzer: http://127.0.0.1:4567

---

## Onlayn Demo (Binder)

Dashboard-u heç nə quraşdırmadan brauzerdə yoxlaya bilərsiniz:

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/main?urlpath=shiny)

> Binder-də PostgreSQL olmadığı üçün məlumatlar CSV faylından oxunur. Claude AI təhlil funksiyası tam işləyir.

---

## Layihə Strukturu

```
Az_dili_standartlar/
├── app.R                    # R Shiny dashboard (əsas fayl)
├── CLAUDE.md                # Layihə təlimatları
├── .env                     # Konfiqurasiya (git-ə daxil deyil)
├── .gitignore               # Git ignore qaydaları
├── runtime.txt              # Binder R versiyası
├── install.R                # Binder paket quraşdırması
├── setup.sh                 # Quraşdırma skripti
├── data/
│   ├── movcud_standartlar_backup.csv    # 442 standart (yedək)
│   └── yenilenmi_standartlar_backup.csv # Yenilənmiş standartlar
├── html_reports/
│   └── sinif_N_tehlil.html  # AI təhlil nəticələri
├── scripts/
│   ├── 01_db_export.R       # Bazadan standart köçürmə
│   ├── 02_update_standards.R # Claude API ilə standart yeniləmə
│   └── 03_generate_html.R   # HTML hesabat generasiyası
├── sql/
│   └── create_tables.sql    # PostgreSQL baza sxemi
└── www/                     # Statik resurslar
```

---

## Verilənlər Bazası Sxemi

### movcud_standartlar
Azərbaycan dili kurikulumunun rəsmi standartları (2024).

| Sütun | Tip | Təsvir |
|-------|-----|--------|
| sinif | INTEGER | Sinif (1-11) |
| mezmun_xetti | VARCHAR | Dinləmə və danışma / Oxu / Yazı / Dil qaydaları |
| standart_kodu | VARCHAR | Standart kodu (məs: 1.1, 2.3) |
| standart_metni | TEXT | Standartın tam mətni |
| alt_standart_kodu | VARCHAR | Alt standart kodu (məs: 1-1.1.1) |
| alt_standart_metni | TEXT | Alt standartın mətni |
| pisa_seviyyesi | VARCHAR | PISA səviyyəsi (1a, 1b, 2...) |
| pirls_kateqoriya | VARCHAR | PIRLS kateqoriyası |
| bloom_seviyyesi | VARCHAR | Bloom taksonomiyası səviyyəsi |

### yenilenmi_standartlar
Claude AI tərəfindən təhlil olunmuş və yenilənmiş standartlar.

| Sütun | Tip | Təsvir |
|-------|-----|--------|
| deyisiklik_novu | VARCHAR | movcud / yenilenib / yeni / silinib |
| esaslandirma | TEXT | Dəyişikliyin əsaslandırması |
| beynelxalq_istinad | TEXT | PISA/PIRLS/CEFR istinadı |
| bloom_seviyyesi | VARCHAR | Bloom səviyyəsi |
| cefr_seviyyesi | VARCHAR | CEFR səviyyəsi (A1-C1) |

---

## Claude API İstifadəsi

Dashboard-da "Claude AI Təhlil" tab-ında seçilmiş sinif üçün Claude API ətraflı təhlil aparır:

1. Mövcud standartları PISA, PIRLS, CEFR, Bloom prizmasından qiymətləndirir
2. 6 aparıcı ölkənin standartları ilə müqayisə edir
3. Hər standart üçün saxlanmalı/yenilənməli/silinməli qərarı verir
4. Yeni standart təklifləri irəli sürür
5. Nəticəni HTML formatında qaytarır

**Təhlil zamanı görünən:**
- Yaşıl nöqtəli canlı taymer (saniyə sayğacı)
- Giriş/çıxış token sayları
- Təxmini xərc ($)
- Tamamlandıqdan sonra statistika paneli

---

## Rəng Sxemi

| Element | Rəng | Kod |
|---------|------|-----|
| Əsas mavi | Tünd mavi | #0D47A1 |
| Navbar | Orta mavi | #1565C0 |
| Box haşiyə | Mavi | #1976D2 |
| Mövcud standart | Açıq mavi fon | #E3F2FD |
| Yenilənmiş standart | Yaşıl fon | #E8F5E9 |
| Yeni standart | Narıncı fon | #FFF3E0 |
| Silinmiş standart | Qırmızı fon | #FFEBEE |

---

## Onlayn Platformalar

| Platforma | Link |
|-----------|------|
| **Binder** | [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ttariyel-1954/Az_dili_standartlar/main?urlpath=shiny) |
| **ShinyApps.io** | [t01061954.shinyapps.io/Az_dili_standartlar](https://t01061954.shinyapps.io/Az_dili_standartlar/) |
| **GitHub** | [github.com/Ttariyel-1954/Az_dili_standartlar](https://github.com/Ttariyel-1954/Az_dili_standartlar) |

---

## Layihə Rəhbəri

**Talıbov Tariyel İsmayıl oğlu**
Riyaziyyat üzrə fəlsəfə doktoru
Azərbaycan Respublikası Təhsil İnstitutunun direktor müavini

**2026-cı il**

---

## Lisenziya

Bu layihə təhsil məqsədli istifadə üçün nəzərdə tutulub.
