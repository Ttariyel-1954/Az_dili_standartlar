# Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

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

## İş Axını

Layihə iki mərhələli iş axını ilə işləyir:

### Mərhələ 1: Təhlil et və Yaz
- Sidebar-dan sinif seçilir (1-11)
- **"Təhlil et və Yaz"** düyməsi basılır
- Claude AI mövcud standartları PISA, PIRLS, CEFR, Bloom və 6 ölkə standartları əsasında təhlil edir
- Hər standart üçün status təyin olunur:
  - **Dəyişməyib** — standart beynəlxalq tələblərə uyğundur
  - **Yenilənib** (3-5 ədəd) — standart yenidən yazılıb, əsaslandırma ilə
  - **Silinib** — standart artıq lazım deyil (nadir)
- Nəticə birbaşa PostgreSQL bazasına yazılır
- Xarakteristika: dəyişməyib üçün 2-3 cümlə, yenilənib üçün 4-5 cümlə

### Mərhələ 2: Yeni Standartlar Təklif Et
- **"Yeni standartlar təklif et"** düyməsi basılır
- Claude AI boşluqları müəyyən edib 3-5 yeni standart təklif edir
- Yeni standartlar `yeni_yazilmis` statusu ilə yekun cədvələ əlavə olunur
- Hər sinif üçün fərqli say ola bilər (təbii görünsün deyə)

---

## Dashboard Tabları

### Ana Səhifə
- Seçilmiş sinif üzrə value box-lar: Ümumi, Dəyişməyib, Yenilənib, Yeni yazılıb, Silinib
- Bütün siniflər üzrə yekun diaqram (stack bar chart)
- Sidebar-da sinif dəyişdikdə bütün göstəricilər avtomatik yenilənir

### Mövcud Standartlar
- Orijinal, dəyişilməmiş standartların cədvəli
- 3 sütun: Kod (Az_dili_I_1.1.1 formatı), Məzmun sahəsi, Standart
- Sinif və məzmun sahəsi filtri

### Müqayisə
- Yalnız dəyişiklik olan standartları göstərir (dəyişməyib olanlar gizlədilir)
- Köhnə standart (italik, boz) vs Yeni standart + Xarakteristika
- Rəngli sətir: yaşıl (yenilənib), narıncı (yeni yazılıb), qırmızı (silinib)

### Yekun Standartlar
- Tam yekun cədvəl: Kod, Məzmun sahəsi, Status, Standart, Xarakteristika
- Rənglər: ağ (dəyişməyib), yaşıl (yenilənib), narıncı (yeni yazılıb), qırmızı (silinib)
- Sinif və status filtri, CSV/Excel ixrac

### Beynəlxalq
- PISA/PIRLS/CEFR uyğunluq cədvəli
- 6 ölkə standartları (filtrlə)
- Bloom taksonomiyası paylanması

### Statistika
- Dəyişiklik növləri üzrə pie chart
- Məzmun xətləri üzrə bar chart
- CEFR və Bloom səviyyələri üzrə qrafiklər

### Claude AI Təhlil
- "Təhlil et və Yaz" — mövcud standartları təhlil edib bazaya yazır
- "Yeni standartlar təklif et" — boşluqları doldurmaq üçün yeni standartlar əlavə edir
- Canlı taymer, token izləmə, xərc hesabı

### Export
- HTML hesabat yükləmə (nəfis rəngli cədvəl)
- CSV data ixracı
- Word (DOCX) ixracı — redaktə oluna bilən, landscape formatda, rəngli cədvəl

---

## Mövcud Standartlar Bazası

1-11-ci siniflər üzrə **442 alt standart**:

| Sinif | Alt standart | CEFR |
|-------|-------------|------|
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
| **YEKUN** | **442** | |

**Məzmun xətləri:** Dinləmə və danışma, Oxu, Yazı, Dil qaydaları

---

## Texniki Stek

| Komponent | Texnologiya |
|-----------|-------------|
| Verilənlər bazası | PostgreSQL (`az_dili_standartlar`) |
| Backend/Analiz | R, Claude API (Anthropic) |
| Dashboard | R Shiny (shinydashboard) |
| AI Model | Claude Sonnet 4 |
| Vizuallaşdırma | Plotly, DT |
| Word ixrac | officer, flextable |
| Hesabatlar | HTML5, DOCX |
| Port | 5678 |

---

## Verilənlər Bazası

### movcud_standartlar
Azərbaycan dili kurikulumunun rəsmi standartları (442 ədəd).
- `kod` — Standart kodu (Az_dili_I_1.1.1 formatı)
- `sinif` — Sinif (1-11)
- `mezmun_xetti` — Məzmun sahəsi
- `standart_metni`, `alt_standart_metni` — Standart mətni

### yekun_standartlar
AI tərəfindən təhlil olunmuş yekun standartlar.
- `kod` — Standart kodu
- `mezmun_sahesi` — Məzmun sahəsi
- `status` — deyismeyib / yenilenib / yeni_yazilmis / silinib
- `standart_metni` — Yekun standart mətni
- `xarakteristika` — Əsaslandırma (beynəlxalq istinad)
- `kohne_standart_metni` — Köhnə mətn (müqayisə üçün)
- `bloom_taksonomiyasi`, `cetinlik_seviyyesi` — Bloom və çətinlik

---

## Layihə Strukturu

```
Az_dili_standartlar/
├── app.R                    # R Shiny dashboard (əsas fayl)
├── README.md                # Bu fayl
├── CLAUDE.md                # Layihə təlimatları (texniki)
├── .env                     # Konfiqurasiya (git-ə daxil deyil)
├── .gitignore               # Git ignore qaydaları
├── runtime.txt              # R versiyası
├── install.R                # R paket quraşdırması
├── setup.sh                 # Quraşdırma skripti
├── data/
│   └── movcud_standartlar_backup.csv    # 442 standart (yedək)
├── html_reports/             # AI təhlil nəticələri (HTML)
├── word_reports/             # Yekun standartlar (Word/DOCX)
├── scripts/
│   ├── 01_db_export.R       # Bazadan standart köçürmə
│   ├── 02_update_standards.R # Claude API ilə standart yeniləmə
│   └── 03_generate_html.R   # HTML hesabat generasiyası
├── sql/
│   └── create_tables.sql    # PostgreSQL baza sxemi
└── www/                     # Statik resurslar
```

---

## Quraşdırma

### Tələblər
- R (>= 4.2)
- PostgreSQL (>= 14)
- R paketləri: shiny, shinydashboard, DBI, RPostgres, DT, httr, jsonlite, plotly, officer, flextable

### Addımlar

```bash
# 1. Repo-nu klonlayın
git clone https://github.com/Ttariyel-1954/Az_dili_standartlar.git
cd Az_dili_standartlar

# 2. R paketlərini quraşdırın
Rscript install.R

# 3. .env faylını yaradın (ANTHROPIC_API_KEY daxil edin)
cp .env.example .env

# 4. PostgreSQL bazasını yaradın
psql -U postgres -f sql/create_tables.sql

# 5. Standartları bazaya import edin
Rscript scripts/01_db_export.R

# 6. Dashboard-u işə salın
Rscript -e "shiny::runApp('app.R', port=5678, host='0.0.0.0')"
```

Brauzer: http://localhost:5678

---

## Onlayn Platformalar

| Platforma | Link |
|-----------|------|
| **GitHub** | [github.com/Ttariyel-1954/Az_dili_standartlar](https://github.com/Ttariyel-1954/Az_dili_standartlar) |
| **ShinyApps.io** | [t01061954.shinyapps.io/Az_dili_standartlar](https://t01061954.shinyapps.io/Az_dili_standartlar/) |

---

## Layihə Rəhbəri

**Talıbov Tariyel İsmayıl oğlu**
Riyaziyyat üzrə fəlsəfə doktoru
Azərbaycan Respublikası Təhsil İnstitutunun direktor müavini

**2026-cı il**

---

## Lisenziya

Bu layihə təhsil məqsədli istifadə üçün nəzərdə tutulub.
