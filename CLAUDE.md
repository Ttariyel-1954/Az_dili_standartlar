# Az_dili_standartlar — Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

## LAYİHƏNİN MƏQSƏDİ
Bu layihə Azərbaycan Respublikasında 1-11-ci siniflər üzrə Azərbaycan dili fənninin mövcud standart və alt standartlarını beynəlxalq tələblərə uyğun yeniləmək, təkmilləşdirmək və yeni standartlar əlavə etmək üçün nəzərdə tutulub. Layihə R Shiny dashboard vasitəsilə 3 addımlı iş axını ilə işləyir: AI təhlil → İnteraktiv seçim → PostgreSQL-ə saxlama.

## TEXNİKİ STEK
- **Verilənlər bazası**: PostgreSQL (`az_muellim_db` bazası, `movcud_standartlar` və `yenilenmi_standartlar` cədvəlləri)
- **Backend/Analiz**: R, Claude API (Anthropic)
- **Frontend**: R Shiny (shinydashboard), HTML5 (statik hesabatlar)
- **Əsas paketlər**: shiny, shinydashboard, DT, httr, jsonlite, plotly, RPostgres
- **Dil**: Azərbaycan dili (bütün interfeys və məzmun)
- **Port**: 4567

## LAYİHƏ STRUKTURU
```
Az_dili_standartlar/
├── app.R                    # Əsas Shiny dashboard (bütün UI + server)
├── .env                     # API açarları və baza konfiqurasiyası
├── CLAUDE.md                # Layihə təlimatı
├── TELIMAT.html             # Layihə təlimatı (HTML formatda)
├── install.R                # R paketlərinin quraşdırılması
├── runtime.txt              # R versiyası
├── setup.sh                 # İlkin quraşdırma skripti
├── data/                    # Statik data faylları
├── html_reports/            # AI təhlil nəticələrinin HTML hesabatları
│   └── sinif_N_*.html       # Hər sinif/məzmun xətti üçün hesabat
├── scripts/
│   ├── 01_db_export.R       # Az_agent bazasından məlumat köçürmə
│   ├── 02_update_standards.R # Standartları yeniləmə (batch)
│   └── 03_generate_html.R   # HTML hesabat generasiyası
├── sql/
│   └── create_tables.sql    # Baza cədvəllərinin yaradılması
├── rsconnect/               # Deployment konfiqurasiyası
└── www/                     # Statik veb resursları
```

## VERİLƏNLƏR BAZASI

### PostgreSQL Baza: `az_muellim_db`

**Cədvəl 1: `movcud_standartlar`** — Az_agent-dən import olunmuş mövcud standartlar
- sinif (1-11)
- standart_kodu (məs: 1.1.1, 1.1.2...)
- standart_adi (əsas standart)
- alt_standart_kodu
- alt_standart_adi (alt standart mətni)
- mezmun_xetti (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları)

**Cədvəl 2: `yenilenmi_standartlar`** — AI tərəfindən yenilənmiş/əlavə olunmuş standartlar
```sql
CREATE TABLE yenilenmi_standartlar (
  id SERIAL PRIMARY KEY,
  sinif INTEGER NOT NULL,
  mezmun_xetti VARCHAR(100),
  standart_kodu VARCHAR(20),
  standart_metni TEXT,
  alt_standart_kodu VARCHAR(20),
  alt_standart_metni TEXT,
  deyisiklik_novu VARCHAR(20),  -- 'movcud', 'yenilenib', 'yeni', 'silinib'
  esaslandirma TEXT,
  beynelxalq_istinad TEXT,
  bloom_seviyyesi VARCHAR(50),
  pisa_elaqesi TEXT,
  pirls_elaqesi TEXT,
  cefr_seviyyesi VARCHAR(10),
  yaradilma_tarixi TIMESTAMP DEFAULT NOW()
);
```

### Dəyişiklik növü dəyərləri (deyisiklik_novu)
Bazada saxlanılan son dəyərlər:
- `movcud` — Dəyişiklik tələb etməyən standart (olduğu kimi saxlanılır)
- `yenilenib` — AI tərəfindən yenidən yazılmış standart
- `yeni` — Yeni əlavə olunmuş standart (əvvəl mövcud deyildi)
- `silinib` — Silinməsi təsdiqlənmiş standart

## DASHBOARD TABLARI (app.R)

### 1. Ana Səhifə (`home`)
- 11 sinifin hamısı üçün statistika (value box-lar)
- Ümumi standart sayı, yenilənib, yeni əlavə sayları (tam ədəd formatda)
- Plotly diaqram — sinif üzrə standart paylanması
- Hər iki cədvəldən (`movcud_standartlar` + `yenilenmi_standartlar`) məlumat birləşdirilir

### 2. Standartlar (`standards`)
- Sinif seçimi (1-11) və dəyişiklik növü filtri
- Rəngli DT cədvəl: mövcud (mavi), yenilənib (yaşıl), yeni (narıncı), silinib (qırmızı)
- `yenilenmi_standartlar` cədvəlindən oxuyur

### 3. Müqayisə (`compare`)
- Köhnə vs yeni standartları yan-yana göstərir
- Rəngli kartlar ilə fərqləri vurğulayır

### 4. Beynəlxalq (`international`)
- PISA, PIRLS, CEFR, Bloom uyğunluq göstəriciləri

### 5. Statistika (`stats`)
- Plotly diaqramlar — sinif, məzmun xətti, dəyişiklik növü üzrə

### 6. Claude AI Təhlil (`ai_analysis`) — ⭐ ƏN ƏSS TAB
3 addımlı iş axını (aşağıda ətraflı təsvir olunub)

### 7. HTML Export (`export`)
- Seçilmiş sinif üçün HTML hesabat yükləmə

## 3 ADDIMLI İŞ AXINI (Claude AI Təhlil tabı)

### Addım 1: AI Təhlil (`btn_step1`)
- İstifadəçi sinif (1-11) və məzmun xətti (Dinləmə/Danışma/Oxu/Yazı/Dil qaydaları) seçir
- Claude API yalnız seçilmiş məzmun xəttinə aid standartları təhlil edir
- AI hər standart üçün status təyin edir: `qalsin` / `yenilensin` / `silinsin`
- Nəticə rəngli kartlarla göstərilir:
  - 🟢 QALSIN (yaşıl) — standart yaxşıdır
  - 🟠 YENİLƏNSİN (narıncı) — təkmilləşdirmə tələb edir
  - 🔴 SİLİNSİN (qırmızı) — artıq lazım deyil
  - 🔵 YENİ TƏKLİF (mavi) — AI-nin təklif etdiyi yeni standartlar
- Nəticə `step1_results` reactiveVal-da saxlanılır
- HTML hesabat avtomatik `html_reports/` qovluğuna yazılır
- AI cavab formatı:
```json
{
  "umumi_qiymetlendirme": "...",
  "standartlar": [
    {
      "standart_kodu": "1.1.1",
      "standart_metni": "...",
      "alt_standart_metni": "...",
      "mezmun_xetti": "Oxu",
      "status": "qalsin|yenilensin|silinsin",
      "esaslandirma": "...",
      "bloom_seviyyesi": "Anlama"
    }
  ],
  "yeni_teklifler": [
    {
      "standart_kodu": "1.1.X",
      "standart_metni": "...",
      "alt_standart_metni": "...",
      "mezmun_xetti": "...",
      "esaslandirma": "...",
      "bloom_seviyyesi": "...",
      "cetinlik": "asan|orta|çətin"
    }
  ]
}
```

### Addım 2: İnteraktiv Seçim və Yenidən Yazma (`btn_step2_load`, `btn_step2_ai`)
- **Yüklə** (`btn_step2_load`): Step 1 nəticələrindən YALNIZ dəyişiklik tələb edən standartları yükləyir
  - `yenilensin` statuslu standartlar
  - `silinsin` statuslu standartlar
  - Yeni təkliflər (`yeni` statuslu)
  - `qalsin` olanlar GÖSTƏRİLMİR
- **İnteraktiv cədvəl** (`dt_step2_review`): Hər sətirdə 3 düymə:
  - `Saxla` → status `movcud` olur (dəyişiklikdən imtina)
  - `Yenilə` → status `yenilensin` qalır (AI yenidən yazacaq)
  - `Sil` → status `silinsin` olur (silinəcək)
- **AI ilə yenilə** (`btn_step2_ai`): Yalnız `yenilensin` statuslu standartları Claude API-yə göndərir
  - AI hər birinin yeni mətnini yazır
  - Uğurlu olduqda status `yenilensin` → `yenilenib` olur
  - Yenilənmiş mətn cədvəldə görünür

#### Status keçid məntiqi:
```
Addım 1 → Addım 2 yüklə:
  "yenilensin" → "yenilensin" (təklif: hələ yenilənməyib)
  "silinsin"   → "silinsin"   (təklif: hələ silinməyib)
  yeni_teklifler → "yeni"

Addım 2 düymələr:
  "Saxla" düyməsi  → "movcud"     (dəyişiklikdən imtina)
  "Yenilə" düyməsi → "yenilensin" (AI yenidən yazacaq)
  "Sil" düyməsi    → "silinsin"   (silinəcək)

Addım 2 AI yenilə:
  "yenilensin" → "yenilenib" (AI artıq yeniləyib)

Addım 3 bazaya yazanda:
  "yenilensin" → "yenilenib" (avtomatik çevrilir)
  "silinsin"   → "silinib"   (avtomatik çevrilir)
```

#### Rəng sxemi (Step 2 cədvəl):
- `movcud` — açıq mavi (#E3F2FD)
- `yenilensin` — narıncı (#FFF3E0) — təklif, hələ icra olunmayıb
- `yenilenib` — yaşıl (#E8F5E9) — AI artıq yeniləyib
- `yeni` — açıq mavi (#E3F2FD)
- `silinsin` — açıq qırmızı (#FFEBEE) — təklif
- `silinib` — tünd qırmızı (#FFCDD2) — təsdiqlənib

### Addım 3: Bazaya Saxla (`btn_step3`)
- Step 1-dən `qalsin` standartlar → `movcud` olaraq yazılır
- Step 2-dən dəyişdirilmiş standartlar yazılır:
  - `yenilensin` → `yenilenib` (avtomatik çevrilir)
  - `silinsin` → `silinib` (avtomatik çevrilir)
  - `yeni` → `yeni`
  - `movcud` → skip (artıq yuxarıda yazılıb)
- Əvvəlki qeydlər silinir (sinif + məzmun xətti üzrə) və yeniləri yazılır

## BEYNƏLXALQ ÇƏRÇƏVƏLƏR

### 1. PISA (Oxu Savadlılığı)
- Məlumat əldə etmə və axtarış
- İnteqrasiya və interpretasiya
- Əks etdirmə və qiymətləndirmə
- PISA 2025: rəqəmsal oxu, çoxsaylı mənbələr, kritik qiymətləndirmə

### 2. PIRLS (Oxu Bacarıqları)
- Aydın ifadə olunmuş məlumatın tapılması
- Birbaşa nəticə çıxarma
- İnterpretasiya və ideyaların inteqrasiyası
- Məzmun və formanın qiymətləndirilməsi
- PIRLS 2026: rəqəmsal mətnlər, onlayn oxu strategiyaları

### 3. CEFR (Avropa Dil Çərçəvəsi)
- A1 (1-2-ci siniflər)
- A2 (3-4-cü siniflər)
- B1 (5-7-ci siniflər)
- B2 (8-9-cu siniflər)
- C1 (10-11-ci siniflər)
- Mediation (vasitəçilik) — CEFR 2020 yenilik

### 4. Bloom Taksonomiyası (Yenilənmiş)
- Xatırlama → Anlama → Tətbiq → Analiz → Qiymətləndirmə → Yaratma
- Hər standart Bloom səviyyəsi ilə etiketlənir

### 5. Aparıcı 6 Ölkə (Ana dili standartları müqayisəsi)
1. **Finlandiya** — fənlərarası oxu, çoxsaylı mətnlər, media savadlılığı
2. **Sinqapur** — STELLAR proqramı, kommunikativ yanaşma, kritik düşüncə
3. **Estoniya** — rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya
4. **Yaponiya** — dərin oxu, estetik duyum, yaradıcı ifadə
5. **Kanada (Ontario)** — diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər
6. **İrlandiya** — ikidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma

## İŞƏ SALMA

### İlkin quraşdırma (bir dəfə):
```bash
cd ~/Desktop/Az_dili_standartlar
Rscript install.R                              # R paketlərini quraşdır
psql -U royatalibova -d az_muellim_db -f sql/create_tables.sql  # Cədvəlləri yarat
Rscript scripts/01_db_export.R                 # Az_agent-dən məlumat köçür
```

### Dashboard-u işə salmaq:
```bash
cd ~/Desktop/Az_dili_standartlar
Rscript -e "shiny::runApp('app.R', port=4567, host='0.0.0.0')"
```
Dashboard açılacaq: http://localhost:4567

### Port problemi olduqda:
```bash
lsof -ti:4567 | xargs kill -9    # Köhnə prosesi öldür
```

## TEXNİKİ QEYDLƏR

### Claude API
- `.env` faylından `ANTHROPIC_API_KEY` oxunur
- `call_claude()` funksiyası API çağırışı edir və token/xərc statistikası qaytarır
- `extract_json_array()` funksiyası Claude-un ```json...``` sarğısını təmizləyir
- JSON parse xətaları üçün robust error handling mövcuddur

### Shiny ReactiveVal-lar
- `step1_results` — Addım 1 AI təhlil nəticəsi (parsed JSON object)
- `step2_data` — Addım 2 interaktiv cədvəl datası (data.frame)
- `session$onFlushed()` callback-lərində `isolate()` istifadə olunmalıdır

### DT Cədvəl
- `formatStyle()` yalnız `colnames` ilə adlandırılmış sütun adını qəbul edir
- Sütun adları data.frame-da `names(df) <- ...` ilə dəyişdirilir

## VACİB QAYDALAR
- Bütün interfeys və məzmun AZƏRBAYCAN DİLİNDƏ olmalıdır
- Standart kodları: 1.1.1., 1.1.2. formatında (Azərbaycan kurikulum formatı)
- Məzmun xətləri: Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları
- Hər dəyişiklik mütləq əsaslandırılmalıdır (beynəlxalq istinad)
- Value box rəqəmləri tam ədəd (`as.integer()`) olmalıdır
- HTML hesabatlar çap üçün uyğun olmalıdır
- İdarəetmə tabı SİLİNİB — funksionallıq 3 addımlı iş axınına birləşdirilib
