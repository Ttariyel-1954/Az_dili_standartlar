# Az_dili_standartlar — Azərbaycan Dili Standartlarının Yenilənməsi Layihəsi

## LAYİHƏNİN MƏQSƏDİ
Bu layihə Azərbaycan Respublikasında 1-11-ci siniflər üzrə Azərbaycan dili fənninin mövcud standart və alt standartlarını beynəlxalq tələblərə uyğun yeniləmək, təkmilləşdirmək və yeni standartlar əlavə etmək üçün nəzərdə tutulub.

## TEXNİKİ STEK
- **Verilənlər bazası**: PostgreSQL (mövcud Az_agent bazasından köçürmə + yeni baza)
- **Backend/Analiz**: R, Claude API
- **Frontend**: HTML5 (statik hesabatlar), R Shiny (interaktiv dashboard)
- **Dil**: Azərbaycan dili (bütün interfeys və məzmun)

## VERİLƏNLƏR BAZASI
### Mövcud Baza (Az_agent-dən köçürmə)
Az_agent layihəsindəki PostgreSQL bazasında `az_dili_standartlar` cədvəli var:
- sinif (1-11)
- standart_kodu (məs: 1.1.1, 1.1.2...)
- standart_adi (əsas standart)
- alt_standart_kodu
- alt_standart_adi (alt standart mətni)
- mezmun_xetti (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları)

### Yeni Baza Sxemi
```sql
-- Mövcud standartlar (Az_agent-dən import)
CREATE TABLE movcud_standartlar (...)

-- Yenilənmiş standartlar
CREATE TABLE yenilenmi_standartlar (
  id SERIAL PRIMARY KEY,
  sinif INTEGER NOT NULL,
  mezmun_xetti VARCHAR(100),
  standart_kodu VARCHAR(20),
  standart_metni TEXT,
  alt_standart_kodu VARCHAR(20),
  alt_standart_metni TEXT,
  deyisiklik_novu VARCHAR(20), -- 'movcud', 'yenilenib', 'yeni', 'silinib'
  esaslandirma TEXT, -- niyə dəyişib
  beynelxalq_istinad TEXT, -- PISA/PIRLS/CEFR/hansı ölkə
  bloom_seviyyesi VARCHAR(50),
  pisa_elaqesi TEXT,
  pirls_elaqesi TEXT,
  cefr_seviyyesi VARCHAR(10),
  yaradilma_tarixi TIMESTAMP DEFAULT NOW()
);
```

## BEYNƏLXALQ ÇƏRÇƏVƏLƏR (MÜTLƏQq NƏZƏRƏ ALINMALI)

### 1. PISA (Oxu Savadlılığı)
- Məlumat əldə etmə və axtarış (Accessing and retrieving)
- İnteqrasiya və interpretasiya (Integrating and interpreting)  
- Əks etdirmə və qiymətləndirmə (Reflecting and evaluating)
- PISA 2025 yeniliklər: rəqəmsal oxu, çoxsaylı mənbələr, kritik qiymətləndirmə

### 2. PIRLS (Oxu Bacarıqları)
- Aydın ifadə olunmuş məlumatın tapılması
- Birbaşa nəticə çıxarma
- İnterpretasiya və ideyaların inteqrasiyası
- Məzmun və formanın qiymətləndrilməsi
- PIRLS 2026 yeniliklər: rəqəmsal mətnlər, onlayn oxu strategiyaları

### 3. CEFR (Avropa Dil Çərçəvəsi)
- A1-A2 (1-4-cü siniflər)
- B1-B2 (5-9-cu siniflər)
- C1 (10-11-ci siniflər)
- Mediation (vasitəçilik) — CEFR 2020 yenilik

### 4. Bloom Taksonomiyası (Yenilənmiş)
- Xatırlama → Anlama → Tətbiq → Analiz → Qiymətləndirmə → Yaratma
- Hər standart mütləq Bloom səviyyəsi ilə etiketlənməlidir

### 5. APARICI 6 ÖLKƏ (Ana dili standartları müqayisəsi)
1. **Finlandiya** — Əsas dil (Finnish mother tongue): fənlərarası oxu, çoxsaylı mətnlər, media savadlılığı
2. **Sinqapur** — İngilis dili standartları: STELLAR proqramı, kommunikativ yanaşma, kritik düşüncə
3. **Estoniya** — Eston dili kurikulumu: rəqəmsal savadlılıq, mədəniyyətlərarası kommunikasiya
4. **Yaponiya** — Kokugo (ana dili): dərin oxu, estetik duyum, yaradıcı ifadə
5. **Kanada (Ontario)** — Language Arts: diferensial öyrənmə, inklüziv yanaşma, çoxmədəniyyətli mətnlər
6. **İrlandiya** — Gaeilge/English: ikidilli yanaşma, şifahi ənənə, rəqəmsal mətn yaratma

## HTML5 HESABAT FORMATI
Hər sinif üçün ayrıca HTML5 fayl yaradılmalıdır:
- **Fayl adı**: `sinif_N_standartlar.html` (N = 1-11)
- **Rəng sxemi**: Mavi çalarlar (#0D47A1, #1565C0, #1976D2, #2196F3, #42A5F5, #64B5F6, #90CAF9)
- **Mövcud standartlar**: Ağ fonda mavi mətn
- **Yenilənmiş standartlar**: Açıq yaşıl fon (#E8F5E9), yaşıl haşiyə
- **Yeni standartlar**: Açıq narıncı fon (#FFF3E0), narıncı haşiyə
- **Silinmiş standartlar**: Açıq qırmızı fon (#FFEBEE), üstündən xətt
- **Şrift ölçüsü**: Əsas mətn 16-18px, başlıqlar 22-28px (oxunaqlılıq üçün böyük)
- **Font**: "Segoe UI", "Noto Sans", sans-serif
- **Responsive dizayn**: Mobil uyğunluq

## R SHINY DASHBOARD TƏLƏBLƏRİ
Dashboard aşağıdakıları əhatə etməlidir:
1. **Sinif seçimi** (1-11) — dropdown
2. **Məzmun xətti filtri** (Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları)
3. **Mövcud standartların göstərilməsi** — cədvəl
4. **Yenilənmiş standartların göstərilməsi** — rəngli cədvəl
5. **Müqayisə rejimi** — köhnə vs yeni yan-yana
6. **Beynəlxalq istinad paneli** — PISA/PIRLS/CEFR uyğunluğu
7. **Statistika paneli** — neçə mövcud, neçə yenilənib, neçə yeni əlavə olunub
8. **HTML export düyməsi** — seçilmiş sinif üçün HTML hesabat yükləmə
9. **Claude API inteqrasiyası** — istənilən sinif üçün canlı standart təhlili

## İCRA ADDIM-ADDIM

### Addım 1: Bazanı köçürmək
```bash
cd ~/Desktop/Az_dili_standartlar
Rscript scripts/01_db_export.R
```

### Addım 2: Yeni bazanı yaratmaq
```bash
psql -U postgres -f sql/create_tables.sql
```

### Addım 3: Standartları yeniləmək (Claude API ilə)
```bash
Rscript scripts/02_update_standards.R
```

### Addım 4: HTML hesabatları yaratmaq
```bash
Rscript scripts/03_generate_html.R
```

### Addım 5: Dashboard-u işə salmaq
```bash
cd ~/Desktop/Az_dili_standartlar
Rscript -e "shiny::runApp('app.R', port=4567)"
```

## CLAUDE API İSTİFADƏSİ
Hər sinif üçün Claude API-yə göndəriləcək prompt:

```
Sən Azərbaycan dili kurikulumu ekspertisən. 
Sənə {sinif}-ci sinif üçün mövcud standartlar veriləcək.

Tapşırıq:
1. Hər mövcud standartı PISA, PIRLS, CEFR, Bloom taksonomiyası prizmasından qiymətləndir
2. Finlandiya, Sinqapur, Estoniya, Yaponiya, Kanada (Ontario), İrlandiya standartları ilə müqayisə et
3. Hər standart üçün:
   - Saxlanmalı, yenilənməli, yoxsa silinməli olduğunu göstər
   - Yeniləmə varsa, yeni mətni yaz
   - Əsaslandırma ver (hansı beynəlxalq çərçəvəyə əsasən)
   - Bloom səviyyəsini göstər
   - CEFR uyğunluğunu göstər
4. Əlavə olunmalı YENİ standartları təklif et
5. Cavabı JSON formatında ver

Mövcud standartlar:
{standartlar_json}
```

## VACİB QAYDALAR
- Bütün interfeys və məzmun AZƏRBAYCAN DİLİNDƏ olmalıdır
- Standart kodları Azərbaycan kurikulum formatına uyğun olmalıdır (məs: 1.1.1., 1.1.2.)
- Məzmun xətləri: Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları
- Hər dəyişiklik əsaslandırılmalıdır
- HTML hesabatlar çap üçün uyğun olmalıdır
- Dashboard mütləq PostgreSQL ilə işləməlidir
