-- ============================================================
-- Az_dili_standartlar — PostgreSQL Verilənlər Bazası Sxemi
-- Azərbaycan dili 1-11 sinifləri üçün standartların yenilənməsi
-- ============================================================

-- Əvvəlcə bazanı yaradaq
-- psql -U postgres -c "CREATE DATABASE az_dili_standartlar;"

\c az_dili_standartlar;

-- ============================================================
-- 1. Mövcud standartlar (Az_agent bazasından köçürüləcək)
-- ============================================================
CREATE TABLE IF NOT EXISTS movcud_standartlar (
    id SERIAL PRIMARY KEY,
    sinif INTEGER NOT NULL CHECK (sinif BETWEEN 1 AND 11),
    mezmun_xetti VARCHAR(100) NOT NULL,  -- Dinləmə, Danışma, Oxu, Yazı, Dil qaydaları
    standart_kodu VARCHAR(30) NOT NULL,
    standart_metni TEXT NOT NULL,
    alt_standart_kodu VARCHAR(30),
    alt_standart_metni TEXT,
    yaradilma_tarixi TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 2. Yenilənmiş standartlar (Claude API tərəfindən yaradılacaq)
-- ============================================================
CREATE TABLE IF NOT EXISTS yenilenmi_standartlar (
    id SERIAL PRIMARY KEY,
    sinif INTEGER NOT NULL CHECK (sinif BETWEEN 1 AND 11),
    mezmun_xetti VARCHAR(100) NOT NULL,
    
    -- Standart məlumatları
    standart_kodu VARCHAR(30) NOT NULL,
    standart_metni TEXT NOT NULL,
    alt_standart_kodu VARCHAR(30),
    alt_standart_metni TEXT,
    
    -- Dəyişiklik məlumatları
    deyisiklik_novu VARCHAR(20) NOT NULL 
        CHECK (deyisiklik_novu IN ('movcud', 'yenilenib', 'yeni', 'silinib')),
    kohne_metni TEXT,  -- Əvvəlki mətn (yenilənibsə)
    esaslandirma TEXT,  -- Niyə dəyişib
    
    -- Beynəlxalq istinadlar
    beynelxalq_istinad TEXT,  -- PISA/PIRLS/CEFR/hansı ölkə
    bloom_seviyyesi VARCHAR(50),  -- Xatırlama, Anlama, Tətbiq, Analiz, Qiymətləndirmə, Yaratma
    pisa_elaqesi TEXT,
    pirls_elaqesi TEXT,
    cefr_seviyyesi VARCHAR(10),  -- A1, A2, B1, B2, C1
    olke_istinadi TEXT,  -- Hansı ölkə standartı əsas götürülüb
    
    -- Metadata
    yaradilma_tarixi TIMESTAMP DEFAULT NOW(),
    versiya INTEGER DEFAULT 1,
    status VARCHAR(20) DEFAULT 'aktiv'
);

-- ============================================================
-- 3. Beynəlxalq çərçəvələr (istinad cədvəli)
-- ============================================================
CREATE TABLE IF NOT EXISTS beynelxalq_cerceveler (
    id SERIAL PRIMARY KEY,
    cerceve_adi VARCHAR(50) NOT NULL,  -- PISA, PIRLS, CEFR, Bloom
    kateqoriya VARCHAR(100),
    alt_kateqoriya VARCHAR(200),
    tesvir TEXT,
    sinif_araligi VARCHAR(20)  -- məs: "1-4", "5-9", "10-11"
);

-- ============================================================
-- 4. Ölkə standartları (müqayisə üçün)
-- ============================================================
CREATE TABLE IF NOT EXISTS olke_standartlari (
    id SERIAL PRIMARY KEY,
    olke VARCHAR(50) NOT NULL,
    sinif_araligi VARCHAR(20),
    mezmun_xetti VARCHAR(100),
    standart_metni TEXT,
    xususi_yanaşma TEXT,
    menbe TEXT
);

-- ============================================================
-- 5. Dəyişiklik jurnalı
-- ============================================================
CREATE TABLE IF NOT EXISTS deyisiklik_jurnali (
    id SERIAL PRIMARY KEY,
    yenilenmi_standart_id INTEGER REFERENCES yenilenmi_standartlar(id),
    deyisiklik_tarixi TIMESTAMP DEFAULT NOW(),
    deyisiklik_tesviri TEXT,
    istifadeci VARCHAR(100) DEFAULT 'system'
);

-- ============================================================
-- İNDEKSLƏR
-- ============================================================
CREATE INDEX idx_movcud_sinif ON movcud_standartlar(sinif);
CREATE INDEX idx_movcud_mezmun ON movcud_standartlar(mezmun_xetti);
CREATE INDEX idx_yeni_sinif ON yenilenmi_standartlar(sinif);
CREATE INDEX idx_yeni_mezmun ON yenilenmi_standartlar(mezmun_xetti);
CREATE INDEX idx_yeni_deyisiklik ON yenilenmi_standartlar(deyisiklik_novu);
CREATE INDEX idx_olke ON olke_standartlari(olke);

-- ============================================================
-- BEYNƏLXALQ ÇƏRÇƏVƏ DATALARI
-- ============================================================

-- PISA Oxu Savadlılığı
INSERT INTO beynelxalq_cerceveler (cerceve_adi, kateqoriya, alt_kateqoriya, tesvir, sinif_araligi) VALUES
('PISA', 'Oxu Savadlılığı', 'Məlumat əldə etmə və axtarış', 'Mətndə konkret məlumatı tapmaq, axtarmaq və seçmək', '5-11'),
('PISA', 'Oxu Savadlılığı', 'İnteqrasiya və interpretasiya', 'Mətnin mənasını anlamaq, müxtəlif hissələri birləşdirmək', '5-11'),
('PISA', 'Oxu Savadlılığı', 'Əks etdirmə və qiymətləndirmə', 'Mətnin məzmunu və forması haqqında mühakimə yürütmək', '5-11'),
('PISA', 'Oxu Savadlılığı', 'Rəqəmsal oxu', 'Onlayn mühitdə çoxsaylı mənbələrdən oxu və qiymətləndirmə', '5-11'),
('PISA', 'Oxu Savadlılığı', 'Kritik qiymətləndirmə', 'Məlumatın etibarlılığını və keyfiyyətini qiymətləndirmək', '7-11');

-- PIRLS Oxu Bacarıqları
INSERT INTO beynelxalq_cerceveler (cerceve_adi, kateqoriya, alt_kateqoriya, tesvir, sinif_araligi) VALUES
('PIRLS', 'Oxu Anlama', 'Aydın ifadə olunmuş məlumatın tapılması', 'Mətndə birbaşa verilmiş faktları tapmaq', '1-4'),
('PIRLS', 'Oxu Anlama', 'Birbaşa nəticə çıxarma', 'Mətndəki məlumatdan sadə nəticələr çıxarmaq', '1-4'),
('PIRLS', 'Oxu Anlama', 'İnterpretasiya və ideyaların inteqrasiyası', 'Şəxsi biliklərlə mətn məlumatını birləşdirmək', '1-6'),
('PIRLS', 'Oxu Anlama', 'Məzmun və formanın qiymətləndirilməsi', 'Müəllifin üslubunu və məqsədini qiymətləndirmək', '3-6'),
('PIRLS', 'Oxu Anlama', 'Rəqəmsal mətnlər', 'Onlayn mətnləri oxumaq və naviqasiya etmək', '3-6');

-- CEFR Səviyyələri
INSERT INTO beynelxalq_cerceveler (cerceve_adi, kateqoriya, alt_kateqoriya, tesvir, sinif_araligi) VALUES
('CEFR', 'A1 - Başlanğıc', 'Dinləmə/Oxu', 'Sadə ifadələri və gündəlik sözləri anlayır', '1-2'),
('CEFR', 'A2 - Təməl', 'Dinləmə/Oxu', 'Tez-tez istifadə olunan ifadələri anlayır, sadə mətnləri oxuyur', '3-4'),
('CEFR', 'B1 - Orta', 'Dinləmə/Oxu/Yazı', 'Əsas fikirləri anlayır, sadə əlaqəli mətn yaza bilir', '5-7'),
('CEFR', 'B2 - Orta-yuxarı', 'Bütün bacarıqlar', 'Mürəkkəb mətnləri anlayır, aydın, ətraflı yaza bilir', '8-9'),
('CEFR', 'C1 - İrəliləmiş', 'Bütün bacarıqlar', 'Çətin mətnləri anlayır, sərbəst ifadə edir, akademik yazı', '10-11'),
('CEFR', 'Mediation', 'Vasitəçilik', 'Mətnləri başqalarına uyğunlaşdırma, ümumiləşdirmə (2020 yenilik)', '5-11');

-- Bloom Taksonomiyası
INSERT INTO beynelxalq_cerceveler (cerceve_adi, kateqoriya, alt_kateqoriya, tesvir, sinif_araligi) VALUES
('Bloom', 'Xatırlama', 'Bilik əldə etmə', 'Faktları, terminləri, anlayışları xatırlama', '1-11'),
('Bloom', 'Anlama', 'Mənanı dərk etmə', 'İzah etmə, təsnif etmə, ümumiləşdirmə, nəticə çıxarma', '1-11'),
('Bloom', 'Tətbiq', 'Praktik istifadə', 'Bilikləri yeni situasiyalarda tətbiq etmə', '1-11'),
('Bloom', 'Analiz', 'Təhlil etmə', 'Hissələrə ayırma, əlaqələri müəyyən etmə, fərqləndirmə', '3-11'),
('Bloom', 'Qiymətləndirmə', 'Mühakimə yürütmə', 'Tənqidi düşüncə, əsaslandırma, mühakimə', '5-11'),
('Bloom', 'Yaratma', 'Yeni məhsul yaratma', 'Dizayn etmə, qurmaq, planlaşdırma, istehsal etmə', '5-11');

-- ============================================================
-- ÖLKƏ STANDARTLARI (aparıcı 6 ölkə)
-- ============================================================
INSERT INTO olke_standartlari (olke, sinif_araligi, mezmun_xetti, standart_metni, xususi_yanaşma, menbe) VALUES
-- Finlandiya
('Finlandiya', '1-2', 'Oxu', 'Şagird müxtəlif növ mətnləri oxuyur və əsas məzmunu başa düşür', 'Fənlərarası oxu — bütün fənlərdə oxu bacarıqları inkişaf etdirilir', 'Finnish National Core Curriculum 2014/2020'),
('Finlandiya', '3-6', 'Oxu', 'Şagird müxtəlif mənbələrdən məlumat toplayır, müqayisə edir, qiymətləndirir', 'Media savadlılığı — çoxsaylı media mətnlərinin kritik oxusu', 'Finnish National Core Curriculum 2014/2020'),
('Finlandiya', '7-9', 'Yazı', 'Şagird müxtəlif auditoriya üçün müxtəlif janrlarda yaza bilir', 'Çox-modalı mətn yaratma — mətn, şəkil, video birlikdə', 'Finnish National Core Curriculum 2014/2020'),

-- Sinqapur
('Sinqapur', '1-2', 'Dinləmə/Danışma', 'Şagird aydın və əminliklə danışır, müxtəlif kontekstlərdə ünsiyyət qurur', 'STELLAR proqramı — kommunikativ yanaşma, real həyat kontekstləri', 'Singapore English Language Syllabus 2020'),
('Sinqapur', '3-6', 'Oxu', 'Şagird mətni tənqidi nöqteyi-nəzərdən oxuyur, müəllifin məqsədini müəyyən edir', 'Kritik düşüncə — oxu prosesində sual vermə strategiyaları', 'Singapore English Language Syllabus 2020'),
('Sinqapur', '7-9', 'Yazı', 'Şagird müxtəlif məqsədlər üçün əsaslandırılmış, strukturlu mətn yaza bilir', 'Proses yönümlü yazı — planlaşdırma, qaralama, redaktə, nəşr', 'Singapore English Language Syllabus 2020'),

-- Estoniya
('Estoniya', '1-3', 'Dil qaydaları', 'Şagird ana dilinin əsas qrammatik strukturlarını istifadə edir', 'Rəqəmsal savadlılıq — texnologiya ilə dil öyrənmə birləşdirilir', 'Estonian National Curriculum 2014/2023'),
('Estoniya', '4-6', 'Oxu', 'Şagird rəqəmsal və çap mətnlərini eyni səviyyədə oxuyur', 'Mədəniyyətlərarası kommunikasiya — müxtəlif mədəniyyətlərdən mətnlər', 'Estonian National Curriculum 2014/2023'),
('Estoniya', '7-9', 'Danışma', 'Şagird mübahisə aparır, öz fikrini əsaslandırır, başqalarının fikirlərini qiymətləndirir', 'Debat mədəniyyəti — strukturlu mübahisə və əsaslandırma', 'Estonian National Curriculum 2014/2023'),

-- Yaponiya
('Yaponiya', '1-2', 'Oxu', 'Şagird mətnin gözəlliyini hiss edir, oxuduğunu obrazlı təsəvvür edir', 'Estetik duyum — ədəbi mətnlərin emosional qavranılması', 'Japanese Course of Study (Kokugo) 2017/2020'),
('Yaponiya', '3-6', 'Oxu', 'Şagird müxtəlif janrları dərindən oxuyur, mətn strukturunu təhlil edir', 'Dərin oxu — mətnlə uzun müddət işləmə, təkrar oxu', 'Japanese Course of Study (Kokugo) 2017/2020'),
('Yaponiya', '7-9', 'Yazı', 'Şagird yaradıcı və analitik mətnlər yazır, öz üslubunu inkişaf etdirir', 'Yaradıcı ifadə — şeir, esse, hekayə yazma ənənəsi', 'Japanese Course of Study (Kokugo) 2017/2020'),

-- Kanada (Ontario)
('Kanada', '1-3', 'Oxu', 'Şagird müxtəlif mədəniyyətlərdən gələn mətnləri oxuyur və müzakirə edir', 'Diferensial öyrənmə — hər şagirdin səviyyəsinə uyğun materiallar', 'Ontario Language Curriculum 2023'),
('Kanada', '4-6', 'Danışma', 'Şagird qrup müzakirələrində aktiv iştirak edir, fikirlərini əsaslandırır', 'İnklüziv yanaşma — bütün şagirdlərin səsinin eşidilməsi', 'Ontario Language Curriculum 2023'),
('Kanada', '7-9', 'Oxu', 'Şagird çoxmədəniyyətli mətnləri kritik oxuyur, öz mövqeyini formalaşdırır', 'Çoxmədəniyyətli mətnlər — yerli xalqlar, miqrantlar, müxtəlif toplumlar', 'Ontario Language Curriculum 2023'),

-- İrlandiya
('İrlandiya', '1-2', 'Danışma', 'Şagird şifahi hekayə danışır, dinləyicini cəlb edir', 'Şifahi ənənə — hekayə danışma mədəniyyəti', 'Irish Primary Language Curriculum 2019'),
('İrlandiya', '3-6', 'Yazı', 'Şagird rəqəmsal və ənənəvi formatda mətn yaradır', 'Rəqəmsal mətn yaratma — blog, wiki, rəqəmsal hekayə', 'Irish Primary Language Curriculum 2019'),
('İrlandiya', '7-9', 'Oxu', 'Şagird ədəbi və informativ mətnləri müqayisə edir, tənqidi təhlil aparır', 'İkidilli yanaşma — ana dili və ikinci dil paralel inkişaf', 'Irish Junior Cycle Specification 2017');

-- ============================================================
-- VIEW: Müqayisə üçün birləşdirilmiş görünüş
-- ============================================================
CREATE OR REPLACE VIEW v_standart_muqayise AS
SELECT 
    m.sinif,
    m.mezmun_xetti,
    m.standart_kodu AS kohne_kod,
    m.standart_metni AS kohne_metni,
    m.alt_standart_kodu AS kohne_alt_kod,
    m.alt_standart_metni AS kohne_alt_metni,
    y.standart_kodu AS yeni_kod,
    y.standart_metni AS yeni_metni,
    y.alt_standart_kodu AS yeni_alt_kod,
    y.alt_standart_metni AS yeni_alt_metni,
    y.deyisiklik_novu,
    y.esaslandirma,
    y.beynelxalq_istinad,
    y.bloom_seviyyesi,
    y.cefr_seviyyesi
FROM movcud_standartlar m
LEFT JOIN yenilenmi_standartlar y 
    ON m.sinif = y.sinif 
    AND m.standart_kodu = y.standart_kodu
    AND m.mezmun_xetti = y.mezmun_xetti

UNION ALL

SELECT 
    y.sinif,
    y.mezmun_xetti,
    NULL AS kohne_kod,
    NULL AS kohne_metni,
    NULL AS kohne_alt_kod,
    NULL AS kohne_alt_metni,
    y.standart_kodu AS yeni_kod,
    y.standart_metni AS yeni_metni,
    y.alt_standart_kodu AS yeni_alt_kod,
    y.alt_standart_metni AS yeni_alt_metni,
    y.deyisiklik_novu,
    y.esaslandirma,
    y.beynelxalq_istinad,
    y.bloom_seviyyesi,
    y.cefr_seviyyesi
FROM yenilenmi_standartlar y
WHERE y.deyisiklik_novu = 'yeni';

-- ============================================================
-- VIEW: Statistika
-- ============================================================
CREATE OR REPLACE VIEW v_statistika AS
SELECT 
    sinif,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'movcud') AS movcud_say,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yenilenib') AS yenilenib_say,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'yeni') AS yeni_say,
    COUNT(*) FILTER (WHERE deyisiklik_novu = 'silinib') AS silinib_say,
    COUNT(*) AS umumi_say
FROM yenilenmi_standartlar
GROUP BY sinif
ORDER BY sinif;

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO postgres;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO postgres;
