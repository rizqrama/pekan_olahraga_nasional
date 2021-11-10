# load packages ----
library(tidyverse)
library(gt)
library(gtExtras)

# load data ----
juara_umum <- read_csv("data_raw/pon_juara_umum_rev.csv") %>% 
  janitor::clean_names() %>% 
  rename(prov_juara = juara_umum)

perolehan_medali <- read_delim("data_raw/perolehan_medali.csv", delim = ";") %>% 
  janitor::clean_names() %>% select(1:7)

# data wrangling ----
## pada bagian ini kita akan mempersiapkan hal-hal yang ingin ditampilkan dalam visualisasi, yaitu: provinsi dengan perolehan juara umum paling banyak, tren kemenangan, serta total perolehan medali selama mengikuti gelaran PON

# > jumlah juara umum yang berhasil diraih provinsi ----

daftar_juara <- juara_umum %>% 
  dplyr::filter(is.na(prov_juara) == FALSE) %>% 
  count(prov_juara,
        name = "total_juara") %>% 
  arrange(desc(total_juara))

# kustomisasi nama provinsi dengan lambang provinsi

daftar_juara <- daftar_juara %>% 
  mutate(
    juara = case_when(
      str_detect(prov_juara, "DKI Jakarta") ~ "https://raw.githubusercontent.com/rizqrama/pekan_olahraga_nasional/master/supporting_files/logo_provinsi/dki_jakarta.png",
      str_detect(prov_juara, "Jawa Barat") ~ "https://raw.githubusercontent.com/rizqrama/pekan_olahraga_nasional/master/supporting_files/logo_provinsi/jawa_barat.png",
      str_detect(prov_juara, "Jawa Timur") ~ "https://raw.githubusercontent.com/rizqrama/pekan_olahraga_nasional/master/supporting_files/logo_provinsi/jawa_timur.png",
      str_detect(prov_juara, "Jawa Tengah") ~ "https://raw.githubusercontent.com/rizqrama/pekan_olahraga_nasional/master/supporting_files/logo_provinsi/jawa_tengah.png"
    ),
    slogan = case_when(
      str_detect(prov_juara, "DKI Jakarta") ~ "Jaya Raya",
      str_detect(prov_juara, "Jawa Barat") ~ "Gemah Ripah Repeh Rapih",
      str_detect(prov_juara, "Jawa Timur") ~ "Jer Basuki Mawa Beya",
      str_detect(prov_juara, "Jawa Tengah") ~ "Prasetya Ulah Sakti Bhakti Praja"
    )
  )

# mari kita coba plot untuk sementara
daftar_juara %>% 
  gt() %>% 
  tab_header(
    title = md("Daftar Juara Umum *Pekan Olahraga Nasional*")
  ) %>% 
  gtExtras::gt_theme_nytimes() %>% #memilih tema
  gtExtras::gt_merge_stack(
    col1 = prov_juara,
    col2 = slogan
  ) %>% 
  gtExtras::gt_img_rows( # menambahkan gambar pada tabel
    columns = juara,
    height = 20
  ) %>% 
  gtExtras::gt_fa_repeats( # menambahkan icon dari fontawesome.com
    column = total_juara,
    palette = "orange",
    name = "trophy",
    align = "left"
  )

# > tren kemenangan tiap provinsi ----

nama_juara <- daftar_juara %>% 
  pull(prov_juara)

tren_juara <- juara_umum %>% 
  mutate(jwr = 1) %>%  # menambahkan variabel "1 kali juara tiap tahun"
  complete( # menambahkan tahun yang tidak juara dengan "0 juara"
    prov_juara,
    edisi,
    fill = list(jwr = 0)
  ) %>% 
  group_by(prov_juara) %>% 
  summarise(tren = list(jwr)) %>% # membuat daftar tren kemenangan
  dplyr::filter(prov_juara %in% nama_juara) # hanya untuk 3 provinsi teratas

# mari kita coba visualisasi sementara

tren_juara %>% 
  gt() %>% 
  gtExtras::gt_plt_winloss(
    tren,
    max_wins = 20,
    colors = c("orange", "gray", "gray"),
    type = "pill",
    width = 28
  ) %>% 
  tab_options(data_row.padding = px(2))

# > total perolehan medali ----

medali_juara <- perolehan_medali %>% 
  dplyr::filter(provinsi %in% nama_juara) %>% 
  select(-7) %>% 
  pivot_longer( # membuat data yang bersifat horisontal menjadi vertikal
    4:6,
    names_to = "tipe_mdl",
    values_to = "jml_mdl"
  ) %>% 
  group_by(
    provinsi,
    tipe_mdl
  ) %>% 
  summarise(
    total_medali = sum(jml_mdl)
  ) %>% 
  ungroup() %>% 
  group_by(provinsi) %>% 
  summarise(jumlah_medali = list(total_medali)) %>% 
  rename(prov_juara = provinsi)

# mari kita coba visualisasikan sementara 

warna_medali <- c("#FFD700", '#C0C0C0', "#CD7F32")

medali_juara %>% 
  gt() %>% 
  gt_plt_bar_stack( # membuat komparasi medali tiap provinsi
    column = jumlah_medali,
    position = "stack",
    labels = c("Emas", "Perak", "Perunggu"),
    palette = warna_medali,
    width = 60,
    fmt_fn = scales::label_number(accuracy = 1, trim = TRUE)
  ) 

# menggabungkan ketiga subset data ----
juara_joined <- daftar_juara %>% 
  select(3,1,4,2) %>% 
  left_join(tren_juara, by = "prov_juara") %>% 
  left_join(medali_juara, by = "prov_juara")

# visualisasi gabungan ----

vis_juara <- juara_joined %>% 
  gt() %>% # konversi menjadi gt_object()
  gtExtras::gt_merge_stack(
    col1 = prov_juara,
    col2 = slogan
  ) %>% 
  gtExtras::gt_img_rows( # menambahkan gambar pada tabel
    columns = juara,
    height = 40
  ) %>% 
  gtExtras::gt_fa_repeats( # menambahkan icon dari fontawesome.com
    column = total_juara,
    palette = "#FFD700",
    name = "trophy",
    align = "left"
  ) %>% 
  gtExtras::gt_plt_winloss(
    tren,
    max_wins = 20,
    colors = c("#16A75C", "gray", "gray"),
    type = "pill",
    width = 40
  ) %>% 
  tab_options(data_row.padding = px(2)) %>% 
  gt_plt_bar_stack( # membuat komparasi medali tiap provinsi
    column = jumlah_medali,
    position = "stack",
    labels = c("Emas", "Perak", "Perunggu"),
    palette = warna_medali,
    width = 60,
    fmt_fn = scales::label_number(accuracy = 1, trim = TRUE)
  ) %>% 
  cols_label( # mengubah nama/label pada kolom
    juara = "Provinsi",
    prov_juara = " ",
    total_juara = md("Total Perolehan Juara Umum"),
    tren = md("Histori<br/>Juara Umum")
  ) %>% 
  cols_align( # mengatur alinemen kolom
    align = "center",
    columns = c(juara, tren)
  ) %>% 
  tab_spanner( # membuat spanner pada kolom jumlah medali
    label = "Perolehan Medali",
    columns = c(jumlah_medali)
  ) %>% 
  # memberikan narasi dan detail pada tabel
  tab_header( # judul dan deskripsi
    title = md("Para Jawara Pekan Olahraga Nasional"),
    subtitle = md(
      "**Pekan Olahraga Nasional** atau disingkat *PON* merupakan pesta olahraga nasional di Indonesia yang diselenggarakan oleh Komite Olahraga Nasional Indonesia setiap empat tahun sekali dan diikuti oleh seluruh provinsi di Indonesia. Dalam 73 tahun keberjalannya, hanya **empat provinsi** yang berhasil menyabet gelar juara umum pada 20 kali perhelatan PON"
    )
  ) %>% 
  tab_source_note( # membuat keterangan sumber data
    source_note = md(
      "**Data:** Wikipedia & prokabar.com | **Tabel:** Insight Officer Jabar Digital Service" 
    )
  ) %>% 
  tab_footnote( # membuat catatan kaki
    footnote = md(
      "Jawa Tengah menjadi juara PON edisi pertama dengan diwakili oleh Karesidenan Surakarta. PON edisi ke 6 pada tahun 1965 dibatalkan karena adanya peristiwa G-30S-PKI. Untuk edisi terbaru, yaitu PON ke 20 di Papua, yang seharusnya diselenggkarakan pada tahun 2020 diundur ke 2021 karena adanya Pandemi Covid-19."
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%  ## mengubah style pada tabel (fonta, ukuran fonta, dan warna fonta)
  tab_style(# Title
    style = list(
      cell_text(
        font=google_font(name = "Playfair Display"), 
        size = "xx-large",
        align = "left",
        color='#006430')),
    locations = cells_title(groups = "title")
  )%>%
  tab_style(# Subtitle
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "left",
        size = "medium",
        color = "#191919")
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(# Header
    style = list(
      cell_text(
        font=google_font(name = "Roboto"), 
        align = "left",
        v_align = "middle",
        color = "#191919")),
    locations = cells_column_labels(
      columns = c(prov_juara, total_juara)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto"), 
        align = "center",
        v_align = "middle",
        color = "#191919")),
    locations = cells_column_labels(
      columns = c(juara, tren)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "center",
        size='small')),
    locations = cells_column_labels(
      columns = c(jumlah_medali)
    )
  )%>%
  tab_style( # Spanner
    style = list(
      cell_text(
        font=google_font(name = "Roboto"), 
        align = "center",
        color = "#191919"
      )),
    locations = cells_column_spanners()
  ) %>%
  tab_style( # Body
    style = list(
      cell_text(font=google_font(name = "Open Sans"),
                align = 'left'
      )),
    locations = cells_body(
      columns = c(prov_juara, total_juara, jumlah_medali)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Open Sans"),
                align = 'center'
      )),
    locations = cells_body(
      columns = c(juara, tren)
    )
  )%>%
  tab_style( # Footnote
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"),
        style = "italic",
        size = "small")),
    locations = cells_footnotes()
  ) %>%
  tab_style( # source note
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"),
        align = "right",
        size = "small")),
    locations = cells_source_notes()
  )%>%
  tab_options( # Borders
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden"
  )

gtsave_extra(
  data = vis_juara,
  filename = "outfile/tabel_pon.png"
)
