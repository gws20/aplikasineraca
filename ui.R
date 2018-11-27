navbarPage(
  "Aplikasi Neraca",
  tabPanel("INPUT",
           ####Struktur Ekonomi####
           pageWithSidebar(
             headerPanel('Masukan Nilai'),
             sidebarPanel(
               numericInput("tahun_awal","Tahun Awal",2010, min =2010, max=2010),
               numericInput("tahun_akhir","Tahun AKhir",2010, min = 2010)
             ),
             mainPanel(
               tabsetPanel(id = "tabset_input_data",
                           tabPanel("PDB Produksi AHK", value = "tab_pdb_produksi_ahk",
                                    fluidRow(
                                      h1("Value Added"),
                                      rHandsontableOutput("hot_produksi_ahk"),
                                      hr(),
                                      h1("Input Pajak"),
                                      rHandsontableOutput("hot_pajak_produksi_ahk"),
                                      br())
                           ),
                           tabPanel("PDB Produksi AHB", value = "tab_pdb_produksi_ahb",
                                    fluidRow(
                                      h1("Value Added"),
                                      rHandsontableOutput("hot_produksi_ahb"),
                                      hr(),
                                      h1("Input Pajak"),
                                      rHandsontableOutput("hot_pajak_produksi_ahb"),
                                      br())
                           ),
                           tabPanel("PDB Pengeluaran AHK", value = "tab_pdb_pengeluaran_ahk",
                                    fluidRow(
                                      rHandsontableOutput("hot_pengeluaran_ahk"),
                                      hr())),
                           tabPanel("PDB Pengeluaran AHB", value = "tab_pdb_pengeluaran_ahb",
                                    fluidRow(
                                      rHandsontableOutput("hot_pengeluaran_ahb"),
                                      hr()))
               )
             )
             
           )
  ),
  
  theme = shinytheme('flatly')
)

