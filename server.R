library(rhandsontable)
library(shiny)
library(shinythemes)
library(googleVis)
library(DT)
library(ggplot2)

function(input, output, session) {
  
  kode<-c('A','B','C','D','E','F','G','H','I','J','K','L','M,N','O','P','Q','R,S,T,U')
  lap_usaha<-c('Pertanian, Kehutanan, dan Perikanan','Pertambangan, dan Penggalian','Industri Pengolahan','Pengadaan Listrik dan Gas','Pengadaan Air, Pengelolaan Sampah, Limbah dan Daur Ulang','Konstruksi ','Perdagangan Besar dan Eceran; Reparasi Mobil dan Sepeda Motor','Transportasi dan Pergudangan','Penyediaan Akomodasi dan Makan Minum','Informasi dan Komunikasi','Jasa Keuangan dan Asuransi','Real Estat','Jasa Perusahaan ','Administrasi Pemerintahan, Pertahanan dan Jaminan Sosial Wajib','Jasa Pendidikan','Jasa Kesehatan dan Kegiatan Sosial','Jasa Lainnya')
  T2010<-c(0)
  
  kode2<-c("Pengeluaran Konsumsi Rumah Tangga",
           "Pengeluaran Konsumsi LNPRT",
           "Pengeluaran Konsumsi Pemerintahan", 
           "Pembentukan Modal Tetap Domestik Bruto", 
           "Perubahan Inventori",
           "Ekspor Barang dan Jasa", 
           "Dikurangi Impor Barang dan Jasa") 
  
  
  kode_pajak<-c("Pajak dikurangi subsisdi atas produk")
  DF <- data.frame(kode,lap_usaha,T2010)
  DF2 <- data.frame(kode2,T2010)
  pajak<-data.frame(T2010)
  
  colnames1<-c("KODE","LAPANGAN USAHA")
  colnames2<-c("KODE")
  
  colnames(DF)<-c(colnames1,"T2010")
  colnames(DF2)<-c(colnames2,"T2010")
  
  DF_produksi_ahk<-DF
  DF_produksi_ahb<-DF
  pajak_produksi_ahk<-pajak
  pajak_produksi_ahb<-pajak
  DF_pengeluaran_ahk<-DF2
  DF_pengeluaran_ahb<-DF2
  
  values <- reactiveValues()
  
  input_produksi_ahk<-reactive({input$hot_produksi_ahk})
  input_produksi_ahb<-reactive({input$hot_produksi_ahb})
  input_pajak_produksi_ahk<-reactive({input$hot_pajak_produksi_ahk})
  input_pajak_produksi_ahb<-reactive({input$hot_pajak_produksi_ahb})
  input_pengeluaran_ahk<-reactive({input$hot_pengeluaran_ahk})
  input_pengeluaran_ahb<-reactive({input$hot_pengeluaran_ahb})
  hot_produksi_ahk<-reactive({return(hot_to_r(input_produksi_ahk()))})
  hot_produksi_ahb<-reactive({return(hot_to_r(input_produksi_ahb()))})
  hot_pajak_produksi_ahk<-reactive({return(hot_to_r(input_pajak_produksi_ahk()))})
  hot_pajak_produksi_ahb<-reactive({return(hot_to_r(input_pajak_produksi_ahb()))})
  hot_pengeluaran_ahk<-reactive({return(hot_to_r(input_pengeluaran_ahk()))})
  hot_pengeluaran_ahb<-reactive({return(hot_to_r(input_pengeluaran_ahb()))})
  
  ## input_produksi_ahk####
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_produksi_ahk())) {
      DF_produksi_ahk = hot_produksi_ahk()
      m = dim(DF_produksi_ahk)[2]-3
      n = dim(DF_produksi_ahk)[1]
      th_dasar_DF<-as.numeric(substr(colnames(DF_produksi_ahk)[3],2,5))
      if(input$tahun_awal!=th_dasar_DF){
        selisih<-input$tahun_akhir-th_dasar_DF
        updateNumericInput(session, "tahun_akhir", value = input$tahun_awal+selisih, min = input$tahun_awal)
        tmpColnames<-c(colnames1)
        for(i in 0:m){
          tmpColnames<-c(tmpColnames,paste("T",input$tahun_awal+i,sep=""))
        }
        DF_produksi_ahk = hot_produksi_ahk()
        colnames(DF_produksi_ahk)<-tmpColnames
      }else if(m<nTahun){
        for(i in (m+1):(nTahun)){
          colname<-paste("T",input$tahun_awal+i,sep="")
          DF_produksi_ahk[,"new"]<-rep(0,n)
          colnames(DF_produksi_ahk)<-c(colnames(DF_produksi_ahk[-dim(DF_produksi_ahk)[2]]),colname)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        DF_produksi_ahk<-DF_produksi_ahk[-((dim(DF_produksi_ahk)[2]-selisih+1):dim(DF_produksi_ahk)[2])]
      }
    } else {
      if (is.null(values[["DF_produksi_ahk"]]))
        DF_produksi_ahk <- DF_produksi_ahk
      else
        DF_produksi_ahk <- values[["DF_produksi_ahk"]]
    }
    
    
    values[["DF_produksi_ahk"]] <- DF_produksi_ahk
  })
  
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_pajak_produksi_ahk())) {
      pajak_produksi_ahk = hot_pajak_produksi_ahk()
      m = dim(pajak_produksi_ahk)[2]-1
      n = dim(pajak_produksi_ahk)[1]
      th_dasar_pajak<-as.numeric(substr(colnames(pajak_produksi_ahk)[1],2,5))
      if(th_dasar_pajak!=input$tahun_awal){
        colname_pajak<-c()
        for(i in 0:m){
          colname_pajak<-c(colname_pajak,paste("T",input$tahun_awal+i,sep=""))
        }
        colnames(pajak_produksi_ahk)<-colname_pajak
      }else if(m<nTahun){ #jika ada tahun akhir ditambah
        for(i in (m+1):(nTahun)){
          colname_pajak<-paste("T",input$tahun_awal+i,sep="")
          pajak_produksi_ahk[,"new"]<-rep(0,n)
          colnames(pajak_produksi_ahk)<-c(colnames(pajak_produksi_ahk[-dim(pajak_produksi_ahk)[2]]),colname_pajak)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        pajak_produksi_ahk<-pajak_produksi_ahk[-((dim(pajak_produksi_ahk)[2]-selisih+1):dim(pajak_produksi_ahk)[2])]
      }
    } else {
      if (is.null(values[["pajak_produksi_ahk"]]))
        pajak_produksi_ahk <- pajak_produksi_ahk
      else
        pajak_produksi_ahk <- values[["pajak_produksi_ahk"]]
    }
    values[["pajak_produksi_ahk"]] <- pajak_produksi_ahk
    
  })
  
  
  
  output$hot_produksi_ahk <- renderRHandsontable({
    DF_produksi_ahk <- values[["DF_produksi_ahk"]]
    
    if (!is.null(DF_produksi_ahk))
      rhandsontable(DF_produksi_ahk, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)%>%
      hot_col(col = "KODE", type = "text")%>%
      hot_col(col = "LAPANGAN USAHA", type = "text")
  })
  
  
  output$hot_pajak_produksi_ahk <- renderRHandsontable({
    pajak_produksi_ahk<-values[["pajak_produksi_ahk"]]
    if (!is.null(pajak_produksi_ahk))
      rhandsontable(pajak_produksi_ahk, stretchH = "all")%>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  ## input_produksi_ahb####
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_produksi_ahb())) {
      DF_produksi_ahb = hot_produksi_ahb()
      m = dim(DF_produksi_ahb)[2]-3
      n = dim(DF_produksi_ahb)[1]
      th_dasar_DF<-as.numeric(substr(colnames(DF_produksi_ahb)[3],2,5))
      if(input$tahun_awal!=th_dasar_DF){
        selisih<-input$tahun_akhir-th_dasar_DF
        updateNumericInput(session, "tahun_akhir", value = input$tahun_awal+selisih, min = input$tahun_awal)
        tmpColnames<-c(colnames1)
        for(i in 0:m){
          tmpColnames<-c(tmpColnames,paste("T",input$tahun_awal+i,sep=""))
        }
        DF_produksi_ahb = hot_produksi_ahb()
        colnames(DF_produksi_ahb)<-tmpColnames
      }else if(m<nTahun){
        for(i in (m+1):(nTahun)){
          colname<-paste("T",input$tahun_awal+i,sep="")
          DF_produksi_ahb[,"new"]<-rep(0,n)
          colnames(DF_produksi_ahb)<-c(colnames(DF_produksi_ahb[-dim(DF_produksi_ahb)[2]]),colname)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        DF_produksi_ahb<-DF_produksi_ahb[-((dim(DF_produksi_ahb)[2]-selisih+1):dim(DF_produksi_ahb)[2])]
      }
    } else {
      if (is.null(values[["DF_produksi_ahb"]]))
        DF_produksi_ahb <- DF_produksi_ahb
      else
        DF_produksi_ahb <- values[["DF_produksi_ahb"]]
    }
    
    
    values[["DF_produksi_ahb"]] <- DF_produksi_ahb
  })
  
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_pajak_produksi_ahb())) {
      pajak_produksi_ahb = hot_pajak_produksi_ahb()
      m = dim(pajak_produksi_ahb)[2]-1
      n = dim(pajak_produksi_ahb)[1]
      th_dasar_pajak<-as.numeric(substr(colnames(pajak_produksi_ahb)[1],2,5))
      if(th_dasar_pajak!=input$tahun_awal){
        colname_pajak<-c()
        for(i in 0:m){
          colname_pajak<-c(colname_pajak,paste("T",input$tahun_awal+i,sep=""))
        }
        colnames(pajak_produksi_ahb)<-colname_pajak
      }else if(m<nTahun){ #jika ada tahun akhir ditambah
        for(i in (m+1):(nTahun)){
          colname_pajak<-paste("T",input$tahun_awal+i,sep="")
          pajak_produksi_ahb[,"new"]<-rep(0,n)
          colnames(pajak_produksi_ahb)<-c(colnames(pajak_produksi_ahb[-dim(pajak_produksi_ahb)[2]]),colname_pajak)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        pajak_produksi_ahb<-pajak_produksi_ahb[-((dim(pajak_produksi_ahb)[2]-selisih+1):dim(pajak_produksi_ahb)[2])]
      }
    } else {
      if (is.null(values[["pajak_produksi_ahb"]]))
        pajak_produksi_ahb <- pajak_produksi_ahb
      else
        pajak_produksi_ahb <- values[["pajak_produksi_ahb"]]
    }
    values[["pajak_produksi_ahb"]] <- pajak_produksi_ahb
    
  })
  
  
  
  output$hot_produksi_ahb <- renderRHandsontable({
    DF_produksi_ahb <- values[["DF_produksi_ahb"]]
    
    if (!is.null(DF_produksi_ahb))
      rhandsontable(DF_produksi_ahb, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)%>%
      hot_col(col = "KODE", type = "text")%>%
      hot_col(col = "LAPANGAN USAHA", type = "text")
  })
  
  
  output$hot_pajak_produksi_ahb <- renderRHandsontable({
    pajak_produksi_ahb<-values[["pajak_produksi_ahb"]]
    if (!is.null(pajak_produksi_ahb))
      rhandsontable(pajak_produksi_ahb, stretchH = "all")%>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  ## input_pengeluaran_ahk####
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_pengeluaran_ahk())) {
      DF_pengeluaran_ahk = hot_pengeluaran_ahk()
      m = dim(DF_pengeluaran_ahk)[2]-2
      n = dim(DF_pengeluaran_ahk)[1]
      th_dasar_DF<-as.numeric(substr(colnames(DF_pengeluaran_ahk)[2],2,5))
      if(input$tahun_awal!=th_dasar_DF){
        selisih<-input$tahun_akhir-th_dasar_DF
        updateNumericInput(session, "tahun_akhir", value = input$tahun_awal+selisih, min = input$tahun_awal)
        tmpColnames<-c(colnames2)
        for(i in 0:selisih){
          tmpColnames<-c(tmpColnames,paste("T",input$tahun_awal+i,sep=""))
        }
        DF_pengeluaran_ahk = hot_pengeluaran_ahk()
        colnames(DF_pengeluaran_ahk)<-tmpColnames
      }else if(m<nTahun){
        for(i in (m+1):(nTahun)){
          colname<-paste("T",input$tahun_awal+i,sep="")
          DF_pengeluaran_ahk[,"new"]<-rep(0,n)
          colnames(DF_pengeluaran_ahk)<-c(colnames(DF_pengeluaran_ahk[-dim(DF_pengeluaran_ahk)[2]]),colname)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        DF_pengeluaran_ahk<-DF_pengeluaran_ahk[-((dim(DF_pengeluaran_ahk)[2]-selisih+1):dim(DF_pengeluaran_ahk)[2])]
      }
    } else {
      if (is.null(values[["DF_pengeluaran_ahk"]]))
        DF_pengeluaran_ahk <- DF_pengeluaran_ahk
      else
        DF_pengeluaran_ahk <- values[["DF_pengeluaran_ahk"]]
    }
    
    values[["DF_pengeluaran_ahk"]] <- DF_pengeluaran_ahk
  })
  
  
  
  output$hot_pengeluaran_ahk <- renderRHandsontable({
    DF_pengeluaran_ahk <- values[["DF_pengeluaran_ahk"]]
    
    if (!is.null(DF_pengeluaran_ahk))
      rhandsontable(DF_pengeluaran_ahk, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)%>%
      hot_col(col = "KODE", type = "text")
  })
  
  ## input_pengeluaran_ahb####
  observe({
    nTahun<-input$tahun_akhir-input$tahun_awal
    if (!is.null(input_pengeluaran_ahb())) {
      DF_pengeluaran_ahb = hot_pengeluaran_ahb()
      m = dim(DF_pengeluaran_ahb)[2]-2
      n = dim(DF_pengeluaran_ahb)[1]
      th_dasar_DF<-as.numeric(substr(colnames(DF_pengeluaran_ahb)[2],2,5))
      if(input$tahun_awal!=th_dasar_DF){
        selisih<-input$tahun_akhir-th_dasar_DF
        updateNumericInput(session, "tahun_akhir", value = input$tahun_awal+selisih, min = input$tahun_awal)
        tmpColnames<-c(colnames2)
        for(i in 0:selisih){
          tmpColnames<-c(tmpColnames,paste("T",input$tahun_awal+i,sep=""))
        }
        DF_pengeluaran_ahb = hot_pengeluaran_ahb()
        colnames(DF_pengeluaran_ahb)<-tmpColnames
      }else if(m<nTahun){
        for(i in (m+1):(nTahun)){
          colname<-paste("T",input$tahun_awal+i,sep="")
          DF_pengeluaran_ahb[,"new"]<-rep(0,n)
          colnames(DF_pengeluaran_ahb)<-c(colnames(DF_pengeluaran_ahb[-dim(DF_pengeluaran_ahb)[2]]),colname)
        }
      }else if(m>nTahun){
        selisih<-m-nTahun
        DF_pengeluaran_ahb<-DF_pengeluaran_ahb[-((dim(DF_pengeluaran_ahb)[2]-selisih+1):dim(DF_pengeluaran_ahb)[2])]
      }
    } else {
      if (is.null(values[["DF_pengeluaran_ahb"]]))
        DF_pengeluaran_ahb <- DF_pengeluaran_ahb
      else
        DF_pengeluaran_ahb <- values[["DF_pengeluaran_ahb"]]
    }
    
    values[["DF_pengeluaran_ahb"]] <- DF_pengeluaran_ahb
  })
  
  
  
  output$hot_pengeluaran_ahb <- renderRHandsontable({
    DF_pengeluaran_ahb <- values[["DF_pengeluaran_ahb"]]
    
    if (!is.null(DF_pengeluaran_ahb))
      rhandsontable(DF_pengeluaran_ahb, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)%>%
      hot_col(col = "KODE", type = "text")
  })
  
  
  #### PErtumbuhan Ekonomi####
  
  #### Struktur Ekonomi####
  output$table_struktur <- DT::renderDataTable({
    ValAdd <- values[['DF_produksi_ahb']]
    tax <- data.frame(c("Pajak dikurangi Subsidi Atas Produk","",values[['pajak_produksi_ahb']]))
    colnames(tax)<-colnames(ValAdd)
    
    datatable(strukEko(ValAdd,tax), 
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                lengthChange=FALSE,
                dom='tB',
                bPaginate=FALSE,
                buttons = c('copy', 'csv', 'excel')),
              class = "display") %>% 
      formatRound(c(3:ncol(ValAdd)), 1)
  })
  
  output$graph_struktur <- renderPlot({
    ValAdd <- values[['DF_produksi_ahb']]
    tax <- data.frame(c("Pajak dikurangi Subsidi Atas Produk","",values[['pajak_produksi_ahb']]))
    colnames(tax)<-colnames(ValAdd)
    data<-graph_format_struktur(ValAdd,tax,input$tahun_awal,input$tahun_akhir)
    my_fun=function(vec){ as.numeric(vec[3]) / sum(data$value[data$year==vec[2]]) *100 }
    data$prop=apply(data , 1 , my_fun)
    
    ggplot(data, aes(x=year, y=prop, fill=sector)) + 
      geom_area(alpha=0.6 , size=1, colour="black")
  })
  
  #### Inflasi####
  
  #### ICOR ####
  
  #### Perkiraan Investasi####
}