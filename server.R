# server.R
library(shiny)
library(pheatmap)
library(RColorBrewer)
library(dplyr)

# Define server logic to read selected file ----

function(input, output) {
  
  # inputdf <- reactive({
  #   inFile1 <- input$file1
  #   inFile2 <- input$file2
  #   inFile3 <- input$file3
  #   inFile4 <- input$file4
  # })
  
  
  output$heatmapP <- renderPlot({
      
      req(input$file1, input$file2, input$file3, input$file4)
      
      adjplist <- list(f1 = input$file1$datapath,
                       f2 = input$file2$datapath,
                       f3 = input$file3$datapath,
                       f4 = input$file4$datapath
      )
      
      
      # adjplist <- list(f1 = "./AMS1k_nAMS1k_MRM_test_result.csv",
      #                  f2 = "./AMS4k_AMS1k_MRM_test_result.csv",
      #                  f3 = "./AMS4k_nAMS4k_MRM_test_result.csv",
      #                  f4 = "./nAMS4k_nAMS1k_MRM_test_result.csv"
      # )
      # input$goButton
      ########## output 4 compgroup DEPs overlap symbols ######
      sym <- data.frame(var_a =character())
      
      for (i in 1:4) {
          # i=1
          raw <- adjplist[[i]]
          
          raw_i <- read.csv(raw)
          
          data_extract <- function(dat) {
              raw <- dplyr::filter(dat, q_value < 0.05) %>%
                  subset(select = c("symbol"))
          } 
          sym <- rbind(sym, data_extract(raw_i) )
      } 
      overlap_sym <- unique(sym)
      
      
      ##########################################################################
      # raw_list store the logFC, q_list store the q-value
      raw_list <- list()
      q_list <- list()
      
      for ( i in 1:4) {
          
          # i <- 1
          raw <- adjplist[[i]]
          
          raw_i <- read.csv(raw)
          
          heat_raw <- raw_i %>% right_join(overlap_sym, by = 'symbol' ) %>%
              subset(select = c("symbol", "log2FC")) %>% arrange( symbol)  %>% 
              tibble::column_to_rownames(var = "symbol") 
          colnames(heat_raw) <- basename(raw)
          
          raw_list[[basename(raw)]] <- heat_raw #q_list
          
          q_sub <- raw_i %>% right_join(overlap_sym, by = 'symbol' ) %>%
              subset(select = c("symbol", "q_value")) %>% arrange( symbol)  %>% 
              tibble::column_to_rownames(var = "symbol") 
          
          colnames(q_sub) <- basename(raw)
          
          q_list[[basename(raw)]] <- q_sub
          
      }
      
      raw <- do.call(cbind,raw_list)
      q <- do.call(cbind,q_list)
      
      # NA replace with 1 to represent non-sig
      q[is.na(q)] <- 1
      
      # sig replace with * 
      if (!is.null(q)){
          ssmt <- q< 0.05
          q[ssmt] <-'*'
          q[!ssmt]<- ''
      } else {
          q <- F
      }
      
      ######## pheatmap plot #####
      #bk <- c(seq(-2,-0.1,by=0.1),seq(0,2,by=0.1))
      
      p <- pheatmap(raw,
                    #color = c(colorRampPalette(colors = c("#285BBC","lightyellow"))(length(bk)/2),
                    # colorRampPalette(colors = c("lightyellow","#AF0000"))(length(bk)/2)),
                    # legend_breaks=seq(-1.5,1.5,1),
                    # breaks = bk,
                    cluster_rows  = FALSE,
                    cluster_cols  = FALSE,
                    display_numbers = q,
                    # width = 100,
                    # height = 100,
                    # fontsize_row = 10,
                    # fontsize_col = 10,
                    legend = TRUE,
                    legend.position= "left",
                    # cellwidth = 22,
                    # cellheight = 10,
                    # fontsize_number = 12,
                    # show_colnames = F,
                    angle_col = 45,
                    border=NA,
                    na_col = "white"
                    #  display_numbers = matrix(ifelse(heat_raw != 0, "*", ""))
                    
      )
  })
  
  # output$plot <- renderPlot({
  #   if (! is.null(inputdf())){
  #     
  #     adjplist <- list(f1 = inFile1$datapath,
  #                      f2 = inFile2$datapath,
  #                      f3 =inFile3$datapath,
  #                      f4= inFile4$datapath
  #     )
  #     
  #     ########## output 4 compgroup DEPs overlap symbols ######
  #     sym <- data.frame(var_a =character())
  #     
  #     for (i in 1:4) {
  #      i=1
  #       raw <- adjplist[[i]]
  #       
  #       raw_i <- read.csv(raw)
  # 
  #       data_extract <- function(data) {
  #         raw <- data %>% filter(q_value < 0.05) %>%
  #           subset(select = c("symbol"))
  #       } 
  #       sym <- rbind(sym,data_extract(raw_i))
  #     } 
  #     overlap_sym <- unique(sym)
  #     
  #     raw_list <- list()
  #     q_list <- list()
  #     
  #     for ( i in 1:4) {
  #    
  #       
  #       raw <- adjplist[[i]]
  #       
  #       raw_i <- read.csv(raw)
  #       
  #       heat_raw <- raw_i %>% right_join(overlap_sym, by = 'symbol' ) %>%
  #         subset(select = c("symbol", "log2FC")) %>% arrange( symbol)  %>% 
  #         tibble::column_to_rownames(var = "symbol") 
  #       colnames(heat_raw) <- raw_var
  #       
  #       raw_list[[raw_var]] <- heat_raw #q_list
  #       
  #       q_sub <- raw_i %>% right_join(overlap_sym, by = 'symbol' ) %>%
  #         subset(select = c("symbol", "q_value")) %>% arrange( symbol)  %>% 
  #         tibble::column_to_rownames(var = "symbol") 
  #       
  #       colnames(q_sub) <- raw_var
  #       
  #       q_list[[raw_var]] <- q_sub
  #       
  #     }
  #     
  #     raw <- do.call(cbind,raw_list)
  #     q <- do.call(cbind,q_list)
  #     
  #     # NA replace with 1 to represent non-sig
  #     q[is.na(q)] <- 1
  #     
  #     # sig replace with * 
  #     if (!is.null(q)){
  #       ssmt <- q< 0.05
  #       q[ssmt] <-'*'
  #       q[!ssmt]<- ''
  #     } else {
  #       q <- F
  #     }
  #     
  #     ######## pheatmap plot #####
  #     #bk <- c(seq(-2,-0.1,by=0.1),seq(0,2,by=0.1))
  #     
  #     p <- pheatmap(raw,
  #                   #color = c(colorRampPalette(colors = c("#285BBC","lightyellow"))(length(bk)/2),
  #                   # colorRampPalette(colors = c("lightyellow","#AF0000"))(length(bk)/2)),
  #                   # legend_breaks=seq(-1.5,1.5,1),
  #                   # breaks = bk,
  #                   cluster_rows  = FALSE,
  #                   cluster_cols  = FALSE,
  #                   display_numbers = q,
  #                   width = 100,
  #                   height = 100,
  #                   fontsize_row = 10,
  #                   fontsize_col = 10,
  #                   legend = TRUE,
  #                   legend.position= "left",
  #                   cellwidth = 22,
  #                   cellheight = 10,
  #                   fontsize_number = 12,
  #                   # show_colnames = F,
  #                   angle_col = 45,
  #                   border=NA,
  #                   na_col = "white"
  #                   #  display_numbers = matrix(ifelse(heat_raw != 0, "*", ""))
  #   
  #     )
  #   }
  # }
  # )
}

  
    
    


