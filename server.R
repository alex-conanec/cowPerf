token <- readRDS("droptoken.rds")
rdrop2::drop_acc(dtoken = token)
require(shiny)

server <- function(input, output, session) {
  
  col_names <- colnames(read.csv('www/colnames_data.csv', header = TRUE, sep = ';'))
  load('www/models_final_scale.RData')
  load('www/indicateurs_scale_PI_cor.RData')
  
  PI = list(EI = 1:3,
            QC = 4:10,
            QN = 11:17,
            QS = 18:24)
  
  cow0 <- setNames(data.frame(matrix(0, ncol = length(col_names), nrow = 1)), col_names)
  save_slider_S1 <- setNames(data.frame(matrix(0, ncol = length(col_names), nrow = 3)), col_names)
  rownames(save_slider_S1) <- c('value', 'min', 'max')
  save_slider_S2 <- setNames(data.frame(matrix(0, ncol = length(col_names), nrow = 3)), col_names)
  rownames(save_slider_S2) <- c('value', 'min', 'max')
  
  slider_S1_id <- c('EI1_S1', 'EI2_S1', 'EI3_S1',
                    'QC1_S1', 'QC2_S1', 'QC3_S1', 'QC4_S1', 'QC5_S1', 'QC6_S1', 'QC7_S1',
                    'QN1_S1', 'QN2_S1', 'QN3_S1', 'QN4_S1', 'QN5_S1', 'QN6_S1', 'QN7_S1',
                    'QS1_S1', 'QS2_S1', 'QS3_S1', 'QS4_S1', 'QS5_S1', 'QS6_S1', 'QS7_S1')
  
  
  slider_S2_id <- c('EI1_S2', 'EI2_S2', 'EI3_S2',
                    'QC1_S2', 'QC2_S2', 'QC3_S2', 'QC4_S2', 'QC5_S2', 'QC6_S2', 'QC7_S2',
                    'QN1_S2', 'QN2_S2', 'QN3_S2', 'QN4_S2', 'QN5_S2', 'QN6_S2', 'QN7_S2',
                    'QS1_S2', 'QS2_S2', 'QS3_S2', 'QS4_S2', 'QS5_S2', 'QS6_S2', 'QS7_S2')
  
  
  output$dynamic_PI <- renderUI({
    
    
    
    if (input$PI == 'EI'){
      
      
      if (!is.null(input$EI1_S1)) save_slider_S1[1,1] <- input$EI1_S1
      if (!is.null(input$EI2_S1)) save_slider_S1[1,2] <- input$EI2_S1
      if (!is.null(input$EI3_S1)) save_slider_S1[1,3] <- input$EI3_S1
      
      for(i in PI[[1]]){
        
        save_slider_S1[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                         newdata = save_slider_S1[1,])$min, 1)
        
        save_slider_S1[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                         newdata = save_slider_S1[1,])$max, 1)
      }
      
      wellPanel('Vache 1',
                sliderInput(slider_S1_id[1], value = save_slider_S1[1,1], label = col_names[1], 
                            min = save_slider_S1[2,1], max = save_slider_S1[3,1], step = 0.1),
                
                sliderInput(slider_S1_id[2], value = save_slider_S1[1,2], label = col_names[2], 
                            min = save_slider_S1[2,2], max = save_slider_S1[3,2], step = 0.1),
                
                sliderInput(slider_S1_id[3], value = save_slider_S1[1,3], label = col_names[3], 
                            min = save_slider_S1[2,3], max = save_slider_S1[3,3], step = 0.1)
      )
      
    }else if (input$PI == 'QC'){
      
      
      if (!is.null(input$QC1_S1)) save_slider_S1[1,4] <- input$QC1_S1
      if (!is.null(input$QC2_S1)) save_slider_S1[1,5] <- input$QC2_S1
      if (!is.null(input$QC3_S1)) save_slider_S1[1,6] <- input$QC3_S1
      if (!is.null(input$QC4_S1)) save_slider_S1[1,7] <- input$QC4_S1
      if (!is.null(input$QC5_S1)) save_slider_S1[1,8] <- input$QC5_S1
      if (!is.null(input$QC6_S1)) save_slider_S1[1,9] <- input$QC6_S1
      if (!is.null(input$QC7_S1)) save_slider_S1[1,10] <- input$QC7_S1      
      
      for(i in PI[[2]]){
        
        save_slider_S1[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                         newdata = save_slider_S1[1,])$min, 1)
        
        save_slider_S1[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                         newdata = save_slider_S1[1,])$max, 1)
      }
      
      wellPanel('Vache 1',
                sliderInput(slider_S1_id[4], value = save_slider_S1[1,4], label = col_names[4], 
                            min = save_slider_S1[2,4], max = save_slider_S1[3,4], step = 0.1),
                
                sliderInput(slider_S1_id[5], value = save_slider_S1[1,5], label = col_names[5], 
                            min = save_slider_S1[2,5], max = save_slider_S1[3,5], step = 0.1),
                
                sliderInput(slider_S1_id[6], value = save_slider_S1[1,6], label = col_names[6], 
                            min = save_slider_S1[2,6], max = save_slider_S1[3,6], step = 0.1),
                
                sliderInput(slider_S1_id[7], value = save_slider_S1[1,7], label = col_names[7], 
                            min = save_slider_S1[2,7], max = save_slider_S1[3,7], step = 0.1),
                
                sliderInput(slider_S1_id[8], value = save_slider_S1[1,8], label = col_names[8], 
                            min = save_slider_S1[2,8], max = save_slider_S1[3,8], step = 0.1),
                
                sliderInput(slider_S1_id[9], value = save_slider_S1[1,9], label = col_names[9], 
                            min = save_slider_S1[2,9], max = save_slider_S1[3,9], step = 0.1),
                
                sliderInput(slider_S1_id[10], value = save_slider_S1[1,10], label = col_names[10], 
                            min = save_slider_S1[2,10], max = save_slider_S1[3,10], step = 0.1)
      )
      
    }else if (input$PI == 'QN'){
      
      
      if (!is.null(input$QN1_S1)) save_slider_S1[1,11] <- input$QN1_S1
      if (!is.null(input$QN2_S1)) save_slider_S1[1,12] <- input$QN2_S1
      if (!is.null(input$QN3_S1)) save_slider_S1[1,13] <- input$QN3_S1
      if (!is.null(input$QN4_S1)) save_slider_S1[1,14] <- input$QN4_S1
      if (!is.null(input$QN5_S1)) save_slider_S1[1,15] <- input$QN5_S1
      if (!is.null(input$QN6_S1)) save_slider_S1[1,16] <- input$QN6_S1
      if (!is.null(input$QN7_S1)) save_slider_S1[1,17] <- input$QN7_S1 
      
      
      for(i in PI[[3]]){
        
        save_slider_S1[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                         newdata = save_slider_S1[1,])$min, 1)
        
        save_slider_S1[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                         newdata = save_slider_S1[1,])$max, 1)
      }
      
      wellPanel('Vache 1',
                sliderInput(slider_S1_id[11], value = save_slider_S1[1,11], label = col_names[11], 
                            min = save_slider_S1[2,11], max = save_slider_S1[3,11], step = 0.1),
                
                sliderInput(slider_S1_id[12], value = save_slider_S1[1,12], label = col_names[12], 
                            min = save_slider_S1[2,12], max = save_slider_S1[3,12], step = 0.1),
                
                sliderInput(slider_S1_id[13], value = save_slider_S1[1,13], label = col_names[13], 
                            min = save_slider_S1[2,13], max = save_slider_S1[3,13], step = 0.1),
                
                sliderInput(slider_S1_id[14], value = save_slider_S1[1,14], label = col_names[14], 
                            min = save_slider_S1[2,14], max = save_slider_S1[3,14], step = 0.1),
                
                sliderInput(slider_S1_id[15], value = save_slider_S1[1,15], label = col_names[15], 
                            min = save_slider_S1[2,15], max = save_slider_S1[3,15], step = 0.1),
                
                sliderInput(slider_S1_id[16], value = save_slider_S1[1,16], label = col_names[16], 
                            min = save_slider_S1[2,16], max = save_slider_S1[3,16], step = 0.1),
                
                sliderInput(slider_S1_id[17], value = save_slider_S1[1,17], label = col_names[17], 
                            min = save_slider_S1[2,17], max = save_slider_S1[3,17], step = 0.1)
      )
    }else if (input$PI == 'QS'){
      
      
      if (!is.null(input$QS1_S1)) save_slider_S1[1,18] <- input$QS1_S1
      if (!is.null(input$QS2_S1)) save_slider_S1[1,19] <- input$QS2_S1
      if (!is.null(input$QS3_S1)) save_slider_S1[1,20] <- input$QS3_S1
      if (!is.null(input$QS4_S1)) save_slider_S1[1,21] <- input$QS4_S1
      if (!is.null(input$QS5_S1)) save_slider_S1[1,22] <- input$QS5_S1
      if (!is.null(input$QS6_S1)) save_slider_S1[1,23] <- input$QS6_S1
      if (!is.null(input$QS7_S1)) save_slider_S1[1,24] <- input$QS7_S1     
      
      
      for(i in PI[[4]]){
        
        save_slider_S1[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                         newdata = save_slider_S1[1,])$min, 1)
        
        save_slider_S1[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                         newdata = save_slider_S1[1,])$max, 1)
      }
      
      wellPanel('Vache 1',
                sliderInput(slider_S1_id[18], value = save_slider_S1[1,18], label = col_names[18], 
                            min = save_slider_S1[2,18], max = save_slider_S1[3,18], step = 0.1),
                
                sliderInput(slider_S1_id[19], value = save_slider_S1[1,19], label = col_names[19], 
                            min = save_slider_S1[2,19], max = save_slider_S1[3,19], step = 0.1),
                
                sliderInput(slider_S1_id[20], value = save_slider_S1[1,20], label = col_names[20], 
                            min = save_slider_S1[2,20], max = save_slider_S1[3,20], step = 0.1),
                
                sliderInput(slider_S1_id[21], value = save_slider_S1[1,21], label = col_names[21], 
                            min = save_slider_S1[2,21], max = save_slider_S1[3,21], step = 0.1),
                
                sliderInput(slider_S1_id[22], value = save_slider_S1[1,22], label = col_names[22], 
                            min = save_slider_S1[2,22], max = save_slider_S1[3,22], step = 0.1),
                
                sliderInput(slider_S1_id[23], value = save_slider_S1[1,23], label = col_names[23], 
                            min = save_slider_S1[2,23], max = save_slider_S1[3,23], step = 0.1),
                
                sliderInput(slider_S1_id[24], value = save_slider_S1[1,24], label = col_names[24], 
                            min = save_slider_S1[2,24], max = save_slider_S1[3,24], step = 0.1)
      )
    }
    
  })
  
  output$dynamic_scenario <- renderUI({
    
    if (input$scenario_comparatif){
      if (input$PI == 'EI'){
        
        
        if (!is.null(input$EI1_S2)) save_slider_S2[1,1] <- input$EI1_S2
        if (!is.null(input$EI2_S2)) save_slider_S2[1,2] <- input$EI2_S2
        if (!is.null(input$EI3_S2)) save_slider_S2[1,3] <- input$EI3_S2
        
        for(i in PI[[1]]){
          
          save_slider_S2[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                           newdata = save_slider_S2[1,])$min, 1)
          
          save_slider_S2[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                           newdata = save_slider_S2[1,])$max, 1)
        }
        
        wellPanel('Vache 1',
                  sliderInput(slider_S2_id[1], value = save_slider_S2[1,1], label = col_names[1], 
                              min = save_slider_S2[2,1], max = save_slider_S2[3,1], step = 0.1),
                  
                  sliderInput(slider_S2_id[2], value = save_slider_S2[1,2], label = col_names[2], 
                              min = save_slider_S2[2,2], max = save_slider_S2[3,2], step = 0.1),
                  
                  sliderInput(slider_S2_id[3], value = save_slider_S2[1,3], label = col_names[3], 
                              min = save_slider_S2[2,3], max = save_slider_S2[3,3], step = 0.1)
        )
        
      }else if (input$PI == 'QC'){
        
        
        if (!is.null(input$QC1_S2)) save_slider_S2[1,4] <- input$QC1_S2
        if (!is.null(input$QC2_S2)) save_slider_S2[1,5] <- input$QC2_S2
        if (!is.null(input$QC3_S2)) save_slider_S2[1,6] <- input$QC3_S2
        if (!is.null(input$QC4_S2)) save_slider_S2[1,7] <- input$QC4_S2
        if (!is.null(input$QC5_S2)) save_slider_S2[1,8] <- input$QC5_S2
        if (!is.null(input$QC6_S2)) save_slider_S2[1,9] <- input$QC6_S2
        if (!is.null(input$QC7_S2)) save_slider_S2[1,10] <- input$QC7_S2      
        
        for(i in PI[[2]]){
          
          save_slider_S2[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                           newdata = save_slider_S2[1,])$min, 1)
          
          save_slider_S2[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                           newdata = save_slider_S2[1,])$max, 1)
        }
        
        wellPanel('Vache 1',
                  sliderInput(slider_S2_id[4], value = save_slider_S2[1,4], label = col_names[4], 
                              min = save_slider_S2[2,4], max = save_slider_S2[3,4], step = 0.1),
                  
                  sliderInput(slider_S2_id[5], value = save_slider_S2[1,5], label = col_names[5], 
                              min = save_slider_S2[2,5], max = save_slider_S2[3,5], step = 0.1),
                  
                  sliderInput(slider_S2_id[6], value = save_slider_S2[1,6], label = col_names[6], 
                              min = save_slider_S2[2,6], max = save_slider_S2[3,6], step = 0.1),
                  
                  sliderInput(slider_S2_id[7], value = save_slider_S2[1,7], label = col_names[7], 
                              min = save_slider_S2[2,7], max = save_slider_S2[3,7], step = 0.1),
                  
                  sliderInput(slider_S2_id[8], value = save_slider_S2[1,8], label = col_names[8], 
                              min = save_slider_S2[2,8], max = save_slider_S2[3,8], step = 0.1),
                  
                  sliderInput(slider_S2_id[9], value = save_slider_S2[1,9], label = col_names[9], 
                              min = save_slider_S2[2,9], max = save_slider_S2[3,9], step = 0.1),
                  
                  sliderInput(slider_S2_id[10], value = save_slider_S2[1,10], label = col_names[10], 
                              min = save_slider_S2[2,10], max = save_slider_S2[3,10], step = 0.1)
        )
        
      }else if (input$PI == 'QN'){
        
        
        if (!is.null(input$QN1_S2)) save_slider_S2[1,11] <- input$QN1_S2
        if (!is.null(input$QN2_S2)) save_slider_S2[1,12] <- input$QN2_S2
        if (!is.null(input$QN3_S2)) save_slider_S2[1,13] <- input$QN3_S2
        if (!is.null(input$QN4_S2)) save_slider_S2[1,14] <- input$QN4_S2
        if (!is.null(input$QN5_S2)) save_slider_S2[1,15] <- input$QN5_S2
        if (!is.null(input$QN6_S2)) save_slider_S2[1,16] <- input$QN6_S2
        if (!is.null(input$QN7_S2)) save_slider_S2[1,17] <- input$QN7_S2 
        
        
        for(i in PI[[3]]){
          
          save_slider_S2[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                           newdata = save_slider_S2[1,])$min, 1)
          
          save_slider_S2[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]],
                                                                           newdata = save_slider_S2[1,])$max, 1)
        }
        
        wellPanel('Vache 1',
                  sliderInput(slider_S2_id[11], value = save_slider_S2[1,11], label = col_names[11], 
                              min = save_slider_S2[2,11], max = save_slider_S2[3,11], step = 0.1),
                  
                  sliderInput(slider_S2_id[12], value = save_slider_S2[1,12], label = col_names[12], 
                              min = save_slider_S2[2,12], max = save_slider_S2[3,12], step = 0.1),
                  
                  sliderInput(slider_S2_id[13], value = save_slider_S2[1,13], label = col_names[13], 
                              min = save_slider_S2[2,13], max = save_slider_S2[3,13], step = 0.1),
                  
                  sliderInput(slider_S2_id[14], value = save_slider_S2[1,14], label = col_names[14], 
                              min = save_slider_S2[2,14], max = save_slider_S2[3,14], step = 0.1),
                  
                  sliderInput(slider_S2_id[15], value = save_slider_S2[1,15], label = col_names[15], 
                              min = save_slider_S2[2,15], max = save_slider_S2[3,15], step = 0.1),
                  
                  sliderInput(slider_S2_id[16], value = save_slider_S2[1,16], label = col_names[16], 
                              min = save_slider_S2[2,16], max = save_slider_S2[3,16], step = 0.1),
                  
                  sliderInput(slider_S2_id[17], value = save_slider_S2[1,17], label = col_names[17], 
                              min = save_slider_S2[2,17], max = save_slider_S2[3,17], step = 0.1)
        )
      }else if (input$PI == 'QS'){
        
        
        if (!is.null(input$QS1_S2)) save_slider_S2[1,18] <- input$QS1_S2
        if (!is.null(input$QS2_S2)) save_slider_S2[1,19] <- input$QS2_S2
        if (!is.null(input$QS3_S2)) save_slider_S2[1,20] <- input$QS3_S2
        if (!is.null(input$QS4_S2)) save_slider_S2[1,21] <- input$QS4_S2
        if (!is.null(input$QS5_S2)) save_slider_S2[1,22] <- input$QS5_S2
        if (!is.null(input$QS6_S2)) save_slider_S2[1,23] <- input$QS6_S2
        if (!is.null(input$QS7_S2)) save_slider_S2[1,24] <- input$QS7_S2     
        
        
        for(i in PI[[4]]){
          
          save_slider_S2[2,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                           newdata = save_slider_S2[1,])$min, 1)
          
          save_slider_S2[3,i] <- round(OptFilBov::predict_cluster_interval(list_models_lm_i = indicateurs_PI_cor[[i]], 
                                                                           newdata = save_slider_S2[1,])$max, 1)
        }
        
        wellPanel('Vache 1',
                  sliderInput(slider_S2_id[18], value = save_slider_S2[1,18], label = col_names[18], 
                              min = save_slider_S2[2,18], max = save_slider_S2[3,18], step = 0.1),
                  
                  sliderInput(slider_S2_id[19], value = save_slider_S2[1,19], label = col_names[19], 
                              min = save_slider_S2[2,19], max = save_slider_S2[3,19], step = 0.1),
                  
                  sliderInput(slider_S2_id[20], value = save_slider_S2[1,20], label = col_names[20], 
                              min = save_slider_S2[2,20], max = save_slider_S2[3,20], step = 0.1),
                  
                  sliderInput(slider_S2_id[21], value = save_slider_S2[1,21], label = col_names[21], 
                              min = save_slider_S2[2,21], max = save_slider_S2[3,21], step = 0.1),
                  
                  sliderInput(slider_S2_id[22], value = save_slider_S2[1,22], label = col_names[22], 
                              min = save_slider_S2[2,22], max = save_slider_S2[3,22], step = 0.1),
                  
                  sliderInput(slider_S2_id[23], value = save_slider_S2[1,23], label = col_names[23], 
                              min = save_slider_S2[2,23], max = save_slider_S2[3,23], step = 0.1),
                  
                  sliderInput(slider_S2_id[24], value = save_slider_S2[1,24], label = col_names[24], 
                              min = save_slider_S2[2,24], max = save_slider_S2[3,24], step = 0.1)
        )
      }
    }    
  })
  
  
  output$plot_scenario2 <- renderUI({
    if (input$scenario_comparatif){
      plotOutput('plot_2')
    }
  })  
  
  output$plot_tableau <- renderUI({
    if (input$scenario_comparatif){
      wellPanel(tableOutput('comparaison'))
    }
  })
  
  
  #ecoute des valeurs des scenario ----
  
  ecoute_S1 <- reactive({
    if (input$PI == 'EI'){
      list_index <- PI[[1]]
      list_value <- c(input$EI1_S1, input$EI2_S1, input$EI3_S1)
      
    }else if(input$PI == 'QC'){
      list_index <- PI[[2]]
      list_value <- c(input$QC1_S1, input$QC2_S1, input$QC3_S1, input$QC4_S1, 
                      input$QC5_S1, input$QC6_S1,input$QC7_S1)
      
    }else if(input$PI == 'QN'){
      list_index <- PI[[3]]
      list_value <- c(input$QN1_S1, input$QN2_S1, input$QN3_S1, input$QN4_S1, 
                      input$QN5_S1, input$QN6_S1, input$QN7_S1)
      
    }else if(input$PI == 'QS'){
      list_index <- PI[[4]]
      list_value <- c(input$QS1_S1, input$QS2_S1, input$QS3_S1, input$QS4_S1, 
                      input$QS5_S1, input$QS6_S1, input$QS7_S1)
      
    }
    
    return(list(list_index = list_index, list_value = list_value))
  })
  
  ecoute_S2 <- reactive({
    if (input$PI == 'EI'){
      list_index <- PI[[1]]
      list_value <- c(input$EI1_S2, input$EI2_S2, input$EI3_S2)
    }else if(input$PI == 'QC'){
      list_index <- PI[[2]]
      list_value <- c(input$QC1_S2, input$QC2_S2, input$QC3_S2, input$QC4_S2, 
                      input$QC5_S2, input$QC6_S2, input$QC7_S2)
    }else if(input$PI == 'QN'){
      list_index <- PI[[3]]
      list_value <- c(input$QN1_S2, input$QN2_S2, input$QN3_S2, input$QN4_S2, 
                      input$QN5_S2, input$QN6_S2, input$QN7_S2)
    }else if(input$PI == 'QS'){
      list_index <- PI[[4]]
      list_value <- c(input$QS1_S2, input$QS2_S2, input$QS3_S2, input$QS4_S2, 
                      input$QS5_S2, input$QS6_S2, input$QS7_S2)
    }
    
    return(list(list_index = list_index, list_value = list_value))
  })
  
  cow_perf_reac_S1 <- reactive({OptFilBov::predict_cow(cow_simulated = cow0,
                                                       models = models,
                                                       list_index = ecoute_S1()$list_index, 
                                                       list_value = ecoute_S1()$list_value,
                                                       B=3,
                                                       R=10)})
  
  
  cow_perf_reac_S2 <- reactive({OptFilBov::predict_cow(cow_simulated = cow0,
                                                       models = models,
                                                       list_index = ecoute_S2()$list_index, 
                                                       list_value = ecoute_S2()$list_value, 
                                                       B=3, 
                                                       R=10)})
  
  
  output$plot_1 <- renderPlot({
    
    require(OptFilBov)
    plot(cow_perf_reac_S1(), models, choice = input$graph_choice, ylim = c(-2, 2), main = 'Vache 1',
         label_angle = 90, fixed_value = ecoute_S2()$list_index, radial.lim = c(-3, 3), R = 3, 
         bg_par = rgb(242, 103, 0, maxColorValue = 255))
    
  })
  
  output$plot_2 <- renderPlot({
    
    require(OptFilBov)
    plot(cow_perf_reac_S2(), models, choice = input$graph_choice, ylim = c(-2, 2), main = 'Vache 2',
         label_angle = 90, fixed_value = ecoute_S2()$list_index, radial.lim = c(-3, 3), R = 3, 
         bg_par = rgb(242, 103, 0, maxColorValue = 255))
    
  })
  
  
  
  #realisation du tableau ----
  output$comparaison <- renderTable({
    
    #creation du tableau
    tab_res <- matrix(NA, ncol = length(col_names), nrow = 3)
    colnames(tab_res) <- col_names
    rownames(tab_res) <- c('scenario 1', 'scenario 2', 'p-value')
    
    
    for (i in 1:ncol(tab_res)){
      cow_S1 <- cow_perf_reac_S1()
      cow_S2 <- cow_perf_reac_S2()
      attr(cow_S1, "class") <- NULL
      attr(cow_S2, "class") <- NULL
      
      if (i %in% ecoute_S2()$list_index){
        for (k in 1:length(ecoute_S2()$list_index)){
          tab_res[1,ecoute_S2()$list_index[k]] <- ecoute_S1()$list_value[k]
          tab_res[2,ecoute_S2()$list_index[k]] <- ecoute_S2()$list_value[k]
        }
      }else{
        
        all_pred_clusteri_S1 <- predict(model = models[[i]], newdata = as.data.frame(cow_S1), predict.all = TRUE, R = 15)$individual
        all_pred_clusteri_S2 <- predict(model = models[[i]], newdata = as.data.frame(cow_S2), predict.all = TRUE, R = 15)$individual
        
        #put the mean in the tab for the senario 1
        tab_res[1,i] <- round(cow_S1[,i], 2)
        
        #put the mean in the tab for the senario 2
        tab_res[2,i] <- round(cow_S2[,i], 2)
        
        #add the p.value
        if (sum(abs(all_pred_clusteri_S1 - all_pred_clusteri_S2), na.rm = TRUE) != 0){
          if (shapiro.test(all_pred_clusteri_S1 - all_pred_clusteri_S2)$p.value > 0.05){
            
            #remove the pair which contain at least one NA in one of the matrix
            na_pb <- unique(c(attr(na.omit(all_pred_clusteri_S1),"na.action"),
                              attr(na.omit(all_pred_clusteri_S2),"na.action")))
            if (length(na_pb)){
              all_pred_clusteri_S1 <- all_pred_clusteri_S1[-na_pb,]
              all_pred_clusteri_S2 <- all_pred_clusteri_S2[-na_pb,]
            }
            
            tab_res[3,i] <- round(t.test(all_pred_clusteri_S1,
                                         all_pred_clusteri_S2,
                                         paired = TRUE)$p.value, digits = 3)
          }else{
            
            tab_res[3,i] <- round(wilcox.test(all_pred_clusteri_S1,
                                              all_pred_clusteri_S2,
                                              paired = TRUE)$p.value, digits = 3)
          }
        }else tab_res[3,i] <- 1
      }
      
    }
    cbind(colnames(tab_res), t(tab_res))
    
  })
}
