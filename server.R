library(vcd)
library(scales)
source("packages.RData")
source("CountryEdit.RData")
source("Numerization.RData")
source("ResidentsNationality.RData")
metadata1 <- read_excel(path = "Book2.xls",sheet = "Con-var")
metadata2 <- read_excel(path = "Book2.xls",sheet = "Cat-var")
shinyServer
(
  function(input,output)
  {
    
    output$mydata <- renderTable({head(df,28)})
    
    #*******************************
    var_names <- reactive({
      names(dataset_UK_res)
    })
    #*******************************
    categorical_var_names <- reactive({
      names(df[(2:10)])
    })
    numerical_var_names <- reactive({
      names(df[-(2:10)])
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ UK TO OVERSEAS SECTION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$UK_select_plot_type <- renderUI({
      radioButtons(inputId = "Radio_plot_type",
                   label = "Specify the type of PLOT",
                   choices = c("Categorical","Numerical","Categorical-Numerical"),
                   selected = "Categorical")
    })
    output$UK_selected_plot_type <- renderUI({
      switch (input$Radio_plot_type,
              "Categorical" = uiOutput("UK_plot_cat_selection"),
              "Numerical"   = uiOutput("UK_plot_cont_selection"),
              "Categorical-Numerical" = uiOutput("UK_plot_CAT_NUM_selection")
      )
    })
    output$UK_selected_input_var_box <- renderUI({
      switch (input$Radio_plot_type,
              "Categorical" = uiOutput("UK_plot_cat_input"),
              "Numerical" = uiOutput("UK_plot_cont_input"),
              "Categorical-Numerical" = uiOutput("UK_plot_CAT_NUM_input")
      )
      
      
      
    })
    output$UK_PLOTS <- renderUI({
      
      switch (input$Radio_plot_type,
              "Categorical" = switch (input$UK_plot_cat_type,
                                      "Pie Plot" = plotOutput("UKpiePlot"),
                                      "Bar Plot" = plotOutput("UKbarPlot"),
                                      "Spine Plot" = plotOutput("UKspinePlot")
              ),
              "Numerical"   = switch (input$UK_plot_cont_type,
                                      "Histogram" = plotOutput("UKHistPlot"),
                                      "Density Plot" = plotOutput("UKDensPlot") ,
                                      "Scatter Plot" = plotOutput("UKScatPlot")
              ),
              "Categorical-Numerical" = switch (input$UK_plot_cat_num_type,
                                                "Box Plot" = plotOutput("UKBoxPlot"),
                                                "Violin Plot" = plotOutput("UKViolinPlot"),
                                                "Scatter Facet Plot" = plotOutput("UKScatFacetPlot")
              )
              
      )
      
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CATEGORICAL ANALYSIS (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    output$UK_plot_cat_selection <- renderUI({
      
      selectInput(inputId = "UK_plot_cat_type",
                  label = "Select the type of PLOT",
                  choices = c("Bar Plot",
                              "Pie Plot",
                              "Spine Plot"))
      
    })
    output$UK_plot_cat_input <- renderUI({
      switch (input$UK_plot_cat_type,
              "Pie Plot" = uiOutput("UK_Pie_input"),
              "Bar Plot" = fluidRow(box(uiOutput("UK_Bar_input1"),width = 6,height = 120, background = "black"),
                                    box(uiOutput("UK_Bar_input2"),width = 6,height = 120, background = "black")),
              "Spine Plot" = fluidRow(box(uiOutput("UK_Spine_input_x"),width = 6,height = 120, background = "black"),
                                      box(uiOutput("UK_Spine_input_y"),width = 6,height = 120, background = "black"))
      )
      
    })
    output$UK_Pie_input <- renderUI({
      selectInput(inputId = "UK_Pie_variable",
                  label = "Select the variable",
                  choices = categorical_var_names()
                  )
    })
    output$UK_Bar_input1 <- renderUI({
      selectInput(inputId = "UK_Bar_variable1",
                  label = "Select the variable",
                  choices = categorical_var_names()
                  )
    })
    output$UK_Bar_input2 <- renderUI({
      selectInput(inputId = "UK_Bar_variable2",
                  label = "Select the variable",
                  choices = categorical_var_names()
      )
    })
    output$UK_Spine_input_x <- renderUI({
      selectInput(inputId = "UK_Spine_x_variable", 
                  label = "Select the First CATEGORICAL variable (X)",
                  choices = categorical_var_names())
    })
    output$UK_Spine_input_y <- renderUI({
      selectInput(inputId = "UK_Spine_y_variable", 
                  label = "Select the Second CATEGORICAL variable (Y)",
                  choices = categorical_var_names())
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CONTINUOUS ANALYSIS (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    output$UK_plot_cont_selection <- renderUI({
      selectInput(inputId = "UK_plot_cont_type",
                  label = "Select the type of GRAPH",
                  choices = c("Histogram","Density Plot","Scatter Plot"))

    })
    output$UK_plot_cont_input <- renderUI({
      switch (input$UK_plot_cont_type,
              "Histogram" = uiOutput("UK_Hist_input"),
              "Density Plot" = fluidRow(box(uiOutput("UK_Dens_input1"), width = 6, background = "black"),
                                        box(uiOutput("UK_Dens_input2"), width = 6, background = "black")),
              "Scatter Plot" = fluidRow(box(uiOutput("UK_Scat_input1"), width = 6, background = "black"),
                                        box(uiOutput("UK_Scat_input2"), width = 6, background = "black"))
      )

    })
    output$UK_Hist_input <- renderUI({
      selectInput(inputId = "UK_Hist_variable",
                  label = "Select the variable",
                  choices = numerical_var_names()
      )
    })
    output$UK_Dens_input1 <- renderUI({
      selectInput(inputId = "UK_Dens_variable1",
                  label = "Select the first variable",
                  choices = numerical_var_names(),
                  selected = "spend"
      )
    })
    output$UK_Dens_input2 <- renderUI({
      selectInput(inputId = "UK_Dens_variable2",
                  label = "Select the second variable",
                  choices = numerical_var_names(),
                  selected = "visits"
      )
    })
    output$UK_Scat_input1 <- renderUI({
      selectInput(inputId = "UK_Scat_variable1",
                  label = "Select the first variable",
                  choices = numerical_var_names()
      )
    })
    output$UK_Scat_input2 <- renderUI({
      selectInput(inputId = "UK_Scat_variable2",
                  label = "Select the second variable",
                  choices = numerical_var_names()
      )
    })

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CATEGORICAL AND NUMERICAL ANALYSIS (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$UK_plot_CAT_NUM_selection <- renderUI({
      selectInput(inputId = "UK_plot_cat_num_type",
                  label = "Select the type of PLOT",
                  choices = c("Box Plot","Violin Plot","Scatter Facet Plot"))
    })
    output$UK_plot_CAT_NUM_input <- renderUI({
      switch (input$UK_plot_cat_num_type,
              "Box Plot" = fluidRow(box(uiOutput("UK_Box_input1"), width = 6, background = "black"),
                                        box(uiOutput("UK_Box_input2"), width = 6, background = "black")),
              "Violin Plot" = fluidRow(box(uiOutput("UK_Violin_input1"), width = 6, background = "black"),
                                        box(uiOutput("UK_Violin_input2"), width = 6, background = "black")),
              "Scatter Facet Plot" = fluidRow(box(uiOutput("UK_Scat_Facet_input1"), width = 4, background = "black"),
                                              box(uiOutput("UK_Scat_Facet_input2"), width = 4, background = "black"),
                                              box(uiOutput("UK_Scat_Facet_input3"),width = 4, background = "black"))
      )
      
    })
    output$UK_Box_input1 <- renderUI({
      selectInput(inputId = "UK_Box_variable1",
                  label = "Select the CATEGORICAL variable",
                  choices = categorical_var_names()
      )
    })
    output$UK_Box_input2 <- renderUI({
      selectInput(inputId = "UK_Box_variable2",
                  label = "Select the NUMERICAL variable",
                  choices = numerical_var_names()
      )
    })
    output$UK_Violin_input1 <- renderUI({
      selectInput(inputId = "UK_Violin_variable1",
                  label = "Select the CATEGORICAL variable",
                  choices = categorical_var_names()
      )
    })
    output$UK_Violin_input2 <- renderUI({
      selectInput(inputId = "UK_Violin_variable2",
                  label = "Select the NUMERICAL variable",
                  choices = numerical_var_names()
      )
    })
    output$UK_Scat_Facet_input1 <- renderUI({
      selectInput(inputId ="UK_Scat_Facet_variable1",
                  label = "Select the first NUMERICAL variable",
                  choices = numerical_var_names(),
                  selected = "visits")
    })
    output$UK_Scat_Facet_input2 <- renderUI({
      selectInput(inputId ="UK_Scat_Facet_variable2",
                  label = " Select the second NUMERICAL variable",
                  choices = numerical_var_names(),
                  selected = "nights")
    })
    output$UK_Scat_Facet_input3 <- renderUI({
      selectInput(inputId ="UK_Scat_Facet_variable3",
                  label = " Select the CATEGORICAL (FACET) variable",
                  choices = categorical_var_names() )
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ALL PLOTS (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@ Categorical Plots (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$UKpiePlot <- renderPlot({

      pie_values <- table(dataset_UK_res[,input$UK_Pie_variable])
      pie_values_rate <- round(pie_values/sum(pie_values) *100, 2)
      lbls <- paste(names(pie_values), "\n", pie_values_rate," %", sep="")
      pie3D(pie_values_rate,labels = lbls,main="Pie Chart")


    })
    output$UKbarPlot <- renderPlot({
      
      barplot(table(dataset_UK_res[,input$UK_Bar_variable1], dataset_UK_res[,input$UK_Bar_variable2]),
              beside = TRUE, 
              legend= levels(unique(as.factor(dataset_UK_res[,input$UK_Bar_variable1]))),
              col = rainbow(NROW(unique(as.factor(dataset_UK_res[,input$UK_Bar_variable1])))),
              main = paste( "bar plot of ",input$UK_Bar_variable1," and ",input$UK_Bar_variable2 ,"distribution "),
              xlab = input$UK_Bar_variable2
              )

      })
    output$UKspinePlot <- renderPlot({
                   X_var <- as.factor(dataset_UK_res[,input$UK_Spine_x_variable])
                   Y_var <- as.factor(dataset_UK_res[,input$UK_Spine_y_variable])
                   spineplot(x=X_var, y=Y_var,
                           xlab = input$UK_Spine_x_variable, 
                           ylab = input$UK_Spine_y_variable,
                           main="SPINE PLOT",
                           col = rainbow(NROW(unique(as.factor(dataset_UK_res[,input$UK_Spine_y_variable])))))
                           
    })
    #@@@@@@@@@@@@@@@@@@ Numerical Plots (UK) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$UKHistPlot <- renderPlot({
      ggplot(data = dataset_UK_res,aes(x= dataset_UK_res[,input$UK_Hist_variable]))+
        geom_histogram(fill= "black" )+
        # facet_grid(input$UK_Hist_variable~dataset_UK_res$quarter+dataset_UK_res$Age)+
        labs(x = input$UK_Hist_variable, y = "Count", title= paste("Histogram of ", input$UK_Hist_variable)) +
        scale_x_continuous(labels = comma) + 
        scale_y_continuous(labels = comma)
        
    })
    output$UKDensPlot <- renderPlot({
      dv1 = density(scale(dataset_UK_res[,input$UK_Dens_variable1]))
      dv2 = density(scale (dataset_UK_res[,input$UK_Dens_variable2]))
      plot(range(dv1$x, dv2$x), range(dv1$y, dv2$y), type = "n", 
           xlab = paste("Density plot of ",input$UK_Dens_variable1," and ",input$UK_Dens_variable2),
           ylab = "Density")
      lines(dv1, col = "red")
      lines(dv2, col = "blue")
    })
    output$UKScatPlot <- renderPlot({
      radius <- sqrt(scale(dataset_UK_res$spend)/3.14)
      symbols(x = dataset_UK_res[,input$UK_Scat_variable1], 
              y = dataset_UK_res[,input$UK_Scat_variable2],
              xlab = input$UK_Scat_variable1,
              ylab = input$UK_Scat_variable2,
              circles = radius, inches = 0.25, fg = "white", 
              bg = rainbow(12), main = "Sized by SPEND variable")
    })
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@ Categorical and Numerical Plots (UK) @@@@@@@@@@@@@@@
    output$UKBoxPlot <- renderPlot({
      xbox <- dataset_UK_res
      colcode <- NROW(levels(factor(xbox[,input$UK_Box_variable1])))
      ggplot(xbox,aes(y= xbox[,input$UK_Box_variable2],x = factor(xbox[,input$UK_Box_variable1]))) + 
        geom_boxplot(color="black", fill=rainbow(colcode))+
        xlab(input$UK_Box_variable1)+
        ylab(input$UK_Box_variable2)+
        scale_y_continuous(labels = comma)
    })
    output$UKViolinPlot <- renderPlot({
      xviolin <- dataset_UK_res
      col_code <- NROW(levels(factor(xviolin[,input$UK_Violin_variable1])))
      ggplot(xviolin,aes(y= xviolin[,input$UK_Violin_variable2],x = factor(xviolin[,input$UK_Violin_variable1]))) +
        geom_violin(color="blue", fill="black")+
        xlab(input$UK_Violin_variable1)+
        ylab(input$UK_Violin_variable2)+
        scale_y_continuous(labels = comma)
    })
    output$UKScatFacetPlot <- renderPlot({
       colcode_fac <-dataset_UK_res[,input$UK_Scat_Facet_variable3]
      
        ggplot(dataset_UK_res,aes(x=dataset_UK_res[,input$UK_Scat_Facet_variable1], 
                                y=dataset_UK_res[,input$UK_Scat_Facet_variable2]
                                )) + 
        geom_point() + facet_wrap(. ~ dataset_UK_res[,input$UK_Scat_Facet_variable3], scales = "free_y")+
        xlab(input$UK_Scat_Facet_variable1)+
        ylab(input$UK_Scat_Facet_variable2)+
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) 
        
        
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END OF UK @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ OVERSEAS TO UK SECTION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$OS_select_plot_type <- renderUI({
      radioButtons(inputId = "OS_Radio_plot_type",
                   label = "Specify the type of PLOT",
                   choices = c("Categorical","Numerical","Categorical-Numerical"),
                   selected = "Categorical")
    })
    output$OS_selected_plot_type <- renderUI({
      switch (input$OS_Radio_plot_type,
              "Categorical" = uiOutput("OS_plot_cat_selection"),
              "Numerical"   = uiOutput("OS_plot_cont_selection"),
              "Categorical-Numerical" = uiOutput("OS_plot_CAT_NUM_selection")
      )
    })
    output$OS_selected_input_var_box <- renderUI({
      switch (input$OS_Radio_plot_type,
              "Categorical" = uiOutput("OS_plot_cat_input"),
              "Numerical" = uiOutput("OS_plot_cont_input"),
              "Categorical-Numerical" = uiOutput("OS_plot_CAT_NUM_input")
      )
      
      
      
    })
    output$OS_PLOTS <- renderUI({

      switch (input$OS_Radio_plot_type,
              "Categorical" = switch (input$OS_plot_cat_type,
                                      "Pie Plot" = plotOutput("OSpiePlot"),
                                      "Bar Plot" = plotOutput("OSbarPlot"),
                                      "Spine Plot" = plotOutput("OSspinePlot")
              ),
              "Numerical"   = switch (input$OS_plot_cont_type,
                                      "Histogram" = plotOutput("OSHistPlot"),
                                      "Density Plot" = plotOutput("OSDensPlot") ,
                                      "Scatter Plot" = plotOutput("OSScatPlot")
              ),
              "Categorical-Numerical" = switch (input$OS_plot_cat_num_type,
                                                "Box Plot" = plotOutput("OSBoxPlot"),
                                                "Violin Plot" = plotOutput("OSViolinPlot"),
                                                "Scatter Facet Plot" = plotOutput("OSScatFacetPlot")
              )

      )

    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    output$OS_plot_cat_selection <- renderUI({
      
      selectInput(inputId = "OS_plot_cat_type",
                  label = "Select the type of PLOT",
                  choices = c("Bar Plot",
                              "Pie Plot",
                              "Spine Plot"))
      
    })
    output$OS_plot_cat_input <- renderUI({
      switch (input$OS_plot_cat_type,
              "Pie Plot" = uiOutput("OS_Pie_input"),
              "Bar Plot" = fluidRow(box(uiOutput("OS_Bar_input1"),width = 6,height = 120, background = "black"),
                                    box(uiOutput("OS_Bar_input2"),width = 6,height = 120, background = "black")),
              "Spine Plot" = fluidRow(box(uiOutput("OS_Spine_input_x"),width = 6,height = 120, background = "black"),
                                      box(uiOutput("OS_Spine_input_y"),width = 6,height = 120, background = "black"))
      )
      
    })
    output$OS_Pie_input <- renderUI({
      selectInput(inputId = "OS_Pie_variable",
                  label = "Select the variable",
                  choices = categorical_var_names()
      )
    })
    output$OS_Bar_input1 <- renderUI({
      selectInput(inputId = "OS_Bar_variable1",
                  label = "Select the variable",
                  choices = categorical_var_names()
      )
    })
    output$OS_Bar_input2 <- renderUI({
      selectInput(inputId = "OS_Bar_variable2",
                  label = "Select the variable",
                  choices = categorical_var_names()
      )
    })
    output$OS_Spine_input_x <- renderUI({
      selectInput(inputId = "OS_Spine_x_variable", 
                  label = "Select the First CATEGORICAL variable (X)",
                  choices = categorical_var_names())
    })
    output$OS_Spine_input_y <- renderUI({
      selectInput(inputId = "OS_Spine_y_variable", 
                  label = "Select the Second CATEGORICAL variable (Y)",
                  choices = categorical_var_names())
    })
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    output$OS_plot_cont_selection <- renderUI({
      selectInput(inputId = "OS_plot_cont_type",
                  label = "Select the type of GRAPH",
                  choices = c("Histogram","Density Plot","Scatter Plot"))
      
    })
    output$OS_plot_cont_input <- renderUI({
      switch (input$OS_plot_cont_type,
              "Histogram" = uiOutput("OS_Hist_input"),
              "Density Plot" = fluidRow(box(uiOutput("OS_Dens_input1"), width = 6, background = "black"),
                                        box(uiOutput("OS_Dens_input2"), width = 6, background = "black")),
              "Scatter Plot" = fluidRow(box(uiOutput("OS_Scat_input1"), width = 6, background = "black"),
                                        box(uiOutput("OS_Scat_input2"), width = 6, background = "black"))
      )
      
    })
    output$OS_Hist_input <- renderUI({
      selectInput(inputId = "OS_Hist_variable",
                  label = "Select the variable",
                  choices = numerical_var_names()
      )
    })
    output$OS_Dens_input1 <- renderUI({
      selectInput(inputId = "OS_Dens_variable1",
                  label = "Select the first variable",
                  choices = numerical_var_names(),
                  selected = "spend"
      )
    })
    output$OS_Dens_input2 <- renderUI({
      selectInput(inputId = "OS_Dens_variable2",
                  label = "Select the second variable",
                  choices = numerical_var_names(),
                  selected = "visits"
      )
    })
    output$OS_Scat_input1 <- renderUI({
      selectInput(inputId = "OS_Scat_variable1",
                  label = "Select the first variable",
                  choices = numerical_var_names()
      )
    })
    output$OS_Scat_input2 <- renderUI({
      selectInput(inputId = "OS_Scat_variable2",
                  label = "Select the second variable",
                  choices = numerical_var_names()
      )
    })
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$OS_plot_CAT_NUM_selection <- renderUI({
      selectInput(inputId = "OS_plot_cat_num_type",
                  label = "Select the type of PLOT",
                  choices = c("Box Plot","Violin Plot","Scatter Facet Plot"))
    })
    output$OS_plot_CAT_NUM_input <- renderUI({
      switch (input$OS_plot_cat_num_type,
              "Box Plot" = fluidRow(box(uiOutput("OS_Box_input1"), width = 6, background = "black"),
                                    box(uiOutput("OS_Box_input2"), width = 6, background = "black")),
              "Violin Plot" = fluidRow(box(uiOutput("OS_Violin_input1"), width = 6, background = "black"),
                                       box(uiOutput("OS_Violin_input2"), width = 6, background = "black")),
              "Scatter Facet Plot" = fluidRow(box(uiOutput("OS_Scat_Facet_input1"), width = 4, background = "black"),
                                              box(uiOutput("OS_Scat_Facet_input2"), width = 4, background = "black"),
                                              box(uiOutput("OS_Scat_Facet_input3"),width = 4, background = "black"))
      )
      
    })
    output$OS_Box_input1 <- renderUI({
      selectInput(inputId = "OS_Box_variable1",
                  label = "Select the CATEGORICAL variable",
                  choices = categorical_var_names()
      )
    })
    output$OS_Box_input2 <- renderUI({
      selectInput(inputId = "OS_Box_variable2",
                  label = "Select the NUMERICAL variable",
                  choices = numerical_var_names()
      )
    })
    output$OS_Violin_input1 <- renderUI({
      selectInput(inputId = "OS_Violin_variable1",
                  label = "Select the CATEGORICAL variable",
                  choices = categorical_var_names()
      )
    })
    output$OS_Violin_input2 <- renderUI({
      selectInput(inputId = "OS_Violin_variable2",
                  label = "Select the NUMERICAL variable",
                  choices = numerical_var_names()
      )
    })
    output$OS_Scat_Facet_input1 <- renderUI({
      selectInput(inputId ="OS_Scat_Facet_variable1",
                  label = "Select the first NUMERICAL variable",
                  choices = numerical_var_names(),
                  selected = "visits")
    })
    output$OS_Scat_Facet_input2 <- renderUI({
      selectInput(inputId ="OS_Scat_Facet_variable2",
                  label = " Select the second NUMERICAL variable",
                  choices = numerical_var_names(),
                  selected = "nights")
    })
    output$OS_Scat_Facet_input3 <- renderUI({
      selectInput(inputId ="OS_Scat_Facet_variable3",
                  label = " Select the CATEGORICAL (FACET) variable",
                  choices = categorical_var_names() )
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ALL PLOTS (OS) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@ Categorical Plots (OS) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$OSpiePlot <- renderPlot({
      
      pie_values <- table(dataset_Overseas_res[,input$OS_Pie_variable])
      pie_values_rate <- round(pie_values/sum(pie_values) *100, 2)
      lbls <- paste(names(pie_values), "\n", pie_values_rate," %", sep="")
      pie3D(pie_values_rate,labels = lbls,main="Pie Chart")
      
      
    })
    output$OSbarPlot <- renderPlot({
      
      barplot(table(dataset_Overseas_res[,input$OS_Bar_variable1], dataset_Overseas_res[,input$OS_Bar_variable2]),
              beside = TRUE, 
              legend= levels(unique(as.factor(dataset_Overseas_res[,input$OS_Bar_variable1]))),
              col = rainbow(NROW(unique(as.factor(dataset_Overseas_res[,input$OS_Bar_variable1])))),
              main = paste( "bar plot of ",input$OS_Bar_variable1," and ",input$OS_Bar_variable2 ,"distribution "),
              xlab = input$OS_Bar_variable2
      )
      
    })
    output$OSspinePlot <- renderPlot({
      X_var <- as.factor(dataset_Overseas_res[,input$OS_Spine_x_variable])
      Y_var <- as.factor(dataset_Overseas_res[,input$OS_Spine_y_variable])
      spineplot(x=X_var, y=Y_var,
                xlab = input$OS_Spine_x_variable, 
                ylab = input$OS_Spine_y_variable,
                main="SPINE PLOT",
                col = rainbow(NROW(unique(as.factor(dataset_Overseas_res[,input$OS_Spine_y_variable])))))
      
    })
    #@@@@@@@@@@@@@@@@@@ Numerical Plots (OS) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$OSHistPlot <- renderPlot({
      ggplot(data = dataset_Overseas_res,aes(x= dataset_Overseas_res[,input$OS_Hist_variable]))+
        geom_histogram(fill= "black" )+
        # facet_grid(input$UK_Hist_variable~dataset_UK_res$quarter+dataset_UK_res$Age)+
        labs(x = input$OS_Hist_variable, y = "Count", title= paste("Histogram of ", input$OS_Hist_variable))
      
    })
    output$OSDensPlot <- renderPlot({
      dv1 = density(scale(dataset_Overseas_res[,input$OS_Dens_variable1]))
      dv2 = density(scale (dataset_Overseas_res[,input$OS_Dens_variable2]))
      plot(range(dv1$x, dv2$x), range(dv1$y, dv2$y), type = "n", 
           xlab = paste("Density plot of ",input$OS_Dens_variable1," and ",input$OS_Dens_variable2),
           ylab = "Density")
      lines(dv1, col = "red")
      lines(dv2, col = "blue")
    })
    output$OSScatPlot <- renderPlot({
      radius <- sqrt(scale(dataset_Overseas_res$spend)/3.14)
      symbols(x = dataset_Overseas_res[,input$OS_Scat_variable1], 
              y = dataset_Overseas_res[,input$OS_Scat_variable2],
              xlab = input$OS_Scat_variable1,
              ylab = input$OS_Scat_variable2,
              circles = radius, inches = 0.25, fg = "white", 
              bg = rainbow(12), main = "Sized by SPEND variable")
    })
    #@@@@@@@@@@@@@@@@@@ Categorical and Numerical Plots (OS) @@@@@@@@@@@@@@@
    output$OSBoxPlot <- renderPlot({
      xbox <- dataset_Overseas_res
      colcode <- NROW(levels(factor(xbox[,input$OS_Box_variable1])))
      ggplot(xbox,aes(y= xbox[,input$OS_Box_variable2],x = factor(xbox[,input$OS_Box_variable1]))) + 
        geom_boxplot(color="black", fill=rainbow(colcode))+
        xlab(input$OS_Box_variable1)+
        ylab(input$OS_Box_variable2)
    })
    output$OSViolinPlot <- renderPlot({
      xviolin <- dataset_Overseas_res
      col_code <- NROW(levels(factor(xviolin[,input$OS_Violin_variable1])))
      ggplot(xviolin,aes(y= xviolin[,input$OS_Violin_variable2],x = factor(xviolin[,input$OS_Violin_variable1]))) +
        geom_violin(color="blue", fill="black")+
        xlab(input$OS_Violin_variable1)+
        ylab(input$OS_Violin_variable2)
    })
    output$OSScatFacetPlot <- renderPlot({
      colcode_fac <-dataset_Overseas_res[,input$OS_Scat_Facet_variable3]
      
      ggplot(dataset_Overseas_res,aes(x=dataset_Overseas_res[,input$OS_Scat_Facet_variable1], 
                                y=dataset_Overseas_res[,input$OS_Scat_Facet_variable2]
      )) + 
        geom_point() + facet_wrap(. ~ dataset_Overseas_res[,input$OS_Scat_Facet_variable3], scales = "free_y")+
        xlab(input$OS_Scat_Facet_variable1)+
        ylab(input$OS_Scat_Facet_variable2)+
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) 
      
      
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Recommendation Section @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$recom_sys_input <- renderUI({
      fluidRow(
        box(radioButtons(inputId = "radio_residency",
                         label = "Where do you live ",
                         choices = c("I am living in UK", "I am living overseas"),
                         selected = "I am living overseas"),
            width = 2,background = "black", height = 110),
        box(uiOutput("country"), width = 2, background = "black",height = 110),
        box(uiOutput("purpose"), width = 2, background = "black",height = 110),
        box(uiOutput("traveler_age"), width = 2, background = "black",height = 110),
        box(uiOutput("visit_time"), width = 2, background = "black",height = 110),
        box(uiOutput("travel_duration"), width = 2, background = "black",height = 110),
        box(uiOutput("traveler_gender"), width = 2, background = "black",height = 110),
        box(uiOutput("travel_package"), width = 2, background = "black",height = 110),
        box(uiOutput("travel_option"), width = 2, background = "black",height = 110)
               
               )

     

    })
    output$recom_sys_output <- renderUI({
      switch (input$radio_residency,
              "I am living overseas" = valueBoxOutput("overseas_recommendation"),
              "I am living in UK" = valueBoxOutput("UK_recommendation")
      )
    })
    output$overseas_recommendation <- renderValueBox({
      dfdf <- dataset_Overseas_res
      dfdf <- dfdf[,c(2,4,5,6,7,8,9,10,13)]
      for (i in 1:8) {
        if(is.character(dfdf[,i]))
          dfdf[,i] <- factor(dfdf[,i])
        
      }
      library(rpart)
      library(caTools)
      split <- sample.split(Y = dfdf$spend,SplitRatio = .95)
      train_set <- subset(x = dfdf,split==TRUE)
      test_set <- subset(x = dfdf, split==FALSE)
      model <- rpart(formula = spend ~ ., data = train_set, method = "anova")
      
      new_entry <- as.data.frame(list(quarter=as.factor(input$travel_time),
                                      mode = as.factor(input$traveler_option),
                                      country = as.factor(input$overseas_to_UK_country),
                                      purpose = as.factor(input$travel_purpose),
                                      package = as.factor(input$package),
                                      Age = as.factor(input$age),
                                      Sex = as.factor(input$traveler_sex),
                                      duration = as.factor(input$duration)
      ))
      

      valueBox(value =paste(round(predict(model,new_entry),2),"£"),subtitle = "WOULD BE THE APPROXIMATE SPEND",width = 3 )
    })
    output$UK_recommendation <- renderValueBox({
      dfdf <- dataset_UK_res
      dfdf <- dfdf[,c(2,4,5,6,7,8,9,10,13)]
      for (i in 1:8) {
        if(is.character(dfdf[,i]))
          dfdf[,i] <- factor(dfdf[,i])
        
      }
      library(rpart)
      library(caTools)
      split <- sample.split(Y = dfdf$spend,SplitRatio = .95)
      train_set <- subset(x = dfdf,split==TRUE)
      test_set <- subset(x = dfdf, split==FALSE)
      model <- rpart(formula = spend ~ ., data = train_set, method = "anova")
      
      new_entry <- as.data.frame(list(quarter=as.factor(input$travel_time),
                                      mode = as.factor(input$traveler_option),
                                      country = as.factor(input$UK_to_overseas_country),
                                      purpose = as.factor(input$travel_purpose),
                                      package = as.factor(input$package),
                                      Age = as.factor(input$age),
                                      Sex = as.factor(input$traveler_sex),
                                      duration = as.factor(input$duration)
      ))

      valueBox(value =paste(round(predict(model,new_entry),2),"£"),subtitle = "WOULD BE THE APPROXIMATE SPEND",width = 3 )
    })
    #**********************************************
    output$country <- renderUI({
      switch (input$radio_residency,
              "I am living overseas" = uiOutput("from_country_input"),
              "I am living in UK" = uiOutput("to_country_input")
      )
      
    })
    output$from_country_input <- renderUI({
      selectInput(inputId = "overseas_to_UK_country",label = "Select the country you are comming from",
                  choices = dataset_Overseas_res$country,
                  selected = "Asia")
    })
    output$to_country_input <- renderUI({
      selectInput(inputId = "UK_to_overseas_country",label = "Select the country you intend to visit",
                  choices = dataset_UK_res$country,
                  selected = "Asia")
    })
    output$visit_time <- renderUI({
      selectInput(inputId = "travel_time",label = "When do you want to visit",
                  choices = dataset_Overseas_res$quarter,
                  selected = "Jan-Mar")
    })
    output$purpose <- renderUI({
      selectInput(inputId = "travel_purpose",label = "What is the purpose of travel",
                  choices = dataset_Overseas_res$purpose,
                  selected = "Holiday")
    })
    output$traveler_gender <- renderUI({
      radioButtons(inputId = "traveler_sex",label = "What is your gender", choices =c("Male", "Female") )
    })
    output$travel_package <- renderUI({
      radioButtons(inputId = "package",label = "Are you traveling with family (Non-idependent) or not (independent)",
                   choices = c("Independent","Non-Independent"))
    })
    output$traveler_age <- renderUI({
      selectInput(inputId = "age",label = "How old are you",choices = dataset_Overseas_res$Age,
                  selected = "25-34")
    })
    output$travel_option <- renderUI({
      radioButtons(inputId = "traveler_option",label = "How do you prefer to travel",
                   choices = c("Air","Sea","Tunnel"))
    })
    output$travel_duration <- renderUI({
      selectInput(inputId = "duration",label = "How long do you want to stay",
                  choices = dataset_Overseas_res$duration,
                  selected = "1-3 nights")
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    output$structure <- renderPrint(
      str(df)
    )
    #**************************
    output$summary <- renderPrint(
      summary(df)
    )
    #*******************************************************************
    output$desc <- renderText(paste("Travelpac is a simplified version of the IPS database containing
                                    14 of the most widely used categorical and continuous variables"))
     output$metadata1 <- renderTable({metadata1})
     output$metadata2 <- renderTable({metadata2})
     output$cat_ <- renderInfoBox({
      infoBox(title = "", value = tableOutput("metadata1"),color = "black",icon = icon("clipboard"))
    })
    output$con_ <- renderInfoBox({
      infoBox(title = "", value = tableOutput("metadata2"),color = "black",icon = icon("clipboard"))
    })
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    # *****************************************************************
    
  }
)