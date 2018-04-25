# library(shiny)
# library(miniUI)

#source("physiologyHelpers.R")
#'@export
iviveGadget <- function(save_flag = F,base_path = NULL){

  ui <- miniPage(
    shinyjs::useShinyjs(),
    gadgetTitleBar("Perform In-vitro to In-vivo Extrapolation",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done","Done")),
    miniTabstripPanel(
      miniTabPanel("Physiological Parameters",
                   miniContentPanel(
                     fillCol(flex = c(1,1,6),
                             fillRow(flex = c(16,2,3,1),
                                     fillRow(
                                       numericInput("age","Reference Age(Years)",25,0.1,80,0.1),
                                       selectInput("gender","Gender",choice = c("Male"="M","Female"="F")),
                                       numericInput("bw","Body Weight(kg)",80.15),
                                       numericInput("liv_wt","Liver Weight(kg)",1.5)

                                     ),

                               tags$div(),
                               fillRow(
                                       shinyWidgets::searchInput("out_age","Additional output ages",btnSearch = icon("plus"))),

                               tags$div()
                             ),
                             fillRow(flex = c(8,14),
                                     fillRow(
                                       numericInput("km","Michelis-Menten Constant",0),
                                       numericInput("hpgl","10^6 Hepatocytes/gm Liver",137)

                                     ),
                                     tags$div()
                                     ),
                             fillRow(flex = c(0.5,2,0.5,8,1,2,1),
                                     tags$div(),
                                     DT::dataTableOutput("mgpglTble"),
                                     tags$div(),
                                     DT::dataTableOutput("cypDb"),
                                     tags$div(),
                                     tags$div(
                                       DT::dataTableOutput("ageTble"),
                                       actionButton("rem_row","Remove Row")
                                     ),

                                     tags$div()


                             )
                     )



                   )
                   ),
      miniTabPanel("Cellular Clearance \n in Reference Age",
                   miniContentPanel(
                     fillCol(flex = c(1,2,1),
                             fillRow(flex = c(2,1,2),
                                     tags$div(),
                                     selectInput("heptype","Hepatic Clearance",
                                                 choices= list("None"="hep_null",
                                                               "Whole Hepatocyte"= "hep_whole",
                                                               "S9 Fraction" = "hep_s9",
                                                               "Subcellular Fraction"= "hep_sc",
                                                               "Premeasured Value"="hep_fnl")),
                                     tags$div()
                                     ),
                             fillRow(flex = c(1,1),
                                     fillCol(
                                       fillRow(flex = c(1,3,3,1),
                                               tags$div(),

                                               numericInput("num_whcl",
                                                            "Measure Whole Hepatocyte Clearance",0),
                                               selectInput("sel_whunit",label = "Units",
                                                           choices = list("L/h"="Lh",
                                                                          "L/h/10^6 Hepatocytes"="LhmH")),
                                               tags$div()
                                       ),
                                       fillRow(flex = c(1,3,3,1),
                                               tags$div(),
                                               numericInput("num_S9cl",
                                                            "Measured S9 Fraction Clearance",0),
                                               selectInput("sel_s9unit",label = "Units",
                                                           choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                          "\u03BCL/h/mg Protein"="ulhmP",
                                                                          "mL/min/mg Protein"="mlmmP",
                                                                          "mL/h/mg Protein"="mlhmP")),
                                               tags$div()
                                       )

                                     ),
                                     fillCol(
                                       fillRow(flex = c(1,3,3,1),
                                               tags$div(),
                                               numericInput("num_mscl","Measure Microsomal Clearance",0),
                                               selectInput("sel_msunit",label = "Units",
                                                           choices = list("\u03BCmol/min/mg Protein"="ummmP",
                                                                          "\u03BCL/min/mg Protein"="ulmmP",
                                                                          "\u03BCL/h/mg Protein"="ulhmP",
                                                                          "mL/min/mg Protein"="mlmmP",
                                                                          "mL/h/mg Protein"="mlhmP")),
                                               tags$div()
                                       ),
                                       fillRow(flex = c(1,3,3,1),
                                               tags$div(),
                                               numericInput("num_cycl","Measure Cytosolic Clearance",0),
                                               selectInput("sel_cyunit",label = "Units",
                                                           choices = list("\u03BCL/min/mg Protein"="ulmmP",
                                                                          "\u03BCL/h/mg Protein"="ulhmP",
                                                                          "mL/min/mg Protein"="mlmmP",
                                                                          "mL/h/mg Protein"="mlhmP")),
                                               tags$div()
                                       )
                                     )

                             ),
                             fillRow(flex = c(2,1,2),
                               tags$div(),
                               numericInput("num_pmcl","Precalculated Scaled clearance(L/h)",0),
                               tags$div()
                             )
                     )
                   )),
      miniTabPanel("Enzymatic Clearance \n in Reference Age",
                  miniContentPanel(
                    fillCol(flex = c(3,1),
                            # fillRow(flex=c(6,1,6),
                            #         tags$div(),
                            #         tags$h4("clearance in uL/min/pmol"),
                            #         tags$div()
                            # ),
                            fillRow(flex = c(2,3,2),
                                    tags$div(),
                                    DT::DTOutput("cypCl"),
                                    tags$div()
                                    ),
                            fillRow(flex = c(1,2,5),
                              fileInput("cypCl_upload","Upload Cyp clearance",
                                        multiple = F,placeholder = "Select CSV File",
                                        buttonLabel = icon("search"),
                                        accept = c("text/csv")
                                        ),
                              downloadLink("cypCl_temp","Template for the CSV file"),
                              tags$div()
                              )
                            )
                    )
                  ),
      miniTabPanel("Output",
                   miniContentPanel(
                     fillCol(flex = c(1,7,4),
                             fillRow(
                                     actionButton("run","Do IVIVE")
                                     ),
                             fillRow(flex= c(1,4,1,4,1),
                                     tags$div(),
                                     DT::DTOutput("out_heptble"),
                                     tags$div(),
                                     DT::DTOutput("out_enztble"),
                                     tags$div()),
                             fillRow(
                               fillCol(flex = c(1,1),
                                       fillRow(
                                               shinyWidgets::radioGroupButtons("ret_data","Select Data to return",
                                                                                  choiceNames = c("None",
                                                                                                  "Total clearance at reference age",
                                                                                                  "Total Clearance at all ages",
                                                                                                  "Detailed clearance data"),
                                                                                  choiceValues = c("none","cl_ref",
                                                                                                   "cl_all",
                                                                                                   "cl_detail"),
                                                                                  checkIcon = list(yes = icon("ok",lib = "glyphicon")),
                                                                                  individual = T,
                                                                                  justified = T)

                                       ),
                                       # fillRow(flex = c(2,NA),
                                       #         tags$h4("Downloads"),
                                       #         tags$div()),
                                       fillRow(
                                           shinyjs::hidden(textInput("fname_vmax", "File name for scaled Vmax values")),
                                           shinyjs::hidden(textInput("fname_vliv","File name for scaled Liver Clerance"))
                                         )
                               )
                             )
                             )

                     #textOutput("cl_val")

                   ))


    )
  )
  server <- function(input,output,session){
    if(save_flag){
      shinyjs::show("fname_vmax")
      shinyjs::show("fname_vliv")
    }
    observe({
      hep_type <- input$heptype
      hep_ids <- c("num_whcl","sel_whunit","num_S9cl",
                   "sel_s9unit","num_mscl","sel_msunit",
                   "num_cycl","sel_cyunit","num_pmcl")
      lapply(hep_ids,shinyjs::disable)
      if(hep_type == "hep_whole"){
        lapply(c("num_whcl","sel_whunit"),shinyjs::enable)
      }else if(hep_type == "hep_s9"){
        lapply(c("num_s9cl","sel_s9unit"),shinyjs::enable)
      }else if(hep_type == "hep_sc"){
        lapply(c("num_mscl","sel_msunit","num_cycl","sel_cyunit"),shinyjs::enable)
      }else if(hep_type == "hep_fnl"){
        lapply(c("num_pmcl"),shinyjs::enable)
      }
    })


    #Get Cyp Data from main database
    query <- "SELECT name,abundance,isef,fumic,loc FROM CypData;"
    cypdata <- mainDbSelect(query)
   # cypCl <- reactiveVal(data.frame("Names"= cypdata[["name"]],"Clearance"= rep(0,length(cypdata[["name"]])),stringsAsFactors = F))
    #Create reactive object to store changing values
    react_obj <- reactiveValues()
    react_obj$SCData <- list()
    react_obj$text_list <- reactiveVal(c())
    react_obj$ageTble <- reactive({vector()})
    react_obj$SCData <- reactive({
      MPCPPGL <- calcMPCPPGL(input$age)
      #MPCPPGL <- as.vector(MPCPPGL)
      MPPGL <- signif(MPCPPGL$MPPGL,4)
      CPPGL <- signif(MPCPPGL$CPPGL,4)
      return(data.frame("Fraction"=c("Microsomal","Cytosolic"),
                        "Values"=c(MPPGL,CPPGL)))
    })
    observe({
      age <- input$age
      gender <- input$gender
      liv_wt <- getLifecourseLiverVolume(age,gender)
      bw <- getLifecourseBodyWeight(age,gender)
      updateNumericInput(session,"liv_wt",value = signif(liv_wt,4))
      updateNumericInput(session,"bw",value = signif(bw,4))
    })
    output$mgpglTble <- DT::renderDT(DT::datatable(react_obj$SCData(),
                                                   caption = "Proteins/gm Liver Values",
                                                   rownames = NULL,autoHideNavigation = T,
                                                   options= list(dom = "t")),server = T)
    cypDb <- reactive({
      data <- as.data.frame(getAllCypData(input$age),stringsAsFactors = F)
      data <- merge(cypdata,data,by.x = "name",by.y = "Enzymes")
      data[["Ontogeny"]]<- signif(data[["Ontogeny"]],4)


      return(data)
    })
    output$cypDb <- DT::renderDT(DT::datatable(cypDb(),
                                               caption = "CYP Data",
                                               rownames = NULL,autoHideNavigation = T,
                                               colnames = c("Name","Abundance","ISEF","fumic","Location","Ontogeny"),
                                               options= list(dom = "tp",pageLength = 5)),server = T)

    observeEvent(input$out_age_search,{
      temp <- gsub("[';',' ']",",",input$out_age)
      out_age_vector <-do.call("as.numeric",(strsplit(temp,",")))
      req(!(any(is.na(out_age_vector))))

      current_list <- react_obj$ageTble()
      react_obj$ageTble <- reactiveVal(append(current_list,out_age_vector))

    },ignoreInit = T,ignoreNULL = T)
    output$ageTble <- DT::renderDT(DT::datatable(as.data.frame(react_obj$ageTble()),
                                                 caption = "Output Ages",
                                                 colnames = "Age",rownames = NULL,
                                                 options= list(dom = "tp",pageLength = 5)),
                                   server = T)
    observeEvent(input$rem_row,{
      selected_row <- input$ageTble_rows_selected
      current_list <- react_obj$ageTble()

      react_obj$ageTble <<- reactiveVal(current_list[-selected_row])
    })
    # download handler for CSV file template
    output$cypCl_temp <- downloadHandler(
      filename= function(){return("Cyp_template.csv")},
      content = function(file){
        data <- data.frame("Names"= cypdata[["name"]],
                           "Clearance"= rep(0,length(cypdata[["name"]])),
                           stringsAsFactors = F)
        write.csv(data,file,row.names = F)
      },
      contentType = c("text/csv")
      )
    # The selected file
    cypFile <- reactive({
      input$cypCl_upload
    })

    # The user's data, parsed into a data frame
    cypCl <- reactive({
      if(!(is.null(input$cypCl_upload))){
        ret_dat <- read.csv(cypFile()$datapath)
      }else{
       ret_dat <- data.frame("Names"= cypdata[["name"]],"Clearance"= rep(0,length(cypdata[["name"]])),stringsAsFactors = F)
      }



      return(ret_dat)

    })


    output$cypCl <- DT::renderDT(DT::datatable(cypCl(),
                                               caption = "clearance in \u00B5L/min/pmol",
                                               rowname = NULL,
                                               options= list(dom = "tp",pageLength = 5)),
                                 server = T
                                 )

    cypcl_proxy = DT::dataTableProxy('cypCl')

    # observeEvent(input$cypCl_cell_edit,{
    #   databck <- input$cypCl_cell_edit
    #   row <- databck$row
    #   col <- databck$col+1
    #   val <- databck$value
    #   old_table<- cypCl()
    #   old_table[row,col]<- val
    #   cypCL <<- old_table
    #   DT::replaceData(cypcl_proxy,cypCl(),resetPaging = F,rownames = F)
    # })

    result_vals <- eventReactive(input$run,{
      hepcl_type <- input$heptype
      org <- "human"
      age <- input$age
      liver_wt<- input$liv_wt
      hpgl <- input$hpgl
      km <- input$km
      scaled_hepcl <- switch(hepcl_type,
                             "hep_sc"=calculateScaledSCClearance(c(input$num_mscl,input$num_cycl),
                                                                 c(input$sel_msunit,input$sel_cyunit),
                                                                 org,age,
                                                                 liver_wt,km,return_total = F),
                             "hep_s9"=calculateScaledS9Clearance(input$num_s9cl,input$sel_s9unit,
                                                                 org,age,
                                                                 liver_wt),
                             "hep_whole"=calculateScaledWholeHepClearance(input$num_whcl,input$sel_whunit,
                                                                          liver_wt,hpgl,km),
                                                
                             "hep_fnl" = input$num_pmcl,
                             0

                             )

     # hep_vals_2_return <- do.call(cbind,list(names(scaled_hepcl),scaled_hepcl))

      recombList <- lapply(cypCl()$Clearance,'as.numeric')

      names(recombList)<- as.list(cypCl()$Names)
      scaled_recomcl <- calculateRecombClearance(recombList,"human",age,liver_wt,cypDb(),return_total = F)
      # get the ages for which scaling has to happen
      out_ages <- react_obj$ageTble()
      if((length(out_ages)==0)){
        out_ages <- c(age)
      }else{
        out_ages <- c(age,out_ages)
      }
      tot_scaled_recomcl <- sum(unname(scaled_recomcl))/liver_wt
      tot_scaled_hepcl <- sum(unname(scaled_hepcl))/liver_wt
      if (tot_scaled_hepcl>0 && tot_scaled_recomcl>0){
        age_wise_enzymecl_table <- scale_cellular_enzymatic(out_ages,tot_scaled_hepcl,
                                                              tot_scaled_recomcl,cypDb(),
                                                              recombList,
                                                              input$gender)
      }else{
        age_wise_enzymecl_table <- scale_enzymatic(out_ages,
                                                     tot_scaled_recomcl,cypDb(),
                                                     recombList,
                                                     input$gender)
      }


      hep_vals_2_return <- switch(hepcl_type,
                                  "hep_sc"= data.frame("name" =c("Microsomes","Cytosol"),val = unname(scaled_hepcl)/liver_wt),
                                  "hep_s9"= data.frame("name"="S9Fraction","val"=scaled_hepcl/liver_wt,stringsAsFactors = F),
                                  "hep_whole"= data.frame("name"="Whole Hepatocyte","val"=scaled_hepcl/liver_wt,stringsAsFactors = F),
                                  "hep_fnl"= data.frame("name"="Precomputed","val"=scaled_hepcl/liver_wt,stringsAsFactors = F),
                                  data.frame("name"="No Cellular Clearance","val"=0,stringsAsFactors = F)
      )
      recom_vals_2_return <-data.frame("name" = names(scaled_recomcl),"val"= unname(scaled_recomcl)/liver_wt,stringsAsFactors = F)

      return(list("cellular_tble"=hep_vals_2_return,
                  "enzymatic_tble"=recom_vals_2_return,
                  "all_data" = list("ages"= out_ages,
                                    "cellular_clearance"= scaled_hepcl/liver_wt,
                                    "enzymatic_clearance" = age_wise_enzymecl_table)
      ))

    })

    output$out_heptble <- DT::renderDT(DT::datatable(result_vals()$cellular_tble,
                                                     caption = "Cellular Clearance",
                                                     colnames = c("Type","Clearance(L/h/kg liver)"),
                                                     rownames = NULL,autoHideNavigation = T,
                                                     options = list(dom = "tp",pageLength = 5)),server = T)
    output$out_enztble <- DT::renderDT(DT::datatable(result_vals()$enzymatic_tble,
                                                     caption = "Recombinant Enzyme Clearance",
                                                     colnames = c("Type","Clearance(L/h/kg liver)"),
                                                     rownames = NULL,
                                                     options = list(dom = "tp",pageLength = 5)),server = T)




    observeEvent(input$done,{
      # handle data to be returned to the console
      if (input$ret_data == "cl_ref"){
        final_vals <- result_vals()$all_data
        age <- final_vals$ages[1]
        cellular_clearance <- sum(unname(final_vals$cellular_clearance))
        enzymatic_clearance <- final_vals$enzymatic_clearance["Total",as.character(age)]
        ret_data <- data.frame("Age"= age,"Cellular"= cellular_clearance,"Enzymatic"= enzymatic_clearance)


      }else if(input$ret_data == "cl_all"){

        final_vals <- result_vals()$all_data
        age <- final_vals$ages
        enzymatic_clearance <- final_vals$enzymatic_clearance["Total",]
        cellular_clearance <- sum(unname(final_vals$cellular_clearance))
        cellular_clearance <- c(cellular_clearance,rep(0,length(age)-1))
        ret_data <- data.frame("Age"= age,"Cellular"= cellular_clearance,"Enzymatic"= enzymatic_clearance)
      }else if(input$ret_data == "cl_detail"){

        final_vals <- result_vals()$all_data
        age <- final_vals$ages
        enzymatic_clearance <- final_vals$enzymatic_clearance["Total",]
        cellular_clearance <- sum(unname(final_vals$cellular_clearance))
        cellular_clearance <- c(cellular_clearance,rep(0,length(age)-1))
        cl_data <- data.frame("Age"= age,"Cellular"= cellular_clearance,"Enzymatic"= enzymatic_clearance)
        ret_data <- list()
        ret_data$tot_cl_data <- cl_data
        ret_data$ind_enzyme_cl <- final_vals$enzymatic_clearance
        ret_data$enzyme_data <- cypDb()
      }else if(input$ret_data == "none"){

        ret_data <- NULL
      }
      # handle file saving for vmax and vliv clearance if save flag is set to true
      if(save_flag){
        final_vals <- result_vals()$all_data
        if (all(final_vals$enzymatic_clearance["Total",]!=0)){
          vliv <- final_vals$enzymatic_clearance["Total",]
          age <- final_vals$ages
        }else{
          vliv <- sum(unname(final_vals$cellular_clearance))
          age <- final_vals$ages[1]
        }
        vmax <- vliv*input$km*input$liv_wt/(input$bw^0.75)
        if(input$fname_vliv != ""){
          write_data <- data.frame("Age"= age,"Clearance"=vliv,stringsAsFactors = F)
          write.csv(write_data,file.path(base_path,paste0(input$fname_vliv,".csv")),row.names = F)
        }
        if(input$fname_vmax != ""){
          write_data <- data.frame("Age"= age,"Clearance"=vmax,stringsAsFactors = F)
          write.csv(write_data,file.path(base_path,paste0(input$fname_vmax,".csv")),row.names = F)
        }

      }

      stopApp(returnValue = ret_data)
    })
  }
  runGadget(ui,server,viewer =dialogViewer("IVIVE",width = 1200,height = 1200)) #dialogViewer("IVIVE",width = 800) )
}


