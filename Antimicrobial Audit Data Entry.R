
##### Libraries #####
library(shiny)
library(googlesheets4)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(shinyjs)
library(bslib)
library(DBI)
library(RSQLite)
library(jsonlite)

tryCatch({
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "himanshucmc-9d06ab2bebfc.json")
  gs4_auth(path = "himanshucmc-9d06ab2bebfc.json",
           scopes = "https://www.googleapis.com/auth/spreadsheets",
           cache = TRUE,
           use_oob = FALSE)
}, error = function(e) {
  print(paste("Error: ", e$message))
})

aware <- read_sheet("https://docs.google.com/spreadsheets/d/14SaX51yiTbHLyU-2NKfwORIMI8aabf5RO7pQVzeGepo/edit?gid=2144428046#gid=2144428046",
                    sheet = "Sheet5")
antibiotics <- aware$Drug
Organ <- aware$Organisms
specialties <- aware$Specialty
wards <- aware$Area
residents <- unique(aware$Resident)

##### Antibiotics UI Panel #####
max_ama <- 5

# Generate UI for antimicrobial details dynamically
ama_ui <- lapply(1:max_ama, function(i) {
  conditionalPanel(
    condition = paste0("input.num_ama >= ", i),
    h4(tags$span("_____________________________________________________________________", 
                 style = "color: yellow;")),
    div(
      h4(tags$span(paste("Drug Details for Antibiotic, Antifungal, Antiviral", i), 
                   style = "color: blue; font-weight: bold; text-decoration: underline;"))
    ),
    div(
      style = "display: flex; flex-wrap: wrap; gap: 20px;",
      selectInput(paste0("drug", i), "Name of Drug", 
                  choices = antibiotics, 
                  selected = NULL,
                  multiple = FALSE),
      conditionalPanel( 
        condition <- paste0("input.drug", i, " == 'Others Antibiotic' || input.drug", i, " == 'Others Antifungal' || input.drug", i, " == 'Others Antiviral'"),
        textInput(paste0("other", i), "Any other name of drug mention here")),
      selectInput(paste0("category_drug", i), "Category of Drug",
                  choices = c("", "Antibiotic", "Antifungal", "Antiviral"),
                  selected = NULL),
      selectInput(paste0("drug_class", i), "Classification of Drug", choices = NULL),
      selectInput(paste0("WHO_category", i), "Category as per WHO", choices = NULL),
      selectInput(paste0("restricted", i), "Restricted", choices = NULL),
      textInput(paste0("route", i), "Route of Administration"),
      selectInput(paste0("indication_type", i), "Indication Type", 
                  choices = c("", "Empirical", "Evidence-based (Culture)", "Preemptive (Anti Fungal)", "Surgical prophylaxis")),
      numericInput(paste0("dose", i), "Dose (mg)", value = NULL, min = 0),
      numericInput(paste0("frequency", i), "Frequency (per day)", value = NULL, min = 0),
      dateInput(paste0("start_ama", i), "Start Date of AMA"),
      selectInput(paste0("antibiotic_review", i), "Antimircobial Reviewed?", choices = c("", "Yes", "No", "Not Documented")),
      selectInput(paste0("review_action", i), "Action Taken", choices = c("", "Escalated", "De-escalated", "Continued", "Stopped"))
    )
  )
})

# Wrap the dynamically generated UI inside a div
ama_ui_panel <- div(ama_ui)

##### UI started ##### 

# Define Custom Theme
my_theme <- bs_theme(
  version = 4,
  bootswatch = "journal",  # Available themes: cerulean, lux, sketchy, etc.
  base_font = font_google("Nunito"),
  heading_font = font_google("Lora")
)

gslink <- "https://docs.google.com/spreadsheets/d/14SaX51yiTbHLyU-2NKfwORIMI8aabf5RO7pQVzeGepo/edit?gid=350449966#gid=350449966"

# Define UI
ui <- fluidPage(
  theme = my_theme,
  useShinyjs(),
  
  # Add custom CSS to freeze tab names
  tags$head(
    tags$style(HTML("
    
      .nav-tabs {
        position: fixed; /* Fix the tabs at the top */
        top: 0; /* Position them at the top */
        width: 100%; /* Make them span the full width */
        z-index: 1000; /* Ensure they appear above other content */
        background-color: lightblue; /* Optional: Background color for tabs */
      }
      .tab-content {
        margin-top: 70px; /* Add space below the fixed tabs */
      }
    "))
  ),

  tabsetPanel(
    tabPanel(h4(tags$span("Data Entry Form",
                          style = "color: maroon; font-weight: bold; text-decoration: underline;")),
             h4("__________________________________________________________________________"),
             
             # Title with subtitle
             titlePanel(
               tags$div(
                 tags$span("Antimicrobial Audit Data Entry", 
                           style = "color: green; font-weight: bold; text-decoration: underline;"),
                 tags$br(),  # Line break between title and subtitle
                 tags$span("*  AMA stands for Antimicrobial Agent", 
                           style = "color: blue; font-size: 18px; font-style: italic;"),
                 tags$br(),  # Line break between title and subtitle
                 tags$span("*  Select 'No' & 'NA' if not applicable", 
                           style = "color: gray; font-size: 14px; font-style: italic;"),
               )
             ),
             
             mainPanel(
               
               ##### Basic and Starting Questions till Cultures #####
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 
                 textInput("entered_by", "Form filled by (employee name)"),
                 textInput("UHID", HTML("<b style='color: red;'>UHID*</b>")),
                 selectInput("ward", "Ward/Unit", choices = wards),
                 conditionalPanel(
                   condition = "input.ward == 'Others'",
                   textInput("ow", "Other ward")),
                 selectInput("specialty", "Specialty", choices = specialties),
                 conditionalPanel(
                   condition = "input.specialty == 'Others'",
                   textInput("os", "Other specialty")),
                 dateInput("dob", "Date of Birth"),
                 htmlOutput("age"),
                 selectInput("sex", "Sex", choices = c("","Male", "Female", "Other")),
                 numericInput("weight", "Weight (kg)", value = NULL, min = 0),
                 textInput("diagnosis", "Diagnosis"),
                 selectInput("comorbidities", "Comorbidities", 
                             choices = c("","DM", "HTN",
                                         "Respiratory", "Cardiac", "Renal", "Others")),
                 conditionalPanel(
                   condition = "input.comorbidities == 'Others'",
                   textInput("oc", "Other Comorbidities"))
               ),  # Display the computed LOS
               
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 
                 dateInput("doa", "Date of Admission"),
                 dateInput("audit_date", "Date of Audit"),
                 htmlOutput("los_output")
               ),
               
               # Surgical procedure
               div(
                 h4(tags$span("Surgical Prophylaxis Details", 
                              style = "color: blue; font-weight: bold; text-decoration: underline;"))
               ),
               
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 selectInput("surgical_proc", "Is any Surgical procedure done?",
                             choices = c("","No", "Yes")),
                 
                 conditionalPanel(
                   condition = "input.surgical_proc == 'Yes'",
                   textInput("name_surgery", "Name of the surgery"),
                   radioButtons("planned", "Planned/emergency", 
                                choices = list("Planned",
                                               "Emergency"),
                                selected = character(0)),
                   radioButtons("cleaned", "clean/clean contaminated", 
                                choices = list("clean",
                                               "clean contaminated"),
                                selected = character(0))
                 )
               ),
               
               # Cultures data
               div(
                 h4(tags$span("Culture Details", 
                              style = "color: blue; font-weight: bold; text-decoration: underline;"))
               ),
               
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 selectInput("cultures_sent", "Representative Cultures Sent Before Emperical AMA?", 
                             choices = c("","NA (only for surgical Prophylaxis)", "No", "Yes")),
                 
                 conditionalPanel(
                   condition = "input.cultures_sent == 'Yes'",
                   dateInput("docs", "Date of cultures sent"),
                   dateInput("docr", "Date of cultures report"),
                   selectInput("culture_site", "Culture Site", 
                               choices = c("","Blood", "Urine", "Respiratory", "Other"),
                               multiple = TRUE),
                   textInput("other_cultures","Mention Other Culture sites"),
                   
                   selectInput("positive_culture", "Positive Culture Organism Name",
                               choices = Organ,
                               multiple = TRUE)
                 )
               ),
               
               h4("Biomarkers (Most Relevant with 24 hours of starting AMA) (TLC/N, CRP, PCT, Fever)",
                  style = "color: blue; font-weight: bold; text-decoration: underline;"),
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 textInput("tlcn1", "TLC/N"),
                 textInput("crp1", "CRP"),
                 textInput("pct1", "PCT"),
                 textInput("fever_sep1", "Fever")
               ),
               
               ##### AMA Details UI created using lapply function #####
               div(
                 h4(tags$span("AMA Details", style = "color: blue; font-weight: bold; text-decoration: underline;"))),
               selectInput("on_ama", "Patient on Antimicrobials?", choices = c("", "Yes", "No")),
               
               conditionalPanel(
                 condition = "input.on_ama == 'Yes'",
                 div(
                   style = "display: flex; flex-wrap: wrap; gap: 20px;",
                   selectInput("ama_day_written", "Was the antibiotic day written or not in the chart?",
                               choices = c("", "Yes", "No")),
                   selectInput("category", "Risk Category of Patient", choices = c("", "Not categorized", "1", "2", "3", "4")),
                   selectInput("indication", "Possible site of infection", 
                               choices = c("", "CNS", "RTI", "VAP", "SSI", "GI/IA", "UTI", "CAUTI", 
                                           "Skin & Soft Tissue", "BSI", "CLABSI", "Other"), multiple = TRUE)
                 ),
                 numericInput("num_ama", "Number of AMAs on Audit Day", value = 0, min = 0),
                 
                 # Inject the dynamically created conditional AMA UI
                 ama_ui_panel
               ),
               
               h4(tags$span("_____________________________________________________________________",
                            style = "color: yellow;")),
               
               ##### Discharge Information #####
               
               div(
                 h4(tags$span("Discharge Information",
                              style = "color: blue; font-weight: bold; text-decoration: underline;"))
               ),
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 selectInput("discharge_ama", "Discharge Patient on AMA?", 
                             choices = c("","Yes", "No")),
                 selectInput("appropriate_dose", "Prescribed Dose Appropriate?",
                             choices = c("","Yes", "No")),
                 selectInput("discharge_instructions", 
                             "Duration/Instruction of AMA Mentioned on Discharge?", 
                             choices = c("", "Yes", "No")),
                 textInput("nod", "Name of the drug at time of discharge"),
                 textInput("remarks", "Remarks/Conclusion")
               ),
               
               ##### Buttons #####
               h4(tags$span("_____________________________________________________________________", 
                            style = "color: red;")),
               
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 actionButton("save_draft", "Save Draft", class = "btn-warning"),
                 actionButton("load_draft", "Load Draft", class = "btn-info"),
                 actionButton("save_data", h5("Submit Form"), class = "btn-success btn-lg"),
                 downloadButton("download", "Download Data", class = "btn-primary btn-sm")
               ),
               h4("_____________________________________________________________________"),
               
               DTOutput("table")
             )
      
    ),
    
    #####    Dashboard #####
    tabPanel(h4(tags$span("Analysis",
                          style = "color: maroon; font-weight: bold; text-decoration: underline;")),
             mainPanel(
               h4("__________________________________________________________________________"),
               
               div(
                 style = "display: flex; flex-wrap: wrap; gap: 20px;",
                 plotOutput("bar_chart"),
                 h4("__________________________________________________________________________"),
                 plotOutput("bar_chart_culture", width = "1200px", height = "600px"),
                 h4("__________________________________________________________________________"),
                 plotOutput("bar_chart_restrict"),
                 h4("__________________________________________________________________________"),
                 selectInput("res_in", "Stratify for Drugs", choices = c("Indication", "Drugs", "Cultures sent", "Department")),
                 plotOutput("bar_chart_restrict_indication", width = "2400px", height = "600px"),
                 DTOutput("indication_table"),
                 h4("__________________________________________________________________________"),
                 plotOutput("three_AMA_3days")
               ),
             )
             ),
    ##### Google sheets link #####
    tabPanel(h4(tags$span("Google Sheet",
                          style = "color: maroon; font-weight: bold; text-decoration: underline;")),
             mainPanel(
               tags$head(
                 tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
               ),
               tags$div(
                 style = "display: flex; align-items: center; justify-content: flex-start;", # Align items in a row and start from the left
                 tags$span(
                   class = "material-icons",
                   style = "font-size: 24px; margin-right: 12px;", # Add margin to the right of the icon
                   "table_chart"  # Google Sheets icon
                 ),
                 tags$a(
                   href = gslink,
                   "Open Google Sheets",
                   target = "_blank" # Opens in a new tab
                 )
               ),
               
               tags$iframe(src = gslink,  # Replace with your actual embed URL
                           width = "150%", 
                           height = "700px", 
                           frameborder = "5")
             )
    ),
    #####  Looker studio #####
    tabPanel(h4(tags$span("Looker",
                          style = "color: maroon; font-weight: bold; text-decoration: underline;")),
             mainPanel(
               
               tags$head(
                 tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
               ),
               tags$div(
                 style = "display: flex; align-items: center; justify-content: flex-start;",  # Align items in a row
                 tags$span(
                   class = "material-icons",
                   style = "font-size: 24px; margin-right: 12px;",
                   "analytics"  # Looker Studio icon
                 ),
                 tags$a(
                   href = "https://lookerstudio.google.com/embed/reporting/233f9ba6-aee3-4f0f-80d3-f72f688a7d7b/page/egJ4E",
                   "Open Looker Studio",
                   target = "_blank"  # Opens in a new tab
                 )
               ),
               
               tags$iframe(src = "https://lookerstudio.google.com/embed/reporting/233f9ba6-aee3-4f0f-80d3-f72f688a7d7b/page/egJ4E",  # Replace with your actual embed URL
                           width = "100%", 
                           height = "1500px", 
                           frameborder = "5")
             )
    ),
    )
  )

##### SERVER CODE STARTS #####
server <- function(input, output, session) {

  ##### SQL Draft #####
  # Connect to SQLite database
  db <- dbConnect(RSQLite::SQLite(), "drafts.db")
  
  # Create table with UNIQUE constraint on uhid
  dbExecute(db, "
  CREATE TABLE IF NOT EXISTS drafts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    uhid TEXT UNIQUE,  -- Explicit UNIQUE constraint
    data TEXT
  )
")
  
  # Save Draft (corrected query)
  observeEvent(input$save_draft, {
    dbExecute(db, "DELETE FROM drafts WHERE uhid = ?", params = list(input$UHID))
    draft_data <- reactiveValuesToList(input)
    draft_json <- jsonlite::toJSON(draft_data, auto_unbox = TRUE)
    
    # Use INSERT OR REPLACE to handle conflicts
    dbExecute(db, "
    INSERT OR REPLACE INTO drafts (uhid, data)
    VALUES (?, ?)
  ", params = list(input$UHID, draft_json))
    
    showNotification("Draft saved successfully!", type = "message")
  })
  
  # Load Draft (unchanged)
  observeEvent(input$load_draft, {
    draft <- dbGetQuery(db, "SELECT data FROM drafts WHERE uhid = ?", params = list(input$UHID))
    
    if (nrow(draft) > 0) {
      draft_data <- jsonlite::fromJSON(draft$data)
      for (name in names(draft_data)) {
        updateTextInput(session, name, value = draft_data[[name]])
      }
      showNotification("Draft loaded successfully!", type = "message")
    } else {
      showNotification("No draft found for this UHID.", type = "warning")
    }
    dbExecute(db, "DELETE FROM drafts WHERE uhid = ?", params = list(input$UHID))
  })
  
  # Disconnect from the database when the session ends
  onStop(function() {
    dbDisconnect(db)
  })
  
#####  Automation of classification and WHO category ######
  
  # Function to update drug class and WHO category dynamically
  updateDrugDetails <- function(drug_input_id, category_input_id, class_input_id, who_cat_input_id, restricted_id) {
    observeEvent(input[[drug_input_id]], {
      req(input[[drug_input_id]])  # Ensure the input is not NULL
      
      selected_drug <- input[[drug_input_id]]  # Get selected drug
      
      # Fetch drug classification and WHO category
      class <- aware$Class[aware$Drug == selected_drug]
      who_cat <- aware$WHO_AWaRe_Category[aware$Drug == selected_drug]
      category_drg <- aware$Category[aware$Drug == selected_drug]
      restricted_drg <- aware$Restricted[aware$Drug == selected_drug]
      
      # Ensure we handle cases where no classification or category is found
      if (length(category_drg) == 0) class <- "Unknown"
      if (length(class) == 0) class <- "Unknown"
      if (length(who_cat) == 0) who_cat <- "Unknown"
      if (length(restricted_drg) == 0) restricted_drg <- "Unknown"
      
      # Update classification and WHO category dropdowns
      updateSelectInput(session, category_input_id, choices = c(category_drg), selected = category_drg)
      updateSelectInput(session, class_input_id, choices = c(class), selected = class)
      updateSelectInput(session, who_cat_input_id, choices = c(who_cat), selected = who_cat)
      updateSelectInput(session, restricted_id, choices = c(restricted_drg), selected = restricted_drg)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }
  
  # List of input IDs for drugs, classification, and WHO category
  category_ids <- c("category_drug1", "category_drug2", "category_drug3", "category_drug4", "category_drug5")
  drug_ids <- c("drug1", "drug2", "drug3", "drug4", "drug5")
  class_ids <- c("drug_class1", "drug_class2", "drug_class3", "drug_class4", "drug_class5")
  who_cat_ids <- c("WHO_category1", "WHO_category2", "WHO_category3", "WHO_category4", "WHO_category5")
  restricted_ids <- c("restricted1", "restricted2", "restricted3", "restricted4", "restricted5")
  
  # Loop to apply the function for each drug dynamically
  lapply(seq_along(drug_ids), function(i) {
    updateDrugDetails(drug_ids[i], category_ids[i], class_ids[i], who_cat_ids[i], restricted_ids[i])
  })
  
##### Reactive expression to compute LOS #####
  los <- reactive({
    req(input$doa, input$audit_date)
    as.numeric(difftime(input$audit_date, input$doa, units = "days"))
  })
  
  # Render the computed LOS
  output$los_output <- renderText({
    paste("<b style='color: blue; font-size: 16px;'>Length of Stay (LOS): </b><br><b style='color: green; font-size: 16px;'>", 
          los(), " days</b>")
  })
  
##### Reactive expression to compute Age #####
  ageyears <- reactive({
    total_days <- as.numeric(difftime(input$audit_date, input$dob, units = "days"))
    # Approximate months in a year
    months_per_year <- 12
    
    # Approximate days in a month
    days_per_month <- 30.44
    
    # Calculate years
    years <- floor(total_days / (days_per_month * months_per_year))
    
    # Calculate remaining days after subtracting years
    remaining_days <- total_days %% (days_per_month * months_per_year)
    
    # Calculate months
    months <- floor(remaining_days / days_per_month)
    
    # Calculate remaining days after subtracting months
    days <- round(remaining_days %% days_per_month)
    return(list(years = years, months = months, days = days))
  })
  
  # Render the computed LOS
  output$age <- renderText({
    breakdown <- ageyears()
    paste("<b style='color: blue; font-size: 16px;'>Age on date of audit: </b><br>",
          paste0("<b style='color: green; font-size: 16px;'>", breakdown$years, " years</b>, "),
          paste0("<b style='color: green; font-size: 16px;'>", breakdown$months, " months</b>, "),
          paste0("<b style='color: green; font-size: 16px;'>", breakdown$days, " days</b>"))
  })

##### Google Sheets Authentication  #####
  # Define Google Sheet Path
  sheet_path <- "https://docs.google.com/spreadsheets/d/14SaX51yiTbHLyU-2NKfwORIMI8aabf5RO7pQVzeGepo/edit?gid=350449966#gid=350449966"
  
  existing_data <- tryCatch({
    read_sheet(sheet_path, sheet = "AD Updated")
  }, error = function(e) {
    print("Error reading Google Sheet, returning empty data frame.")
    data.frame()
  })
  
  data <- reactiveVal(existing_data) 
##### Add new record #####
  
  # Function to handle missing conditional inputs
  safe_value <- function(input_value, condition, na_value = NA) {
    if (is.null(input_value) || condition == "No") return(na_value)
    return(input_value)
  }
  
  observeEvent(input$save_data, {
      if (is.null(input$UHID) || input$UHID == "") {
        showModal(modalDialog(
          title = "UHID Required",
          "Please enter a new UHID before proceeding. Previous form Submitted successfully. Thank you.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        Sys.setenv(TZ = "Asia/Calcutta")
    new_record <- data.frame(
  # Basic Details till On_AMA 
      Timestamp = format(Sys.time(), tz = "Asia/Kolkata", usetz = TRUE),
      Audit_Date = as.character(input$audit_date),
      Filled_by = as.character(input$entered_by),
      UHID = as.character(input$UHID),
      Ward = input$ward,
      Other_ward = input$ow,
      Specialty = input$specialty,
      Other_specialty = input$os,
      Date_of_birth = as.character(input$dob),
      Age = ageyears()$years,
      Sex = input$sex,
      Weight = input$weight,
      Date_of_Admission = as.character(input$doa),
      Length_of_Stay = los(),
      Comorbidities = input$comorbidities,
      Other_comorbitidites = input$oc,
      Diagnosis = input$diagnosis,
      
  # Surgical Procedure
      Surgical_Procedure = input$surgical_proc,
      Name_of_Surgery = safe_value(input$name_surgery, input$surgical_proc),
      Planned_or_Emergency = safe_value(input$planned, input$surgical_proc),
      Clean_or_CleanContaminated = safe_value(input$cleaned, input$surgical_proc),
      
  # Cultures Sent Before AMA
      Cultures_Sent_Before_AMA = input$cultures_sent,
      Date_culture_sent = safe_value(as.character(input$docs), input$cultures_sent),
      Date_culture_report = safe_value(as.character(input$docr), input$cultures_sent),
      Culture_Site = safe_value(paste(input$culture_site, collapse = "//"), input$cultures_sent),
      Other_cultures_sites = safe_value(input$other_cultures, input$cultures_sent),
      Positive_Culture = safe_value(paste(input$positive_culture, collapse = "//"), input$cultures_sent),
      
      TLCN_1 = safe_value(input$tlcn1, input$on_ama),
      CRP_1 = safe_value(input$crp1, input$on_ama),
      PCT_1 = safe_value(input$pct1, input$on_ama),
      Fever_Sepsis_1 = safe_value(input$fever_sep1, input$on_ama),
      
 # AMA Details
      On_AMA = input$on_ama,
      AMA_day_written_on_chart = safe_value(input$ama_day_written, input$on_ama),
      Risk_Category = safe_value(input$category, input$on_ama),
      Possible_Site_of_Infection = safe_value(paste(input$indication, collapse = "//"), input$on_ama),
      Number_of_AMAs = safe_value(input$num_ama, input$on_ama, 0),
      stringsAsFactors = FALSE
    )
    
    # Create drug details dynamically
    for (i in 1:max_ama) {
      new_record[[paste0("Category_Drug", i)]] <- safe_value(input[[paste0("category_drug", i)]], input$on_ama)
      new_record[[paste0("Drug_", i)]] <- safe_value(paste(input[[paste0("drug", i)]], collapse = ", "), input$on_ama)
      new_record[[paste0("Other_Drug_", i)]] <- safe_value(input[[paste0("other", i)]], input$on_ama)
      new_record[[paste0("Class_", i)]] <- safe_value(input[[paste0("drug_class", i)]], input$on_ama)
      new_record[[paste0("WHO_category_", i)]] <- safe_value(input[[paste0("WHO_category", i)]], input$on_ama)
      new_record[[paste0("Restricted_", i)]] <- safe_value(input[[paste0("restricted", i)]], input$on_ama)
      new_record[[paste0("Route_", i)]] <- safe_value(input[[paste0("route", i)]], input$on_ama)
      new_record[[paste0("Indication_Type_", i)]] <- safe_value(input[[paste0("indication_type", i)]], input$on_ama)
      new_record[[paste0("Dose_", i)]] <- safe_value(input[[paste0("dose", i)]], input$on_ama)
      new_record[[paste0("Frequency_", i)]] <- safe_value(input[[paste0("frequency", i)]], input$on_ama)
      new_record[[paste0("Days_on_AMA_", i)]] <- safe_value(as.numeric(difftime(input$audit_date, input[[paste0("start_ama", i)]], units = "days")), input$on_ama)
      new_record[[paste0("Start_AMA_", i)]] <- safe_value(as.character(input[[paste0("start_ama", i)]]), input$on_ama)
      new_record[[paste0("Antibiotic_Review_", i)]] <- safe_value(input[[paste0("antibiotic_review", i)]], input$on_ama)
      new_record[[paste0("Review_Action_", i)]] <- safe_value(input[[paste0("review_action", i)]], input$on_ama)
    }
    
    # Discharge Information
    new_record$Appropriate_Dose <- input$appropriate_dose
    new_record$Discharge_AMA <- input$discharge_ama
    new_record$Discharge_Instructions <- input$discharge_instructions
    new_record$Name_of_Drug_at_Discharge <- input$nod
    new_record$Remarks <- input$remarks
    
# Appending the row
    # Fix Column Name Mismatch
    colnames(new_record) <- colnames(existing_data)
    
    # nsure New Data Matches Existing Columns
    new_record <- new_record[, colnames(existing_data), drop = FALSE]
    
    updated_data <- rbind(data(), new_record)
    data(updated_data)
    showNotification("Submitted Successfully.", type="message")
    showModal(modalDialog(
      title = "Submitted Successfully",
      "Please enter a new UHID before proceeding.",
      easyClose = TRUE,
      footer = NULL
    ))
    
    tryCatch({
      sheet_append(sheet_path, new_record, sheet = "AD Updated")
    }, error = function(e) {
      print(paste("Error writing to Google Sheet:", e$message))
    })
    
    dbExecute(db, "DELETE FROM drafts WHERE uhid = ?", params = list(input$UHID))
    
    #####   Update session #####
    # List of static input IDs
    input_ids <- c(
      "entered_by", "UHID", "ward", "specialty", "dob", "age", "sex", "weight",
      "diagnosis", "comorbidities", "oc", "doa", "audit_date", "surgical_proc",
      "name_surgery", "planned", "cleaned", "cultures_sent", "docs", "docr",
      "culture_site", "other_cultures", "positive_culture", "tlcn1", "crp1",
      "pct1", "fever_sep1", "on_ama", "ama_day_written", "category",
      "indication", "num_ama", "discharge_ama", "appropriate_dose",
      "discharge_instructions", "nod", "remarks"
    )
    
    # Reset all standard text inputs in one loop
    lapply(input_ids, function(id) {
      updateTextInput(session, id, value = "")
    })
    
    # Reset dynamically generated AMA UI
    for (i in 1:max_ama) {
      dynamic_ids <- c(
        paste0("drug", i), paste0("other", i), paste0("category_drug", i),
        paste0("drug_class", i), paste0("WHO_category", i), paste0("restricted", i),
        paste0("route", i), paste0("indication_type", i), paste0("dose", i),
        paste0("frequency", i), paste0("start_ama", i), paste0("days_ama", i),
        paste0("antibiotic_review", i), paste0("review_action", i)
      )
      
      # Reset dynamic inputs in a loop
      lapply(dynamic_ids, function(id) {
        updateTextInput(session, id, value = "")
      })
    }
    #####
    } #else ends here
  })
  
  # Full data table #####
  output$table <- renderDT({
    
    extra_col <- add_col(data())
    
    sheet_write(extra_col, 
                ss = "https://docs.google.com/spreadsheets/d/14SaX51yiTbHLyU-2NKfwORIMI8aabf5RO7pQVzeGepo/edit?gid=1561198537#gid=1561198537", 
                sheet = "Full data")
    
    datatable(extra_col,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black; font-size:200%;', 
                                                'Complete data'))
    
  })
  
  # Data table for Restricted antibiotics
  output$indication_table <- renderDT({
    
    res_AMA_details <- add_col(data()) %>%
      filter(add_col(data())$UHID %in% unique(res_indi()$UHID)) %>%
      select(-Remarks)
    
    datatable(res_AMA_details,
              options = list(pageLength = 5),
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black; font-size:200%;', 
                                                'Patients on restricted antimicrobials'))
  })
  
  # THEME #####
  theme <- theme(
    plot.title = element_text(size = 24, face = "bold"),   
    axis.title.x = element_text(size = 20),                
    axis.title.y = element_text(size = 20),               
    axis.text.x = element_text(size = 16),            
    axis.text.y = element_text(size = 16)) 
  
  ##### Charts #####
  output$bar_chart <- renderPlot({
    chart_data <- data() %>%
      filter(!is.na(Number_of_AMAs)) %>%
      group_by(Number_of_AMAs) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(
        Percentage = round(Count / sum(Count) * 100, 1)
      )
    
    ggplot(chart_data, aes(x = as.factor(Number_of_AMAs), 
                           y = Count,
                           fill = Number_of_AMAs))+
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(Count, "/", sum(Count), "(",Percentage, "%)")), 
                vjust = -0.5, size = 6) +
      labs(title = "Number(%) of patients on antimicrobials", 
           x = "Number_of_AMAs", y = "Count") +
      theme
  })
  output$bar_chart_culture <- renderPlot({
    chart_data <- data() %>%
      filter(!is.na(Number_of_AMAs) & Number_of_AMAs != 0) %>%
      group_by(Cultures_Sent_Before_AMA) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(
        Percentage = round(Count / sum(Count) * 100, 1)
      )
    
    ggplot(chart_data, aes(x = factor(Cultures_Sent_Before_AMA, 
                                      levels = c("Yes", "No"), 
                                      labels = c("Yes", "No")), 
                           y = Count,
                           fill = Cultures_Sent_Before_AMA))+
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      labs(title = "Number(%) when appropriate cultures was sent", 
           x = "Cultures_Sent_Before_AMA", y = "Count") +
      scale_fill_manual(values = c("Yes" = "#F8766D", "No" = "#619CFF"))+
      theme
  })
  output$bar_chart_restrict <- renderPlot({
    
    extra_col <- add_col(data())
    
    chart_data <- extra_col %>%
      filter(!is.na(Number_of_AMAs) & Number_of_AMAs != 0) %>%
      group_by(Restricted_Drug) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(
        Percentage = round(Count / sum(Count) * 100, 1)
      )
    
    ggplot(chart_data, aes(x = factor(Restricted_Drug, 
                                      levels = c("Yes", "No"), 
                                      labels = c("Yes", "No")), 
                           y = Count,
                           fill = Restricted_Drug))+
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      labs(title = "Number(%) of patients on restricted antimicrobials", 
           x = "Restricted_Drug", y = "Count") +
      scale_fill_manual(values = c("Yes" = "#F8766D", "No" = "#619CFF"))+
      theme
  })
  output$bar_chart_restrict_indication <- renderPlot({
    
    if (input$res_in == "Indication"){
      chart_data <- res_indi() %>%
        group_by(Indication) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(
          Percentage = round(Count / sum(Count) * 100, 1)
        )
      
      ggplot(chart_data, aes(x = Count, 
                             y = as.factor(Indication),
                             fill = Indication))+
        expand_limits(x = max(chart_data$Count) * 1.2) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                  position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
        labs(title = "Number(%) of patients on restricted antimicrobials stratified by Indication type", 
             x = "Count", y = "Type of Indication") +
        theme
      
    } 
    else if (input$res_in == "Drugs"){
      chart_data <- res_indi() %>%
        group_by(Indication, Drug_Name) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(
          Percentage = round(Count / sum(Count) * 100, 1)
        )
      
      ggplot(chart_data, aes(x = Count, 
                             y = as.factor(Drug_Name),
                             fill = Indication))+
        expand_limits(x = max(chart_data$Count) * 1.2) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                  position = position_dodge(width = 0.9), vjust = -0.3, size = 6) +
        labs(title = "Number(%) of restricted antimicrobials stratified by Indication & Drug", 
             x = "Count", y = "Antimicrobial") +
        theme
    } 
    else if(input$res_in == "Cultures sent"){
      chart_data <- res_indi() %>%
        group_by(Indication, Cultures_Sent_Before_AMA) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(
          Percentage = round(Count / sum(Count) * 100, 1)
        )
      
      ggplot(chart_data, aes(x = Count, 
                             y = as.factor(Cultures_Sent_Before_AMA),
                             fill = Indication))+
        expand_limits(x = max(chart_data$Count) * 1.2) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                  position = position_dodge(width = 0.9), vjust = -0.3, size = 6) +
        labs(title = "Number(%) of patients on restricted antimicrobials stratified by Indication & Cultures", 
             x = "Count", y = "Cultures Sent Before AMA") +
        theme
    } 
    else if(input$res_in == "Department"){
      chart_data <- res_indi() %>%
        group_by(Indication, Specialty) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(
          Percentage = round(Count / sum(Count) * 100, 1)
        )
      
      ggplot(chart_data, aes(x = Count, 
                             y = as.factor(Specialty),
                             fill = Indication))+
        expand_limits(x = max(chart_data$Count) * 1.2) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                  position = position_dodge(width = 0.9), vjust = -0.3, size = 6) +
        labs(title = "Number(%) of patients on restricted antimicrobials stratified by Indication & Cultures", 
             x = "Count", y = "Department") +
        theme
    }
  })
  output$three_AMA_3days <- renderPlot({
    
    extra_col <- add_col(data())
    
    chart_data <- extra_col %>%
      filter(!is.na(Number_of_AMAs) & Number_of_AMAs != 0) %>%
      group_by(Three_AMA_3days) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(
        Percentage = round(Count / sum(Count) * 100, 1)
      )
    
    ggplot(chart_data, aes(x = factor(Three_AMA_3days, 
                                      levels = c("Yes", "No"), 
                                      labels = c("Yes", "No")), 
                           y = Count,
                           fill = Three_AMA_3days))+
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(Count, "/", sum(Count), "(", Percentage, "%)")),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      labs(title = "Number(%) of patients on more than 3 antimicrobials for more than 3 days", 
           x = "3 Antimicrobials for >= 3 days", y = "Count") +
      scale_fill_manual(values = c("Yes" = "#F8766D", "No" = "#619CFF"))+
      theme
  })
  
  ##### Columns for analysis Purpose #####
  add_col <- function(df) {
    
    extra_col <- df %>%
      mutate(
        Antibiotic = ifelse(
          rowSums(.[, paste0("Category_Drug", 1:5)] == "Antibiotic", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Antifungal = ifelse(
          rowSums(.[, paste0("Category_Drug", 1:5)] == "Antifungal", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Antiviral = ifelse(
          rowSums(.[, paste0("Category_Drug", 1:5)] == "Antiviral", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Restricted_Drug = ifelse(
          rowSums(.[, paste0("Restricted_", 1:5)] == "Yes", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Emperical = ifelse(
          apply(.[, paste0("Indication_Type_", 1:5)], 
                1, function(x) any(x == "Empirical", na.rm = TRUE)), 
          "Yes", "No"
        ),
        Evidence = ifelse(
          rowSums(.[, paste0("Indication_Type_", 1:5)] == "Evidence-based (Culture)", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Sp = ifelse(
          rowSums(.[, paste0("Indication_Type_", 1:5)] == "Surgical prophylaxis", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        Preemptive = ifelse(
          rowSums(.[, paste0("Indication_Type_", 1:5)] == "Preemptive (Anti Fungal)", na.rm = TRUE) > 0, 
          "Yes", "No"
        ),
        LOS_10days = ifelse(Length_of_Stay >= 10, "Yes", "No"),
        AMA_10days = ifelse(
          apply(.[, paste0("Days_on_AMA_", 1:5)], 
                1, function(x) any(x >= 10, na.rm = TRUE)), 
          "Yes", "No"
        ),
        Three_AMA_3days = ifelse(
          apply(.[, paste0("Days_on_AMA_", 1:5)], 
                1, function(x) any(x >= 3, na.rm = TRUE)) & Number_of_AMAs >= 3, 
          "Yes", "No"
        ),
        Who_category = case_when(
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x == "Access", na.rm = TRUE)) ~ "Access", 
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x == "Watch", na.rm = TRUE)) ~ "Watch",
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x == "Reserve", na.rm = TRUE)) ~ "Reserve",
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x %in% c("Access", "Watch"), na.rm = TRUE)) ~ "Access/Watch",
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x %in% c("Access", "Reserve"), na.rm = TRUE)) ~ "Access/Reserve",
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x %in% c("Watch", "Reserve"), na.rm = TRUE)) ~ "Watch/Reserve",
          apply(.[, paste0("WHO_category_", 1:5)], 
                1, function(x) any(x %in% c("Access", "Watch", "Reserve"), 
                                   na.rm = TRUE)) ~ "Access/Watch/Reserve",
          TRUE ~ ""
        )
      ) %>%
      mutate(indication_type = ifelse(Emperical == "Yes" & Evidence != "Yes", "Emperical",
                                      ifelse(Evidence == "Yes" & Emperical != "Yes", "Evidence", 
                                             ifelse(Emperical == "Yes" & Evidence == "Yes",
                                                    "Both", "")
                                      )
      )
      )
    return(extra_col)
  }
  
  res_indi <- function(){
    indi_data <- add_col(data()) %>%
      filter(!is.na(Number_of_AMAs) & Number_of_AMAs != 0 & Restricted_Drug == "Yes")
    
    drug_long <- indi_data %>%
      select(UHID, Cultures_Sent_Before_AMA, Specialty, Drug_1, Drug_2, Drug_3, Drug_4, Drug_5) %>%
      pivot_longer(
        cols = starts_with("Drug_"),                  # Pivot all Drug columns
        names_to = "Drug_Type",
        values_to = "Drug_Name"
      ) %>%
      ungroup() %>%
      mutate(S_No = row_number())
    
    category_long <- indi_data %>%
      select(UHID, Category_Drug1, Category_Drug2, Category_Drug3, Category_Drug4, Category_Drug5) %>%
      pivot_longer(
        cols = starts_with("Category_Drug"),                  # Pivot all Drug columns
        names_to = "Category",
        values_to = "Drug_Category"
      ) %>%
      ungroup() %>%
      mutate(S_No = row_number())
    
    aware_long <- indi_data %>% 
      select(UHID, WHO_category_1, WHO_category_2, 
             WHO_category_3, WHO_category_4, WHO_category_5) %>%
      pivot_longer(
        cols = starts_with("WHO_category_"), 
        names_to = "WHO_Category_Type",
        values_to = "WHO_Category"
      ) %>%
      ungroup() %>%
      mutate(S_No = row_number())
    
    indi_long <- indi_data %>% 
      select(UHID, Indication_Type_1, Indication_Type_2, 
             Indication_Type_3, Indication_Type_4, Indication_Type_5) %>%
      pivot_longer(
        cols = starts_with("Indication_Type_"), 
        names_to = "Indication_Type",
        values_to = "Indication"
      ) %>%
      ungroup() %>%
      mutate(S_No = row_number())
    
    long_data <-  drug_long %>%
      left_join(category_long, by = c("UHID", "S_No")) %>%
      left_join(aware_long, by = c("UHID", "S_No")) %>%
      left_join(indi_long, by = c("UHID", "S_No")) %>%
      filter(!is.na(Drug_Name) & Drug_Name != "NA") %>%
      filter(Drug_Name %in% aware$Restricted_Drugs) %>%
      select(UHID, Cultures_Sent_Before_AMA, Specialty, Drug_Category, Drug_Name, WHO_Category, Indication)
    
    return(long_data)
  }
  
##### Download handler #####
  output$download <- downloadHandler(
    filename = function() { "Antimicrobial_data.csv" },
    content = function(file) {
      extra_col <- add_col(data())
      write.csv(extra_col, file)
    }
  )
  
  
  } # server Code ends

##### Run the app #####
shinyApp(ui = ui, server = server)

