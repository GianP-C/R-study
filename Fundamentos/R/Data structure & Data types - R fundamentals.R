# Title: R Fundamentals and Healthcare Data Analysis
# Author: GIANCARLO PRIOLI CARDOZO
# Date: March 2026
# Description: Exercises covering vectorization, data frames, 
# clinical trial analysis, and string manipulation using stringr.


# --- EXERCISE 1: Vector Basics & Filtering ---

# 1. Creating the vector
weekly_sales <- c(120, 150, 90, 200, 250, 180, 110)
mean(weekly_sales) 

# 2. Filtering high sales (above 150) using logical indexing
high_sales <- weekly_sales[weekly_sales > 150]
high_sales 

# 3. Updating the sixth element using position index
weekly_sales[6] <- 130
weekly_sales[6]

# 4. Displaying the updated vector
print(weekly_sales)


# --- EXERCISE 2: Data Frames & Vectorization ---

# 1. Object Types & Creation
sales_reps <- c("Ana", "Bruno", "Caio", "Duda")
sales_values <- c(1200, 850, 2100, 1500)

sales_df <- data.frame(
  reps = sales_reps, 
  sales = sales_values
)

# 2. Vectorization (10% Bonus Calculation)
# Applied to the entire vector without loops
sales_df$total_with_bonus <- sales_df$sales * 1.10

# 3. Indexing & Filtering rows where original sales > 1300
high_sales_df <- sales_df[sales_df$sales > 1300, ]
high_sales_df

# 4. Data Inspection
str(sales_df)       
summary(sales_df)   
print(sales_df)


# --- EXERCISE 3: Patient Triage Analysis ---

# 1. Data Creation 
patient_names <- c("John", "Mary", "Peter", "Anne", "Lewis")
age <- c(25, 68, 45, 12, 75)
systolic <- c(120, 150, 130, 115, 160)
diastolic <- c(80, 95, 85, 75, 100)

triage_df <- data.frame(patient_names, age, systolic, diastolic)

# 2. Vectorization: Calculating Mean Arterial Pressure (MAP)
triage_df$map <- (triage_df$systolic + 2 * triage_df$diastolic) / 3

# 3. Indexing & Filtering: High-Risk Patients (Age >= 65 OR MAP > 115)
priority_list <- triage_df[triage_df$age >= 65 | triage_df$map > 115, ]

# 4. Data Inspection
str(triage_df)
summary(triage_df)
print(priority_list)


# --- EXERCISE 4: Clinical Trial Analysis ---

# 1. Dataset Construction
clinical_trial <- data.frame(
  patient = c("P01", "P02", "P03", "P04", "P05", "P06"),
  group = c("Treatment", "Control", "Treatment", "Control", "Treatment", "Control"),
  initial_chol = c(240, 210, 250, 190, 280, 205),
  final_chol = c(200, 208, 210, 195, 230, 200)
)

# 2. Vectorization: Calculating Reduction and Percentage
clinical_trial$reduction <- clinical_trial$initial_chol - clinical_trial$final_chol
clinical_trial$pct_reduction <- (clinical_trial$reduction / clinical_trial$initial_chol) * 100

# 3. Advanced Indexing
# A. High Success: Treatment group AND reduction > 15%
success <- clinical_trial[clinical_trial$group == "Treatment" & clinical_trial$pct_reduction > 15, ]

# B. Negative results: Patients whose cholesterol increased (reduction < 0)
negative_results <- clinical_trial[clinical_trial$reduction < 0, c("patient", "reduction")]

# 4. Summary Functions
summary(clinical_trial$reduction)
print(clinical_trial)
print(success)


# --- EXERCISE 5: String Manipulation & Genetic Records ---

# 1. String Cleaning & Dynamic Substring Extraction
raw_records <- c("hosp12-ALA_A-agtc", "hosp45-ALA_B-ttga", "hosp09-ALA_A-cccc", "hosp33-ALA_C-aaaa")
raw_records <- toupper(raw_records)

# Extracting the last 4 characters dynamically using spaces around operators
dna_seq <- substr(raw_records, nchar(raw_records) - 3, nchar(raw_records))

# 2. Data Frame & String Concatenation
db_patients <- data.frame(
  id = 1:4,
  original_record = raw_records,
  sequence = dna_seq,
  stringsAsFactors = FALSE
)

db_patients$full_code <- paste(db_patients$id, db_patients$sequence, sep = "_")

# 3. Vectorized Logic: ifelse()
db_patients$status <- ifelse(db_patients$sequence == "TTGA", "Alert", "Normal")

# 4. Advanced Filtering using pattern matching (grepl)
wing_a_patients <- db_patients[grepl("ALA_A", db_patients$original_record), ]

# Inspection
str(db_patients)
print(db_patients)


# --- EXERCISE 6: Advanced String Manipulation (stringr) ---

library(stringr)

# 1. Setup 
raw_input <- "  John Doe  "

# 2. Basic Inspection and Manipulation
str_length(raw_input)                  
str_c("Hello", "World", sep = " ")      
str_sub(raw_input, 3, 6)                

# 3. Text Formatting and Trimming
str_to_lower(raw_input)                 
str_to_upper(raw_input)                 
clean_text <- str_trim(raw_input)       

# 4. Pattern Matching and Detection
str_detect(clean_text, "Jo")            
str_detect(clean_text, "Ja")            

# 5. Efficient Replacement
updated_name <- str_replace(clean_text, "Doe", "Smith")
modified_string <- str_replace_all(clean_text, "o", "i")

print(updated_name)


# --- EXERCISE 7: Therapeutic Response & Categorization ---

library(stringr)

# 1. Dataset Construction
study_data <- data.frame(
  patient_id = c("  PAT-001 ", " PAT-002", "PAT-003  ", " PAT-004 ", "PAT-005"),
  age = c(45, 62, 38, 71, 54),
  group = c("Placebo", "Drug_A", "Drug_A", "Placebo", "Drug_A"),
  bp_initial = c(150, 165, 145, 170, 155),
  bp_final = c(148, 130, 142, 165, 128),
  stringsAsFactors = FALSE
)

# [TASK A] String Standardization & Factor Conversion
study_data$patient_id <- str_to_upper(str_trim(study_data$patient_id))
study_data$group <- as.factor(study_data$group)

# 2. Vectorization: Calculating Efficacy
study_data$reduction <- study_data$bp_initial - study_data$bp_final

# [TASK B] Response Categorization using nested ifelse()
study_data$status <- ifelse(study_data$reduction > 15, "High Responder",
                            ifelse(study_data$reduction >= 5, "Moderate Responder", 
                                   "Non-Responder"))

# 3. Statistical Aggregation (Formula Syntax: Y ~ X)
summary_report <- aggregate(reduction ~ group, data = study_data, FUN = mean)
colnames(summary_report) <- c("Group", "Mean_Reduction")

# 4. Final Inspection
print("--- Full Clinical Dataset ---")
print(study_data)

print("--- Group Efficacy Report ---")
print(summary_report)


# --- EXERCISE 8: Patient Monitoring & Pharmacy Inventory ---

library(stringr)

# 1. Dataset Construction (Vital Signs Monitoring)
monitoring_data <- data.frame(
  patient_tag = c("ICU-01_STABLE", "ICU-02_CRITICAL", "ICU-03_STABLE", "ICU-04_ALERT", "ICU-05_CRITICAL"),
  temp_celsius = c(36.5, 38.9, 37.2, 39.5, 36.8),
  heart_rate = c(72, 110, 80, 125, 65),
  stringsAsFactors = FALSE
)

# [TASK A] String Manipulation & Cleaning via Regex
# Strips the "ICU-XX_" prefix to extract only the severity status
monitoring_data$status_label <- str_remove(monitoring_data$patient_tag, "ICU-\\d{2}_")

# [TASK B] Vectorization: Celsius to Fahrenheit Conversion
monitoring_data$temp_fahrenheit <- (monitoring_data$temp_celsius * 9 / 5) + 32

# [TASK C] Conditional Logic: Clinical Severity Triage
monitoring_data$triage_level <- ifelse(monitoring_data$temp_celsius > 39 | monitoring_data$heart_rate > 120, 
                                       "Emergency",
                                       ifelse(monitoring_data$temp_celsius >= 38, "Observation", "Normal"))

# 2. Pharmacy Inventory Logic (Pattern Matching)
inventory <- c("MED_Amoxicillin_500mg", "MED_Dipyrone_1g", "SUP_Sterile_Gauze", "MED_Paracetamol_750mg", "SUP_Syringe_5ml")

# [TASK D] Advanced Filtering with stringr Regex anchors
medications_only <- inventory[str_detect(inventory, "^MED")]

# 3. Final Inspection
print("--- Patient Monitoring Table ---")
print(monitoring_data)

print("--- Medications in Stock ---")
print(medications_only)


# --- EXERCISE 9: Hospital Bed Management & Patient Flow ---

library(stringr)

# 1. Dataset Construction (Admissions Data with Missing Values)
admissions <- data.frame(
  patient_id = c("HOSP-9901", "HOSP-9902", "HOSP-9903", "HOSP-9904", "HOSP-9905"),
  age = c(2, 45, 82, 31, 67),
  admission_date = as.Date(c("2026-03-01", "2026-03-02", "2026-03-05", "2026-03-07", "2026-03-10")),
  discharge_date = as.Date(c("2026-03-04", NA, "2026-03-15", "2026-03-08", NA)),
  unit = c("Pediatrics", "ICU", "Geriatrics", "General Ward", "ICU"),
  stringsAsFactors = FALSE
)

# [TASK A] Handling Dates: Length of Stay (LOS) Calculation
# Uses a fixed snapshot date to calculate active duration for non-discharged patients (NA)
today <- as.Date("2026-04-07")
admissions$los <- ifelse(is.na(admissions$discharge_date), 
                         as.numeric(today - admissions$admission_date), 
                         as.numeric(admissions$discharge_date - admissions$admission_date))

# [TASK B] Using cut() for Continuous to Categorical Transformation
admissions$age_group <- cut(admissions$age, 
                            breaks = c(0, 12, 18, 60, 110), 
                            labels = c("Child", "Teen", "Adult", "Senior"))

# [TASK C] Advanced Logic: Discharge Readiness Flag
admissions$action_required <- ifelse(admissions$unit == "ICU" & admissions$los > 10 & is.na(admissions$discharge_date), 
                                     "URGENT_REVIEW", "Stable")

# [TASK D] Regex Data Cleaning: Numeric ID Extraction
admissions$id_numeric <- str_extract(admissions$patient_id, "\\d+")

# [TASK E] Statistical Aggregation: Mean LOS per Unit
occupancy_report <- aggregate(los ~ unit, data = admissions, FUN = mean)

# 4. Final Inspection
print("--- Hospital Admissions Dashboard ---")
print(admissions)

print("--- Average LOS per Unit ---")
print(occupancy_report)

currently_admitted <- admissions[is.na(admissions$discharge_date), ]
print("--- Currently Occupied Beds ---")
print(currently_admitted)


# --- EXERCISE 10: Vaccination Campaign Tracking ---

library(stringr)

# 1. Dataset Construction
vaccination_data <- data.frame(
  entry_code = c("VAC_99-SINO-2026", "vac_88-PFIZ-2026", "VAC_77-ASTRA-2026", "VAC_66-SINO-2026"),
  patient = c("RICARDO GOMES", "ANA COSTA", "paulo souza", "BEATRIZ LIMA"),
  age = c(55, 29, 42, 68),
  first_dose_date = as.Date(c("2026-01-10", "2026-01-15", "2026-02-01", "2026-02-10")),
  dose_interval_days = c(28, 21, 90, 28),
  stringsAsFactors = FALSE
)

# [TASK A] Text Standardization (Title Case and Uppercase)
vaccination_data$patient <- str_to_title(vaccination_data$patient)
vaccination_data$entry_code <- str_to_upper(vaccination_data$entry_code)

# [TASK B] Date Arithmetic: Second Dose Scheduling
vaccination_data$second_dose_date <- vaccination_data$first_dose_date + vaccination_data$dose_interval_days

# [TASK C] Advanced Regex: Vaccine Brand Extraction
# Uses lookbehind and lookahead assertions to isolate text between hyphens
vaccination_data$vaccine_brand <- str_extract(vaccination_data$entry_code, "(?<=-)[A-Z]+(?=-)")

# [TASK D] Conditional Logic: Target Demographic Triage
vaccination_data$group_type <- ifelse(vaccination_data$age >= 60, "Priority", "Routine")

# [TASK E] Campaign Status Tracking against Reference Date
reference_date <- as.Date("2026-03-15")
vaccination_data$status <- ifelse(vaccination_data$second_dose_date < reference_date, 
                                  "Overdue", "Scheduled")

# 2. Final Inspection & Summary Statistics
print("--- Vaccination Campaign Dashboard ---")
print(vaccination_data)

age_report <- aggregate(age ~ vaccine_brand, data = vaccination_data, FUN = mean)
print("--- Average Age per Vaccine ---")
print(age_report)


# --- EXERCISE 11: Data Types, Factors & Implicit Coercion ---

# 1. Understanding Atomic Vectors & Coercion
# R vectors must hold a single data type. If mixed, R forces a conversion (coercion).
patient_ids <- c(101L, 102L, 103L, 104L) # Integer (Note the 'L' suffix)
is_active <- c(TRUE, TRUE, FALSE, TRUE)  # Logical

# CRITICAL CONCEPT: Mixing Character and Logical forces everything to Character
mixed_vector <- c("HOSP-A", TRUE, 45)
print(mixed_vector) # Notice how R converted everything to "HOSP-A", "TRUE", "45"

# 2. Advanced Factor Manipulation (Essential for Categorical Clinical Data)
# Simulating clinical trial phases that need a specific logical order
raw_phases <- c("Phase III", "Phase I", "Phase II", "Phase III", "Phase I")

# Converting to an Ordered Factor to protect data hierarchy in statistical models
ordered_phases <- factor(raw_phases, 
                         levels = c("Phase I", "Phase II", "Phase III"), 
                         ordered = TRUE)

# 3. Data Frame Assembly and Structure Inspection
clinical_metadata <- data.frame(
  id = patient_ids,
  active = is_active,
  trial_phase = ordered_phases,
  stringsAsFactors = FALSE
)

# Vectorized logic check on Factor levels
high_phase_patients <- clinical_metadata[clinical_metadata$trial_phase >= "Phase II", ]

# 4. Final Inspection
str(clinical_metadata)
print(high_phase_patients)


# --- EXERCISE 12: Matrices & Grid-Based Diagnostic Data ---

# 1. Matrix Construction (Homogeneous 2D Structure)
# Simulating a 3x3 grid of a simplified digital medical image scan (pixel intensity)
# Data goes from 0 (black) to 255 (white)
scan_pixels <- c(120, 150, 80, 240, 255, 190, 95, 110, 130)

# Creating a 3x3 matrix ordered by rows
diagnostic_matrix <- matrix(scan_pixels, nrow = 3, ncol = 3, byrow = TRUE)

# Assigning dimension names for professional traceability
rownames(diagnostic_matrix) <- c("Row_1", "Row_2", "Row_3")
colnames(diagnostic_matrix) <- c("Col_A", "Col_B", "Col_C")

# 2. Mathematical Matrix Operations (Vectorized Scalar Operations)
# Simulating a contrast adjustment filter: multiplying all elements by a factor
brightened_matrix <- diagnostic_matrix * 1.15

# 3. Matrix Indexing and Subsetting
# Extracting a specific Region of Interest (ROI): Row 2, Columns B and C
region_of_interest <- diagnostic_matrix[2, c("Col_B", "Col_C")]

# 4. Diagnostic Metrics (Matrix Margins)
# rowSums and colMeans are highly optimized native mathematical functions
row_total_intensity <- rowSums(diagnostic_matrix)
col_mean_intensity  <- colMeans(diagnostic_matrix)

# Final Inspection
print("--- Original Diagnostic Matrix ---")
print(diagnostic_matrix)

print("--- Extracted Region of Interest ---")
print(region_of_interest)

print("--- Column-wise Mean Intensities ---")
print(col_mean_intensity)


# --- EXERCISE 13: Lists as Complex Metadata Containers ---

# 1. List Construction (Heterogeneous Structure)
# Lists allow storing objects of different types, shapes, and sizes in a single variable
lab_results_list <- list(
  facility_name = "Central Metro Labs",                        # Character scalar
  operational_status = TRUE,                                   # Logical scalar
  equipment_ids = c(445, 882, 119),                            # Integer vector
  reference_ranges = data.frame(                               # Data Frame
    test = c("Glucose", "Creatinine"),
    min_val = c(70, 0.6),
    max_val = c(100, 1.2)
  )
)

# 2. Advanced List Indexing ($ vs [[]] vs [])
# A. Single brackets [] return a LIST containing the element (keeps structure)
sub_list <- lab_results_list[1] 

# B. Double brackets [[]] or $ return the ACTUAL object inside the element
actual_name <- lab_results_list[[1]]
facility_status <- lab_results_list$operational_status

# 3. Dynamic Expansion (Adding elements to an existing list)
# Appending a newly calculated vector into the list dynamically
lab_results_list$daily_sample_counts <- c(140, 185, 92, 210, 305)

# 4. Accessing Nested Data Structures
# Extracting the maximum creatinine value from the data frame inside the list
max_creatinine <- lab_results_list$reference_ranges$max_val[2]

# Final Inspection
str(lab_results_list)
print(paste("Facility Name extracted:", actual_name))
print(paste("Extracted Nested Value (Max Creatinine):", max_creatinine))


# --- EXERCISE 14: Flow Control (Conditional Logic and Iteration) ---

# 1. Complex Conditionals (if, else if, else)
# This construct checks single scalar values (unlike the vectorized ifelse)
patient_systolic <- 135

if (patient_systolic < 120) {
  clinical_category <- "Normal"
} else if (patient_systolic >= 120 & patient_systolic < 130) {
  clinical_category <- "Elevated"
} else {
  clinical_category <- "Hypertension"
}

# 2. Iteration: Processing elements with a 'for' loop
# Useful when sequential dependencies exist or for executing external tasks
heart_rates <- c(72, 105, 68, 120, 84)
tachycardia_count <- 0

for (hr in heart_rates) {
  if (hr > 100) {
    tachycardia_count <- tachycardia_count + 1
  }
}

# 3. Iteration: 'while' loop for simulation thresholds
# Simulating a decreasing drug concentration in the bloodstream over hours
concentration <- 100 # Initial percentage
hours <- 0

while (concentration > 15) {
  concentration <- concentration * 0.80 # Decreases by 20% each hour
  hours <- hours + 1
}

# 4. Final Inspection
print(paste("Single patient category:", clinical_category))
print(paste("Patients with tachycardia:", tachycardia_count))
print(paste("Hours until drug concentration drops below 15%:", hours))


# --- EXERCISE 15: Special Values & Missing Data Management ---

# 1. Understanding Special Values in R (NA, NaN, Inf)
# NA = Not Available (Missing), NaN = Not a Number (Undefined), Inf = Infinity
experimental_data <- c(14.5, NA, 18.2, 0/0, 22.1, 5/0)
print(experimental_data) # Contains numbers, NA, NaN, and Inf

# 2. Identifying Special Values with Logical Masks
has_na  <- is.na(experimental_data)   # True for BOTH NA and NaN
has_nan <- is.nan(experimental_data) # True ONLY for NaN
has_inf <- is.infinite(experimental_data)

# 3. Handling Missing Data (NA) in Math Functions
# Native functions like mean() return NA if any value is missing, unless instructed
bad_mean <- mean(experimental_data) # Returns NA
good_mean <- mean(experimental_data, na.rm = TRUE) # Strips NA/NaN before computing

# 4. Cleaning Data Frames (Complete Case Analysis)
patient_survey <- data.frame(
  id = 1:4,
  score = c(8, 10, NA, 7),
  weight = c(70, NA, 65, 82)
)

# na.omit() drops any row containing at least one NA value
cleaned_survey <- na.omit(patient_survey)

# Final Inspection
print(paste("Mean calculated by ignoring NAs:", good_mean))
print("--- Cleaned Patient Survey (Complete Cases) ---")
print(cleaned_survey)


# --- EXERCISE 16: Building Custom Functions for Healthcare Analytics ---

# 1. Custom Function with Default Arguments
# A modular, reusable function to calculate Body Mass Index (BMI)
# Weight in kg, height in meters. Default height is set to 1.70 if omitted
calculate_bmi <- function(weight, height = 1.70) {
  
  # Defensive Programming: Ensure inputs are numeric
  if (!is.numeric(weight) | !is.numeric(height)) {
    stop("Inputs must be numeric values.")
  }
  
  bmi <- weight / (height ^ 2)
  return(bmi)
}

# 2. Executing the Custom Function (Scalar and Vectorized Input)
# R functions naturally accept vectors if they use vectorized internal math
single_patient_bmi <- calculate_bmi(weight = 75, height = 1.82)

patient_weights <- c(60, 85, 110, 52)
calculated_vector <- calculate_bmi(weight = patient_weights) # Uses default height (1.70)

# 3. Integrating Functions into Data Frames
cohort_df <- data.frame(
  w = c(70, 95, 55),
  h = c(1.75, 1.80, 1.60)
)

# Dynamically creating a new column using our function
cohort_df$bmi_result <- calculate_bmi(weight = cohort_df$w, height = cohort_df$h)

# 4. Final Inspection
print(paste("Single BMI Result:", round(single_patient_bmi, 2)))
print("--- Vectorized BMI on Cohort Data Frame ---")
print(cohort_df)

# --- EXERCISE 17: The 'apply' Family (Functional Programming) ---

# 1. Using 'apply' on Matrices (Dimension-based execution)
# Simulating a 4-patient panel tracking blood pressure metrics across 3 days
bp_matrix <- matrix(
  c(120, 122, 118,
    140, 145, 142,
    115, 110, 112,
    130, 135, 128), 
  nrow = 4, ncol = 3, byrow = TRUE
)
rownames(bp_matrix) <- c("Pat_1", "Pat_2", "Pat_3", "Pat_4")

# MARGIN = 1 means Rows. Calculates maximum blood pressure for each patient.
patient_max_bp <- apply(bp_matrix, MARGIN = 1, FUN = max)

# 2. Using 'lapply' and 'sapply' on Vectors/Lists
patient_notes <- c("Discharged on Monday", "Transferred to ICU", "Under observation")

# lapply() returns a LIST containing the results
split_words_list <- lapply(patient_notes, str_split, pattern = " ")

# sapply() simplifies the output into a structured VECTOR or MATRIX if possible
string_lengths <- sapply(patient_notes, nchar)

# 3. Final Inspection
print("--- Maximum BP per Patient (apply) ---")
print(patient_max_bp)

print("--- Text Lengths per Note (sapply) ---")
print(string_lengths)


# --- EXERCISE 18: Advanced Data Splitting and Subgroup Analyses ---

# 1. Dataset Construction (Multicenter Clinical Trial Data)
multicenter_df <- data.frame(
  hospital = c("Hosp_A", "Hosp_B", "Hosp_A", "Hosp_B", "Hosp_A", "Hosp_B"),
  treatment = c("Drug", "Drug", "Placebo", "Placebo", "Drug", "Placebo"),
  recovery_score = c(85, 92, 40, 55, 88, 50)
)

# 2. Using 'split()' to Segment Data
# Separates the data frame into a list of data frames based on a factor variable
hospital_subsets <- split(multicenter_df, multicenter_df$hospital)

# 3. Advanced Group Evaluation with 'by()'
# Applies a custom function to data subsets grouped by one or more factors
# Calculating the average recovery score grouped simultaneously by Hospital AND Treatment
subgroup_means <- by(
  data = multicenter_df$recovery_score, 
  INDICES = list(multicenter_df$hospital, multicenter_df$treatment), 
  FUN = mean
)

# 4. Final Inspection
print("--- Subset Generated for Hospital A ---")
print(hospital_subsets$Hosp_A)

print("--- Multi-factor Aggregation Report (by) ---")
print(subgroup_means)


# --- EXERCISE 19: Defensive Programming and Error Handling (tryCatch) ---

# 1. The Necessity of Error Catching
# In pipelines, a single broken input can crash hours of computations.
# tryCatch allows the script to gracefully log errors and keep running.

# Simulating a chaotic list of lab results where some inputs are corrupted (text instead of numbers)
raw_lab_inputs <- list(patient_1 = 12.5, patient_2 = "Error_Corrupted_Value", patient_3 = 14.1)

# 2. Implementing tryCatch inside a Loop
processed_results <- numeric(length(raw_lab_inputs))
names(processed_results) <- names(raw_lab_inputs)

for (i in seq_along(raw_lab_inputs)) {
  
  processed_results[i] <- tryCatch({
    # TRY BLOCK: Intended logic goes here
    # Attempting to add a scalar to the value (will fail if input is text)
    raw_lab_inputs[[i]] + 2.5
    
  }, error = function(e) {
    # ERROR BLOCK: Executes ONLY if an error occurs in the Try block
    # Return a safe fallback value (NA) and log a custom message
    message(paste("Warning: Evaluation failed for index", i, "-", e$message))
    return(NA)
    
  }, finally = {
    # FINALLY BLOCK: Optional. Executes no matter what (success or failure)
    # Good for closing file connections or logging progress checkpoints
  })
}

# 3. Final Inspection
print("--- Safe Evaluation Output Vector ---")
print(processed_results)

# --- EXERCISE 20: Deconstructing Object Attributes & Metadata ---

# 1. Understanding Object Metadata
# In R, complex structures (factors & matrices) are just atomic vectors with attributes.
raw_scores <- c(1, 2, 2, 3, 1)

# Adding factor attributes to a numeric vector manually via structure()
# This shows how R under the hood builds a categorical variable
custom_factor <- structure(raw_scores, 
                           class = "factor", 
                           levels = c("Mild", "Moderate", "Severe"))

# 2. Matrix Dimensions as Attributes
# A matrix is physically an atomic vector with a "dim" attribute attached
flat_temperatures <- c(36.2, 37.5, 38.9, 36.8, 37.0, 39.1)

# Inspecting attributes BEFORE transformation
initial_attr <- attributes(flat_temperatures) # Returns NULL

# Transforming the vector into a 3x2 matrix by injecting the dimension attribute
attr(flat_temperatures, "dim") <- c(3, 2)
print(flat_temperatures) # It behaves exactly like a matrix now

# 3. Checking Classes vs Storage Types (typeof vs class)
# 'typeof' shows how data is stored in memory; 'class' shows how R interprets it
storage_type <- typeof(custom_factor) # Returns "integer"
interpreted_class <- class(custom_factor) # Returns "factor"

# 4. Final Inspection
print("--- Custom Built Factor ---")
print(custom_factor)
print(paste("Storage Type:", storage_type, "| Object Class:", interpreted_class))


# --- EXERCISE 21: Flattening Lists & Transforming Structures ---

# 1. The Disconnected Data Problem
# Simulating a common pipeline issue: data arriving as an unstructured list of patients
raw_patient_list <- list(
  p1 = list(name = "Alice", age = 28, group = "A"),
  p2 = list(name = "Bob", age = 34, group = "B"),
  p3 = list(name = "Charlie", age = 19, group = "A")
)

# 2. Transforming Lists into Data Frames using do.call()
# do.call executes a function (like rbind) across all elements of a list simultaneously
# First, convert each sub-list into a single-row data frame using lapply
dataframe_rows <- lapply(raw_patient_list, as.data.frame)

# Combine all rows into a consolidated master Data Frame
patient_registry <- do.call(rbind, dataframe_rows)

# 3. Fixing Coerced Types in the Flattened Data Frame
# Row binding unstandardized lists often converts factors/numbers into characters
patient_registry$age <- as.numeric(patient_registry$age)
patient_registry$group <- as.factor(patient_registry$group)

# 4. Final Inspection
str(patient_registry)
print("--- Reconstructed Master Data Frame ---")
print(patient_registry)


# --- EXERCISE 22: Matrix Algebra in Epidemiological Modeling ---

# 1. Vector and Matrix Setups
# Matrix A: Probability distribution of disease progression stages (Rows: Mild, Severe) 
# Columns represent probabilities across 2 different age groups
transition_matrix <- matrix(
  c(0.70, 0.30,  # Age group 1
    0.45, 0.55), # Age group 2
  nrow = 2, ncol = 2, byrow = TRUE
)

# Vector B: Total initial patient counts in each age group (1500 in Group 1, 850 in Group 2)
initial_patients <- c(1500, 850)

# 2. Proper Matrix Multiplication (%*%) vs Scalar Multiplication (*)
# CRITICAL CONCEPT: '*' performs element-wise multiplication. 
# '%*%' performs true mathematical matrix dot-product multiplication.
predicted_outcomes <- initial_patients %*% transition_matrix

# 3. Matrix Transposition
# Changing the orientation of the layout using the native t() function
transposed_matrix <- t(transition_matrix)

# 4. Final Inspection
print("--- Transition Probability Matrix ---")
print(transition_matrix)

print("--- Forecasted Patient Distribution (Dot-Product) ---")
# Result represents total expected patients ending up in [Mild, Severe] conditions
print(predicted_outcomes)
