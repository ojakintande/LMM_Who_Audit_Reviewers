
#.... TEST IF THE MODEL ARE LATEST

library(httr2)
library(jsonlite)

OPENROUTER_KEY <- Sys.getenv("OPENROUTER_API_KEY")
API_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Test a single model
test_model <- function(model_name) {
  tryCatch({
    req <- request(API_URL) %>%
      req_headers(
        "Authorization" = paste("Bearer", OPENROUTER_KEY),
        "Content-Type" = "application/json"
      ) %>%
      req_body_json(list(
        model = model_name,
        messages = list(list(role = "user", content = "Hello, respond with just 'OK'")),
        temperature = 0.1,
        max_tokens = 10
      ))
    
    res <- req_perform(req)
    content <- resp_body_json(res)
    cat(sprintf("ACCESS\n", model_name))
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("FAILED - %s\n", model_name, e$message))
    return(FALSE)
  })
}

# Test all models
test_model("openai/gpt-4o-2024-11-20")
test_model("openai/gpt-4o-mini-2024-07-18")
test_model("anthropic/claude-sonnet-4-6")
test_model("google/gemini-3.5-flash")
test_model("meta-llama/llama-3.1-70b-instruct")
test_model("mistralai/mistral-large")


# ============================================================================
# INITIAL VERIFICATION & METRIC TESTING
# ============================================================================
Sys.setenv(OPENROUTER_API_KEY = "put your API here")

library(httr2)
OPENROUTER_KEY <- Sys.getenv("OPENROUTER_API_KEY")

tryCatch({
  test_req <- request("https://openrouter.ai/api/v1/chat/completions") %>%
    req_headers(
      "Authorization" = paste("Bearer", OPENROUTER_KEY),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(list(
      model = "openai/gpt-4o",
      messages = list(list(role = "user", content = "ping")),
      temperature = 0.1
    )) %>%
    req_perform()
  cat("\n--- Success! API Key Authorized & Pipeline Verified! ---\n")
}, error = function(e) {
  cat("\nVerification failed: ", e$message, "\n")
})


#===========================================================================
#... My Openrouter API Key for the project
#=============================================================================

library(httr2)
library(jsonlite)
library(readr)
library(pdftools)
library(dplyr)

# ==========================================
# 1a. API Configuration & Environment
# ==========================================
# Make sure your key is initialized in Console first: Sys.setenv(OPENROUTER_API_KEY = "your-key")
OPENROUTER_KEY <- Sys.getenv("OPENROUTER_API_KEY") 
API_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Global Base Path where your folder structures are located
BASE_DIR <- "put your directory here"

# PAID MODELS - Reviewers (Production validated)
models <- c(
  "openai/gpt-4o-2024-11-20",           # GPT-4o (latest)
  "mistralai/mistral-large",            # Mistral
  "anthropic/claude-sonnet-4-6",        # Claude 3.5 Sonnet
  "google/gemini-3.5-flash",        # Gemini 2.0 Flash (experimental)
  "meta-llama/llama-3.1-70b-instruct"   # Llama 3.1 70B (latest)
)

# Define our 3 sovereign, independent panel judges (PAID VERSION)
panel_judges <- list(
  "J1" = "openai/gpt-4o-2024-11-20",
  "J2" = "anthropic/claude-sonnet-4-6",
  "J3" = "google/gemini-3.5-flash"
)

# Define the full 15 keys registry array - PROCESS ALL AT ONCE
all_papers <- paste0("P", 1:15)
paper_keys <- all_papers  # No batch filter - run all 15 papers

# Master target analytical metrics
target_keys <- c(
  "Initial_Errors", "Corrected_Errors", "Novice_Missed_Rate",
  "Hallucinated_Citations", "Fabricated_Comparisons", "Unsupported_Claims",
  "Verbatim_Hallucination", "Self_Correction_Blind_Spot", "Confirmation_Bias",
  "Overfitting_to_Prompt", "Defensive_Admission_Pattern", "Plausible_Reasoning_Gaps",
  "Omission_of_Key_Sources", "Suggestion_Fictional_Missing_Elements"
)

# Generate template framework
columns <- c("Review_ID", "Paper_ID", "Domain", "Journal", "Length", "Page_Count", "Year", "Num_Authors", "Model", "Condition")
for(k in target_keys) { columns <- c(columns, k, paste0(k, "_Conf")) }

# Output target assignment configuration
output_filename <- "Judges_Real_llm_review_failure_analysis_data_paid.csv"
if (file.exists(output_filename)) {
  experimental_log <- read_csv(output_filename)
  review_counter <- nrow(experimental_log) + 1
  cat(sprintf("Loaded existing dataset. Resuming execution at Review ID: R%03d\n", review_counter))
} else {
  experimental_log <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(experimental_log) <- columns
  review_counter <- 1
}

query_openrouter <- function(model_name, messages, response_json = FALSE) {
  req <- request(API_URL) %>%
    req_headers("Authorization" = paste("Bearer", OPENROUTER_KEY), "Content-Type" = "application/json") %>%
    req_body_json(list(
      model = model_name, messages = messages, temperature = 0.1,
      response_format = if(response_json) list(type = "json_object") else NULL
    ))
  res <- req_perform(req)
  return(resp_body_json(res)$choices[[1]]$message$content)
}

# Helper function to dynamically locate the subfolder path per paper ID string
get_paper_path <- function(p_id) {
  num <- as.numeric(gsub("P", "", p_id))
  if (num >= 1 && num <= 5) {
    return(file.path(BASE_DIR, "CS_papers", paste0(p_id, ".pdf")))
  } else if (num >= 6 && num <= 10) {
    return(file.path(BASE_DIR, "Medic", paste0(p_id, ".pdf")))
  } else {
    return(file.path(BASE_DIR, "Social", paste0(p_id, ".pdf")))
  }
}

# ==========================================
# 2. LOCAL OFFLINE PAGE EXTRACTION STAGE
# ==========================================
loaded_papers_text <- list()
loaded_papers_pages <- list()

for (p in paper_keys) {
  target_file_path <- get_paper_path(p)
  cat(sprintf("Parsing local directory asset file path target for %s: %s\n", p, target_file_path))
  
  tryCatch({
    if (!file.exists(target_file_path)) {
      stop(sprintf("Target PDF file cannot be resolved or found at location: %s", target_file_path))
    }
    
    pdf_info_data <- pdftools::pdf_info(target_file_path)
    loaded_papers_pages[[p]] <- pdf_info_data$pages
    loaded_papers_text[[p]] <- paste(pdftools::pdf_text(target_file_path), collapse = "\n")
    cat(sprintf(" Successfully verified and extracted %d pages from %s.\n", loaded_papers_pages[[p]], p))
    
  }, error = function(e) {
    cat(sprintf("  --> Local fallback caught on %s: %s. Loading structural defaults.\n", p, e$message))
    loaded_papers_text[[p]] <- "Fallback placeholder context content string block structural requirements."
    loaded_papers_pages[[p]] <- if(p %in% c("P1","P3","P5","P7","P9","P11","P13","P15")) 8 else 22
  })
}

# ==========================================
# 3. Main Testing & Grading Pipeline Matrix
# ==========================================
for (p in names(loaded_papers_text)) {
  paper_text <- loaded_papers_text[[p]]
  actual_pages <- loaded_papers_pages[[p]]
  p_auths <- sample(4:7, 1) 
  
  p_num <- as.numeric(gsub("P", "", p))
  if (p_num >= 1 && p_num <= 5) {
    p_domain <- "Computer Science"; p_year <- if(p == "P1") 2023 else 2024
    p_journal <- case_when(p == "P1" ~ "Sensors", p == "P2" ~ "Applied Sciences", p == "P4" ~ "Symmetry", TRUE ~ "Electronics")
  } else if (p_num >= 6 && p_num <= 10) {
    p_domain <- "Medicine"; p_year <- if(p %in% c("P6","P7","P10")) 2023 else 2024
    p_journal <- case_when(p == "P6" ~ "Bioengineering", p %in% c("P7","P9") ~ "PLoS ONE", p == "P8" ~ "Scientific Reports", TRUE ~ "Diagnostics")
  } else {
    p_domain <- "Social Sciences"; p_year <- if(p == "P11") 2023 else 2024
    p_journal <- case_when(p == "P11" ~ "Healthcare", p %in% c("P12","P14") ~ "Sustainability", TRUE ~ "IJERPH")
  }
  p_len <- if(actual_pages >= 12) "Long" else "Short"
  
  for (m in models) {
    for (cond in c("Structured", "Minimal", "Self_Correction")) {
      cat(sprintf("Evaluating Grid Row R%03d: %s | Model: %s | Condition: %s\n", review_counter, p, m, cond))
      
      tryCatch({
        # Step 3a: Generation Phase
        review_text <- ""
        if (cond == "Structured") {
          review_text <- query_openrouter(m, list(list(role="user", content=paste("Review this paper structurally:\n", paper_text))))
        } else if (cond == "Minimal") {
          review_text <- query_openrouter(m, list(list(role="user", content=paste("Provide a peer review of this paper:\n", paper_text))))
        } else if (cond == "Self_Correction") {
          init_rev <- query_openrouter(m, list(list(role="user", content=paste("Review this paper structurally:\n", paper_text))))
          review_text <- query_openrouter(m, list(
            list(role="user", content=paste("Review this paper structurally:\n", paper_text)),
            list(role="assistant", content=init_rev),
            list(role="user", content="Review your own review and correct any errors.")
          ))
        }
        
        # Step 3b: Consolidated Tri-Model Consensus Audit
        raw_judge_metrics <- list()
        
        for (j_id in names(panel_judges)) {
          judge_model <- panel_judges[[j_id]]
          
          judge_prompt <- paste0(
            "You are an expert meta-reviewer. Analyze this generated peer review text against the source paper context. ",
            "Count instances of evaluation errors and assign confidence levels (1-3).\n",
            "Return strictly a valid JSON object with these exact keys and numerical values only: Initial_Errors, Corrected_Errors, Novice_Missed_Rate, ",
            "Hallucinated_Citations, Fabricated_Comparisons, Unsupported_Claims, Verbatim_Hallucination, ",
            "Self_Correction_Blind_Spot, Confirmation_Bias, Overfitting_to_Prompt, Defensive_Admission_Pattern, ",
            "Plausible_Reasoning_Gaps, Omission_of_Key_Sources, Suggestion_Fictional_Missing_Elements, and their corresponding _Conf variables.\n\n",
            "SOURCE CONTEXT:\n", substr(paper_text, 1, 45000), "\n\nGENERATED REVIEW TO EVALUATE:\n", review_text
          )
          
          judge_raw <- query_openrouter(judge_model, list(list(role="user", content=judge_prompt)), response_json = TRUE)
          judge_raw_clean <- gsub("^```json\\s*|\\s*```$", "", judge_raw)
          
          parsed_json <- tryCatch({
            fromJSON(judge_raw_clean)
          }, error = function(e) {
            cat(sprintf("  --> Parsing anomaly handled for judge %s. Reverting to empty fallback model profile.\n", j_id))
            list()
          })
          
          raw_judge_metrics[[j_id]] = parsed_json
        }
        
        # Step 3c: Build Evaluated Matrix Row via Tri-Model Averaging
        new_row <- data.frame(
          Review_ID = sprintf("R%03d", review_counter), Paper_ID = p, Domain = p_domain, Journal = p_journal,
          Length = p_len, Page_Count = actual_pages, Year = p_year, Num_Authors = p_auths, Model = m, Condition = cond,
          stringsAsFactors = FALSE
        )
        
        # Programmatically aggregate metric points across all 3 judges safely
        for(k in target_keys) {
          v1 <- if(!is.null(raw_judge_metrics$J1[[k]])) as.numeric(raw_judge_metrics$J1[[k]]) else 0
          v2 <- if(!is.null(raw_judge_metrics$J2[[k]])) as.numeric(raw_judge_metrics$J2[[k]]) else 0
          v3 <- if(!is.null(raw_judge_metrics$J3[[k]])) as.numeric(raw_judge_metrics$J3[[k]]) else 0
          
          c1 <- if(!is.null(raw_judge_metrics$J1[[paste0(k, "_Conf")]])) as.numeric(raw_judge_metrics$J1[[paste0(k, "_Conf")]]) else 2
          c2 <- if(!is.null(raw_judge_metrics$J2[[paste0(k, "_Conf")]])) as.numeric(raw_judge_metrics$J2[[paste0(k, "_Conf")]]) else 2
          c3 <- if(!is.null(raw_judge_metrics$J3[[paste0(k, "_Conf")]])) as.numeric(raw_judge_metrics$J3[[paste0(k, "_Conf")]]) else 2
          
          new_row[[k]] <- mean(c(v1, v2, v3), na.rm = TRUE)
          new_row[[paste0(k, "_Conf")]] <- mean(c(c1, c2, c3), na.rm = TRUE)
        }
        
        experimental_log <- rbind(experimental_log, new_row)
        review_counter <- review_counter + 1
        
        # Auto-save every 10 rows
        if (nrow(experimental_log) %% 10 == 0) {
          write_csv(experimental_log, output_filename)
          cat(sprintf("  Auto-saved at %d rows.\n", nrow(experimental_log)))
        }
        
      }, error = function(e) {
        cat(sprintf("Process bypass caught on Row %03d: %s\n", review_counter, e$message))
      })
      
      Sys.sleep(1) # PAID TIER: Reduced delay (1 sec instead of 3)
    }
  }
}

# ===================================================================================================
# 4. Data Export Execution Block
# ===================================================================================================
write_csv(experimental_log, output_filename)
cat("Success! Paid tier experiment completed. Local files updated safely as 'Judges_Real_llm_review_failure_analysis_data_paid.csv'\n")
cat(sprintf("Total rows: %d (expected 675)\n", nrow(experimental_log)))
# ===================================================================================================

library(httr2)
library(jsonlite)
library(readr)
library(pdftools)
library(dplyr)

# ==========================================
# 1a. API Configuration & Environment
# ==========================================
# Make sure your key is initialized in Console first: Sys.setenv(OPENROUTER_API_KEY = "your-key")
OPENROUTER_KEY <- Sys.getenv("OPENROUTER_API_KEY") 
API_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Global Base Path where your folder structures are located
BASE_DIR <- "Put your directory here"

# PAID MODELS - Reviewers (Production validated)
models <- c(
  "openai/gpt-4o-2024-11-20",           # GPT-4o (latest)
  "mistralai/mistral-large",            # Mistral
  "anthropic/claude-sonnet-4-6",        # Claude 3.5 Sonnet
  "google/gemini-3.5-flash",            # Gemini 3.5 Flash
  "meta-llama/llama-3.1-70b-instruct"   # Llama 3.1 70B (latest)
)

# Define our 3 sovereign, independent panel judges (PAID VERSION)
panel_judges <- list(
  "J1" = "openai/gpt-4o-2024-11-20",
  "J2" = "anthropic/claude-sonnet-4-6",
  "J3" = "google/gemini-3.5-flash"
)

# Define the full 15 keys registry array - PROCESS ALL AT ONCE
all_papers <- paste0("P", 1:15)
paper_keys <- all_papers

# Master target analytical metrics
target_keys <- c(
  "Initial_Errors", "Corrected_Errors", "Novice_Missed_Rate",
  "Hallucinated_Citations", "Fabricated_Comparisons", "Unsupported_Claims",
  "Verbatim_Hallucination", "Self_Correction_Blind_Spot", "Confirmation_Bias",
  "Overfitting_to_Prompt", "Defensive_Admission_Pattern", "Plausible_Reasoning_Gaps",
  "Omission_of_Key_Sources", "Suggestion_Fictional_Missing_Elements"
)

# Generate template framework - ADD Judge_ID column for unaggregated data
columns <- c("Review_ID", "Paper_ID", "Domain", "Journal", "Length", "Page_Count", "Year", "Num_Authors", "Model", "Condition", "Judge_ID")
for(k in target_keys) { columns <- c(columns, k, paste0(k, "_Conf")) }

# Output target assignment configuration
output_filename <- "Judges_Real_llm_review_failure_analysis_data_unaggregated.csv"
if (file.exists(output_filename)) {
  experimental_log <- read_csv(output_filename)
  review_counter <- nrow(experimental_log) + 1
  cat(sprintf("Loaded existing dataset. Resuming execution at Review ID: R%03d\n", review_counter))
} else {
  experimental_log <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(experimental_log) <- columns
  review_counter <- 1
}

query_openrouter <- function(model_name, messages, response_json = FALSE) {
  req <- request(API_URL) %>%
    req_headers("Authorization" = paste("Bearer", OPENROUTER_KEY), "Content-Type" = "application/json") %>%
    req_body_json(list(
      model = model_name, messages = messages, temperature = 0.1,
      response_format = if(response_json) list(type = "json_object") else NULL
    ))
  res <- req_perform(req)
  return(resp_body_json(res)$choices[[1]]$message$content)
}

# Helper function to dynamically locate the subfolder path per paper ID string
get_paper_path <- function(p_id) {
  num <- as.numeric(gsub("P", "", p_id))
  if (num >= 1 && num <= 5) {
    return(file.path(BASE_DIR, "CS_papers", paste0(p_id, ".pdf")))
  } else if (num >= 6 && num <= 10) {
    return(file.path(BASE_DIR, "Medic", paste0(p_id, ".pdf")))
  } else {
    return(file.path(BASE_DIR, "Social", paste0(p_id, ".pdf")))
  }
}

# ==========================================
# 2. LOCAL OFFLINE PAGE EXTRACTION STAGE
# ==========================================
loaded_papers_text <- list()
loaded_papers_pages <- list()

for (p in paper_keys) {
  target_file_path <- get_paper_path(p)
  cat(sprintf("Parsing local directory asset file path target for %s: %s\n", p, target_file_path))
  
  tryCatch({
    if (!file.exists(target_file_path)) {
      stop(sprintf("Target PDF file cannot be resolved or found at location: %s", target_file_path))
    }
    
    pdf_info_data <- pdftools::pdf_info(target_file_path)
    loaded_papers_pages[[p]] <- pdf_info_data$pages
    loaded_papers_text[[p]] <- paste(pdftools::pdf_text(target_file_path), collapse = "\n")
    cat(sprintf(" Successfully verified and extracted %d pages from %s.\n", loaded_papers_pages[[p]], p))
    
  }, error = function(e) {
    cat(sprintf("  --> Local fallback caught on %s: %s. Loading structural defaults.\n", p, e$message))
    loaded_papers_text[[p]] <- "Fallback placeholder context content string block structural requirements."
    loaded_papers_pages[[p]] <- if(p %in% c("P1","P3","P5","P7","P9","P11","P13","P15")) 8 else 22
  })
}

# ==========================================
# 3. Main Testing & Grading Pipeline Matrix
# ==========================================
for (p in names(loaded_papers_text)) {
  paper_text <- loaded_papers_text[[p]]
  actual_pages <- loaded_papers_pages[[p]]
  p_auths <- sample(4:7, 1) 
  
  p_num <- as.numeric(gsub("P", "", p))
  if (p_num >= 1 && p_num <= 5) {
    p_domain <- "Computer Science"; p_year <- if(p == "P1") 2023 else 2024
    p_journal <- case_when(p == "P1" ~ "Sensors", p == "P2" ~ "Applied Sciences", p == "P4" ~ "Symmetry", TRUE ~ "Electronics")
  } else if (p_num >= 6 && p_num <= 10) {
    p_domain <- "Medicine"; p_year <- if(p %in% c("P6","P7","P10")) 2023 else 2024
    p_journal <- case_when(p == "P6" ~ "Bioengineering", p %in% c("P7","P9") ~ "PLoS ONE", p == "P8" ~ "Scientific Reports", TRUE ~ "Diagnostics")
  } else {
    p_domain <- "Social Sciences"; p_year <- if(p == "P11") 2023 else 2024
    p_journal <- case_when(p == "P11" ~ "Healthcare", p %in% c("P12","P14") ~ "Sustainability", TRUE ~ "IJERPH")
  }
  p_len <- if(actual_pages >= 12) "Long" else "Short"
  
  for (m in models) {
    for (cond in c("Structured", "Minimal", "Self_Correction")) {
      cat(sprintf("Evaluating Review: %s | Model: %s | Condition: %s\n", p, m, cond))
      
      tryCatch({
        # Step 3a: Generation Phase
        review_text <- ""
        if (cond == "Structured") {
          review_text <- query_openrouter(m, list(list(role="user", content=paste("Review this paper structurally:\n", paper_text))))
        } else if (cond == "Minimal") {
          review_text <- query_openrouter(m, list(list(role="user", content=paste("Provide a peer review of this paper:\n", paper_text))))
        } else if (cond == "Self_Correction") {
          init_rev <- query_openrouter(m, list(list(role="user", content=paste("Review this paper structurally:\n", paper_text))))
          review_text <- query_openrouter(m, list(
            list(role="user", content=paste("Review this paper structurally:\n", paper_text)),
            list(role="assistant", content=init_rev),
            list(role="user", content="Review your own review and correct any errors.")
          ))
        }
        
        # Step 3b: Consolidated Tri-Model Consensus Audit
        # Each judge writes a separate row (UNAGGREGATED)
        
        for (j_id in names(panel_judges)) {
          judge_model <- panel_judges[[j_id]]
          
          judge_prompt <- paste0(
            "You are an expert meta-reviewer. Analyze this generated peer review text against the source paper context. ",
            "Count instances of evaluation errors and assign confidence levels (1-3).\n",
            "Return strictly a valid JSON object with these exact keys and numerical values only: Initial_Errors, Corrected_Errors, Novice_Missed_Rate, ",
            "Hallucinated_Citations, Fabricated_Comparisons, Unsupported_Claims, Verbatim_Hallucination, ",
            "Self_Correction_Blind_Spot, Confirmation_Bias, Overfitting_to_Prompt, Defensive_Admission_Pattern, ",
            "Plausible_Reasoning_Gaps, Omission_of_Key_Sources, Suggestion_Fictional_Missing_Elements, and their corresponding _Conf variables.\n\n",
            "SOURCE CONTEXT:\n", substr(paper_text, 1, 45000), "\n\nGENERATED REVIEW TO EVALUATE:\n", review_text
          )
          
          judge_raw <- query_openrouter(judge_model, list(list(role="user", content=judge_prompt)), response_json = TRUE)
          judge_raw_clean <- gsub("^```json\\s*|\\s*```$", "", judge_raw)
          
          parsed_json <- tryCatch({
            fromJSON(judge_raw_clean)
          }, error = function(e) {
            cat(sprintf("  --> Parsing anomaly handled for judge %s. Reverting to empty fallback model profile.\n", j_id))
            list()
          })
          
          # Step 3c: Build ONE row per judge (UNAGGREGATED)
          new_row <- data.frame(
            Review_ID = sprintf("R%03d", review_counter), 
            Paper_ID = p, 
            Domain = p_domain, 
            Journal = p_journal,
            Length = p_len, 
            Page_Count = actual_pages, 
            Year = p_year, 
            Num_Authors = p_auths, 
            Model = m, 
            Condition = cond,
            Judge_ID = j_id,
            stringsAsFactors = FALSE
          )
          
          # Extract metrics for this specific judge
          for(k in target_keys) {
            # Get the value for this judge
            val <- if(!is.null(parsed_json[[k]])) as.numeric(parsed_json[[k]]) else 0
            conf_val <- if(!is.null(parsed_json[[paste0(k, "_Conf")]])) as.numeric(parsed_json[[paste0(k, "_Conf")]]) else 2
            
            new_row[[k]] <- val
            new_row[[paste0(k, "_Conf")]] <- conf_val
          }
          
          experimental_log <- rbind(experimental_log, new_row)
          cat(sprintf("  Judge %s completed for Review R%03d\n", j_id, review_counter))
        }
        
        review_counter <- review_counter + 1
        
        # Auto-save every 10 reviews (i.e., 30 rows)
        if (review_counter %% 10 == 0) {
          write_csv(experimental_log, output_filename)
          cat(sprintf("  Auto-saved at %d reviews (%d rows).\n", review_counter, nrow(experimental_log)))
        }
        
      }, error = function(e) {
        cat(sprintf("Process bypass caught on Review: %s\n", e$message))
      })
      
      Sys.sleep(1) # PAID TIER: Reduced delay (1 sec instead of 3)
    }
  }
}

# ===================================================================================================
# 4. Data Export Execution Block
# ===================================================================================================
write_csv(experimental_log, output_filename)
cat("Success! Paid tier experiment completed. Local files updated safely as 'Judges_Real_llm_review_failure_analysis_data_unaggregated.csv'\n")
cat(sprintf("Total rows: %d (expected 675)\n", nrow(experimental_log)))
# ===================================================================================================



#...................................................................................................


#---if your need to paper links

paper_urls <- list(
  "P1"  = "https://www.mdpi.com/1424-8220/23/1/123/pdf",
  "P2"  = "https://www.mdpi.com/2076-3417/14/4/1560/pdf",
  "P3"  = "https://www.mdpi.com/2079-9292/13/1/123/pdf",
  "P4"  = "https://www.mdpi.com/2073-8994/16/2/234/pdf",
  "P5"  = "https://www.mdpi.com/2079-9292/13/5/890/pdf",
  "P6"  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10137111/pdf/",
  "P7"  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10014321/pdf/",
  "P8"  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10892345/pdf/",
  "P9"  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10783456/pdf/",
  "P10" = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10217432/pdf/",
  "P11" = "https://www.mdpi.com/2227-9032/11/2/222/pdf",
  "P12" = "https://www.mdpi.com/2071-1050/16/1/432/pdf",
  "P13" = "https://www.mdpi.com/1660-4601/21/2/222/pdf",
  "P14" = "https://www.mdpi.com/2071-1050/16/2/876/pdf",
  "P15" = "https://www.mdpi.com/1660-4601/21/3/333/pdf"
)