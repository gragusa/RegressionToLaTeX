# Helper function to get model type and family
get_model_type <- function(model) {
  if (inherits(model, "fixest")) {
    if (!is.null(model$family)) {
      family_name <- model$family$family
      if (family_name == "binomial") {
        link <- model$family$link
        return(list(type = "glm", family = link))
      }
    }
    # Check for IV
    if (!is.null(model$iv_first_stage)) {
      return(list(type = "iv", first_stage = model$iv_first_stage))
    }
    return(list(type = "linear"))
  } else if (inherits(model, "glm")) {
    family_name <- model$family$family
    if (family_name == "binomial") {
      link <- model$family$link
      return(list(type = "glm", family = link))
    }
    return(list(type = "linear"))
  }
  return(list(type = "linear"))
}

# Function to clean IV variable names
clean_iv_name <- function(name) {
  gsub("^fit_", "", name)
}



hasintercept <- function(model) {
  # For fixest models
  if (inherits(model, "fixest")) {
    # Get the formula
    fml <- model$fml

    # Check if there's a -1 or +0 or -0 in the RHS of the formula
    rhs <- deparse(fml[[3]])
    if (grepl("-1|\\+0|-0", rhs)) {
      return(FALSE)
    }
    return(TRUE)
  }

  # For lm and glm models
  if (inherits(model, c("lm", "glm"))) {
    # Extract the terms
    tt <- terms(model)

    # Check the intercept attribute
    return(attr(tt, "intercept") == 1)
  }

  # For unknown model types, return NULL
  return(NULL)
}

# Helper function for smart number formatting
format_number_smart <- function(x, sig_digits = 3, force_scientific = FALSE) {
  # Handle zero explicitly
  if (x == 0) return("0")

  # Format to significant digits
  formatted <- format(x, digits = sig_digits, scientific = force_scientific)

  # Check if the number rounded to sig_digits is effectively zero
  rounded_val <- as.numeric(formatted)
  if (rounded_val == 0) {
    # Use scientific notation for effectively zero numbers
    return(format(x, digits = sig_digits, scientific = TRUE))
  }

  # Remove trailing zeros after decimal point while preserving sig digits
  formatted <- sub("\\.$", "", formatted)  # Remove trailing decimal point
  formatted <- sub("0+$", "", formatted)   # Remove trailing zeros

  return(formatted)
}

# Helper function to extract R-squared based on model class
get_r_squared <- function(model) {
  if (inherits(model, "fixest")) {
    return(list(
      r2 = fixest::r2(model)["r2"],
      adj_r2 = fixest::r2(model)["ar2"]
    ))
  } else if (inherits(model, c("lm", "glm"))) {
    return(list(
      r2 = summary(model)$r.squared,
      adj_r2 = summary(model)$adj.r.squared
    ))
  }
  return(NULL)
}

# Helper function to extract fixed effects with safe handling

get_fixed_effects <- function(model) {
  if (!inherits(model, "fixest")) return(NULL)

  # Extract formula
  fml <- model$fml_all$fixef
  if (is.null(fml)) return(NULL)

  # Get the fixed effects part
  fe_part <- fml[[2]]
  if (is.null(fe_part)) return(NULL)

  # Initialize lists for both types
  fixed_effects <- c()
  varying_slopes <- c()

  # Helper function to process a single term
  process_term <- function(term) {
    if (is.call(term) && term[[1]] == "[[") {
      # This is a varying slope
      varying_var <- as.character(term[[2]])
      by_var <- as.character(term[[3]])
      varying_slopes <<- c(varying_slopes, paste0(varying_var, " (", by_var, ")"))
    } else {
      # This is a regular fixed effect
      fixed_effects <<- c(fixed_effects, as.character(term))
    }
  }

  # Process terms
  if (is.call(fe_part) && fe_part[[1]] == "+") {
    # Multiple terms
    for (i in 2:length(fe_part)) {
      process_term(fe_part[[i]])
    }
  } else {
    # Single term
    process_term(fe_part)
  }

  # Return both types of effects
  list(
    fixed = if(length(fixed_effects) > 0) fixed_effects else NULL,
    varying = if(length(varying_slopes) > 0) varying_slopes else NULL
  )
}


#' Convert Regression Results to LaTeX Equations
#'
#' @description
#' Converts regression results from various R model types (lm, glm, fixest) into LaTeX equations.
#' Supports different model specifications including linear models, probit/logit models, and
#' instrumental variables. Provides options for formatting coefficients, displaying R², handling
#' fixed effects, and managing instrumental variable specifications.
#'
#' @param model A model object of class \code{lm}, \code{glm}, or \code{fixest}
#' @param vcov. Optional variance-covariance matrix or function to compute standard errors
#' @param R2 Logical. Should R² be included in the output? Default is FALSE
#' @param adj_R2 Logical. Should adjusted R² be included in the output? Default is FALSE
#' @param sig_digits Integer. Number of significant digits to display for coefficients and standard errors. Default is 3
#' @param split Logical or numeric. Should the equation be split into multiple lines? If numeric, specifies the character limit per line. Default is FALSE
#' @param fe_notes Logical. Should fixed effects information be included in the output? Default is TRUE
#' @param show_first_stage Logical. For IV models, should first stage equations be displayed? Default is FALSE
#' @param omit Character vector of regular expressions identifying coefficients to omit from the output. Default is NULL
#'
#' @details
#' The function supports multiple model types and provides various formatting options:
#'
#' \strong{Model Types Supported:}
#' \itemize{
#'   \item Linear models (\code{lm})
#'   \item Generalized linear models (\code{glm})
#'   \item Fixed effects models (\code{fixest})
#'   \item IV regression models (\code{feols} with instruments)
#'   \item Probit/Logit models (\code{feglm} with binomial family)
#' }
#'
#' \strong{Special Cases:}
#' \itemize{
#'   \item Probit models: Outputs Φ(.) notation
#'   \item Logit models: Outputs Λ(.) notation
#'   \item IV models: Removes "fitted_" prefix from endogenous variables
#'   \item Fixed effects: Displays both standard fixed effects and varying slopes
#' }
#'
#' @return
#' A character string containing the LaTeX equation representation of the model results.
#' The output is wrapped in \code{$$} delimiters for math mode.
#'
#' @examples
#' # Linear regression
#' model_lm <- lm(mpg ~ wt + hp, mtcars)
#' regression_to_latex(model_lm, R2 = TRUE)
#'
#' \dontrun{
#' # Fixed effects model
#' library(fixest)
#' model_fe <- feols(y ~ x1 | id + firm, data = data)
#' regression_to_latex(model_fe, R2 = TRUE, fe_notes = TRUE)
#'
#' # Probit model
#' model_probit <- feglm(y ~ x1 + x2,
#'                       family = binomial(link = "probit"),
#'                       data = data)
#' regression_to_latex(model_probit)
#'
#' # IV regression with first stage
#' model_iv <- feols(y ~ x1 | x_endog ~ z, data = data)
#' regression_to_latex(model_iv, show_first_stage = TRUE)
#'
#' # Omitting certain variables
#' model_big <- lm(y ~ x1 + x2 + factor(group), data = data)
#' regression_to_latex(model_big, omit = "group")
#'
#' # Split long equation
#' regression_to_latex(model_big, split = 80)
#' }
#'
#' @seealso
#' \code{\link[fixest]{feols}} for fixed effects and IV models
#' \code{\link[fixest]{feglm}} for generalized linear models with fixed effects
#'
#' @importFrom stats coef vcov terms
#' @importFrom fixest fixef r2
#' @importFrom stats formula
#'
#' @export
regression_to_latex <- function(model,
                                vcov. = NULL,
                                R2 = FALSE,
                                adj_R2 = FALSE,
                                sig_digits = 3,
                                split = FALSE,
                                fe_notes = TRUE,
                                show_first_stage=FALSE,
                                omit=NULL) {

  # Get model type and properties
  model_info <- get_model_type(model)
  # Extract coefficients
  coefs <- coef(model)
  # Fix name of endogenous variable fitted with IV
  if (model_info$type == "iv") {
    names(coefs) <- sapply(names(coefs), clean_iv_name)
  }
  fixed_effects <- NULL
  omitted = FALSE
  if (!is.null(omit)) {
    # Convert omit to character vector if it's not already
    if (!is.character(omit)) omit <- as.character(omit)

    # Create the filter
    keep_idx <- !sapply(names(coefs), function(name) {
      any(sapply(omit, function(pattern) grepl(pattern, name)))
    })

    if (length(keep_idx) != length(coefs)) {
      omitted=FALSE
    } else {
      omitted=TRUE
    }

    # Filter coefficients
    coefs <- coefs[keep_idx]
  }
  ses <- if (is.null(vcov.)) {
    sqrt(diag(vcov(model)))
  } else if (is.function(vcov.)) {
    sqrt(diag(vcov.(model)))
  } else if (is.matrix(vcov.)) {
    if (nrow(vcov.) == length(coefs) && ncol(vcov.) == length(coefs)) {
      sqrt(diag(vcov.))
    } else {
      stop("Error: The dimensions of vcov. do not match the number of coefficients")
    }
  } else {
    stop("Error: Invalid vcov. specification")
  }

  # Get the dependent variable name
  if (inherits(model, "fixest")) {
    dep_var <- as.character(model$fml[[2]])
  } else {
    dep_var <- as.character(formula(model)[[2]])
  }

  # Get variable names and process them
  var_names <- names(coefs)
  process_var_name <- function(name) {
    # Handle interaction terms
    if (grepl(":", name)) {
      terms <- strsplit(name, ":")[[1]]
      return(paste(sapply(terms, process_var_name), collapse = "\\times "))
    }

    # Handle transformed variables
    if (grepl("^I\\(", name)) {
      inner <- gsub("^I\\(|\\)$", "", name)
      inner <- gsub("\\^", "^", inner)
      return(inner)
    }

    # Handle factor variables
    if (grepl("^factor\\(", name)) {
      return(gsub("^factor\\(|\\)$", "", name))
    }

    return(name)
  }
  var_names <- sapply(var_names, process_var_name)
  var_names[var_names=="(Intercept)"] <- ""
  # Format coefficients and standard errors with smart formatting
  coefs_formatted <- sapply(coefs, format_number_smart, sig_digits = sig_digits)
  ses_formatted <- sapply(ses, format_number_smart, sig_digits = sig_digits)

  # Create the LaTeX equation parts
  latex_parts <- c()
  for (i in seq_along(coefs)) {
    if (i == 1) {
      ## Handle first terms without sign
      term <- paste0("\\underset{(", ses_formatted[i], ")}{", coefs_formatted[i], "}")
    } else {
      sign <- ifelse(coefs[i] >= 0, "+", "-")
      abs_coef <- abs(as.numeric(coefs[i]))
      abs_coef_formatted <- format_number_smart(abs_coef, sig_digits = sig_digits)
      term <- paste0(sign, "\\underset{(", ses_formatted[i], ")}{",
                     abs_coef_formatted, "}", ifelse(var_names[i]!="", " \\times ", ""), var_names[i])
    }
    latex_parts <- c(latex_parts, term)
  }

  if(omitted) {
    latex_parts <- c(latex_parts, "+\\{\\dots\\}")
  }

  # Add R² information if requested
  r2_info <- get_r_squared(model)
  r2_text <- ""
  if (!is.null(r2_info)) {
    if (R2 && !is.null(r2_info$r2)) {
      r2_formatted <- format_number_smart(r2_info$r2, sig_digits = sig_digits)
      r2_text <- paste0("\\quad R^2=", r2_formatted)
    }
    if (adj_R2 && !is.null(r2_info$adj_r2)) {
      adj_r2_formatted <- format_number_smart(r2_info$adj_r2, sig_digits = sig_digits)
      r2_text <- paste0(r2_text, "\\quad \\text{Adj. }R^2=", adj_r2_formatted)
    }
  }

  # Add fixed effects information if applicable
  fe_text <- ""
  ffe_text <- ""
  if (fe_notes && inherits(model, "fixest")) {
    effects <- get_fixed_effects(model)
    if (!is.null(effects)) {
      fe_parts <- c()
      ffe_parts <- c()
      # Add fixed effects if present
      if (!is.null(effects$fixed)) {
        ffe_parts <- c(fe_parts,
                      #paste0("\\text{FE: }",
                             #paste(effects$fixed, collapse = ", "))#
                      paste0("[", fixed_effects, "]", collapse = " + ")
        )
      }

      # Add varying slopes if present
      if (!is.null(effects$varying)) {
        fe_parts <- c(fe_parts,
                      paste0("\\text{Varying slopes: }",
                             paste(effects$varying, collapse = ", ")))
      }

      # Combine all parts
      if (length(fe_parts)> 0) {
        fe_text <- paste0("\\quad ", paste(fe_parts, collapse = "\\quad "))
      }
      if (length(ffe_parts)>0) {
        ffe_text <- paste0("+", ffe_parts)
      }
    }
  }

  # Modify the output based on model type
  if (model_info$type == "glm") {
    if (model_info$family == "probit") {
      dep_var <- paste0("\\Pr(", dep_var, "=1) = \\Phi\\left(")
      equation_end <- "\\right)"
    } else if (model_info$family == "logit") {
      dep_var <- paste0("\\Pr(", dep_var, "=1) = \\Lambda\\left(")
      equation_end <- "\\right)"
    }
  } else {
    equation_end <- ""
  }

  # Combine all parts
  if (!split) {
    full_latex <- paste0("$$\n",
                         dep_var, ifelse(model_info$family=="linear","=",""),
                         paste(latex_parts, collapse = ""),
                         ffe_text,
                         equation_end,
                         r2_text,
                         fe_text,
                         "\n$$")
  } else {
    # Split the equation into multiple lines
    lines <- c(paste0(dep_var, "&=", latex_parts[1]))
    current_line <- lines[1]
    for (i in 2:length(latex_parts)) {
      if (nchar(current_line) + nchar(latex_parts[i]) > split) {
        lines <- c(lines, paste0("&\\phantom{=}", latex_parts[i]))
        current_line <- lines[length(lines)]
      } else {
        lines[length(lines)] <- paste0(lines[length(lines)], latex_parts[i])
        current_line <- lines[length(lines)]
      }
    }

    # Add R² and fixed effects on new lines if they exist
    if (nchar(r2_text) > 0 || nchar(fe_text) > 0) {
      lines <- c(lines, paste0("&", r2_text, fe_text))
    }

    aligned_equation <- paste(lines, collapse = " \\\\\n")
    full_latex <- paste0("$$\n\\begin{aligned}\n",
                         aligned_equation,
                         "\n\\end{aligned}\n$$")
  }

  # Add first stage if requested for IV models
  if (show_first_stage && model_info$type == "iv") {
    first_stages <- list()
    for (endog in names(model_info$first_stage)) {
      first_model <- model_info$first_stage[[endog]]
      # Recursive call for first stage, but prevent infinite recursion
      first_latex <- regression_to_latex(first_model,
                                         vcov. = vcov.,
                                         R2 = R2,
                                         adj_R2 = adj_R2,
                                         sig_digits = sig_digits,
                                         split = split,
                                         fe_notes = fe_notes,
                                         omit = omit,
                                         show_first_stage = FALSE)
      first_stages[[endog]] <- first_latex
    }

    # Combine main equation with first stages
    full_latex <- paste(c(
      "\\text{Second Stage:}",
      full_latex,
      "\\text{First Stage:}",
      unlist(first_stages)
    ), collapse = "\n\n")
  }
  return(full_latex)
}

recursive_formatting <- function(x, digits = options()$digits, scientific=FALSE) {
  xx <- round(x, digits=digits)
  XX <- format(xx, nsmall=digits)
  done = TRUE
  while (done) {
    ind <- (xx==0)
    if (sum(ind) != 0) {
      digits = digits + 1
      xx <- round(x, digits=digits)
      XX[ind] <- format(xx[ind], nsmall=digits, scientific=scientific)
    } else {
      done <- FALSE
    }
  }
  return(XX)
}
