#' Calculate effect sizes
#'
#' Function to convert effect sizes. The following conversions are available:
#' from lnOR -> SMD, from SMD -> lnOR, from Zr -> SMD, and from SMD -> Zr. The
#' conversions involving Zr can also be calculated with 'r'. The variables
#' required for each conversion are listed in details and will be returned by
#' running convert_effect_sizes()
#'
#' @import data.table
#' @import crayon
#' @importFrom tools R_user_dir
#' @param ... variables required by effect size calculation (e.g., x, x1, x2,
#'   sd1, sd2, see Description)
#' @param from Current effect size. Character of length 1.
#' @param to New effect size. Character of length 1.
#' @param data An optional data frame containing the columns
#' @param bind Whether to bind outputs to 'data'.
#' @return A data.table with effect sizes and sample variances (if bind = TRUE). Transformed effect sizes are labeled 'yi_trans' and 'vi_trans'.
#' @export
convert_effect_sizes <- function(...,
                                 from = NULL,
                                 to = NULL,
                                 data = NULL,
                                 bind = FALSE
                                 ){
  require("crayon")
  require("data.table")
  env <- parent.frame()
  
  if(!is.null(data)) setDT(data)
  
  # Prepare formulas:
  formulas <- fread("/Users/ejlundgren/Dropbox/Projects/Meta_Methods_UAlberta/package_development/metatools_dev/data/conversion_formulas.csv")
  
  if(is.null(from) | is.null(to)){
    
    cat(blue(("\nEffect type name must be specified with 'effect_type' argument 
    and provide necessary variables (as named arguments, e.g., n1 = group1_n or a numeric vector).\n")), 
        blue("\nReturning effect size names & required variables for reference.\n\n"))
    return(unique(formulas[, .(from_effect, to_effect, vars_required)]))
  }
  
  if(length(from) > 1 | length(to) > 1){
    stop("Vectors `from` and `to` must be length 1.")
  }
  
  sub_formulas <- formulas[from_effect == from &
                              to_effect == to, ]
  cat(blue(unique(sub_formulas$message)), "\n\n")
  
  if(nrow(sub_formulas) == 0 | nrow(sub_formulas) > 2){
    cat("Conversion options mispecified. Returning conversion table for your reference")
    return(formulas)
  }
  
  # Get the variables required
  vars <- strsplit(unique(sub_formulas$vars_required), split = ", ") |> 
    unlist()
  
  # Get variables with Non Standard Evaluation
  call_expr <- match.call(expand.dots = TRUE)
  args <- as.list(call_expr)[-1]
  if("" %in% names(args)) stop("Numeric arguments must be named")
  input <- list()
  args <- args[vars[vars %in% names(args)]]
  
  #
  # print(args)
  if(!is.null(data)){
    setDT(data)
    for(i in 1:length(args)){
      input[[i]] <- data[, eval(args[[i]], envir = env)]
    }
    
  }else if(is.vector(eval(args[[1]], envir = env))){
    for(i in 1:length(args)){
      input[[i]] <- eval(args[[i]], envir = env)
    }
  }
  names(input) <- names(args)
  
  if(!all(vars %in% names(input))){
    cat(red("Missing the following variables:"), paste(setdiff(vars, names(input)), collapse = ", "))
    return(unique(sub_formulas[, .(from_effect, to_effect, vars_required)]))
  }
  
  # Evaluate formulas:
  eval(parse(text = sub_formulas$exec_formula))

  # This gathers them into a data.table:
  res <- eval(parse(text = paste0("data.table(", paste(unique(sub_formulas$label), collapse = ", "), ")")))
  
  if(bind == TRUE & !is.null(data)){
    res <- cbind(data, res)
  }
  return(res)
  
}


testing <- F
if(testing){
  formulas <- fread("/Users/ejlundgren/Dropbox/Projects/Meta_Methods_UAlberta/package_development/metatools_dev/data/conversion_formulas.csv")
  
  formulas[from_effect == "Zr" & to_effect == "SMD", ]
  
  
  sub_formulas <- formulas[from_effect == "r" &
                             to_effect == "SMD", ]
  sub_formulas <- formulas[from_effect == "Zr" &
                             to_effect == "SMD", ]
  from <- "Zr"
  to <- "SMD"
  yi <- 3.5
  vi <- .1
  n <- 10
  # n2 <- 10
  
  # J <- ifelse((input$n1 + input$n2 - 2) <= 1, NA_real_, exp(lgamma((input$n1 + input$n2 - 2)/2) - log(sqrt((input$n1 + input$n2 - 2)/2)) - lgamma(((input$n1 + input$n2 - 2) - 1)/2))); d <- input$yi / J; a <- (input$n1 + input$n2)^2 / (input$n1 * input$n2); r <- d/sqrt(d^2 + a); yi_trans <- atanh(r)
  dat <- data.table(n = c(9, 4), 
                    yi = c(-1.331789, -1.659096), 
                    vi = c(12, 4))
  
  convert_effect_sizes(n = n, yi = yi, vi = vi,
                       from = "Zr", to = "SMD",
                       bind = TRUE,
                       data = dat)
  input <- list("yi" = dat$yi, "n" = dat$n)
  from <- "Zr"
  to <- "SMD"
  # input_vars <- list("yi" = yi, "n1" = n1, "n2" = n2)
  # input_vars
  # 
  #' I think this formula is wrong.
  # But function seems to work
  dat <- data.frame(yi, vi, n1, n2)
  convert_effect_sizes(yi = yi, vi = vi, n1 = n1, n2 = n2,
                       data = dat,
                       bind = TRUE,
                       from = "SMD", to = "Zr")
  
  convert_effect_sizes(yi = yi, vi = vi, n1 = n1, n2 = n2,
                       data = dat,
                       bind = TRUE,
                       from = "SMD", to = "lnOR")  
  
  convert_effect_sizes()
  convert_effect_sizes(r = 0.1, n = 100,
                       bind = FALSE,
                       from = "r", to = "SMD")  
  from = "r"; to = "SMD"
  input <- list(r = 0.1, n = 100)
  
  
}


# convert_effect_sizes()


