mean_sd2 <- function(x) mean_sd(x, denote_sd = "paren", show_n = "never", na_rm = TRUE)

tab_summary2 <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
    UseMethod("tab_summary2")
}

tab_summary2.numeric <- function(x, n_perc_args = list(digits = 0, show_symbol = TRUE), envir = parent.frame()) {
    v <- deparse(substitute(x))
    
    if (length(n_perc_args)) { 
        n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
    } else {
        n_args <- ""
    }
    
    if (any(is.na(x))) {
        s <- list("Min"          = paste("~ min(", v, ", na.rm = TRUE)"),
                  "Mean (SD)"    = paste("~ qwraps2::mean_sd(", v, ", na_rm = TRUE, denote_sd = 'paren', show_n = 'never')"),
                  "Max"          = paste("~ max(", v, ", na.rm = TRUE)"))
        
        s <- c(s, Missing = paste(" ~ qwraps2::n_perc(is.na(", v, "),", n_args, ")"))
        
    } else {
        s <- list("Min"          = paste("~ min(", v, ")"),
                  "Mean (SD)"    = paste("~ qwraps2::mean_sd(", v, ", denote_sd = 'paren')"),
                  "Max"          = paste("~ max(", v, ")"))
    } 
    lapply(s, stats::as.formula, env = envir)
}

tab_summary2.character <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
    v <- deparse(substitute(x)) 
    
    if (length(n_perc_args)) { 
        n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
    } else {
        n_args <- ""
    }
    
    if (any(is.na(x))) {
        x <- stats::na.omit(x)
        s <- lapply(sort(unique(x)), 
                    function(xx) {
                        paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ", na_rm = TRUE)")
                    })
        s <- c(s, paste(" ~ qwraps2::n_perc(is.na(", v, ")", n_args, ")"))
        s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
    } else {
        s <- lapply(sort(unique(x)), 
                    function(xx) {
                        paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ")")
                    })
        s <- stats::setNames(s, sort(unique(x)))
    } 
    lapply(s, stats::as.formula, env = envir)
}

tab_summary2.factor <- function(x, n_perc_args = list(digits = 0, show_symbol = TRUE), envir = parent.frame()) {
    v <- deparse(substitute(x))
    
    if (length(n_perc_args)) { 
        n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
    } else {
        n_args <- ""
    }
    
    if (any(is.na(x))) {
        s <- lapply(levels(x),
                    function(xx) {
                        paste0("~ qwraps2::n_perc0(", v, " == '", xx, "'", n_args, ", na_rm = TRUE)")
                    })
        s <- c(s, paste(" ~ qwraps2::n_perc(is.na(", v, ")", n_args, ")"))
        s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
    } else {
        s <- lapply(levels(x),
                    function(xx) {
                        paste0("~ qwraps2::n_perc0(", v, " == '", xx, "'", n_args, ")")
                    })
        s <- stats::setNames(s, sort(unique(x)))
    } 
    lapply(s, stats::as.formula, env = envir)
}

summary_table.data.frame <- function(.data, summaries) {
    out <- lapply(summaries, 
                  function(dots, .df) {
                      t(dplyr::summarize_(.df, .dots = dots))
                  },
                  .df = .data)
    out <- do.call(rbind, out)
    colnames(out) <- paste0(deparse(substitute(.data)), " (N = ", nrow(.data) %>% format(big.mark = ","), ")")
    attr(out, "rgroups") <- sapply(summaries, length)
    class(out) <- c("qwraps2_summary_table", class(out))
    out
}

summary_table.grouped_df <- function(.data, summaries) {
    ngrps <- length(attr(.data, "vars"))
    
    lbs <- attr(.data, "labels")
    grpsz <- attr(.data, "group_sizes") %>% format(big.mark = ",")
    
    lbs <- apply(cbind(matrix(paste(rep(names(lbs), each = nrow(lbs)), as.matrix(lbs), sep= ": "), nrow = nrow(lbs)), paste0("(N = ", grpsz, ")")), 1, paste, collapse = " ")
    
    out <- lapply(summaries, 
                  function(dots, .df) {
                      t(dplyr::summarize_(.df, .dots = dots))[-seq(1, ngrps, by = 1), ]
                  },
                  .df = .data)
    out <- do.call(rbind, out)
    colnames(out) <- lbs
    attr(out, "rgroups") <- sapply(summaries, length)
    class(out) <- c("qwraps2_summary_table", class(out))
    out
}

render_table <- function(tab_summ, ...) {
    col_names <- colnames(tab_summ) %>% 
        str_replace_all(c("data2" = "Overall ", "award_dummy: " = ""))
    qable(tab_summ, 
          rgroup = attr(tab_summ, "rgroups"), 
          rnames = rownames(tab_summ), 
          cnames = col_names,
          markup = "markdown",
          booktabs = TRUE,
          digits = 2,
          format.args = list(big.mark = ","),
          ...)
}

qable <- function(x, rtitle, rgroup, rnames = rownames(x), cnames = colnames(x), markup = getOption("qwraps2_markup", "latex"), ...) { 
    
    if (!(markup %in% c("latex", "markdown"))) {
        stop("markup is either 'latex' or 'markdown'")
    }
    
    if (missing(rgroup)) { rgroup <- numeric(0) }
    
    xmat <- matrix("~", nrow = nrow(x) + length(rgroup), ncol = 1 + ncol(x))
    
    
    if (length(rgroup) > 0) { 
        rg_idx <- cumsum(c(1, 1 + rgroup[-length(rgroup)])) 
        
        if (markup == "latex") { 
            xmat[rg_idx, 1] <- paste0("\\bf{", names(rgroup), "}")
            xmat[-rg_idx, 1] <- paste("~~", rnames)
        } else {
            xmat[rg_idx, 1] <- paste0("**", names(rgroup), "**")
            xmat[-rg_idx, 1] <- paste("&nbsp;&nbsp;", rnames)
        } 
        xmat[-rg_idx, -1] <- as.matrix(x)
    } else {
        xmat[, 1] <- rnames 
        xmat[, -1] <- as.matrix(x)
    }
    
    if (markup == "markdown") {
        xmat <- apply(xmat, 1:2, function(x) gsub("~", "&nbsp;&nbsp;", x))
    }
    
    if (missing(rtitle)) {
        cnames <- c("", cnames)
    } else {
        cnames <- c(rtitle, cnames)
    }
    
    knitr::kable(xmat,
                 format = "pandoc",
                 escape = FALSE, 
                 row.names =  FALSE, 
                 col.names = cnames,
                 ...)
}

