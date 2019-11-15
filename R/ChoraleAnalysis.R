#' Produce harmonic analyses of chorales
#'
#' This is the master API for the package. A user may specify
#' one or more critera for either filtering or ranking analyses. These criteria
#' will be used to produced specific analyses and output them to new humdrum files.
#'
#' @param filters A list of lists. The first element in each list will be used as the
#' \code{rank} argument to \code{\link{filterAnalyses}}. The second argument
#' in each list will be used as the \code{filter} argument to \code{\link{filterAnalyses}}.
#' @param output.dir A string indicating the path where humdrum files should be output.
#' @param output.label A string (defaulting to "") which is appended to the filename of each
#' outputted file. This is useful for distinguihing analyzes output with separate calls
#' to \code{choraleAnalyze}.
#'
#' @return A humdrum analysis table, as output be \code{\link{generateHumdrum}} (returned invisibly).
#' @export
choraleAnalyze <- function(filters, output.dir = './', output.label = '') {
 rank <- filter <- list()
 analyses <- lapply(filters,
                    function(filt) {
                      if (length(filt) > 0) rank   <- filt[[1]]
                      if (length(filt) > 1) filter <- filt[[2]]
                      filtered <- filterAnalyses(rank, filter)

                      getChordSymbols(filtered)

                    })
 humdrumTables <- generateHumdrum(analyses)
 humdrumTables <- writeHumdrum(humdrumTables, output.dir, output.label)

 invisible(return(humdrumTables))
}

#' Filter permutational analyses to select specific analyses
#'
#' This function selects one analysis for each window in the
#' \code{\link{PermutationalChoraleAnnotations}} object. It also references
#' \code{\link{ChoraleTable_Notes}} and \code{\link{ChoraleTable_Slices}} objects.
#'
#' @param rank A list of formulae
#' @param rank A list of formulae
#'
#' @export
filterAnalyses <- function(rank = list(), filter = list()) {
  rank   <- parseFormulae(rank)
  filter <- parseFormulae(filter)

  rank <- lapply(rank, formEval)
  rank <- as.data.table(rank)

  perms <- PermutationalChoraleAnnotations
  perms$RN <- seq_len(nrow(perms))

  #
  if (length(filter) > 0) {
    filter <- do.call('cbind', lapply(filter, formEval))
    filter <- rowSums(filter) == ncol(filter)
    perms$Filter <- filter
    perms <- perms[ ,
                    if (any(Filter)) {
                      .SD[Filter]
                      } else {
                        out <- .SD[1]
                        out$Root[[1]] <- list(rep('X', length(Root[[1]])))
                        out$Quality[[1]] <- list(rep('?', length(Quality[[1]])))
                        out
                        }
                    ,  by = Slice_WindowNumber
                    , .SDcols = c('Root', 'Quality', 'Chord_NumberOf_Slices',
                                  'Slice_WindowNumber', 'Filter', 'RN')]
   rank <- rank[perms$RN]
  }

  if (length(rank) > 0) {
    perms <- perms[do.call('order', rank)]

    perms <- perms[order(perms$Slice_WindowNumber)]


  }

  perms[ , .SD[1], by = Slice_WindowNumber]




}








#' Parse formulae used by \code{\link{filterAnalyses}}
#'
#' This function takes a list of formulae and filters and groups them
#' into three categories: "Windows", "Notes", "Slices." Each formula's
#' category is determined by the left-side of the formula. If the left side
#' is empty, the default is "Windows." Left-sides that don't \code{\link{base::pmatch}}
#'  with one of the three categories are removed.
#'
#'  @param forms A list of formulae.
#'
#'  @return A list (of length three) of lists of expressions.
parseFormulae <- function(forms) {
 lapply(forms,
        function(form) {
          lh <- rlang::f_lhs(form)
          rlang::f_env(form) <- if (is.null(lh))  {
            ChoraleAnalyses
          } else {
             eval(parse(text = paste('ChoraleAnalyses$', deparse(lh), sep = ''))[[1]])
          }
          rlang::f_lhs(form) <- NULL
          form
        })
}

formEval <- function(form) {
 eval(rlang::f_rhs(form), envir = rlang::f_env(form))
}




#' Rank preferences
#'
#' @param x A list of vectors of rankable values
#' @param ... Any number of numeric values, each named. Namesless
#' values are ignored. Each name should correspond to a value in x.
#' @param other Any values in x which do not appear as a name in ...
#' are replaced with other.
prefer <- function(x, ..., other = max(unlist(list(...))) + 1, combine = max) {
  if (!is.list(x)) x <- as.list(x)

  args <- unlist(list(...), use.names = TRUE)

  if (is.null(names(args))) return(setNames(rep(other, length(x)), x))
  args <- args[args != '']

  ranks <- lapply(x,
                  function(vals) {
                    mat <- lapply(names(args),
                                  function(re) {
                                    grepl(re, vals)
                                  })
                    mat <- do.call('cbind', mat)
                    apply(mat, 1,
                          function(r) {
                            if (!any(r)) other else  args[which(r)[1]]
                            })

                    })

  sapply(ranks, combine)

}


#' Extracts vector of chord symols from annotation table
#'
#' @export
getChordSymbols <- function(DTpermutations) {
  DTpermutations[ , rep(paste(Root[[1]], Quality[[1]], sep = ''),
                        Chord_NumberOf_Slices[[1]]),
                  by = .(1:nrow(DTpermutations))]$V1
}

a = list(Chord~sapply(Durations, min)<0.5, Chord~sapply(SeventhsResolve, function(bool) any(bool %in% c(FALSE))), NCTs~Count, Chord~Count, Chord~sapply(CompletionDelay$Durations, mean))
b = list()
c = list (a, b)
choraleAnalyze(list(list(a)))

