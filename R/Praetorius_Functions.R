library(stringr)

metposer <- function(base, n) {
          n <- n * 4
          n <- n + 1
          
          ifelse(base == 3,
                    c(3, .25, .5, .25, 1, .25, .5, .25, 1.1, .25,.5,.25)[n],
                    c(4, .25, .5, .25, 1, .25, .5, .25, 2, .25, .5, .25, 1, .25, .5, .25)[n])
}
metposer <- Vectorize(metposer, vectorize.args = c('base', 'n'))

pnames = c('F', 'C', 'G', 'D', 'A', 'E', 'B')

pc2fifth = function(ps) {
          notNA = !is.na(ps)
          # 
          splits = strsplit(toupper(ps[notNA]), split = '')
          # 
          # 
          #           
          # splits %ll% (X := {
          #           
          # }) -> ns
          lapply(splits,
                 function(X) {
                           name = match(X[1], pnames) - 1
                           if (length(X) > 1) {
                                     acc = X[-1]
                                     acc = c(-7, 7, 14)[match(acc, c('-', '#', 'X'))]
                           } else {acc = 0}
                           name + sum(acc)
                           
                 }
          ) -> ns
          
          output = numeric(length(ps))
          output[notNA] = unlist(ns)
          output[!notNA] = NA_integer_
          output
}

### TOOLS

scorer <- function(expr, w = 3) {
# scorer <- function(dat, w = 3) {
          expr <- deparse(substitute(expr))
          dat <- subset(Data, eval(parse(text = expr)) )
          list2env(dat, envir = environment())
          
          off <- Offset
          off <- (off - min(off, na.rm = T)) + 1
          off <- off / min(diff(off), na.rm = T)
          empty <- rep(' ' %str*% w, max(off, na.rm = T))
          
          placer <- function(x) { `[<-`(empty, off, x)          }
          
          cat('\n', FileName[[1]], '\n')
          
          
          wind <- harmRhythm
          if (!is.null(wind)) { 
                    windowns <- windows
                    windowns[!harmRhythm] <- ''
                    cat(collapse(str_pad(placer(windowns), width = w, side = 'left')), '\n')
                    cat(collapse(str_pad(placer(c('', '|')[1 + as.numeric(wind)]), width = w, side = 'left')), '\n') 
                    
                    }
          
       
          
          lapply(rev(seq_len(max(lengths(dat$PCs)))),
                 function(i) {
                   pcs <- PCs %i% i %|% unlist 
                   
                   ison <- isOnset %i% i %|% unlist
                   dur <- Dur %i% i %|% unlist
                   
                   pcs[!ison] <- ''
                   
                   cat(collapse(str_pad(placer(pcs), width = w, side ='left')), '\n')
                 }
          ) -> x
          
          metpos <- MetPos
          metpos[metpos != 0] <- ''
          metpos[metpos == '0'] <- '.'
          cat(collapse(str_pad(placer(metpos), width = w, side = 'left')), '\n')
          cat('\n')
          

          
          records <- Record
          records[MetPos != 0] <- ''
          records[seq_along(records) > 99 & seq_along(records) %% 2 == 1] <- ''
          cat(collapse(str_pad(placer(records), width = w, side = 'left')), '\n')
          
          if(exists('Chord')){
          qual <- Chord
          qual[!harmRhythm & qual == rotate(qual)] <- ''
          qual[qual == 'NANA' | is.na(qual)] <- '?'
          qual <- gsub('\\(', '', gsub('\\)', '', qual))
          qual <- translatesymbol(qual)
          cat(collapse(str_pad(placer(qual), width = w, side = 'left')), '\n')
          }
          # cat(collapse(str_pad(placer(round(ModelRank, 1)), width = w, side = 'left')), '\n')
          invisible(x)
}


translatesymbol <- function(sym) {
 sym <- gsub('Mm', '7', sym)
 sym <- gsub('MM', '^7', sym)
 sym <- gsub('mn', 'm7', sym)
 sym <- gsub('dd', 'o7', sym)
 sym <- gsub('dm', 'mo7', sym)
 sym <- gsub('d', 'o', sym)
 
 sym[sym %~% '[A-G[#-]*M'] <- gsub('M', '', sym[sym %~% '[A-G[#-]*M'])   
 sym[sym %~% '[^M]m|d'] <- tolower(sym[sym %~% '[^M]m|d'])
 sym[sym %~% '[A-G[#-]*m'] <- gsub('m', '',sym[sym %~% '[A-G[#-]*m'])
 
 sym
}

cumsumx <- function(n) {
          out <- n
          out[is.na(n)] <- 0
          
          out <- cumsum(out)
          
          out <- c(0, head(out, -1))
          out[is.na(n)] <- NA
          out
}
          
# 
# ngram <- function(n, front = TRUE) {
#           function(func, ...) {
#                     function(.sd) {
#                               inds <- lapply(seq_len(nrow(.sd) - 1), function(x) x:(x + 1))
#                               
#                               lapply(inds,
#                                      function(i) {
#                                                cur <- .sd[i, ]
#                                                func(cur, ...)
#                                      }) -> returns
#                               
#                               if (front)  c(returns, NA) else c(NA, returns)
#                               
#                     }
#           }
# }

eachpart <- function(func, d = 1) {
          function(ls) {
                    cur <- do.call('rbind', ls)
                    out <- apply(cur, 2, func)
                    unlist(apply(out, d, list), recursive = FALSE)
          }
}

`%any==%` <- function(x, pat) sapply(x, function(z) any(pat %in% z, na.rm = TRUE))



#####


chords = list(
  c(Root = 0, Third = 4, Fifth = 1),
  c(Root = 3, Third = 0, Fifth = 4),
  c(Root = 6, Third = 3, Fifth = 0),
  c(Root = 0, Third = 4, Fifth = 8),
  c(Root = 2, Third = 6, Fifth = 3, Seventh = 0),
  c(Root = 3, Third = 0, Fifth = 4, Seventh = 1),
  c(Root = 0, Third = 4, Fifth = 1, Seventh = 5),
  c(Root = 6, Third = 3, Fifth = 0, Seventh = 4),
  c(Root = 9, Third = 6, Fifth = 3, Seventh = 0),
  c(Root = 2, Third = 6, Seventh = 0),
  c(Root = 3, Third = 0, Seventh = 1),
  c(Root = 2, Fifth = 3, Seventh = 0),
  c(Root = 0, Third = 4, Seventh = 5),
  c(Root = 6, Fifth = 0, Seventh = 4),
  c(Root = 9, Fifth = 3, Seventh = 0),
  c(Root = 9, Third = 6, Seventh = 0),
  c(Root = 0, Fifth = 1, Seventh = 5),
  c(Root = 2, Fifth = 3, Seventh = 0),
  c(Root = 0, Fifth = 1),
  c(Third = 0, Root = 3),
  c(Root = 0, Third = 4)
  )
names(chords) = c('M', 'm', 'd', 'a',
                  'Mm', 'mm', 'MM',  'dm' ,'dd', 
                  '(Mm)', '(mm)','(?m)', '(MM)', '(dm)', '(dd)', '(dd)',
                  '(?M)', '(?m)',
                  '(P5)', 'm(?5)', 'M(?5)')

chordbadness <- c(0, 0, 0, 3,
                  .5,.5,2,1,.5,
                  2, 2, 2, 2, 2, 2, 2,
                  3, 3,
                  4, 4, 4)

basicChordParse <- function(pcs) {
          fifthline = pc2fifth(pcs)
          
          zeroed = fifthline - min(fifthline, na.rm = TRUE)
          
          rightlength <- lengths(chords) == length(unique(zeroed[!is.na(zeroed)]))
          nchords = chords[rightlength]
          
          if (len0(nchords)) return(    list(Root = NA, Quality = NA, Rank = 0))
          
          sapply(nchords, 
                 function(chord) {
                           length(chord) == length(intersect(chord, zeroed[!is.na(zeroed)])) 
                 }
          ) -> hits
          
          if (lennot0(hits) && any(hits)) {
                    matchingchord = nchords[hits]
                    cts <- names(matchingchord[[1]])[match(zeroed, matchingchord[[1]])]
                    root <- if (!is.null(cts)) pcs[which(cts == 'Root')][1] else NA
                    qual <- names(matchingchord)
                    # if (qual == '(P5)' && (!is.na(cts) & cts[1] == 'Fifth')) return(list(Root = NA, Quality = NA, Rank = 0)) 
                    
                    rank <- 5 - chordbadness[rightlength][hits]
                    if (cts[1] %in% c('Seventh',  'Fifth')) rank <- rank - 1
                    
                    
                    list(Root = root,
                         Quality = qual, 
                         Rank = rank) 
          } else { 
                    list(Root = NA, Quality = NA, Rank = 0)
                    }
}

#
chordequiv <- function(cs) {
          
          rs <- str_extract(cs, '^[A-Ga-g][#-]?')
          if(any(is.na(rs)) || length(cs) == 1) return(cs)
          if (rs[1] != rs[2])  return(cs)
          
          qs <- str_replace(cs, '^[A-Ga-g][#b]*', '')

          
          
          if (cs[1] %~% 'P5' && cs[2] %!~% 'P5') {
                    cs[1] <- cs[2] 
                    } 
          
          cs
          
}
###

fifth2int = function(int, ic = FALSE) {
  out <- int          
  notna <- !is.na(int)           
          
  if (ic) {
    out[notna] <- c('0', '5', '2', '3', '4', '1', '6')[1 + (abs(int[notna]) %% 7)]
    out <- factor(out, levels = c(0:6, 'Rest'))
  } else {
    out[notna] <- c('dd7', 'd4', 'd1','d5', 'm2', 'm6', 'm3', 'm7', 'P4', 'Uni', 'P5', 'M2', 'M6', 'M3', 'M7', 'A4', 'A1', 'A5', 'A2', 'A6', 'A3', 'A7' )[int[notna] + 10]
    out <- factor(out, levels = c('Uni', 'm2', 'M2', 'm3', 'M3', 'P4', 'A4', 'd5', 'P5', 'm6', 'M6', 'm7', 'M7', 'Rest'), exclude = NULL)
  }
  
  out
}

parseRootMotion = function(roots, ends) {
          
  asfifths <- diff(pc2fifth(roots))
  
  ints <- fifth2int(asfifths)
  ics  <- fifth2int(asfifths, ic = TRUE)
  
  ics[ends] <- factor('Rest', levels = levels(ics))
  ints[ends] <- factor('Rest', levels = levels(ints))

  data.frame(RMDeparture = ints, ICRMDeparture = ics)
  
}

VLplots <- function(rown, pc = TRUE) {
  old <- par(family = 'Times')
  on.exit(par(old))
  
  ante <- prae[rown]
  cons <- prae[rown + 1]
  
  pcns <- ante$Semits[[1]] 
  voicns <- (1:4)[!is.na(pcns)]
  pcns <- pcns[!is.na(pcns)]
  if (length(pcns) == 0) {
    axis(3, 1, "Rest", cex.axis = 2)
    return(invisible(NULL))
  }
  
  targets <- pcns + ante$VoiceLeading[[1]]
  if (pc) {
    pcns <- pcns %% 12
    targets <- targets %% 12
  }
  
  repp <- function(x) rep(x, length(pcns))
  
  yrange <- if (pc) c(0, 11) else c(min(0, min(c(pcns, targets), na.rm = TRUE)), max(11, max(c(pcns, targets), na.rm = TRUE)))
  
  plot(-1, type = 'n', xlim = c(0,3), ylim = yrange, axes = FALSE, xlab = 'Chords', ylab = 'Semitones')
  axis(2, seq(yrange[1], yrange[2]), 
       c('C', 'C#/D-', 'D', 'D#/E-', 'E', 'F', 'F#', 'G', 'G#/A-', 'A', 'A#/B-','B')[(seq(yrange[1], yrange[2]) %% 12) + 1],
       las = 1, tick = FALSE)  
  lapply(seq(yrange[1], yrange[2]), abline, col = 'grey80', lty = 'dashed', b = 0)
  
  
  
  sustains <- !ante$isOnset[[1]][!ante$isRest[[1]]]
  
  points(repp(1),
         pcns , pch = c(16, 1)[sustains + 1],
         cex = 3)
  
  text(repp(.8), pcns, ante$ChordTones[[1]])
  axis(3, at = 1, paste0(ante$Root, ante$Quality), xpd = TRUE, cex.axis = 2)
  
  
  voicns <- tapply(voicns, pcns, paste, collapse = ',')
  text(rep(1.05, length(voicns)),
       as.numeric(names(voicns)),
       voicns, cex = .9)
  
  
  text(repp(.95),
       pcns,
       ante$PCs[[1]][!is.na(ante$PCs)])
  
  if (all(is.na(targets))) {
    axis(3, at = 2, 'rest', cex.axis = 2)        
    return(invisible(NULL))
  }
  # paths
  
  arrows(x0 = repp(1.1), x1 = repp(1.9), angle = 10,
         y0 = pcns, y1 = targets)
  
  
  # consequent
  sustains <- !cons$isOnset[[1]][!cons$isRest[[1]]]
  points(rep(2, length(targets)),
         targets , pch = c(16, 1)[sustains + 1],
         cex = 3)
  par(family = 'Times')
  text(repp(2.2), targets, cons$ChordTones[[1]])
  axis(3, at = 2, paste0(cons$Root, cons$Quality), xpd = TRUE, cex.axis = 2)
  
  voicns <- (1:4)[!is.na(cons$Semits)]
  voicns <- tapply(voicns, targets, paste, collapse = ',')
  text(rep(2.05, length(voicns)),
       as.numeric(names(voicns)),
       voicns, cex = .9)
  
  text(repp(1.95),
       targets,
       cons$PCs[[1]][!is.na(cons$PCs)])
  
}

##########################

VLplot_byquality <- function(ante_expr, cons_expr) {
  cur <- prae
  cur$NextQuality <- c(cur$Quality[-1], NA)
  
  
  antehits <- if (!missing(ante_expr)) eval(substitute(ante_expr), cur) else !logical(nrow(cur))
  conshits <- if (!missing(cons_expr)) eval(substitute(cons_expr), cur) else !logical(nrow(cur))
  hits <- cur[antehits & c(conshits[-1], FALSE)]
  
  cat(nrow(hits),'matching chords\n')
  
  ##
  lapply(c('Root', 'Third', 'Fifth', 'Seventh'), 
         function(ct) {
           prop.table(table(factor(unlist(lapply(hits$ChordToneVL, '[[', ct)), levels = -14:14)))
         }
  ) -> tables
  
  plot(1, type = 'n', xlim = c(0, 3), ylim = c(-5, 12), axes = FALSE, xlab = '', ylab = '')
  text(rep(1, 4), c(0, 4, 7, 10), c('Root', 'Third', 'Fifth', 'Seventh'))
  
  
  lapply(1:4, 
         function(n) {
           tab <- tables[[n]] 
           if(length(tab) == 0) return(invisible(NULL))
           
           ystart <- c(0, 4, 7, 10)[n]
           tab <- tab[tab != 0]
           tab <- tab[tab > .05]
           
           arrows(x0 = rep(1.5, length(tab)), x1 = rep(2, length(tab)), lwd = (tab*10),
                  y0 = ystart, ystart + as.numeric(names(tab)))
         }
  )
  invisible(NULL)
}


###




###
#ANALYSES
###

chordVLanal = function(sd, directed = TRUE) {
  if(is.na(sd$Root) || sd$Root == '?') return(NULL)
  
  chordvl = sd$ChordToneVL[[1]]
  
  ctnames = c('Root', 'Third', 'Fifth', 'Seventh')
  chordvl = chordvl[ctnames]
  names(chordvl) = ctnames
  
  profile = paste(paste0(ctnames, ': ', str_pad(unlist(lapply(chordvl, 
                                                              function(ints) {
                                                                ints = if (directed) ints else ints %% 12
                                                                paste(str_pad(ints, 2, 'left'), collapse = ', ')
                                                              })), 6, 'right')), collapse = '  ')
  
  curqual = sd$Quality
  rootmo  = sd$RMDeparture
  nexqual = if(sd$..I <= nrow(sd)) dataset$Quality[sd$..I + 1] else  "?"
  
  data.table(From = curqual, By = rootmo, To = nexqual, VL = profile)
  
}

##melodic idioms
skipsize <- 3




##############################################
# parse <- function(pat, bach = FALSE) {
#   
#   files = dir('Data', pattern = paste0(pat,'.dat$'), full.names = TRUE)
#   dataset = lapply(files, read.delim, header = FALSE, check.names = FALSE, sep = '\t', stringsAsFactors = FALSE)
#   
#   
#   
#   
#   
#   dataset = Map(parseDat, dataset, files)
#   dataset = do.call('rbind', dataset)
#   
#   #### chord id
#   
#   
#   dataset = cbind(dataset, rbindlist(lapply(dataset$PC, parseChord)))
#   dataset[, N := match(FileName, unique(FileName))]
#   dataset$Root <- factor(dataset$Root, levels = c('?', 'C', 'C#', 'D-', 'D', 'D#', 'E-', 'E', 'F', 'F#', 'G', 'G#', 'A-', 'A', 'A#', 'B-','B'))
#   
#   
#   dataset = cbind(dataset, parseRootMotion(dataset$Root))
#   
#   dataset[ , NOnsets := sapply(isOnset, sum)]
#   
#   
#   
#   ############
#   ### voice leading
#   #############
#   
#   dataset$VoiceLeading = I(c( Map(head(dataset$Semits, -1), tail(dataset$Semits, -1), f = function(ante, cons) cons - ante), list(c(NA, NA, NA,NA))))
#   
#   Map(function(vl, cts) { 
#     if(is.list(cts)) browser()
#     if (is.null(cts) || sum(is.na(vl)) > 1) return(list())
#     
#     
#     tapply(vl, cts, list) 
#   },
#   dataset$VoiceLeading,
#   dataset$ChordTones) -> dataset$ChordToneVL
#   
#   
#   dataset[ , NMoves := c(NA, head(sapply(VoiceLeading, function(ints) sum(ints != 0)), -1))]
#   
#   
#   dataset[ , ..I := .I]
#   # VL = dataset[ , chordVLanal(.SD), by = 1:nrow(dataset)]
#   # VLd = dataset[ , chordVLanal(.SD, directed = FALSE), by = 1:nrow(dataset)]
#   
#   
#   dataset[ , MelClass := .(melclassifier(.SD)), by = N]
#   dataset$MelClass <- lapply(dataset$MelClass, unlist)
#   
#   
#   #### INFER TRUE CHORDS
#   ## P and N
#   dataset$Interp <- FALSE
#   for(i in which(dataset$NMoves == 1 & dataset$Quality %in% c('?', 'MM', 'Mm','mm') & dataset$NOnsets < 3 & dataset$MelClass %any==% c('P','N'))) {
#     curmc <- dataset$MelClass[[i]]
#     
#     if (sum(!is.na(curmc)) == 1) {
#       pcs <- rep('', 4)
#       pcs[is.na(curmc)] <- dataset$PCs[[i]][is.na(curmc)]
#       pcs[!is.na(curmc)] <- dataset$PCs[[i - 1]][!is.na(curmc)]
#       
#       newchord <- parseChord(pcs)
#       
#       dataset$Quality[[i]] <- newchord$Quality
#       dataset$Root[[i]] <- newchord$Root
#       dataset$Bass[[i]] <- newchord$Bass
#       
#       cts <- newchord$ChordTones[[1]]
#       cts[!is.na(curmc)] <- curmc[!is.na(curmc)]
#       
#       dataset$ChordTones[[i]] <- cts
#       dataset$Interp[[i]] <- TRUE
#     }
#   }
#   
#   
#   ## suspensions
#   for(i in which(dataset$Root == '?' & dataset$MelClass %any==% 'S')) {
#     curmc <- dataset$MelClass[[i]]
#     
#     if (sum(!is.na(curmc)) == 1) {
#       pcs <- rep('', 4)
#       pcs[is.na(curmc)] <- dataset$PCs[[i]][is.na(curmc)]
#       pcs[!is.na(curmc)] <- dataset$PCs[[i + 1]][!is.na(curmc)]
#       
#       newchord <- parseChord(pcs)
#       
#       dataset$Quality[[i]] <- newchord$Quality
#       dataset$Root[[i]] <- newchord$Root
#       dataset$Bass[[i]] <- newchord$Bass
#       
#       cts <- newchord$ChordTones[[1]]
#       cts[!is.na(curmc)] <- curmc[!is.na(curmc)]
#       
#       dataset$ChordTones[[i]] <- cts
#       dataset$Interp[[i]] <- TRUE
#     }
#   }
#   
#   ####
#   if (!bach) {
#     dataset$End <- FALSE
#     dataset$End[which(sapply(dataset$isRest, all)) - 1] = TRUE
# 
#     dataset$Start <- FALSE
#     dataset$Start[which(sapply(dataset$isRest, all)) + 1] = TRUE
#     dataset$Start[1] = TRUE
# 
#     dataset <- dataset[!sapply(isRest, all)]
# 
#   }
# 
# 
#   
#   return(dataset)
#   
# }#end of pat function


#####################
########################################################################################
####roots by piece
# 
# tapply(prae$Root, prae$FileName, table)
# tapply(prae$Root, prae$FileName, 
#        function(x) {
#          fifths <- getfifth(x)
#          fifths <- fifths - min(fifths, na.rm = TRUE)
#          table(fifths)
#        })

####pcs by piece
# 
# tapply(prae$PCs, prae$FileName,
#        function(x) {
#          # pcs <- factor(unlist(x), levels = c('r', 'C', 'C#', 'D-', 'D', 'D#', 'E-', 'E', 'F', 'F#', 'G', 'G#', 'A-', 'A', 'A#', 'B-','B'))
#          pcs <- factor(unlist(x), levels = c('r', 'D-', 'A-', 'E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#', 'D#', 'A#'))
#          round(prop.table(table(pcs)), 2)
#        }
# ) -> pcdists


# 
# tapply(prae$PCs, prae$FileName,
#        function(x) {
#          pcs <- unlist(x)
#          fifths <- getfifth(pcs)
#          fifths <- fifths - min(fifths, na.rm = TRUE)
#          round(prop.table(table(fifths)), 2)
#        }
# ) -> pcfifthdist

zerothdists <- function(n = unique(prae$N)) {
  dt <- prae[ N %in% n]
  
  old <- par(mfcol = c(2, 1), family = 'Times')
  on.exit(par(old))
  
  pcs <- unlist(dt$PCs)
  
  pcs <- factor(pcs[!is.na(pcs)], levels = c('D-', 'A-', 'E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#', 'D#', 'A#'))
  pctab <- table(pcs)
  
  maxpc <- max(pctab)
  
  x <- barplot(prop.table(pctab), border = NULL, col = 'grey60', axes = FALSE, xlab = 'Pitch Class', ylab = 'Proportion of Notes')
  axis(2, round( seq(0, round(maxpc / sum(pctab), 2), length.out = 8), 2),  las = 1, tick = FALSE)
  text(x, (pctab / sum(pctab)) + .01, pctab, cex = .7, xpd = TRUE)
  
  ## roots
  roots <- factor(dt$Root, levels = c('D-', 'A-', 'E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#', 'D#', 'A#'))
  qual <- factor(dt$Quality, levels = c('M', 'm', 'd', 'mm', 'MM', 'dm', 'Mm', 'a', '(P5)', '(M3)', '(M6)', '?', 'sus'))
  roottab <- table(qual, roots)
  maxroot <- max(roottab)
  
  cols <- rainbow(nrow(roottab), alpha = .8)[c(4,7,3,6,8,5,2,9,1,10,11)]
  cols <- c(cols, c('black', 'grey20'))
  
  x <- barplot(prop.table(roottab), beside = FALSE, axes = FALSE, xlab = 'Chord Roots', ylab = 'Proportion of Chords', col = cols)
  legend('topright', legend = rownames(roottab), fill = cols, cex = 1.3, border = FALSE, bty = 'n')
  axis(2,  round( seq(0, round(maxroot / sum(roottab), 2), length.out = 8), 2), las = 1, tick = FALSE)
  text(x, (colSums(roottab) / sum(roottab)) + .01, colSums(roottab), cex = .7, xpd = TRUE)
  list(PC = pctab, Pitch = roottab)
}

firstdists <- function(n = unique(prae$N)) {
  dt <- prae[N %in% n]  
  
  
  roots <- factor(dt$Root, levels = c('E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#'))
  roottab <- table(head(roots, -1), tail(roots, -1))
  
  svg('tmp.svg', width = ncol(roottab), height = 2*nrow(roottab))
  par(mfcol = c(nrow(roottab), 1), family = 'Times')
  
  for(i in 1:nrow(roottab)) {
    
    barplot(roottab[i,], axes = FALSE, border = FALSE, space = 0, ylim = c(0,max(roottab)), cex.names = 2)
    mtext(side = 2, text = rownames(roottab)[i], cex = 5, las = 1, line = -3)
  }
  
  dev.off()
  browseURL('tmp.svg')
  
}

firstdists <- function(n = unique(prae$N)) {
  dt <- prae[N %in% n]  
  
  
  old <- par(mfcol = c(1, 1), family = 'Times', mar = c(5,4,4,5))
  on.exit(par(old))
  
  roots <- factor(dt$Root, levels = c('E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#'))
  roottab <- table(head(roots, -1), tail(roots, -1))
  
  rownames(roottab) = colnames(roottab) = c('E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#')
  roottabp <- prop.table(roottab, 2)
  ars <- roottabp ^ 2
  ars <- apply(roottab, 2, function(x) x / max(x)) * 4
  ars <- ars + .2
  
  plot(-1, xlim = c(1, 13), ylim = c(1, 13), axes = FALSE, xlab = '', ylab = '')
  axis(1, 1:12, c('E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#'), las = 1, tick = F, line = F)
  axis(2, 1:12, c('E-', 'B-', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#', 'C#', 'G#'), las = 1, tick = F, line = F)
  
  abline(-1, 1, lty = 'dashed') ; text(14, 13, '+P5', xpd = T)
  abline(0, 1, lty = 'dashed') ; text(14, 14, 'Unison', xpd = T)
  abline(1, 1, lty = 'dashed') ; text(13, 14, '-P5', xpd = T)
  abline(2, 1, lty = 'dashed') ; text(12, 14, '-M2', xpd = T)
  
  invisible(outer(1:nrow(ars), 1:nrow(ars), function(i, j) {points(i, j, pch = 16, col = 'grey50', cex = ars[cbind(i, j)]) ; roottab}))
  
  roottab
  
}
