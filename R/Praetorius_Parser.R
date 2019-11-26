setwd("C:/Users/juyao/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/rootfs/home/juyaolong/Flexible_harmonic_chorale_annotations/")
#setwd("inst/extdata")
library(stringi)
source('R/Praetorius_Functions.R')
library(composeR)
library(data.table)

#if (FALSE) {
          
tsvs <- read.tsvs(path = 'data', '\\.dat$', quote = "")
Map(tsvs, names(tsvs), seq_along(tsvs),
    f = function(., .name, .ind) {
          parts <- unique(grep('I".*', value = TRUE, .[grep('I"', .[,1]),]))
          parts = lapply(parts,
                         function(part) {
                                   subframe <- .[ , .[grep('I"', .[ , 1]), ] == part]
                                   colnames(subframe) = str_sub(subframe[1, ], 3L)
                                   subframe = subframe[!grepl('^[=!*]', subframe[, 1]), ]
                                   frame = as.data.frame(subframe, stringsAsFactors = FALSE)
                                   frame$semits <- gsub('\\(r\\)', 'r', frame$semits)
                                   
                                   frame$Rest = frame$semits == 'r'
                                   frame$Onset = !grepl('\\(.*\\)', frame$semits)
                                   frame$Onset[frame$Rest] = FALSE
                                   
                                   
                                   frame$semits[frame$Rest] = NA
                                   frame$semits = str_extract(frame$semits, '-*[0-9][0-9]*')
                                   frame$semits = as.numeric(frame$semits)
                                   frame$pc = gsub('b)*$', '-', frame$pc)
                                   frame$pc = str_extract(frame$pc, '[A-G][#-]*')
                                   frame$pc = factor(frame$pc, levels = c('r', 'C-', 'C', 'C#', 'D-', 'D', 'D#', 'E-', 'E', 'E#', 'F-', 'F', 'F#', 'F##', 'G-', 'G', 'G#', 'A-', 'A', 'A#', 'B-', 'B', 'B#'))
                                   
                                   ## departing
                                   mintsr <- rep(NA, nrow(frame))
                                   mintsr[frame$Onset | frame$Rest] <- c(diff(frame$semits[frame$Onset | frame$Rest]), NA)
                                   # mintsi <-  c(diff(frame$semits), NA)
                                   mintsi <- fill_forward(NA, mintsr)
                                   mintsi[frame$Rest] <- NA
                                   
                                   frame$VLD <- complex(real = mintsr, imaginary = mintsi)
                                   
                                   ## approaching
                                   mintsr <- rep(NA, nrow(frame))
                                   mintsr[frame$Onset | frame$Rest] <- c(NA, diff(frame$semits[frame$Onset | frame$Rest]))
                                   # mintsi <-  c(NA, diff(frame$semits))
                                   mintsi <- fill_forward(NA, mintsr)
                                   mintsi[frame$Rest] <- NA
                                   
                                   frame$VLA <- complex(real = mintsr, imaginary = mintsi)
                                   #
                                   recip = frame$recip
                                   recip[grepl('\\(.*\\)', recip)] <- NA
                                   dots = str_count(recip, '\\.')
                                   recip = str_extract(recip, '[0-9][0-9]*')
                                   recip[recip == '00'] = '.25'
                                   recip[recip == '0'] = '.5'
                                   recip = 1 / as.numeric(recip)
                                   recip = recip * (2 - (.5 ^ dots))
                                   
                                   ##Special parsing for particular files
                                   #Bach Chorale 194 is written with half-note beats for some reason
                                   # if (.name == 'Data/chor194.dat') recip <- recip / 2
                                   
                                   #Praetorus Chorale 003 written with whole-note beats
                                   # if (.name == 'Data/Praetorius003.dat') recip <- recip / 2
                                   
                                   # recip <- recip * ifelse(.name %~% 'Pr', 2, 4)
                                   
                                   frame$recip = recip
                                   
                                   frame[ , colnames(frame) != 'meter']
                         }
          )
           
          for (i in c('semits', 'pc', 'recip', 'Rest', 'Onset', 'VLD', 'VLA')) {
                    assign(i, unlist(apply(sapply(parts, '[[', i) , 1, function(row) list(rev(row))), recursive = FALSE))
          }
           
          measures <- .[!grepl('^[=!*]', .[ , 1]) , .[1,] == '**measure']
          measures[measures == '.'] <- '0'
          measures <- as.numeric(measures)
          meter <- .[!grepl('^[=!*]', .[ , 1]) , .[1,] == '**meter']
          #
          offsets  <- unlist(apply(apply(do.call('rbind', recip), 2, cumsumx), 1, list), recursive = FALSE)
          offsets  <- sapply(offsets, function(x) unique(x[!is.na(x)]))
          if (.name %~% 'chor') {
           offsets <- offsets - offsets[Position(EQ(1), measures)]      
          }
          ##
          Rest %l|v% all -> allrests
          end <- logical(length(semits))
          last(end) <- TRUE
          end[which(allrests) - 1] <- TRUE
          if (grepl('chor', .name)) {
                    #### FERMATA
                    fermata <- .[!grepl('^[=!*]', .[ , 1]) , ncol(.)]
                    fermata[is.na(fermata)] <- 0
                    end[c(0, diff(floor(cumsum(fermata) / 4))) == 1] <- TRUE
          } else {
                    fermata <- rep(0, length(semits))
                    end[which(diff(measures) != 0) + 1] <- TRUE
                    }
          
          ###meter stuff
          numerator <- as.numeric(str_sub(meter, end = 1L))
          numerator[numerator == 2] <- 4
          denominator <- as.numeric(str_sub(meter, start = 3L))
          beat <- 1 / denominator
          recip <- Map(`/`, recip, beat)
          offsets <- offsets / beat
          
          measurePos <- offsets %% numerator
          metLev <- metposer(numerator, measurePos)
          metRel <- unlist(apply(sapply(parts, 
                                        function(part) {
                                         metRels <- rep(NA_integer_, nrow(part))
                                         metRels_ <- c(-1 * sign(diff(metLev[part$Onset])), NA)
                                         metRels[part$Onset] <- metRels_
                                         
                                         restenum <- unlist(tapply(part$Onset, contiguous(part$Onset), function(x) ifelse(x, 0, seq_along(x))))
                                         for (n in  sort(unique(restenum[restenum != 0]))) {
                                                   hits <- which(restenum == n)
                                                   ons <- closest(hits, which(restenum == 0), direction = 'above')
                                                   
                                                   metRels[hits] <- sign(metLev[hits] - metLev[ons])
                                         }
                                         
                                         metRels
                                        }  ) , 1, function(row) list(rev(row))), recursive = FALSE)
          
          data.table(FileName   = .name,
                     FileN      = .ind,
                     Composer   = ifelse(grepl('chor', .name), 'Bach', 'Praetorius'),
                     Record     = as.numeric(rownames(.)[-1][!grepl('^[=!*]', .[ , 1])]) + {if (grepl('chor', .name)) {5} else {1}},
                     Semits     = I(semits),
                     PCs        = I(pc),
                     Dur        = I(recip),
                     Offset     = offsets,
                     TS         = meter, 
                     Sign       = str_sub(fill_forward(notRE('\\*met'), .[,1]), start = 5L)[!grepl('^[=!*]', .[ , 1])],
                     MetPos     = measurePos,
                     MetLev     = metLev,
                     MetRel     = I(metRel),
                     isRest     = I(Rest),
                     isOnset    = I(Onset),
                     Measure    = measures,
                     Meter      = meter,
                     End        = end,
                     VLD        = VLD,
                     VLA        = VLA
          ) -> df
          
          
          df <- df[!allrests]
          
          
          }) -> Data
           

##

Data <- rbindlist(Data)

Data[ , NOnsets := sapply(isOnset, sum)]
Data[ , Phrases := 1 + c(0, head(cumsum(End), -1))]
Data <- Data[NOnsets > 0]
Data[ , Accented := Map(MetRel, MetPos, f = function(mr, mp) (mr > 0) | mp == 0)]
Data[ , NVoices := lengths(PCs)]
Data[ , NSounding := NVoices - sapply(isRest, sum)]
Data[ , SliceLength := sapply(Dur, min, na.rm = TRUE)]
Data[ , newPC  := Map(function(cur, prev) !cur %in% prev, PCs, rotate(PCs, 1))]
Data[ , NnewPCun := unlist(Map(function(pc, new) { sum(new[!duplicated(pc)], na.rm = TRUE)}, PCs, newPC) )]
Data[ , NnewPC :=  sapply(newPC, sum, na.rm = TRUE)]
Data[ , Fifths := lapply(PCs, pc2fifth)]


Data[ , DurRemain := {
          durs <- do.call('rbind', Dur)
          durs.ditto <- fill_forward(NA, durs)
          offs <- offs.na <- replicate(ncol(durs), Offset)
          offs.na[is.na(durs)] <- NA
          offs.ditto <- fill_forward(NA, offs.na)
          offs <- offs - offs.ditto
          durrems <- durs.ditto - offs
          list(split(durrems, 1:nrow(durs)))
          
          }, by = FileN]
Data[ , ExtendPastNextBeat := Map(f = function(d, mp, ons) {d[d == 1.1] <- 1;  d > mp & ons}, DurRemain, MetLev, isOnset)]

by(Data, INDICES = Data$FileName,
   function(sd) {
             if (uset(sd$NSounding) %len==% 1L) return(sd$NVoices)
             
             nVs <- sd$NVoice
             nV <- unique(nVs)
             
             c('lens', 'vals') %<-% rle(sd$NSounding)
             
             if (!(any(lens[vals != nV] > 1))) return(nVs)
             
             Map(rep, vals, each = lens) %|% unlist -> valvec
             Map(rep, lens, each = lens) %|% unlist -> lenvec
             
             contiguous(valvec != nV & lenvec > 1) -> groupvec
             groupvec[valvec == nV | lenvec == 1] <- 0
             
             tapply(sd$SliceLength, groupvec, sum)[-1] -> groupdurs
             longgroups <- as.numeric(names(groupdurs)[groupdurs >= 2])
             
             for (g in longgroups) {
                       nVs[groupvec == g] <- sd$NSounding[groupvec == g]
             }
             
             nVs
             
   }  ) %|% unlist -> Data$NActive


Data[ , Longer := lapply(DurRemain, function(dur) dur > min(dur, na.rm = TRUE))]


####Classify melodic trajectories of notes
# 
classrow <- function(sd) {
          list2env(sd, envir = environment())
          
          for(j in ls()) {
           if (is.list(get(j))) assign(j,  do.call('rbind', get(j))         )
          }

          
          rVLA <- Re(VLA)
          rVLD <- Re(VLD)
          
          
          output <- matrix('CT', nrow = nrow(rVLA), ncol = ncol(rVLA))
          output[!isOnset & !isRest] <- NA_character_
          
          changed <- sign(rVLA) != sign(rVLD) & rVLA != 0 & rVLD != 0
          changed
          
          unito    <- rVLA == 0
          unifrom  <- rVLD == 0
          stepto   <- abs(rVLA) < 3 & !unito
          stepfrom <- abs(rVLD) < 3 & !unifrom
          leapto   <- !stepto & !unito
          leapfrom <- !stepfrom & !unifrom
          sustain <- !isOnset & !isRest
          
          Durx <- fill_forward(NA, Dur)
          
          output[stepto & stepfrom & changed & abs(rVLA) == abs(rVLD) & !ExtendPastNextBeat] <- 'NT'
          output[stepto & stepfrom & changed & abs(rVLA) != abs(rVLD) & !ExtendPastNextBeat] <- 'NTchrom'
          output[stepto & stepfrom & !changed & !ExtendPastNextBeat] <- 'PT'
          
          output[stepto & stepfrom & changed & abs(rVLA) == abs(rVLD) & ExtendPastNextBeat] <- 'syncNT'
          output[stepto & stepfrom & !changed & ExtendPastNextBeat] <- 'syncPT'
          

          
          output[(unito | sustain) & (Accented | (Meter %~% '3/.*' & MetPos == 1)) & Im(VLD) < 0 & Im(VLD) > -3 & !ExtendPastNextBeat] <- 'Sus'
          output[(unito | sustain) & (Accented | (Meter %~% '3/.*' & MetPos == 1)) & Im(VLD) > 0 & Im(VLD) < 3 & !ExtendPastNextBeat ] <- 'Ret'
          
          output[!(unito | sustain) & !(rotate(sustain, -1)) & unifrom & !Accented & !ExtendPastNextBeat] <- 'Ant'
          output[changed & leapto & stepfrom & Accented & !ExtendPastNextBeat] <- 'App'
          
          
          output[stepto & leapfrom & changed & !ExtendPastNextBeat & !Accented & rotate(Accented, 1) & Dur <= rotate(Durx, 1)] <- 'Esc'
          output[stepfrom & leapto & changed & !ExtendPastNextBeat & !Accented & rotate(Accented, 1) & Dur <= rotate(Durx, 1)] <- 'Inc'
          
          
          output[(unito | sustain) & (unifrom | rotate(is.na(unifrom), -1) | End)] <- 'Ped' 
          
          output[output == 'PT' & rotate(output, -1) == 'PT' & !Accented & !rotate(changed, -1) & rotate(MetPos, -1) != 0 & Dur == rotate(Dur, -1)] <- 'DPT1'
          output[rotate(output,  1) == 'DPT1' & output != 'DPT1'] <- 'DPT2'
          
          output[unito & Accented & !rotate(Accented, -1) & !ExtendPastNextBeat & rVLD < -2 & (rVLD + rotate(rVLD, -1)) %in% c(-1, -2)] <- 'Sus+'
          output[rotate(output, 1) == 'Sus+' &  rVLD %in% c(1,2)] <- 'SusInc'
          
          output[stepto & changed & abs(rVLD) %in% c(3,4) & !ExtendPastNextBeat & Accented & !rotate(Accented, -1) & rotate(stepfrom, -1) ] <- 'Camb1'
          output[rotate(output, 1) == 'Camb1'] <- 'Camb2'
          # output <- `dim<-`(paste(Accented, output, sep = ':'), dim(Accented))
          # Accented <- dim<-`(ifelse(Accented, 'A', 'U'), dim(Accented))
          lapply(1:nrow(output), function(i) output[i,])
          
          
}

Data[ , pNCTs := list(classrow(.SD)), by = FileN]

Data[ , NoteIndices := tapply(seq_len(length(unlist(Data$PCs))), rep(1:nrow(Data), lengths(Data$PCs)), list)]

save(Data, file = 'data/ChoraleData.rData', compress = TRUE)
#}

####





####




###


#





load(file = 'data/ChoraleData.rData')

Data[ , Voice := lapply(PCs, seq_along)]


####################################################################################
##################################################################################


###########################################3
#### 
#### find start of windows
# Data$harmRhythm <- (Data$NOnsets == Data$NActive & Data$MetPos == 0) | 
#           # if all voices are attacking and we're on beat
#                    (Data$NOnsets == Data$NActive & Data$NOnsets > rotate(Data$NOnsets, -1)) |
#           # if all voices are attacking and there are more onsets then on the next slice, regardless of metric position
#                    (Data$MetPos != 0 & Data$ExtendPastNextBeat > 0 & Data$NOnsets > 1) | 
#           # if the metric position is off beat, but at least one attacked note rings past the next beat
#                     rotate(Data$End)

##
# 
# harmRhythm <- Data$NOnsets >= 4 & Data$MetPos == 0 #as default, four notes on strong beat is new chord
# 
# harmRhythm[(Data$Contour %l|v% frst %|% RE('^[.]')) & Data$MetPos == 0] <- FALSE # a sustatined bass note on a strong beat is not a new chord
# harmRhythm[Data$NOnsets == 4 & Data$NnewPC == 4] <- TRUE # four onsets that are all new pcs is a chord
# harmRhythm[Data$NOnsets >= 2 & (sapply(Data$Dur, '[[', 1) >= 1) ] <- TRUE # 2 or more onsets, where bass durations is at least one beat
# harmRhythm[Data$NOnsets >= 2 & sapply(Data$Dur, function(x) sum(x >= 1, na.rm = TRUE) >= 2) & Data$MetPos == 0 ] <- TRUE
# 
# harmRhythm[ Map(function(Contour, newPC) any(grepl('^L', Contour) & newPC), Data$Contour, Data$newPC) %|% unlist ]  <- TRUE
# 
# 
# harmRhythm[Data$NOnset == 1] <- FALSE
# harmRhythm[Map(function(isOnset, newPC) sum(isOnset & newPC) == 1, Data$isOnset, Data$newPC) %|% unlist] <- FALSE
# 
# 
# harmRhythm[Data$NSounding == 1 & rotate(Data$NSounding, -1) == 1] <- TRUE 
# 
# harmRhythm[(sapply(Data$Contour, '[[', 1) %~% '^L' ) & sapply(Data$newPC, sum) >= 1  ] <- TRUE # if bass leaps and there are at least one new pitch classes
# harmRhythm[ Map(function(ContourX, isOnset, newPC) any(grepl('L$', ContourX) & isOnset & newPC), Data$ContourX, Data$isOnset, Data$newPC) %|% unlist ]  <- TRUE
# harmRhythm[rotate(Data$End) <- TRUE
# Data$harmRhythm <- harmRhythm




trans <- function(chords) {
  
  chords <- gsub('Mm', '7', chords)
  chords <- gsub('MM', '^7', chords)
  chords <- gsub('mm', '_7', chords)
  chords <- gsub('dm', 'o7', chords)
  chords <- gsub('dd', 'oo7', chords)
  chords <- gsub('M', '', chords)
  chords[grep('d', chords)] <- tolower(gsub('d{1}', 'o', chords[grep('d', chords)]))
  chords[grep('m', chords)] <- tolower(gsub('m{1}', '', chords[grep('m', chords)]))
  chords[grep('-', chords)] <- gsub('-', 'b',  chords[grep('-', chords)])
  chords <- gsub('_7', 'm7', chords)
  chords[chords == rotate(chords)] <- '.'
  fill_forward(EQ('.'), chords)
  
}


########################################################################################################
#########a####################################################################################
writechords <- function(filename, chords) {
          
          
          basename <- filename %str-% '^data/' %str-% '\\.dat$'
          
          krnfile <- if (grepl('chor', filename)) 
                    'inst/extdata/' %str+% basename %str+% '.krn'
          else
                    '~/Bridge/Research/Data/Praetorius/Krn/' %str+% basename %str+% '.krn'
          
          krnfile <- readLines(krnfile)
          
          #get rid of non kern spines
          krnmat <- stri_list2matrix(strsplit(krnfile[!grepl('^!!', krnfile)], split = '\t'))
          krnmat <- krnmat[krnmat[,1] == '**kern',]
          krnfile[!grepl('^!!', krnfile)] <- apply(krnmat, 2, paste, collapse = '\t')
          
          records <- Data$Record[Data$FileName == filename]
          
          # account for reference records
          ref <- grepl('^!!!', krnfile)
          ref <- ref[1:(which(!ref)[1] - 1)] # only at beginning of file
          offset <- sum(ref) - {if (grepl('chor', filename)) 6 else 2}
          records <- records + offset
          
          ###################
          harmRh <-  Data$harmRhythm[Data$FileName == filename]
                    
          
          
          
          ###
          outfile <- krnfile
          
          for (i in 1:length(chords)) {
                    chords  <- chords[[1]][Data$FileName == filename]
                    chords[chords == rotate(chords) & !harmRh] <- '.'
                    
                    chords <- gsub('Mm', '7', chords)
                    chords <- gsub('MM', '^7', chords)
                    chords <- gsub('mm', '_7', chords)
                    chords <- gsub('dm', 'o7', chords)
                    chords <- gsub('dd', 'oo7', chords)
                    chords <- gsub('M', '', chords)
                    chords[grep('d', chords)] <- tolower(gsub('d{1}', 'o', chords[grep('d', chords)]))
                    chords[grep('m', chords)] <- tolower(gsub('m{1}', '', chords[grep('m', chords)]))
                    chords[grep('-', chords)] <- gsub('-', 'b',  chords[grep('-', chords)])
                    chords <- gsub('_7', 'm7', chords)
                    chords[chords == rotate(chords)] <- '.'
                    #
                    harmRh <- c('.', '|')[1 + harmRh]
                    
                    
                    
                    
                    outfile[records] <- krnfile[records] %str+% '\t' %str+% chords %str+% '\t' %str+% harmRh
                    
                    outfile[grep('^\\*\\*', krnfile)] <- outfile[grep('^\\*\\*', krnfile)] %str+% '\t**text\t**text'
                    outfile[grep('^\\*[^*]', krnfile)] <- outfile[grep('^\\*[^*]', krnfile)] %str+% '\t*\t*'
                    outfile[grep('^\\*-\t', krnfile)] <-  krnfile[grep('^\\*-\t', krnfile)] %str+% '\t*-\t*-'
                    
                    outfile[grep('^![^!]', krnfile)] <- outfile[grep('^![^!]', krnfile)] %str+% '\t!\t!'
                    outfile[grep('=', krnfile)] <- outfile[grep('^=', krnfile)] %str+% (('\t' %str+% (outfile[grep('^=', krnfile)] %strkeep% '^=[^\t]*')) %str*% 2)
                    
                    #all rest records:
                    outfile[grepl('^[^!=*]', outfile) & !grepl('[A-Ga-g]', outfile)] <- outfile[grepl('^[^!=*]', outfile) & !grepl('[A-Ga-g]', outfile)] %str+% '\t.\t.'
                    
                    outfile[!grepl('^!!!', outfile)] <- sapply(outfile[!grepl('^!!!', outfile)],
                                                               function(rec) {
                                                                         rec <- rec %str/% '\t'
                                                                         
                                                                         chord <- rec[length(rec) -1]
                                                                         rhy <- rec[length(rec)]
                                                                         
                                                                         notes <- rec[1:(length(rec) - 2)]
                                                                         
                                                                         rec <- c(notes[1], chord, notes[2], rhy, notes[-1:-2])
                                                                         paste(rec, collapse = '\t')
                                                               })
                    
          }
          
          writeLines(outfile, con = 'inst/' %str+% basename %str+% '.krn')
          
          
          
}



##################################
#####get fifth based analyses
# 
# Fifths <- data.frame(row.names = rownames(Data))
#           
# Fifths$Pitchs <- lapply(Data$PCs, pc2fifth)
# Fifths$Abs <- lapply(Data$PCs, pc2fifth %.% f.(. - min(., na.rm = TRUE)))
# Fifths$AbsU <- lapply(Data$PCs, pc2fifth %.% f.(. - min(., na.rm = TRUE)) %.% uset)
# Fifths$Bass <- lapply(Data$PCs, pc2fifth %.% f.(. - .[1]))
# Fifths$BassU <- lapply(Data$PCs, pc2fifth %.% f.(. - .[1]) %.% uset)
# Fifths$Ints <- lapply(Data$PCs, pc2fifth %.% uset %.% diff)
# Fifths$IntCombs <- lapply(Data$PCs, pc2fifth %.% f.(combn(., 2, diff)) %.% abs %.% uset)
# 
# Fifths$VLD <- unlist(tapply(Fifths$Pitchs, Data$Phrases, apply2gram_forward(`-`)), recursive = F)
# Fifths$VLD <- ifelse(lengths(Fifths$VLD) == 0, 
#                      lapply(lengths(Fifths$Pitchs), f.(rep(NA,.))), 
#                      Fifths$VLD)
# 
# Fifths$VLDgen <- sapply(Fifths$VLD, f.(. %% 7))
# 
# allons <- (Data$isOnset %l|l% sum) == lengths(Data$PCs)
                      