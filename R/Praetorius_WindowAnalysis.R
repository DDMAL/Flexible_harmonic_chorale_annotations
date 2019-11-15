# Data$Qualities <- rep('X', nrow(Data))
library(abind)
library(MASS)

Data$harmRhythm <- (Data$NOnsets == Data$NActive & Data$MetPos == floor(Data$MetPos)) | 
          # if all voices are attacking and we're on beat
          (Data$NOnsets == Data$NActive & Data$NOnsets > rotate(Data$NOnsets, -1)) |
          # if all voices are attacking and there are more onsets then on the next slice, regardless of metric position
          (Data$MetPos != floor(Data$MetPos) & sapply(Data$ExtendPastNextBeat, sum, na.rm = TRUE) > 0 & Data$NOnsets > 1) | 
          # if the metric position is off beat, but at least one attacked note rings past the next beat
          rotate(Data$End) # OR time signature change?



Data$harmRhythm[Data$FileName %~% 'chor043' & Data$Record %in% c(24,25, 44, 45,87)] <- FALSE #chor043 (call and response)


# chor 015
chor.tmp <- 'chor015'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 99] <- FALSE

# chor 024
chor.tmp <- 'chor024'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 75] <- FALSE

# chor 024
chor.tmp <- 'chor041'
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 122][[1]][3] <- 'Sus'

# chor 120
chor.tmp <- 'chor120'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 32] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 46] <- FALSE # false positive caused by ExtendPastNextBeat

# chor 127
chor.tmp <- 'chor127'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 78] <- FALSE

# chor 133
chor.tmp <- 'chor133'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 126] <- FALSE


# chor 145
chor.tmp <- 'chor145'
# Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 35][[1]][3] <- 'NT' # C B C# where C is neighbor

# 201

chor.tmp <- 'chor201'
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 64][[1]][1] <- 'Esc' # quarter note escape tone
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 65][[1]][3] <- 'Inc' # Incomplete neighbor, not changing direction

# 202
chor.tmp <- 'chor202'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 180] <- FALSE 
# 

# chor 220
chor.tmp <- 'chor220'
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 108][[1]][1] <- 'Esc' # Weird escape tone "b13" chord

# chor 234
chor.tmp <- 'chor234'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 156] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 157] <- FALSE
# 

# chor 251
chor.tmp <- 'chor251'
# Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 75][[1]][1] <- 'NT' # Bb A Bn where A is neighbor
# 
# chor 283
chor.tmp <- 'chor283'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 36] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(40,45)] <- TRUE


# 306 (which is identical to 201)

chor.tmp <- 'chor306'
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 64][[1]][1] <- 'Esc' # quarter note escape tone
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 65][[1]][3] <- 'Inc' # Incomplete neighbor, not changing direction


# chor 316
chor.tmp <- 'chor316'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 35] <- TRUE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 38] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 40] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 42] <- TRUE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 44] <- FALSE

# chor 371
chor.tmp <- 'chor371'
# Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 67][[1]][4] <- 'NT' # C B C# where C is neighbor


# Prat 059
chor.tmp <- 'Pra.*059'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(61, 65, 70, 82, 86, 95, 99)] <- TRUE


# Prat 088
chor.tmp <- 'Pra.*088'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(51, 54, 56, 88, 95)] <- TRUE

# Prat 090
chor.tmp <- 'Pra.*090'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(58, 62, 144, 148, 159, 168)] <- TRUE

# Prat 091
chor.tmp <- 'Pra.*091'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(21,25,28, 30)] <- TRUE

# Prat 102
chor.tmp <- 'Pra.*102'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 56] <- TRUE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 57] <- FALSE
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 58] <- FALSE
Data$pNCTs[Data$FileName %~% chor.tmp & Data$Record == 57][[1]][3] <- 'Ant' # shift syncopation!



# Prat 103
chor.tmp <- 'Pra.*103'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 101] <- TRUE



# Prat 130a
chor.tmp <- 'Pra.*130a'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(47,52)] <- TRUE

# Prat 130b
chor.tmp <- 'Pra.*130b'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record %in% c(40,43, 121)] <- TRUE



# Prat 179
chor.tmp <- 'Pra.*179'
Data$harmRhythm[Data$FileName %~% chor.tmp & Data$Record == 144] <- TRUE

###
Data$harmRhythm[Data$NOnsets == 1 & Data$NActive == 1 & rotate(Data$NActive, 1) == 1] <- FALSE # deal with few weird single voice places


Data$windows <- cumsum(Data$harmRhythm)
Data$windowsize <- tapply(Data$windows, Data$windows, function(x) rep(length(x), length(x))) %|% unlist

#######################DATA FOR Rpackage
#######################
Data$Slice <- seq_len(nrow(Data))

colnames(Data) <- c('Slice_FileName',
                    'Slice_FileNumber',
                    'Slice_Composer',
                    'Slice_Record',
                    'Note_Semits',
                    'Note_TonalName', 
                    'Note_Duration',
                    'Slice_Duration_FromBeginning',
                    'Slice_TimeSignature',
                    'Slice_MensurationSign',
                    'Slice_Metric_Position',
                    'Slice_Metric_Level',
                    'Note_Metric_Level_Relative',
                    'Note_isRest',
                    'Note_isOnset',
                    'Slice_Measure',
                    'Slice_Meter',
                    'Slice_Phrase_Boundary',
                    'Note_VoiceLeading_Departure',
                    'Note_VoiceLeading_Approach',
                    'Slice_NumberOf_Onsets',
                    'Slice_Phrase_Number',
                    'Note_Accented_Relative',
                    'Slice_NumberOfVoices',
                    'Slice_NumberOfSoundingNotes',
                    'Slice_Duration',
                    'Note_isNewPitchClass',
                    'Slice_NumberOfNewPitchClasses_Unique',
                    'Slice_NumberOf_NewPitchClasses',
                    'Note_CircleOfFifths',
                    'Note_Duration_Remaining',
                    'Note_ExtendsPastNextBeat',
                    'Slice_NumberOfVoices_Active',
                    'Note_isLongerThanSlice',
                    'Note_PotentialNonChordToneType',
                    'Note_Number',
                    'Note_VoiceNumber',
                    'Slice_isNewWindow',
                    'Slice_WindowNumber',
                    'Slice_WindowSize',
                    'Slice_Number')
setcolorder(Data, neworder = sort(colnames(Data)))
Data[ , Slice_TimeSignature := NULL]

                    
                    


Notes <- copy(Data)
cbind(Notes[ , lapply(.SD[ ,  sapply(Notes, class) %in% c('list', 'AsIs'), with = FALSE], unlist)],
      Notes[ , lapply(.SD[ , !sapply(Notes, class) %in% c('list', 'AsIs'), with = FALSE], function(col) rep(col, lengths(Note_TonalName)))]) -> Notes




ChoraleTable_Slices <- Data
ChoraleTable_Notes  <- Notes

# save(ChoraleTable_Slices, file = 'ChoraleTable_Slices.RData', compress = 'xz')
# save(ChoraleTable_Notes,   file = 'ChoraleTable_Notes.RData',   compress = 'xz')




###############################################################################

legal <- c('?' = '0',
           'M' = '1_3',
           'm' = '3_1',
           'd' = '3_3',
           'Mm' = '2_1_3',
           'MM' = '1_3_1',
           'mm' = '1_2_1',
           'dm' = '3_1_2',
           'dd' = '3_3_3',
           'Gr+6' = '1_3_6',
           'Fr+6' = '4_2_4',
           'It+6' = '4_6',
           'P5' = '1',
           'M3' = '4',
           'm3' = '3',
           'Mm?5' = '2_4',
           'dm?3' = '4_2',
           'mm?5' = '1_2',
           # 'MM?3' = '1_4',
           'MM?5' = '4_1',
           '?m'   = '2_1',
           '?d'   = '6_3',
           '?dd'   = '3_6')



### this controls which sets are "folded into" larger supersets. For instance, a incomplete o7 chord should be ignored if the full dominant o7 chord is present.
### the list's names are the superset, the vectors are the subsets.
### The main point here is to determine 1) which triads are subsumed into larger tetrachords 2) which diads are NOT absorbed into larger chords.
subchords <- list(
  `1_3`   = c('1', '4', '3'), # P5s, M3s, or m3s are folded in to major chords triads if present. 
  `3_1`   = c('1', '3'), # P5s and m3s are folded into minor traids.
  `3_3`   = c('3', '6'), 
  `2_1_3` = c('6', '1', '4', '3', '2',
              '2_1', '2_4', '3_3'),
  `1_3_1` = c('4_1', '1_4'), # don't absorb M3s, or P5s, since there is a p5 between third and seventh.
  `1_2_1` = c('1_2', '2_1'), # don't absorb P5s, since there is a p5 between third and seventh.
  `3_1_2` = c('6', '3',
              '4_2', '1_2', '3_3'),
  `3_3_3` = c('6', '3', '9',
              '3_6', '6_3', '3_3')
)

## translate any combo of PCs to a fifth interval representation
allPCs <- unique(unlist(Data$Note_TonalName))
allPCs <- allPCs[!is.na(allPCs)]
2:4 %lxl% {
          
          combn(allPCs, ., simplify = FALSE, FUN = sort) -> combs
          lapply(combs, pc2fifth) -> fifs
          names(fifs) <- sapply(lapply(combs, sort), collapsestr('_'))
          fifs
          
          
          } %>% unlist(recursive = FALSE) %l|l% sort %l|l% diff %l|v% collapsestr('_') -> allpccombofifths
allpccombofifths <- c(setNames(rep('0', length(allPCs)), allPCs), allpccombofifths)


##translate any combo of PCs to a chord name
lapply(names(allpccombofifths),
       function(pcs) {
         pcs <- pcs %str/% '_'
         if (length(pcs) == 1) return(pcs %str+% '??')
         chord <- basicChordParse(pcs)
         
         if (is.na(chord$Root) || is.na(chord$Quality)[1]) {
              NA_character_
         } else {
               chord$Root %str+% chord$Quality[1]
         }
         
       }) %|% unlist -> allpccombochords
names(allpccombochords) <- names(allpccombofifths)

library(stringr)

allpccomboQuality <- setNames(stringr::str_replace(allpccombochords, '^[A-G][#-]*', ''), names(allpccombochords))
allpccomboRoot    <- setNames(stringr::str_extract(allpccombochords, '^[A-G][#-]*'), names(allpccombochords))

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

allcombs <- function(x, min = 1, max = length(x)) {
 setNames(lapply(min:max, function(M) combn(x, M, simplify = FALSE)), num2word(min:max))
}

####################What are legal ways to divide a window?

contiguouscombs <- function(N) {
          
          lvec <- logical(N)
          lvec[1] <- TRUE
          vec <- seq_len(N)
          
          lapply(vec - 1,
                 function(n) {
                          combs <- if (vec %len<=% 2L) { 
                            if (n == 0) list() else list(vec[-1])
                            
                          } else { combs <- combn(vec[-1], n, simplify = FALSE)}
                          if (len0(combs)) return( list(tapply(vec, cumsum(lvec), force, simplify = FALSE)))
                          lapply(combs,
                                 function(x) {
                                   lvec[x] <- TRUE
                                   tapply(vec, cumsum(lvec), force, simplify = FALSE)
                                 }) 
                           
                 }) %>% unlist(recursive = FALSE)
}
legaldivys <- lapply(1:max(Data$Slice_WindowSize), contiguouscombs) 
# legaldivys <- lapply(legaldivys,
                 # C    function(x) browser())
# divys <- divys[!(divys %l|l% lengths %l|l% divs %l|v% collapsestr('_') ) %in% c('0.333333333333333', '0.5_2')] # get rid of illegal divys, 1_234 or 1_23_4
ncttypes <- unique(unlist(Data$pNCTs))
ncttypes <- ncttypes[!is.na(ncttypes)]
################

exclusion <- function(candi, pcs) {
 if (allsame(pcs)) return(list())
   
 notna <- !is.na(candi)
 candi <- candi[notna]
 pcs <- pcs[notna]
          
 runs <- rle(pcs)
 pcs <- runs$values
 candi <- candi[cumsum(c(1, init(runs$len)))]
 
 
 pairs <- cbind(pcs[candi], c(pcs[rotate(candi, 1, pad = FALSE)], pcs[rotate(candi, -1, pad = FALSE)]))
 pairs <- apply(pairs, 1, sort %.% unique)
  
 do.call('rbind', pairs)
}

excluder <- function(pcs, pncts, mays) {
 if (ncol(pcs) == 1) return(matrix("", ncol = 2, nrow = 0))
 both <- leftward <- rightward <- indirect <- matrix(FALSE, nrow(pcs), ncol(pcs))
 both[] <- pncts %in% c('NT', "PT", 'Sus', 'Esc', 'Inc', 'App', 'Ant', 'Ret', 'syncNT', 'syncPT', 'NTchrom') & mays
 
 leftpairs <- rightpairs <- indirectpairs <- list()
 
 lapply(which(apply(both, 1, any) & apply(pcs, 1, length %.% unique ) > 1),
        function(i) {
          pc   <- pcs[i, ]
          sing <- both[i, ]
          runs <- rle(pc)
          pc <- runs$values
          sing <- sing[cumsum(c(1, init(runs$len)))]
          
          curpcs <- pc[sing]
          
          nextpcs <- pc[rotate(sing,  1, pad = FALSE)]
          prevpcs <- pc[rotate(sing, -1, pad = FALSE)]
          
          ps <- list( cbind(head(curpcs, length(nextpcs)), nextpcs), cbind(tail(curpcs, length(prevpcs)), prevpcs))
          ps <- unique(unlist(unlist(lapply(ps[lengths(ps) > 1], function(p) apply(p, 1, list %.% sort)), recursive = FALSE), recursive = FALSE))
          ps[sapply(ps, length %.% unique) > 1]
        }) -> bpairs
 bpairs <- unlist(bpairs, recursive = FALSE)
 #
 leftward[]  <- pncts %in% c('DPT1', 'Sus+', 'Camb1') & mays
 if (any(leftward, na.rm = TRUE)) {
           lapply(which(apply(leftward, 1, any) & apply(pcs, 1, length %.% unique ) > 1),
                  function(i) {
                            pc   <- pcs[i, ]
                            
                            runs <- rle(pc)
                            pc <- runs$values
                            
                            
                            ## d1
                            left <- leftward[i, ]
                            left <- left[cumsum(c(1, init(runs$len)))]
                            
                            curpcs <- pc[left]
                            prevpcs <- pc[rotate(left, -1, pad = FALSE)]
          
                            ps <- list(cbind(tail(curpcs, length(prevpcs)), prevpcs))
                            
                            ps <- unique(unlist(unlist(lapply(ps[lengths(ps) > 1], function(p) apply(p, 1, list %.% sort)), recursive = FALSE), recursive = FALSE))
                            ps[sapply(ps, length %.% unique) > 1]
                            
                  }) -> leftpairs
           leftpairs <- unlist(leftpairs, recursive = FALSE)
 }
 
 #
 rightward[] <- pncts %in% c('DPT2', 'SusInc', 'Camb2') & mays
 if (any(rightward, na.rm = TRUE)) {
           lapply(which(apply(rightward, 1, any) & apply(pcs, 1, length %.% unique ) > 1),
                  function(i) {
                            pc   <- pcs[i, ]
                            
                            runs <- rle(pc)
                            pc <- runs$values
                            
                            
                            right <- rightward[i, ]
                            right <- right[cumsum(c(1, init(runs$len)))]
                            
                            curpcs <- pc[right]
                            nextpcs <- pc[rotate(right,  1, pad = FALSE)]
                            
                            ps <- list(cbind(tail(curpcs, length(nextpcs)), nextpcs))
                            
                            ps <- unique(unlist(unlist(lapply(ps[lengths(ps) > 1], function(p) apply(p, 1, list %.% sort)), recursive = FALSE), recursive = FALSE))
                            ps[sapply(ps, length %.% unique) > 1]
                            
                  }) -> rightpairs
                            
           rightpairs <- unlist(rightpairs, recursive = FALSE)
 }
 #
 indirect[] <- (pncts == 'Sus+' & mays) & (rotate(pncts == 'Sus+', margin = 2, rotation = 2, pad = FALSE) & mays)
 if (any(indirect, na.rm = TRUE)) {
           lapply(which(apply(indirect, 1, any) & apply(pcs, 1, length %.% unique ) > 1),
                  function(i) {
                            pc   <- pcs[i, ]
                            
                            runs <- rle(pc)
                            pc <- runs$values
                            
                            
                            right <- indirect[i, ]
                            right <- right[cumsum(c(1, init(runs$len)))]
                            
                            curpcs <- pc[right]
                            nextpcs <- pc[rotate(right,  2, pad = FALSE)]
                            
                            ps <- list(cbind(tail(curpcs, length(nextpcs)), nextpcs))
                            
                            ps <- unique(unlist(unlist(lapply(ps[lengths(ps) > 1], function(p) apply(p, 1, list %.% sort)), recursive = FALSE), recursive = FALSE))
                            ps[sapply(ps, length %.% unique) > 1]
                            
                  }) -> indirectpairs
           
           indirectpairs <- unlist(indirectpairs, recursive = FALSE)
 }
 
 pairs <- c(bpairs, leftpairs, rightpairs, indirectpairs)
 if (!is.null(pairs) & length(pairs) > 0 ) do.call('rbind', pairs) else matrix('', ncol = 2, nrow = 0)
}



windowParse <- function(rows, p = 100) {
          win <- unique(rows$Slice_WindowNumber)
          if ((win %% p) == 0) print(win)
          #this function takes a slice window (rows from the Data object) and returns a data.frame of possible analyses of that window
          rows %lxl% {. %splat|% cbind} -> data
          
          slice <- col(data$Note_TonalName)
          nslices <- ncol(slice)
          
          divys <- legaldivys[[nslices]]
          ##
          data$mustbeC <- data$Note_PotentialNonChordToneType == 'CT'
          # if (nslices > 1) { #
          #           
          #           relativelylongA <- data$Dur[, -nslices] < data$Dur[, -1]    
          #           relativelylongA[is.na(relativelylongA)] <- FALSE
          #           
          #           data$mustbeC[, -1] <- data$mustbeC[, -1]  |  (relativelylongA  & data$ContourX[ , -1]  == 'SS')
          # }
          lapply(divys,
                 function(divy) {
                   lapply(divy,
                          function(j) {
                                    list2env(lapply(data, function(obj) obj[ , j, drop = FALSE]), envir = environment())
                                    ncols <- ncol(Note_TonalName)
                                    mustbeC[, ncols] <- mustbeC[ ,ncols] | 
                                              Note_isLongerThanSlice[ , ncols]   |# if last note in a chord sustains past the end of the chord, it can't be a non-chord tone (because first slice of next window must be new harmony)
                                               Note_PotentialNonChordToneType[ , ncols] %in% c('Sus+', 'Camb1')  # can't have Suspension+ figures across window
                                    
                                    Note_PotentialNonChordToneType[ , ncols] <- gsub("DPT1", "PT", Note_PotentialNonChordToneType[ ,ncols])
                                    Note_PotentialNonChordToneType[ , ncols] <- gsub("DPT1", "PT", Note_PotentialNonChordToneType[ ,ncols])
                                    Note_PotentialNonChordToneType[ , 1] <- gsub("DPT2", "PT", Note_PotentialNonChordToneType[ ,1])
                                    Note_PotentialNonChordToneType[ , 1] <- gsub('SusInc', "Inc", Note_PotentialNonChordToneType[ ,1])
                                    Note_PotentialNonChordToneType[ , 1] <- gsub('Camb2', "Inc", Note_PotentialNonChordToneType[ ,1])
                                              
                              
                                    mustbeC[ , 1] <- mustbeC[ , 1] |
                                              (is.na(mustbeC[ , 1]) & abs(Im(Note_VoiceLeading_Departure[ , 1])))  # notes which sustain at beginning of chord change can only be suspension or chord tone

                                             
                                    musts  <- unique(Note_TonalName[mustbeC])
                                    musts <- musts[!is.na(musts)]
                                    mustbeC[Note_TonalName %in% musts] <- TRUE # if a PC already mustbeC, then all other instances of that same PC must also be
                                    
                                    
                                    maybes <- unique(Note_TonalName[!mustbeC])
                                    maybes <- maybes[!is.na(maybes)]
                                    candidates <- !mustbeC & !is.na(Note_TonalName)
                                    # which PCs exclude each other
                                    exclusions <- if(length(maybes) > 1) excluder(Note_TonalName, Note_PotentialNonChordToneType, candidates) else matrix("", ncol=2, nrow = 0)
                       
                       
                                    if ( maybes %len>% 0L) {
                                      pccombs <- allcombs(maybes, 
                                                               min = max(0, 1 - length(musts)),
                                                               max = max(length(maybes), min(4 - length(musts), length(maybes))))
                                      pccombs <- lapply(pccombs, function(x) if(lennot0(x)) x else list(c()))
                                      pccombs <- unlist(pccombs, recursive = FALSE)
                                      
                                      pccombs <- lapply(pccombs, append, musts)
                                  
                                      if (nrow(exclusions) > 0) pccombs <- pccombs[sapply(pccombs, function(comb) all(exclusions[ ,1] %in% comb | exclusions[ ,2] %in% comb))]
                                      
                                    } else {
                                      pccombs <- list(musts)
                                    }
                                    #####
                                    pccombstrs <- sapply(pccombs, function(x) paste(sort(x), collapse = '_'))
                                    hints   <- allpccombofifths[pccombstrs] # get actual chord representation
                                    roots      <- allpccomboRoot[pccombstrs]
                                    qualities  <- allpccomboQuality[pccombstrs]
                                    
                                    pccombls <- lengths(pccombs)
                                    if (any(pccombls > 1 & !is.na(roots))) {
                                              keep <- pccombls > 1 & !is.na(roots)
                                              hints <- hints[keep]
                                              roots <- roots[keep]
                                              qualities <- qualities[keep]
                                              pccombs <- pccombs[keep]
                                              pccombls <- pccombls[keep]
                                              pccombstrs <- pccombstrs[keep]
                                    }
                                    
                                    
                                    
                                    ################# # this removes any three-note chord which is an incomplete seventh chord, when the complete 7th chord is also an option
                                    if (any(names(subchords) %in% hints)) {
                                        superchords <- which(hints %in% names(subchords))
                                              
                                        Map(hints[superchords], pccombs[superchords],
                                                             f = function(.fifths, .pcs) {
                                                                       which(hints %in% subchords[[.fifths]] & sapply(pccombs, function(.p) all(.p %in% .pcs)))
                                                             }) %>% unlist -> subchordstoremove 
                                      if (lennot0(subchordstoremove)) {
                                        hints <- hints[-subchordstoremove]
                                        roots <- roots[-subchordstoremove]
                                        qualities <- qualities[-subchordstoremove]
                                        pccombs <- pccombs[-subchordstoremove]
                                      }
                                      
                                    }
                                    if (len0(pccombs)) return(NULL)
                                    ######
                                    legalchords <- hints %in% legal
                                    pccombs <- pccombs[legalchords]
                                    hints   <- hints[legalchords]
                                    roots  <- roots[legalchords]
                                    qualities  <- qualities[legalchords]
                                    
                                    ###
                                    if (len0(pccombs)) return(NULL)
                                    ###############################
                                    
                                    ###############
                                    ######Now that we've found the possiblities, the following code
                                    ######comes up with different measures/descriptions of these possibilities,
                                    ######which can be used in the filtering stage later.
                                    # susins <- col(PCs) == 1 & !isOnset
                                    # onsets <- isOnset & !susins
                                    # oncons <- factor(c(pNCTs[susins], pNCTs[onsets]), levels = ncttypes)
                                    # onpcs  <- c(PCs[susins], PCs[onsets])
                                    # # onfifs <- c(Fifths[susins], Fifths[onsets])
                                    # 
                                    # onrels <- factor(c(MetRel[susins], MetRel[onsets]), c('0', '-1','1',NA))
                                    #  
                                    # pccombs %lxl% {
                                    #            oncons[onpcs %in% .] <- 'CT'
                                    #            table(onrels,
                                    #                  oncons) -> tab
                                    #           rownames(tab) <- c('Same', 'Weak', 'Strong')
                                    #            tab
                                    #           } -> ContourTabs
                                    # # 
                                    # # 
                                    # ContourTabs %>% abind(along =3) %>% aperm(perm = c(3,2,1)) -> ContourTabs
                                    # 
                                    # 
                                    # ########### how many slices do you need before all the chord's PCs are present?
                                    pcfirstj <- c(musts, maybes) %lxv% min(apply(Note_TonalName, 1, function(pc) which(pc == .)[1]) , na.rm = TRUE)
                                    names(pcfirstj) <- c(musts, maybes)
                                    pccombs %lxv% {max(pcfirstj[.])} -> pccombdelay
                                    pccombdelay %lxv% {sum(c(0, Slice_Duration)[1:.])} -> pccombdelaybeats
                                    #####################
                                    #####Assemble output
                                    ####################
                                    indices    <- lapply(pccombs, function(pcs) Note_Number[ Note_TonalName %in% pcs])
                                    notindices <- lapply(pccombs, function(pcs) Note_Number[!Note_TonalName %in% pcs])
                                    NSlices <- ncol(Note_TonalName)
                                    data.table(Root = I(roots),
                                               Quality = I(qualities),
                                               CT_Note_Number = I(indices),
                                               NCT_Note_Number = I(notindices),
                                               Chord_Duration = sum(Slice_Duration),
                                               Chord_NumberOf_Slices = NSlices,
                                               Slice_Number = list(Slice_Number[1,])[rep(1, length(roots))],
                                               PCs = I(pccombs),
                                               Chord_CompletionDelay_Slices = pccombdelay - 1,
                                               Chord_CompletionDelay_Duration = pccombdelaybeats) -> output
                                    # data.frame(Hints = hints, 
                                               # Nslices = ncol(Note_TonalName),
                                               # PCs = I(pccombs),
                                               # Ind_nCTs = I(indices),
                                               # Ind_CTs  = I(notindices),
                                               # FileName = FileName[1],
                                               # FileN    = FileN[1],
                                               # Record   = I(replicate(length(pccombs), Record[1,], simplify = FALSE)),
                                               # ChordDelay = pccombdelay - 1,
                                               # ChordDelayBeats = pccombdelaybeats,
                                               
                                               # ChordName = chords,
                                               # NewPCs = Slice_NumberOf_NewPitchClasses[,1],
                                               # oldPCs = lengths(pccombs) - Slice_NumberOf_NewPitchClasses[ , 1],
                                               # nCTs  = rowSums(ContourTabs[ ,  1, , drop = FALSE]),
                                               # nNCTs = rowSums(ContourTabs[ , -1, , drop = FALSE]),
                                               # MetPos = MetPos[,1],
                                               # Duration = sum(SliceLength),
                                               # Onsets = sum(isOnset[,1]),
                                               # ContourTabs,
                                               # stringsAsFactors = FALSE) -> output
                                    output
                                    
                          }) -> eachdivy
                    divy %l|v% collapsestr('') -> names(eachdivy)
                   if (eachdivy %len>% 1L & !any(sapply(eachdivy, is.null))) { # this removes chords that are subset of previous chord
                     for(d in length(eachdivy) : 2) {
                               cur <- eachdivy[[d]]$PCs
                               prev <- eachdivy[[d - 1]]$PCs

                               subsets <- Vectorize(isSubset, vectorize.args = 'of', SIMPLIFY = FALSE)(prev, cur)
                               do.call('rbind', subsets) %>% apply(2, any) -> removes
                      eachdivy[[d]] <- eachdivy[[d]][!removes, ]
                     }
                   }
                    
                    if (any(sapply(eachdivy, is.null)) || any(sapply(eachdivy, EQ(0) %.% nrow))) eachdivy <- NULL
                    if (length(eachdivy) > 1 && {
                              sapply(lapply(lapply(eachdivy, function(x) x$Root %str+% x$Quality), sort %.% unique), collapsestr('')) -> chordsperdivy
                              any(chordsperdivy == rotate(chordsperdivy), na.rm = TRUE)}
                        ) NULL else eachdivy
                 }) -> alldivys
          
          # alldivys <- Filter(function(x) all(sapply(x, nrow) > 0) , alldivys)
          # alldivys <- alldivys[sapply(alldivys, length) > 0]
          alldivys <- Filter(Negate(is.null), alldivys)
          
          alldivys %l|v% (function(x) collapsestr('_', names(x))) -> names(alldivys)
          
          lapply(alldivys,
                 function(cur) {
                           cur %lxl% {seq_len(nrow(.))} %splat|% expand.grid %>% 
                             apply(1, f.({ 
                               Map(function(w, i) w[i ,], cur, .) %splat|% rbind -> chords
                               if (nrow(chords) > 1 && 
                                   any(chords$Root == rotate(chords$Root) & 
                                       chords$Quality == rotate(chords$Quality), na.rm = TRUE)) return(NULL) # this gets rid of repetition
                               
                               chords[ , lapply(.SD, list %.% c %.% unlist)]
                               
                               }))
                           
                           
                 }) %>% unlist(recursive = FALSE) %splat|% rbind -> progressions
          # progressions <- Filter(f.({ ## remove repetition
                    # if (nrow(.) == 1) return(TRUE)
                    # root <- str_extract(.$ChordName, '[A-G][-#]*')
                    # !any(root == rotate(root) & .$ChordName == rotate(.$ChordName), na.rm = TRUE)
                    # }), 
                    # progressions)
          
          
          progressions$Window_Number <- rows$Slice_WindowNumber[1]
          progressions$Window_NumberOf_Chords <- lengths(progressions$Root)
          progressions$Window_NumberOf_Slices <- lengths(progressions$Slice_Number)
          
          progressions[ , 'PCs' := NULL]
          
          progressions
        
      
} 



####################################################################################################################################
##parse some windows!

windowCombs <- Data[ , windowParse(.SD, 100), by = Slice_WindowNumber, verbose = TRUE, 
                     .SDcols = c('Note_TonalName', 'Slice_WindowNumber', 'Note_Metric_Level_Relative', 'Note_PotentialNonChordToneType', 
                                'Note_isOnset', 'Note_isRest', 'Slice_NumberOf_NewPitchClasses', 'Note_VoiceLeading_Departure',
                                'Slice_Duration', 'Note_isLongerThanSlice', 'Slice_Metric_Position', 'Note_Number', 'Slice_Number')]
          
# save(windowCombs, file = 'windowCombinations_Sep12_2018.rData', compress = TRUE)

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

legalranks <- c(7,0, 0, 1, 2, 4, 2, 2, 2, 6, 6, 6, 5, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5)
names(legalranks) <- names(legal)

summarizeWindowCombs <- function(windows) {
  lapply(windows,
         function(progs) {
           lapply(progs,
                  f.({
                    list2env(., envir = environment())
                    data.frame(
                      NChords = nrow(.),
                      stringsAsFactors = FALSE,
                      NOnsets = min(Onsets),
                      NCTs   = fractions(sum(nNCTs) / (sum(nCTs) + sum(nNCTs))),
                      Delay   =  max(ChordDelayBeats),
                      Rank   = max(legalranks[legal %in% Hints]),
                      Duration = min(Duration)
                    ) -> output
                    
                    contcols <- colnames(.) %~% 'Strong|Weak|Same'
                    contours <- .[, contcols] %|% colSums
                    dim(contours) <- c(1, length(contours))
                    colnames(contours) <- colnames(.)[contcols]
                    output$Contours <- list(contours)
                    
                    output
                  })) %splat|% rbind -> analyses
         })
}

windowCombs[, Summaries := summarizeWindowCombs(Combs)]

# save(windowCombs, file = 'windowCombinationsJuly24.rData', compress = TRUE)
load(file = 'windowCombinationsJuly24.rData')


unwrapFormula <- function(x) {
          if (length(x) > 1 && deparse(x[[1]]) == '~') {
                    out <- sys.function()(x[[2]])
                    if(length(x) > 2) append(out, x[[3]]) else out
          } else {
                    list(x)
          }
}

Contour <- function(form) {
          # forms <- unwrapFormula(form)
          # 
          contours <- parent.frame()$Contours %splat|% rbind
          
          
          apply(contours, 1,
                function(row) {
                          getContour(form, row)
                          #                 lapply(forms,
                          #                        function(form) {
                          #                                 which(sapply(form, is.character)) -> res
                          #                                 for(i in res) {
                          #                                  form[[i]] <- getContour(form[[i]], row)         
                          #                                 }
                          #                                  if (is.character(form)) as.numeric(form) else  eval(form)
                          #                        }) -> evaled
                          #                 
                          #                 browser()
                }) 
          
}
getContour <- function(str, row) { sum(row[names(row) %~% str]) }



# load(file = 'windowCombinations.rData')

interpretWindow <- function(.SD, filter = NULL, sort = NULL, verbose = TRUE) {
          progs <- .SD$Combs[[1]]
          analyses <- .SD$Summaries[[1]]
          wn <- unique(.SD$windows)
          if (wn %len>% 1) stop("Can't interpret multiple windows", call. = FALSE)
          if ( length(analyses) > 1) {
          if (progs %len>% 1L && !is.null(filter)) {
                    filters <- unwrapFormula(filter)
                    sapply(filters, eval, envir = analyses) %>% apply(1, all) -> evaledfilters
                    if (!any(evaledfilters)) return(NULL)
                    progs <- progs[evaledfilters]
                    analyses <- analyses[evaledfilters, ]


          }
          if (progs %len>% 1L && !is.null(sort)) {
                    sorts <- unwrapFormula(sort)    
                    sorts <- lapply(sorts, function(s) call('order', s))
                    
                    lapply(rev(sorts), 
                           function(fil) {
                                     ord <- eval(fil, envir = analyses)
                                     analyses <<- analyses[ord, ]
                                     progs <<- progs[ord]
                           })
                    
                    
          }
}
          
          if (verbose) {
                    cat('\n')
                    cat(' Window ', wn, ':\n', sep = '') 
                    for(i in 1:length(progs)) {
                              cat('------------------------------------\n')
                              cat('\t')
                              cat(progs[[i]]$ChordName, '\n')
                              currow <- analyses[i, 1:6]
                              currow$NCTs <- attr(currow$NCTs, 'fracs')
                              cat('\t\t' %str+% colnames(currow) %str+% ' ' %str+% unlist(currow) %str+% '\n')
                              cat('\n')
                    }
                    invisible(analyses)
          } else { 
                    prog <- progs[[1]]
                   output <- rep(prog$ChordName, prog$Nslices)
                   if (length(output) == 0) output <- rep(NA, sum(Data$windows == wn))
                    # if (length(Data$windows[Data$windows == wn]) != length(output)) browser()
                   output
          }
                    
          
                    
}
interpretWindows <- function(wns = TRUE, filter = NULL, sort = NULL, wcombs = windowCombs) {
          
          wcombs[wns, interpretWindow(.SD, filter = filter, 
                                      sort = sort, verbose = FALSE), by = windows] -> output
          
          # data.frame(Chord = output, stringsAsFactors = FALSE)
          output
          
}


######SORTING

#1 Only accept excape tone/cambiata (leapt from) if no other option
#2 Only accept appagiatura (leapt to) if no other option
#3 Take Delay == 0, unless there is no other option
#4 Take only lowest rank, unless other option
#5 take fewest chords

take1 <- ~ Contour('Esc|App|Ret|Inc|syn|[+]') > 0 ~ Rank ~  NChords

chords1 <- interpretWindows(TRUE, sort = take1)

for (i in seq_along(outs)) {
 # print(i)        
# writechords(i)
  writeLines(trans(fs[[outs[i]]]), fns[i])
# sapply(unique(Data$FileName), writechords)
}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
