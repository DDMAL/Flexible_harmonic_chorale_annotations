perms <- copy(PermutationalChoraleAnnotations)

contours <- ChoraleTable_Notes$Note_PotentialNonChordToneType %>% unique
ChoraleTable_Notes$Note_PotentialNonChordToneType <- factor(ChoraleTable_Notes$Note_PotentialNonChordToneType, levels = contours)

contourTables <- perms[ , list(CT = list(table(ChoraleTable_Notes$Note_PotentialNonChordToneType[CT_Note_Number[[1]]])),
              NCT = list(table(ChoraleTable_Notes$Note_PotentialNonChordToneType[NCT_Note_Number[[1]]]))), by = .(1:nrow(perms))]



CT.Contour <- as.data.table(do.call('rbind', contourTables$CT))
NCT.Contour <- as.data.table(do.call('rbind', contourTables$NCT))


durTables <- perms[ ,
                    list(CT = list(ChoraleTable_Notes$Note_Duration_Remaining[CT_Note_Number[[1]]]),
                         NCT = list(ChoraleTable_Notes$Note_Duration_Remaining[NCT_Note_Number[[1]]])),
                    by = .(1:nrow(perms))]


attacks <- perms[ ,
                  {
                    list(CT = list(ChoraleTable_Notes$Note_Duration_Remaining[CT_Note_Number[[1]]]),
                         NCT = list(ChoraleTable_Notes$Note_Duration_Remaining[NCT_Note_Number[[1]]])),
                    by = .(1:nrow(perms))]



ChoraleAnalyses <- list(Window = list(Number = perms$Slice_WindowNumber,
                               Duration = sapply(perms$Chord_Duration, sum),
                               Slices = perms$Window_NumberOf_Slices),

                        Chord = list(Count = perms$Window_NumberOf_Chords,
                              Slices = perms$Chord_NumberOf_Slices,
                              Roots     = perms$Root,
                              Qualities = perms$Quality,
                              Durations = perms$Chord_Duration,
                              Inversion = perms$Chord_Inversion,
                              SeventhsResolve = perms$Chord_7thsResolve,
                              CompletionDelay = list(Slices = perms$Chord_CompletionDelay_Slices,
                                                     Durations = perms$Chord_CompletionDelay_Duration)),
                        CTs  = list(Count = lengths(perms$CT_Note_Number),
                                    Contour = CT.Contour,
                                    Durations = lapply(durTables$CT, function(x) x[!is.na(x)])),
                        NCTs = list(Count = lengths(perms$NCT_Note_Number),
                                    Contour = NCT.Contour,
                                    Durations = lapply(durTables$NCT, function(x) x[!is.na(x)]))
)








