

PermutationalChoraleAnnotations[,
                                {
                                  chordslices <- tapply(Slice_Number[[1]],
                                                        sapply(Chord_NumberOf_Slices, function(x) rep(c(letters, LETTERS)[1:length(x)], x)),
                                                        c)

                                  list(list(lapply(chordslices,
                                         function(sn) {
                                           fifths <- ChoraleTable_Notes[Slice_Number %in% sn & Note_Number %in% CT_Note_Number[[1]], Note_CircleOfFifths]
                                           fifths - min(fifths)
                                         }) ))


                                },
                                by = .(1:nrow(PermutationalChoraleAnnotations))]$V1 -> fifths

lapply(fifths,
       function(slices) {
         lapply(slices,
                function(slice) {
                  sort(unique(slice - min(slice)))
                })}) -> normalized

sapply(unlist(normalized, recursive = FALSE),
       function(x) paste(x, collapse = '_')) %>% unique %>% sort -> cats


roots <- list(c('Root'), c('Root', 'Fifth'), c('Third', 'Seventh', 'Root'), c('Third', 'Seventh', 'Root', 'Fifth'),
              c('Root', 'Fifth', 'Third'), c('Root', 'Fifth', 'Third', 'Seventh'), c('Seventh', 'Root', 'Fifth'), c('Seventh', 'Root', 'Fifth', 'Third'),
              c('Seventh', 'Root', 'Third'), c('Third', 'Root'), c('Third', 'Root', 'Fifth'), c('Fifth', 'Third', 'Seventh', 'Root'),
              c('Fifth', 'Third', 'Root'), c('Seventh', 'Fifth', 'Third', 'Root'), c('Seventh', 'Fifth', 'Root'), c('Root', 'Third'),
              c('Root', 'Third', 'Seventh'), c('Fifth', 'Seventh', 'Root'), c('Seventh', 'Third', 'Root'))
names(roots) <- cats


PermutationalChoraleAnnotations[,
                                {
                                  chordslices <- tapply(Slice_Number[[1]],
                                                        sapply(Chord_NumberOf_Slices, function(x) rep(c(letters, LETTERS)[1:length(x)], x)),
                                                        c)

                                  list(list(lapply(chordslices,
                                                   function(sn) {
                                                     ChoraleTable_Notes[Slice_Number %in% sn & Note_Number %in% CT_Note_Number[[1]], Note_Semits]
                                                   }) ))


                                },
                                by = .(1:nrow(PermutationalChoraleAnnotations))]$V1 -> semits


Map(fifths, normalized,
    f= function(fs, ns) {
      Map(fs, ns,
          f = function(f, n) {
          cat <- roots[[paste(n, collapse = '_')]]
          cat[match(f, n)]

      })
    }) -> ChordTones

Map(ChordTones, semits,
    f = function(cts, sts) {
      Map(cts, sts,
          f = function(ct, st) {
            ct[which.min(st)]
          })
    }) -> bassnote

PermutationalChoraleAnnotations$Chord_Inversion <- lapply(bassnote, function(x) match(unlist(x), c('Root', 'Third', 'Fifth', 'Seventh')) - 1)


##

PermutationalChoraleAnnotations[,
                                {
                                  chordslices <- tapply(Slice_Number[[1]],
                                                        sapply(Chord_NumberOf_Slices, function(x) rep(c(letters, LETTERS)[1:length(x)], x)),
                                                        c)
                                  print(chordslices[[1]])
                                  list(list(lapply(chordslices,
                                                   function(sn) {
                                                     ChoraleTable_Notes[Slice_Number %in% sn & Note_Number %in% CT_Note_Number[[1]], Note_VoiceLeading_Departure]
                                                   }) ))


                                },
                                by = .(1:nrow(PermutationalChoraleAnnotations))]$V1 -> vld



Map(ChordTones, vld,
    f = function(cts, vl) {
      Map(cts, vl,
          f = function(ct, v) {
            if (any(ct == 'Seventh')) {
              all(Re(v[cts == 'Seventh'] < 0), na.rm = TRUE)
            } else {
              NA
            }
          })
    }) -> seventhresolve
seventhresolve <- lapply(seventhresolve, function(a) {names(a) <- NULL; a})
PermutationalChoraleAnnotations$Chord_7thsResolve <- lapply(seventhresolve, unlist)



###
PermutationalChoraleAnnotations[,
                                {
                                  chordslices <- tapply(Slice_Number[[1]],
                                                        sapply(Chord_NumberOf_Slices, function(x) rep(c(letters, LETTERS)[1:length(x)], x)),
                                                        c)
                                  print(chordslices[[1]])

                                  list(list(lapply(chordslices,
                                                   function(sn) {
                                                     ChoraleTable_Notes[Slice_Number %in% sn & Note_Number %in% c(CT_Note_Number[[1]], NCT_Note_Number),
                                                                        Note_VoiceLeading_Departure] ->x

                                                   }) ))


                                },
                                by = .(1:nrow(PermutationalChoraleAnnotations))]$V1 -> vld

