#' Features parsed from 572 chorale **kern files, by slice.
#'
#' This dataset contains a large set of useful features parsed
#' from  572 humdrum (**kern) files representing chorales by
#' J.S. Bach and M. Praetorius, organized by slice (records
#' in the original humdrum files).
#'
#' The dataset contains 40 columns of information about each
#' slice in the data.Twenty-three columns (prefix = "Slice_") pertain
#' to the entire slice---for instance, the beat position. The remaining
#' seventeen variables actually contain different information for each musical
#' voice in each slice: these are represented as lists of vectors,
#' with the first element in each vector being the highest voice (e.g., the soprano),
#' and the last being the lowest (e.g., the bass).
#'
#' @seealso \code{\link{ChoraleTable_Notes}}
#'
#' @format A data table with 42,928 rows and 40 columns.
#' \describe{
#'  \item{Note_Accented}{Tonal letter name of note. "-" = flat. "#" = sharp.}
#'  \item{Note_TonalName}{Tonal letter name of note. "-" = flat. "#" = sharp.}
#' }
"ChoraleTable_Slices"

#' Features parsed from 572 chorale **kern files, by note.
#'
#' This dataset contains a large set of useful features parsed
#' from  572 humdrum (**kern) files representing chorales by
#' J.S. Bach and M. Praetorius, organized by individual note.
#'
#' The dataset contains 40 pieces of information about each
#' note position in the data. ("Note position" includes places
#' where a voice rests or sustains a previously attacked note.)
#' Twenty-three columns (prefix = "Slice_") pertain to the entire slice---
#' for instance, the beat position---and are thus duplicated for
#' notes which occur at the same time. The remaining seventeen variables
#' actually contain different information for each note.
#'
#' @seealso \code{\link{ChoraleTable_Slices}}
#'
#' @format A data table with 171,915 rows and 40 columns.
#' \describe{
#'  \item{Notes}{Tonal letter name of note. "-" = flat. "#" = sharp.}
#' }
"ChoraleTable_Notes"

#' Numerous harmonic analyses of the 572 chorales.
#'
#' This data table contains numerous permutational analyses
#' of the 572 chorales. The columns \code{Slice_WindowNumber}
#' and \code{Slice_Number} correspond with the same columns in the
#'  \code{\link{ChoraleTable_Notes}} and \code{\link{ChoraleTable_Slices}}
#' objects. The \code{CT_Note_Number} and \code{NCT_Note_Number} columns
#' contain lists of integers which correspond to the \code{Note_Number} column
#' in the ChoraleTable_* objects.
#'
#' @format A data table with 98,453 rows and 12 columns.
"PermutationalChoraleAnnotations"

#' Numerous harmonic analyses of the 572 chorales.
#'
#'
#' @format A list of length 4 with depth ranging from 1 to 3. All lists
#' contain (at some level) in a vector or list of length 98,453 (the number of permutational
#' analyses).
"ChoraleAnalyses"
