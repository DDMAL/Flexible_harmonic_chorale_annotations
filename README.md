# Flexible_harmonic_chorale_annotations

This page is a companion to the <a href="http://ismir2018.ircam.fr/doc/pdfs/283_Paper.pdf">paper</a> presented at the <a href="https://www.ismir.net">International Society for Music Information Retrieval</a> <a href="https://ismir2018.ircam.fr">2018 conference</a> in Paris France.

The kernData folder contains raw humdrum-format (\*\*kern) files of the 571 chorales used in the project: 371 by J.S. Bach and 200 by Michael Pr&aelig;torius.
The 371 Bach chorales originated from <a href="http://kern.ccarh.orh">Kern Scores</a>, but some corrections have been made (annotated in reference records, i.e., 184, 194, 246, 329) and one chorale (the five-part chorale, 150) was has been added to the set.
The 200 Pr&aelig;torius chorales were newly created for this project, and are not available anywhere else.


# FlexibleChoraleHarmonicAnalysis Rpackage

The R package `FlexibleChoraleHarmonicAnalysis` contains the permutational analysis data for the 571 chorales, as well as several functions for filtering analyses.
To install it, simply download the `tar.gz` file, open R, change the working directory to the directory where you downloaded the tar ball (`setwd('FILE_PATH')`), 
and run the command `install.packages("FlexibleChoraleHarmonicAnalysis_0.8.0_R_x86_64-pc-linux-gnu.tar.gz" , repos = NULL)`.
You may need to install the following dependencies first, if you don't have them already:
The package requires `R > 3.2` as well as the <a href="https://cran.r-project.org/web/packages/data.table/index.html">data.table</a>,
 <a href="https://cran.r-project.org/web/packages/stringr">stringr</a>,
and <a href="https://cran.r-project.org/web/packages/rlang/index.html">rlang</a> packages. You can install them with `install.packages("data.table")`, `install.packages("stringr")`, and `install.packages("rlang")`, respectively.
The package can then be loaded using the command `library(FlexibleChoraleHarmonicAnalsis)`.


## Documentation

Once you have installed and loaded the library, the usage can be found in the documentation as pdf right here in the file `FlexibleChoraleHarmonicAnalysis_0.8.0.pdf`.
Alternatively, you can call `?FlexibleChoraleHarmonicAnalysis` to get started reading the documentation for the package, from within R.

# GUI


Sorry! The GUI is now defunct.
