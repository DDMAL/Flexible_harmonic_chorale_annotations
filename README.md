# Flexible_harmonic_chorale_annotations

This page is a companion to the <a href="http://ismir2018.ircam.fr/doc/pdfs/283_Paper.pdf">paper</a> presented at the <a href="https://www.ismir.net">International Society for Music Information Retrieval</a> <a href="https://ismir2018.ircam.fr">2018 conference</a> in Paris France.

The kernData folder contains raw humdrum-format (\*\*kern) files of the 571 chorales used in the project: 371 by J.S. Bach and 200 by Michael Pr&aelig;torius.
The 371 Bach chorales originated from <a href="http://kern.ccarh.orh">Kern Scores</a>, but some corrections have been made (annotated in reference records) and one chorale (the five-part chorale) was has been added to the set.
The 200 Pr&aelig;torius chorales were newly created for this project, and are not available anywhere else.


# FlexibleChoraleHarmonicAnalysis Rpackage

The R package `FlexibleChoraleHarmonicAnalysis` contains the permutational analysis data for the 571 chorales, as well as several functions for filtering analyses.
The package requires `R > 3.2` as well as the <a href="https://cran.r-project.org/web/packages/data.table/index.html">data.table</a> and <a href="https://cran.r-project.org/web/packages/stringr">stringr</a> packages.
To install, simply download the `tar.gz` file, open R, change the working directory to the directory where you downloaded the tar ball, and run the command `install.packages("FlexibleChoraleHarmonicAnalysis_0.1.0.tar.gz, repos = NULL)`.
The package can then be loaded using the command `library(FlexibleChoraleHarmonicAnalsis)`.


# GUI

The R package API can be used through a GUI <a href="https://natsguitar.github.io/FlexibleChoraleHarmonicAnalysisGUI/">here</a>.

