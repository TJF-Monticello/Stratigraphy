This repository contains an R code script with six sections. It prepares data to be composed into a [Harris Matrix](http://harrismatrix.com/), a type of graphic representation that enables users to visualize the stratigraphic sequence of an archaeological site.

**Sections 1-5**
Pull stratigraphic information from the context tables in the DAACS PostgreSQL database and get the information into a form that works for the drawing program ArchEd. There are several data integrity checks that provide feedback on whether stratigraphic relationships have been entered correctly in the database.

One important aspect of using this code is deciding the level of aggregation.  Section 3.5 contains a code block that allows the user to replace individual contexts with their SG, or stratigraphic group.

At the end of Section 5 a file named output.lst is exported that can be used by the ArchEd program to draw the Harris Matrix.

**Section 6**
Section 6 uses the Rgraphviz package to visualize stratigraphic relationships in a different way. This serves as a check for ArchEd, which does not tell you where stratigraphic relationshi perrors are, only that they exist. 
