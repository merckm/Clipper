# Clipper
Some old CLIPPER programs

# Intro
This programs were developed by me and Ulrich Schweier in 1987 and 1988
as a migration from ADABAS/NATURAL on IBM 4300 Series to PCs with DBASE and Clipper.

I keep the code here for historical purposes, but nowadays I'm not very proud of it.
All code was developed as a student and formed the basis for rntering a career in programming.

# Changes needed to make it work with HARBOUR 3.0.0
## Add a MAIN function
It was necessary to include a Function declaration to the

## Added ALIAS and SHARED to USE statements
When opening the same database in different workareas, Harbour needs to have different ALIASes defined for these databases. Also to allow writing to Databases, the database must be USEed with the shared optin. 

## Change GO BOT
Harbour does not accept the GO BOT command, which had to be changed to GO BOTTOM