# Note
## The labs/assignments for this course (and the next course in Mainframe-6202) builds off each other and I didn't save each progress.

# Project Description
## Throughout the two semesters, we were working towards the front end part of a school database system with logic to be able to create, read and edit entries into the database and handle validation.

# File Description
## DCJB8PGM / DCJB8MM
This is the main menu of the program. Here the user can view the options and select them

## DCJB8PGC / DCJB8MC
When selected, user goes to a new map where user can create a new student for the database after input values into the fields

## DCJB8PGR / DCJB8MR
When selected, user goes to a map where user can input a student number to retrieve from the database and display it on the terminal

## DCJB8PGU / DCJB8MC
When selected, user goes to a map where there are two modes, Inquiry mode and Update mode. 
In Inquiry mode, only the student number field is unlocked; user can only input a student number which displays it in the terminal which then switches to Update mode. 
In Update mode, it locks the student number field and unlocks the rest of the field which displays the inputed student number's information. Here the user can edit the fields and save.

## DCJB8PGB / DCJB8PGU / DCJB8MB
When selected, user goes to a map where user can scroll a paginated list of students that displays their student number, student name and phone number. User can then input the line count number which sends them to DCJB8PGU / DCJB8MC in Update mode to edit the information of the student they have selected
