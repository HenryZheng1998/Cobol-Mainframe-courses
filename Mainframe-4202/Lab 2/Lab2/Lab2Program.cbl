       identification division.
       program-id. Lab2Program.
       author. Henry Zheng.
       date-written. 2018-02-08. 
      * Purpose: To input a file (lab2.dat) and output the lab2.out  
      *          specified to what the lab2.doc file wanted.

       environment division.
       configuration section.

       input-output section.
       file-control.
      * configure input file
           select input-file 
               assign to "../../../Data/lab2.dat"
               organization is line sequential.
      * configure output file
           select output-file
               assign to "../../../Data/lab2.out"
               organization is line sequential.

       data division.
       file section. 

      *declare an input record definition
       fd input-file 
           data record is input-line
           record contains 35 characters.

       01 input-line.
           05 il-student-number            pic x(9).
           05 il-student-name              pic x(20).
           05 il-student-enroll-year       pic 9(4).
           05 il-student-year              pic 9(2).

       fd output-file
           data record is output-line
           record contains 65 characters.

       01 output-line.
           05 ol-student-number            pic x(9).
           05 filler                       pic x(9).
           05 ol-student-name              pic x(20).
           05 filler                       pic x(10).
           05 ol-student-enroll-year       pic 9(4).
           05 filler                       pic x(6).
           05 ol-student-years             pic z9.
           05 filler                       pic x(5).

       working-storage section.
       01 ws-current-year                  pic 9(4)
           value 2018.

       01 ws-eof-flag                      pic x
           value "n".

       01 ws-myName.
           05 filler                       pic x(71)
               value spaces.
           05 filler                       pic x(18)
               value "Henry Zheng, Lab 3".
           05 filler                       pic x
               value space.
       01 ws-heading.
           05 filler                       pic x(26)
               value "Student ID+------+Name----".
           05 filler                       pic x(13)
               value "------------+".
           05 filler                       pic x(26)
               value "--------+-----Enrolled---+".

       01 ws-description.
           05 filler                       pic x(48)
               value spaces.
           05 filler                       pic x(8)
               value "Starting".
           05 filler                       pic x(3)
               value spaces.
           05 filler                       pic x(5)
               value "Years".

       procedure division.
           open input input-file.
           open output output-file.
      
      * Write the report heading
           write output-line               from ws-myName.
           write output-line               from ws-heading.
           write output-line               from ws-description.
           read input-file 
               at end move "y"             to ws-eof-flag.

           perform until ws-eof-flag equals "y"
      * Clear the output buffer
               move spaces                 to output-line
      * Move input data to ouptut record and print
               write output-line
               move il-student-name        to ol-student-name
               move il-student-number      to ol-student-number
               move il-student-enroll-year to ol-student-enroll-year
      *Calculations
               subtract ws-current-year    from il-student-enroll-year 
                   giving il-student-year
               move il-student-year        to ol-student-years
               write output-line
      * Read input for the next iteration
               read input-file
                   at end move "y"         to ws-eof-flag
           end-perform.

           close input-file output-file.
           goback.

       end program Lab2Program.