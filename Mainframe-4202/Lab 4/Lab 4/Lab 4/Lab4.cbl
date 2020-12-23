       identification division.
       program-id. Lab4.
       author. Henry Zheng.
       date-written. 2018-03-02. 
      * Purpose:  To demonstrate the student's ability to use nested if
      *           statements and conditional if statements
       environment division.
       configuration section.

       input-output section.
       file-control.
      * configure input file
           select input-file 
               assign to "../../../Data/lab4.dat"
               organization is line sequential.
      * configure output file
           select output-file
               assign to "../../../Data/lab4.out"
               organization is line sequential.

       data division.
       file section.

      * declare an input record definition 
       fd input-file
           data record is input-line
           record contains 23 characters.

       01 input-line.
           05 il-id                    pic xx.
           05 il-name                  pic x(8).
           05 il-sale                  pic 9(4).
           05 il-rate                  pic 99.
           05 il-min                   pic 999.
           05 il-max                   pic 9(4).


       fd output-file
           data record is output-line
           record contains 59 characters.
       
       01 output-line.
           05 ol-id                    pic xx.
           05 filler                   pic x(3)
               value spaces.
           05 ol-name                  pic x(8).
           05 filler                   pic x(3)
               value spaces.
           05 ol-sale                  pic z,zz9.
           05 filler                   pic x(4)
               value spaces.
           05 ol-min                   pic zz9.
           05 filler                   pic x
               value spaces.
           05 ol-max                   pic z,zz9.
           05 filler                   pic x(2)
               value spaces.
           05 ol-rate                  pic zz9.
           05 ol-perc                  pic x.
           05 filler                   pic x(5)
               value spaces.
           05 ol-earn                  pic zz,zz9.
           05 filler                   pic xx
               value spaces.
           05 ol-paid                  pic $*,**9.

       working-storage section.

       01 ws-eof-flag                  pic x
           value "n".  

       01 new-line                     pic x
           value space.
       
       01  name-line.
           05  filler                  pic x(5)
               value spaces.
           05  filler                  pic x(28)
                   value "Henry Zheng, LAB 4".
           05  filler                  pic x(5)
               value spaces.
           05  nl-date                 pic 9(6).
           05  filler                  pic x(5)
               value spaces.
           05  nl-time                 pic 9(8).

       01  rpt-heading.
           05 filler                   pic x(21).
           05 filler                   pic x(23)
               value "SALES COMMISSION REPORT".

       01  heading-line1.
           05  filler                  pic x(3)
               value "NO.".
           05  filler                  pic x(2)
               value spaces.
           05  filler                  pic x(4)
               value "NAME".
           05  filler                  pic x(7)
               value spaces.
           05  filler                  pic x(5)
               value "SALES".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(3)
               value "MIN".
           05  filler                  pic x(3)
               value spaces.
           05  filler                  pic x(3)
               value "MAX".
           05  filler                  pic x(2)
               value spaces.
           05  filler                  pic x(4)
               value "RATE".
           05  filler                  pic x(5)
               value spaces.
           05  filler                  pic x(6)
               value "EARNED".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(4)
               value "PAID".

       01  underlines.
           05  filler                  pic x(3)
               value "---".
           05  filler                  pic x(2)
               value spaces.
           05  filler                  pic x(4)
               value "----".
           05  filler                  pic x(7)
               value spaces.
           05  filler                  pic x(5)
               value "-----".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(3)
               value "---".
           05  filler                  pic x(3)
               value spaces.
           05  filler                  pic x(3)
               value "---".
           05  filler                  pic x(2)
               value spaces.
           05  filler                  pic x(4)
               value "----".
           05  filler                  pic x(5)
               value spaces.
           05  filler                  pic x(6)
               value "------".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(4)
               value "----".

       01  total-line.                 
           05  filler                  pic x(36)
               value spaces.
           05  filler                  pic x(6)
               value "TOTALS".
           05  filler                  pic xx
               value spaces.
           05  ws-earned-total-final   pic $$$,zzz.
           05  filler                  pic x
               value space.
           05  ws-paid-total-edited    pic $$$,zzz.

       01  num-max-line.
           05  filler                  pic x(32)
               value "NUMBER WITH BONUS MORE THAN MAX".
           05  filler                   pic x(7)
               value spaces.
           05  maxl-num-mor-max        pic x(4).

       01  num-min-line.
           05  filler                  pic x(34)
               value "NUMBER WITH NO BONUS LESS THAN MIN ".
           05  filler                  pic x(5)
               value spaces.
           05  minl-num-less-min       pic x(4).

      * Constants
       01  ws-bonus-const              pic 9(4)
           value 5000.
       01  ws-bonus-perc-const         pic 9V999
           value 0.125.
       01  ws-perc-convert-const       pic 999
           value 100.
       01  ws-count-const              pic 9
           value 1.
       01  ws-sale-per-page            pic 9
           value 4.
       01  perc-sign-const             pic x
           value "%".
       
      * Work storage for calculation earned
       01  ws-earned-calc              pic 9(5).
       01  ws-earned-final             pic zz,zz9.
       01  ws-earned-total-calc        pic 9(6).
      * For Calculation paid
       01  ws-paid-calc                pic 9(5).
       01  ws-paid-final               pic zz,zz9.
       01  ws-paid-total               pic 9(5).

      *Bottom line stuff rename later
       01 ws-bonus-max                 pic 9
           value 0.
       01 ws-nobonus-min               pic 9
           value 0.

       01 ws-line-count                pic 9
           value 0.
       procedure division.
           open input input-file,
                output output-file.
           

      * Write the report heading and name
           accept nl-date              from date.
           accept nl-time              from time.
           write output-line           from name-line.
           write output-line           from rpt-heading.

           perform 100-sale-heading
           read input-file
                   at end move "y"     to ws-eof-flag.
           perform until ws-eof-flag equals "y"
      * Clear the output buffer
               move spaces             to output-line
               perform 200-calculation
      * Move input data to output record and print saleperson's data
               write output-line       from new-line      
               move il-id              to ol-id
               move il-name            to ol-name
               move il-sale            to ol-sale
               move il-min             to ol-min
               move il-max             to ol-max
               move il-rate            to ol-rate
               move perc-sign-const    to ol-perc
               move ws-earned-final    to ol-earn
               move ws-paid-calc       to ol-paid
               write output-line

      * Every 5th saleperson, it will print the sale headings
               if (ws-line-count >= ws-sale-per-page)
                   move 0              to ws-line-count
                   perform 100-sale-heading
               else
                   add 1 to ws-line-count
               end-if

      * Read input for the next iteration
               read input-file
                   at end move "y"     to ws-eof-flag  
           end-perform.
      *    Displays bonuses and totals
           perform 300-bonus-and-totals.
           close input-file output-file.
           goback. 

       100-sale-heading.
           write output-line           from new-line.
           write output-line           from heading-line1.
           write output-line           from underlines.

       200-calculation.
           if (il-sale > ws-bonus-const)
      * Earned heading procedure
               compute ws-earned-calc rounded = 
               (il-sale * (il-rate/ws-perc-convert-const)) +
                      (ws-bonus-perc-const * (il-sale - ws-bonus-const))
               move ws-earned-calc     to ws-earned-final
               add ws-earned-calc      to ws-earned-total-calc
      * Paid heading procedure
                   if (ws-earned-calc > il-max)
                       move il-max         to ws-paid-calc
                       add il-max          to ws-paid-total
                       add ws-count-const  to ws-bonus-max
                   else
                       move ws-earned-calc to ws-paid-calc
                       add ws-earned-calc  to ws-paid-total
                   end-if
           else
      * Earned heading procedure
               compute ws-earned-calc rounded = 
               (il-sale * (il-rate/ws-perc-convert-const))
               move ws-earned-calc     to ws-earned-final
               add ws-earned-calc      to ws-earned-total-calc
      * Paid heading procedure
                   if (ws-earned-calc < il-min)
                       move il-min         to ws-paid-calc
                       add  il-min         to ws-paid-total
                       add  ws-count-const to ws-nobonus-min
                   else
                       move ws-earned-calc to ws-paid-calc
                       add  ws-earned-calc to ws-paid-total
                   end-if
           end-if.
       300-bonus-and-totals.
           move ws-earned-total-calc   to ws-earned-total-final
           move ws-paid-total          to ws-paid-total-edited
           move ws-bonus-max           to maxl-num-mor-max
           move ws-nobonus-min         to minl-num-less-min
           write output-line           from new-line
           write output-line           from new-line
           write output-line           from total-line
           write output-line           from new-line
           write output-line           from new-line
           write output-line           from num-max-line
           write output-line           from new-line
           write output-line           from num-min-line

       end program Lab4.