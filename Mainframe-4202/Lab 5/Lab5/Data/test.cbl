       identification division.
       program-id. test.

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
               assign to "../../../Data/lab5.dat"
               organization is line sequential.
      * configure output file
           select output-file
               assign to "../../../Data/lab5.out"
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
           record contains 80 characters.
       
       01 output-line                  pic x(80).


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
           05  filler                  pic xxx
               value spaces.
           05  filler                  pic x(8)
               value "COMMENTS".

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
           05  filler                  pic xxx
               value spaces.
           05  filler                  pic x(8)
               value "--------".

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
           05  ws-comments             pic x(16).

       01  num-max-line.
           05  filler                  pic x(41)
               value "NUMBER WITH BONUS & PAID THE MAXIMUM    :".
           05  filler                  pic x(3)
               value spaces.
           05  maxl-num-mor-max        pic x(4).

       01  num-min-line.
           05  filler                  pic x(41)
               value "NUMBER WITH NO BONUS & PAID THE MINIMUM :".
           05  filler                  pic x(3)
               value spaces.
           05  minl-num-less-min       pic x(4).

       01  ws-output.
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
           05 filler                   pic xxx
               value spaces.
           05 ol-comments              pic x(16).

      * Percentage of people paid what they earned
       01  ws-total-emp                pic 99.
       01  ws-paid-eql-earn            pic 99.
       01  ws-earn-perc-calc           pic 99.
       01  ws-earn-perc-final          pic x(4).
      * Constants
       77  ws-bonus-const              pic 9(4)
           value 5000.
       77  ws-bonus-perc-const         pic 9V999
           value 0.125.
       77  ws-perc-convert-const       pic 999
           value 100.
       77  ws-count-const              pic 9
           value 1.
       77  ws-sale-per-page            pic 9
           value 9.
       77  perc-sign-const             pic x
           value "%".
       77  ws-max-earned-const         pic x(15)
           value "EARNED OVER MAX".
       77 ws-min-earned-const          pic x(16)
           value "EARNED UNDER MIN".
       
      * Work storage for calculation earned
       01  ws-earned-calc              pic 9(5).
       01  ws-earned-final             pic zz,zz9.
       01  ws-earned-total-calc        pic 9(6).
      * For Calculation paid
       01  ws-paid-calc                pic 9(5).
       01  ws-paid-final               pic zz,zz9.
       01  ws-paid-total               pic 9(5).

      *Bottom line stuff rename later
       01 ws-bonus-max                 pic 99
           value 0.
       01 ws-nobonus-min               pic 99
           value 0.

       01 ws-line-count                pic 99
           value 0.

       procedure division.
           open input input-file,
                output output-file.
           
           perform 100-sale-heading
           read input-file
                   at end move "y"     to ws-eof-flag.
           perform until ws-eof-flag equals "y"
      * Clear the output buffer
               move spaces             to output-line
               perform 200-calculation-totals
      * Split the 200-calculation-total into what lab 5 wants
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
               move ws-comments        to ol-comments
               write output-line       from ws-output

      * Every 10th saleperson, it will print the sale headings
      *        move spaces to output-line
      *        write output-line from ws-
      *        perform 200-calculation-totals
      *            varying ws-line-count
      *            from 1 by 1
      *            until ( ws-eof-flag = "Y"
      *                OR ws-line-count > 10)
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
           accept nl-date              from date.
           accept nl-time              from time.
           write output-line           from new-line.
           write output-line           from name-line.
           write output-line           from new-line.
           write output-line           from rpt-heading.
           write output-line           from new-line.
           write output-line           from heading-line1.
           write output-line           from underlines.

       200-calculation-totals.
           move spaces                 to ws-comments
           if (il-sale > ws-bonus-const)
      * Earned heading procedure
               perform 210-greater-than-5000
      * Paid heading procedure
                   if (ws-earned-calc > il-max)
                       perform 220-over-maximum
                   else
                       perform 225-not-maximum-or-minimum
                   end-if
           else
      * Earned heading procedure
               perform 230-less-than-5000
      * Paid heading procedure
                   if (ws-earned-calc < il-min)
                       perform 235-under-minimum
                   else
                       perform 225-not-maximum-or-minimum
                   end-if
           end-if
           perform 400-paid-equal-earn.

       210-greater-than-5000.
           compute ws-earned-calc rounded = 
               (il-sale * (il-rate/ws-perc-convert-const)) +
                      (ws-bonus-perc-const * (il-sale - ws-bonus-const))               
           move ws-earned-calc     to ws-earned-final
           add ws-earned-calc      to ws-earned-total-calc.

       220-over-maximum.
           move il-max         to ws-paid-calc
           add il-max          to ws-paid-total
           add ws-count-const  to ws-bonus-max
           move ws-max-earned-const to ws-comments.

       225-not-maximum-or-minimum.
           move ws-earned-calc to ws-paid-calc
           add ws-earned-calc  to ws-paid-total.

       230-less-than-5000.
           compute ws-earned-calc rounded = 
           (il-sale * (il-rate/ws-perc-convert-const))
           move ws-earned-calc     to ws-earned-final
           add ws-earned-calc      to ws-earned-total-calc.

       235-under-minimum.
           move il-min         to ws-paid-calc
           add  il-min         to ws-paid-total
           add  ws-count-const to ws-nobonus-min
           move ws-min-earned-const to ws-comments.

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

           compute ws-earn-perc-calc =
             (ws-paid-eql-earn / ws-total-emp)
           move ws-earn-perc-calc    to ws-earn-perc-final           
           display ws-earn-perc-final
           display ws-total-emp
           display ws-earn-perc-calc.

       400-paid-equal-earn.
           if (ws-earned-calc = ws-paid-calc) then
               add ws-count-const          to ws-paid-eql-earn
               add ws-count-const          to ws-total-emp
           else
               add ws-count-const          to ws-total-emp
           end-if.
       end program test.