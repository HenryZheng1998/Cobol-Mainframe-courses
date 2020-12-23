       identification division.
       program-id. Lab6Demo.

       author. Henry Zheng.
       date-written. 2018-03-02. 
      * Purpose:  To demonstrate the student's ability to use performs 
      *            within performs
       environment division.
       configuration section.

       input-output section.
       file-control.
      * configure input file
           select input-file 
               assign to "../../../Data/lab6.dat"
               organization is line sequential.
      * configure output file
           select output-file
               assign to "../../../Data/lab6.out"
               organization is line sequential.

       data division.
       file section.

      * declare an input record definition 
       fd input-file
           data record is input-line
           record contains 28 characters.

       01 input-line.
           05 il-emp-num               pic xxx.
           05 il-name                  pic x(15).
           05 il-edu-code              pic x.
           05 il-years                 pic 99.
           05 il-cur-salary            pic 9(5)V99.


       fd output-file
           data record is output-line
           record contains 90 characters.
       
       01 output-line                  pic x(90).


       working-storage section.

       01 ws-eof-flag                  pic x
           value "n".  

       01 ws-new-line                  pic x
           value space.

      * Will display lab number and date
       01 ws-name-line.
           05  filler                  pic x(5)
               value "LAB 6".
           05  filler                  pic x(20)
               value spaces.
           05  nl-date                 pic 9(6).
           05  filler                  pic x(5)
               value spaces.
           05  nl-time                 pic 9(8).

      * Displays the file's title and page number
       01 ws-rpt-heading.
           05 filler                   pic x(18)
              value spaces.
           05 filler                   pic x(22)
               value "EMPLOYEE SALARY REPORT".
           05 filler                   pic x(26)
               value spaces.
           05 filler                   pic x(6)
               value "PAGE  ".
           05 ws-page-count            pic 9
               value 1.

      * First heading line
       01 ws-heading-line1.
           05  filler                  pic x(3)
               value "EMP".
           05  filler                  pic xx
               value spaces.
           05  filler                  pic x(3)
               value "EMP".
           05  filler                  pic x(30)
               value spaces.
           05  filler                  pic x(7)
               value "PRESENT".
           05  filler                  pic xx
               value spaces.        
           05  filler                  pic x(8)
               value "INCREASE".
           05  filler                  pic x(6)
               value spaces.
           05  filler                  pic x(3)
               value "PAY".
           05  filler                  pic x(9)
               value spaces.
           05  filler                  pic xxx
               value "NEW".
           05  filler                  pic x(4)
               value spaces.

      * Second heading line
       01 ws-heading-line2.
           05  filler                  pic x(3)
               value "NUM".
           05  filler                  pic xx
               value spaces.
           05  filler                  pic x(4)
               value "NAME".
           05  filler                  pic x(10)
               value spaces.
           05  filler                  pic x(5)
               value "YEARS".
           05  filler                  pic xx
               value spaces.        
           05  filler                  pic x(8)
               value "POSITION".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(6)
               value "SALARY".
           05  filler                  pic x(7)
               value spaces.
           05  filler                  pic x
               value "%".
           05  filler                  pic x(6)
               value spaces.
           05  filler                  pic x(8)
               value "INCREASE".
           05  filler                  pic x(5)
               value spaces.
           05  filler                  pic x(6)
               value "SALARY".

      * Displays the type of employees on that page
       01 ws-sub-emp-l1.
           05  filler                  pic x(15)
               value "EMPLOYEE CLASS:".
           05  filler                  pic x(8)
               value spaces.
           05  filler                  pic x(7)
               value "Analyst".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(8)
               value "Sen Prog".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(4)
               value "Prog".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(7)
               value "Jr Prog".
           05  filler                  pic x(4)
               value spaces.
           05  filler                  pic x(12)
               value "Unclassified".

      * Displays the number of employees of the type on the page
      * Removes leading zeros and accounts for double digits
       01 ws-sub-emp-l2.
           05  filler                  pic x(15)
               value "# ON THIS PAGE:".
           05  filler                  pic x(13)
               value spaces.
           05  ws-analyst-final        pic z9.
           05  filler                  pic x(10)
               value spaces.
           05  ws-sprog-final          pic z9.
           05  filler                  pic x(6)
               value spaces.
           05  ws-prog-final           pic z9.
           05  filler                  pic x(9)
               value spaces.
           05  ws-jrprog-final         pic z9
               value 0.
           05  filler                  pic x(14)
               value spaces.  
           05  ws-unclass-final        pic z9
               value 0.
       
      * Counts the type of employees (Does not remove leading zeros)
       01 ws-analyst-stotal            pic 99
           value 0.
       01 ws-sprog-stotal              pic 99
           value 0.
       01 ws-prog-stotal               pic 99
           value 0.
       01 ws-jrprog-stotal             pic 99
           value 0.
       01 ws-unclass-stotal            pic 99
           value 0.

      * Counts the total type of employee
      * to calculate total average pay increase for each type
       01 ws-analyst-count             pic 99
           value 0.
       01 ws-sprog-count               pic 99
           value 0.
       01 ws-prog-count                pic 99
           value 0.
       01 ws-jrprog-count              pic 99
           value 0.

      * Displays Average pay increase of all class employees
       01 ws-avg-total-line1.
           05  filler                  pic x(18)
               value "AVERAGE INCREASES:".
           05  filler                  pic xxx
               value spaces.
           05  filler                  pic x(8)
               value "ANALYST=".
           05  ws-analyst-avg-final    pic zzz,zz9.99.
           05  filler                  pic x(6)
               value spaces.
           05  filler                  pic x(9)
               value "SEN PROG=".
           05  ws-sprog-avg-final      pic zzz,zz9.99.
 
      * Continuation of average pay increase of all class employees
       01 ws-avg-total-line2.
           05  filler                  pic x(24)
               value spaces.
           05  filler                  pic x(5)
               value "PROG=".
           05  ws-prog-avg-final       pic zzz,zz9.99.
           05  filler                  pic x(7)
               value spaces.
           05  filler                  pic x(8)
               value "JR PROG=".
           05  ws-jrprog-avg-final     pic zzz,zz9.99.

      * Adds up all class type increase pay amount
       01 ws-analyst-inc-total         pic 9(5)V99.
       01 ws-sprog-inc-total           pic 9(5)V99.
       01 ws-prog-inc-total            pic 9(5)V99.
       01 ws-jrprog-inc-total          pic 9(5)V99.

      * Outputs
       01 ws-output.
           05  ol-emp-num              pic xxx.
           05  filler                  pic xx
               value spaces.
           05  ol-name                 pic x(15).
           05  filler                  pic xx
               value spaces.
           05  ol-years                pic z9.
           05  filler                  pic xx
               value spaces.
           05  ol-position             pic x(10).
           05  filler                  pic xx
               value spaces.
           05  ol-cur-salary           pic zz,zzz.99.
           05  filler                  pic xx
               value spaces.
           05  ol-inc-perc             pic z9.9.
      * Only way to blank ol-inc-perc is to move spaces to ws-spaces but I need to make the perc and plus signs a variable
           05  ol-perc-sign            pic x
               value "%".
           05  filler                  pic xx
               value spaces.
           05  ol-pay-inc              pic $$$,$$9.99.
           05  ol-plus-sign            pic x
               value "+".
           05  filler                  pic xx
               value spaces.
           05  ol-new-sal              pic $$$,$$9.99.
           
      * Stores number of lines counted
       01 ws-line-count                pic 99
           value 0.

      * Limits the number of employees per page
       01 ws-emp-limit                 pic 99
           value 14.
       
      * Holds calculation of pay increase * current salary
       01 ws-increase-by-calc          pic 9(5)V99.

      *Constants
       77 ws-increase-analyst-cnst     pic 9V999
           value 0.118.
       77 ws-increase-sprog-cnst       pic 9V999
           value 0.083.
       77 ws-increase-prog-cnst        pic 9V999
           value 0.067.
       77 ws-increase-jrprog-cnst      pic 9V999
           value 0.042.
       77 ws-perc-convert-const        pic 999
           value 100.
       77 ws-perc-sign-cnst            pic x
           value "%".
       77 ws-plus-sign-cnst            pic x
           value "+".

       77 ws-GRADS                     pic x
           value "G".
       77 ws-NON-GRADS                 pic x
           value "N".
       77 ws-class-analyst-cnst        pic x(7)
           value "ANALYST".
       77 ws-class-sprog-cnst          pic x(8)
           value "SEN PROG".
       77 ws-class-prog-cnst           pic x(4)
           value "PROG".
       77 ws-class-jrprog-cnst         pic x(7)
           value "JR PROG".

       77 ws-analyst-GT-cnst           pic 99
           value 15.
       77 ws-sprog-LTE-cnst            pic 99
           value 15.
       77 ws-sprog-GTE-cnst            pic 99
           value 7.
       77 ws-G-prog-GT-cnst            pic 99
           value 2.
       77 ws-G-prog-LT-cnst           pic 99
           value 7.
       77 ws-G-unclass-LTE-cnst           pic 99
           value 2.

       77 ws-N-prog-GT-cnst            pic 99
           value 10.
       77 ws-N-jrprog-LTE-cnst         pic 99
           value 10.
       77 ws-N-jrprog-GT-cnst         pic 99
           value 4.
       77 ws-N-unclass-LTE-cnst         pic 99
           value 4.


       77 ws-inc-one-cnst              pic 9
           value 1.
       77 ws-reset-zero-cnst           pic 9
           value 0.

       procedure division.
           open input input-file,
               output output-file.

           perform 100-heading.
           read input-file
                   at end move "y"     to ws-eof-flag.
           perform until ws-eof-flag equals "y"

      * Clear the output buffer
               move spaces             to ws-output
               perform 200-process-main

      * Move input data to output record and print saleperson's salary
               move il-emp-num         to ol-emp-num
               move il-name            to ol-name
               move il-years           to ol-years
               move il-cur-salary      to ol-cur-salary

               write output-line       from ws-output
               write output-line       from ws-new-line

      * For every 15th employee it will print the headings and subtotals
      * and reset the appropriate counters and subtotals for the next
      * page
               if (ws-line-count >= ws-emp-limit)
                   add ws-inc-one-cnst to ws-page-count
                   perform 300-subtotals
                   perform 310-new-page-head
                   perform 400-reset-count-and-total
               else
                   add ws-inc-one-cnst to ws-line-count
               end-if
      * Read input for the next iteration
               read input-file
                   at end move "y"     to ws-eof-flag  
           end-perform.

      * Displays details at the end of the line
           perform 500-subtotals-and-average-pay.
           close input-file output-file.
           goback.

       100-heading.
           accept nl-date              from date.
           accept nl-time              from time.
           write output-line           from ws-name-line.
           write output-line           from ws-new-line.
           write output-line           from ws-rpt-heading.
           write output-line           from ws-new-line.
           write output-line           from ws-heading-line1.
.          write output-line           from ws-heading-line2.
           write output-line           from ws-new-line.


       200-process-main.
           move spaces                 to ol-position.
           if (il-edu-code = ws-GRADS)
               if (il-years <=  ws-G-unclass-LTE-cnst)
                  perform 250-process-unclass
               end-if
               if (il-years < ws-G-prog-LT-cnst
                   and > ws-G-prog-GT-cnst) then
                   perform 230-process-prog
               end-if
               if (il-years <= ws-sprog-LTE-cnst
                   and >= ws-sprog-GTE-cnst) then
                   perform 220-process-sprog
               end-if
               if (il-years > ws-analyst-GT-cnst) then
                   perform 210-process-analyst
               end-if
           end-if.

           if (il-edu-code = ws-NON-GRADS)
               if (il-years > ws-N-prog-GT-cnst)
                   perform 230-process-prog
               end-if
               if (il-years <= ws-N-jrprog-LTE-cnst
                   and > ws-N-jrprog-GT-cnst)
                   perform 240-process-jrprog
               end-if
               if (il-years <= ws-N-unclass-LTE-cnst)
                   perform 250-process-unclass
               end-if
           end-if.

       210-process-analyst.
           move ws-class-analyst-cnst  to ol-position. 
           compute ol-inc-perc =
               (ws-perc-convert-const * ws-increase-analyst-cnst).
           compute ws-increase-by-calc rounded =
               (ws-increase-analyst-cnst * il-cur-salary).
           move ws-increase-by-calc    to ol-pay-inc.
           add ws-increase-by-calc     to ws-analyst-inc-total.
           compute ol-new-sal = 
             (ws-increase-by-calc + il-cur-salary).
           add ws-inc-one-cnst         to ws-analyst-stotal.
           add ws-inc-one-cnst         to ws-analyst-count.
           perform 260-move-suffix-signs.

       220-process-sprog.
           move ws-class-sprog-cnst    to ol-position.
           compute ol-inc-perc =
               (ws-perc-convert-const * ws-increase-sprog-cnst).
           compute ws-increase-by-calc rounded =
               (ws-increase-sprog-cnst * il-cur-salary).
           move ws-increase-by-calc    to ol-pay-inc.
           add ws-increase-by-calc     to ws-sprog-inc-total.
           compute ol-new-sal = 
             (ws-increase-by-calc + il-cur-salary).
           add ws-inc-one-cnst         to ws-sprog-stotal.
           add ws-inc-one-cnst         to ws-sprog-count.
           perform 260-move-suffix-signs.

       230-process-prog.
           move ws-class-prog-cnst     to ol-position.
           compute ol-inc-perc =
               (ws-perc-convert-const * ws-increase-prog-cnst).
           compute ws-increase-by-calc rounded =
               (ws-increase-prog-cnst * il-cur-salary).
           move ws-increase-by-calc    to ol-pay-inc.
           add ws-increase-by-calc     to ws-prog-inc-total.
           compute ol-new-sal = 
             (ws-increase-by-calc + il-cur-salary).
           add ws-inc-one-cnst         to ws-prog-stotal.
           add ws-inc-one-cnst         to ws-prog-count.
           perform 260-move-suffix-signs.

       240-process-jrprog.
           move ws-class-jrprog-cnst   to ol-position.
           compute ol-inc-perc =
               (ws-perc-convert-const * ws-increase-jrprog-cnst).
           compute ws-increase-by-calc rounded =
               (ws-increase-jrprog-cnst * il-cur-salary).
           move ws-increase-by-calc    to ol-pay-inc.
           add ws-increase-by-calc     to ws-jrprog-inc-total.
           compute ol-new-sal = 
             (ws-increase-by-calc + il-cur-salary).
           add ws-inc-one-cnst         to ws-jrprog-stotal.
           add ws-inc-one-cnst         to ws-jrprog-count.
           perform 260-move-suffix-signs.

       250-process-unclass.
           move spaces                 to ol-position.
           move ws-reset-zero-cnst     to ol-pay-inc.
           move il-cur-salary          to ol-new-sal.
           add ws-inc-one-cnst         to ws-unclass-stotal.

       260-move-suffix-signs.
           move ws-perc-sign-cnst      to ol-perc-sign.
           move ws-plus-sign-cnst      to ol-plus-sign.


       300-subtotals.
           write output-line           from ws-sub-emp-l1.
           move ws-analyst-stotal      to ws-analyst-final.
           move ws-sprog-stotal        to ws-sprog-final.
           move ws-prog-stotal         to ws-prog-final.
           move ws-jrprog-stotal       to ws-jrprog-final.
           move ws-unclass-stotal      to ws-unclass-final.
           write output-line           from ws-sub-emp-l2.
           write output-line           from ws-new-line.

      * Had to split, wouldn't make sense to display heading at the
      * bottom based on how I structured my program

       310-new-page-head.
           write output-line           from ws-rpt-heading.
           write output-line           from ws-new-line.


       400-reset-count-and-total.
           move ws-reset-zero-cnst     to ws-line-count.
           move ws-reset-zero-cnst     to ws-analyst-stotal.
           move ws-reset-zero-cnst     to ws-prog-stotal.
           move ws-reset-zero-cnst     to ws-sprog-stotal.
           move ws-reset-zero-cnst     to ws-jrprog-stotal.
           move ws-reset-zero-cnst     to ws-unclass-stotal.
       
       500-subtotals-and-average-pay.
           perform 300-subtotals
           compute ws-analyst-avg-final rounded =
             (ws-analyst-inc-total / ws-analyst-count).
           compute ws-sprog-avg-final rounded =
             (ws-sprog-inc-total / ws-sprog-count).
           compute ws-prog-avg-final rounded =
             (ws-prog-inc-total / ws-prog-count).
           compute ws-jrprog-avg-final rounded =
             (ws-jrprog-inc-total / ws-jrprog-count).

           write output-line           from ws-avg-total-line1.
           write output-line           from ws-avg-total-line2.

       end program Lab6Demo.