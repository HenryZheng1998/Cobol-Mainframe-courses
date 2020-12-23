       identification division.
       program-id. Lab9CallCenterOperatorReport.
       author. Henry Zheng.
       date-written. 2018-04-25.
      * Demonstrate the student's understanding of arrays and redefines

       environment division.
       input-output section.
       file-control.

           select input-file assign to '../../../data/lab9.data'
               organization is line sequential.

           select report-file assign to '../../../data/lab9.out'
               organization is line sequential.

       data division.
       file section.

       fd input-file 
           data record is emp-rec
           record contains 35 characters.

       01 emp-rec.
           05 emp-rec-num               pic x(3).
           05 emp-rec-name              pic x(12).
           05 emp-rec-calls.
               10 emp-rec-array         pic 999 occurs 6 times.

       fd report-file 
           data record is print-line.

       01 print-line                    pic x(132).

       working-storage section.
       
      *create the necessary working storage variables for your code here
       01 ws-constants.
           05 ws-number-of-months       pic 99   value 6.
           05 ws-zero                   pic x(4) value "ZERO".
           05 ws-no-calls-total         pic 999  value 000.
           05 ws-no-calls-month         pic 9    value 0.
           05 ws-increment              pic 9    value 1.
                                        
       01 ws-array-iter                 pic 99   value 1.
                                        
       01 ws-count-month                pic 99   value 0.
       01 ws-count-calls                pic 9(4) value 0.
       01 ws-op-avg                     pic zz9  value 0.
       01 ws-op-rem                     pic 9    value 0.
       
       01 ws-calculated-fields.
           05 ws-non-zero-month-count   pic 9(2) value 0.
           
       
       01 found-eof                     pic x value 'n'.
           88 is-end-of-file                  value "y".

       01 ws-totals.
           05 ws-grand-total            pic 9(5) value 0.
           05 ws-emp-total              pic 9(4) value 0.
           05 ws-total-no-calls         pic 9(2) value 0.

       01 name-line.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(29)
                                        value 'Henry Zheng, lab 9'.
           05 filler                    pic x(5)  value spaces.
           05 name-line-date            pic 9(6).
           05 filler                    pic x(5)  value spaces.
           05 name-line-time            pic 9(8).

       01 report-heading.
           05 filler                    pic x(20).
           05 filler                    pic x(39)
                        value 'call centre volumes for july - december'.

       01 heading-line1.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(8) value 'operator'.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(8) value 'operator'.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(3) value 'jul'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'aug'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'sep'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'oct'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'nov'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'dec'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(5) value 'total'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'avg'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'rem'.

       01 heading-line2.
           05 filler                    pic x(5) value spaces.
           05 filler                    pic x(1) value '#'.
           05 filler                    pic x(8) value spaces.
           05 filler                    pic x(4) value 'name'.

       01 detail-line.
           05 filler                    pic x(4) value spaces.
           05 detail-line-num           pic x(3).
           05 filler                    pic x(6) value spaces.
           05 detail-line-name          pic x(12).
           05 filler                    pic x(1) value spaces.
           05 detail-line-months        pic x(7) occurs 6 times.   
           05 filler                    pic x(1) value spaces.
           05 detail-line-total         pic zz9.
           05 filler                    pic x(4) value spaces.
           05 detail-line-avg           pic zzz9.
           05 detail-line-avg-text      redefines detail-line-avg
                                        pic x(4).
           05 filler                    pic x(4) value spaces.
           05 detail-line-rem           pic 9.
           05 detail-line-rem-text      redefines detail-line-rem
                                        pic x.

       01 monthly-total.
           05 filler                    pic x(21)
                            value "Total number of calls".
       01 total-line1.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(35)
                            value "number of operators with no calls: ".
           05 total-line-no-calls       pic z9.

       01 total-line2.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(20)
                                           value "overall total calls:".
           05 filler                    pic x(15) value spaces.
           05 total-line-calls          pic zzz99.

       procedure division.
           *> open file handles
           open input input-file,
                output report-file.

           *> grab the current date & time
           accept name-line-date from date.
           accept name-line-time from time.

           *> output heading
           perform 000-print-headings.

           *> process input file & output results
           perform 100-read-input-file.
           perform 200-process-records until is-end-of-file.

           *> output total lines
           perform 500-print-totals.

           *> close file handles
           close input-file
                 report-file.
                 
           stop run.

       000-print-headings.
           write print-line from name-line before advancing 1 line.

           write print-line from report-heading after advancing 1 line.

           write print-line from heading-line1 after advancing 2 lines.
           write print-line from heading-line2 after advancing 1 line.


       100-read-input-file.
           *> reads a line from input file & stores it in emp-rec
           *> - unless eof is encountered in which case it sets found-eof to y
           read input-file at end move 'y' to found-eof.

       200-process-records.
      **>  TODO: iterate through monthly calls table to find total
      *          and how many months were non-zero. 

           perform 
               varying ws-array-iter
               from 1 by 1
               until ws-array-iter > ws-number-of-months

               if (emp-rec-array(ws-array-iter) not = ws-no-calls-month)
                   add 1 to ws-count-month
               end-if

               add emp-rec-array(ws-array-iter)
                   to ws-emp-total

               add emp-rec-array(ws-array-iter)
                   to ws-count-calls
               
               add emp-rec-array(ws-array-iter)
                   to ws-grand-total

               move emp-rec-array(ws-array-iter)
                   to detail-line-months(ws-array-iter)

           end-perform.
           
           
      **>  TODO: Implement average calculation logic as outlined in the
      *          the requirments
           
           move emp-rec-num             to detail-line-num.
           move emp-rec-name            to detail-line-name.
           move ws-emp-total            to detail-line-total.

           if (ws-emp-total equals ws-no-calls-total) then 
               move ws-zero             to detail-line-avg-text
               move spaces              to detail-line-rem-text
               add ws-increment         to ws-total-no-calls
           else
               divide ws-count-calls by ws-count-month
                 giving ws-op-avg remainder ws-op-rem
               move ws-op-avg           to detail-line-avg
               move ws-op-rem           to detail-line-rem
           end-if.

           *> print detail line
           write print-line from detail-line
           after advancing 2 lines.

           *> reset field for next record
           perform 250-reset-field
           *> read next record (if any)
           perform 100-read-input-file.

       250-reset-field.
           move 0                       to ws-emp-total.
           move 0                       to ws-non-zero-month-count.
           move 0                       to ws-emp-total.
           move 0                       to ws-count-calls.
           move 0                       to ws-count-month.

       500-print-totals.
           move ws-total-no-calls       to total-line-no-calls.
           move ws-grand-total          to total-line-calls.
           
           write print-line from total-line1
               after advancing 2 lines.
           write print-line from total-line2
               after advancing 2 lines.

       end program Lab9CallCenterOperatorReport.