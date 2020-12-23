       identification division.
       program-id. Lab3Program.
       author. Henry Zheng.
       date-written. 2018-02-08. 
      * Purpose:  To demonstrate the student's ability to use if
      *           statements and arithmetic operations and headaches

       environment division.
       configuration section.

       input-output section.
       file-control.
      * configure input file
           select input-file 
               assign to "../../../Data/lab3.dat"
               organization is line sequential.
      * configure output file
           select output-file
               assign to "../../../Data/lab3.out"
               organization is line sequential.
       

       data division.
       file section.

      * declare an input record definition
       fd input-file
           data record is input-line
           record contains 27 characters.

       01 input-line.
           05 il-item-number           pic x(4).
           05 il-product-class         pic x.
           05 il-item-description      pic x(13).
           05 il-quantity              pic 9(3). 
           05 il-price-per-unit        pic 9(4)V99.

       fd output-file
           data record is output-line
           record contains 90 characters.

       01 output-line.
           05 filler                   pic x
               value spaces.   
           05 ol-item-description      pic x(15).
           05 ol-extended-prices       pic x(15).
           05 ol-discount              pic x(15).
           05 ol-net-price             pic x(16).
           05 ol-class                 pic A.
           05 filler                   pic x(5)
               value spaces.
           05 ol-trans-percent         pic 99.9.
           05 filler                   pic x(5)
               value spaces.
           05 ol-trans-charge          pic x(16).


       working-storage section.

       01 ws-eof-flag                      pic x
           value "n".  

       01 ws-new-line                      pic x
           value space.

       01 ws-myName.
           05 filler                       pic x(71)
               value spaces.
           05 filler                       pic x(18)
               value "Henry Zheng, Lab 3".
           05 filler                       pic x
               value space.               

       01 ws-heading-2.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(4)
               value "ITEM".
           05 filler                       pic x(15)
               value spaces.
           05 filler                       pic x(8)
               value "EXTENDED".
           05 filler                       pic x(8)
               value spaces.
           05 filler                       pic x(8)
               value "DISCOUNT".
           05 filler                       pic x(6)
               value spaces.
           05 filler                       pic x(9)
               value "NET PRICE".
           05 filler                       pic xx
               value spaces.
           05 filler                       pic x(5)
               value "CLASS".
           05 filler                       pic xx
               value spaces.
           05 filler                       pic x(5)
               value "TRANS".
           05 filler                       pic xxx
               value spaces.
           05 filler                       pic x(14)
               value "TRANSPORTATION".

       01 ws-heading-3.
           05 filler                       pic x(22)
               value spaces.
           05 filler                       pic x(5)
               value "PRICE".
           05 filler                       pic x(10)
               value spaces.
           05 filler                       pic x(6)
               value "AMOUNT".
           05 filler                       pic x(26)
               value spaces.
           05 filler                       pic x
               value "%".
           05 filler                       pic x(10)
               value spaces.
           05 filler                       pic x(6)
               value "CHARGE".
           05 filler                       pic x(5)
               value spaces.

       01 ws-tailing-1.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(28)
               value "ITEMS WITH OUT A DISCOUNT = ".
           05 ws-no-discount-count-final   pic ZZ9.9.
           05 filler                       pic x
               value "%".
           

      * Calculation Variables
       01 ws-extended-prices-calc          pic 9(8)v9(6).
       01 ws-extended-prices-edited        pic zz,zzz,zz9.99.

       01 ws-discount-calc                 pic 9(8)v9(6).
       01 ws-discount-edited               pic zz,zzz,zz9.99.

       01 ws-net-calc                      pic 9(8)v9(6).
       01 ws-net-edited                    pic zz,zzz,zz9.99.

       01 ws-discount                      pic 9V99
           value 0.05.
       01 ws-no-discount                   pic 9
           value 0.

       01 ws-trans-a                       pic 99V999
           value 11.5.
       01 ws-trans-a-percent               pic 99V999
           value 0.115.

       01 ws-trans-b                       pic 99V999
           value 3.5.
       01 ws-trans-b-percent               pic 99V999
           value 0.035.

       01 ws-trans-other                   pic 99V999
           value 8.5.
       01 ws-trans-other-percent           pic 99V999
           value 0.085.        
       01 ws-trans-calc                    pic 9(8)V99.
       01 ws-trans-percent                 pic 9(8)v9(6).
       01 ws-trans-charge-edited           pic zz,zzz,zz9.99.

       01 ws-trans-checker                 pic 9
           value 0.
       01 ws-trans-adder                   pic 99
           value 65.

       01 ws-extended-total-calc           pic 9(8)v9(6).
       01 ws-net-total-calc                pic 9(8)v9(6).
       01 ws-trans-total-calc              pic 9(8)v9(6).

      
       01 ws-discount-counter              pic 9(8)v9(6).
       01 ws-no-discount-counter           pic 9(8)v9(6).
       01 ws-discount-count                pic 9(8)v9(6).
       01 ws-discount-count-calc           pic 9(8)v9(6).

       01 ws-total-line.
           05 filler                       pic x(15)
               value spaces.
           05 ws-extended-total-final      pic $$$,$$$,$$9.99.
           05 filler                       pic x(16)
               value spaces.
           05 ws-net-total-final           pic $$$,$$$,$$9.99.
           05 filler                       pic x(17)
               value spaces.
           05 ws-trans-total-final         pic $$$,$$$,$$9.99.

       
       procedure division.
           open input input-file.
           open output output-file.
      
      * Write the report heading
           write output-line               from ws-myName.
           write output-line               from ws-new-line.
           write output-line               from ws-new-line.
           write output-line               from ws-heading-2.
           write output-line               from ws-heading-3
           write output-line               from ws-new-line.
           write output-line               from ws-new-line.
           read input-file 
               at end move "y"             to ws-eof-flag.

           perform until ws-eof-flag equals "y"
      * Clear the output buffer
               move spaces                 to output-line
      * Calculations
               Multiply    il-quantity
                   by      il-price-per-unit
                   giving  ws-extended-prices-calc
               move ws-extended-prices-calc
                                           to
                                           ws-extended-prices-edited
               move ws-extended-prices-edited  to ol-extended-prices
      * Discount ammount procedure
               if (il-product-class = 'A' and
                   ws-extended-prices-calc greater than 19900)
                   add 1 to ws-discount-counter
                   multiply ws-discount 
                       by   ws-extended-prices-calc
                   giving   ws-discount-calc

                   move ws-discount-calc       to ws-discount-edited
               else
                   move ws-no-discount         to ws-discount-calc
                   move ws-no-discount         to ws-discount-edited
               end-if
                
               if (il-product-class = 'D'
                              and il-quantity greater than 10)
                   add 1 to ws-discount-counter
                   multiply   ws-discount
                       by     ws-extended-prices-calc
                       giving ws-discount-calc
                   move ws-discount-calc       to ws-discount-edited
               end-if

               if (il-product-class not = 'A' or not ='D' and
                   not  ws-extended-prices-calc greater than 1990 and
                   not  il-quantity greater than 10)

                   add 1                       to ws-no-discount-counter
               end-if
      * Net Price procedure
               Subtract ws-extended-prices-calc    from ws-discount-calc
                                                   giving ws-net-calc
               move ws-net-calc                    to ws-net-edited
               add ws-net-calc                     to
                                               ws-extended-total-calc
               add ws-net-calc                     to
                                                   ws-net-total-calc
      * Transportation charges procedure
      * Set default value
               move 000 to ol-trans-percent
               move 000 to ws-trans-percent

               if (il-product-class = 'A')
                   move ws-trans-a                 to ol-trans-percent
                   move ws-trans-a-percent         to ws-trans-percent  

               end-if

               if (il-product-class = 'B')
                   move ws-trans-b                 to ol-trans-percent
                   move ws-trans-b-percent         to ws-trans-percent
               end-if

               if (il-product-class not = 'A'and not = 'B'
                   and il-quantity less than 144)
                   move ws-trans-other             to ol-trans-percent
                   move ws-trans-other-percent     to ws-trans-percent
               end-if

      * Transportation Calculations
               Multiply   ws-extended-prices-calc 
                   by     ws-trans-percent  
                   giving ws-trans-calc rounded
               if (ws-trans-percent = ws-trans-checker) 
                   add ws-trans-adder
                                                   to ws-trans-calc
               end-if
               move ws-trans-calc                  to
                                               ws-trans-charge-edited
               add ws-trans-calc                  to
                                                   ws-trans-total-calc
       
               display ws-trans-calc
      * Move input data to output record and print
               move il-item-description            
                                               to ol-item-description
               move ws-discount-edited         to ol-discount
               move ws-net-edited              to ol-net-price
               move il-product-class           to ol-class
               move ws-trans-charge-edited     to ol-trans-charge
               write output-line
               write output-line               from ws-new-line

      * Read input for the next iteration
               read input-file
                   at end move "y"             to ws-eof-flag

      * Final Calculations
               move ws-extended-total-calc     to 
                                               ws-extended-total-final
               move ws-net-total-calc          to
                                               ws-net-total-final
               move ws-trans-total-calc        to
                                               ws-trans-total-final
               write output-line               from 
                                               ws-total-line
               write output-line               from ws-new-line
               write output-line               from ws-new-line
      * Average calculation
               add ws-discount-counter         to ws-discount-count
               add ws-no-discount-counter      to ws-discount-count

               divide   ws-discount-counter
                 by     ws-discount-count
                 giving ws-discount-count-calc
               
               multiply ws-discount-count-calc
                 by     100
                 giving ws-discount-count-calc

               move ws-discount-count-calc     to
                                           ws-no-discount-count-final
               write output-line               from ws-tailing-1
           end-perform.

           close input-file output-file.
           goback.

       end program Lab3Program.