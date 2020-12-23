       identification division.
       program-id. Lab_1.
       author. Henry Zheng.
       date-written. 2018-01-28. 
      * Purpose: Display formatted contact list with our names, emails,
      *          phone numbers and two other students.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 ws-accept-message                pic x(10).
       01 ws-title-info.
           05 filler                       pic x(20) value spaces.
           05 ws-title                     pic x(22) 
               value "MAFD 4202 Contact List". 
       01 ws-contact-info.
           05 ws-full-name                 pic x(16).
           05 ws-email                     pic x(33).
           05 ws-phone.
               10 ws-areacode              pic x(3).
               10 ws-dash                  pic x     value "-".
               10 ws-number                pic x(8).

      *Typically you would reuse groups
      *Don't overcomplicate this assignment, lots of flexibility on this
       procedure division.
       000-MAIN-PROCEDURE.
           display ws-title-info.
           display " ".
           move    spaces                  to ws-full-name.
           move    "Henry Zheng"           to ws-full-name.
           move    "Henry.Zheng@dcmail.ca" to ws-email.
           move    "437"                   to ws-areacode.
           move    "123-4567"              to ws-number.

           display ws-contact-info.

           move    "Hasan Syed"            to ws-full-name.
           move    "Hasan.Syed@dcmail.ca"  to ws-email.
           move    "905"                   to ws-areacode.
           move    "234-5678"              to ws-number.

           display ws-contact-info.

           move    "John Doe"              to ws-full-name.
           move    "JohnDoe@gmail.com"     to ws-email.
           move    "456"                   to ws-areacode.
           move    "316-2548"              to ws-number.

           display ws-contact-info.

           accept ws-accept-message.
           goback.

       end program Lab_1.