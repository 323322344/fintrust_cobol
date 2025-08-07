       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINMENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-CHOICE        PIC 9 VALUE ZEROS.

       PROCEDURE DIVISION.

       *> Main program logic for FinTrust COBOL menu interface
       MAIN-PARAGRAPH.
           PERFORM UNTIL USER-CHOICE = 9
               DISPLAY "==========================================="
               DISPLAY "      WELCOME TO FINTRUST COBOL             "
               DISPLAY "==========================================="
               DISPLAY " Please select an option:"
               DISPLAY " 1 - Account Management"
               DISPLAY " 2 - View Transactions"
               DISPLAY " 3 - Ledger Summary"
               DISPLAY " 4 - Authenticate User"
               DISPLAY " 9 - Exit"
               ACCEPT USER-CHOICE

               EVALUATE USER-CHOICE
                   WHEN 1
                       PERFORM ACCOUNT-MANAGEMENT *> Placeholder for future CALL statement
                   WHEN 2
                       PERFORM VIEW-TRANSACTIONS *> Placeholder for future CALL statement
                   WHEN 3
                       PERFORM LEDGER-SUMMARY *> Placeholder for future CALL statement
                   WHEN 4
                       PERFORM AUTHENTICATE-USER *> Placeholder for future CALL statement
                   WHEN 9
                       DISPLAY ">> Exiting FinTrust COBOL. Goodbye! :)"
                   WHEN OTHER
                       DISPLAY ">> Invalid choice. Please restart."
               END-EVALUATE
           END-PERFORM.

       ACCOUNT-MANAGEMENT.
           DISPLAY ">> Loading Account Management Module..."           
           DISPLAY ">> [This feature is in dev mode.".
           DISPLAY "     I will replace MOCK-PROGRAM".
           DISPLAY "     with CALL statements in future modules.]".

       VIEW-TRANSACTIONS.
           DISPLAY ">> Loading View Transactions Module..."
           DISPLAY ">> [This feature is in dev mode.". 
           DISPLAY "     I will replace MOCK-PROGRAM".
           DISPLAY "     with CALL statements in future modules.]".

       LEDGER-SUMMARY.
           DISPLAY ">> Loading Ledger Summary Module..."
           DISPLAY ">> [This feature is in dev mode.".
           DISPLAY "     I will replace MOCK-PROGRAM".
           DISPLAY "     with CALL statements in future modules.]".

       AUTHENTICATE-USER.
           DISPLAY ">> Loading Authenticate User Module..."
           DISPLAY ">> [This feature is in dev mode.".
           DISPLAY "     I will replace MOCK-PROGRAM".
           DISPLAY "     with CALL statements in future modules.]".
