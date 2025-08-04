       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINMENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-CHOICE        PIC 9 VALUE ZEROS.
       01 NEWLINE            PIC X VALUE X"0A".

       PROCEDURE DIVISION.

       *> Main program logic for FinTrust COBOL menu interface
       MAIN-PARAGRAPH.
           DISPLAY "==========================================="
           DISPLAY "      WELCOME TO FINTRUST COBOL             "
           DISPLAY "==========================================="
           DISPLAY " Please select an option:"
           DISPLAY " 1 - Account Management"
           DISPLAY " 2 - View Transactions"
           DISPLAY " 3 - Ledger Summary"
           DISPLAY " 4 - Authenticate User"
           DISPLAY " 9 - Exit"
           DISPLAY NEWLINE
           ACCEPT USER-CHOICE
           EVALUATE USER-CHOICE
               WHEN 1
                   DISPLAY ">> Loading Account Management Module..."
                   PERFORM MOCK-PROGRAM *> Placeholder for future CALL statement
               WHEN 2
                   DISPLAY ">> Loading Transactions Module..."
                   PERFORM MOCK-PROGRAM *> Placeholder for future CALL statement
               WHEN 3
                   DISPLAY ">> Loading Ledger Summary Module..."
                   PERFORM MOCK-PROGRAM *> Placeholder for future CALL statement
               WHEN 4
                   DISPLAY ">> Starting Authentication Flow..."
                   PERFORM MOCK-PROGRAM *> Placeholder for future CALL statement
               WHEN 9
                   DISPLAY ">> Exiting system. Goodbye! :)"
                   STOP RUN
               WHEN OTHER
                   DISPLAY ">> Invalid choice. Please restart."
                   STOP RUN
           END-EVALUATE.
           STOP RUN.

       MOCK-PROGRAM.
           DISPLAY ">> [This feature is in dev mode. ". 
           DISPLAY "     I will replace MOCK-PROGRAM ".
           DISPLAY "     with CALL statements in future modules.]"
           EXIT.
