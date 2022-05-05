      ******************************************************************
      *PROGRAM : PROJECT 5 MASTER/TRANSACTION PROCESSING             *
      *AUTHOR  :                                                       *
      *DATE    : 05/05/2022                                            *
      *ABSTRACT:                                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOHNSON-P04-MSTR-TRANS. 
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-MST      ASSIGN TO 'p05-mstr.txt'
                                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS         ASSIGN TO 'p05-trans.txt'
                                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RPT-FILE      ASSIGN TO 'p05-report.rpt'
                                ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-MST.
       01  CUST-REC.
           03  CUST-ID                     PIC X(5).
           03  CUST-NAME                   PIC X(20).
           03  CUST-BAL                    PIC 9(5)V99.
       
       FD  TRANS.
       01  TRANS-REC.
           03  TRANS-ID                    PIC 9(5).
           03  TRANS-DATE.
               05  TRANS-YR                PIC 9999.
               05  TRANS-MO                PIC 99.
               05  TRANS-DAY               PIC 99.
           03  TRANS-DESC                  PIC X(20).
           03  TRANS-AMT                   PIC 9(5)V99.
       
       FD  RPT-FILE.  
       01  RPT-REC                         PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       COPY SYS-DATE-TIME-WS.
       01  WS-TITLE-LN.
           03  FILLER                      PIC X(28) 
                     VALUE 'P05-LNAME'.
           03  FILLER                      PIC X(42) 
                     VALUE 'CUSTOMER ACCOUNT REPORT'.
           03  WS-TITLE-DATE               PIC X(10).
           
       01  WS-RPT-BEG-BAL-LN.    
           03  FILLER                      PIC X(2)        VALUE SPACES.
           03  WS-RPT-CUST-ID              PIC X(5).
           03  FILLER                      PIC X           VALUE SPACES.
           03  WS-RPT-CUST-NAME            PIC X(20).
           03  FILLER                      PIC X(33)       VALUE SPACES.
           03  WS-RPT-CUST-BEG-BAL         PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)   VALUE ' BEG BAL'.

       01  WS-RPT-TRANS-LN.
           03  FILLER                      PIC X(20)       VALUE SPACES.
           03  WS-RPT-TRANS-ID             PIC X(5).
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-MO             PIC 99.
           03  FILLER                      PIC X           VALUE '/'.
           03  WS-RPT-TRANS-DAY            PIC 99.
           03  FILLER                      PIC X           VALUE '/'.
           03  WS-RPT-TRANS-YR             PIC 9999.
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-DESC           PIC X(20).
           03  FILLER                      PIC XX          VALUE SPACES.
           03  WS-RPT-TRANS-AMT            PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)       VALUE SPACES.

       01  WS-RPT-END-BAL-LN.
           03  FILLER                      PIC X(61)   VALUE SPACES.
           03  WS-RPT-END-BAL              PIC ZZ,ZZ9.99.
           03  FILLER                      PIC X(10)   VALUE ' END BAL'.

       01  WS-FLAGS.
           03  WS-EOF-FLAG                 PIC X           VALUE 'N'.
               88  EOF-TRANS                               VALUE 'Y'.

       01  WS-MISC-VARS.
           03  WS-CUST-RUNNING-BAL         PIC 9(5)V99     VALUE ZERO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT  CUST-MST
                       TRANS.
           OPEN OUTPUT RPT-FILE.
           PERFORM 300-PRINT-TITLE-LN.
           
           DISPLAY 'Project 5 - Fname Lname'.
           PERFORM 200-PRINT-SAMPLE-OUTPUT 3 TIMES.
           DISPLAY 'End of run'.
           
           CLOSE CUST-MST
                 TRANS
                 RPT-FILE.
           STOP RUN.
      *-----------------------------------------------------------------
       200-PRINT-SAMPLE-OUTPUT.    
           READ    CUST-MST.
           PERFORM 400-WRITE-BEG-BAL-LN.
           
           READ    TRANS.
           PERFORM 500-WRITE-TRANSACTION-LN.
           READ    TRANS.
           PERFORM 500-WRITE-TRANSACTION-LN.
           READ    TRANS.
           PERFORM 500-WRITE-TRANSACTION-LN.
           
           PERFORM 600-WRITE-END-BAL-LN.
      *-----------------------------------------------------------------
       300-PRINT-TITLE-LN.
           COPY  SYS-DATE-TIME-MOVE.
           MOVE  WS-FMTD-DATE TO WS-TITLE-DATE.
           WRITE RPT-REC FROM WS-TITLE-LN.
           WRITE RPT-REC FROM SPACES.
      *-----------------------------------------------------------------
       400-WRITE-BEG-BAL-LN.
           MOVE  CUST-ID               TO   WS-RPT-CUST-ID
           MOVE  CUST-NAME             TO   WS-RPT-CUST-NAME.
           MOVE  CUST-BAL              TO   WS-RPT-CUST-BEG-BAL.
           WRITE RPT-REC               FROM WS-RPT-BEG-BAL-LN.
           MOVE  CUST-BAL              TO   WS-CUST-RUNNING-BAL.
      *-----------------------------------------------------------------
       500-WRITE-TRANSACTION-LN.
           MOVE  TRANS-ID              TO   WS-RPT-TRANS-ID.
           MOVE  TRANS-MO              TO   WS-RPT-TRANS-MO.
           MOVE  TRANS-DAY             TO   WS-RPT-TRANS-DAY.
           MOVE  TRANS-YR              TO   WS-RPT-TRANS-YR.
           MOVE  TRANS-DESC            TO   WS-RPT-TRANS-DESC.
           MOVE  TRANS-AMT             TO   WS-RPT-TRANS-AMT.
           WRITE RPT-REC               FROM WS-RPT-TRANS-LN.
           ADD   TRANS-AMT             TO   WS-CUST-RUNNING-BAL.
      *-----------------------------------------------------------------
       600-WRITE-END-BAL-LN.
           MOVE  WS-CUST-RUNNING-BAL   TO   WS-RPT-END-BAL.
           WRITE RPT-REC               FROM WS-RPT-END-BAL-LN.
           WRITE RPT-REC               FROM SPACES.             
           WRITE RPT-REC               FROM SPACES.             
      *-----------------------------------------------------------------
       