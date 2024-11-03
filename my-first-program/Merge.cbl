       ******************************************************************
       * PROGRAM: FILE-JOIN-CALCULATION
       * AUTHOR: [Hexaware]
       * DATE:   [03/11/24]
       * PURPOSE: To join two files on SECNR key, perform calculations, 
       *          and access IMS database records.
       ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-JOIN-CALCULATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO 'file1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE2 ASSIGN TO 'file2.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'outputfile.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD  FILE1.
       01  FILE1-RECORD.
           05 FILE1-SECNR         PIC X(10).
           05 FILE1-AMOUNT        PIC 9(7)V99.

       FD  FILE2.
       01  FILE2-RECORD.
           05 FILE2-SECNR         PIC X(10).
           05 FILE2-AMOUNT        PIC 9(7)V99.

       FD  OUT-FILE.
       01  OUT-RECORD.
           05 OUT-SECNR           PIC X(10).
           05 OUT-AMOUNT1         PIC 9(7)V99.
           05 OUT-AMOUNT2         PIC 9(7)V99.
           05 OUT-TOTAL           PIC 9(9)V99.

       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05 WS-RECORD-COUNT     PIC 9(6) VALUE 0.
           05 WS-ERROR-COUNT       PIC 9(6) VALUE 0.
           05 WS-TOTAL-AMOUNT      PIC 9(9)V99 VALUE 0.
           05 WS-EOF1-FLAG         PIC X VALUE 'N'.
           05 WS-EOF2-FLAG         PIC X VALUE 'N'.

       01  WS-KEY-FIELDS.
           05 WS-SECNR            PIC X(10).

       01  IMS-STATUS             PIC X(2).
       01  IMS-RESPONSE-AREA.
           05 IMS-SECNR           PIC X(10).
           05 IMS-OTHER-DATA      PIC X(20).
           05 IMS-AMOUNT          PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-PARA
           PERFORM PROCESS-FILES
           PERFORM CLOSE-FILES
           STOP RUN.

       INITIALIZE-PARA.
           OPEN INPUT FILE1
           OPEN INPUT FILE2
           OPEN OUTPUT OUT-FILE
           MOVE SPACES TO OUT-RECORD.

       PROCESS-FILES.
           READ FILE1 INTO FILE1-RECORD
               AT END
                   MOVE 'Y' TO WS-EOF1-FLAG
           END-READ
           
           READ FILE2 INTO FILE2-RECORD
               AT END
                   MOVE 'Y' TO WS-EOF2-FLAG
           END-READ

           PERFORM UNTIL WS-EOF1-FLAG = 'Y' AND WS-EOF2-FLAG = 'Y'
               IF WS-EOF1-FLAG = 'N' AND WS-EOF2-FLAG = 'N' AND 
                  FILE1-SECNR = FILE2-SECNR
                   MOVE FILE1-SECNR TO OUT-SECNR
                   MOVE FILE1-AMOUNT TO OUT-AMOUNT1
                   MOVE FILE2-AMOUNT TO OUT-AMOUNT2
                   ADD FILE1-AMOUNT FILE2-AMOUNT TO OUT-TOTAL

                   MOVE OUT-SECNR TO WS-SECNR
                   PERFORM IMS-RETRIEVE-PARA

                   IF IMS-STATUS = '00'
                       ADD IMS-AMOUNT TO OUT-TOTAL
                   ELSE
                       DISPLAY "IMS RECORD NOT FOUND FOR SECN "
                       MOVE SPACES TO IMS-RESPONSE-AREA
                   END-IF

                   WRITE OUT-RECORD
                   ADD 1 TO WS-RECORD-COUNT

                   READ FILE1 INTO FILE1-RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF1-FLAG
                   END-READ

                   READ FILE2 INTO FILE2-RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF2-FLAG
                   END-READ
               ELSE
                   IF FILE1-SECNR < FILE2-SECNR
                       READ FILE1 INTO FILE1-RECORD
                           AT END
                               MOVE 'Y' TO WS-EOF1-FLAG
                       END-READ
                   ELSE
                       READ FILE2 INTO FILE2-RECORD
                           AT END
                               MOVE 'Y' TO WS-EOF2-FLAG
                       END-READ
                   END-IF
               END-IF
           END-PERFORM.

       IMS-RETRIEVE-PARA.
           CALL 'CBLTDLI' USING
               DL/I-GU
               IMS-DB-PCB
               IMS-SECNR
           IF IMS-STATUS NOT = '00'
               DISPLAY 'IMS Error, Status:' IMS-STATUS.

       CLOSE-FILES.
           CLOSE FILE1
           CLOSE FILE2
           CLOSE OUT-FILE
           DISPLAY "Total records processed: " WS-RECORD-COUNT
           DISPLAY "Total errors: " WS-ERROR-COUNT
           DISPLAY "Total amount: " WS-TOTAL-AMOUNT.

       END PROGRAM FILE-JOIN-CALCULATION.
