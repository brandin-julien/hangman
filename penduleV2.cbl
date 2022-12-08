      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
            SELECT DATAFILE ASSIGN TO "dataForPendule.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD DATAFILE.
       01 words-input PIC x(30).

       WORKING-STORAGE SECTION.

       77 listingwords PIC x(30) OCCURS 50.
       77 seed PIC 999.
       77 randomPic PIC 99.*>V99999
       01 choosenWord.
          05 cw-charactere PIC X(1) OCCURS 30.
       01 answerWord.
          05 aw-charactere PIC X(1) OCCURS 30.
       01 letterArray.
          05 la-letter PIC X(1) OCCURS 5.

       77 input-letter PIC X(1).
       77 i PIC 99.
       77 charactere-found PIC 9. *>BOOL
       77 nb-life PIC 9 VALUE 5.
       77 game-finish PIC 9 VALUE 0.
       01 END-OF-FILE  PIC 9   VALUE 0.
       77 maxIndex PIC 99.
       77 nb-party PIC 9 VALUE 1.
       77 nb-count-party PIC 9 VALUE 1.

      *====step of graphic content

       01 first-step.
          05   PIC x(6)    VALUE'|     '.
          05   PIC x(6)    VALUE'|     '.
          05   PIC x(6)    VALUE'|     '.
          05   PIC x(6)    VALUE'|     '.
          05   PIC x(6)    VALUE'|     '.
          05   PIC x(6)    VALUE'|     '.


       *>SCREEN SECTION.

       *>    01 pla-win.
       *>        05 LINE 4 COL 5 VALUE 'WIN !!!!!!'
       *>    FOREGROUND-COLOR 15
       *>    BACKGROUND-COLOR 4.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

          PERFORM generateArray.

          DISPLAY "nb of party "
          ACCEPT nb-party

          PERFORM VARYING nb-count-party FROM 1 BY 1
          UNTIL nb-count-party > nb-party
               PERFORM launch-game
          END-PERFORM

          STOP RUN.

       launch-game.
           DISPLAY "game #" nb-count-party
           MOVE 5 TO nb-life
           MOVE 0 TO game-finish
           MOVE SPACES TO letterArray
           PERFORM generateNumber.
           MOVE listingwords(randomPic) TO choosenWord.
           *>DISPLAY "mot a deviner : "choosenWord.
           PERFORM get-lenght-word.
           DISPLAY "max  : "maxIndex.

           INITIALIZE answerWord
           PERFORM display-answer-word

           PERFORM UNTIL game-finish = 1
               ACCEPT input-letter
               MOVE 0 TO i
               MOVE 0 TO charactere-found
               PERFORM verif-charactere
               PERFORM display-info
               PERFORM display-letter

               PERFORM verif-game
           END-PERFORM.


       generateArray.

          OPEN INPUT DATAFILE.

          READ DATAFILE
           AT END MOVE 1 TO END-OF-FILE.

          IF END-OF-FILE = 1
              display 'end of file'
              stop run
          END-IF.

          MOVE 1 to I

          PERFORM UNTIL END-OF-FILE = 1

      *         display "words-input : " words-input
      *         display "i : " i
               MOVE words-input to listingwords(i)
      *         display "move " listingwords(i) "to listingwords(i)"
               COMPUTE I = I + 1
               READ DATAFILE
               AT END MOVE 1 TO END-OF-FILE

          END-PERFORM

          PERFORM endFile.

       generateNumber.
               MOVE FUNCTION CURRENT-DATE(13:2) TO seed.
      *         display "seed after ", seed.
               COMPUTE randomPic = FUNCTION RANDOM(seed) *1000000 + 1.
               display "randompic : ", randomPic

               IF randomPic > 50
                   MOVE 50 TO randomPic
                   display "randomPic changed : " randomPic
               END-IF.

               *>MOVE 3 TO randomPic.

       verif-charactere.

           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 30
                  *>IF charactere-found = 0
                      if input-letter = cw-charactere(i)
                          MOVE 1 to charactere-found
                          MOVE input-letter to aw-charactere(i)
                      END-IF
                  *>END-IF
           END-PERFORM
          IF charactere-found = 0
               MOVE input-letter TO la-letter(6 - nb-life)
          END-IF.


       get-lenght-word.

          PERFORM VARYING I FROM 1 BY 1
          UNTIL cw-charactere(i) = ''
               MOVE I TO maxIndex
          END-PERFORM.

       display-info.

           IF charactere-found = 1
               DISPLAY "Bonne réponse ! "
           ELSE
               DISPLAY "FAUX"
               COMPUTE nb-life = nb-life - 1
           END-IF

          PERFORM display-answer-word
          DISPLAY "Numer of life : " nb-life.

       display-answer-word.
          PERFORM VARYING I FROM 1 BY 1 UNTIL i > maxIndex
               IF aw-charactere(i) NOT = SPACE
                   DISPLAY aw-charactere(i) NO ADVANCING
               ELSE
                   display '-' NO ADVANCING
               END-IF
          END-PERFORM
          display SPACE.


       display-letter.
           display "lettres fausses : " NO ADVANCING
          PERFORM VARYING I FROM 1 BY 1 UNTIL i > 5
             DISPLAY la-letter(i) '-' NO ADVANCING
          END-PERFORM
          display SPACE.



       verif-game.

           IF nb-life = 0
               MOVE 1 TO game-finish
               DISPLAY "YOU ARE DEAD !!!"
               DISPLAY "Word was : " choosenWord
           END-IF

           IF answerWord = choosenWord
               MOVE 1 TO game-finish
               DISPLAY "WIN!!!!!!!"
               *>DISPLAY pla-win
           END-IF.

       endFile.

          IF END-OF-FILE = 1
              CLOSE DATAFILE
              display "end of file"
          END-IF.



      *====== Grahpic content of death

       step1.

            *> etape 1
          display '|     '
          display '|     '
          display '|     '
          display '|     '
          display '|     '.


       step2.

          *> etape 2
          display ' ____'
          display '|     '
          display '|     '
          display '|     '
          display '|     '
          display '|     '.

       step3.

          *> etape 3
          display ' ____'
          display '|  |  '
          display '|     '
          display '|     '
          display '|     '
          display '|     '.

       step4.

          *> etape 4
           display ' ____'
          display '|  |  '
          display '|  O  '
          display '|     '
          display '|     '
          display '|     '.

       step5.
          *> etape 5
          display ' ____ '
          display '|  |  '
          display '|  O  '
          display '| /|\ '
          display '|  |  '
          display '| / \ '
          display '|     '.

       END PROGRAM YOUR-PROGRAM-NAME.
