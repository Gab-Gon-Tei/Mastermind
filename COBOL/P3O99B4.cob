
      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         P3O99B3.
       AUTHOR.                             FELIPE.

      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
       77  WS-MSG-ERRO                     PIC X(80).
       77  WS-LENGTH                       PIC S9(04) COMP.

       01  WS-VAR-TEMPO.
           05 WS-DATA                      PIC X(10).
           05 WS-HORARIO                   PIC X(08).

       77  WS-SQLCODE                      PIC X(10).

       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).

           COPY M3O99B5.
           COPY DFHAID.
           COPY DFHBMSCA.

      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           05 OCCURS 0 TO 24576 TIMES DEPENDING ON EIBCALEN
                                           PIC X(01).
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
           EXEC CICS HANDLE CONDITION
              MAPFAIL(999-MAPFAIL)
              ERROR(999-ERROR)
           END-EXEC

           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA

           EVALUATE WS-FASE
              WHEN '1' PERFORM 100-FASE1
              WHEN '2' PERFORM 200-FASE2
              WHEN OTHER
                 MOVE +80                  TO WS-LENGTH
                 MOVE 'ERRO NO NUMERO DA FASE'
                                           TO WS-MSG-ERRO
                 PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE
           .

       100-FASE1.
           MOVE LOW-VALUES                 TO MAPAVITO
           PERFORM 999-TRATA-FASE2
           .

       200-FASE2.
           EXEC CICS HANDLE AID
              ENTER   (210-ENTER)
              PF3     (230-PF3)
              ANYKEY  (240-ANYKEY)
           END-EXEC

           EXEC CICS RECEIVE
              MAP   ('MAPAVIT')
              MAPSET('M3O99B5')
              INTO  (MAPAVITI)
           END-EXEC
           .
      
       210-ENTER.
           MOVE '1'                        TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('P3O99B6')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       230-PF3.
           MOVE +80                        TO WS-LENGTH
           MOVE 'TERMINO NORMAL DA TRANSACAO Y1B4'
                                           TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       240-ANYKEY.
           PERFORM 999-TRATA-FASE2
           .

       999-ENCERRA-TRANSACAO.
            EXEC CICS SEND TEXT
               FROM (WS-MSG-ERRO)
               LENGTH(WS-LENGTH)
               ERASE FREEKB ALARM
            END-EXEC
            EXEC CICS RETURN
            END-EXEC

            .

       999-MANDA-TELA.
            EXEC CICS SEND
               MAP ('MAPAVIT')
               MAPSET('M3O99B4')
               MAPONLY CURSOR
            END-EXEC
            .

       999-CHAMA-FASE1.
           MOVE '1'                        TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('P3O99B4')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-CHAMA-FASE2.
           MOVE '2'                        TO WS-FASE

           EXEC CICS RETURN
               TRANSID('Y1B4')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-TRATA-FASE2.
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .

       999-MAPFAIL.
           MOVE +80                        TO WS-LENGTH
           MOVE 'ERRO NO MAPA VITORIA'     TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       999-ERROR.
           MOVE +80                        TO WS-LENGTH
           MOVE 'ERRO GENERICO'            TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
