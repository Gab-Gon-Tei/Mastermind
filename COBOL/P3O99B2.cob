      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         P3O99B2.
       AUTHOR.                             GABRIEL E FELIPE.

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

      *----------------------------------------------------------------*
      * VARIAVEIS DE DATA E HORARIO
      *----------------------------------------------------------------*
       01  WS-DATA.
           05 WS-ANO                       PIC X(02).
           05 WS-MES                       PIC X(02).
           05 WS-DIA                       PIC X(02).

       01  WS-HORARIO.
           05 WS-HORA                      PIC X(02).
           05 WS-MIN                       PIC X(02).
           05 WS-SEG                       PIC X(02).

       01  WS-DATA-F.
           05 WS-DIA-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE '/'.
           05 WS-MES-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE '/'.
           05 WS-ANO-F                     PIC X(02).

       01  WS-HORARIO-F.
           05 WS-HORA-F                    PIC X(02).
           05 FILLER                       PIC X(01) VALUE ':'.
           05 WS-MIN-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE ':'.
           05 WS-SEG-F                     PIC X(02).
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VARIAVEIS DA DFHCOMMAREA
       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
       01  WS-SENHA.
           05 WS-LETRA-1                   PIC X(01).
           05 WS-LETRA-2                   PIC X(01).
           05 WS-LETRA-3                   PIC X(01).
           05 WS-LETRA-4                   PIC X(01).
           05 WS-LETRA-5                   PIC X(01).
       01  WS-TENTATIVA.
           05 WS-LETRA-1-T                 PIC X(01).
           05 WS-LETRA-2-T                 PIC X(01).
           05 WS-LETRA-3-T                 PIC X(01).
           05 WS-LETRA-4-T                 PIC X(01).
           05 WS-LETRA-5-T                 PIC X(01).
      *----------------------------------------------------------------*

      *MAPA REFERENTE A TELA DE CADASTRO
           COPY M3O99B3.
      *COMANDO TECLAS PRESSIONADAS
           COPY DFHAID.
      *CARACTERES E ATRIBUTOS
           COPY DFHBMSCA.

      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           05 OCCURS 0 TO 24579 TIMES DEPENDING ON EIBCALEN
                                           PIC X(01).
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
      *ROTINA DEFINIDA PARA LIDAR COM ERROS DE MAPA E GENERICOS--------*
           EXEC CICS HANDLE CONDITION
              MAPFAIL(999-MAPFAIL)
              ERROR(999-ERROR)
           END-EXEC
      *----------------------------------------------------------------*
      * MAPEAMENTO DOS CAMPOS DO DFHCOMMAREA PARA O WS-DFHCOMMAREA
      * E SELECAO DE FASE DEPENDENDO DO CONTEUDO DO MAPA

           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA

           IF EIBCALEN EQUAL 0
              MOVE '1'                     TO WS-FASE
           END-IF

           EVALUATE WS-FASE
              WHEN '1'  PERFORM 100-FASE1
              WHEN '2' PERFORM 200-FASE2
              WHEN OTHER
                 MOVE +80                  TO WS-LENGTH
                 MOVE 'ERRO NO NUMERO DA FASE'
                                           TO WS-MSG-ERRO
                 PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      * FASE 1 - O PROGRAMA ACESSA O BANCO DE DADOS DAS SENHAS, CONTA
      * QUANTAS SENHAS EXISTEM. COM ISSO, O PROGRAMA PODE GERAR UMA
      * SENHA ALEATORIA, SE BASEANDO NO ID DAS SENHAS.

       100-FASE1.
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .

       200-FASE2.
           EXEC CICS HANDLE AID
              ENTER   (210-ENTER)
              PF3     (220-PF3)
              ANYKEY  (250-ANYKEY)
           END-EXEC

           EXEC CICS RECEIVE
              MAP   ('MAPATUT')
              MAPSET('M3O99B3')
              INTO  (MAPATUTI)
           END-EXEC
           .

       210-ENTER.
           EXEC CICS XCTL
              PROGRAM('P3O99B0')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       220-PF3.
           MOVE +80                        TO WS-LENGTH
           MOVE 'FIM NORMAL DA TRANSACAO Y1B2'
                                           TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       250-ANYKEY.
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

       999-CHAMA-FASE1.
           MOVE '1'                       TO WS-FASE

           EXEC CICS XCTL
              PROGRAM('P3O99B0')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-CHAMA-FASE2.
           MOVE '2'                       TO WS-FASE

           EXEC CICS RETURN
               TRANSID('Y1B2')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-TRATA-FASE2.
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .

       999-MANDA-TELA.
           EXEC CICS SEND
              MAP ('MAPATUT')
              MAPSET('M3O99B3')
              FROM(MAPATUTO)
              ERASE FREEKB ALARM CURSOR
           END-EXEC
           .

       999-MAPFAIL.
           MOVE 'ERRO MAPA T04MLOG'        TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       999-ERROR.
           MOVE 'ERRO GENERICO'   TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
