      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         P3O99B0.
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
      * VARIVEIS DE TRABALHO
       77  WS-COUNT-SENHAS                 PIC 9(04).
       77  WS-SEED-RANDOM                  PIC 9(04).
       77  WS-ID-RANDOM                    PIC 9(04).
       01  WS-LETRAS.
           05 WS-LETRA-1                   PIC X(01).
           05 WS-LETRA-2                   PIC X(01).
           05 WS-LETRA-3                   PIC X(01).
           05 WS-LETRA-4                   PIC X(01).
           05 WS-LETRA-5                   PIC X(01).
      *----------------------------------------------------------------*
      * VARIAVEIS DA DFHCOMMAREA
       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
      *----------------------------------------------------------------*

      *MAPA REFERENTE A TELA DE CADASTRO
           COPY M3O99B1.
      *COMANDO TECLAS PRESSIONADAS
           COPY DFHAID.
      *CARACTERES E ATRIBUTOS
           COPY DFHBMSCA.

           EXEC SQL
              INCLUDE DCLSENHA
           END-EXEC.

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
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
           MOVE LOW-VALUES                 TO MAPASENO
           MOVE -1                         TO LETRA1L
           EXEC SQL
               SELECT COUNT (ID)
               INTO :WS-COUNT-SENHAS
               FROM SENHAS
           END-EXEC
           EVALUATE SQLCODE
            WHEN +100
               MOVE 0 TO WS-COUNT-SENHAS
               MOVE +80 TO WS-LENGTH
               MOVE 'ARQUIVO DE SENHAS VAZIO' TO WS-MSG-ERRO
               PERFORM 999-ENCERRA-TRANSACAO
            WHEN 0
               CONTINUE
            WHEN OTHER
                MOVE +80                    TO WS-LENGTH
               MOVE 'ERRO NO CONTADOR DE SENHAS' TO WS-MSG-ERRO
               PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE

      * O RANDOM PRECISA DE UMA SEED (OU SEMENTE), QUE SERIA UM NUMERO 
      * QUALQUER PARA QUE A FUNCAO TENHA UM NUMERO ALEATORIO.
      * APOS ISSO, O LIMITE E A QUANTIDADE DE SENHAS.     
           ACCEPT WS-SEED-RANDOM FROM TIME
           COMPUTE WS-ID-RANDOM = 
           (FUNCTION RANDOM(WS-SEED-RANDOM) * WS-COUNT-SENHAS) + 1
           
           EXEC SQL
           SELECT LETRA_1, LETRA_2, LETRA_3, LETRA_4, LETRA_5
               INTO :WS-LETRA-1, :WS-LETRA-2, :WS-LETRA-3, :WS-LETRA-4, 
               :WS-LETRA-5
           FROM SENHAS
           WHERE ID = :WS-ID-RANDOM;
           END-EXEC
           EVALUATE SQLCODE
            WHEN +100
               MOVE +80                        TO WS-LENGTH
               MOVE 'SENHA NAO ENCONTRADA' TO WS-MSG-ERRO
               PERFORM 999-ENCERRA-TRANSACAO
            WHEN 0
               MOVE 'USE A FORCA E DESCUBRA A SENHA' TO MSGO
            WHEN OTHER
               MOVE +80                    TO WS-LENGTH
               MOVE 'ERRO AO BUSCAR SENHA' TO WS-MSG-ERRO
               PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE

           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .

       200-FASE2.
           EXEC CICS HANDLE AID
              ENTER   (210-ENTER)
      *        PF3     (220-PF3)
      *        PF5     (230-PF5)
      *        CLEAR   (230-PF5)
      *        PF2     (240-PF2)
              ANYKEY  (250-ANYKEY)
           END-EXEC

           EXEC CICS RECEIVE
              MAP   ('MAPLOG')
              MAPSET('T04MLOG')
              INTO  (MAPLOGI)
           END-EXEC
           .

       210-ENTER.
           MOVE LETRA1I                            TO DCLSNH-LETRA-1
           MOVE LETRA2I                            TO DCLSNH-LETRA-2
           MOVE LETRA3I                            TO DCLSNH-LETRA-3
           MOVE LETRA4I                            TO DCLSNH-LETRA-4
           MOVE LETRA5I                            TO DCLSNH-LETRA-5
           
           
           
       210-VALIDA-USUARIO.
           EXEC SQL
              SELECT NOME_USUARIO
                    ,SENHA
                    ,CPF
              INTO :DCLCLI-NOME-USUARIO
                   ,:DCLCLI-SENHA
                   ,:DCLCLI-CPF
              FROM CLIENTES
              WHERE NOME_USUARIO = :DCLCLI-NOME-USUARIO
           END-EXEC

           IF SQLCODE = +100
               MOVE 'USUARIO NAO ENCONTRADO'
                                           TO T1MSGO
               PERFORM 999-TRATA-FASE2
           ELSE
               IF SQLCODE NOT EQUAL 0
                   MOVE 'ERRO AO CONSULTAR USUARIO'
                                           TO T1MSGO
                   PERFORM 999-TRATA-FASE2
               END-IF
           END-IF
           .

       210-VALIDA-SENHA.
           IF DCLCLI-SENHA EQUAL T1SENHAI
               CONTINUE
           ELSE
               MOVE 'SENHA INCORRETA'         TO T1MSGO
               PERFORM 999-TRATA-FASE2
           END-IF
           .

       260-LISTAGEM-DE-PRODUTOS.
           MOVE '1'                       TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T04PPRL')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       220-PF3.
           MOVE +80                        TO WS-LENGTH
           MOVE 'FIM NORMAL DA TRANSACAO FT4A'
                                           TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       230-PF5.
           PERFORM 999-CHAMA-FASE1
           .

       240-PF2.
           MOVE '1'                       TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T04PCAD')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       250-ANYKEY.
           MOVE 'TECLA PRESSIONADA INVALIDA!'
                                           TO T1MSGO
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
           MOVE EIBTRMID                  TO TERMO
           MOVE EIBTRNID                  TO TRANSO
           MOVE EIBTASKN                  TO TASKO
           MOVE WS-FASE                   TO FASEO

           ACCEPT WS-DATA FROM DATE
           ACCEPT WS-HORARIO FROM TIME

           MOVE WS-DIA                     TO WS-DIA-F
           MOVE WS-MES                     TO WS-MES-F
           MOVE WS-ANO                     TO WS-ANO-F

           MOVE WS-HORA                    TO WS-HORA-F
           MOVE WS-MIN                     TO WS-MIN-F
           MOVE WS-SEG                     TO WS-SEG-F

           MOVE WS-DATA-F                   TO T1DATAO
           MOVE WS-HORARIO-F                TO T1HORAO

           EXEC CICS SEND
              MAP ('MAPASEN')
              MAPSET('SENHA')
              FROM(MAPSENO)
              ERASE FREEKB ALARM CURSOR
           END-EXEC
           .

       999-CHAMA-FASE1.
           MOVE '1'                       TO WS-FASE

           MOVE 'USE A FORÇA E DESCUBRA A SENHA'
                                          TO MSGO
           EXEC CICS XCTL
              PROGRAM('T04PLOG')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-CHAMA-FASE2.
           MOVE '2'                       TO WS-FASE

           EXEC CICS RETURN
               TRANSID('FT4A')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

       999-TRATA-FASE2.
      *    MOVE LOW-VALUES                TO MAPLOGO
           MOVE -1                        TO T1USERL

           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
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