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
       77  I                               PIC 9(04).
       77  WS-CHAR                         PIC 9(04).
       77  WS-ACERTOS-POSICAO-CORRETA      PIC 9(04).
       77  WS-ACERTOS-POSICAO-ERRADA       PIC 9(04).
       77  WS-SENHA-S                      PIC 9(04).
       77  WS-SENHA-E                      PIC 9(04).
       77  WS-SENHA-N                      PIC 9(04).
       77  WS-SENHA-H                      PIC 9(04).
       77  WS-SENHA-A                      PIC 9(04).
       77  WS-TENT-S                       PIC 9(04).
       77  WS-TENT-E                       PIC 9(04).
       77  WS-TENT-N                       PIC 9(04).
       77  WS-TENT-H                       PIC 9(04).
       77  WS-TENT-A                       PIC 9(04).
      *----------------------------------------------------------------*
      * VARIAVEIS DA DFHCOMMAREA
       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
           05  WS-SENHA.
               10 WS-LETRA-1                   PIC X(01).
               10 WS-LETRA-2                   PIC X(01).
               10 WS-LETRA-3                   PIC X(01).
               10 WS-LETRA-4                   PIC X(01).
               10 WS-LETRA-5                   PIC X(01).
           05  WS-TENTATIVA.
               10 WS-LETRA-1-T                 PIC X(01).
               10 WS-LETRA-2-T                 PIC X(01).
               10 WS-LETRA-3-T                 PIC X(01).
               10 WS-LETRA-4-T                 PIC X(01).
               10 WS-LETRA-5-T                 PIC X(01).
           05  WS-CONT-TENTATIVAS              PIC 9(04) VALUE 0.
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
           MOVE WS-CONT-TENTATIVAS         TO CONTO
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

            PERFORM 999-TRATA-FASE2
           .

       200-FASE2.
           EXEC CICS HANDLE AID
              ENTER   (210-ENTER)
               PF3     (220-PF3)
      *        PF5     (230-PF5)
      *        CLEAR   (230-PF5)
      *        PF2     (240-PF2)
              ANYKEY  (250-ANYKEY)
           END-EXEC

           EXEC CICS RECEIVE
              MAP   ('MAPASEN')
              MAPSET('T04MLOG')
              INTO  (MAPASENI)
           END-EXEC
           .

       210-ENTER.
           MOVE LETRA1I                            TO WS-LETRA-1-T
           MOVE LETRA2I                            TO WS-LETRA-2-T
           MOVE LETRA3I                            TO WS-LETRA-3-T
           MOVE LETRA4I                            TO WS-LETRA-4-T
           MOVE LETRA5I                            TO WS-LETRA-5-T
           ADD 1                                   TO WS-CONT-TENTATIVAS

           PERFORM 212-FREQUENCIA-SENHA
           PERFORM 213-FREQUENCIA-TENTATIVA
           PERFORM 211-CONTA-POSICAO-CERTA
           PERFORM 214-CONTA-POSICAO-ERRADA

           COMPUTE WS-ACERTOS-POSICAO-ERRADA =
            WS-ACERTOS-POSICAO-ERRADA - WS-ACERTOS-POSICAO-CORRETA 
           
           EVALUATE WS-CONT-TENTATIVAS
               WHEN 1
                   MOVE WS-TENTATIVA TO TENT1I
               WHEN 2
                   MOVE WS-TENTATIVA TO TENT2I
               WHEN 3
                   MOVE WS-TENTATIVA TO TENT3I
               WHEN 4
                   MOVE WS-TENTATIVA TO TENT4I
               WHEN 5
                   MOVE WS-TENTATIVA TO TENT5I
               WHEN 6
                   MOVE WS-TENTATIVA TO TENT6I
               WHEN 7
                   MOVE WS-TENTATIVA TO TENT7I
               WHEN 8
                   MOVE WS-TENTATIVA TO TENT8I
               WHEN 9
                   MOVE WS-TENTATIVA TO TENT9I
               WHEN 10
                   MOVE WS-TENTATIVA TO TENT10I
               WHEN 11
                   MOVE WS-TENTATIVA TO TENT11I
               WHEN 12
                   MOVE WS-TENTATIVA TO TETN12I
               WHEN 13
                   MOVE WS-TENTATIVA TO TENT13I
               WHEN 14
                   MOVE WS-TENTATIVA TO TENT14I
               WHEN 15
                   MOVE WS-TENTATIVA TO TENT15I
               WHEN 16
                   MOVE WS-TENTATIVA TO TENT16I
               WHEN OTHER
                   MOVE 'TENTATIVAS EXCEDIDAS/ VOCE PERDEU' TO MSGO
           END-EVALUATE
           MOVE WS-CONT-TENTATIVAS TO CONTO
           
           EVALUATE WS-ACERTOS-POSICAO-CORRETA ALSO WS-CONT-TENTATIVAS
               WHEN 5 ALSO 1 THRU 16
                   MOVE WS-ACERTOS-POSICAO-CORRETA TO CERTASI
                   MOVE WS-ACERTOS-POSICAO-ERRADA  TO ERRADASI
      *             MOVE 'GREEN'                    TO TENT11C
                   MOVE 'SENHA DECODIFICADA/ VOCE VENCEU' TO MSGO
                   PERFORM 999-TRATA-VITORIA
               WHEN 1 THRU 4 ALSO 1 THRU 16
                   MOVE WS-ACERTOS-POSICAO-CORRETA TO CERTASI
                   MOVE WS-ACERTOS-POSICAO-ERRADA  TO ERRADASI
                   MOVE 'TENTE NOVAMENTE' TO MSGO
                   PERFORM 999-TRATA-FASE2
               WHEN 1 THRU 4 ALSO 17
                   MOVE 'VOCE PERDEU' TO MSGO
           END-EVALUATE           
           .
           
       212-FREQUENCIA-SENHA.
      * VERIFICA A FREQUENCIA DE CADA LETRA NA SENHA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE WS-SENHA(I:1) TO WS-CHAR
               EVALUATE WS-CHAR
                   WHEN 'S' ADD 1 TO WS-SENHA-S
                   WHEN 'E' ADD 1 TO WS-SENHA-E
                   WHEN 'N' ADD 1 TO WS-SENHA-N
                   WHEN 'H' ADD 1 TO WS-SENHA-H
                   WHEN 'A' ADD 1 TO WS-SENHA-A
               END-EVALUATE
               ADD 1 TO I
           END-PERFORM
           .

       213-FREQUENCIA-TENTATIVA.
      * VERIFICA A FREQUENCIA DE CADA LETRA NA TENTATIVA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE WS-TENTATIVA(I:1) TO WS-CHAR
               EVALUATE WS-CHAR
                   WHEN 'S' ADD 1 TO WS-TENT-S
                   WHEN 'E' ADD 1 TO WS-TENT-E
                   WHEN 'N' ADD 1 TO WS-TENT-N
                   WHEN 'H' ADD 1 TO WS-TENT-H
                   WHEN 'A' ADD 1 TO WS-TENT-A
               END-EVALUATE
               ADD 1 TO I
           END-PERFORM
           .
       211-CONTA-POSICAO-CERTA.
           IF WS-LETRA-1 EQUAL WS-LETRA-1-T
               ADD 1 TO WS-ACERTOS-POSICAO-CORRETA
           END-IF
           IF WS-LETRA-2 EQUAL WS-LETRA-2-T
               ADD 1 TO WS-ACERTOS-POSICAO-CORRETA
           END-IF
           IF WS-LETRA-3 EQUAL WS-LETRA-3-T
               ADD 1 TO WS-ACERTOS-POSICAO-CORRETA
           END-IF
           IF WS-LETRA-4 EQUAL WS-LETRA-4-T
               ADD 1 TO WS-ACERTOS-POSICAO-CORRETA
           END-IF
           IF WS-LETRA-5 EQUAL WS-LETRA-5-T
               ADD 1 TO WS-ACERTOS-POSICAO-CORRETA
           END-IF
           .
       
       214-CONTA-POSICAO-ERRADA.
      * ADICIONA O MENOR VALOR DE FREQUENCIA A QUANTIDADE DE ACERTOS
      * NA POSICAO ERRADA
      * PARA A LETRA S
           IF WS-SENHA-S < WS-TENT-S
               ADD WS-SENHA-S TO WS-ACERTOS-POSICAO-ERRADA
           ELSE
               ADD WS-TENT-S TO WS-ACERTOS-POSICAO-ERRADA
           END-IF
       
      * PARA A LETRA E
           IF WS-SENHA-E < WS-TENT-E
               ADD WS-SENHA-E TO WS-ACERTOS-POSICAO-ERRADA
           ELSE
               ADD WS-TENT-E TO WS-ACERTOS-POSICAO-ERRADA
           END-IF

      * PARA A LETRA N
           IF WS-SENHA-N < WS-TENT-N
               ADD WS-SENHA-N TO WS-ACERTOS-POSICAO-ERRADA
           ELSE
               ADD WS-TENT-N TO WS-ACERTOS-POSICAO-ERRADA
           END-IF

      * PARA A LETRA H
           IF WS-SENHA-H < WS-TENT-H
               ADD WS-SENHA-H TO WS-ACERTOS-POSICAO-ERRADA
           ELSE
               ADD WS-TENT-H TO WS-ACERTOS-POSICAO-ERRADA
           END-IF

      * PARA A LETRA A
           IF WS-SENHA-A < WS-TENT-A
               ADD WS-SENHA-A TO WS-ACERTOS-POSICAO-ERRADA
           ELSE
               ADD WS-TENT-A TO WS-ACERTOS-POSICAO-ERRADA
           END-IF
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
                                           TO MSGO
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

           MOVE WS-DATA-F                   TO DATAO
           MOVE WS-HORARIO-F                TO HORAO

           EXEC CICS SEND
              MAP ('MAPASEN')
              MAPSET('SENHA')
              FROM(MAPASENO)
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
           MOVE -1                        TO LETRA1L

           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .

       999-TRATA-VITORIA.
      *    MOVE LOW-VALUES                TO MAPLOGO
           MOVE -1                        TO MSGO

           PERFORM 999-MANDA-TELA

           MOVE '2'                       TO WS-FASE
           
           MOVE 'Z'                        TO LETRA1A 
           MOVE 'Z'                        TO LETRA2A 
           MOVE 'Z'                        TO LETRA3A 
           MOVE 'Z'                        TO LETRA4A 
           MOVE 'Z'                        TO LETRA5A 

           EXEC CICS RETURN
               TRANSID('FT4A')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       999-MAPFAIL.
           MOVE 'ERRO MAPA M3O99B1'        TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

       999-ERROR.
           MOVE 'ERRO GENERICO'   TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
