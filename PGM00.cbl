       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM00.
       AUTHOR. VALDEMAR.
       DATE-WRITTEN. 14/04/2017.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-OPCAO PIC 9 VALUE ZEROS.
       77 WS-LIMPA PIC x VALUE space.
       01 WS-DATA-SIST.
           05 WS-ANO-SIST PIC 9(4) VALUEs 0.
           05 WS-MES-SIST PIC 99 VALUEs 0.
           05 WS-DIA-SIST PIC 99 VALUEs 0.
       01 WS-DATA-FORMATADA PIC x(10) VALUE spaces.
        
       SCREEN SECTION.
           01 TELA-MENS FOREGROUND-COLOR 04.
           05 LINE 23 COLUMN 22 VALUE "Opcao incorreta! tecle <enter>".
           05 LINE 23 COLUMN 79 PIC x TO WS-LIMPA AUTO.
       
       01 TELA01.
           05 BLANK SCREEN.
           05 TITULO.
               10 LINE 02 COLUMN 01 PIC x(80) VALUE ALL "=".
               10 LINE 04 COLUMN 25 VALUE
               "SISTEMA DE CADASTRO DE PESSOAS".
               10 LINE 06 COLUMN 32 VALUE "MENU PRINCIPAL".
               10 LINE 08 COLUMN 01 PIC x(80) VALUE ALL "=".
           05 DATA-HOJE.
               10 LINE 06 COLUMN 67 PIC x(10) FROM WS-DATA-FORMATADA.
           05 OPCOES.
               10 LINE 10 COLUMN 34 VALUE "1. Incluir".
               10 LINE 11 COLUMN 34 VALUE "2. Alterar".
               10 LINE 12 COLUMN 34 VALUE "3. Remover".
               10 LINE 13 COLUMN 34 VALUE "4. Consultar".
               10 LINE 14 COLUMN 34 VALUE "5. Encerrar Programa".
           05 INF-ESCOLHA.
               10 LINE 16 COLUMN 01 PIC x(80) VALUE ALL "=".
               10 LINE 18 COLUMN 17 VALUE "Informe sua opcao: ".
               10 LINE 20 COLUMN 01 PIC x(80) VALUE ALL "=".
           01 RESP-ESCOLHA.
               05  LINE 18 COLUMN 43 PIC 9 USING WS-OPCAO. 
        
       PROCEDURE DIVISION.
       INICIO.
           ACCEPT WS-DATA-SIST FROM DATE YYYYMMDD
       
           MOVE WS-DIA-SIST TO WS-DATA-FORMATADA(1:2)
           MOVE "/" TO WS-DATA-FORMATADA(3:1)
           MOVE WS-MES-SIST TO WS-DATA-FORMATADA(4:2)
           MOVE "/" TO WS-DATA-FORMATADA(6:1)
           MOVE WS-ANO-SIST TO WS-DATA-FORMATADA(7:4)
           MOVE ZEROS TO WS-OPCAO
        
       PERFORM PROCESSA UNTIL WS-OPCAO=4
       STOP RUN.
        
       PROCESSA.
           DISPLAY TELA01
           ACCEPT RESP-ESCOLHA
         
           IF WS-OPCAO = 1
               CALL "PGM01"
           ELSE
           IF WS-OPCAO = 2
               CALL "PGM02"
           ELSE
           IF WS-OPCAO = 3
               CALL "PGM03"
           ELSE
           IF WS-OPCAO = 4
               CALL "PGM04"
           ELSE
           IF WS-OPCAO = 5
               STOP RUN
       END-IF.