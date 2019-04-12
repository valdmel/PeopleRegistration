       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM04  INITIAL.
       AUTHOR. VALDEMAR.
       DATE-WRITTEN. 28/04/2017.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
           SELECT  ARQ-PESSOAS ASSIGN TO "C:\TEMP\PESSOAS.DAT"
                ORGANIZATION   INDEXED
                RECORD KEY  CPF
                ACCESS  RANDOM
                FILE  STATUS  CODERRO.
       DATA DIVISION.
       FILE  SECTION.
       FD  ARQ-PESSOAS
           LABEL  RECORD  STANDARD.
       01  REG-PESSOAS.
           02  CPF    PIC X(11).
           02  NOME PIC X(30).
           02  ENDERECO  PIC X(45).
           02  COMPLEMENTO PIC X(10).
           02  BAIRRO PIC X(20).
           02  CIDADE PIC X(20).
           02  ESTADO PIC X(2).
           02  CEP PIC X(9).
           02  FILLER      PIC X(41).
           
       WORKING-STORAGE SECTION.
       77  CODERRO   PIC  X(2)  VALUE SPACES.
       77  OPC  PIC X VALUE SPACE.
       88  OPC-OK  VALUE "S" "N".
       77  W-CPF-PESQUISA   PIC 9(3) VALUE ZEROS.
	
       01 WS-DATA-SIST.
           05 WS-ANO-SIST PIC 9(4) VALUES 0.
           05 WS-MES-SIST PIC 99 VALUES 0.
           05 WS-DIA-SIST PIC 99 VALUES 0.
       01 WS-DATA-FORMATADA PIC x(10) VALUE SPACES.

       SCREEN SECTION.
           01 TELA01.
           05 BLANK SCREEN.
           05 TITULO.
               10 LINE 02 COLUMN 01 PIC x(80) VALUE ALL "=".
               10 LINE 04 COLUMN 25 VALUE
               "SISTEMA DE CADASTRO DE PESSOAS".
               10 LINE 06 COLUMN 32 VALUE "CONSULTAR PESSOA".
               10 LINE 08 COLUMN 01 PIC x(80) VALUE ALL "=".
           05 DATA-HOJE.
               10 LINE 06 COLUMN 67 PIC x(10) FROM WS-DATA-FORMATADA.
       
       PROCEDURE DIVISION.
       INICIO.
           ACCEPT WS-DATA-SIST FROM DATE YYYYMMDD
       
           MOVE WS-DIA-SIST TO WS-DATA-FORMATADA(1:2)
           MOVE "/" TO WS-DATA-FORMATADA(3:1)
           MOVE WS-MES-SIST TO WS-DATA-FORMATADA(4:2)
           MOVE "/" TO WS-DATA-FORMATADA(6:1)
           MOVE WS-ANO-SIST TO WS-DATA-FORMATADA(7:4)
           
   	       PERFORM   INICIALIZACAO.
           PERFORM   PROCESSAMENTO  UNTIL  OPC = "N".
           PERFORM   FINALIZACAO.
           STOP RUN.
           
       INICIALIZACAO.
           DISPLAY ERASE.
           DISPLAY TELA01.

           PERFORM ABRIR-ARQUIVO.
           
       PROCESSAMENTO.
           PERFORM   FORMATAR-TELA.
	       PERFORM   ROTINA-LEITURA
           PERFORM   EXIBIR-DADOS-LIDOS.
           PERFORM   RECEBER-OPCAO-CONTINUIDADE.
           
       FORMATAR-TELA.
           INITIALIZE CODERRO REG-PESSOAS W-CPF-PESQUISA.
           
           DISPLAY  "CPF:"   AT  1210.
           DISPLAY  "NOME:"   AT  1310.
           DISPLAY  "ENDERECO:"   AT  1410.
           DISPLAY  "COMPLEMENTO:"   AT  1510.
           DISPLAY  "BAIRRO:"   AT  1610.
           DISPLAY  "CIDADE:"   AT  1710.
           DISPLAY  "ESTADO:"   AT  1810.
           DISPLAY  "CEP:"   AT  1910.
           DISPLAY  "OUTRO REGISTRO? (S/N):"   AT  2210.
           
       ROTINA-LEITURA.
           DISPLAY  "DIGITE O CPF A PESQUISAR"  AT 1010
           ACCEPT CPF  AT 1222.
           READ ARQ-PESSOAS.
       
       EXIBIR-DADOS-LIDOS.
           IF  CODERRO NOT = "00"
               DISPLAY "PESSOA NAO FOI ENCONTRADA" AT 1140 WITH BLINK
           ELSE
               DISPLAY  NOME  AT 1322
               DISPLAY  ENDERECO   AT 1422
               DISPLAY  COMPLEMENTO AT 1522
               DISPLAY  BAIRRO AT 1622
               DISPLAY  CIDADE AT 1722
               DISPLAY  ESTADO AT 1822
               DISPLAY  CEP AT 1922
           END-IF.
      *
       RECEBER-OPCAO-CONTINUIDADE.
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT OPC  AT  2235 WITH AUTO
               MOVE FUNCTION UPPER-CASE (OPC) TO OPC
               
               IF  OPC-OK
                   DISPLAY "                   " AT 2040
               ELSE
                   DISPLAY " DIGITE S OU N" AT 2040
               END-IF
           END-PERFORM.

       ABRIR-ARQUIVO.
           OPEN  I-O  ARQ-PESSOAS.
           IF  CODERRO NOT = "00"
               DISPLAY "ARQUIVO NAO ENCONTRADO" AT 2040 WITH
                       FOREGROUND-COLOR 4
               STOP  " "
               
               MOVE  "N"  TO  OPC
           ELSE
               DISPLAY "                       " AT 2040
           END-IF.
           
       FINALIZACAO.
           CLOSE  ARQ-PESSOAS.
           DISPLAY "FIM DE PROCESSAMENTO" AT 2455.
           STOP  " ".

       FIM-ULTIMA-LINHA.
