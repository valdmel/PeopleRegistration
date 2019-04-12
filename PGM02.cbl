       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM02  INITIAL.
       AUTHOR. VALDEMAR.
       DATE-WRITTEN. 03/05/2017.
       
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
       77  OPC-ALT        PIC   X  VALUE SPACE.
	
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
               10 LINE 06 COLUMN 32 VALUE "ALTERAR PESSOA".
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
           PERFORM   PROCESSAMENTO UNTIL OPC = "N".
           PERFORM   FINALIZACAO.
           STOP RUN.
           
       INICIALIZACAO.
           DISPLAY ERASE.
           DISPLAY TELA01.
           
           PERFORM ABRIR-ARQUIVO.
           
       PROCESSAMENTO.
           PERFORM  FORMATAR-TELA.
           PERFORM  ROTINA-LEITURA
           PERFORM  ROTINA-ALTERACAO.
           PERFORM  RECEBER-OPCAO-CONTINUIDADE.
           
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
           DISPLAY  "DIGITE O CPF DA PESSOA A ALTERAR"  AT 1010
           ACCEPT CPF AT 1222.

           READ ARQ-PESSOAS
           IF  CODERRO NOT = "00"
               DISPLAY "PESSOA NAO FOI ENCONTRADA" AT 1040 WITH BLINK
           END-IF.

       ROTINA-ALTERACAO.
            IF  CODERRO = "00"
                PERFORM   RECEBER-NOVO-NOME
                PERFORM   RECEBER-NOVO-ENDERECO
                PERFORM   RECEBER-NOVO-COMPLEMENTO
                PERFORM   RECEBER-NOVO-BAIRRO
                PERFORM   RECEBER-NOVA-CIDADE
                PERFORM   RECEBER-NOVO-ESTADO
                PERFORM   RECEBER-NOVO-CEP

                DISPLAY  "CONFIRMA A ALTERACAO?(S/N): "  AT  2110
                ACCEPT   OPC-ALT AT 2140 WITH UPPER AUTO
                
		   IF  OPC-ALT  =  "S"
  			REWRITE  REG-PESSOAS
                ELSE
			DISPLAY  "ALTERACAO NAO EFETIVADA"  AT 1844
                   STOP  "   <ENTER> PARA CONTINUAR"
                END-IF
             ELSE
                NEXT  SENTENCE
             END-IF.
      *
       RECEBER-NOVO-NOME.
           PERFORM WITH TEST AFTER UNTIL NOME NOT = SPACES
               ACCEPT NOME  AT  1330
               
               IF  NOME = SPACES
                   DISPLAY "ERRO NO NOME!" AT 2421
               ELSE
                   DISPLAY "   " AT 2421
               END-IF
           END-PERFORM.
           
       RECEBER-NOVO-ENDERECO.      
           PERFORM WITH TEST AFTER UNTIL ENDERECO NOT = SPACES
               ACCEPT ENDERECO  AT  1430
               
               IF  ENDERECO = SPACES
                   DISPLAY "ERRO NO ENDERECO!" AT 2421
               ELSE
                   DISPLAY "    " AT 2421
               END-IF
           END-PERFORM.
           
       RECEBER-NOVO-COMPLEMENTO.
               ACCEPT COMPLEMENTO  AT  1530.
               
       RECEBER-NOVO-BAIRRO.
           PERFORM WITH TEST AFTER UNTIL BAIRRO NOT = SPACES
               ACCEPT BAIRRO  AT  1630
               
               IF  BAIRRO = SPACES
                   DISPLAY "ERRO NO BAIRRO!" AT 2421
               ELSE
                   DISPLAY "    " AT 2421
               END-IF
           END-PERFORM.

       RECEBER-NOVA-CIDADE.
           PERFORM WITH TEST AFTER UNTIL CIDADE NOT = SPACES
               ACCEPT CIDADE  AT  1730
               
               IF  CIDADE = SPACES
                   DISPLAY "ERRO NA CIDADE!" AT 2421
               ELSE
                   DISPLAY "    " AT 2421
               END-IF
           END-PERFORM.
           
       RECEBER-NOVO-ESTADO.
           PERFORM WITH TEST AFTER UNTIL ESTADO = "SP" OR ESTADO = "RJ"
               ACCEPT ESTADO  AT  1830
               
               IF  ESTADO NOT = "SP" AND ESTADO NOT = "RJ" 
                   DISPLAY "ERRO NO ESTADO!" AT 2421
               ELSE
                   DISPLAY "    " AT 2421
               END-IF
           END-PERFORM.

       RECEBER-NOVO-CEP.
           PERFORM WITH TEST AFTER UNTIL CEP NOT = SPACES               
               ACCEPT CEP  AT  1930
               
               IF  CEP = SPACES
                   DISPLAY "ERRO NO CEP!" AT 2421
               ELSE
                   DISPLAY "    " AT 2421
               END-IF
           END-PERFORM.
           
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
           STOP  "  ".
           
       FIM-ULTIMA-LINHA.