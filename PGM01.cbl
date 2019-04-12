       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM01  INITIAL.
       AUTHOR. VALDEMAR.
       DATE-WRITTEN. 14/04/2017.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PESSOAS ASSIGN TO "C:\TEMP\PESSOAS.DAT"
               ORGANIZATION INDEXED
               RECORD KEY CPF
               ACCESS RANDOM
               FILE STATUS IS W-COD-ERRO.
               
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PESSOAS
           LABEL RECORD STANDARD.
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
       01  W-COD-ERRO          PIC XX VALUE SPACES.
       01  W-OPCAO             PIC X  VALUE SPACE.
       01  W-INCLUI            PIC X  VALUE SPACE.
       01  W-BRANCO            PIC X(50) VALUE SPACE.
       
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
               10 LINE 06 COLUMN 32 VALUE "INSERIR PESSOA".
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
       
           PERFORM INICIALIZACAO.
           PERFORM PROCESSAMENTO UNTIL W-OPCAO = "N".
           PERFORM FINALIZACAO.
           EXIT PROGRAM.
           
       INICIALIZACAO.
           DISPLAY ERASE.
           DISPLAY TELA01.
           PERFORM   LIMPAR-VARIAVEIS.
           OPEN  I-O  ARQ-PESSOAS.

       PROCESSAMENTO.
           PERFORM FORMATAR-TELA.
           PERFORM RECEBER-DADOS.
           PERFORM GRAVAR-DADOS.
           PERFORM OPCAO-CONTINUIDADE.
           
       FORMATAR-TELA.
	       DISPLAY  "CPF:"   AT  1010.
           DISPLAY  "NOME:"   AT  1110.
           DISPLAY  "ENDERECO:"   AT  1210.
           DISPLAY  "COMPLEMENTO:"   AT  1310.
           DISPLAY  "BAIRRO:"   AT  1410.
           DISPLAY  "CIDADE:"   AT  1510.
           DISPLAY  "ESTADO:"   AT  1610.
           DISPLAY  "CEP:"   AT  1710.
           DISPLAY  "CONFIRMA A INCLUSAO? (S/N):"   AT  2010.
	       DISPLAY  "MENSAGEM: "   AT  2210.
           
       RECEBER-DADOS.
           PERFORM   LIMPAR-VARIAVEIS.
           
           PERFORM WITH TEST AFTER UNTIL CPF NOT = SPACES AND CPF 
           NUMERIC
               ACCEPT CPF  AT  1030
               
               IF  CPF = SPACES OR CPF NOT NUMERIC 
                   DISPLAY "ERRO NO CPF!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.                                                 
                                                                        
           PERFORM WITH TEST AFTER UNTIL NOME NOT = SPACES
               ACCEPT NOME  AT  1130
               
               IF  NOME = SPACES
                   DISPLAY "ERRO NO NOME!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           
           PERFORM WITH TEST AFTER UNTIL ENDERECO NOT = SPACES
               ACCEPT ENDERECO  AT  1230
               
               IF  ENDERECO = SPACES
                   DISPLAY "ERRO NO ENDERECO!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.

           ACCEPT COMPLEMENTO AT 1330
           
           PERFORM WITH TEST AFTER UNTIL BAIRRO NOT = SPACES
               ACCEPT BAIRRO  AT  1430
               
               IF  BAIRRO = SPACES
                   DISPLAY "ERRO NO BAIRRO!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.

           PERFORM WITH TEST AFTER UNTIL CIDADE NOT = SPACES
               ACCEPT CIDADE  AT  1530
               
               IF  CIDADE = SPACES
                   DISPLAY "ERRO NA CIDADE!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.

           PERFORM WITH TEST AFTER UNTIL ESTADO = "SP" OR ESTADO = "RJ"
               ACCEPT ESTADO  AT  1630
               
               IF  ESTADO NOT = "SP" AND ESTADO NOT = "RJ" 
                   DISPLAY "ERRO NO ESTADO!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
                                                                        
           PERFORM WITH TEST AFTER UNTIL CEP NOT = SPACES               
               ACCEPT CEP  AT  1730
               
               IF  CEP = SPACES
                   DISPLAY "ERRO NO CEP!" AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           
       GRAVAR-DADOS.
           PERFORM WITH TEST AFTER UNTIL W-INCLUI = "S" OR "N"
               ACCEPT W-INCLUI AT  2045 WITH UPPER AUTO
               
               IF  W-INCLUI NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA GRAVAR E N PARA DESITIR"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           IF  W-INCLUI = "S"    
               WRITE  REG-PESSOAS  
               IF W-COD-ERRO NOT = "00"
                  DISPLAY "REGISTRO DUPLICADO" AT 2421  WITH
                          FOREGROUND-COLOR 4
               ELSE
                  DISPLAY "                   " AT 2421
               END-IF
           ELSE
               DISPLAY "REGISTRO DESCARTADO" AT 2421
               
               STOP  "<ENTER> PARA CONTINUAR"
               
               DISPLAY W-BRANCO AT 2421
           END-IF.
           
       OPCAO-CONTINUIDADE.
           DISPLAY "DESEJA INCLUIR OUTRO REGISTRO? (S/N):" AT 2220
           
           PERFORM WITH TEST AFTER UNTIL W-OPCAO = "S" OR "N"
                   
               ACCEPT W-OPCAO AT 2265 WITH UPPER AUTO
               
               IF  W-OPCAO NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA INCLUIR OUTRO REGISTRO E N PA
      -                    "RA TERMINAR"  AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           
       LIMPAR-VARIAVEIS.
           INITIALIZE   REG-PESSOAS.
           MOVE SPACES TO W-INCLUI  W-OPCAO.
           
       FINALIZACAO.
           CLOSE  ARQ-PESSOAS.
           DISPLAY "TERMINO DO PROCESSAMENTO" AT 2421.
  		   STOP   "   ".
       FIM.