       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTVT.
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *TESTE DE TELA VT100 (TERATERM) SEM CONSOLE IS CRT.
      *AS SEQUENCIAS ESC SAO ENVIADAS PELO PROPRIO PROGRAMA
      *COM LITERAIS HEXADECIMAIS X"1B". SEM ACCEPT FROM DATE/TIME
      *(NAO SUPORTADO PELO CIS COBOL 4.5 - ERRO 110).
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VT-LIMPA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[2J".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[H".
       01  VT-INV.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
       01  VT-PISCA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[5m".
       01  VT-FORTE.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
       01  VT-NORMAL.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  VT-POS.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC X VALUE "[".
           03 VT-LIN            PIC 99.
           03 FILLER            PIC X VALUE ";".
           03 VT-COL            PIC 99.
           03 FILLER            PIC X VALUE "H".
       01  WS-TECLA             PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY VT-LIMPA.
           DISPLAY VT-INV
              " RETROLABS ACME - TESTE DE TELA VT100      "
              "                          15/12/84 " VT-NORMAL.
           DISPLAY "--------------------------------------------"
              "------------------------------------".
           DISPLAY "  1. ESTA LINHA E NORMAL".
           DISPLAY "  2. " VT-FORTE "ESTA LINHA E EM ALTA INTENSIDADE"
              VT-NORMAL.
           DISPLAY "  3. " VT-INV " ESTA LINHA E EM VIDEO INVERSO "
              VT-NORMAL.
           DISPLAY "  4. " VT-PISCA "ESTA LINHA DEVE PISCAR"
              VT-NORMAL.
           DISPLAY "  5. " VT-INV VT-PISCA " INVERSO E PISCANTE "
              VT-NORMAL.
           MOVE 12 TO VT-LIN.
           MOVE 30 TO VT-COL.
           DISPLAY VT-POS "<-- LINHA 12, COLUNA 30".
           MOVE 16 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS "  6. ESTA LINHA DEVE ESTAR NA LINHA 16".
           MOVE 20 TO VT-LIN.
           DISPLAY VT-POS "  TECLE RETURN PARA ENCERRAR".
           ACCEPT WS-TECLA.
           DISPLAY VT-NORMAL VT-LIMPA.
           STOP RUN.
