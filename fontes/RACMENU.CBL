       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACMENU.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO                  PIC X.
       01  TM-TRACO              PIC X(79) VALUE ALL "-".
       01  TM-TRACO-A            PIC X(78) VALUE SPACES.
      *---- SEQUENCIAS VT100 (TERATERM EM MODO VT100)
       01  VT-LIMPA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
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
       01  VT-APAGA.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[K".
       01  VT-HOME.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XX VALUE "[H".
       01  VT-POS.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC X VALUE "[".
           03 VT-LIN            PIC 99.
           03 FILLER            PIC X VALUE ";".
           03 VT-COL            PIC 99.
           03 FILLER            PIC X VALUE "H".
       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABERTURA.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU-PRINCIPAL UNTIL WS-OPCAO = "X".
           DISPLAY VT-LIMPA.
           STOP RUN.
      *---- TELA DE ABERTURA (V22)
       ABERTURA.
           DISPLAY VT-LIMPA.
           MOVE 2 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV " " TM-TRACO-A VT-NORMAL.
           MOVE 4 TO VT-LIN.
           MOVE 31 TO VT-COL.
           DISPLAY VT-POS VT-FORTE "R E T R O L A B S" VT-NORMAL.
           MOVE 6 TO VT-LIN.
           MOVE 15 TO VT-COL.
           DISPLAY VT-POS "  " VT-INV "      " VT-NORMAL "        "
              VT-INV "        " VT-NORMAL "    " VT-INV "  " VT-NORMAL
              "      " VT-INV "  " VT-NORMAL "    " VT-INV
              "          " VT-NORMAL.
           MOVE 7 TO VT-LIN.
           MOVE 15 TO VT-COL.
           DISPLAY VT-POS VT-INV "  " VT-NORMAL "      " VT-INV "  "
              VT-NORMAL "    " VT-INV "  " VT-NORMAL "            "
              VT-INV "    " VT-NORMAL "  " VT-INV "    " VT-NORMAL
              "    " VT-INV "  " VT-NORMAL "        ".
           MOVE 8 TO VT-LIN.
           MOVE 15 TO VT-COL.
           DISPLAY VT-POS VT-INV "          " VT-NORMAL "    " VT-INV
              "  " VT-NORMAL "            " VT-INV "  " VT-NORMAL "  "
              VT-INV "  " VT-NORMAL "  " VT-INV "  " VT-NORMAL "    "
              VT-INV "        " VT-NORMAL "  ".
           MOVE 9 TO VT-LIN.
           MOVE 15 TO VT-COL.
           DISPLAY VT-POS VT-INV "  " VT-NORMAL "      " VT-INV "  "
              VT-NORMAL "    " VT-INV "  " VT-NORMAL "            "
              VT-INV "  " VT-NORMAL "      " VT-INV "  " VT-NORMAL
              "    " VT-INV "  " VT-NORMAL "        ".
           MOVE 10 TO VT-LIN.
           MOVE 15 TO VT-COL.
           DISPLAY VT-POS VT-INV "  " VT-NORMAL "      " VT-INV "  "
              VT-NORMAL "      " VT-INV "        " VT-NORMAL "    "
              VT-INV "  " VT-NORMAL "      " VT-INV "  " VT-NORMAL
              "    " VT-INV "          " VT-NORMAL.
           MOVE 12 TO VT-LIN.
           MOVE 29 TO VT-COL.
           DISPLAY VT-POS VT-FORTE "COMPONENTES ELETRONICOS" VT-NORMAL.
           MOVE 14 TO VT-LIN.
           MOVE 21 TO VT-COL.
           DISPLAY VT-POS "SISTEMA INTEGRADO COMERCIAL - VERSAO 2.4".
           MOVE 16 TO VT-LIN.
           MOVE 22 TO VT-COL.
           DISPLAY VT-POS "DATA-BASE DA DEMONSTRACAO: 15/12/84".
           MOVE 18 TO VT-LIN.
           MOVE 20 TO VT-COL.
           DISPLAY VT-POS "ALTOS 5-15AD  -  MP/M II  -  CIS COBOL 4.5".
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV " " TM-TRACO-A VT-NORMAL.
           MOVE 22 TO VT-LIN.
           MOVE 27 TO VT-COL.
           DISPLAY VT-POS VT-PISCA VT-FORTE
              "TECLE RETURN PARA INICIAR" VT-NORMAL.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV
              "         (C) 1984 RETROLABS ACME  -  "
              "TODOS OS DIREITOS RESERVADOS             " VT-NORMAL
              VT-HOME.
      *A PRIMEIRA LEITURA DO CONSOLE PODE VIR VAZIA (PARAMETROS
      *DO RUNA); A SEGUNDA ESPERA O RETURN DO VISITANTE.
           ACCEPT WS-OPCAO.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           ACCEPT WS-OPCAO.
       MENU-PRINCIPAL.
           DISPLAY VT-LIMPA VT-INV
              " RETROLABS ACME - SISTEMA INTEGRADO COMERCIA"
              "L                     VERSAO 2.4 " VT-NORMAL.
           DISPLAY " COMPONENTES ELETRONICOS                    "
              "             (C) 1984 RETROLABS".
           DISPLAY TM-TRACO.
           DISPLAY " ".
           DISPLAY VT-FORTE
              "      C A D A S T R O S                     "
              "M O V I M E N T O" VT-NORMAL.
           DISPLAY "      -----------------                     "
              "-----------------".
           DISPLAY "   " VT-INV " 1 " VT-NORMAL
              " CLIENTES                            " VT-INV " 5 "
              VT-NORMAL " FATURAMENTO / VENDAS".
           DISPLAY "   " VT-INV " 2 " VT-NORMAL
              " FORNECEDORES                        " VT-INV " 6 "
              VT-NORMAL " COMPRAS".
           DISPLAY "   " VT-INV " 3 " VT-NORMAL
              " PRODUTOS / ESTOQUE                  " VT-INV " 9 "
              VT-NORMAL " MOVIMENTO DE ESTOQUE".
           DISPLAY "   " VT-INV " 4 " VT-NORMAL
              " PRODUTO X FORNECEDOR                " VT-INV " A "
              VT-NORMAL " ITENS DE VENDAS".
           DISPLAY "                                           " VT-INV
              " B " VT-NORMAL " ITENS DE COMPRAS".
           DISPLAY " ".
           DISPLAY VT-FORTE
              "      F I N A N C E I R O                   "
              "C O N T R O L E" VT-NORMAL.
           DISPLAY "      -------------------                   "
              "---------------".
           DISPLAY "   " VT-INV " 7 " VT-NORMAL
              " CONTAS A RECEBER                    " VT-INV " R "
              VT-NORMAL " RELATORIOS".
           DISPLAY "   " VT-INV " 8 " VT-NORMAL
              " CONTAS A PAGAR                      " VT-INV " C "
              VT-NORMAL " AUDITORIA DA BASE".
           DISPLAY "                                           " VT-INV
              " X " VT-NORMAL " ENCERRAR".
           DISPLAY " ".
           DISPLAY TM-TRACO.
           DISPLAY " ALTOS 5-15AD  -  MP/M II  -  CIS COBOL 4.5 "
              "             DADOS NO DRIVE B:".
           DISPLAY TM-TRACO.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV " DIGITE A OPCAO E TECLE RETURN    "
              VT-NORMAL " " VT-INV " X = ENCERRA O SISTEMA        "
              "RETROLABS 1984 " VT-NORMAL VT-HOME.
           MOVE 23 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " OPCAO:" VT-NORMAL.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "a" MOVE "A" TO WS-OPCAO.
           IF WS-OPCAO = "b" MOVE "B" TO WS-OPCAO.
           IF WS-OPCAO = "c" MOVE "C" TO WS-OPCAO.
           IF WS-OPCAO = "r" MOVE "R" TO WS-OPCAO.
           IF WS-OPCAO = "x" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "1" PERFORM CHAMA-CLI.
           IF WS-OPCAO = "2" PERFORM CHAMA-FOR.
           IF WS-OPCAO = "3" PERFORM CHAMA-PRO.
           IF WS-OPCAO = "4" PERFORM CHAMA-PF.
           IF WS-OPCAO = "5" PERFORM CHAMA-VEN.
           IF WS-OPCAO = "6" PERFORM CHAMA-COM.
           IF WS-OPCAO = "7" PERFORM CHAMA-REC.
           IF WS-OPCAO = "8" PERFORM CHAMA-PAG.
           IF WS-OPCAO = "9" PERFORM CHAMA-MOV.
           IF WS-OPCAO = "A" PERFORM CHAMA-IV.
           IF WS-OPCAO = "B" PERFORM CHAMA-IC.
           IF WS-OPCAO = "C" PERFORM CHAMA-CHK.
           IF WS-OPCAO = "R" PERFORM CHAMA-REL.
       CHAMA-CLI.
           CALL "A:RACCLI.INT".
           CANCEL "A:RACCLI.INT".
       CHAMA-FOR.
           CALL "A:RACFOR.INT".
           CANCEL "A:RACFOR.INT".
       CHAMA-PRO.
           CALL "A:RACPRO.INT".
           CANCEL "A:RACPRO.INT".
       CHAMA-PF.
           CALL "A:RACPF.INT".
           CANCEL "A:RACPF.INT".
       CHAMA-VEN.
           CALL "A:RACVEN.INT".
           CANCEL "A:RACVEN.INT".
       CHAMA-COM.
           CALL "A:RACCOM.INT".
           CANCEL "A:RACCOM.INT".
       CHAMA-REC.
           CALL "A:RACREC.INT".
           CANCEL "A:RACREC.INT".
       CHAMA-PAG.
           CALL "A:RACPAG.INT".
           CANCEL "A:RACPAG.INT".
       CHAMA-MOV.
           CALL "A:RACMOV.INT".
           CANCEL "A:RACMOV.INT".
       CHAMA-IV.
           CALL "A:RACIV.INT".
           CANCEL "A:RACIV.INT".
       CHAMA-IC.
           CALL "A:RACIC.INT".
           CANCEL "A:RACIC.INT".
       CHAMA-CHK.
           CALL "A:RACCHK.INT".
           CANCEL "A:RACCHK.INT".
       CHAMA-REL.
           CALL "A:RACREL.INT".
           CANCEL "A:RACREL.INT".
