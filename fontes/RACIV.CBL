       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACIV.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-ITEM ASSIGN TO "B:ITVEN.DAT"
              ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
              RECORD KEY IS IV-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-ITEM RECORD CONTAINS 45 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 REG-ITEM-VENDA.
          03 IV-CHAVE.
             05 IV-VENDA            PIC 9(8).
             05 IV-ITEM             PIC 9(3).
          03 IV-PRODUTO             PIC 9(7).
          03 IV-QUANTIDADE          PIC 9(6).
          03 IV-PRECO               PIC 9(6)V99.
          03 IV-DESCONTO            PIC 99V9.
          03 IV-TOTAL               PIC 9(8)V99.
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01 WS-OPCAO                  PIC X.
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
       01  TL-MSG               PIC X(76) VALUE SPACES.
       01  TL-MSG-R REDEFINES TL-MSG.
           03 TL-MSG-1          PIC X.
           03 FILLER            PIC X(75).
       01  TL-PROMPT            PIC X(40) VALUE SPACES.
       01  TL-TRACO             PIC X(79) VALUE ALL "-".
       01  TL-TITULO            PIC X(78) VALUE SPACES.
       01  TL-TIT-PADRAO.
           03 FILLER            PIC X(39) VALUE
              "ITENS DE VENDAS                        ".
           03 FILLER            PIC X(39) VALUE
              "                       RETROLABS ACME  ".
       01  TL-RODAPE            PIC X(79) VALUE SPACES.
       01  TL-RODAPE-R REDEFINES TL-RODAPE.
           03 TL-ROD-A          PIC X(34).
           03 TL-ROD-B          PIC X(45).
       01  TL-TEM               PIC X VALUE "N".
       01  TL-DTW               PIC 9(6).
       01  TL-DTW-R REDEFINES TL-DTW.
           03 TL-DTW-AA         PIC 99.
           03 TL-DTW-MM         PIC 99.
           03 TL-DTW-DD         PIC 99.
       01  TL-DT-ED.
           03 TL-DE-DD          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TL-DE-MM          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 TL-DE-AA          PIC 99.
       01  TL-ROD-PADRAO.
           03 FILLER            PIC X(39) VALUE
              " CONSULTA POR CHAVE  -  RETURN APOS CAD".
           03 FILLER            PIC X(39) VALUE
              "A CAMPO                                ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TF-R01.
           03 FILLER            PIC X(14) VALUE
              " PEDIDO.....: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF01-1               PIC 9(8).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(16) VALUE
              "          ITEM: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF01-2               PIC 999.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R03.
           03 FILLER            PIC X(20) VALUE
              "    PRODUTO.......: ".
           03 TF03-1               PIC 9(7).
       01  TF-R04.
           03 FILLER            PIC X(20) VALUE
              "    QUANTIDADE....: ".
           03 TF04-1               PIC ZZZ,ZZ9.
       01  TF-R05.
           03 FILLER            PIC X(20) VALUE
              "    PRECO UNIT....: ".
           03 TF05-1               PIC ZZZ,ZZ9.99.
       01  TF-R06.
           03 FILLER            PIC X(20) VALUE
              "    DESCONTO %....: ".
           03 TF06-1               PIC ZZZZZ9.9.
       01  TF-R07.
           03 FILLER            PIC X(20) VALUE
              "    TOTAL ITEM....: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 TF07-1               PIC Z,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  WS-ACHOU             PIC X VALUE "N".
       01  NV-POS               PIC 9999 VALUE ZERO.
       01  NV-N                 PIC 9999 VALUE ZERO.
       01  NV-I                 PIC 9999 VALUE ZERO.
       01  NV-J                 PIC 9999 VALUE ZERO.
       01  NV-K                 PIC 9(6) VALUE ZERO.
       01  NV-FIM               PIC X VALUE "N".
       01  NV-ACHOU             PIC X VALUE "N".
       01  NV-TAB-OK            PIC X VALUE "N".
       01  NV-CHEIA             PIC X VALUE "S".
       01  NV-TELA-OK           PIC X VALUE "N".
       01  NV-KSAVE             PIC X(11).
       01  NV-KATU              PIC X(11) VALUE SPACES.
       01  NV-ED-POS            PIC ZZZ9.
       01  NV-ED-N              PIC ZZZ9.
       01  NV-ENT               PIC X(20) VALUE SPACES.
       01  NV-ENT-R REDEFINES NV-ENT.
           03 NV-ENT-C          PIC X OCCURS 20 TIMES.
       01  NV-MSG-NC.
           03 FILLER            PIC X(4) VALUE "*** ".
           03 NV-MK             PIC X(11).
           03 FILLER            PIC X(35) VALUE
              " NAO EXISTE - CONFIRA O CODIGO    ".
           03 FILLER            PIC X(3) VALUE "***".
       01  NV-ROD1              PIC X(34) VALUE
           " = PROXIMO  I INICIO              ".
       01  NV-TAB.
           03 NV-TK             PIC X(11) OCCURS 1 TIMES.
       01  NV-ROD2.
           03 FILLER            PIC X(39) VALUE
              " 2 BUSCA POR CHAVE        X SAI        ".
           03 FILLER            PIC X(5) VALUE
              "     ".
      *---- ENTRADA AMIGAVEL (V22)
       01  LC-ENT               PIC X(20) VALUE SPACES.
       01  LC-ENT-R REDEFINES LC-ENT.
           03 LC-C              PIC X OCCURS 20 TIMES.
       01  LC-DIGS              PIC X(10) VALUE "0123456789".
       01  LC-DIGS-R REDEFINES LC-DIGS.
           03 LC-DG             PIC X OCCURS 10 TIMES.
       01  LC-NUM               PIC 9(12) VALUE ZERO.
       01  LC-OK                PIC X VALUE "S".
       01  LC-BARRA             PIC X VALUE "N".
       01  LC-ND                PIC 99 VALUE ZERO.
       01  LC-I                 PIC 99 VALUE ZERO.
       01  LC-J                 PIC 99 VALUE ZERO.
       01  LC-D                 PIC 99 VALUE ZERO.
       01  LC-DATA              PIC 9(6) VALUE ZERO.
       01  LC-DATA-R REDEFINES LC-DATA.
           03 LC-AA             PIC 99.
           03 LC-MM             PIC 99.
           03 LC-DD             PIC 99.
       01  LC-DMY               PIC 9(6) VALUE ZERO.
       01  LC-DMY-R REDEFINES LC-DMY.
           03 LC-X1             PIC 99.
           03 LC-X2             PIC 99.
           03 LC-X3             PIC 99.
       01  LC-VAL               PIC 9(10)V99 VALUE ZERO.
       01  LC-SP                PIC 99 VALUE ZERO.
       01  LC-NDEC              PIC 99 VALUE ZERO.
       01  LC-DIAS              PIC 999 VALUE ZERO.
       01  LC-K                 PIC 999 VALUE ZERO.
       01  LC-FIMMES            PIC 99 VALUE ZERO.
       01  LC-TAB-MES           PIC X(24) VALUE
           "312931303130313130313031".
       01  LC-TAB-MES-R REDEFINES LC-TAB-MES.
           03 LC-UD             PIC 99 OCCURS 12 TIMES.
       01  LC-DT-ED.
           03 LC-E-DD           PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 LC-E-MM           PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 LC-E-AA           PIC 99.
       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQ-ITEM.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           MOVE ZERO TO NV-POS.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
           MOVE "N" TO NV-TELA-OK.
           MOVE SPACE TO WS-OPCAO.
           PERFORM CONSULTA UNTIL WS-OPCAO = "X".
           CLOSE ARQ-ITEM.
           GO TO FIM-PROGRAMA.
       CONSULTA.
           PERFORM TELA.
           MOVE "OPCAO OU NUMERO DO PEDIDO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-MSG.
           PERFORM NV-LE-OPCAO.
           IF WS-OPCAO = "2" PERFORM BUSCA.
           PERFORM NV-ACAO.
       BUSCA.
           MOVE "NUMERO DO PEDIDO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO IV-VENDA.
           MOVE "ITEM:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO IV-ITEM.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-ITEM
              INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
              MOVE IV-CHAVE TO NV-KSAVE
              PERFORM NV-SEGUINTE
           ELSE
              PERFORM NV-ACHA
              IF NV-TAB-OK = "S"
                 PERFORM NV-LE-POS
              ELSE
                 PERFORM NV-APOS-LER.
       TELA.
           MOVE "N" TO NV-CHEIA.
           IF NV-TELA-OK = "N"
              MOVE "S" TO NV-CHEIA
              MOVE "S" TO NV-TELA-OK
              MOVE TL-TIT-PADRAO TO TL-TITULO
              PERFORM TL-TITULO-P.
           IF TL-TEM = "S"
              PERFORM MOSTRA
           ELSE
              MOVE 3 TO VT-LIN
              MOVE 1 TO VT-COL
              DISPLAY VT-POS VT-APAGA " ARQUIVO VAZIO".
           PERFORM TL-MSG-P.
           PERFORM NV-BARRA-P THRU NV-BP-FIM.
           IF NV-CHEIA = "S" PERFORM NV-RODAPE-P.
       MOSTRA.
           MOVE IV-VENDA TO TF01-1.
           MOVE IV-ITEM TO TF01-2.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TF-R01.
           DISPLAY VT-APAGA
              TL-TRACO.
           MOVE IV-PRODUTO TO TF03-1.
           DISPLAY VT-APAGA
              TF-R03.
           MOVE IV-QUANTIDADE TO TF04-1.
           DISPLAY VT-APAGA
              TF-R04.
           MOVE IV-PRECO TO TF05-1.
           DISPLAY VT-APAGA
              TF-R05.
           MOVE IV-DESCONTO TO TF06-1.
           DISPLAY VT-APAGA
              TF-R06.
           MOVE IV-TOTAL TO TF07-1.
           DISPLAY VT-APAGA
              TF-R07.
      *---- ROTINAS DE TELA VT100 (V17)
       TL-TITULO-P.
           DISPLAY VT-LIMPA VT-INV " " TL-TITULO VT-NORMAL.
           DISPLAY TL-TRACO.
       TL-MSG-P.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           IF TL-MSG = SPACES
              DISPLAY VT-POS VT-APAGA
           ELSE
              IF TL-MSG-1 = "*"
                 DISPLAY VT-POS VT-APAGA VT-INV VT-PISCA " " TL-MSG
                         " " VT-NORMAL
              ELSE
                 DISPLAY VT-POS VT-APAGA VT-INV " " TL-MSG " "
                         VT-NORMAL.
       TL-PERGUNTA.
           MOVE 23 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 22 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-FORTE " " TL-PROMPT VT-NORMAL.
       TL-RODAPE-P.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV TL-ROD-A VT-NORMAL " " VT-INV TL-ROD-B
                   VT-NORMAL VT-HOME.
       TL-FMT-DATA.
           MOVE TL-DTW-DD TO TL-DE-DD.
           MOVE TL-DTW-MM TO TL-DE-MM.
           MOVE TL-DTW-AA TO TL-DE-AA.
      *---- NAVEGACAO SEQUENCIAL (SEM TABELA NA MEMORIA)
      *RETURN=PROXIMO, I=INICIO, CODIGO DIRETO (MOSTRA O SEGUINTE).
       NV-CARREGA.
           MOVE ZERO TO NV-N.
           MOVE ZERO TO NV-POS.
           MOVE LOW-VALUES TO IV-CHAVE.
           START ARQ-ITEM KEY IS NOT < IV-CHAVE
              INVALID KEY GO TO NV-CC-FIM.
           MOVE "N" TO NV-FIM.
           READ ARQ-ITEM NEXT RECORD
              AT END MOVE "S" TO NV-FIM.
           IF NV-FIM = "N"
              MOVE 1 TO NV-N
              MOVE IV-CHAVE TO NV-KATU.
       NV-CC-FIM.
           EXIT.
       NV-LE-POS.
           MOVE NV-KATU TO IV-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-ITEM
              INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
       NV-APOS-LER.
           IF NV-ACHOU = "S" MOVE IV-CHAVE TO NV-KATU.
           IF NV-ACHOU = "S"
              MOVE "S" TO TL-TEM
           ELSE
              MOVE "N" TO TL-TEM.
           MOVE SPACE TO NV-FIM.
       NV-LE-OPCAO.
           MOVE SPACES TO NV-ENT.
           ACCEPT NV-ENT.
           MOVE SPACE TO WS-OPCAO.
           IF NV-ENT = SPACES
              MOVE "6" TO WS-OPCAO
           ELSE
              IF NV-ENT-C (2) = SPACE
                 MOVE NV-ENT-C (1) TO WS-OPCAO
              ELSE
                 MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "x" MOVE "X" TO WS-OPCAO.
           IF WS-OPCAO = "i" MOVE "I" TO WS-OPCAO.
           IF WS-OPCAO = "0" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "1" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "3" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "4" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "5" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "6" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "7" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "8" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "9" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "+" MOVE "6" TO WS-OPCAO.
           IF WS-OPCAO = "=" MOVE "6" TO WS-OPCAO.
       NV-ACAO.
           IF WS-OPCAO = "6" PERFORM NV-PROXIMO.
           IF WS-OPCAO = "I" PERFORM NV-PRIMEIRO.
           IF WS-OPCAO = "K"
              PERFORM NV-K-CONV
              PERFORM NV-VAI-CHAVE.
       NV-K-CONV.
           MOVE NV-ENT TO LC-ENT.
           PERFORM LC-CONVERTE.
           IF LC-OK = "S"
              IF LC-ND < 9
                 MOVE LC-NUM TO IV-VENDA
                 MOVE 1 TO IV-ITEM
              ELSE
                 MOVE NV-ENT TO IV-CHAVE
           ELSE
              MOVE NV-ENT TO IV-CHAVE.
       NV-VAI-CHAVE.
           MOVE IV-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-ITEM
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-SEGUINTE.
       NV-SEGUINTE.
           MOVE NV-KSAVE TO IV-CHAVE.
           MOVE "S" TO NV-ACHOU.
           START ARQ-ITEM KEY IS NOT < IV-CHAVE
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              READ ARQ-ITEM NEXT RECORD
                 AT END MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-PRIMEIRO.
           MOVE NV-KSAVE TO NV-MK.
           MOVE NV-MSG-NC TO TL-MSG.
       NV-PROXIMO.
           IF NV-N = ZERO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              MOVE NV-KATU TO IV-CHAVE
              MOVE "S" TO NV-ACHOU
              START ARQ-ITEM KEY IS GREATER THAN IV-CHAVE
                 INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-N > ZERO
              IF NV-ACHOU = "S"
                 READ ARQ-ITEM NEXT RECORD
                    AT END MOVE "N" TO NV-ACHOU.
           IF NV-N > ZERO
              IF NV-ACHOU = "S"
                 PERFORM NV-APOS-LER
              ELSE
                 PERFORM NV-PRIMEIRO
                 MOVE "FIM DO ARQUIVO - VOLTOU AO PRIMEIRO REGISTRO"
                   TO TL-MSG.
       NV-PRIMEIRO.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
       NV-ACHA.
           MOVE IV-CHAVE TO NV-KATU.
           MOVE 1 TO NV-POS.
           MOVE "S" TO NV-TAB-OK.
       NV-INSERE.
           MOVE IV-CHAVE TO NV-KATU.
           MOVE 1 TO NV-N.
           MOVE 1 TO NV-POS.
           PERFORM NV-LE-POS.
       NV-REMOVE.
           MOVE NV-KATU TO NV-KSAVE.
           PERFORM NV-PROXIMO.
       NV-BARRA-P.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
       NV-BP-FIM.
           EXIT.
       NV-RODAPE-P.
           MOVE 24 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-INV NV-ROD1 VT-NORMAL " " VT-INV NV-ROD2
                   VT-NORMAL VT-HOME.
       LIMPA-REGISTRO.
           MOVE "N" TO TL-TEM.
       LC-LE-CODIGO.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           PERFORM LC-CONVERTE.
       LC-CONVERTE.
           MOVE ZERO TO LC-NUM.
           MOVE ZERO TO LC-ND.
           MOVE "S" TO LC-OK.
           MOVE "N" TO LC-BARRA.
           MOVE 1 TO LC-I.
           PERFORM LC-DIG UNTIL LC-I > 20.
       LC-DIG.
           IF LC-C (LC-I) NOT = SPACE
              IF LC-C (LC-I) = "/"
                 MOVE "S" TO LC-BARRA
              ELSE
                 MOVE ZERO TO LC-J
                 MOVE 99 TO LC-D
                 PERFORM LC-PROCURA UNTIL LC-J > 9
                 IF LC-D = 99
                    MOVE "N" TO LC-OK
                 ELSE
                    MULTIPLY 10 BY LC-NUM
                    ADD LC-D TO LC-NUM
                    ADD 1 TO LC-ND.
           ADD 1 TO LC-I.
       LC-PROCURA.
           ADD 1 TO LC-J.
           IF LC-DG (LC-J) = LC-C (LC-I)
              SUBTRACT 1 FROM LC-J GIVING LC-D
              MOVE 10 TO LC-J.
      *---- LC-LE-VALOR: VALOR COM DECIMAIS (6.40  6,40  1.234,56)
      *O ULTIMO PONTO OU VIRGULA SEPARA OS DECIMAIS. SAIDA: LC-VAL
       FIM-PROGRAMA.
           EXIT PROGRAM.
