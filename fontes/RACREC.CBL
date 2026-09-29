       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACREC.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - CONTAS A RECEBER
      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-REC ASSIGN TO "B:REC.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-CHAVE.
           SELECT ARQ-CLI ASSIGN TO "B:CLI.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-REC RECORD CONTAINS 102 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-RECEBER.
           03 CR-CHAVE.
              05 CR-TITULO           PIC 9(8).
              05 CR-PARCELA          PIC 99.
           03 CR-CLIENTE             PIC 9(6).
           03 CR-VENDA               PIC 9(8).
           03 CR-FATURA              PIC 9(8).
           03 CR-EMISSAO             PIC 9(6).
           03 CR-VENCIMENTO          PIC 9(6).
           03 CR-VALOR               PIC 9(8)V99.
           03 CR-JUROS               PIC 9(6)V99.
           03 CR-DESCONTO            PIC 9(6)V99.
           03 CR-RECEBIDO            PIC 9(8)V99.
           03 CR-DT-PAGTO            PIC 9(6).
           03 CR-PORTADOR            PIC 9(3).
           03 CR-NOSSO-NUMERO        PIC X(12).
           03 CR-SITUACAO            PIC X.
       FD  ARQ-CLI RECORD CONTAINS 341 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-CLIENTE.
           03 CL-CHAVE.
              05 CL-CODIGO              PIC 9(6).
           03 CL-RAZAO               PIC X(38).
           03 CL-FANTASIA            PIC X(25).
           03 CL-CGC-MF              PIC X(18).
           03 CL-INSC-EST            PIC X(15).
           03 CL-ENDERECO            PIC X(35).
           03 CL-BAIRRO              PIC X(20).
           03 CL-CIDADE              PIC X(22).
           03 CL-UF                  PIC X(2).
           03 CL-CEP                 PIC 9(5).
           03 CL-DDD                 PIC 9(3).
           03 CL-TELEFONE            PIC X(10).
           03 CL-TELEX               PIC X(15).
           03 CL-CONTATO             PIC X(25).
           03 CL-CARGO               PIC X(20).
           03 CL-PRACA               PIC X(22).
           03 CL-COND-PAGTO          PIC 9(3).
           03 CL-VENDEDOR            PIC 9(3).
           03 CL-LIM-CREDITO         PIC 9(8)V99.
           03 CL-DESCONTO            PIC 99V9.
           03 CL-TOTAL-ANO           PIC 9(9)V99.
           03 CL-SALDO               PIC 9(8)V99.
           03 CL-ULT-COMPRA          PIC 9(6).
           03 CL-TIT-ABERTOS         PIC 9(4).
           03 CL-MAIOR-ATRASO        PIC 9(3).
           03 CL-DT-CADASTRO         PIC 9(6).
           03 CL-SITUACAO            PIC X.
       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO                  PIC X.
       01  WS-ACHOU                  PIC X.
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
              "CONTAS A RECEBER                       ".
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
              " 1-CONSULTA     2-BAIXA     X-RETORNA  ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-M-BAIXA           PIC X(30) VALUE
           "*** ERRO NA BAIXA ***".
       01  TF-R01.
           03 FILLER            PIC X(14) VALUE
              " TITULO.....: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF01-1               PIC 9(8).
           03 FILLER            PIC X(3) VALUE
              " / ".
           03 TF01-2               PIC 99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(29) VALUE
              "                             ".
           03 FILLER            PIC X(10) VALUE
              "SITUACAO: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(1) VALUE
              " ".
           03 TF01-3               PIC X(9).
           03 FILLER            PIC X(1) VALUE
              " ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R02.
           03 FILLER            PIC X(14) VALUE
              " CLIENTE....: ".
           03 TF02-1               PIC 9(6).
           03 FILLER            PIC X(2) VALUE
              "  ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF02-2               PIC X(38).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R03.
           03 FILLER            PIC X(14) VALUE
              " PEDIDO.....: ".
           03 TF03-1               PIC 9(8).
           03 FILLER            PIC X(16) VALUE
              "        FATURA: ".
           03 TF03-2               PIC 9(8).
       01  TF-R05.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(40) VALUE
              "    D A T A S                          V".
           03 FILLER            PIC X(12) VALUE
              " A L O R E S".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R06.
           03 FILLER            PIC X(40) VALUE
              "    ---------                          -".
           03 FILLER            PIC X(12) VALUE
              "------------".
       01  TF-R07.
           03 FILLER            PIC X(20) VALUE
              "    EMISSAO.......: ".
           03 TF07-1               PIC X(8).
           03 FILLER            PIC X(27) VALUE
              "           VALOR.........: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 TF07-2               PIC ZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R08.
           03 FILLER            PIC X(20) VALUE
              "    VENCIMENTO....: ".
           03 TF08-1               PIC X(8).
           03 FILLER            PIC X(27) VALUE
              "           JUROS.........: ".
           03 TF08-2               PIC ZZ,ZZZ,ZZ9.99.
       01  TF-R09.
           03 FILLER            PIC X(20) VALUE
              "    PAGAMENTO.....: ".
           03 TF09-1               PIC X(8).
           03 FILLER            PIC X(27) VALUE
              "           DESCONTO......: ".
           03 TF09-2               PIC ZZ,ZZZ,ZZ9.99.
       01  TF-R10.
           03 FILLER            PIC X(20) VALUE
              "    PORTADOR......: ".
           03 TF10-1               PIC ZZZZZZZ9.
           03 FILLER            PIC X(27) VALUE
              "           RECEBIDO......: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF10-2               PIC ZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R11.
           03 FILLER            PIC X(20) VALUE
              "    NOSSO NUMERO..: ".
           03 TF11-1               PIC X(12).
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
       01  NV-KSAVE             PIC X(10).
       01  NV-KATU              PIC X(10) VALUE SPACES.
       01  NV-ED-POS            PIC ZZZ9.
       01  NV-ED-N              PIC ZZZ9.
       01  NV-ENT               PIC X(20) VALUE SPACES.
       01  NV-ENT-R REDEFINES NV-ENT.
           03 NV-ENT-C          PIC X OCCURS 20 TIMES.
       01  NV-MSG-NC.
           03 FILLER            PIC X(4) VALUE "*** ".
           03 NV-MK             PIC X(10).
           03 FILLER            PIC X(35) VALUE
              " NAO EXISTE - CONFIRA O CODIGO    ".
           03 FILLER            PIC X(3) VALUE "***".
       01  NV-ROD1              PIC X(34) VALUE
           " = PROX  - ANT  I=INICIO  F=FIM   ".
       01  NV-TAB.
           03 NV-TK             PIC X(10) OCCURS 300 TIMES.
       01  NV-ROD2.
           03 FILLER            PIC X(39) VALUE
              " 1 CONSULTA   2 BAIXA   X SAI          ".
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
       01  WS-PR-DATA.
           03 FILLER            PIC X(19) VALUE "DATA DO PAGAMENTO [".
           03 WP-DATA           PIC X(8).
           03 FILLER            PIC X(2) VALUE "]:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARQ-REC ARQ-CLI.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           MOVE ZERO TO NV-POS.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
           MOVE "N" TO NV-TELA-OK.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU UNTIL WS-OPCAO = "X".
           CLOSE ARQ-REC ARQ-CLI.
           GO TO FIM-PROGRAMA.
       MENU.
           PERFORM TELA-TITULO.
           MOVE "OPCAO OU NUMERO DO TITULO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-MSG.
           PERFORM NV-LE-OPCAO.
           IF WS-OPCAO = "1" PERFORM CONSULTA.
           IF WS-OPCAO = "2" PERFORM BAIXA THRU BAIXA-FIM.
           PERFORM NV-ACAO.
       CHAVE.
           MOVE "NUMERO DO TITULO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO CR-TITULO.
           MOVE "PARCELA:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO CR-PARCELA.
       LOCALIZA.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-REC
               INVALID KEY MOVE "N" TO WS-ACHOU.
       CONSULTA.
           PERFORM CHAVE.
           PERFORM LOCALIZA.
           IF WS-ACHOU = "N"
               MOVE CR-CHAVE TO NV-KSAVE
               PERFORM NV-SEGUINTE
           ELSE
               PERFORM NV-ACHA
               IF NV-TAB-OK = "S"
                  PERFORM NV-LE-POS
               ELSE
                  PERFORM NV-APOS-LER.
      *A BAIXA AGE SOBRE O TITULO QUE ESTA NA TELA.
       BAIXA.
           IF TL-TEM NOT = "S"
               MOVE "*** NENHUM TITULO NA TELA ***" TO TL-MSG
               GO TO BAIXA-FIM.
           MOVE NV-KATU TO CR-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-REC
               INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
           IF NV-ACHOU = "N"
               MOVE "*** TITULO NAO ENCONTRADO ***" TO TL-MSG
               GO TO BAIXA-FIM.
           IF CR-SITUACAO = "B"
               MOVE "*** TITULO JA BAIXADO ***" TO TL-MSG
               GO TO BAIXA-FIM.
           PERFORM TELA-TITULO.
           MOVE 841215 TO LC-DATA.
           PERFORM LC-FMT.
           MOVE LC-DT-ED TO WP-DATA.
           MOVE WS-PR-DATA TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-DATA.
           MOVE LC-DATA TO CR-DT-PAGTO.
           MOVE CR-CLIENTE TO CL-CODIGO.
           MOVE "S" TO WS-ACHOU.
           READ ARQ-CLI
               INVALID KEY MOVE "N" TO WS-ACHOU.
           IF WS-ACHOU = "N"
               MOVE "*** CLIENTE NAO ENCONTRADO ***" TO TL-MSG
               GO TO BAIXA-FIM.
           MOVE CR-VALOR TO CR-RECEBIDO.
           MOVE ZERO TO CR-JUROS CR-DESCONTO.
           IF CR-VALOR > CL-SALDO
               MOVE ZERO TO CL-SALDO
           ELSE
               SUBTRACT CR-VALOR FROM CL-SALDO.
           IF CL-TIT-ABERTOS > ZERO
               SUBTRACT 1 FROM CL-TIT-ABERTOS.
           REWRITE REG-CLIENTE
               INVALID KEY DISPLAY "ERRO ATUALIZANDO CLIENTE".
           MOVE "B" TO CR-SITUACAO.
           REWRITE REG-RECEBER
               INVALID KEY MOVE TL-M-BAIXA TO TL-MSG.
           IF TL-MSG = SPACES
              MOVE "TITULO BAIXADO INTEGRALMENTE" TO TL-MSG.
       BAIXA-FIM.
           EXIT.
       TELA-TITULO.
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
              DISPLAY VT-POS VT-APAGA " NENHUM TITULO NO ARQUIVO".
           PERFORM TL-MSG-P.
           PERFORM NV-BARRA-P THRU NV-BP-FIM.
           IF NV-CHEIA = "S" PERFORM NV-RODAPE-P.
       LE-NOME.
           MOVE CR-CLIENTE TO CL-CODIGO.
           READ ARQ-CLI
               INVALID KEY MOVE SPACES TO CL-RAZAO.
       MOSTRA.
           MOVE CR-TITULO TO TF01-1.
           MOVE CR-PARCELA TO TF01-2.
           MOVE "EM ABERTO" TO TF01-3.
           IF CR-SITUACAO = "B" MOVE "BAIXADO  " TO TF01-3.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TF-R01.
           MOVE CR-CLIENTE TO TF02-1.
           MOVE CL-RAZAO TO TF02-2.
           DISPLAY VT-APAGA
              TF-R02.
           MOVE CR-VENDA TO TF03-1.
           MOVE CR-FATURA TO TF03-2.
           DISPLAY VT-APAGA
              TF-R03.
           DISPLAY VT-APAGA
              TL-TRACO.
           DISPLAY VT-APAGA
              TF-R05.
           DISPLAY VT-APAGA
              TF-R06.
           MOVE CR-EMISSAO TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF07-1.
           IF CR-EMISSAO = ZERO MOVE SPACES TO TF07-1.
           MOVE CR-VALOR TO TF07-2.
           DISPLAY VT-APAGA
              TF-R07.
           MOVE CR-VENCIMENTO TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF08-1.
           IF CR-VENCIMENTO = ZERO MOVE SPACES TO TF08-1.
           MOVE CR-JUROS TO TF08-2.
           DISPLAY VT-APAGA
              TF-R08.
           MOVE CR-DT-PAGTO TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF09-1.
           IF CR-DT-PAGTO = ZERO MOVE SPACES TO TF09-1.
           MOVE CR-DESCONTO TO TF09-2.
           DISPLAY VT-APAGA
              TF-R09.
           MOVE CR-PORTADOR TO TF10-1.
           MOVE CR-RECEBIDO TO TF10-2.
           DISPLAY VT-APAGA
              TF-R10.
           MOVE CR-NOSSO-NUMERO TO TF11-1.
           DISPLAY VT-APAGA
              TF-R11.
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
      *---- NAVEGACAO (V21): TABELA DE CHAVES EM MEMORIA,
      *RETURN=PROXIMO, - ANTERIOR, I INICIO, F FIM, CODIGO DIRETO.
       NV-CARREGA.
           MOVE ZERO TO NV-N.
           MOVE LOW-VALUES TO CR-CHAVE.
           START ARQ-REC KEY IS NOT < CR-CHAVE
              INVALID KEY GO TO NV-CC-FIM.
           MOVE "N" TO NV-FIM.
           PERFORM NV-LER UNTIL NV-FIM = "S".
       NV-CC-FIM.
           EXIT.
       NV-LER.
           READ ARQ-REC NEXT RECORD
              AT END MOVE "S" TO NV-FIM.
           IF NV-FIM = "N"
              IF NV-N < 300
                 ADD 1 TO NV-N
                 MOVE CR-CHAVE TO NV-TK (NV-N).
       NV-LE-POS.
           MOVE NV-TK (NV-POS) TO CR-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-REC
              INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
       NV-APOS-LER.
           IF NV-ACHOU = "S" MOVE CR-CHAVE TO NV-KATU.
           IF NV-ACHOU = "S"
              MOVE "S" TO TL-TEM
              PERFORM LE-NOME
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
           IF WS-OPCAO = "f" MOVE "F" TO WS-OPCAO.
      *UM DIGITO QUE NAO E OPCAO DESTA TELA E UM CODIGO (EX.: 5)
           IF WS-OPCAO = "0" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "3" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "4" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "5" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "6" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "7" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "8" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "9" MOVE "K" TO WS-OPCAO.
           IF WS-OPCAO = "+" MOVE "6" TO WS-OPCAO.
           IF WS-OPCAO = "=" MOVE "6" TO WS-OPCAO.
           IF WS-OPCAO = "-" MOVE "5" TO WS-OPCAO.
       NV-ACAO.
           IF WS-OPCAO = "5" PERFORM NV-ANTERIOR.
           IF WS-OPCAO = "6" PERFORM NV-PROXIMO.
           IF WS-OPCAO = "I" PERFORM NV-PRIMEIRO.
           IF WS-OPCAO = "F" PERFORM NV-ULTIMO.
           IF WS-OPCAO = "K"
              PERFORM NV-K-CONV
              PERFORM NV-VAI-CHAVE.
      *CODIGO DIGITADO NO "OPCAO": ACEITA SEM OS ZEROS A ESQUERDA
       NV-K-CONV.
           MOVE NV-ENT TO LC-ENT.
           PERFORM LC-CONVERTE.
           IF LC-OK = "S"
              IF LC-ND < 9
                 MOVE LC-NUM TO CR-TITULO
                 MOVE 1 TO CR-PARCELA
              ELSE
                 MOVE NV-ENT TO CR-CHAVE
           ELSE
              MOVE NV-ENT TO CR-CHAVE.

       NV-VAI-CHAVE.
           MOVE CR-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-REC
              INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-ACHA
              IF NV-TAB-OK = "S"
                 PERFORM NV-LE-POS
              ELSE
                 PERFORM NV-APOS-LER
           ELSE
              PERFORM NV-SEGUINTE.
       NV-SEGUINTE.
           IF NV-N = ZERO
              PERFORM LIMPA-REGISTRO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              IF NV-POS > ZERO PERFORM NV-LE-POS.
           IF NV-N > ZERO
              MOVE NV-KSAVE TO NV-MK
              MOVE NV-MSG-NC TO TL-MSG.
       NV-PROXIMO.
           IF NV-N = ZERO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              IF NV-POS NOT < NV-N
                 MOVE 1 TO NV-POS
                 PERFORM NV-LE-POS
                 MOVE "FIM DO ARQUIVO - VOLTOU AO PRIMEIRO REGISTRO"
                   TO TL-MSG
              ELSE
                 ADD 1 TO NV-POS
                 PERFORM NV-LE-POS.
       NV-ANTERIOR.
           IF NV-N = ZERO
              MOVE "*** ARQUIVO VAZIO ***" TO TL-MSG
           ELSE
              IF NV-POS < 2
                 MOVE NV-N TO NV-POS
                 PERFORM NV-LE-POS
                 MOVE "INICIO DO ARQUIVO - FOI PARA O ULTIMO REGISTRO"
                   TO TL-MSG
              ELSE
                 SUBTRACT 1 FROM NV-POS
                 PERFORM NV-LE-POS.
       NV-PRIMEIRO.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
       NV-ULTIMO.
           IF NV-N > ZERO
              MOVE NV-N TO NV-POS
              PERFORM NV-LE-POS.
       NV-ACHA.
           MOVE "N" TO NV-TAB-OK.
           MOVE 1 TO NV-I.
           PERFORM NV-TESTA UNTIL NV-I > NV-N.
       NV-TESTA.
           IF NV-TK (NV-I) = CR-CHAVE
              MOVE NV-I TO NV-POS
              MOVE "S" TO NV-TAB-OK
              MOVE NV-N TO NV-I.
           ADD 1 TO NV-I.
       NV-INSERE.
           MOVE CR-CHAVE TO NV-KSAVE.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           MOVE NV-KSAVE TO CR-CHAVE.
           PERFORM NV-ACHA.
           PERFORM NV-LE-POS.
       NV-REMOVE.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           IF NV-POS > NV-N MOVE NV-N TO NV-POS.
           IF NV-POS > ZERO
              PERFORM NV-LE-POS
           ELSE
              PERFORM LIMPA-REGISTRO.
      *---- BARRA DE POSICAO (LINHA 21) E RODAPE EM DOIS GRUPOS
       NV-BARRA-P.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           MOVE NV-POS TO NV-ED-POS.
           MOVE NV-N TO NV-ED-N.
           IF NV-N = ZERO
              DISPLAY VT-POS VT-APAGA
           ELSE
              DISPLAY VT-POS VT-APAGA "                    " VT-INV
                      " REGISTRO " NV-ED-POS " DE " NV-ED-N " "
                      VT-NORMAL.
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
       LC-LE-DATA.
           MOVE SPACES TO LC-ENT.
           ACCEPT LC-ENT.
           IF LC-ENT NOT = SPACES
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 IF LC-ND = 6
                    PERFORM LC-MONTA-DATA.
       LC-MONTA-DATA.
           MOVE LC-NUM TO LC-DMY.
           IF LC-BARRA = "S"
              MOVE LC-X3 TO LC-AA
              MOVE LC-X2 TO LC-MM
              MOVE LC-X1 TO LC-DD
           ELSE
              MOVE LC-X1 TO LC-AA
              MOVE LC-X2 TO LC-MM
              MOVE LC-X3 TO LC-DD.
           IF LC-MM < 1 MOVE 12 TO LC-MM.
           IF LC-MM > 12 MOVE 12 TO LC-MM.
           IF LC-DD < 1 MOVE 1 TO LC-DD.
           IF LC-DD > LC-UD (LC-MM) MOVE LC-UD (LC-MM) TO LC-DD.
      *---- LC-SOMA-DIAS: LC-DATA + LC-DIAS (ANOS 80, 84 BISSEXTO)
       LC-FMT.
           MOVE LC-DD TO LC-E-DD.
           MOVE LC-MM TO LC-E-MM.
           MOVE LC-AA TO LC-E-AA.
       FIM-PROGRAMA.
           EXIT PROGRAM.
