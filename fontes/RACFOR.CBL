       IDENTIFICATION DIVISION.
       PROGRAM-ID. RACFOR.
      *RETROLABS ACME - FONTES V23
       AUTHOR. JOSE LUIZ MARTINS - RETROLABS.
      *RETROLABS ACME - SISTEMA INTEGRADO COMERCIAL
      *AMBIENTE ALVO: CIS COBOL 4.5 / MP/M / ALTOS 5-15AD
      *VERSAO DOS FONTES: 0.1 - 1984 DEMO

      *V12: DADOS NO DRIVE B: / SEM LOCK (UM TERMINAL POR VEZ)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT ARQ-FORNEC ASSIGN TO "B:FORN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-FORNEC RECORD CONTAINS 330 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REG-FORNECEDOR.
           03 FR-CHAVE.
              05 FR-CODIGO              PIC 9(5).
           03 FR-RAZAO               PIC X(38).
           03 FR-FANTASIA            PIC X(25).
           03 FR-CGC-MF              PIC X(18).
           03 FR-INSC-EST            PIC X(15).
           03 FR-ENDERECO            PIC X(35).
           03 FR-BAIRRO              PIC X(20).
           03 FR-CIDADE              PIC X(22).
           03 FR-UF                  PIC X(2).
           03 FR-CEP                 PIC 9(5).
           03 FR-DDD                 PIC 9(3).
           03 FR-TELEFONE            PIC X(10).
           03 FR-TELEX               PIC X(15).
           03 FR-CONTATO             PIC X(25).
           03 FR-CARGO               PIC X(20).
           03 FR-PRACA               PIC X(22).
           03 FR-COND-PAGTO          PIC 9(3).
           03 FR-DESCONTO            PIC 99V9.
           03 FR-PRAZO-ENTREGA       PIC 9(3).
           03 FR-FRETE               PIC X(3).
           03 FR-TOTAL-ANO           PIC 9(9)V99.
           03 FR-SALDO               PIC 9(8)V99.
           03 FR-ULT-COMPRA          PIC 9(6).
           03 FR-TIT-ABERTOS         PIC 9(4).
           03 FR-DT-CADASTRO         PIC 9(6).
           03 FR-SITUACAO            PIC X.

       WORKING-STORAGE SECTION.
       01  WS-DATA-DRIVE          PIC XX VALUE "B:".
       01  WS-OPCAO              PIC X.
       01  WS-ACHOU              PIC X.
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
              "CADASTRO DE FORNECEDORES               ".
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
              " 1-INCLUI  2-CONSULTA  3-ALTERA  4-INAT".
           03 FILLER            PIC X(39) VALUE
              "IVA  X-RETORNA                         ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  TL-TIT-INC.
           03 FILLER            PIC X(39) VALUE
              " CADASTRO DE FORNECEDORES - INCLUSAO   ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
       01  TL-TIT-ALT.
           03 FILLER            PIC X(39) VALUE
              " CADASTRO DE FORNECEDORES - ALTERACAO  ".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
       01  TL-M-INAT            PIC X(30) VALUE
           "*** ERRO NA INATIVACAO ***".
       01  TL-M-ALT             PIC X(30) VALUE
           "*** ERRO NA ALTERACAO ***".
       01  TL-M-GRAV            PIC X(30) VALUE
           "*** ERRO NA GRAVACAO ***".
       01  TF-R01.
           03 FILLER            PIC X(14) VALUE
              " CODIGO.....: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF01-1               PIC 9(5).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
           03 FILLER            PIC X(39) VALUE
              "                                       ".
           03 FILLER            PIC X(10) VALUE
              "SITUACAO: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 FILLER            PIC X(1) VALUE
              " ".
           03 TF01-2               PIC X(7).
           03 FILLER            PIC X(1) VALUE
              " ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R02.
           03 FILLER            PIC X(14) VALUE
              " RAZAO......: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 TF02-1               PIC X(38).
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R03.
           03 FILLER            PIC X(14) VALUE
              " FANTASIA...: ".
           03 TF03-1               PIC X(25).
       01  TF-R04.
           03 FILLER            PIC X(14) VALUE
              " CGC-MF.....: ".
           03 TF04-1               PIC X(18).
           03 FILLER            PIC X(23) VALUE
              "        INSC.ESTADUAL: ".
           03 TF04-2               PIC X(15).
       01  TF-R05.
           03 FILLER            PIC X(14) VALUE
              " ENDERECO...: ".
           03 TF05-1               PIC X(35).
           03 FILLER            PIC X(10) VALUE
              "  BAIRRO: ".
           03 TF05-2               PIC X(20).
       01  TF-R06.
           03 FILLER            PIC X(14) VALUE
              " CIDADE.....: ".
           03 TF06-1               PIC X(22).
           03 FILLER            PIC X(6) VALUE
              "  UF: ".
           03 TF06-2               PIC XX.
           03 FILLER            PIC X(9) VALUE
              "    CEP: ".
           03 TF06-3               PIC 99999.
       01  TF-R07.
           03 FILLER            PIC X(15) VALUE
              " TELEFONE...: (".
           03 TF07-1               PIC 999.
           03 FILLER            PIC X(2) VALUE
              ") ".
           03 TF07-2               PIC X(10).
           03 FILLER            PIC X(13) VALUE
              "      TELEX: ".
           03 TF07-3               PIC X(15).
       01  TF-R08.
           03 FILLER            PIC X(14) VALUE
              " CONTATO....: ".
           03 TF08-1               PIC X(25).
           03 FILLER            PIC X(9) VALUE
              "  CARGO: ".
           03 TF08-2               PIC X(20).
       01  TF-R10.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[1m".
           03 FILLER            PIC X(40) VALUE
              "    C O N D I C O E S                  C".
           03 FILLER            PIC X(22) VALUE
              " O M P R A S   1 9 8 4".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R11.
           03 FILLER            PIC X(40) VALUE
              "    -----------------                  -".
           03 FILLER            PIC X(22) VALUE
              "----------------------".
       01  TF-R12.
           03 FILLER            PIC X(20) VALUE
              "    COND.PAGTO....: ".
           03 TF12-1               PIC Z(12)9.
           03 FILLER            PIC X(20) VALUE
              "     TOTAL NO ANO.: ".
           03 TF12-2               PIC ZZZ,ZZZ,ZZ9.99.
       01  TF-R13.
           03 FILLER            PIC X(20) VALUE
              "    PRAZO ENTREGA.: ".
           03 TF13-1               PIC Z(12)9.
           03 FILLER            PIC X(20) VALUE
              "     SALDO A PAGAR: ".
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[7m".
           03 TF13-2               PIC ZZZ,ZZZ,ZZ9.99.
           03 FILLER            PIC X VALUE X"1B".
           03 FILLER            PIC XXX VALUE "[0m".
       01  TF-R14.
           03 FILLER            PIC X(20) VALUE
              "    FRETE.........: ".
           03 TF14-1               PIC X(13).
           03 FILLER            PIC X(20) VALUE
              "     TIT.ABERTOS..: ".
           03 TF14-2               PIC Z(13)9.
       01  TF-R15.
           03 FILLER            PIC X(20) VALUE
              "    DESCONTO %....: ".
           03 TF15-1               PIC Z(10)9.9.
           03 FILLER            PIC X(26) VALUE
              "     ULTIMA COMPRA:       ".
           03 TF15-2               PIC X(8).
       01  TF-R16.
           03 FILLER            PIC X(20) VALUE
              "    PRACA.........: ".
           03 TF16-1               PIC X(13).
           03 FILLER            PIC X(26) VALUE
              "     CADASTRO.....:       ".
           03 TF16-2               PIC X(8).
       01  EC-TABV.
           03 FILLER            PIC X(40) VALUE
              "RAZAO SOCIAL:                           ".
           03 FILLER            PIC X(28) VALUE
              "RAZAO SOCIAL          0415XN".
           03 FILLER            PIC X(40) VALUE
              "NOME FANTASIA:                          ".
           03 FILLER            PIC X(28) VALUE
              "NOME FANTASIA         0515XN".
           03 FILLER            PIC X(40) VALUE
              "CGC-MF:                                 ".
           03 FILLER            PIC X(28) VALUE
              "CGC-MF                0615XN".
           03 FILLER            PIC X(40) VALUE
              "INSCRICAO ESTADUAL:                     ".
           03 FILLER            PIC X(28) VALUE
              "INSCRICAO ESTADUAL    0656XN".
           03 FILLER            PIC X(40) VALUE
              "ENDERECO:                               ".
           03 FILLER            PIC X(28) VALUE
              "ENDERECO              0715XN".
           03 FILLER            PIC X(40) VALUE
              "BAIRRO:                                 ".
           03 FILLER            PIC X(28) VALUE
              "BAIRRO                0760XN".
           03 FILLER            PIC X(40) VALUE
              "CIDADE:                                 ".
           03 FILLER            PIC X(28) VALUE
              "CIDADE                0815XN".
           03 FILLER            PIC X(40) VALUE
              "UF:                                     ".
           03 FILLER            PIC X(28) VALUE
              "UF                    0843XN".
           03 FILLER            PIC X(40) VALUE
              "CEP:                                    ".
           03 FILLER            PIC X(28) VALUE
              "CEP                   0854IN".
           03 FILLER            PIC X(40) VALUE
              "DDD:                                    ".
           03 FILLER            PIC X(28) VALUE
              "DDD                   0916IN".
           03 FILLER            PIC X(40) VALUE
              "TELEFONE:                               ".
           03 FILLER            PIC X(28) VALUE
              "TELEFONE              0921XN".
           03 FILLER            PIC X(40) VALUE
              "TELEX:                                  ".
           03 FILLER            PIC X(28) VALUE
              "TELEX                 0944XN".
           03 FILLER            PIC X(40) VALUE
              "CONTATO:                                ".
           03 FILLER            PIC X(28) VALUE
              "CONTATO               1015XN".
           03 FILLER            PIC X(40) VALUE
              "CARGO:                                  ".
           03 FILLER            PIC X(28) VALUE
              "CARGO                 1049XN".
           03 FILLER            PIC X(40) VALUE
              "CONDICAO DE PAGAMENTO (DIAS):           ".
           03 FILLER            PIC X(28) VALUE
              "CONDICAO DE PAGAMENTO 1421IN".
           03 FILLER            PIC X(40) VALUE
              "PRAZO DE ENTREGA (DIAS):                ".
           03 FILLER            PIC X(28) VALUE
              "PRAZO DE ENTREGA      1521IN".
           03 FILLER            PIC X(40) VALUE
              "FRETE (CIF/FOB):                        ".
           03 FILLER            PIC X(28) VALUE
              "FRETE                 1621XN".
           03 FILLER            PIC X(40) VALUE
              "DESCONTO %:                             ".
           03 FILLER            PIC X(28) VALUE
              "DESCONTO %            1721DN".
           03 FILLER            PIC X(40) VALUE
              "PRACA DE PAGAMENTO:                     ".
           03 FILLER            PIC X(28) VALUE
              "PRACA DE PAGAMENTO    1821XN".
           03 FILLER            PIC X(40) VALUE
              "DATA DE CADASTRO (AAMMDD):              ".
           03 FILLER            PIC X(28) VALUE
              "DATA DE CADASTRO      1860TS".
           03 FILLER            PIC X(40) VALUE
              "SITUACAO (A=ATIVO  B=INATIVO):          ".
           03 FILLER            PIC X(28) VALUE
              "SITUACAO              0370SN".
       01  EC-TAB REDEFINES EC-TABV.
           03 EC-D              OCCURS 21 TIMES.
              05 EC-PROMPT      PIC X(40).
              05 EC-LAB         PIC X(22).
              05 EC-LIN         PIC 99.
              05 EC-COL         PIC 99.
              05 EC-TIPO        PIC X.
              05 EC-INC         PIC X.
       01  EC-I                 PIC 99 VALUE ZERO.
       01  EC-J                 PIC 99 VALUE ZERO.
       01  EC-ORD               PIC 99 VALUE ZERO.
       01  EC-TOT               PIC 99 VALUE ZERO.
       01  EC-X                 PIC X(40) VALUE SPACES.
       01  EC-N                 PIC 9(10)V99 VALUE ZERO.
       01  EC-SHOW              PIC X(38) VALUE SPACES.
       01  EC-ED-I              PIC Z(9)9.
       01  EC-ED-M              PIC ZZ,ZZZ,ZZ9.99.
       01  EC-ED-D              PIC Z(8)9.9.
       01  EC-ED-ORD            PIC Z9.
       01  EC-ED-TOT            PIC Z9.
       01  EC-NUM               PIC 99 VALUE 21.
       01  WS-FIM               PIC X VALUE "N".
       01  WS-MODO              PIC X VALUE "A".
       01  WS-CANCELA           PIC X VALUE "N".
       01  TL-ENT               PIC X(40) VALUE SPACES.
       01  TL-ENT-R REDEFINES TL-ENT.
           03 TL-ENT-C          PIC X OCCURS 40 TIMES.
       01  TL-DIGITOS           PIC X(10) VALUE "0123456789".
       01  TL-DIGITOS-R REDEFINES TL-DIGITOS.
           03 TL-DIGITO         PIC X OCCURS 10 TIMES.
       01  TL-CH                PIC X.
       01  TL-D                 PIC 99.
       01  TL-INT               PIC 9(12).
       01  TL-NUM               PIC 9(10)V99.
       01  TL-NDEC              PIC 99.
       01  TL-SEP               PIC 99.
       01  TL-NUM-OK            PIC X VALUE "S".
       01  TL-M-INV             PIC X(43) VALUE
           "*** VALOR INVALIDO - MANTIDO O ANTERIOR ***".
       01  TL-ROD-EDICAO.
           03 FILLER            PIC X(39) VALUE
              " RETURN = MANTEM O VALOR ATUAL     DIGI".
           03 FILLER            PIC X(39) VALUE
              "TE PARA TROCAR   * = CANCELA A EDICAO  ".
           03 FILLER            PIC X(1) VALUE
              " ".
       01  WS-NALT              PIC 99 VALUE ZERO.
       01  WS-SEMALT            PIC X VALUE "N".
       01  NV-MUDOU             PIC X VALUE "N".
       01  NV-ST                PIC X(10) VALUE SPACES.
       01  NV-PREVL             PIC X(22) VALUE SPACES.
       01  NV-PERG-ALT.
           03 NV-ED-NALT        PIC Z9.
           03 FILLER            PIC X(36) VALUE
              " CAMPO(S) ALTERADO(S) - GRAVA (S/N)?".
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
       01  NV-KSAVE             PIC X(5).
       01  NV-KATU              PIC X(5) VALUE SPACES.
       01  NV-ED-POS            PIC ZZZ9.
       01  NV-ED-N              PIC ZZZ9.
       01  NV-ENT               PIC X(20) VALUE SPACES.
       01  NV-ENT-R REDEFINES NV-ENT.
           03 NV-ENT-C          PIC X OCCURS 20 TIMES.
       01  NV-MSG-NC.
           03 FILLER            PIC X(4) VALUE "*** ".
           03 NV-MK             PIC X(5).
           03 FILLER            PIC X(35) VALUE
              " NAO EXISTE - CONFIRA O CODIGO    ".
           03 FILLER            PIC X(3) VALUE "***".
       01  NV-ROD1              PIC X(34) VALUE
           " = PROX  - ANT  I=INICIO  F=FIM   ".
       01  NV-TAB.
           03 NV-TK             PIC X(5) OCCURS 150 TIMES.
       01  NV-COPIA             PIC X(330).
       01  NV-ROD2.
           03 FILLER            PIC X(39) VALUE
              " 1 INCLUI   3 ALTERA   4 INATIVA   X SA".
           03 FILLER            PIC X(5) VALUE
              "I    ".
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
      *V21: FICHA SEMPRE NA TELA; RETURN=PROXIMO, - ANTERIOR,
      *I INICIO, F FIM, CODIGO DIRETO; BARRA DE POSICAO; SO OS
      *VALORES SAO REDESENHADOS; EDICAO CAMPO A CAMPO COM PROGRESSO.
       INICIO.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           OPEN I-O ARQ-FORNEC.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE ZERO TO NV-POS.
           IF NV-N > ZERO
              MOVE 1 TO NV-POS
              PERFORM NV-LE-POS.
           MOVE "N" TO NV-TELA-OK.
           MOVE SPACE TO WS-OPCAO.
           PERFORM MENU UNTIL WS-OPCAO = "X".
           CLOSE ARQ-FORNEC.
           GO TO FIM-PROGRAMA.
       MENU.
           PERFORM TELA-FICHA.
           MOVE "OPCAO OU CODIGO:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-MSG.
           PERFORM NV-LE-OPCAO.
           IF WS-OPCAO = "1" PERFORM INCLUIR THRU INC-FIM.
           IF WS-OPCAO = "2" PERFORM CONSULTAR.
           IF WS-OPCAO = "3" PERFORM ALTERAR THRU ALT-FIM.
           IF WS-OPCAO = "4" PERFORM EXCLUIR THRU EXC-FIM.
           PERFORM NV-ACAO.
       TELA-FICHA.
           MOVE "N" TO NV-CHEIA.
           IF NV-TELA-OK = "N"
              MOVE "S" TO NV-CHEIA
              MOVE "S" TO NV-TELA-OK
              PERFORM TL-TITULO-P.
           PERFORM MOSTRAR.
           PERFORM TL-MSG-P.
           PERFORM NV-BARRA-P THRU NV-BP-FIM.
           IF NV-CHEIA = "S" PERFORM NV-RODAPE-P.
       LIMPA-REGISTRO.
           MOVE SPACES TO REG-FORNECEDOR.
           MOVE ZERO TO FR-CODIGO.
           MOVE ZERO TO FR-CEP.
           MOVE ZERO TO FR-DDD.
           MOVE ZERO TO FR-COND-PAGTO.
           MOVE ZERO TO FR-DESCONTO.
           MOVE ZERO TO FR-PRAZO-ENTREGA.
           MOVE ZERO TO FR-TOTAL-ANO.
           MOVE ZERO TO FR-SALDO.
           MOVE ZERO TO FR-ULT-COMPRA.
           MOVE ZERO TO FR-TIT-ABERTOS.
           MOVE ZERO TO FR-DT-CADASTRO.
           MOVE "A" TO FR-SITUACAO.
       LER-CHAVE.
           MOVE "CODIGO DO FORNECEDOR:" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           PERFORM LC-LE-CODIGO.
           MOVE LC-NUM TO FR-CODIGO.
       CONSULTAR.
           PERFORM LER-CHAVE.
           PERFORM NV-VAI-CHAVE.
       VOLTA-ATUAL.
           IF NV-POS > ZERO
              PERFORM NV-LE-POS
           ELSE
              PERFORM LIMPA-REGISTRO.
       INCLUIR.
           PERFORM LER-CHAVE.
           MOVE FR-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-FORNEC
               INVALID KEY MOVE "N" TO NV-ACHOU.
           IF NV-ACHOU = "S"
              PERFORM NV-ACHA
              PERFORM NV-LE-POS
              MOVE "*** FORNECEDOR JA CADASTRADO ***" TO TL-MSG
              GO TO INC-FIM.
           PERFORM LIMPA-REGISTRO.
           MOVE NV-KSAVE TO FR-CHAVE.
           MOVE "I" TO WS-MODO.
           MOVE TL-TIT-INC TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE "INCLUSAO CANCELADA" TO TL-MSG
              PERFORM VOLTA-ATUAL
              GO TO INC-FIM.
           MOVE "FORNECEDOR INCLUIDO" TO TL-MSG.
           WRITE REG-FORNECEDOR
              INVALID KEY MOVE TL-M-GRAV TO TL-MSG.
           IF TL-MSG = TL-M-GRAV
              PERFORM VOLTA-ATUAL
           ELSE
              PERFORM NV-INSERE.
       INC-FIM.
           EXIT.
       ALTERAR.
           IF NV-POS = ZERO
              MOVE "*** NENHUM REGISTRO NA TELA ***" TO TL-MSG
              GO TO ALT-FIM.
           MOVE "A" TO WS-MODO.
           MOVE REG-FORNECEDOR TO NV-COPIA.
           MOVE TL-TIT-ALT TO TL-TITULO.
           PERFORM EDICAO THRU ED-FIM.
           MOVE TL-TIT-PADRAO TO TL-TITULO.
           IF WS-CANCELA = "S"
              MOVE NV-COPIA TO REG-FORNECEDOR
              MOVE "ALTERACAO CANCELADA" TO TL-MSG
              IF WS-SEMALT = "S"
                 MOVE "NENHUM CAMPO ALTERADO" TO TL-MSG
                 GO TO ALT-FIM
              ELSE
                 GO TO ALT-FIM.
           MOVE "FORNECEDOR ALTERADO" TO TL-MSG.
           REWRITE REG-FORNECEDOR
              INVALID KEY MOVE TL-M-ALT TO TL-MSG.
       ALT-FIM.
           EXIT.
       EXCLUIR.
           IF NV-POS = ZERO
              MOVE "*** NENHUM REGISTRO NA TELA ***" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "CONFIRMA A INATIVACAO (S/N)?" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           ACCEPT WS-OPCAO.
           IF WS-OPCAO = "s" MOVE "S" TO WS-OPCAO.
           IF WS-OPCAO NOT = "S"
              MOVE "OPERACAO CANCELADA" TO TL-MSG
              GO TO EXC-FIM.
           MOVE "B" TO FR-SITUACAO.
           MOVE "FORNECEDOR INATIVADO" TO TL-MSG.
           REWRITE REG-FORNECEDOR
              INVALID KEY MOVE TL-M-INAT TO TL-MSG.
       EXC-FIM.
           MOVE SPACE TO WS-OPCAO.
      *---- EDICAO CAMPO A CAMPO SOBRE A FICHA
       EDICAO.
           MOVE "N" TO WS-CANCELA.
           MOVE "N" TO WS-SEMALT.
           MOVE ZERO TO WS-NALT.
           MOVE "INICIO DA FICHA" TO NV-PREVL.
           MOVE SPACES TO NV-ST.
           MOVE "S" TO NV-CHEIA.
              PERFORM TL-TITULO-P.
           PERFORM MOSTRAR.
           MOVE 21 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE TL-ROD-EDICAO TO TL-RODAPE.
           PERFORM TL-RODAPE-P.
           PERFORM EC-EDITA.
           PERFORM CONFIRMA-GRAV THRU CG-FIM.
           MOVE "N" TO NV-TELA-OK.
       ED-FIM.
           EXIT.
      *---- EDICAO CAMPO A CAMPO DIRIGIDA POR TABELA (V23)
       EC-EDITA.
           MOVE ZERO TO EC-TOT.
           MOVE 1 TO EC-I.
           PERFORM EC-CONTA UNTIL EC-I > EC-NUM.
           MOVE ZERO TO EC-ORD.
           MOVE 1 TO EC-I.
           PERFORM EC-CAMPO THRU EC-CF UNTIL EC-I > EC-NUM.
       EC-CONTA.
           IF EC-INC (EC-I) = "N"
              ADD 1 TO EC-TOT
           ELSE
              IF WS-MODO = "I" ADD 1 TO EC-TOT.
           ADD 1 TO EC-I.
       EC-CAMPO.
           IF WS-CANCELA = "S"
              MOVE 90 TO EC-I
              GO TO EC-CF.
           IF EC-INC (EC-I) = "S"
              IF WS-MODO NOT = "I" GO TO EC-CF.
           ADD 1 TO EC-ORD.
           MOVE "N" TO NV-MUDOU.
           PERFORM EC-GET THRU EC-GET-FIM.
           PERFORM EC-FORMATA.
           MOVE EC-ORD TO EC-ED-ORD.
           MOVE EC-TOT TO EC-ED-TOT.
           MOVE 1 TO VT-LIN.
           MOVE 60 TO VT-COL.
           DISPLAY VT-POS VT-INV "CAMPO " EC-ED-ORD " DE " EC-ED-TOT " "
                   VT-NORMAL.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA " " VT-FORTE "ANTERIOR:" VT-NORMAL
                   " " NV-ST " " NV-PREVL.
           MOVE 21 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA VT-INV " ATUAL: " EC-SHOW
                   " (RETURN MANTEM, * CANCELA) " VT-NORMAL.
           MOVE EC-LIN (EC-I) TO VT-LIN.
           SUBTRACT 1 FROM EC-COL (EC-I) GIVING VT-COL.
           DISPLAY VT-POS VT-INV VT-PISCA ">" VT-NORMAL VT-HOME.
           MOVE EC-PROMPT (EC-I) TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-ENT.
           ACCEPT TL-ENT.
           MOVE EC-LIN (EC-I) TO VT-LIN.
           SUBTRACT 1 FROM EC-COL (EC-I) GIVING VT-COL.
           DISPLAY VT-POS " " VT-HOME.
           IF TL-ENT = "*"
              MOVE "S" TO WS-CANCELA
              GO TO EC-CF.
           IF TL-ENT NOT = SPACES PERFORM EC-APLICA.
           IF NV-MUDOU = "S"
              PERFORM EC-PUT THRU EC-PUT-FIM
              ADD 1 TO WS-NALT
              MOVE "(ALTERADO)" TO NV-ST
              MOVE "N" TO NV-CHEIA
              PERFORM MOSTRAR
           ELSE
              MOVE "(MANTIDO) " TO NV-ST.
           MOVE EC-LAB (EC-I) TO NV-PREVL.
       EC-CF.
           ADD 1 TO EC-I.
       EC-APLICA.
           MOVE TL-ENT TO LC-ENT.
           IF EC-TIPO (EC-I) = "X"
              MOVE TL-ENT TO EC-X
              MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "I"
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 MOVE LC-NUM TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "T"
              PERFORM LC-CONVERTE
              IF LC-OK = "S"
                 IF LC-ND = 6
                    PERFORM LC-MONTA-DATA
                    MOVE LC-DATA TO EC-N
                    MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "M"
              PERFORM LC-CONV-VALOR
              IF LC-OK = "S"
                 MOVE LC-VAL TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "D"
              PERFORM LC-CONV-VALOR
              IF LC-OK = "S"
                 MOVE LC-VAL TO EC-N
                 MOVE "S" TO NV-MUDOU.
           IF LC-C (1) = "a" MOVE "A" TO LC-C (1).
           IF LC-C (1) = "b" MOVE "B" TO LC-C (1).
           IF LC-C (1) = "s" MOVE "S" TO LC-C (1).
           IF LC-C (1) = "n" MOVE "N" TO LC-C (1).
           IF EC-TIPO (EC-I) = "S"
              IF LC-C (1) = "A"
                 MOVE "A" TO EC-X
                 MOVE "S" TO NV-MUDOU
              ELSE
                 IF LC-C (1) = "B"
                    MOVE "B" TO EC-X
                    MOVE "S" TO NV-MUDOU.
           IF EC-TIPO (EC-I) = "P"
              IF LC-C (1) = "S"
                 MOVE "S" TO EC-X
                 MOVE "S" TO NV-MUDOU
              ELSE
                 IF LC-C (1) = "N"
                    MOVE "N" TO EC-X
                    MOVE "S" TO NV-MUDOU.
           IF NV-MUDOU = "N"
              MOVE TL-M-INV TO TL-MSG
              PERFORM TL-MSG-P
              MOVE SPACES TO TL-MSG.
       EC-FORMATA.
           MOVE EC-X TO EC-SHOW.
           IF EC-TIPO (EC-I) = "I"
              MOVE EC-N TO EC-ED-I
              MOVE EC-ED-I TO EC-SHOW.
           IF EC-TIPO (EC-I) = "M"
              MOVE EC-N TO EC-ED-M
              MOVE EC-ED-M TO EC-SHOW.
           IF EC-TIPO (EC-I) = "D"
              MOVE EC-N TO EC-ED-D
              MOVE EC-ED-D TO EC-SHOW.
           IF EC-TIPO (EC-I) = "T"
              MOVE EC-N TO LC-DATA
              PERFORM LC-FMT
              MOVE LC-DT-ED TO EC-SHOW.
           IF EC-TIPO (EC-I) = "S"
              MOVE "ATIVO" TO EC-SHOW
              IF EC-X = "B" MOVE "INATIVO" TO EC-SHOW.
           IF EC-TIPO (EC-I) = "P"
              MOVE "NAO" TO EC-SHOW
              IF EC-X = "S" MOVE "SIM" TO EC-SHOW.
       EC-GET.
           IF EC-I = 1
              GO TO EC-G01.
           IF EC-I = 2
              GO TO EC-G02.
           IF EC-I = 3
              GO TO EC-G03.
           IF EC-I = 4
              GO TO EC-G04.
           IF EC-I = 5
              GO TO EC-G05.
           IF EC-I = 6
              GO TO EC-G06.
           IF EC-I = 7
              GO TO EC-G07.
           IF EC-I = 8
              GO TO EC-G08.
           IF EC-I = 9
              GO TO EC-G09.
           IF EC-I = 10
              GO TO EC-G10.
           IF EC-I = 11
              GO TO EC-G11.
           IF EC-I = 12
              GO TO EC-G12.
           IF EC-I = 13
              GO TO EC-G13.
           IF EC-I = 14
              GO TO EC-G14.
           IF EC-I = 15
              GO TO EC-G15.
           IF EC-I = 16
              GO TO EC-G16.
           IF EC-I = 17
              GO TO EC-G17.
           IF EC-I = 18
              GO TO EC-G18.
           IF EC-I = 19
              GO TO EC-G19.
           IF EC-I = 20
              GO TO EC-G20.
           IF EC-I = 21
              GO TO EC-G21.
           GO TO EC-GET-FIM.
       EC-G01.
           MOVE FR-RAZAO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G02.
           MOVE FR-FANTASIA TO EC-X.
           GO TO EC-GET-FIM.
       EC-G03.
           MOVE FR-CGC-MF TO EC-X.
           GO TO EC-GET-FIM.
       EC-G04.
           MOVE FR-INSC-EST TO EC-X.
           GO TO EC-GET-FIM.
       EC-G05.
           MOVE FR-ENDERECO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G06.
           MOVE FR-BAIRRO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G07.
           MOVE FR-CIDADE TO EC-X.
           GO TO EC-GET-FIM.
       EC-G08.
           MOVE FR-UF TO EC-X.
           GO TO EC-GET-FIM.
       EC-G09.
           MOVE FR-CEP TO EC-N.
           GO TO EC-GET-FIM.
       EC-G10.
           MOVE FR-DDD TO EC-N.
           GO TO EC-GET-FIM.
       EC-G11.
           MOVE FR-TELEFONE TO EC-X.
           GO TO EC-GET-FIM.
       EC-G12.
           MOVE FR-TELEX TO EC-X.
           GO TO EC-GET-FIM.
       EC-G13.
           MOVE FR-CONTATO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G14.
           MOVE FR-CARGO TO EC-X.
           GO TO EC-GET-FIM.
       EC-G15.
           MOVE FR-COND-PAGTO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G16.
           MOVE FR-PRAZO-ENTREGA TO EC-N.
           GO TO EC-GET-FIM.
       EC-G17.
           MOVE FR-FRETE TO EC-X.
           GO TO EC-GET-FIM.
       EC-G18.
           MOVE FR-DESCONTO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G19.
           MOVE FR-PRACA TO EC-X.
           GO TO EC-GET-FIM.
       EC-G20.
           MOVE FR-DT-CADASTRO TO EC-N.
           GO TO EC-GET-FIM.
       EC-G21.
           MOVE FR-SITUACAO TO EC-X.
           GO TO EC-GET-FIM.
       EC-GET-FIM.
           EXIT.
       EC-PUT.
           IF EC-I = 1
              GO TO EC-P01.
           IF EC-I = 2
              GO TO EC-P02.
           IF EC-I = 3
              GO TO EC-P03.
           IF EC-I = 4
              GO TO EC-P04.
           IF EC-I = 5
              GO TO EC-P05.
           IF EC-I = 6
              GO TO EC-P06.
           IF EC-I = 7
              GO TO EC-P07.
           IF EC-I = 8
              GO TO EC-P08.
           IF EC-I = 9
              GO TO EC-P09.
           IF EC-I = 10
              GO TO EC-P10.
           IF EC-I = 11
              GO TO EC-P11.
           IF EC-I = 12
              GO TO EC-P12.
           IF EC-I = 13
              GO TO EC-P13.
           IF EC-I = 14
              GO TO EC-P14.
           IF EC-I = 15
              GO TO EC-P15.
           IF EC-I = 16
              GO TO EC-P16.
           IF EC-I = 17
              GO TO EC-P17.
           IF EC-I = 18
              GO TO EC-P18.
           IF EC-I = 19
              GO TO EC-P19.
           IF EC-I = 20
              GO TO EC-P20.
           IF EC-I = 21
              GO TO EC-P21.
           GO TO EC-PUT-FIM.
       EC-P01.
           MOVE EC-X TO FR-RAZAO.
           GO TO EC-PUT-FIM.
       EC-P02.
           MOVE EC-X TO FR-FANTASIA.
           GO TO EC-PUT-FIM.
       EC-P03.
           MOVE EC-X TO FR-CGC-MF.
           GO TO EC-PUT-FIM.
       EC-P04.
           MOVE EC-X TO FR-INSC-EST.
           GO TO EC-PUT-FIM.
       EC-P05.
           MOVE EC-X TO FR-ENDERECO.
           GO TO EC-PUT-FIM.
       EC-P06.
           MOVE EC-X TO FR-BAIRRO.
           GO TO EC-PUT-FIM.
       EC-P07.
           MOVE EC-X TO FR-CIDADE.
           GO TO EC-PUT-FIM.
       EC-P08.
           MOVE EC-X TO FR-UF.
           GO TO EC-PUT-FIM.
       EC-P09.
           MOVE EC-N TO FR-CEP.
           GO TO EC-PUT-FIM.
       EC-P10.
           MOVE EC-N TO FR-DDD.
           GO TO EC-PUT-FIM.
       EC-P11.
           MOVE EC-X TO FR-TELEFONE.
           GO TO EC-PUT-FIM.
       EC-P12.
           MOVE EC-X TO FR-TELEX.
           GO TO EC-PUT-FIM.
       EC-P13.
           MOVE EC-X TO FR-CONTATO.
           GO TO EC-PUT-FIM.
       EC-P14.
           MOVE EC-X TO FR-CARGO.
           GO TO EC-PUT-FIM.
       EC-P15.
           MOVE EC-N TO FR-COND-PAGTO.
           GO TO EC-PUT-FIM.
       EC-P16.
           MOVE EC-N TO FR-PRAZO-ENTREGA.
           GO TO EC-PUT-FIM.
       EC-P17.
           MOVE EC-X TO FR-FRETE.
           GO TO EC-PUT-FIM.
       EC-P18.
           MOVE EC-N TO FR-DESCONTO.
           GO TO EC-PUT-FIM.
       EC-P19.
           MOVE EC-X TO FR-PRACA.
           GO TO EC-PUT-FIM.
       EC-P20.
           MOVE EC-N TO FR-DT-CADASTRO.
           GO TO EC-PUT-FIM.
       EC-P21.
           MOVE EC-X TO FR-SITUACAO.
           GO TO EC-PUT-FIM.
       EC-PUT-FIM.
           EXIT.

       EC-FIM.
           EXIT.
       CONFIRMA-GRAV.
           MOVE 20 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA.
           MOVE 21 TO VT-LIN.
           DISPLAY VT-POS VT-APAGA.
           IF WS-CANCELA = "S" GO TO CG-FIM.
           IF WS-MODO = "A"
              IF WS-NALT = ZERO
                 MOVE "S" TO WS-CANCELA
                 MOVE "S" TO WS-SEMALT
                 GO TO CG-FIM.
           MOVE WS-NALT TO NV-ED-NALT.
           IF WS-MODO = "A"
              MOVE NV-PERG-ALT TO TL-PROMPT
           ELSE
              MOVE "CONFIRMA A INCLUSAO (S/N)?" TO TL-PROMPT.
           PERFORM TL-PERGUNTA.
           MOVE SPACES TO TL-ENT.
           ACCEPT TL-ENT.
           IF TL-ENT-C (1) = "s" MOVE "S" TO TL-ENT-C (1).
           IF TL-ENT-C (1) NOT = "S" MOVE "S" TO WS-CANCELA.
       CG-FIM.
           EXIT.
      *---- NAVEGACAO (V21): TABELA DE CHAVES EM MEMORIA,
      *RETURN=PROXIMO, - ANTERIOR, I INICIO, F FIM, CODIGO DIRETO.
       NV-CARREGA.
           MOVE ZERO TO NV-N.
           MOVE LOW-VALUES TO FR-CHAVE.
           START ARQ-FORNEC KEY IS NOT < FR-CHAVE
              INVALID KEY GO TO NV-CC-FIM.
           MOVE "N" TO NV-FIM.
           PERFORM NV-LER UNTIL NV-FIM = "S".
       NV-CC-FIM.
           EXIT.
       NV-LER.
           READ ARQ-FORNEC NEXT RECORD
              AT END MOVE "S" TO NV-FIM.
           IF NV-FIM = "N"
              IF NV-N < 150
                 ADD 1 TO NV-N
                 MOVE FR-CHAVE TO NV-TK (NV-N).
       NV-LE-POS.
           MOVE NV-TK (NV-POS) TO FR-CHAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-FORNEC
              INVALID KEY MOVE "N" TO NV-ACHOU.
           PERFORM NV-APOS-LER.
       NV-APOS-LER.
           IF NV-ACHOU = "S" MOVE FR-CHAVE TO NV-KATU.
           IF NV-ACHOU = "S"
              IF FR-SITUACAO = "B"
                 MOVE "*** FORNECEDOR INATIVO ***"
                   TO TL-MSG.
           IF NV-ACHOU = "N" PERFORM LIMPA-REGISTRO.
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
              MOVE LC-NUM TO FR-CODIGO
           ELSE
              MOVE NV-ENT TO FR-CHAVE.
       NV-VAI-CHAVE.
           MOVE FR-CHAVE TO NV-KSAVE.
           MOVE "S" TO NV-ACHOU.
           READ ARQ-FORNEC
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
           IF NV-TK (NV-I) = FR-CHAVE
              MOVE NV-I TO NV-POS
              MOVE "S" TO NV-TAB-OK
              MOVE NV-N TO NV-I.
           ADD 1 TO NV-I.
       NV-INSERE.
           MOVE FR-CHAVE TO NV-KSAVE.
           PERFORM NV-CARREGA THRU NV-CC-FIM.
           MOVE NV-KSAVE TO FR-CHAVE.
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
       MOSTRAR.
           MOVE FR-CODIGO TO TF01-1.
           MOVE "ATIVO" TO TF01-2.
           IF FR-SITUACAO = "B" MOVE "INATIVO" TO TF01-2.
           MOVE 3 TO VT-LIN.
           MOVE 1 TO VT-COL.
           DISPLAY VT-POS VT-APAGA
              TF-R01.
           MOVE FR-RAZAO TO TF02-1.
           DISPLAY VT-APAGA
              TF-R02.
           MOVE FR-FANTASIA TO TF03-1.
           DISPLAY VT-APAGA
              TF-R03.
           MOVE FR-CGC-MF TO TF04-1.
           MOVE FR-INSC-EST TO TF04-2.
           DISPLAY VT-APAGA
              TF-R04.
           MOVE FR-ENDERECO TO TF05-1.
           MOVE FR-BAIRRO TO TF05-2.
           DISPLAY VT-APAGA
              TF-R05.
           MOVE FR-CIDADE TO TF06-1.
           MOVE FR-UF TO TF06-2.
           MOVE FR-CEP TO TF06-3.
           DISPLAY VT-APAGA
              TF-R06.
           MOVE FR-DDD TO TF07-1.
           MOVE FR-TELEFONE TO TF07-2.
           MOVE FR-TELEX TO TF07-3.
           DISPLAY VT-APAGA
              TF-R07.
           MOVE FR-CONTATO TO TF08-1.
           MOVE FR-CARGO TO TF08-2.
           DISPLAY VT-APAGA
              TF-R08.
           DISPLAY VT-APAGA
              TL-TRACO.
           DISPLAY VT-APAGA
              TF-R10.
           DISPLAY VT-APAGA
              TF-R11.
           MOVE FR-COND-PAGTO TO TF12-1.
           MOVE FR-TOTAL-ANO TO TF12-2.
           DISPLAY VT-APAGA
              TF-R12.
           MOVE FR-PRAZO-ENTREGA TO TF13-1.
           MOVE FR-SALDO TO TF13-2.
           DISPLAY VT-APAGA
              TF-R13.
           MOVE FR-FRETE TO TF14-1.
           MOVE FR-TIT-ABERTOS TO TF14-2.
           DISPLAY VT-APAGA
              TF-R14.
           MOVE FR-DESCONTO TO TF15-1.
           MOVE FR-ULT-COMPRA TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF15-2.
           IF FR-ULT-COMPRA = ZERO MOVE SPACES TO TF15-2.
           DISPLAY VT-APAGA
              TF-R15.
           MOVE FR-PRACA TO TF16-1.
           MOVE FR-DT-CADASTRO TO TL-DTW.
           PERFORM TL-FMT-DATA.
           MOVE TL-DT-ED TO TF16-2.
           IF FR-DT-CADASTRO = ZERO MOVE SPACES TO TF16-2.
           DISPLAY VT-APAGA
              TF-R16.
           DISPLAY VT-APAGA
              " ".
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
       LC-CONV-VALOR.
           MOVE ZERO TO LC-SP.
           MOVE 1 TO LC-I.
           PERFORM LC-ACHA-SEP UNTIL LC-I > 20.
           MOVE ZERO TO LC-NUM.
           MOVE ZERO TO LC-NDEC.
           MOVE "S" TO LC-OK.
           MOVE 1 TO LC-I.
           PERFORM LC-DIG-V UNTIL LC-I > 20.
           MOVE LC-NUM TO LC-VAL.
           PERFORM LC-DIV10 UNTIL LC-NDEC = ZERO.
       LC-ACHA-SEP.
           IF LC-C (LC-I) = "."
              MOVE LC-I TO LC-SP.
           IF LC-C (LC-I) = ","
              MOVE LC-I TO LC-SP.
           ADD 1 TO LC-I.
       LC-DIG-V.
           IF LC-C (LC-I) NOT = SPACE
              IF LC-C (LC-I) NOT = "."
                 IF LC-C (LC-I) NOT = ","
                    MOVE ZERO TO LC-J
                    MOVE 99 TO LC-D
                    PERFORM LC-PROCURA UNTIL LC-J > 9
                    IF LC-D = 99
                       MOVE "N" TO LC-OK
                    ELSE
                       MULTIPLY 10 BY LC-NUM
                       ADD LC-D TO LC-NUM
                       IF LC-SP > ZERO
                          IF LC-I > LC-SP
                             ADD 1 TO LC-NDEC.
           ADD 1 TO LC-I.
       LC-DIV10.
           DIVIDE 10 INTO LC-VAL.
           SUBTRACT 1 FROM LC-NDEC.
      *---- LC-LE-DATA: RETURN MANTEM LC-DATA; ACEITA DD/MM/AA OU AAMMDD
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