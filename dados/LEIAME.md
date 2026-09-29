# Base de demonstração V9

12 arquivos texto, um registro por linha (CR/LF, Control-Z no fim), carregados
pelo `RACLOAD` nos arquivos indexados do drive B:.

| Arquivo | Registros |
|---|---|
| PARAM.TXT | 1 |
| CLI.TXT | 100 clientes |
| FORN.TXT | 40 fornecedores |
| PROD.TXT | 500 produtos |
| PRODF.TXT | 616 relações produto × fornecedor |
| VEN.TXT / ITVEN.TXT | 93 vendas / 250 itens |
| COMP.TXT / ITCOM.TXT | 18 compras / 70 itens |
| REC.TXT / PAG.TXT | 93 títulos a receber / 18 a pagar |
| MOVEST.TXT | 820 movimentos (inventário em 30/09/84, entradas e saídas até 14/12/84) |

A base foi gerada por simulação dia a dia e conferida pela ferramenta
`ferramentas/audita.py` (sem inconsistências). `CARGA.TXT` descreve a carga e
`DEMO_V9.TXT` traz os códigos usados no roteiro da demonstração.
Todos os nomes, CGCs, endereços e telefones são fictícios.
