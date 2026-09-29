# Histórico das versões

| Versão | Principais mudanças |
|---|---|
| V10–V11 | Sistema completo em texto; remoção do controle de lock, que travava as gravações |
| V12 | `PERFORM ... THRU` nos módulos grandes, custo médio ponderado na compra, relatório de vendas por mês com gráfico |
| V13 | Experimento multiusuário com "janelas curtas" de abertura de arquivos |
| V14 | Adaptação ao dialeto real do CIS COBOL (sem VARYING, AND, OR, ADD com vários receptores) |
| V16–V17 | Telas VT100 em todos os módulos (vídeo inverso, destaque, posicionamento) |
| V18 | RACLOAD automático, lendo os TXT do A: |
| V19–V21 | Ficha sempre na tela, navegação entre registros, edição campo a campo com confirmação |
| V22 | Códigos sem zeros, listas `?`, valores sugeridos na venda e na compra, tela de abertura, rodapés padronizados |
| V23 | Programas enxutos para caber na memória (erro 157): edição por tabela, navegação sequencial nos arquivos grandes |
| V24 | Revisão para a apresentação: sem `ROUNDED` e sem `GO TO DEPENDING`; menu chamando `A:`; tecla `=` para o próximo; FILE STATUS na venda e na compra; X para encerrar relatórios no vídeo |

Os problemas encontrados no hardware real (erros 002, 012, 101, 110, 157 e 164)
estão descritos em [02-plataforma.md](02-plataforma.md).
