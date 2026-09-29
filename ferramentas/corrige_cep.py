# uso: python corrige_cep.py PASTA
# Ajusta o CEP (5 digitos, padrao ate 1992) de CLI.TXT e FORN.TXT
# para a faixa da cidade. Nao altera nenhum outro campo.
import sys,os
FAIXA={'SAO PAULO':(1000,5999),'SANTO ANDRE':(9000,9299),'SAO BERNARDO':(9600,9899),
'CAMPINAS':(13000,13139),'SOROCABA':(18000,18109),'RIBEIRAO PRETO':(14000,14109),
'BAURU':(17000,17109),'SAO JOSE CAMPOS':(12200,12249),'RIO DE JANEIRO':(20000,23799),
'NITEROI':(24000,24399),'BELO HORIZONTE':(30000,31999),'UBERLANDIA':(38400,38414),
'VITORIA':(29000,29099),'SALVADOR':(40000,42499),'RECIFE':(50000,52999),
'FORTALEZA':(60000,61599),'BRASILIA':(70000,72799),'GOIANIA':(74000,74899),
'CURITIBA':(80000,82999),'LONDRINA':(86000,86099),'JOINVILLE':(89200,89239),
'FLORIANOPOLIS':(88000,88099),'PORTO ALEGRE':(90000,91999),'CAXIAS DO SUL':(95000,95099)}
def fix(path,klen):
    b=open(path,'rb').read(); z=b.endswith(b'\x1a')
    L=[l for l in b.replace(b'\x1a',b'').decode('ascii').split('\r\n') if l]
    cid0=klen+38+25+18+15+35+20; cep0=cid0+22+2
    out=[];n=0;semfaixa=set()
    for l in L:
        cid=l[cid0:cid0+22].strip(); cod=int(l[:klen])
        if cid in FAIXA:
            a,z2=FAIXA[cid]; cep=a+(cod*37)%(z2-a+1)
            new=l[:cep0]+f'{cep:05d}'+l[cep0+5:]
            if new!=l: n+=1
            l=new
        else: semfaixa.add(cid)
        out.append(l)
    open(path,'wb').write(('\r\n'.join(out)+'\r\n').encode('ascii')+(b'\x1a' if z else b''))
    return n,semfaixa
d=sys.argv[1]
for f,k in (('CLI.TXT',6),('FORN.TXT',5)):
    n,s=fix(os.path.join(d,f),k); print(f,n,'CEPs ajustados',('sem faixa: '+', '.join(sorted(s))) if s else '')
