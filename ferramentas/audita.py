import sys,collections,datetime
D=sys.argv[1]
def L(spec):
    out=[];p=0
    for n,t,w,dec in spec: out.append((n,t,p,w,dec));p+=w
    return out,p
X=lambda n,w:(n,'X',w,0); N=lambda n,w,d=0:(n,'9',w+d,d)
LAY={
'CLI':[N('COD',6),X('RAZAO',38),X('FANT',25),X('CGC',18),X('IE',15),X('END',35),X('BAIRRO',20),X('CID',22),X('UF',2),N('CEP',5),N('DDD',3),X('FONE',10),X('TELEX',15),X('CONT',25),X('CARGO',20),X('PRACA',22),N('COND',3),N('VEND',3),N('LIM',8,2),N('DESC',2,1),N('TOTANO',9,2),N('SALDO',8,2),N('ULTC',6),N('TITAB',4),N('ATRASO',3),N('DTCAD',6),X('SIT',1)],
'FORN':[N('COD',5),X('RAZAO',38),X('FANT',25),X('CGC',18),X('IE',15),X('END',35),X('BAIRRO',20),X('CID',22),X('UF',2),N('CEP',5),N('DDD',3),X('FONE',10),X('TELEX',15),X('CONT',25),X('CARGO',20),X('PRACA',22),N('COND',3),N('DESC',2,1),N('PRAZO',3),X('FRETE',3),N('TOTANO',9,2),N('SALDO',8,2),N('ULTC',6),N('TITAB',4),N('DTCAD',6),X('SIT',1)],
'PROD':[N('COD',7),X('PN',16),X('DESC',30),X('FAB',20),N('GRUPO',3),X('UN',2),X('LOCAL',8),N('EST',7),N('RES',7),N('MIN',7),N('MAX',7),N('PP',7),N('EMCOMP',7),N('CMED',6,2),N('UCUSTO',6,2),N('PRECO',6,2),N('FORN',5),N('DTENT',6),N('DTVEN',6),N('QVANO',8),N('VVANO',8,2),X('SIT',1)],
'PRODF':[N('PROD',7),N('FORN',5),X('PN',16),N('CUSTO',6,2),N('QMIN',6),N('PRAZO',3),N('ULTC',6),N('UCUSTO',6,2),X('PREF',1)],
'VEN':[N('NUM',8),N('DATA',6),N('CLI',6),N('VEND',3),X('OPER',8),N('TERM',2),N('COND',3),N('QIT',3),N('MERC',8,2),N('DESC',7,2),N('FRETE',6,2),N('TOTAL',8,2),X('SIT',1)],
'ITVEN':[N('VENDA',8),N('ITEM',3),N('PROD',7),N('QTD',6),N('PRECO',6,2),N('DESC',2,1),N('TOTAL',8,2)],
'COMP':[N('NUM',8),N('DATA',6),N('FORN',5),X('OPER',8),N('TERM',2),N('QIT',3),N('TOTAL',8,2),X('SIT',1)],
'ITCOM':[N('COMPRA',8),N('ITEM',3),N('PROD',7),N('QTD',7),N('CUSTO',6,2),N('TOTAL',8,2)],
'REC':[N('TIT',8),N('PARC',2),N('CLI',6),N('VENDA',8),N('FAT',8),N('EMIS',6),N('VENC',6),N('VALOR',8,2),N('JUROS',6,2),N('DESC',6,2),N('RECEB',8,2),N('DTPAG',6),N('PORT',3),X('NN',12),X('SIT',1)],
'PAG':[N('TIT',8),N('PARC',2),N('FORN',5),N('COMPRA',8),N('EMIS',6),N('VENC',6),N('VALOR',8,2),N('PAGO',8,2),N('DTPAG',6),N('PORT',3),X('SIT',1)],
'MOVEST':[X('TIPO',1),N('DOC',8),N('ITEM',3),N('DATA',6),N('PROD',7),N('QTD',7),N('CUSTO',6,2),X('OPER',8),N('TERM',2)],
}
KEYS={'CLI':['COD'],'FORN':['COD'],'PROD':['COD'],'PRODF':['PROD','FORN'],'VEN':['NUM'],'ITVEN':['VENDA','ITEM'],'COMP':['NUM'],'ITCOM':['COMPRA','ITEM'],'REC':['TIT','PARC'],'PAG':['TIT','PARC'],'MOVEST':['TIPO','DOC','ITEM']}
issues=collections.defaultdict(list)
def I(cat,msg): issues[cat].append(msg)
T={}
for f,spec in LAY.items():
    lay,ln=L(spec)
    raw=open(f"{D}/{f}.TXT",'rb').read().replace(b'\x1a',b'').decode('ascii').split('\r\n')
    raw=[r for r in raw if r]
    recs=[]
    for i,r in enumerate(raw):
        d={'_raw':r,'_n':i+1}
        for n,t,p,w,dec in lay:
            s=r[p:p+w]
            if t=='9':
                if not s.isdigit(): I('NUMERICO INVALIDO',f"{f} reg {i+1} {n}='{s}'"); d[n]=None
                else: d[n]=int(s)/(10**dec) if dec else int(s)
            else: d[n]=s
        recs.append(d)
    T[f]=recs
    ks=collections.Counter(tuple(d[k] for k in KEYS[f]) for d in recs)
    for k,c in ks.items():
        if c>1: I('CHAVE DUPLICADA',f"{f} {k} x{c}")
def idx(f,*k): return {tuple(d[x] for x in k) if len(k)>1 else d[k[0]]:d for d in T[f]}
CLI=idx('CLI','COD');FOR=idx('FORN','COD');PRO=idx('PROD','COD');VEN=idx('VEN','NUM');COM=idx('COMP','NUM')
def dt(v,f,ctx):
    if v in (0,None): return
    s=f"{v:06d}"
    try: datetime.date(1900+int(s[:2]),int(s[2:4]),int(s[4:]))
    except: I('DATA INVALIDA',f"{f} {ctx}={s}")
for f,fl in [('CLI',['ULTC','DTCAD']),('FORN',['ULTC','DTCAD']),('PROD',['DTENT','DTVEN']),('PRODF',['ULTC']),('VEN',['DATA']),('COMP',['DATA']),('REC',['EMIS','VENC','DTPAG']),('PAG',['EMIS','VENC','DTPAG']),('MOVEST',['DATA'])]:
    for d in T[f]:
        for x in fl: dt(d[x],f,f"reg{d['_n']}.{x}")
# referencias
for d in T['PRODF']:
    if d['PROD'] not in PRO: I('ORFAO',f"PRODF prod {d['PROD']}")
    if d['FORN'] not in FOR: I('ORFAO',f"PRODF forn {d['FORN']}")
for d in T['PROD']:
    if d['FORN'] and d['FORN'] not in FOR: I('ORFAO',f"PROD {d['COD']} forn princ {d['FORN']}")
for d in T['VEN']:
    if d['CLI'] not in CLI: I('ORFAO',f"VEN {d['NUM']} cli {d['CLI']}")
for d in T['COMP']:
    if d['FORN'] not in FOR: I('ORFAO',f"COMP {d['NUM']} forn {d['FORN']}")
ivby=collections.defaultdict(list)
for d in T['ITVEN']:
    ivby[d['VENDA']].append(d)
    if d['VENDA'] not in VEN: I('ORFAO',f"ITVEN venda {d['VENDA']}")
    if d['PROD'] not in PRO: I('ORFAO',f"ITVEN {d['VENDA']}/{d['ITEM']} prod {d['PROD']}")
    if d['QTD'] is not None and d['PRECO'] is not None and abs(round(d['QTD']*d['PRECO']*(1-d['DESC']/100),2)-d['TOTAL'])>0.005:
        I('ITEM TOTAL != QTD*PRECO',f"ITVEN {d['VENDA']}/{d['ITEM']} {d['QTD']}*{d['PRECO']} desc{d['DESC']} -> {d['TOTAL']}")
icby=collections.defaultdict(list)
for d in T['ITCOM']:
    icby[d['COMPRA']].append(d)
    if d['COMPRA'] not in COM: I('ORFAO',f"ITCOM compra {d['COMPRA']}")
    if d['PROD'] not in PRO: I('ORFAO',f"ITCOM prod {d['PROD']}")
    if abs(round(d['QTD']*d['CUSTO'],2)-d['TOTAL'])>0.005: I('ITEM TOTAL != QTD*PRECO',f"ITCOM {d['COMPRA']}/{d['ITEM']} {d['QTD']}*{d['CUSTO']}->{d['TOTAL']}")
for n,v in VEN.items():
    its=ivby.get(n,[])
    if len(its)!=v['QIT']: I('VENDA QTD-ITENS',f"VEN {n} diz {v['QIT']} tem {len(its)}")
    s=round(sum(i['TOTAL'] for i in its),2)
    if abs(s-v['MERC'])>0.005: I('VENDA TOTAL',f"VEN {n} MERC {v['MERC']} soma itens {s}")
    if abs(round(v['MERC']-v['DESC']+v['FRETE'],2)-v['TOTAL'])>0.005: I('VENDA TOTAL',f"VEN {n} TOTAL {v['TOTAL']} != merc-desc+frete")
    sorted_items=sorted(i['ITEM'] for i in its)
    if sorted_items!=list(range(1,len(its)+1)): I('ITENS FORA DE SEQUENCIA',f"VEN {n} {sorted_items}")
    if len(set(i['PROD'] for i in its))!=len(its): I('PRODUTO REPETIDO NA VENDA',f"VEN {n}")
    if not (1<=n<=99999999): I('FAIXA NUMERACAO',f"VEN {n}")
    if v['DATA'] and v['DATA']//10000!=84: I('ANO != 84',f"VEN {n} {v['DATA']}")
    c=CLI.get(v['CLI'])
    if c and c['SIT']=='B': I('VENDA A CLIENTE INATIVO (info)',f"VEN {n} cli {v['CLI']}")
for n,v in COM.items():
    its=icby.get(n,[])
    if len(its)!=v['QIT']: I('COMPRA QTD-ITENS',f"COMP {n} diz {v['QIT']} tem {len(its)}")
    s=round(sum(i['TOTAL'] for i in its),2)
    if abs(s-v['TOTAL'])>0.005: I('COMPRA TOTAL',f"COMP {n} {v['TOTAL']} soma {s}")
# REC
recby=collections.defaultdict(list)
for d in T['REC']:
    recby[d['CLI']].append(d)
    if d['CLI'] not in CLI: I('ORFAO',f"REC {d['TIT']} cli {d['CLI']}")
    v=VEN.get(d['VENDA'])
    if not v: I('ORFAO',f"REC {d['TIT']} venda {d['VENDA']}")
    else:
        if v['CLI']!=d['CLI']: I('REC CLIENTE != VENDA',f"REC {d['TIT']}")
    if d['SIT'] not in 'AB': I('SITUACAO INVALIDA',f"REC {d['TIT']} '{d['SIT']}'")
    if d['SIT']=='B' and (d['RECEB']==0 or d['DTPAG']==0): I('REC BAIXADO SEM PAGTO',f"REC {d['TIT']}")
    if d['SIT']=='A' and (d['RECEB'] or d['DTPAG']): I('REC ABERTO COM PAGTO',f"REC {d['TIT']}")
    if d['VENC'] and d['EMIS'] and d['VENC']<d['EMIS']: I('VENC < EMISSAO',f"REC {d['TIT']}")
recv=collections.defaultdict(float)
for d in T['REC']: recv[d['VENDA']]+=d['VALOR']
for n,v in VEN.items():
    if n not in recv: I('VENDA SEM TITULO',f"VEN {n}")
    elif abs(recv[n]-v['TOTAL'])>0.005: I('REC != VENDA',f"VEN {n} total {v['TOTAL']} titulos {recv[n]:.2f}")
for c,d in CLI.items():
    op=[r for r in recby.get(c,[]) if r['SIT']=='A']
    s=round(sum(r['VALOR'] for r in op),2)
    if abs(s-d['SALDO'])>0.005: I('CLI SALDO',f"CLI {c} saldo {d['SALDO']} abertos {s}")
    if len(op)!=d['TITAB']: I('CLI TIT-ABERTOS',f"CLI {c} {d['TITAB']} real {len(op)}")
    tv=round(sum(v['TOTAL'] for v in VEN.values() if v['CLI']==c),2)
    if abs(tv-d['TOTANO'])>0.005: I('CLI TOTAL-ANO',f"CLI {c} {d['TOTANO']} vendas {tv}")
    if d['SIT'] not in 'AB': I('SITUACAO INVALIDA',f"CLI {c} '{d['SIT']}'")
    if d['SALDO']>d['LIM']: I('CLI SALDO > LIMITE',f"CLI {c} saldo {d['SALDO']} lim {d['LIM']}")
    uc=max([v['DATA'] for v in VEN.values() if v['CLI']==c],default=0)
    if uc!=d['ULTC']: I('CLI ULT-COMPRA',f"CLI {c} {d['ULTC']} real {uc}")
# PAG
pagby=collections.defaultdict(list)
for d in T['PAG']:
    pagby[d['FORN']].append(d)
    if d['FORN'] not in FOR: I('ORFAO',f"PAG {d['TIT']} forn")
    cp=COM.get(d['COMPRA'])
    if not cp: I('ORFAO',f"PAG {d['TIT']} compra {d['COMPRA']}")
    elif cp['FORN']!=d['FORN']: I('PAG FORN != COMPRA',f"PAG {d['TIT']}")
    if d['SIT'] not in 'AB': I('SITUACAO INVALIDA',f"PAG {d['TIT']} '{d['SIT']}'")
pv=collections.defaultdict(float)
for d in T['PAG']: pv[d['COMPRA']]+=d['VALOR']
for n,v in COM.items():
    if abs(pv.get(n,0)-v['TOTAL'])>0.005: I('PAG != COMPRA',f"COMP {n} {v['TOTAL']} titulos {pv.get(n,0):.2f}")
for c,d in FOR.items():
    op=[r for r in pagby.get(c,[]) if r['SIT']=='A']
    s=round(sum(r['VALOR'] for r in op),2)
    if abs(s-d['SALDO'])>0.005: I('FORN SALDO',f"FORN {c} saldo {d['SALDO']} abertos {s}")
    if len(op)!=d['TITAB']: I('FORN TIT-ABERTOS',f"FORN {c} {d['TITAB']} real {len(op)}")
    tv=round(sum(v['TOTAL'] for v in COM.values() if v['FORN']==c),2)
    if abs(tv-d['TOTANO'])>0.005: I('FORN TOTAL-ANO',f"FORN {c} {d['TOTANO']} compras {tv}")
# PROD x movimentos
mov=T['MOVEST']
movk={(m['TIPO'],m['DOC'],m['ITEM']):m for m in mov}
for d in T['ITVEN']:
    m=movk.get(('S',d['VENDA'],d['ITEM']))
    if not m: I('ITVEN SEM MOVEST',f"{d['VENDA']}/{d['ITEM']}")
    elif m['PROD']!=d['PROD'] or m['QTD']!=d['QTD']: I('MOVEST != ITVEN',f"{d['VENDA']}/{d['ITEM']}")
for d in T['ITCOM']:
    m=movk.get(('E',d['COMPRA'],d['ITEM']))
    if not m: I('ITCOM SEM MOVEST',f"{d['COMPRA']}/{d['ITEM']}")
    elif m['PROD']!=d['PROD'] or m['QTD']!=d['QTD']: I('MOVEST != ITCOM',f"{d['COMPRA']}/{d['ITEM']}")
tipos=collections.Counter(m['TIPO'] for m in mov)
for m in mov:
    if m['PROD'] not in PRO: I('ORFAO',f"MOVEST {m['TIPO']}{m['DOC']} prod {m['PROD']}")
    if m['TIPO']=='S' and m['DOC'] not in VEN: I('ORFAO',f"MOVEST S doc {m['DOC']}")
    if m['TIPO']=='E' and m['DOC'] not in COM: I('ORFAO',f"MOVEST E doc {m['DOC']}")
bal=collections.defaultdict(int); qv=collections.defaultdict(int); vv=collections.defaultdict(float)
for m in mov:
    if m['TIPO'] in ('E','I','A'): bal[m['PROD']]+=m['QTD']
    elif m['TIPO']=='S': bal[m['PROD']]-=m['QTD']
for d in T['ITVEN']: qv[d['PROD']]+=d['QTD']; vv[d['PROD']]+=d['TOTAL']
for c,p in PRO.items():
    if p['EST'] is None: continue
    if bal[c]!=p['EST']: I('ESTOQUE != SALDO MOVIMENTOS',f"PROD {c} est {p['EST']} mov {bal[c]}")
    if p['RES']>p['EST']: I('RESERVADO > ESTOQUE',f"PROD {c} est {p['EST']} res {p['RES']}")
    if qv[c]!=p['QVANO']: I('PROD QTD-VEND-ANO',f"PROD {c} {p['QVANO']} real {qv[c]}")
    if abs(round(vv[c],2)-p['VVANO'])>0.005: I('PROD VLR-VEND-ANO',f"PROD {c} {p['VVANO']} real {vv[c]:.2f}")
    if p['SIT'] not in 'AB': I('SITUACAO INVALIDA',f"PROD {c} '{p['SIT']}'")
    if p['PRECO']==0: I('PRECO ZERO',f"PROD {c}")
    if p['PRECO'] and p['CMED'] and p['PRECO']<p['CMED']: I('PRECO < CUSTO (info)',f"PROD {c}")
    if p['MIN']>p['MAX']: I('MINIMO > MAXIMO',f"PROD {c}")
# chronological stock never negative
ev=[]
for m in mov:
    ev.append((m['DATA'],0 if m['TIPO']!='S' else 1,m))
ev.sort(key=lambda e:(e[0],e[1]))
run=collections.defaultdict(int); neg=set()
for _,_,m in ev:
    run[m['PROD']]+= m['QTD'] if m['TIPO']!='S' else -m['QTD']
    if run[m['PROD']]<0: neg.add(m['PROD'])
for p in sorted(neg): I('ESTOQUE NEGATIVO NO TEMPO',f"PROD {p}")
# ranges vs PARAM
maxv=max(VEN); maxc=max(COM)
print("REGISTROS:",{f:len(T[f]) for f in T})
print("TIPOS MOVEST:",dict(tipos))
print("VEN num min/max",min(VEN),maxv," COMP min/max",min(COM),maxc)
print("REC titulos min/max",min(r['TIT'] for r in T['REC']),max(r['TIT'] for r in T['REC']))
print("PAG titulos min/max",min(r['TIT'] for r in T['PAG']),max(r['TIT'] for r in T['PAG']))
print("SIT CLI",collections.Counter(d['SIT'] for d in T['CLI']),"PROD",collections.Counter(d['SIT'] for d in T['PROD']),"FORN",collections.Counter(d['SIT'] for d in T['FORN']))
print("VEN SIT",collections.Counter(d['SIT'] for d in T['VEN']),"REC SIT",collections.Counter(d['SIT'] for d in T['REC']),"PAG SIT",collections.Counter(d['SIT'] for d in T['PAG']))
print("datas VEN",min(v['DATA'] for v in VEN.values()),max(v['DATA'] for v in VEN.values()))
print("\n=== PROBLEMAS ===")
if not issues: print("NENHUM")
for k,v in sorted(issues.items()):
    print(f"[{k}] {len(v)}"); [print("   ",x) for x in v[:6]]
