import math,sys
R={'CLI':(341,6),'FORN':(330,5),'PROD':(188,7),'PRODF':(60,12),'VEN':(77,8),'ITVEN':(45,11),'COMP':(43,8),'ITCOM':(43,11),'REC':(102,10),'PAG':(65,10),'MOVEST':(50,12),'PARAM':(64,2)}
def est(counts,blk=4096,fill=0.67,ptr=4):
    tot=0;rows=[]
    for f,n in counts.items():
        L,k=R[f]
        d=math.ceil(n*(L+1)/blk)*blk or blk
        i=math.ceil(n*(k+ptr)/fill/1024)*1024+1024
        i=math.ceil(i/blk)*blk
        rows.append((f,n,d,i)); tot+=d+i
    return tot,rows
if __name__=='__main__':
    import collections,glob
    d=sys.argv[1]
    c={f:sum(1 for l in open(f"{d}/{f}.TXT",'rb').read().replace(b'\x1a',b'').split(b'\r\n') if l) for f in R}
    t,rows=est(c)
    for r in rows: print(f"{r[0]:7}{r[1]:7}{r[2]:9}{r[3]:9}")
    print("TOTAL ESTIMADO CARREGADO:",t, "bytes", round(t/1024),"KB")
