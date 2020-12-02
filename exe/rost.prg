#include "inkey.ch"

********************   РАСЧЕТ ОСТАТКОВ
clrost=setcolor('g/n,n/g')
save scre to scrost
clea

*@ MAXROW()-1,0 say "__      "
*@ MAXROW()-1,10 say "Ждите идет обновление !!!"

netUse('dokz',,'e')
netUse('doks',,'e')
netUse('dokk',,'e')
netUse('dokkz',,'e')
netUse('bs',,'e')
netUse('dkkln',,'e')
netUse('dknap',,'e')

@ 1,1 say ''

sele bs
set orde to
go top
?'BS'
do while !eof()
   saldo=(dn+db)-(kn+kr)
   if saldo>=0
*      repl dn with abs(saldo),kn with 0,db with dp,kr with kp
      repl dn with abs(saldo),kn with 0,db with 0,kr with 0
   else
*      repl kn with abs(saldo),dn with 0,db with dp,kr with kp
      repl kn with abs(saldo),dn with 0,db with 0,kr with 0
   endi
   repl dp with 0,kp with 0
   skip
endd
use
??' Ok'

sele dkkln
set orde to
go top
?'DKKLN'
do while !eof()
   saldo=(dn+db)-(kn+kr)
   if saldo>=0
      repl dn with abs(saldo),kn with 0,db with 0,kr with 0
   else
      repl kn with abs(saldo),dn with 0,db with 0,kr with 0
   endi
   repl dp with 0,kp with 0
   if abs(dn)+abs(kn)+abs(db)+abs(kr)+abs(dbo)=0
      netdel()
   endi
   skip
endd
??' Ok'
sele dkkln
use

sele dokz
?'DOKZ'
dele all for mn#0
pack
use
??' Ok'

sele doks
?'DOKS'
zap
use
??' Ok'

sele dokk
?'DOKK'
zap
use
??' Ok'

sele dokkz
?'DOKKZ'
zap
use
??' Ok'

?'Файлы месяца'
sele dbft
copy to ldbft for dir=5
sele 0
use ldbft
do while !eof()
   fdbfr=alltrim(fname)+'.dbf'
   fcdxr=alltrim(fname)+'.cdx'
   ffptr=alltrim(fname)+'.fpt'
   falsr=alltrim(als)
   if falsr=='slord'.or.falsr=='slrout'.or.falsr=='nnds'
      skip
      loop
   endif
   aarr=2
   if file(gcPath_d+fdbfr)
      aar:={'Нет','Да'}
      aarr=alert(fdbfr+' Перезаписать?',aar)
      if lastkey()=K_ESC.or.aarr=1
         sele ldbft
         skip
         loop
      endif
   endif
   if aarr=2
      dtr=addmonth(gdTd,-1)
      pathr=gcPath_e+'g'+str(year(dtr),4)+'\m'+iif(month(dtr)<10,'0'+str(month(dtr),1),str(month(dtr),2))+'\'
      if file(pathr+fdbfr)
         copy file (pathr+fdbfr) to (gcPath_d+fdbfr)
         ?fdbfr
      endif
      if file(pathr+fcdxr)
         copy file (pathr+fcdxr) to (gcPath_d+fcdxr)
         ?fcdxr
      endif
      if file(pathr+ffptr)
         copy file (pathr+ffptr) to (gcPath_d+ffptr)
         ?ffptr
      endif
      ?'Индексация '+falsr
      netind(falsr)
      ??' Ok'
   endif
   sele ldbft
   skip
endd
use

?'Файлы месяца corp'
sele dbft
copy to ldbft for dir=9
sele 0
use ldbft
do while !eof()
   fdbfr=alltrim(fname)+'.dbf'
   fcdxr=alltrim(fname)+'.cdx'
   ffptr=alltrim(fname)+'.fpt'
   falsr=alltrim(als)
   aarr=2
   if file(gcPath_cd+fdbfr)
*      aar:={'Нет','Да'}
*      aarr=alert('Перезаписать? '+fdbfr,aar)
*      if lastkey()=K_ESC.or.aarr=1
*         sele ldbft
*         skip
*         loop
*      endif
   aarr=1
   endif
   if aarr=2
      dtr=addmonth(gdTd,-1)
      pathr=gcPath_c+'g'+str(year(dtr),4)+'\m'+iif(month(dtr)<10,'0'+str(month(dtr),1),str(month(dtr),2))+'\'
      if file(pathr+fdbfr)
         copy file (pathr+fdbfr) to (gcPath_cd+fdbfr)
         ?fdbfr
      endif
      if file(pathr+fcdxr)
         copy file (pathr+fcdxr) to (gcPath_cd+fcdxr)
         ?fcdxr
      endif
      if file(pathr+ffptr)
         copy file (pathr+ffptr) to (gcPathc_d+ffptr)
         ?ffptr
      endif
      ?'Индексация '+falsr
      netind(falsr)
      ??' Ok'
   endif
   sele ldbft
   skip
endd
use


?'Удаление маршрутов'
dtpr=addmonth(gdTd,-1)
pathpr=gcPath_e+'g'+str(year(dtpr),4)+'\m'+iif(month(dtpr)<10,'0'+str(month(dtpr),1),str(month(dtpr),2))+'\'
dtr=bom(gdTd)
netuse('cskl')
netuse('czg')
netuse('cmrsh')
sk_rrr=0
*wait
do while !eof()
   mrshr=mrsh
   sele czg
   prdelmr=1
   if netseek('t1','mrshr')
      do while mrsh=mrshr
         skr=sk
         if skr=0
            netdel()
            skip
            loop
         endif
         ttnr=ttn
         if skr#sk_rrr
            nuse('rs1')
            path_r=getfield('t1','skr','cskl','path')
            pathr=pathpr+alltrim(path_r)
            if !netfile('rs1',1)
               sele czg
               sk_rrr=0
               skip
               loop
            endif
            netuse('rs1',,,1)
            sk_rrr=skr
         endif
         sele rs1
         if !netseek('t1','ttnr')
            sele czg
            skip
            loop
         endif
         if prz=0
            prdelmr=0
            exit
         endif
         sele czg
         skip
      endd
   endif
   if prdelmr=1
      sele czg
      if netseek('t1','mrshr')
         do while mrsh=mrshr
            netdel()
            skip
         endd
      endif
      sele cmrsh
      ?str(mrsh,6)+' '+dtos(dmrsh)+' уд'
      netdel()
   endif
   sele cmrsh
   skip
endd
nuse('rs1')
nuse('cskl')
nuse('czg')
nuse('cmrsh')
?'Индексация cmrsh'
netind('cmrsh')
??' Ok'
?'Индексация czg'
netind('czg')
??' Ok'

?'Сжатие MODDOC'
netuse('moddoc')
do while !eof()
   if dtmod<date()-15
      netdel()
   endif
   skip
endd
nuse('moddoc')
??' Ok'

?'Сжатие MDALL'
netuse('mdall')
do while !eof()
   if dtmod<date()-15
      netdel()
   endif
   skip
endd
nuse('mdall')
??' Ok'

*?'Налоги'
*netuse('nnds')
*do while !eof()
*   netdel()
*   skip
*endd
*nuse('nnds')
*?'Индексация nnds'
*netind('nnds')
*??' Ok'

netuse('cntm')
netrepl('nnds','0')
nuse('cntm')

??' Ok'

rest scre from scrost
setcolor(clrost)
retu
