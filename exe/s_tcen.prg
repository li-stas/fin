#include "inkey.ch"
clea
netuse('tcen')
do while !eof()
   if sh=0
      netrepl('grtcen','99')
   endif
   skip
endd
set orde to tag t2
go top
rctcenr=recn()
do while .t.
   set orde to tag t2
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   if fieldpos('dec')=0
      rctcenr=slcf('tcen',,,,,"e:tcen h:'���' c:n(2) e:ntcen h:'������������' c:c(20) e:zen h:'����' c:c(10) e:knds h:'���' c:n(1) e:tcenp h:'���' c:n(3) e:prtcenp h:'%' c:n(6,2) e:sh h:'���' c:n(1) e:grtcen h:'NN' c:n(2) e:kpk h:'���' c:n(1)",,,1,,,,'����')
   else
      rctcenr=slcf('tcen',,,,,"e:tcen h:'���' c:n(2) e:ntcen h:'������������' c:c(20) e:zen h:'����' c:c(10) e:knds h:'���' c:n(1) e:tcenp h:'���' c:n(3) e:prtcenp h:'%' c:n(6,2) e:sh h:'���' c:n(1) e:grtcen h:'NN' c:n(2) e:kpk h:'���' c:n(1) e:dec h:'��' c:n(1)",,,1,,,,'����')
   endif
   sele tcen
   go rctcenr
   tcenr=tcen
   ntcenr=ntcen
   zenr=zen
   kndsr=knds
   tcenpr=tcenp
   prtcenpr=prtcenp
   grtcenr=grtcen
   shr=sh
   kpkr=kpk
   if fieldpos('dec')#0
      decr=dec
   else
      decr=0
   endif
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           tcenins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           if netseek('t1','tcenr')
              netdel()
           endif
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           tcenins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()

stat func tcenins(p1)
if p1=nil
   store 0 to tcenr,tcenpr,prtcenpr,shr,grtcenr,kpkr
   ntcenr=space(20)
   zenr=space(10)
   kndsr=0
endif
clvoins=setcolor('gr+/b,n/w')
wvoins=wopen(6,15,18,60)
wbox(1)
do while .t.
   if p1=nil
      @ 0,1 say '���         ' get tcenr pict '99'
   else
      @ 0,1 say '���          '+str(tcenr,2)
   endif
   @ 1,1 say    '������������' get ntcenr
   @ 2,1 say    '��� ����    ' get zenr
   @ 3,1 say    '�����. � ���' get kndsr
   @ 4,1 say    '����⥫�    ' get tcenpr pict '999'
   @ 5,1 say    '% �� த�⥫' get prtcenpr pict '999.99'
   @ 6,1 say    'N �/� (��) ' get grtcenr pict '99'
   @ 7,1 say    '�ᥣ�� �����' get shr pict '9'
   @ 8,1 say    '���         ' get kpkr pict '9'
   @ 9,1 say    '�� ��� ��� ' get decr pict '9'
   @ 10,1 prom '<��୮>'
   @ 10,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC.or.tcenr=0
      exit
   endif
   menu to mvor
   if mvor=1
      if p1=nil
         if !netseek('t1','tcenr')
            netadd()
            netrepl('tcen,ntcen,zen,knds,tcenp,prtcenp,grtcen,sh,kpk',;
                    'tcenr,ntcenr,zenr,kndsr,tcenpr,prtcenpr,grtcenr,shr,kpkr')
            if fieldpos('dec')#0
               netrepl('dec','decr')
            endif
            exit
         else
            wmess('����� ��� �������',1)
         endif
      else
         if netseek('t1','tcenr')
            netrepl('ntcen,zen,knds,tcenp,prtcenp,grtcen,sh,kpk',;
                    'ntcenr,zenr,kndsr,tcenpr,prtcenpr,grtcenr,shr,kpkr')
            if fieldpos('dec')#0
               netrepl('dec','decr')
            endif
            exit
         endif
      endif
   endif
enddo
wclose(wvoins)
setcolor(clvoins)
retu
**************
func vdnal()
**************
clea
if gdTd<ctod('01.03.2013')
   netuse('vdnal')
else
   netuse('vdnaln','vdnal')
endif
sele vdnal
set orde to tag t1
go top
rcvdnalr=recn()
do while .t.
   sele vdnal
   go rcvdnalr
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   rcvdnalr=slcf('vdnal',,,,,"e:tvdnal h:'���' c:n(1) e:vdnal h:'���' c:n(2) e:cvdnal h:'���� ���. ' c:c(10) e:nvdnal h:'���� ������' c:c(40)",,,1,,,,'���� ���㬥�⮢')
   if lastkey()=K_ESC
      exit
   endif
   sele vdnal
   go rcvdnalr
   tvdnalr=tvdnal
   vdnalr=vdnal
   cvdnalr=cvdnal
   nvdnalr=nvdnal
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           vdnalins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           netdel()
           skip -1
           if bof()
              go top
           endif
           rcvdnalr=recn()
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           vdnalins(1)
   endc
endd
nuse('vdnal')
nuse('vdnaln')
retu .t.

******************
func vdnalins(p1)
******************
if p1=nil
   store 0 to tvdnalr,vdnalr
   cvdnalr=space(10)
   nvdnalr=space(40)
endif
clvnr=setcolor('gr+/b,n/w')
wvnr=wopen(9,15,15,60)
wbox(1)
do while .t.
   @ 0,1 say    '��� 1-���;2-��' get tvdnalr pict '9'
   @ 1,1 say    '���            ' get vdnalr  pict '99'
   @ 2,1 say    '���� ���⪮�  ' get cvdnalr
   @ 3,1 say    '���� ������    ' get nvdnalr
   @ 4,1 prom '<��୮>'
   @ 4,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC.or.vdnalr=0
      exit
   endif
   menu to mvor
   if mvor=1
      if p1=nil
         if !netseek('t1','tvdnalr,vdnalr')
            netadd()
            netrepl('tvdnal,vdnal,cvdnal,nvdnal','tvdnalr,vdnalr,cvdnalr,nvdnalr')
            rcvdnalr=recn()
            exit
         else
            wmess('����� ��� �������',1)
         endif
      else
         if netseek('t1','tvdnalr,vdnalr')
            netrepl('tvdnal,vdnal,cvdnal,nvdnal','tvdnalr,vdnalr,cvdnalr,nvdnalr')
            exit
         endif
      endif
   endif
enddo
wclose(wvnr)
setcolor(clvnr)
retu .t.

