#include "inkey.ch"
clea
netuse('dclr')
do while .t.
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
*   if fieldpos('vt')=0
      kzr=slcf('dclr',1,,18,,"e:kz h:'N' c:n(2) e:nz h:'������������' c:c(20) e:pr h:'��' c:n(2) e:prp h:'��' c:n(2)",'kz',,1)
*   else
*      kzr=slcf('dclr',1,,18,,"e:kz h:'N' c:n(2) e:nz h:'������������' c:c(20) e:pr h:'��' c:n(2) e:prp h:'��' c:n(2) e:vt h:'��' c:n(1)",'kz',,1)
*   endif
   sele dclr
   netseek('t1','kzr')
   nzr=nz
   prr=pr
   prpr=prp
*   if fieldpos('vt')#0
*      vtr=vt
*   else
*      vtr=0
*   endif
*  frmlr=frml
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           dclrins(0)
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           LOCATE FOR kz=kzr
           if FOUND()
              netdel()
           endif
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           dclrins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()

func dclrins(p1)
foot('','')
if p1=0
   store 0 to kzr,prr,prpr,vtr
   nzr=space(20)
*   frmlr=space(40)
endif
cldclrins=setcolor('gr+/b,n/w')
wdclrins=wopen(10,5,17,75)
wbox(1)
do while .t.
   if p1=0
      @ 0,1 say '���         ' get kzr pict '99'
   else
      @ 0,1 say '���          '+str(kzr,2)
   endif
   @ 1,1 say    '������������' get nzr
   @ 2,1 say    '�� ��� ���' get prr pict '9'
   @ 3,1 say    '�� ��� ���' get prpr pict '9'
*   @ 4,1 say    '�� �� ���  ' get vtr pict '9'
*   @ 3,1 say    '���㫠     ' get frmlr
   read
   if lastkey()=K_ESC
      exit
   endif
   @ 5,50 prom '<��୮>'
   @ 5,col()+1 prom '<�� ��୮>'
   menu to mdclrr
   if lastkey()=K_ESC
      exit
   endif
   if mdclrr=1
      if p1=0
         if !netseek('t1','kzr')
            netadd()
            netrepl('kz,nz,pr,prp','kzr,nzr,prr,prpr')
*            if fieldpos('vt')#0
*               netrepl('vt','vtr')
*            endif
            exit
         else
            wselect(0)
            save scre to scdclrins
            mess('����� ��� �������',1)
            rest scre from scdclrins
            wselect(wdclrins)
         endif
      else
         if netseek('t1','kzr')
            netrepl('nz,pr,prp','nzr,prr,prpr')
 *           if fieldpos('vt')#0
 *              netrepl('vt','vtr')
 *           endif
            exit
         endif
      endif
   endif
enddo
wclose()
setcolor(cldclrins)
retu
*************
func fop()
*************
clea
netuse('fop')
rcfopr=recn()
do while .t.
   go rcfopr
   foot('INS,DEL,F4','��������,�������,���४��')
   rcfopr=slcf('fop',1,,18,,"e:fop h:'���' c:n(2) e:nfop h:'������������' c:c(30) ",,,1,,,,'���� ������')
   sele fop
   fopr=fop
   nfopr=nfop
   go rcfopr
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           fopins(0)
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           netdel()
           skip -1
           if bof()
              go top
           endif
           rcfopr=recn()
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           fopins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()
retu .t.
****************
func fopins(p1)
****************
foot('','')
if p1=0
   store 0 to fopr
   nfopr=space(30)
endif
clfop=setcolor('gr+/b,n/w')
wfop=wopen(10,15,14,60)
wbox(1)
do while .t.
   if p1=0
      @ 0,1 say '���         ' get fopr pict '99'
   else
      @ 0,1 say '���          '+str(fopr,2)
   endif
   @ 1,1 say    '������������' get nfopr
   read
   if lastkey()=K_ESC
      exit
   endif
   @ 2,1 prom '<��୮>'
   @ 2,col()+1 prom '<�� ��୮>'
   menu to mfopr
   if lastkey()=K_ESC
      exit
   endif
   if mfopr=1
      if p1=0
         netadd()
         netrepl('fop,nfop','fopr,nfopr')
         rcfopr=recn()
         exit
      else
         netrepl('nfop','nfopr')
         exit
      endif
   endif
enddo
wclose(wfop)
setcolor(clfop)
retu .t.

