#include "common.ch"
#include "inkey.ch"
clea
netuse('lice')
do while .t.
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   licer=slcf('lice',1,,18,,"e:lice h:'���' c:n(1) e:nlice h:'������������' c:c(20)",'lice',,1)
   sele lice
   netseek('t1','licer')
   nlicer=nlice
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           licins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           if netseek('t1','licer')
              netdel()
           endif
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           licins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()

stat func licins(p1)
if p1=nil
   store 0 to licer
   nlicer=space(20)
endif
cllicins=setcolor('gr+/b,n/w')
wlicins=wopen(10,15,15,60)
wbox(1)
do while .t.
   if p1=nil
      @ 0,1 say '���         ' get licer pict '99'
   else
      @ 0,1 say '���          '+str(licer,2)
   endif
   @ 1,1 say    '������������' get nlicer
   @ 3,1 prom '<��୮>'
   @ 3,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mlicr
   if mlicr=1
      if p1=nil
         if !netseek('t1','licer')
            netadd()
            netrepl('lice,nlice','licer,nlicer')
            exit
         else
            wselect(0)
            save scre to sclicins
            mess('����� ��� �������',1)
            rest scre from sclicins
            wselect(wlicins)
         endif
      else
         if netseek('t1','licer')
            netrepl('nlice','nlicer')
            exit
         endif
      endif
   endif
enddo
wclose(wlicins)
setcolor(cllicins)
retu
