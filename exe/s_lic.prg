#include "common.ch"
#include "inkey.ch"
clea
netuse('lic')
do while .t.
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   licr=slcf('lic',1,,18,,"e:lic h:'���' c:n(1) e:nlic h:'������������' c:c(20)",'lic',,1)
   sele lic
   netseek('t1','licr')
   nlicr=nlic
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           licins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           if netseek('t1','licr')
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
   store 0 to licr
   nlicr=space(20)
endif
cllicins=setcolor('gr+/b,n/w')
wlicins=wopen(10,15,15,60)
wbox(1)
do while .t.
   if p1=nil
      @ 0,1 say '���         ' get licr pict '9'
   else
      @ 0,1 say '���          '+str(licr,1)
   endif
   @ 1,1 say    '������������' get nlicr
   @ 3,1 prom '<��୮>'
   @ 3,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mlicr
   if mlicr=1
      if p1=nil
         if !netseek('t1','licr')
            netadd()
            netrepl('lic,nlic','licr,nlicr')
            exit
         else
            wselect(0)
            save scre to sclicins
            mess('����� ��� �������',1)
            rest scre from sclicins
            wselect(wlicins)
         endif
      else
         if netseek('t1','licr')
            netrepl('nlic','nlicr')
            exit
         endif
      endif
   endif
enddo
wclose()
setcolor(cllicins)
retu
