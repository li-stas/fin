#include "common.ch"
#include "inkey.ch"
clea
netuse('vo')
do while .t.
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   vor=slcf('vo',,,,,"e:vo h:'���' c:n(2) e:nvo h:'������������' c:c(10)",'vo',,1)
   sele vo
   netseek('t1','vor')
   nvor=nvo
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           voins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           if netseek('t1','vor')
              netdel()
           endif
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           voins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()

stat func voins(p1)
if p1=nil
   store 0 to vor
   nvor=space(10)
endif
clvoins=setcolor('gr+/b,n/w')
wvoins=wopen(10,15,15,60)
wbox(1)
do while .t.
   if p1=nil
      @ 0,1 say '���         ' get vor pict '99'
   else
      @ 0,1 say '���          '+str(vor,2)
   endif
   @ 1,1 say    '������������' get nvor
   @ 3,1 prom '<��୮>'
   @ 3,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mvor
   if mvor=1
      if p1=nil
         if !netseek('t1','vor')
            netadd()
            netrepl('vo,nvo','vor,nvor')
            exit
         else
            wselect(0)
            save scre to scvoins
            mess('����� ��� �������',1)
            rest scre from scvoins
            wselect(wvoins)
         endif
      else
         if netseek('t1','vor')
            netrepl('nvo','nvor')
            exit
         endif
      endif
   endif
enddo
wclose(wvoins)
setcolor(clvoins)
retu
