#include "common.ch"
#include "inkey.ch"
clea
netuse('tipd')
do while .t.
   foot('INS,DEL,F4,ESC','��������,�������,���४��,��室')
   rctipd=slcf('tipd',1,,18,,"e:tipo h:'��' c:n(2) e:tip h:'���' c:n(3) e:ndok h:'������������' c:c(30) e:prz h:'�' c:n(1)")
   sele tipd
   go rctipd
   tipor=tipo
   tipr=tip
   ndokr=ndok
   przr=prz
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           tipdins(0)
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           if netseek('t1','tipor,tipr')
              netdel()
           endif
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           tipdins(1)
      case lastkey()=K_ESC
           exit
   endc
enddo
nuse()

func tipdins(p1)
if p1=0
   store 0 to tipor,tipr
   store space(30) to ndokr
endif
cltipdins=setcolor('gr+/b,n/w')
wtipdins=wopen(10,20,16,70)
wbox(1)
do while .t.
   if p1=0
      @ 0,1 say '��㯯� ����.' get tipor pict '99'
      @ 1,1 say '��� ���㬥��' get tipr pict '999'
   else
      @ 0,1 say '��㯯� ����. '+str(tipor,2)
      @ 1,1 say '��� ���㬥�� '+str(tipr,3)
   endif
   @ 2,1 say '������������ ' get ndokr
   @ 3,1 say '�ਧ��� ��/��' get przr
   @ 4,1 prom '<��୮>'
   @ 4,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mtipdins
   if mtipdins =1
      if p1=0
         if !netseek('t1','tipor,tipr')
            netadd()
            netrepl('tipo,tip,ndok,prz','tipor,tipr,ndokr,przr')
            exit
         else
            wselect(0)
            save scre to sctipdins
            mess('����� ⨯ ���㬥�� 㦥 �������',1)
            rest scre from sctipdins
            wselect(wtipdins)
         endif
      else
         if netseek('t1','tipor,tipr')
            netrepl('ndok,prz','ndokr,przr')
            exit
         endif
      endif
   endif
endd
wclose(wtipdins)
setcolor(cltipdins)
retu
