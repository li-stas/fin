* �����
#include "inkey.ch"
clea
netuse('bs')
netuse('banks')
rcbanksr=recn()
kobr=space(15)
notbr=space(40)
bbsr=0
forr='.t.'
for_r='.t.'
do while .t.
   foot('INS,DEL,F3,F4','��������,�������,������,���४��')
   rcbanksr=slcf('banks',1,,18,,"e:kob h:'���' c:c(15) e:otb h:'������������' c:c(40) e:bbs h:'���' c:n(6)",,,1,,forr,,'�����')
   if lastkey()=K_ESC
      exit
   endif
   sele banks
   go rcbanksr
   kobr=kob
   notbr=otb
   bbsr=bbs
   do case
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           banksins()
      case lastkey()=7 .and. (dkklnr=1.or.gnadm=1)
           netdel()
           skip -1
           if bof()
              go top
           endif
           rcbanksr=recn()
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           banksins(1)
      case lastkey()=-2
           banksflt()
   endc
endd
nuse()

func banksins(p1)
if p1=nil
   kobr=space(15)
   notbr=space(40)
   bbsr=0
endif
clbanks=setcolor('gr+/b,n/w')
wbank=wopen(10,20,15,60)
wbox(1)
do while .t.
   @ 0,1 say '���         ' get kobr
   @ 1,1 say '������������' get notbr
   @ 2,1 say '���        ' get bbsr pict '999999'
   @ 3,1 prom '<��୮>'
   @ 3,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mbanksr
   if mbanksr=1
      if !netseek('t1','kobr')
         netadd()
      endif
      netrepl('kob,otb,bbs','kobr,notbr,bbsr')
      exit
   endif
enddo
wclose(wbank)
setcolor(clbanks)
retu .t.

func banksflt()
clbanks=setcolor('gr+/b,n/w')
wbank=wopen(10,20,15,60)
wbox(1)
do while .t.
   kobr=space(15)
   notbr=space(40)
   bbsr=0
   @ 0,1 say '���         ' get kobr
   read
   if empty(kobr)
      @ 1,1 say '������������' get notbr
      read
      if empty(notbr)
         @ 2,1 say '���        ' get bbsr pict '999999'
         read
      endif
   endif
   if lastkey()=K_ESC
      forr=for_r
      exit
   endif
   do case
      case !empty(bbsr)
           if bbsr#999999
              forr=for_r+'.and.bbs=bbsr'
           else
              forr=for_r+'.and.bbs#0'
           endif
      case !empty(notbr)
           notbr=upper(alltrim(notbr))
           forr=for_r+'.and.at(notbr,upper(otb))#0'
      case !empty(kobr)
           forr=for_r+'.and.kob=kobr'
      other
           forr=for_r
   endc
   sele banks
   go top
   rcbanksr=recn()
   exit
enddo
wclose(wbank)
setcolor(clbanks)
retu .t.

