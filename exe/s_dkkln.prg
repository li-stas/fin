#include "inkey.ch"
netuse('kln')
netuse('s_tag')
netuse('dkkln')
netuse('dknap')
netuse('kpl')
netuse('nap')
rcdkklnr=recn()
netuse('dkklns')
rcdkklnsr=recn()
store 0 to bsr,kklr,sklr,dnr,knr,dbr,krr
store '' to nklr
if gnScout=0
   clea
endif
for_r='.t.'
forr=for_r
do while .t.
   sele dkkln
   go rcdkklnr
   kklr=kkl
   bsr=bs
   sklr=skl
   nklr=getfield('t1','kklr','kln','nkl')
   dnr=dn
   knr=kn
   dbr=db
   krr=kr
   if dkklnr=1.or.gnadm=1
      foot('F3,F4,F7,F8,INS,ESC','������,���४��,�� ���.,����,��������,��室')
   else
      foot('F3,ESC','������,��室')
   endif
   rcdkklnr=slcf('dkkln',1,1,18,,"e:kkl h:'���' c:n(7) e:getfield('t1','dkkln->kkl','kln','nkl') h:'������������' c:c(18) e:bs h:'C��' c:n(6) e:skl h:'���.' c:n(4) e:dn h:'��.�.' c:n(9,2) e:kn h:'��.�.' c:n(9,2) e:db h:'�����' c:n(9,2) e:kr h:'�।��' c:n(9,2)",,,1,,forr)
   sele dkkln
   go rcdkklnr
   kklr=kkl
   bsr=bs
   sklr=skl
   nklr=getfield('t1','kklr','kln','nkl')
   dnr=dn
   knr=kn
   dbr=db
   krr=kr
   do case
      case lastkey()=K_F3 .and. (dkklnr=1.or.gnadm=1)
           dkklnflt()
      case lastkey()=K_ESC
           exit
      case lastkey()=-3 .and. (dkklnr=1.or.gnadm=1)
           dkklnkor(1)
      case lastkey()=22 .and. (dkklnr=1.or.gnadm=1)
           dkklnkor(0)
      case lastkey()=-6 .and. (dkklnr=1.or.gnadm=1) // �� ��砫�
           dkklnn()
           sele dkkln
           go rcdkklnr
      case lastkey()=13
           sele dkklns
           if netseek('t1','kklr,bsr')
              foot('F4','���४��')
              rcdkklnsr=recn()
              do while .t.
                 go rcdkklnsr
                 rcdkklnsr=slcf('dkklns',10,1,8,,"e:skl h:'���' c:n(4) e:getfield('t1','dkklns->skl','s_tag','fio') h:'�㯥ࢠ����' c:c(20) e:dn h:'��.�.' c:n(9,2) e:kn h:'��.�.' c:n(9,2) e:db h:'�����' c:n(9,2) e:kr h:'�।��' c:n(9,2)",,,,'kkl=kklr.and.bs=bsr',,,str(kklr,7)+' '+str(bsr,6))
                 go rcdkklnsr
                 dnr=dn
                 knr=kn
                 if lastkey()=K_ESC
                    exit
                 endif
                 do case
                    case lastkey()=-3.and.year(gdTd)=2007.and.month(gdTd)=4
                         dkklnsi()
                 endcase
              enddo
           endif
      case lastkey()=-7
           sele dknap
           fldnomr=1
           if netseek('t1','kklr,bsr')
              dkr0r=ctod('')
              if skl=0
                 dkr0r=dkr
              endif
              foot('','')
              rcdknapr=recn()
              do while .t.
                 go rcdknapr
                 rcdknapr=slce('dknap',10,1,8,,"e:skl h:'���' c:n(4) e:getfield('t1','dknap->skl','nap','nnap') h:'���ࠢ�����' c:c(10) e:dn h:'��.�.' c:n(9,2) e:kn h:'��.�.' c:n(9,2) e:db h:'�����' c:n(9,2) e:kr h:'�।��' c:n(9,2) e:dn+db-kn-kr h:'���줮K' c:n(9,2) e:dbo h:'�����O' c:n(9,2) e:ddb h:'���' c:d(10) e:dkr h:'���' c:d(10) e:ddbo h:'���o' c:d(10) e:ddb-iif(!empty(dkr),dkr,dkr0r) h:'���' c:n(3) e:ddbo-iif(!empty(dkr),dkr,dkr0r) h:'���' c:n(3)",,,,'kkl=kklr.and.bs=bsr',,,str(kklr,7)+' '+str(bsr,6))
                 go rcdknapr
                 dnr=dn
                 knr=kn
                 if lastkey()=K_ESC
                    exit
                 endif
                 do case
                    case lastkey()=19 // Left
                         fldnomr=fldnomr-1
                         if fldnomr=0
                            fldnomr=1
                         endif
                    case lastkey()=4 // Right
                         fldnomr=fldnomr+1
                    case lastkey()=-3.and.year(gdTd)=2007.and.month(gdTd)=4
*                         dkklnsi()
                 endcase
              enddo
           endif
   endcase
enddo
nuse()

proc dkklnflt
priv kklr,sklr,bsr,prtzdocr
cldkklnflt=setcolor('gr+/b,n/w')
wdkklnflt=wopen(10,20,15,50)
wbox(1)
do while .t.
   stor 0 to kklr,sklr,bsr,prtzdocr
   @ 0,1 say '���         ' get kklr pict '9999999'
   @ 1,1 say '���        ' get bsr pict '999999'
   @ 2,1 say '�����       ' get sklr pict '9999'
   @ 3,1 say '��� ����    ' get prtzdocr pict '9'
   read
   do case
      case lastkey()=K_ESC
           exit
      case lastkey()=13
           if kklr=0.and.sklr=0.and.bsr=0.and.prtzdocr=0
              set filt to
              forr=for_r
              go top
           else
              if prtzdocr=1
                 forr=for_r+".and.getfield('t1','dkkln->kkl','kpl','tzdoc')=1"
              else
                 forr=for_r
              endif
              sele dkkln
              do case
                 case kklr#0.and.bsr#0.and.skl#0
                      set filt to
                      go top
                      if netseek('t1','kklr,bsr,sklr')
                         rcdkklnr=recn()
                      endif
                 case kklr#0.and.bsr#0.and.sklr=0
                      set filt to
                      go top
                      if netseek('t1','kklr,bsr')
                         rcdkklnr=recn()
                      endif
                 case kklr#0.and.bsr=0.and.sklr=0
                      set filt to
                      go top
                      if netseek('t1','kklr')
                         rcdkklnr=recn()
                      endif
                 case kklr=0
                      set filt to
                      do case
                         case bsr#0.and.sklr#0
                              set filt to bs=bsr.and.skl=sklr
                              go top
                              rcdkklnr=recn()
                         case bsr#0.and.sklr=0
                              set filt to bs=bsr
                              go top
                              rcdkklnr=recn()
                         case bsr=0.and.sklr#0
                              set filt to skl=sklr
                              go top
                              rcdkklnr=recn()
                      endcase
              endcase
              if eof().and.bof()
                 set filt to
                 go top
                 rcdkklnr=recn()
              endif
           endif
           exit
   endcase
enddo
wclose(wdkklnflt)
setcolor(cldkklnflt)

proc dkklnkor(p1)
if p1=0
   store 0 to kklr,bsr,sklr,dnr,knr
   nklr=space(20)
endif
cldkklnflt=setcolor('gr+/b,n/w')
wdkklnflt=wopen(05,10,19,60)
wbox(1)
do while .t.
if p1=0
   @ 0,1 say '���          ' get kklr pict '9999999' valid kkld()
   @ 1,1 say '������������ ' get nklr
   @ 2,1 say '���         ' get bsr pict '999999'
   @ 3,1 say '�����        ' get sklr pict '9999'
else
   @ 0,1 say '���          ' + str(kklr,7)
   @ 1,1 say '������������ ' + getfield('t1','kklr','kln','nkl')
   @ 2,1 say '���         ' +str(bsr,6)
endif
   @ 3,1 say '�����        ' get sklr pict'9999'
   @ 4,1 say '����� �� ���.' get dnr pict '999999.99'
   @ 5,1 say '�।. �� ���.' get knr pict '999999.99'
   @ 6,1 prom '<��୮>'
   @ 6,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to minsr
   sele dkkln
   if minsr=1
      if p1=0
         if !netseek('t1','kklr,bsr,sklr')
            netadd()
            netrepl('kkl,bs,skl,dn,kn','kklr,bsr,sklr,dnr,knr')
            exit
         else
            wselect(0)
            save scre to scdkkln
            mess('����� ��� 㦥 ����',1)
            rest scre from scdkkln
            wselect(wdkklnflt)
            sele dkkln
         endif
      else
         if netseek('t1','kklr,bsr')
            netrepl('skl,dn,kn','sklr,dnr,knr')
         endif
      endif
      exit
   endif
enddo
wclose(wdkklnflt)
setcolor(cldkklnflt)
retu

func kkld
sele kln
if kklr=0
   wselect(0)
   kklr=slct_kl(10,1,12)
   wselect(wdkklnflt)
endif
if kklr=0
   sele dkkln
   return.f.
endif
if netseek('t1','kklr')
   nklr=nkl
else
   kklr=0
   sele dkkln
   return.f.
endif
*@ 1,14 say nklr
retu .t.

func dkklnn()

if gnScOut=0
   save scre to scdkkln
   clea
endif
dtr=bom(gdTd)-1
yy_r=year(dtr)
mm_r=month(dtr)
pathr=gcPath_e+'g'+str(yy_r,4)+'\m'+iif(mm_r<10,'0'+str(mm_r,1),str(mm_r,2))+'\bank\'
if !netfile('dkkln',1)
   retu
endif
if !netuse('dkkln','dkklnp',,1)
   retu
endif
if gnScOut=0
   aqstr=1
   aqst:={"��ᬮ��","���४��"}
   aqstr:=alert(" ",aqst)
   if lastkey()=K_ESC
      rest scre from scdkkln
      nuse('dkklnp')
      retu .t.
   endif
else
   aqstr=2
endif
?'DKKLN'
sele dkklnp
do while !eof()
   kklr=kkl
   bsr=bs
   sklr=skl
   dnr=dn
   knr=kn
   dbr=db
   krr=kr
   ddbr=ddb
   dkrr=dkr
   aaa=dnr-knr+dbr-krr
   store 0 to dn_r,kn_r
   if aaa>0
      dn_r=aaa
   endif
   if aaa<0
      kn_r=abs(aaa)
   endif
   sele dkkln
   if netseek('t1','kklr,bsr,sklr')
      if str(dn,12,2)#str(dn_r,12,2).or.str(kn,12,2)#str(kn_r,12,2)
*         wait
         ?str(kklr,7)+' '+str(bsr,6)+' '+str(skl,7)
         ?'� '+str(dn_r,12,2)+' '+str(kn_r,12,2)+' � '+str(dn,12,2)+' '+str(kn,12,2)
         if aqstr=2
            netrepl('dn,kn','dn_r,kn_r')
         endif
      endif
      if ddbr>ddb.or.dkrr>dkr
         if aqstr=2
            netrepl('ddb,dkr','ddbr,dkrr')
         endif
      endif
   else
      if dn_r#0.or.kn_r#0
         netadd()
         ?'� '+str(dn_r,12,2)+' '+str(kn_r,12,2)+' � ���'
         if aqstr=2
            netrepl('kkl,bs,skl,dn,kn,ddb,dkr','kklr,bsr,sklr,dn_r,kn_r,ddbr,dkrr')
         endif
      endif
   endif
   sele dkklnp
   skip
enddo

sele dkkln
go top
store 9999999999 to kklr,bsr,sklr
do while !eof()
   if kkl=kklr.and.bs=bsr.and.skl=sklr
      ?str(kkl,7)+' '+str(bs,6)+' '+str(skl,7)+' �������'
      if aqstr=2
         netdel()
         skip
         loop
      endif
   endif
   kklr=kkl
   bsr=bs
   sklr=skl
   if dn+kn=0
      sele dkkln
      skip
      loop
   endif
   sele dkklnp
   if !netseek('t1','kklr,bsr,sklr')
      ?str(kklr,7)+' '+str(bsr,6)+' '+str(sklr,7)+' ��� � ��諮�'
      if aqstr=2
         sele dkkln
         netrepl('dn,kn','0,0')
      endif
   endif
   sele dkkln
   skip
enddo

nuse('dkklnp')

if select('dkklns')#0
   if netfile('dkklns',1)
      if netuse('dkklns','dkklnsp',,1)
         ?'DKKLNS'
         sele dkklnsp
         do while !eof()
            kklr=kkl
            bsr=bs
            sklr=skl
            dnr=dn
            knr=kn
            dbr=db
            krr=kr
            aaa=dnr-knr+dbr-krr
            store 0 to dn_r,kn_r
            if aaa>0
               dn_r=aaa
            endif
            if aaa<0
               kn_r=abs(aaa)
            endif
            sele dkklns
            if netseek('t1','kklr,bsr,sklr')
               if str(dn,12,2)#str(dn_r,12,2).or.str(kn,12,2)#str(kn_r,12,2)
                  ?str(kklr,7)+' '+str(bsr,6)+' '+str(skl,7)
                  ?'� '+str(dn_r,12,2)+' '+str(kn_r,12,2)+' � '+str(dn,12,2)+' '+str(kn,12,2)
                  if aqstr=2
                     netrepl('dn,kn','dn_r,kn_r')
                  endif
               endif
            else
               if dn_r#0.or.kn_r#0
                  ?'� '+str(dn_r,12,2)+' '+str(kn_r,12,2)+' � ���'
                  if aqstr=2
                     netadd()
                     netrepl('kkl,bs,skl,dn,kn','kklr,bsr,sklr,dn_r,kn_r')
                  endif
               endif
            endif
            sele dkklnsp
            skip
         enddo

         sele dkklns
         go top
         store 9999999999 to kklr,bsr,sklr
         do while !eof()
            if kkl=kklr.and.bs=bsr.and.skl=sklr
               ?str(kkl,7)+' '+str(bs,6)+' '+str(skl,7)+' �������'
               if aqstr=2
                  netdel()
                  skip
                  loop
               endif
            endif
            kklr=kkl
            bsr=bs
            sklr=skl
            if dn+kn=0
               sele dkklns
               skip
               loop
            endif
            sele dkklnsp
            if !netseek('t1','kklr,bsr,sklr')
               ?str(kklr,7)+' '+str(bsr,6)+' '+str(sklr,7)+' ��� � ��諮�'
               if aqstr=2
                  sele dkklns
                  netrepl('dn,kn','0,0')
               endif
            endif
            sele dkklns
            skip
         enddo
         nuse('dkklnsp')
      endif
   endif
endif

wmess('���४�� �����祭�',0)
if gnScOut=0
   rest scre from scdkkln
endif
retu .t.


func dkklnsi()
cldkklns=setcolor('gr+/b,n/w')
wdkklns=wopen(10,20,14,60)
wbox(1)
do while .t.
   @ 0,1 say '����� ��� ' get dnr pict '9999999.99'
   @ 1,1 say '�।�� ���' get knr pict '9999999.99'
   @ 2,1 prom '<��୮>'
   @ 2,col()+1 prom '<�� ��୮>'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mdksr
   if mdksr=1
      sele dkklns
      netrepl('dn,kn','dnr,knr')
      exit
   endif
enddo
wclose(wdkklns)
setcolor(cldkklns)
retu .t.


/**************
* ��७�� ���⠪�� ����� ����栬�
**************/
func NDKKln()
  clea
  cldkklns=setcolor('gr+/b,n/w')
  wdkklns=wopen(10,20,13,60)
  wbox(1)
  dt1r=gdTd
  dt2r=gdTd
  bs_r=0
  @ 0,1 say '��ਮ� �' get dt1r
  @ col(), row()+1 say "�� "
  @ col(), row()+1 say  DTOC(dt2r) COLOR "W+/B"
  @ 1,1 say '���    ' get bs_r pict '999999'
  read
  wclose(wdkklns)
  setcolor(cldkklns)
  if lastkey()=K_ESC
     retu .t.
  endif
  for yyr=year(dt1r) to year(dt2r)
      do case
         case year(dt1r)=year(dt2r)
              m1r=month(dt1r)
              m2r=month(dt2r)
         case yyr=year(dt1r)
              m1r=month(dt1r)
              m2r=12
         case yyr=year(dt2r)
              m1r=1
              m2r=month(dt2r)
         othe
              m1r=1
              m2r=12
      endcase
      for mmr=m1r to m2r
          dtr=ctod('01.'+iif(mmr<10,'0'+str(mmr,1),str(mmr,2))+'.'+str(yyr,4))
          dtpr=addmonth(dtr,-1)
          pathPr=gcPath_e+'g'+str(year(dtpr),4)+'\m'+iif(month(dtpr)<10,'0'+str(month(dtpr),1),str(month(dtpr),2))+'\bank\'
          pathTr=gcPath_e+'g'+str(year(dtr),4)+'\m'+iif(month(dtr)<10,'0'+str(month(dtr),1),str(month(dtr),2))+'\bank\'
          pathr=pathPr
          if !netfile('dkkln',1)
             loop
          endif
          pathr=pathTr
          if !netfile('dkkln',1)
             loop
          endif

          ?'DKKLN'
          pathr=pathPr
          netuse('dkkln','dkklnp',,1)
          pathr=pathTr
          netuse('dkkln',,,1)
          ?pathr
          sele dkklnp
          do while !eof()
             kklr=kkl
             bsr=bs
             if bs_r#0
                if bsr#bs_r
                   sele dkklnp
                   skip
                   loop
                endif
             endif
             sklr=skl
             dnr=dn
             knr=kn
             dbr=db
             dbor=dbo
             krr=kr
             ddbr=ddb
             ddbor=ddbo
             dkrr=dkr
             aaa=dnr-knr+dbr-krr
             store 0 to dn_r,kn_r
             if aaa>0
                dn_r=aaa
             endif
             if aaa<0
                kn_r=abs(aaa)
             endif
             sele dkkln
             if netseek('t1','kklr,bsr,sklr')
                netrepl('dn,kn','dn_r,kn_r')
                if ddbr>ddb.or.dkrr>dkr
                   netrepl('ddb,dkr','ddbr,dkrr')
                endif
             else
                netadd()
                netrepl('kkl,bs,skl,dn,kn,ddb,dkr,dbo,ddbo','kklr,bsr,sklr,dn_r,kn_r,ddbr,dkrr,dbor,ddbor')
             endif
             sele dkklnp
             skip
          enddo
          sele dkkln
          go top
          store 9999999999 to kklr,bsr,sklr
          do while !eof()
             if kkl=kklr.and.bs=bsr.and.skl=sklr
                netdel()
                skip
                loop
             endif
             kklr=kkl
             bsr=bs
             if bs_r#0
                if bsr#bs_r
                   sele dkkln
                   skip
                   loop
                endif
             endif
             sklr=skl
             if dn+kn=0
                sele dkkln
                skip
                loop
             endif
             sele dkklnp
             if !netseek('t1','kklr,bsr,sklr')
                sele dkkln
                netrepl('dn,kn','0,0')
             endif
             sele dkkln
             skip
          enddo
          nuse('dkkln')
          nuse('dknap')
          nuse('dkklnp')
      next
  next     
