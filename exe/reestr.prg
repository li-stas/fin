#include "inkey.ch"
stor 0 to bsr
dtot:={}

*netUse('dokz')
*netUse('doks')
netUse('dokk')
netUse('kln')
*netUse('operb')
netUse('bs')
*netUse('tipd')

set color to g/n,n/g,,,
stor 1 to dell,deli
@ 0,0 clea
do while .t.
  *����� �ࠢ�筨�� �����ᮢ�� ��⮢
  @ MAXROW()-1,25 say '�롥�� ���� : '
  do whil dell<>7
    sele bs
    @ MAXROW()-1,42 get bsr pict '999999'
    read
    if bsr<>0
      exit
    else
*      bsr=slct_bs(10,1,12)
       bsr=slcf('bs',,,,,"e:bs h:'���' c:n(6) e:nbs h:'������������' c:c(20)",'bs')
    endi
    exit
  endd
  if bsr=0
    exit
  endi
  sele bs
  set color to r+/n,n/gr+,,,
  @ MAXROW()-1,0 clea
  if !netseek('t1','bsr')
    @ MAXROW()-1,20
    wait '������ᮢ��� ��� � �⨬ ����� ���! ������ ���� �������...'
    @ MAXROW()-1,0 clear
    set color to g/n,n/g,,,
    loop
  endi
  @ MAXROW(),2 say '���: '+str(bsr,6)+' - '+nbs
  otch=obs
  ostn=DN-KN
  stor 0 to kntr_d,kntr_k
  set color to g/n,n/g,,,
  ******************************
  ddcr=gdTdn
  ddcr1=gdTdn
  ddcr2=gdTd
  @ 1,66 say ddcr2
  @ 2,22 say '������ �������� �� ��� : � '
  @ 2,53 get ddcr1
  @ 2,62 say ' �� '
  @ 2,66 get ddcr2
  read
  @ 2,53 say ddcr1
  @ 2,66 say ddcr2
   ////////////////////////////////
  do whil .t.
    @ 0,0 Clear To 3,52
    @ 0,0,3,45  box frame
    @ 0,8 say '����஫�� �㬬� �� �����:'
    @ 1,1 say '����� :'
    @ 2,1 say '�।��:'
    stor 0 to sum_k,sum_d,sum_kn,sum_dn
    sele dokk
    copy stru to adb
    copy stru to akr
    sele 0
    use adb excl
    sele 0
    use akr excl
    sele dokk
    Go top
    @ MAXROW()-1,2 Say '�����'

    stor 0 to sum_d,sum_dn,sun_k,sum_kn
    set orde to tag t2
    if netseek('t2','bsr')
       do while bs_d=bsr
          do case
             case ddk<ddcr1
                 sum_dn=sum_dn+bs_s
             case ddk>=ddcr1.and.ddk<=ddcr2
                 sum_d=sum_d+bs_s
                 arec:={}
                 getrec()
                 sele adb
                 netadd()
                 putrec()
          endc
          sele dokk
          skip
       endd
    endif
    set orde to tag t3
    if netseek('t3','bsr')
       do while bs_k=bsr
          do case
             case ddk<ddcr1
                 sum_kn=sum_kn+bs_s
             case ddk>=ddcr1.and.ddk<=ddcr2
                 sum_k=sum_k+bs_s
                 arec:={}
                 getrec()
                 sele akr
                 netadd()
                 putrec()
          endc
          sele dokk
          skip
       endd
    endif
    nuse('dokk')
    @ 1,25 say sum_d pict '99999999999.99'
    @ 2,25 say sum_k pict '99999999999.99'
    sele adb
    inde on str(kkl,7)+dtos(ddk)+str(rn,6)+str(mn,6)+str(rnd,6) tag t1
    sele akr
    inde on str(kkl,7)+dtos(ddk)+str(rn,6)+str(mn,6)+str(rnd,6) tag t1

    deli=1
    @ MAXROW()-1,2       prom '�����'
    @ MAXROW()-1,col()+5 prom '����� ࠡ���'
    menu to deli
    exit
  endd
  if deli=2
     sele adb
     use
     erase adb.dbf
     erase adb.cdx
     sele akr
     use
     erase akr.dbf
     erase akr.cdx
     nuse()
     retu
  endif
  stor 0 to kklr,rndr,ssdr,ssd_d,ssd_k
  List = 0
  Stroka = 50
  if gnOut=1
     set prin to lpt1
  else
     set prin to txt.txt
  endif
  Set Device To Print
  set prin on
  if !empty(gcPrn)
     ?chr(27)+'E'+chr(27)+'&l1h26a0O'+chr(27)+chr(27)+'(3R'+chr(27)+'(s0p21.00h0s0b4099T'+chr(27)  // ������� �4
  else
     ?chr(27)+chr(77)+chr(15)
  endif
  shap_1()
  @ prow()+1,26 say '�室�騥 ���⪨ �� '+dtoc(ddcr1)+' '+Str(Ostn+Sum_dn-Sum_kn)
  Stroka = Stroka + 1
  prn_pld1()
  Stroka = 50
  shap_1()
  prn_pld2()
  exit
endd
eject
set prin to
Set Device To Screen
@ 0,0 clea
if select('adb')#0
   sele adb
   use
endif
erase adb.dbf
erase adb.cdx
if select('akr')#0
   sele akr
   use
endif
erase akr.dbf
erase akr.cdx
nuse()

//////////////////////////////
proc shap_1

If Stroka >= 50
  Stroka = 1
  List = List + 1
  If List > 1
    Eject
  EndIf
  @ prow()+1,5 say '������ ������ ���㬥�⮢ ���� '+Str(bsr)+' � '+dtoc(ddcr1)+' �� '+dtoc(ddcr2)
  Stroka = Stroka + 1
  @ prow()+1,0 say '���� '+Str(List)
  Stroka = Stroka + 1
  @ prow()+1,0 say repl('�',84)
  Stroka = Stroka + 1
  @ prow()+1,1 say'�N ���.�  ���  ���� ������������ ������              �    �����    �    �।��   �'
  Stroka = Stroka + 1
  @ prow()+1,0 say repl('�',84)
  Stroka = Stroka + 1
EndIf
retu

//////////////////////////////
proc prn_pld1

Select ADB
Go top
Ss_db = 0
S_rn = 0
Stroka = 1
Do While .NOT. Eof()
  skr=sk
  kklr=kkl
  S_kkl = 0
  ddkr = ddk
  Do While KKL = Kklr
    rnr=rn
    ddkr = ddk
    Do While RN = Rnr
      If KKL <> Kklr
        Exit
      EndIf
      S_rn = S_rn + Bs_s
      Skip
    EndDo
    sele kln
*    seek str(kklr,7)
    if !netseek('t1','kklr')
      nklr='��� ������ � ��ࠢ�筨� !'
    else
      nklr=nkl
    endif
    Select ADB
    if round(s_rn,2)#0.00
       @ prow()+1,0 say rnr pict '999999'
       @ prow(),8 say ddkr
       @ prow(),17 say str(kklr,7)+' '+nklr
       @ prow(),56 say S_rn pict '999999999.99'
       @ prow(),86 say skr pict '999'
       Stroka = Stroka + 1
       shap_1()
       S_kkl = S_kkl + S_rn
    endif
    S_rn = 0
//    Skip
  EndDo
    @ prow()+1,17 say '�⮣� �� �।�����         '+Str(S_kkl,12,2)
    Stroka = Stroka + 1
    shap_1()
    Ss_db = Ss_db + S_kkl
EndDo
@ prow()+1,17 say '����� �� ������              '+Str(Ss_db,12,2)
Stroka = Stroka + 1
shap_1()
retu

//////////////////////////////
proc prn_pld2

Select AKR
Go top
Ss_kr = 0
S_rn = 0
Stroka = 1
Do While .NOT. Eof()
  skr=sk
  kklr=kkl
  S_kkl = 0
  ddkr = ddk
  Do While KKL = Kklr
    rnr=rn
    ddkr = ddk
    Do While RN = Rnr
      If KKL <> Kklr
        Exit
      EndIf
      S_rn = S_rn + Bs_s
      Skip
    EndDo
    sele kln
    if !netseek('t1','kklr')
      nklr='��� ������ � ��ࠢ�筨� !'
    else
      nklr=nkl
    endif
    Select AKR
    if round(s_rn,2)#0.00
       @ prow()+1,0 say rnr pict '999999'
       @ prow(),8 say ddkr
       @ prow(),17 say str(kklr,7)+' '+nklr
       @ prow(),70 say S_rn pict '999999999.99'
       @ prow(),86 say skr pict '999'
       Stroka = Stroka + 1
       shap_1()
       S_kkl = S_kkl + S_rn
    endif
    S_rn = 0
  EndDo
    @ prow()+1,17 say '�⮣� �� �।�����         '+Str(S_kkl,12,2)
    Stroka = Stroka + 1
    shap_1()
    Ss_kr = Ss_kr + S_kkl
EndDo
@ prow()+1,17 say '����� �� �������             '+Str(Ss_kr,12,2)
Stroka = Stroka + 1
shap_1()
@ prow()+2,26 say '��室�騥 ���⪨ �� '+dtoc(ddcr2)+' '+Str(Ostn+Sum_dn-Sum_kn+Sum_d-Sum_k)
retu

**************
func vz204()
**************
clea
netuse('dokk')
set orde to tag t10
netuse('dknap')
netuse('dkkln')
netuse('kln')
crtt('vz204','f:kkl c:n(7) f:nkl c:c(40) f:smn c:n(12,2) f:sm c:n(12,2)')
sele 0
use vz204 excl
inde on str(kkl,7) tag t1
sele dkkln
go top
do while !eof()
   if bs#631001
      skip
      loop
   endif
   kklr=kkl
*if kklr=1400997
*wait
*endif
   smnr=0
   sele dokk
   set orde to tag t10
   smr=0
   if netseek('t10','kklr,204001')
*      do while kkl=kklr.and.bs_d=204001
      do while kkl=kklr
         if !((bs_d=204001.or.bs_d=644001).and.bs_k=631001)
            skip
            loop
         endif
         if bs_d=204001
            smr=smr+bs_s
         else
            smnr=smnr+bs_s
         endif
         sele dokk
         skip
      endd
   endif
   if smr#0.or.smnr#0
      sele vz204
      seek str(kklr,7)
      if !foun()
         nklr=getfield('t1','kklr','kln','nkl')
         appe blank
         repl kkl with kklr,;
              nkl with nklr
      endif
      repl sm with sm+smr,smn with smnr
   endif
   sele dkkln
   skip
endd
sele vz204
go top
rcr=recn()
do while .t.
   foot('F5','�����')
   sele vz204
   go rcr
   rcr=slcf('vz204',1,1,18,,"e:kkl h:'���' c:n(7) e:nkl h:'������������' c:c(40) e:smn h:'�㬬����' c:n(12,2)  e:sm h:'�㬬�' c:n(12,2)")
   if lastkey()=K_ESC
      exit
   endif
   go rcr
   smnr=smn
   smr=sm
   kklr=kkl
   nklr=nkl
   do case
      case lastkey()= -4
           vz204prn()
   endc
endd
nuse('vz204')
nuse()
retu .t.

****************
func vz204prn()
****************
sm_r=smnr+smr
*sm_r=smr
claprem=setcolor('gr+/b,n/bg')
aklr=2

waprem=wopen(9,20,13,70)
mlptr=0
wbox(1)
do while .t.
   @ 0,1 say '�㬬�    ' get sm_r
   @ 1,1 say '�-�� � ' get aklr pict '9'
   @ 2,1 prom 'LPT1'
   @ 2,col()+1 prom 'LPT2'
   @ 2,col()+1 prom 'LPT3'
   @ 2,col()+1 prom '����'
   read
   if lastkey()=K_ESC
      exit
   endif
   menu to mlptr
   if lastkey()=K_ESC
      exit
   endif
   exit
endd
wclose(waprem)
setcolor(claprem)

if mlptr#0
   set cons off
   do case
      case mlptr=1
           set prin to lpt1
      case mlptr=2
           set prin to lpt2
      case mlptr=3
           set prin to lpt3
      case mlptr=4
           set prin to vz204.txt
           mlptr=1
   endc
   set prin on
   if mlptr=1
      if empty(gcPrn)
         ??chr(27)+chr(80)+chr(15)
      else
         ??chr(27)+'E'+chr(27)+'&l4h25a0O'+chr(27)+'&k2S'+chr(27)+'(3R'+chr(27)+'(s0p18.00h0s1b4102T'+chr(27)
      endif
   else
      ??chr(27)+'E'+chr(27)+'&l4h25a0O'+chr(27)+'&k2S'+chr(27)+'(3R'+chr(27)+'(s0p18.00h0s1b4102T'+chr(27)
   endif
   do while aklr>0
      vzprn()
      aklr=aklr-1
   endd
   set prin off
   set prin to
   set cons on
endif
retu .t.
***************
func vzprn()
***************
?'                                  ��������'
?'                          �� ���i� ������ �����'
?''
?'�.�㬨                                                   '+dtous(gdTd)
?''
?''
?'"'+'�த�����'+'"'+': '+gcName_c
?'� �ᮡi ��४�� ������ �.�., 直� �i� �� �i��⠢i ������, � ���i�i'
?'��஭�'
?'"'+'���㯥��'+'"'+': '+nklr
?'� �ᮡi                                           , 直� �i� �� �i��⠢i'
?'                               � i��i ��஭�, �i���ᠫ� ����� ��⮪��'
?'�� �i����:'
?''
?'       1.'+'"'+'�த�����'+'"'+' � '+'"'+'���㯥��'+'"'+'�਩諨 �� ����� �� ���i�'
?'������ ����� �� ���� 㪠����� ��������, � 直� '+'"'+'�த�����'+'"'
?'i '+'"'+'���㯥��'+'"'+' ������ ��஭��� :'
?'       1.1. �� �������� ��i��i - �த��� '+'"'+'�த�����'+'"'+' ������ �।��஬'
?'� '+'"'+'���㯥��'+'"'+' ������ ��ভ����.'
?'       1.2. �� �������� ��i��i - �த��� '+'"'+'�த�����'+'"'+' ������ ��ভ����'
?'� '+'"'+'���㯥��'+'"'+' ������ �।��஬.'
?''
?''
?'����i� ������ �����, �i ����������, ��室�� i� ��饢�������'
?'�������i�: '+upper(numstr(sm_r))+' �����⥭�i���'
?''
?'       2. ����� ��⮪�� ���㯠� � ᨫ� � ������� ���� �i���ᠭ��'
?'��஭���'
?''
?''
?''
?'         '+'"'+'���������'+'"'+'                        '+'"'+'��������'+'"'
?'       '+subs(gcName_c,1,30)+space(18)+nklr
?'���� '+subs(gcAdr_c,1,30)+space(4)+getfield('t1','kklr','kln','adr')
?'���    '+str(gnKln_c,10)+space(24)+str(getfield('t1','kklr','kln','kkl1'),10)
?'�/�    '+gcScht_c+space(38)+getfield('t1','kklr','kln','ns1')
?'���    '+Right(gnBank_c,6)+space(43)+getfield('t1','kklr','kln','kb1')
?''
?''
?'             _____________�.�.�����   ________________'

eject
retu .t.
