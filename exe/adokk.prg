/***********************************************************
 * �����    : adokk.prg
 * �����    : 0.0
 * ����     :
 * ���      : 11/08/19
 * �������   :
 * �ਬ�砭��: ����� ��ࠡ�⠭ �⨫�⮩ CF ���ᨨ 2.02
 */

#include "common.ch"
#include "inkey.ch"
// adokk
LOCAL nSumRnd

store 0 to TtnSdor, TtnSdvr, bs_sfr, koefr, SmVzr, TtnKopr
store ctod('') to DocPrd
sele dokk
if (!netseek('t1', 'mnr,rndr,kklr'))
  go top
endif

rcDokkr=recn()
while (.t.)
  sele dokk
  set orde to tag t1
  fldnomr=1
  go rcDokkr
  if (int(bsr/1000)=301)
    foot('INS,DEL,F4,F8,F9', '��������,�������,���४��,���㣫���,���������')
  else
    foot('INS,DEL,F4,F9', '��������,�������,���४��,���������')
  endif
  if (przr=1)
    rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_d h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:getfield('t1','dokk->kop','operb','nop') h:'������' c:c(18) e:getfield('t1','dokk->skl','podr','npod') h:'���ࠧ�������' c:c(18)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0')
  else
    if (prFrmr=0)
      if (gnEnt#21)
        if (gnEnt=20)
          if (tzdocr=0)
            rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:getfield('t1','dokk->kop','operb','nop') h:'������' c:c(18) e:getfield('t1','dokk->skl','podr','npod') h:'���ࠧ�������' c:c(18)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
          else
            if (int(bsr/1000)=301.or.int(bsr/1000)=311)
              if (fieldpos('rnpar')=0)
                rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:nap h:'����' c:n(4) e:getfield('t1','dokk->nap','nap','nnap') h:'���ࠢ�����' c:c(15) e:getfield('t1','dokk->skl','podr','npod') h:'���ࠧ�������' c:c(16)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
              else
                rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:nap h:'����' c:n(4) e:getfield('t1','dokk->nap','nap','nnap') h:'���ࠢ�����' c:c(15) e:rnpar h:'P���⥫�' c:n(6)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
              endif

            else
              rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:getfield('t1','dokk->kop','operb','nop') h:'������' c:c(18) e:getfield('t1','dokk->skl','podr','npod') h:'���ࠧ�������' c:c(18)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
            endif

          endif

        else
          rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:getfield('t1','dokk->kop','operb','nop') h:'������' c:c(18) e:getfield('t1','dokk->skl','podr','npod') h:'���ࠧ�������' c:c(18)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
        endif

      else
        rcDokkr=slcf('dokk',,,,, "e:rn h:'N�஢.' c:n(6,0) e:ddk h:' ���     ' c:d(9) e:kop h:'���' c:n(4,0) e:bs_k h:'��' c:n(6,0) e:bs_s h:'�㬬�' c:n(10,2) e:getfield('t1','dokk->kop','operb','nop') h:'������' c:c(18) e:getfield('t1','dokk->skl','podr','npod') h:'����' c:c(13) e:kta h:'��' c:n(4)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
      endif

    else
      if (fieldpos('DocPrd')=0)
        rcDokkr=slcf('dokk',,,,, "e:rn h:'N��' c:n(6) e:kop h:'���' c:n(4) e:bs_k h:'��' c:n(6) e:bs_s h:'�㬬�' c:n(10,2) e:sk h:'���' c:n(3) e:DokkTtn h:'���' c:n(6) e:kta h:'��' c:n(4) e:ktas h:'���' c:n(4) e:getfield('t1','dokk->nap','nap','nnap') h:'���ࠢ�����' c:c(15)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
      else
        rcDokkr=slcf('dokk',,,,, "e:rn h:'N��' c:n(6) e:kop h:'���' c:n(4) e:bs_k h:'��' c:n(6) e:bs_s h:'�㬬�' c:n(10,2) e:sk h:'���' c:n(3) e:DokkTtn h:'���' c:n(6) e:kta h:'��' c:n(4) e:ktas h:'���' c:n(4) e:DocPrd h:'��ਮ� ���' c:d(10) e:getfield('t1','dokk->nap','nap','nnap') h:'���ࠢ�����' c:c(15)",,, 1, 'mn=doks->mn.and.rnd=doks->rnd', '!prc.and.!(bs_d)=0.or.bs_k=0',, '�஢����')
      endif

    endif

  endif

  if (lastkey()=K_ESC)
    exit
  endif

  sele dokk
  go rcDokkr
  rnr=rn
  kopr=kop
  dokkmskr=getfield('t1', 'kopr', 'operb', 'mask')
  bs_dr=bs_d
  bs_kr=bs_k
  bs_sr=bs_s
  bs_sfr=bs_s
  sklr=skl
  ndspstr=ndspst
  ndspstcr=ndspstc
  dndspstr=dndspst
  ddkr=ddk
  ktar=kta
  ktasr=ktas
  tmestor=tmesto
  skr=sk
  DokkTtnr=DokkTtn
  DokkSkr=DokkSk
  mnpr=mnp
  RmSkr=RmSk
  nndsr=nnds
  nopr=getfield('t1', 'kopr', 'operb', 'nop')
  napr=nap
  if (fieldpos('DocPrd')#0)
    DocPrdr=DocPrd
  else
    DocPrdr=0
  endif

  if (fieldpos('rnpar')#0)
    rnparr=rnpar
  else
    rnparr=0
  endif

  if (fieldpos('TVdNal')#0)
    TVdNalr=TVdNal
    VdNalr=VdNal
  else
    TVdNalr=0
    VdNalr=0
  endif

  if (TVdNalr#0)
    if (TVdNalr=1)
      nTVdNalr='�뤠����  '
    else
      nTVdNalr='����祭���'
    endif

    if (VdNalr#0)
      if (gdTd<ctod('01.03.2013'))
        nVdNalr=getfield('t1', 'TVdNalr,VdNalr', 'VdNal', 'nVdNal')
      else
        nVdNalr=getfield('t1', 'TVdNalr,VdNalr', 'VdNaln', 'nVdNal')
      endif

    else
      nVdNalr=''
    endif

  else
    nTVdNalr=''
    nVdNalr=''
  endif

  do case
  case (lastkey()=K_LEFT) // Left
    fldnomr=fldnomr-1
    if (fldnomr=0)
      fldnomr=1
    endif

  case (lastkey()=K_RIGHT)
    fldnomr=fldnomr+1
  case (lastkey()=K_INS.and.prdp().and.prrmbsr=0.and.NChekr=0.and.prFrmr#3)// ��������
    if (prFrmr=1 .and. int(bsr/1000)=301)
      sele dokk
      if (netseek('t1', 'mnr,rndr'))
        If !("U" $ type("nSumRnd")) .and. !EMPTY(nSumRnd)
        Else
          wmess('���쪮 ���� 祪', 3)
          loop
        EndIf
      endif

      go rcDokkr
    endif

    sSumD_r=asum(0, mnr, rndr)
    sSumK_r=asum(1, mnr, rndr)
    if (przr=0)
      smr=abs(ssdr)-abs(sSumD_r)
    else
      smr=abs(ssdr)-abs(sSumK_r)
    endif

    if (smr<=0.and.prFrmr#1)
      wmess('��墠⠥� ���⪠', 1)
      loop
    endif

    ADokkIns(0, kopr, nSumRnd)
    @ 1, 40 say ' ����� '+str(SumDr, 10, 2)+space(3)+' �।�� '+str(SumKr, 10, 2)
    sSumD_r=asum(0, mnr, rndr)
    sSumK_r=asum(1, mnr, rndr)
    @ 2, 40 say ' �����'+str(sSumD_r, 10, 2) + space(3)+' �।��'+str(sSumK_r, 10, 2)
    @ 3, 61 say '�㬬�  '+str(ssdr, 10, 2)
    if (prFrmr=0)
      if (przr=0)
        @ 4, 61 say '���⮪'+str(ssdr-sSumD_r, 10, 2)
      else
        @ 4, 61 say '���⮪'+str(ssdr-sSumK_r, 10, 2)
      endif

    endif

  case (lastkey()=K_DEL.and.prdp().and.prrmbsr=0.and.mn#0.and.NChekr=0)// �������
    kopr=0
    sele dokk
    DocMod('�', 1)
    mdall('�')
    msk(0, 0)
    store 0 to SumD_r, SumK_r
    bs_sr=bs_s
    if (bs_d=bsr)
      SumD_r=bs_sr
    endif

    if (bs_k=bsr)
      SumK_r=bs_sr
    endif

    sele dokz
    SumDr=SumD-SumD_r
    SumKr=SumK-SumK_r
    netrepl('SumD,SumK', { SumDr, SumKr })
    @ 1, 40 say ' ����� '+str(SumDr, 10, 2)+space(3)+' �।�� '+str(SumKr, 10, 2)
    if (prFrmr=1)
      // ���४�� DOKS
      sele doks
      netrepl('ssd', { ssd-bs_sr })
      ssdr=ssd
      // ���४�� ������ ���㬥��
      if (!empty(DocPrdr).and.DokkSkr#0.and.DokkTtnr#0)
        pathmr=gcPath_e+'g'+str(year(DocPrdr), 4)+'\m'+iif(month(DocPrdr)<10, '0'+str(month(DocPrdr), 1), str(month(DocPrdr), 2))+'\'
        sele cskl
        locate for sk=DokkSkr
        pathr=pathmr+alltrim(path)
        if (netfile('rs1', 1))
          netuse('rs1', 'rs1opl',, 1)
          if (fieldpos('sdo')#0)
            if (netseek('t1', 'DokkTtnr'))
              netrepl('sdo', { sdo-bs_sr })
              sdor=sdo
            endif

          endif

          nuse('rs1opl')
        endif

      endif

    endif

    sele dokk
    netdel()
    skip -1
    if (!(mn=mnr.and.rnd=rndr.and.kkl=kklr))
      netseek('t1', 'mnr,rndr,kklr')
    endif

    rcDokkr=recn()
    sSumD_r=asum(0, mnr, rndr)
    sSumK_r=asum(1, mnr, rndr)
    @ 2, 40 say '  �����'+str(sSumD_r, 10, 2) + space(3)+'  �।��'+str(sSumK_r, 10, 2)
    @ 3, 61 say '�㬬�  '+str(ssdr, 10, 2)
    //              if prFrmr=0
    if (przr=0)
      @ 4, 61 say '���⮪'+str(ssdr-sSumD_r, 10, 2)
    else
      @ 4, 61 say '���⮪'+str(ssdr-sSumK_r, 10, 2)
    endif


  //              endif

  case (lastkey()=K_F8) .and. (int(bsr/1000)=301)    // ���㣫����
    loop // �����誠

    sele dokk
    if (netseek('t1', 'mnr,rndr'))
      // ���� ����窨
      nSumRnd := ROUND(dokk->Bs_s,1) - Bs_s
      If nSumRnd < 0 //㬥��襭��
        sele operb
        locate for db = 949001 .and. kr = bsr
        //locate for db = bsr .and. kr = 949001
      elseif nSumRnd > 0 //㢥��祭��
        sele operb
        locate for db = bsr .and. kr = 719001
      Else // =0
         sele dokk
         loop
      EndIf

      If found()
        kopr=kop
        //nSumRnd := ABS(nSumRnd)
        @ 22, 0 say "��� " + str(kopr) +" "+str(nSumRnd)
        KEYBOARD chr(K_INS)+str(kopr) //+chr(K_ENTER)
      else
        nSumRnd:=NIL
        wmess('��� ��� ���㣫����', 2)
        loop
      EndIf

      sele dokk
      //Browse()
    else
      loop // ������� ��祣�
    endif

  case (lastkey()=K_F4)   // ���४��
    #ifdef __CLIP__
          if (NChekr#0)
            loop
          endif

    #endif
    sSumD_r=asum(0, mnr, rndr)
    sSumK_r=asum(1, mnr, rndr)
    ADokkIns(1)
    @ 1, 40 say ' ����� '+str(SumDr, 10, 2)+space(3)+' �।�� '+str(SumKr, 10, 2)
    sSumD_r=asum(0, mnr, rndr)
    sSumK_r=asum(1, mnr, rndr)
    @ 2, 40 say '  �����'+str(sSumD_r, 10, 2) + space(3)+'  �।��'+str(sSumK_r, 10, 2)
    @ 3, 61 say '�㬬�  '+str(ssdr, 10, 2)
    if (prFrmr=0)
      if (przr=0)
        @ 4, 61 say '���⮪'+str(ssdr-sSumD_r, 10, 2)
      else
        @ 4, 61 say '���⮪'+str(ssdr-sSumK_r, 10, 2)
      endif

    endif

  case (lastkey()=K_F5.and.int(bs_kr/1000)=361.and.mn#0.and.NChekr=0)// ��⮬��
                            //           #ifdef __CLIP__
                            //           #else
                            //              autoprov(1)
                            //              sele dokk
                            //              netseek('t1','mnr,rndr')
                            //              rcDokkr=recn()
                            //           #endif
  case (lastkey()=K_ENTER.and.DokkTtnr=0.and.int(bs_kr/1000)=361;
    .and.prFrmr=0)// �� ���
#ifdef __CLIP__
#else
                            //           adokkz()
#endif
  case (lastkey()=K_F9.and.int(bs_kr/1000)=361;
    .and.mn#0.and.gnEnt=20.and.(gnAdm=1.or.gnRnap=1))// ���������
    if (gnEntRm=0)
      if (RmSkr#0)
        wmess('�஢���� 㤠������� ᪫���', 2)
        loop
      endif

    else
      if (RmSkr#gnRmSk)
        wmess('�� ᢮� �஢����', 2)
        loop
      endif

    endif

    if (fieldpos('rnpar')#0)
      if (DokkTtnr=0)
        if (tzdocr#0)
          rzdprv()
        else
          wmess('��� ���ࠢ����� � ������', 2)
        endif

      else
        wmess('������ �� ���', 2)
      endif

    else
      wmess('��� ���� RNPAR', 2)
    endif

  case (lastkey()=K_ESC)  // ��室
    exit
  otherwise
    loop
  endcase

enddo

/***************** */
function ADokkIns(p1, nl_kopr, nSumRnd)
  /***************** */
  static kopr_store, sklr_store
  local lRetured:=.F.
  local nl_Tipr:=Tipr
  local nl_prZr:=prZr

  cldokk=setcolor('n/w,n/bg')
  wdokk=wopen(5, 2, 14, 78)
  wbox(1)
  if (p1=0)
    store 0 to bs_dr, bs_kr, kopr, ktar, ktasr, tmestor, skr, DokkTtnr, mnpr, DokkSkr, napr, TVdNalr, VdNalr
    store space(6) to dokkmskr
    store '' to nopr, fior
    store space(40) to ndspstr, ndspstcr
    store ctod('') to dndspst
    store ddcr to ddkr



    // ����祭�� ᫥� ����� �஢����
    sele dokk
    if (netseek('t1', 'mnr,rndr,kklr'))
      while (mn=mnr.and.rnd=rndr.and.kkl=kklr)
        rnr=rn
        skip
        if (eof())
          exit
        endif

      enddo

      rnr=rnr+1
    else
      rnr=1
    endif
    /////

    If !Empty(nSumRnd) // ���� �㬬� ���㣫����
      Tipr=0
      If nSumRnd < 0
        //prZr=1
      EndIf
    EndIf


    sum1r=Round(bs_sr, 2) // ���⮪
    sklr=83
    if (int(bsr/1000)=301.and.Tipr=1)// ��� ��� ����
      do case
      case (gnRmSk=0)
        bs_drr=301001
        sklr=83
      case (gnRmSk=3)
        bs_drr=301003
        sklr=83
      case (gnRmSk=4)
        bs_drr=301004
        sklr=83
      case (gnRmSk=5)
        bs_drr=301005
        sklr=83
      case (gnRmSk=6)
        bs_drr=301006
        sklr=83
      endcase

      bs_krr=361001
      sele operb
      locate for db=bs_drr.and.kr=bs_krr
      kopr=kop
      dokkmskr=mask
    else

      kopr=0
      If !empty(nl_kopr) // ��।��� ��� ��ࠬ���
        kopr=nl_kopr
      EndIf

      if (str(int(bsr/1000), 3)$'301;311')
        kopr=iif(!ISNIL(kopr_store), kopr_store, 0)
      endif

    endif

    if (str(gnEnt, 2)$' 6; 7;25;26')//
      sklr=10
    endif

    if (przr=0)
      bs_sr=Round(ssdr-sSumD_r, 2)
    else
      bs_sr=Round(ssdr-sSumK_r, 2)
    endif

    DokkTtn1r=0
  else
    kop1r=kopr
    skl1r=sklr
    sum1r=bs_sr
    kkl1r=kklr
    bs_d1r=bs_dr
    bs_k1r=bs_kr
    DokkTtn1r=DokkTtnr
    DokkSk1r=DokkSkr
  endif

  bs_sr:=Round(bs_sr, 2)

  if (prFrmr=0)
    if (przr=0)
      ost_r=ssdr-sSumD_r    // ���⮪
    else
      ost_r=ssdr-sSumK_r    // ���⮪
    endif

  endif

  sele dokk
  while (.t.)
    if (prdp().and.prrmbsr=0)
      if (.t.)
        @ 0, 1 say 'N �஢����   '+str(rnr, 6)
      else
        @ 0, 1 say 'N �஢����   ' get rnr pict '999999'
      endif
      outlog(3,__FILE__,__LINE__,prFrmr=0, kopr=0, int(bsr/1000))
      if (prFrmr=0 .or. kopr=0 .or. int(bsr/1000)=311)
        @ 1, 1 say '��� ����樨 ' get kopr pict '@K 9999' ;
        valid akop() //iif(empty(nSumRnd),akop(),.t.)
        read
        if (lastkey()=K_ESC)
          exit
        endif

        kopr_store:=kopr
      else
        akop()
        @ 1, 1 say '��� ����樨 '+' '+str(kopr, 4)
        @ 1, col()+1 say getfield('t1', 'kopr', 'operb', 'nop')
      endif

      sele dokk
      if (kopr=90.or.kopr=1689.or.kopr=174)
        @ 0, 27 say '�� ���' get ndspstr
        @ 1, 27 say '�� ���' get ndspstcr
        @ 2, 52 say '��� �� ���' get dndspstr
        read
        if (lastkey()=K_ESC)
          exit
        endif

      endif

      if (int(bs_dr/1000)=361.or.int(bs_kr/1000)=361)
        store 0 to ggr, mmr
        if (prFrmr=0)
          tzdocr=getfield('t1', 'kklr', 'kpl', 'tzdoc')
          if (tzdocr=0)
            @ 3, 1 say '�㯥ࢠ����' get ktasr pict '9999' valid aktas()
            @ 3, 18 say alltrim(getfield('t1', 'ktasr', 's_tag', 'fio'))
          else
            @ 3, 1 say '���ࠢ�����' get napr pict '9999' valid adoksnap('napr')
            @ 3, 18 say alltrim(getfield('t1', 'napr', 'nap', 'nnap'))
          endif

          if (napr=0)
            @ 4, 1 say '�����' get ktar pict '9999' valid chkas(ktar)
            @ 4, 18 say alltrim(getfield('t1', 'ktar', 's_tag', 'fio'))
          endif

        else
          if (prFrmr=1)
            @ 3, 1 say '���' get DokkTtnr pict '999999' valid aTtnF(wdokk, @lRetured)
            // @ 3,12 say
            // '������ N ��� ��-��
            //  0 - ��� �롮� �� ᯨ᪠ �� ������'
          // '������ N ��� ���� �� ��२��� ��� ����祭�� ᯨ᪠ ��� ��-��'
          else
            @ 3, 1 say '���'+' '+str(DokkSkr, 3)
            @ 3, col()+1 say '���'+' '+str(DokkTtnr, 6)
          endif

        endif

        keyboard ''
        read
        if (lastkey()=K_ESC)
          exit
        endif

        if (prFrmr=1.and.DokkTtnr=0)
          wmess('�� ������� ��� ��� �� ��࠭�� �� ᯨ᪠', 3)
          exit
        endif

        if (prFrmr=1)
          if (ggr=0)
            DocPrdr=gdTd
          else
            DocPrdr=ctod('01.'+iif(mmr<10, '0'+str(mmr, 1), str(mmr, 2))+'.'+str(ggr, 4))
          endif

          @ 3, 25 say '��ਮ�'+' '+dtoc(DocPrdr)
          @ 4, 1 say '�㬬� ��� '+str(TtnSdvr, 12, 2)+' ��� '+str(TtnSdor, 12, 2)
          if (.f.)        //gnEnt=21
            SmVzr=0
            if (select('ksvz2')#0)
              sele ksvz2
              if (recc()#0)
                go top
                while (!eof())
                  kfr=kf
                  zenr=Round(zen, 2)
                  SmVzr=SmVzr+Round(zenr*kfr, 2)
                  sele ksvz2
                  skip
                enddo

              endif

            endif

            if (SmVzr#0)
              SmVzr=Round(SmVzr*1.2, 2)
              @ 4, col()+1 say ' �� '+str(SmVzr, 10, 2)+'('+str(TtnSdvr-TtnSdor-SmVzr, 10, 2)+')'
            endif

          endif

        endif

      endif

      if (fieldpos('TVdNal')#0)
        db_rr=getfield('t1', 'kopr', 'operb', 'db')
        kr_rr=getfield('t1', 'kopr', 'operb', 'kr')
        if (db_rr=641002.or.kr_rr=641002)
          if (db_rr=641002)
            if (kr_rr=641001.or.int(kr_rr/1000)=311.or.kr_rr=315001)
              TVdNalr=0
            else
              TVdNalr=1
              nTVdNalr='����祭���'
            endif

          else
            TVdNalr=2
            nTVdNalr='�뤠����  '
          endif

          @ 3, 1 say '�����'+' '+nTVdNalr
          if (TVdNalr#0)
            @ 4, 1 say '�����' get VdNalr pict '99' valid aVdNal(wdokk)
            read
            if (lastkey()=K_ESC)
              exit
            endif

            if (VdNalr#0)
              if (gdTd<ctod('01.03.2013'))
                nVdNalr=getfield('t1', 'TVdNalr,VdNalr', 'VdNal', 'nVdNal')
              else
                nVdNalr=getfield('t1', 'TVdNalr,VdNalr', 'VdNaln', 'nVdNal')
              endif

              @ 4, 10 say nVdNalr
            endif

          else
            VdNalr=0
          endif

        endif

      endif

      if (prFrmr=0)
        @ 5, 1 say '�㬬�        ' get bs_sr pict '9999999.99' valid chkbs_s()
        if (p1=0)
          if (bom(gdTd)>=ctod('01.03.2011'))
            if (gnAdm=1.or.gnKto=331.or.gnKto=847.or.gnKto=882)
              nndsr=0
              @ 5, col()+1 get nndsr pict '9999999999' valid chknn()
            else
              @ 5, col()+1 say str(nndsr, 10)
            endif

          endif

        else
          if (bom(gdTd)>=ctod('01.03.2011'))
            @ 5, col()+1 say str(nndsr, 10)
          endif

        endif

        @ 6, 1 say '���ࠧ�������' get sklr pict '9999' VALID apodr()
        @ 6, col()+1 say getfield('t1', 'sklr', 'podr', 'npod')
        read
        if (lastkey()=K_ESC)
          exit
        endif

        sklr_store=sklr
      else
        if (prFrmr#3)
          if (prFrmr=1.and.DokkTtnr#0)

            @ 3, 1 say '���'+' '+str(DokkSkr, 6)
            @ 3, col()+1 say '���'+' '+str(DokkTtnr, 6)
            @ 5, 1 say '�㬬�        '                           ;
             get bs_sr pict '999999999.99'                       ;
             valid iif(lRetured, bs_sr<0, .t.) .and. TtnBs_S()
            read
            if (lastkey()=K_ESC)
              exit
            endif

          else
            @ 5, 1 say '�㬬�        ' get bs_sr pict '999999999.99'
            read
            if (lastkey()=K_ESC)
              exit
            endif

          endif

        else
          @ 5, 1 say '�㬬�        '+str(bs_sr, 12, 2)
        endif

        @ 6, 1 say '���ࠧ�������'+' '+str(sklr, 4)
        @ 6, col()+1 say getfield('t1', 'sklr', 'podr', 'npod')
      endif

    else
      @ 0, 1 say 'N �஢����   '+' '+str(rnr, 6)
      @ 1, 1 say '��� ����樨 '+' '+str(kopr, 4)
      @ 1, col()+1 say getfield('t1', 'kopr', 'operb', 'nop')
      if (kopr=90.or.kopr=1689.or.kopr=174)
        @ 0, 27 say '�� ���'+' '+ndspstr
        @ 1, 27 say '�� ���'+' '+ndspstcr
        @ 2, 52 say '��� �� ���'+' '+dtoc(dndspstr)
      endif

      if (napr=0)
        @ 3, 1 say '�����'+' '+str(skr, 3)+' '+'�����'+' '+str(ktar, 4)+' '+'�㯥ࢠ����'+' '+str(ktasr, 4)
      else
        @ 3, 1 say '���ࠢ�����'+' '+str(napr, 4)+' '+nnapr
      endif

      @ 5, 1 say '�㬬�        '+' '+str(bs_sr, 10, 2)
      @ 6, 1 say '���ࠧ�������'+' '+str(sklr, 4)
      @ 6, col()+1 say getfield('t1', 'sklr', 'podr', 'npod')
    endif

    @ 7, 1 prom '��୮'
    @ 7, col()+1 prom '�� ��୮'
    menu to mdokk
    if (lastkey()=K_ESC)
      exit
    endif

    if (mdokk=1.and.prdp().and.prrmbsr=0)
      sele dokk
      if (p1=0)           // ��������
        if (netseek('t1', 'mnr,rndr,kklr'))
          while (mn=mnr.and.rnd=rndr.and.kkl=kklr)
            rnr:=rn
            skip
          enddo

          rnr:=rnr+1
        else
          rnr:=1
        endif

        sele dokk
        netadd(1)
        rcDokkr=recn()
        netrepl('rn,nplp,ddk,bs_d,bs_k,bs_s,skl,kop,kg,tdc,ddc',                    ;
                 'rnr,nplpr,ddcr,bs_dr,bs_kr,bs_sr,sklr,kopr,gnKto,time(),date()', 1 ;
              )
        if (gnEntrm=1)
          netrepl('RmSk', { gnRmSk }, 1)
        endif

        netrepl('ndspst,ndspstc,dndspst', { ndspstr, ndspstcr, dndspstr }, 1)
        netrepl('dokkmsk', { dokkmskr }, 1)
        netrepl('mn,rnd,kkl', { mnr, rndr, kklr }, 1)
        netrepl('bs_o,ddo,tip', { bsr, ddcr, Tipr }, 1)
        netrepl('dtmod,tmmod', { date(), time() }, 1)
        netrepl('DokkSk', { DokkSkr }, 1)
        netrepl('nap', { napr }, 1)
        if (fieldpos('ktapl')#0)
          netrepl('ktapl', { ktaplr }, 1)
        endif

        if (fieldpos('TVdNal')#0)
          netrepl('TVdNal,VdNal', { TVdNalr, VdNalr }, 1)
        endif

        if (fieldpos('nnds')#0)
          netrepl('nnds', { nndsr }, 1)
        endif

        if (fieldpos('DocPrd')#0)
          netrepl('DocPrd', { DocPrdr }, 1)
        endif

        if (fieldpos('koef')#0)
          netrepl('koef', { koefr }, 1)
        endif

        netrepl('tipo,prz,kta,ktas,sk,DokkTtn,nkkl', { tipor, przr, ktar, ktasr, skr, DokkTtnr, nkklr })
        if (!empty(DocPrdr).and.DokkSkr#0.and.DokkTtnr#0)
          pathmr=gcPath_e+'g'+str(year(DocPrdr), 4)+'\m'+iif(month(DocPrdr)<10, '0'+str(month(DocPrdr), 1), str(month(DocPrdr), 2))+'\'
          sele cskl
          locate for sk=DokkSkr
          pathr=pathmr+alltrim(path)
          if (netfile('rs1', 1))
            netuse('rs1', 'rs1opl',, 1)
            if (fieldpos('sdo')#0)
              if (netseek('t1', 'DokkTtnr'))
                netrepl('sdo', { sdo-bs_sfr+bs_sr })
                sdor=sdo
              endif

            endif

            nuse('rs1opl')
          endif

        endif

        sele dokk
        DocMod('���', 1)
        mdall('���')
        if (bs_dr=bsr.or.bs_kr=bsr)
          sele dokz
          if (netseek('t1', 'bsr,ddcr'))
            if (bs_dr=bsr.or.bs_kr=bsr)
              if (bs_dr=bsr)
                netrepl('SumD', { SumD+bs_sr })
              endif

              if (bs_kr=bsr)
                netrepl('SumK', { SumK+bs_sr })
              endif

              SumDr=SumD
              SumKr=SumK
            endif

            if (prFrmr=1)
              // ���४�� DOKS
              sele doks
              netrepl('ssd', { ssd+bs_sr })
              if (nkkl=0)
                netrepl('nkkl', { nkklr })
              endif

              ssdr=ssd
            endif

            sele dokk
            msk(1, 0)
          endif

        endif

      else                  // ���४��
        sele dokk
        dokknndsr=nnds
        msk(0, 0)
        adokk1:={}
        getrec('adokk1')
        netrepl('kg', { gnKto })
        netrepl('rn,nplp,ddk,bs_d,bs_k,bs_s,skl,kop,kta,ktas,sk,DokkTtn',                            ;
                 { rnr, nplpr, ddkr, bs_dr, bs_kr, bs_sr, sklr, kopr, ktar, ktasr, skr, DokkTtnr }, 1 ;
              )
        netrepl('dtmod,tmmod', { date(), time() }, 1)
        netrepl('tip,tipo,dokkmsk', { Tipr, tipor, dokkmskr }, 1)
        netrepl('ndspst,ndspstc', { ndspstr, ndspstcr }, 1)
        netrepl('DokkSk', { DokkSkr }, 1)
        if (fieldpos('nap')#0)
          netrepl('nap', { napr }, 1)
        endif

        if (fieldpos('ktapl')#0)
          netrepl('ktapl', { ktaplr }, 1)
        endif

        if (fieldpos('TVdNal')#0)
          netrepl('TVdNal,VdNal', { TVdNalr, VdNalr }, 1)
        endif

        if (fieldpos('nnds')#0)
          netrepl('nnds', { dokknndsr }, 1)
        endif

        if (fieldpos('DocPrd')#0)
          netrepl('DocPrd', { DocPrdr }, 1)
        endif

        if (fieldpos('koef')#0)
          netrepl('koef', { koefr }, 1)
        endif

        if (!empty(DocPrdr).and.DokkSkr#0.and.DokkTtnr#0)
          pathmr=gcPath_e+'g'+str(year(DocPrdr), 4)+'\m'+iif(month(DocPrdr)<10, '0'+str(month(DocPrdr), 1), str(month(DocPrdr), 2))+'\'
          sele cskl
          locate for sk=DokkSkr
          pathr=pathmr+alltrim(path)
          if (netfile('rs1', 1))
            netuse('rs1', 'rs1opl',, 1)
            if (fieldpos('sdo')#0)
              if (netseek('t1', 'DokkTtnr'))
                netrepl('sdo', { sdo-bs_sfr+bs_sr })
                sdor=sdo
              endif

            endif

            nuse('rs1opl')
          endif

        endif

        sele dokk
        netrepl('dndspst', { dndspstr })
        DocMod('kopp', 1)
        msk(1, 0)
        adokk2:={}
        getrec('adokk2')
        lfldr=''
        prmdcorr=0
        for iput=1 to len(adokk1)
          if (!(field(iput)='BS_D'        ;
                  .or.field(iput)='BS_K'    ;
                  .or.field(iput)='BS_S'    ;
                  .or.field(iput)='DokkTtn' ;
                  .or.field(iput)='DokkSk'  ;
                  .or.field(iput)='KOP'     ;
                  .or.field(iput)='KKL'     ;
                  .or.field(iput)='KTA'     ;
                  .or.field(iput)='NKKL'    ;
                  .or.field(iput)='KTAS'    ;
               )                             ;
            )
            loop
          endif

          if (adokk1[ iput ]#adokk2[ iput ])
            prmdcorr=1
            lfldr=field(iput)
            exit
          endif

        next

        if (prmdcorr=1)
          if (empty(lfldr))
            mdall('����')
          else
            mdall(lfldr)
          endif

        endif

        sele dokz           // ���४�� DOKZ
        if (netseek('t1', 'bsr,ddcr'))
          if (bs_d1r=bsr)
            netrepl('SumD', { SumD-sum1r }, 1)
          else
            netrepl('SumK', { SumK-sum1r }, 1)
          endif

          if (bs_dr=bsr)
            netrepl('SumD', { SumD+bs_sr }, 1)
          else
            netrepl('SumK', { SumK+bs_sr }, 1)
          endif

          SumDr=SumD
          SumKr=SumK
          sele doks
          netrepl('ssd', { ssd-sum1r+bs_sr })
          if (nkkl=0)
            netrepl('nkkl', { nkklr })
          endif

          ssdr=ssd
        endif

      endif

      exit
    endif

  enddo

  rprZr=nl_prZr
  Tipr=nl_Tipr

  wclose(wdokk)
  setcolor(cldokk)
  return (.t.)

/************ */
function akop()
  /************ */
  sele operb
  if (kopr#0)
    if (!netseek('t1', 'kopr'))
      kopr=0
    endif

  endif

  if (kopr=0)
    wselect(0)
    go top
    while (.t.)
      if (przr=1)
        kopr=slcf('operb',,,,, "e:kop h:'���' c:n(4) e:nop h:'������������' c:c(27) e:db h:' ��' c:n(6) e:kr h:' �� ' c:n(6) e:mask h:'��᪠' c:c(6)", 'kop',,,, 'kr=bsr')
      else
        kopr=slcf('operb',,,,, "e:kop h:'���' c:n(4) e:nop h:'������������' c:c(27) e:db h:' ��' c:n(6) e:kr h:' �� ' c:n(6) e:mask h:'��᪠' c:c(6)", 'kop',,,, 'db=bsr')
      endif

      if (lastkey()=K_ESC)
        kopr=0
        wselect(wdokk)
        return (.f.)
      endif

      if (lastkey()=K_ENTER)
        exit
      endif

    enddo

    wselect(wdokk)
  endif

  sele operb
  if (netseek('t1', 'kopr'))
    nopr=nop
    bs_dr=db
    bs_kr=kr
    if (przr=1)
      if (bs_kr#bsr)
        return (.f.)
      endif

    else
      if (bs_dr#bsr)
        return (.f.)
      endif

    endif

    dokkmskr=mask
  else
    return (.f.)
  endif

  @ 1, 20 say nopr
  return (.t.)

/************** */
function apodr()
  /************** */
  sele podr
  if (sklr#0)
    if (!netseek('t1', 'sklr'))
      sklr=0
    endif

  endif

  go top
  if (sklr=0)
    wselect(0)
    sklr=slcf('podr', 1,, 18,, "e:pod h:'����' c:n(4) e:npod h:'������������' c:c(20)", 'pod',,,, 'pod<100')
    wselect(wdokk)
    if (lastkey()=K_ESC)
      //      sklr=0
    //      retu .f.
    endif

  endif

  if (netseek('t1', 'sklr'))
    fior=npod
    @ 6, 20 say fior
    return (.t.)
  else
    return (.f.)
  endif

  return (.t.)

/********** */
function akta()
  /********** */
  sele s_tag
  set orde to tag t1
  if (ktar#0)
    if (!netseek('t1', 'ktar'))
      ktar=0
    else
      if (ktasr#0)
        if (ktas#ktasr)
          ktar=0
        endif

      endif

    endif

    if (ent#gnEnt)
      ktar=0
    endif

  endif

  if (ktar=0)
    do case
    case (bsr=301001)
      ktaforr='iif(ktasr#0,ktas=ktasr,agsk<300).and.ent=gnEnt'
    case (bsr=301003)
      ktaforr='iif(ktasr#0,ktas=ktasr,agsk=300).and.ent=gnEnt'
    case (bsr=301004)
      if (gnEnt=20)
        ktaforr='iif(ktasr#0,ktas=ktasr,agsk=400).and.ent=gnEnt'
      endif

      if (gnEnt=21)
        ktaforr='iif(ktasr#0,ktas=ktasr,agsk=700).and.ent=gnEnt'
      endif

    case (bsr=301005)
      ktaforr='iif(ktasr#0,ktas=ktasr,agsk=500).and.ent=gnEnt'
    case (bsr=301006)
      ktaforr='iif(ktasr#0,ktas=ktasr,agsk=600).and.ent=gnEnt'
    otherwise
      cRmSkr=getfield('t1', 'kklr', 'kpl', 'cRmSk')
      ktaforr='.t.'
      do case
      case (subs(cRmSkr, 3, 1)='1')
        ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk=500)'
      case (subs(cRmSkr, 4, 1)='1')
        if (gnEnt=20)
          ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk=400)'
        endif

        if (gnEnt=21)
          ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk=700)'
        endif

      case (subs(cRmSkr, 5, 1)='1')
        ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk=500)'
      case (subs(cRmSkr, 6, 1)='1')
        ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk=600)'
      otherwise
        ktaforr=ktaforr+'.and.iif(ktasr#0,ktas=ktasr,agsk<300)'
      endcase

    endcase

    wselect(0)
    set orde to tag t3
    go top
    ktar=slcf('s_tag', 1,, 18,, "e:kod h:'���' c:n(4) e:fio h:'���' c:c(15) e:ktas h:'KTAS' c:n(4)", 'kod',,,, ktaforr,, '������')
    wselect(wdokk)
  endif

  @ 2, 51 say alltrim(getfield('t1', 'ktar', 's_tag', 'fio'))
  return (.t.)

/********** */
function aktas()
  /********** */
  sele s_tag
  set orde to tag t1
  if (ktasr#0)
    if (!netseek('t1', 'ktasr'))
      ktasr=0
    else
      if (kod#ktas)
        ktasr=0
      endif

    endif

    if (ent#gnEnt)
      ktasr=0
    endif

  endif

  if (ktasr=0)
    do case
    case (bsr=301001)
      ktaforr='kod=ktas.and.ent=gnEnt.and.agsk<300'
    case (bsr=301003)
      ktaforr='kod=ktas.and.ent=gnEnt.and.agsk=300'
    case (bsr=301004)
      if (gnEnt=20)
        ktaforr='kod=ktas.and.ent=gnEnt.and.agsk=400'
      endif

      if (gnEnt=21)
        ktaforr='kod=ktas.and.ent=gnEnt.and.agsk=700'
      endif

    case (bsr=301005)
      ktaforr='kod=ktas.and.ent=gnEnt.and.agsk=500'
    case (bsr=301006)
      ktaforr='kod=ktas.and.ent=gnEnt.and.agsk=600'
    otherwise
      cRmSkr=getfield('t1', 'kklr', 'kpl', 'cRmSk')
      ktaforr='.t.'
      do case
      case (subs(cRmSkr, 3, 1)='1')
        ktaforr=ktaforr+'.and.kod=ktas.and.agsk=300'
      case (subs(cRmSkr, 4, 1)='1')
        if (gnEnt=20)
          ktaforr=ktaforr+'.and.kod=ktas.and.agsk=400'
        endif

        if (gnEnt=21)
          ktaforr=ktaforr+'.and.kod=ktas.and.agsk=700'
        endif

      case (subs(cRmSkr, 5, 1)='1')
        ktaforr=ktaforr+'.and.kod=ktas.and.agsk=500'
      case (subs(cRmSkr, 6, 1)='1')
        ktaforr=ktaforr+'.and.kod=ktas.and.agsk=600'
      otherwise
        ktaforr=ktaforr+'.and.kod=ktas.and.agsk<300'
      endcase

    endcase

    wselect(0)
    set orde to tag t3
    go top
    ktasr=slcf('s_tag', 1,, 18,, "e:kod h:'���' c:n(4) e:fio h:'���' c:c(15) e:ktas h:'KTAS' c:n(4)", 'kod',,,, ktaforr,, '�㯥ࢠ�����')
    if (lastkey()=K_ESC)
      ktasr=0
    endif

    wselect(wdokk)
  endif

  @ 3, 18 say alltrim(getfield('t1', 'ktasr', 's_tag', 'fio'))
  return (.t.)

/*********************** */
function asum(p1, p2, p3, p4)
  // p1 0 - db; 1 - kr
  // p2 mn
  // p3 rnd
  // p4 kkl
  /************************ */
  als_rr=alias()
  if (!empty(als_rr))
    rc_rr=recn()
    ord_rr=indexord()
  endif

  if (p1=0)
    cbsr='bs_d'
  else
    cbsr='bs_k'
  endif

  mn_r=p2
  if (p3=nil)
    rnd_r=0
  else
    rnd_r=p3
  endif

  if (p4=nil)
    kkl_r=0
  else
    kkl_r=p4
  endif

  sele dokk
  rc_rrr=recn()
  ord_rrr=indexord()
  set orde to tag t1
  prs_r=0
  do case
  case (mn_r#0.and.rnd_r=0.and.kkl_r=0)
    netseek('t1', 'mn_r')
    prs_r=1
  case (mn_r#0.and.rnd_r#0.and.kkl_r=0)
    netseek('t1', 'mn_r,rnd_r')
    prs_r=2
  case (mn_r#0.and.rnd_r#0.and.kkl_r#0)
    netseek('t1', 'mn_r,rnd_r,kkl_r')
    prs_r=3
  otherwise
    sele dokk
    set orde to ord_rrr
    go rc_rrr
    if (!empty(als_rr))
      if (lower(als_rr)#'dokk')
        sele (als_rr)
        set orde to ord_rr
        go rc_rr
      endif

    endif

    return (0)
  endcase

  if (foun())
    sm_r=0
    do case
    case (prs_r=1)
      sum bs_s to sm_r while mn=mn_r for &cbsr=bsr.and.!prc.and.!(bs_d=0.or.bs_k=0)
    case (prs_r=2)
      sum bs_s to sm_r while mn=mn_r.and.rnd=rnd_r for &cbsr=bsr.and.!prc.and.!(bs_d=0.or.bs_k=0)
    case (prs_r=3)
      sum bs_s to sm_r while mn=mn_r.and.rnd=rnd_r.and.kkl=kkl_r for &cbsr=bsr.and.!prc.and.!(bs_d=0.or.bs_k=0)
    endcase

    sele dokk
    set orde to ord_rrr
    go rc_rrr
    if (!empty(als_rr))
      if (lower(als_rr)#'dokk')
        sele (als_rr)
        set orde to ord_rr
        go rc_rr
      endif

    endif

    return (sm_r)
  else
    sele dokk
    set orde to ord_rrr
    go rc_rrr
    if (!empty(als_rr))
      if (lower(als_rr)#'dokk')
        sele (als_rr)
        set orde to ord_rr
        go rc_rr
      endif

    endif

    return (0)
  endif

RETURN (NIL)

/**************** */
function chkbs_s()
  /**************** */
  if (ssdr>=0)
    if (Round(ost_r+sum1r-bs_sr, 2)<0)
      wmess('��墠⠥� ���⪠ ost_r+sum1r-bs_sr' ;
             +' '+ltrim(str(ost_r, 12, 2))     ;
             +' '+ltrim(str(sum1r, 12, 2))     ;
             +' '+ltrim(str(bs_sr, 12, 2)), 2  ;
          )
      bs_sr=sum1r
      return (.f.)
    endif

  else
    if (Round(abs(ost_r)+abs(sum1r)-abs(bs_sr), 2)<0)
      wmess('��墠⠥� ���⪠', 1)
      bs_sr=sum1r
      return (.f.)
    endif

  endif

  return (.t.)

/************ */
function ask(p1)
  /************ */
  // p1 - ���ਯ�� ����
  if (skr#0)
    sele cskl
    if (!netseek('t1', 'skr', 'cskl'))
      skr=0
      return (.f.)
    else
      if (ent#gnEnt)
        skr=0
        return (.f.)
      endif

      if (rasc=0)
        skr=0
        return (.f.)
      endif

      if (gnEntrm=0)

      else
        if (rm#1)
          skr=0
          return (.f.)
        endif

      endif

    endif

  endif

  if (skr=0)
    wselect(0)
    sele cskl
    go top
    rccsklr=recn()
    while (.t.)
      sele cskl
      go rccsklr
      rccsklr=slcf('cskl',,,,, "e:sk h:'�����' c:n(3) e:nskl h:'������������' c:c(20)",,,,, 'ent=gnEnt.and.rasc=1',, '������')
      if (lastkey()=K_ESC)
        skr=0
        exit
      endif

      if (lastkey()=K_ENTER)
        go rccsklr
        skr=sk
        DokkSkr=sk
        exit
      endif

    enddo

    wselect(p1)
  endif

  return (.t.)

/************ */
function aktaa(p1)
  /************ */
  if (DokkTtnr=0)
    if (ktar#0)
      sele s_tag
      if (!netseek('t1', 'ktar'))
        ktar=0
      else
        if (ent#gnEnt)
          ktar=0
        else
          if (skr#0)
            if (agsk#skr)
              ktar=0
            endif

          endif

        endif

      endif

    endif

    if (ktar=0)
      wselect(0)
      if (skr#0)
        ktaflt_r='.t..and.agsk=skr.and.ent=gnEnt'
      else
        ktaflt_r='.t..and.ent=gnEnt'
      endif

      ktafltr=ktaflt_r
      sele s_tag
      set orde to tag t2
      go top
      rcktar=recn()
      //      foot('F3','������')
      while (.t.)
        sele s_tag
        go rcktar
        rcktar=slcf('s_tag',,,,, "e:kod h:'����' c:n(4) e:fio h:'���' c:c(15) e:ktas h:'���C' c:n(4)",,,,, ktafltr,, '������')
        if (lastkey()=K_ESC)
          ktar=0
          ktasr=0
          exit
        endif

        sele s_tag
        go rcktar
        ktar=kod
        ktasr=ktas
        do case
        case (lastkey()=K_ENTER)
          exit
        case (lastkey()=-2)
        endcase

      enddo

      wselect(p1)
    endif

  endif

  return (.t.)

/************ */
function aktass(p1)
  /************ */
  if (DokkTtnr=0)
    if (ktar=0)
      if (ktasr#0)
        sele s_tag
        if (!netseek('t1', 'ktasr'))
          ktasr=0
        else
          if (ent#gnEnt)
            ktasr=0
          else
            if (skr#0)
              if (agsk#skr)
                ktasr=0
              endif

            endif

          endif

        endif

      endif

      if (ktasr=0)
        wselect(0)
        if (skr#0)
          ktasflt_r='.t..and.agsk=skr.and.ent=gnEnt.and.kod=ktas'
        else
          ktaflt_r='.t..and.ent=gnEnt.and.kod=ktas'
        endif

        ktasfltr=ktaflt_r
        sele s_tag
        set orde to tag t1
        go top
        rcktasr=recn()
        //         foot('F3','������')
        while (.t.)
          sele s_tag
          go rcktasr
          rcktasr=slcf('s_tag',,,,, "e:ktas h:'����' c:n(4) e:fio h:'���' c:c(15) e:ktas h:'���C' c:n(4)",,,,, ktasfltr,, '�㯥ࢠ�����')
          if (lastkey()=K_ESC)
            ktasr=0
            exit
          endif

          sele s_tag
          go rcktasr
          ktasr=ktas
          do case
          case (lastkey()=K_ENTER)
            exit
          case (lastkey()=-2)
          endcase

        enddo

        wselect(p1)
      endif

    endif

  endif

  return (.t.)

/***************** */
function aTtnF(p1, lRetured)
  /***************** */
  local lRet:=.T., nKeyReadExit, aListSk, nLastSkFound

  if (LASTKEY()=K_ENTER)  // ����� ��室 � �� CTR+W, �� skr ������ ���� ��।����
    do case
    case (gnEnt=20)
      do case
      case (int(bsr/1000)=311)
        aListSk:={ 228, 400, 300, 500, 600 }
        skr=228
      case (bsr=301001.or.int(bsr/1000)=361)
        aListSk:={ 228 }
        skr=228
      case (bsr=301003)
        aListSk:={ 300 }
        skr=300
      case (bsr=301004)
        aListSk:={ 400 }
        skr=400
      case (bsr=301005)
        aListSk:={ 500 }
        skr=500
      case (bsr=301006)
        aListSk:={ 600 }
        skr=600
      endcase

    case (gnEnt=21)
      aListSk:={ 232, 700 }
      do case
      case (int(bsr/1000)=311)
        aListSk:={ 232, 700 }
        skr=232
      case (bsr=301001.or.int(bsr/1000)=361)
        aListSk:={ 232, 700 }
        skr=232
      case (bsr=301004)
        aListSk:={ 700 }
        skr=700
      endcase

    endcase

  else
    aListSk:={ skr }
  endif

  if (skr=0)
    wmess('�� 㤠���� ��।����� ᪫��', 2)
    return (.f.)
  endif

  // �஢�ઠ �� ����稥 ᪫��
  lRet:=.F.
  for i:=1 to LEN(aListSk)
    skr:=aListSk[ i ]
    DirDr=alltrim(getfield('t1', 'skr', 'cskl', 'path'))
    pathr=gcPath_d+DirDr
    if (netfile('rs1', 1))
      lRet:=.T.
      exit
    endif

  next

  if (!lRet)
    wmess('��� ᪫��� '+pathr, 2)
    return (.f.)
  endif

  if (kklr=20034)
    dEndSeek=gdTd
  else
    dEndSeek=STOD('20060901')
  endif

  //����� ���
  if (DokkTtnr#0)
    dirdr=''

    nLastSkFound:=0
    while (.T.)
      for i:=1 to LEN(aListSk)
        skr=aListSk[ i ]
        dtSkTtn:=Seek_SkTtn(skr, DokkTtnr, dEndSeek)
        if (!EMPTY(dtSkTtn))
          sele rs1
          if (kpl#kklr)   // ������ �� ᮢ���, �த�����
            loop
          else
            nLastSkFound:=skr
          endif

          exit
        endif

      next i

      if (EMPTY(dtSkTtn) .and. !EMPTY(nLastSkFound))
        aListSk:={ nLastSkFound }
      else
        exit
      endif

    enddo

    // ��� ��᫥����� ���⮢
    DokkSkr=skr
    ggr:=Year(dtSkTtn); mmr:=Month(dtSkTtn)

    // ���뫨 �� ��室�, �.�. ��諨 ���
    if (select('rs1')=0)
      wmess('��� '+str(DokkTtnr, 6)+' ��� � ᪫��� '+str(skr, 3), 2)
      return (.f.)
    endif

    sele rs1
    if (!netseek('t1', 'DokkTtnr'))
      wmess('��� '+str(DokkTtnr, 6)+' ��� � ᪫��� '+str(skr, 3), 2)
      nuse('rs1')
      nuse('rs3')
      DokkTtnr=0
      KEYBOARD allTRIM(str(DokkTtnr))+chr(K_ENTER)
      return (.f.)
    else                    // ��諨 (��ன 横� �室�)
      if (LASTKEY()=K_CTRL_W)
        rs1->(ReadMem4Dokk())
        nuse('rs1')
        nuse('rs3')
        return (.t.)
      endif

    endif

    lRet:=.T.
    if (gnEnt=20 .and. kklr#20034)
      use (gcPath_db + 'accord_deb') ALIAS skdoc NEW SHARED READONLY
      ordsetfocus('t1')
      if (dbseek(str(kklr, 7)))
        // ���� ��� � ��-��
        locate for DokkTtnr = ttn while kpl = kklr
        if (!found())
          wmess('��� '+str(DokkTtnr, 6)+' ��� � "�����થ"', 2)
          DokkTtnr=0
          // close skdoc
          // KEYBOARD allTRIM(str(DokkTtnr))+chr(K_ENTER)
          // retu .f.
          lRet:=.F.
        else
          // ��७�ᥬ � rs1 �㬬� ������ sdo �� �㬬넮��� - sdp
          sele rs1
          netrepl('sdo', { rs1->sdv - skdoc->sdp })

        endif

      else
        wmess('��-�� '+str(kklr, 7)+' ��� � "�����થ"', 2)
        DokkTtnr=0
        // � ������ �� ������
        lRet:=.F.
      endif

      close skdoc
    endif

    if (lRet)
      if (kpl#kklr)
        wmess('�� ᮢ������ ������', 2)

        //nuse('rs1') ��⠢�� ������ ��� ���᪠ �� ᯨ��
        nuse('rs3')
        DokkTtnr=0
        //KEYBOARD allTRIM(str(DokkTtnr))+chr(K_ENTER)
      //retu .f.
      else
        sele rs1
        ReadMem4Dokk()
        sele dokk
        if (kklr=20034.and.gnAdm=0)
          if (!netseek('t9', 'skr,DokkTtnr'))
          else
            if (DokkTtnr#DokkTtn1r .and. int(bsr/1000)#361)
              wmess('��� '+str(DokkTtnr, 6)+' ��� ����祭�', 2)
              aqstr=2
              aqst:={ "���", "��" }
              aqstr:=alert("���쪮 ������ ⮢�� �� ���; ������ ������ �㬬� ����⥫���", aqst)
              if (aqstr = 2)
                lRetured:=.t.
              else
                DokkTtnr=0
              endif

            endif

          endif

        endif

      endif

    endif

    sele rs1
    if (kpl = kklr)
      if (DokkTtnr=0)
        nuse('rs1')
        nuse('rs3')
        return (.f.)
      endif

      if (DokkTtnr#0)
        ksvz2()
      endif

      nuse('rs1')
      nuse('rs3')
    endif

  endif                     // ���� ����� ���

  if (DokkTtnr=0)
    ggr=0
    mmr=0
    if (select('rs1')=0)
      if (kklr=20034)
        // �᫨ 20034, � ⥪��� ��
        pathr=gcPath_d+DirDr
        netuse('rs1',,, 1)
      else
        // �᫨ ��㣨� � ��� 20 �����, ��� 21 ᪤��
        do case
        case (gnEnt=20)
          use (gcPath_db + 'accord_deb') ALIAS rs1 NEW SHARED READONLY
        case (gnEnt=21)

          use (gcPath_db + 'skdoc') ALIAS rs1 NEW SHARED READONLY
        otherwise
          pathr=gcPath_d+dirdr
          netuse('rs1',,, 1)
        endcase

      endif

    else
    // ������ �� �� ᮢ������� ������
    endif

    wselect(0)
    sele rs1
    go top
    rcrs1r=recn()
    while (.t.)
      sele rs1
      go rcrs1r
      zagr='��� '+iif(fieldpos('sdo')#0, 'rs1', 'a�o')                                                      ;
       +' '+iif(ggr=0, dtos(gdTd), str(ggr, 4)+iif(mmr<10, '0'+str(mmr, 1)+'01', str(mmr, 2)+'01'))

      if (int(bsr/1000)#361)
        if (fieldpos('sdo')#0)// Rs1

          if (kklr=20034)
            rcrs1r=slcf('rs1',,,,,                                                                                                                                                                                                                                              ;
                         "e:ttn h:'���' c:n(6) e:kop h:'���' c:n(3) e:prz h:'�' c:n(1) e:dop h:'��⠎' c:d(10) e:sdv h:'�㬬�' c:n(10,2) e:kta h:'����' c:n(4) e:getfield('t1','rs1->kta','s_tag','fio') h:'���' c:c(15) e:ktas h:'���C' c:n(4) e:sdo h:'�����' c:n(12,2)",,,,, ;
                         "kpl=kklr.and.sdv#0.and.!netseek('t9','skr,rs1->ttn','dokk').and.!empty(dop).and.rs1->kop#196.and.rs1->kpl=kklr",, zagr                                                                                                                                 ;
                      )
          else
            // ����� ������
            rcrs1r=slcf('rs1',,,,,                                                                                                                                                                                                                                              ;
                         "e:ttn h:'���' c:n(6) e:kop h:'���' c:n(3) e:prz h:'�' c:n(1) e:dop h:'��⠎' c:d(10) e:sdv h:'�㬬�' c:n(10,2) e:kta h:'����' c:n(4) e:getfield('t1','rs1->kta','s_tag','fio') h:'���' c:c(15) e:ktas h:'���C' c:n(4) e:sdo h:'�����' c:n(12,2)",,,,, ;
                         "kpl=kklr.and.sdv#0.and.!empty(dop).and.rs1->kop#196.and.rs1->kpl=kklr",, zagr                                                                                                                                                                          ;
                      )

          endif

        else
          // �����
          tmFlAc:=IIF(SECONDS() < 3600 * 12, 3600 * 6, 3600 * 13)
          rcrs1r=slcf('rs1',,,,,                                                                                                               ;
                       "e:ttn h:'���' c:n(6) e:kop h:'���' c:n(3) e:prz h:'�' c:n(1) "                                                          ;
                       +"e:dop h:'��⠎' c:d(10) e:sdv h:'�㬬�' c:n(10,2) "                                                                    ;
                       +"e:sdp h:'�㬬��' c:n(10,2) e:kta h:'����' c:n(4) e:getfield('t1','rs1->kta','s_tag','fio') h:'���' c:c(15) "           ;
                       +"e:ktas h:'���C' c:n(4)",,,,,                                                                                           ;
                       "kpl=kklr.and.sdv#0"                                                                                                     ;
                       +".and.iif(netseek('t9','skr,rs1->ttn','dokk'),iif(date()=dokk->Ddc,iif(TimeToSec(dokk->tdc)>=tmFlAc,.f.,.t.),.t.),.t.)" ;
                       +".and.!empty(dop).and.rs1->kop#196.and.rs1->kpl=kklr",, zagr                                                            ;
                    )
        endif

      else
        rcrs1r=slcf('rs1',,,,,                                                                                                                                                                                                                                         ;
                     "e:ttn h:'���' c:n(6) e:kop h:'���' c:n(3) e:prz h:'�' c:n(1) e:dop h:'��⠎' c:d(10) e:sdv h:'�㬬�' c:n(10,2) e:kta h:'����' c:n(4) e:getfield('t1','rs1->kta','s_tag','fio') h:'���' c:c(15) e:ktas h:'���C' c:n(4) e:nap h:'����' c:n(4)",,,,, ;
                     "sdv#0.and.rs1->prz=1.and.rs1->kpl=kklr",, zagr                                                                                                                                                                                                    ;
                  )
      endif

      if (lastkey()=K_ESC)
        DokkTtnr=0
        exit
      endif

      if (lastkey()=K_ENTER)
        go rcrs1r
        nKeyReadExit:=K_ENTER

        if (FieldPos('sk')#0)// ��८�।��塞
          skr:=sk
        endif

        if (FieldPos('sdo')#0)// �� Rs1
          nKeyReadExit:=K_CTRL_W
        endif

        DokkTtnr=ttn
        exit
      endif

    enddo

    wselect(p1)
    close rs1
    if (DokkTtnr#0)
      KEYBOARD allTRIM(str(DokkTtnr))+chr(nKeyReadExit)
    endif

    return (.F.)

  endif

  return (.t.)

/************** */
function aVdNal(p1)
  /************** */
  // p1 - ���ਯ�� ����
  if (VdNalr#0)
    if (gdTd<ctod('01.03.2013'))
      if (!netseek('t1', 'TVdNalr,VdNalr', 'VdNal'))
        VdNalr=0
      endif

    else
      if (!netseek('t1', 'TVdNalr,VdNalr', 'VdNaln'))
        VdNalr=0
      endif

    endif

  endif

  if (VdNalr=0)
    wselect(0)
    if (gdTd<ctod('01.03.2013'))
      sele VdNal
    else
      sele VdNaln
    endif

    if (netseek('t1', 'TVdNalr'))
      rcVnr=recn()
      while (.t.)
        sele VdNal
        go rcVnr
        if (gdTd<ctod('01.03.2013'))
          rcVnr=slcf('VdNal',,,,, "e:VdNal h:'���' c:n(2) e:nVdNal h:'������������' c:c(40)",,,, 'TVdNal=TVdNalr',,, '��� ���㬥��')
        else
          rcVnr=slcf('VdNaln',,,,, "e:VdNal h:'���' c:n(2) e:nVdNal h:'������������' c:c(40)",,,, 'TVdNal=TVdNalr',,, '��� ���㬥��')
        endif

        if (lastkey()=K_ESC)
          exit
        endif

        if (lastkey()=K_ENTER)
          go rcVnr
          VdNalr=VdNal
          exit
        endif

      enddo

    endif

    wselect(p1)
  endif

  return (.t.)

/************************* */
static function TtnBs_S()
  // ����� ॠ�쭮� �㬬�
  /************************* */
  if (!empty(DocPrdr).and.DokkSkr#0.and.DokkTtnr#0)

    if (bs_sr=TtnSdvr)
      koefr=1
    else
      koefr=Round(bs_sr / TtnSdvr, 3)
      if (int(bsr/1000)=301)// ��� �����
        if (abs(koefr) = 1)
          wmess('�����쪠� ࠧ��� ����� �㬬���� � �㬬���', 0)
          return (.f.)

        endif

      endif

    endif

    if (!chk10())
      return (.f.)
    endif

    return (.t.)

    set cons off
    set prin to txt.txt
    set prin on

    pathmr=gcPath_e+'g'+str(year(DocPrdr), 4)+'\m'+iif(month(DocPrdr)<10, '0'+str(month(DocPrdr), 1), str(month(DocPrdr), 2))+'\'
    sele cskl
    locate for sk=DokkSkr
    pathr=pathmr+alltrim(path)
    if (netfile('rs2', 1))
      netuse('rs2', 'rs2opl',, 1)
      if (netseek('t1', 'DokkTtnr'))
        ssvpr=0
        while (ttn=DokkTtnr)
          if (int(mntov/10000)<2)
            skip
            loop
          endif

          ktlr=ktl
          kvpr=kvp
          kfr=0
          if (.f.)        //gnEnt=21
            sele ksvz2
            locate for ktl=ktlr
            if (foun())
              kfr=kf
              kvpr=kvpr-kfr
            endif

          endif

          if (kvpr=0)
            sele rs2opl
            skip
            loop
          endif

          if (koefr#0)
            kvpr=Round(kvpr*koefr, 3)
          endif

          sele rs2opl
          //            zenr=Round(zen,2)
          zenr=zen
          svpr=Round(zenr*kvpr, 2)
          ssvpr=ssvpr+svpr
          ?space(30)+' '+str(kvpr, 9, 3)+' '+str(zenr, 9, 2)+' '+str(svpr, 9, 2)+' '+str(kfr, 9, 3)
          sele rs2opl
          skip
        enddo

        sndsr=ssvpr*1.2
        smndsr=ssvpr*0.2
        ?'�����'+space(46)+str(ssvpr, 9, 2)
        ?'���  '+space(46)+str(smndsr, 9, 2)
        ?'�ᥣ�'+space(46)+str(sndsr, 9, 2)+' '+str(bs_sr, 9, 2)
        bs_sr=sndsr
      endif

      nuse('rs2opl')
    endif

    set prin off
    set cons on
  endif

  return (.t.)

/******************* */
function chk10()
  /*�஢�ઠ �� 10000
   *******************
   */
  local rcDokk_rr, bs_srr
  bs_srr=0
  if (!(TtnKopr=160.or.TtnKopr=161))
    return (.t.)
  endif

  prerr=0
  if (int(bsr/1000)=301)
    sele dokk
    rcDokk_rr=recn()
    set orde to tag t10     // kkl,bs_d,ddk
    if (netseek('t10', 'kklr,bsr,ddcr'))
      smttn_r=0
      while (kkl=kklr.and.bs_d=bsr.and.ddk=ddcr)
        if (bs_k#361001)
          skip
          loop
        endif

        if (mnp#0)
          skip
          loop
        endif

        //         if NChek=0
        //            skip
        //            loop
        //         endif
        smttn_r=smttn_r+bs_s
        skip
      enddo

      if (bs_sr+smttn_r>10000)
        bs_srr=10000-smttn_r
        prerr=1
      endif

    else
      if (bs_sr>10000)
        bs_srr=10000
        prerr=1
      endif

    endif

    sele dokk
    go rcDokk_rr
  endif

  if (prerr=1)
    wmess('����� 10000 ����� '+str(bs_srr, 12, 2), 0)
    return (.f.)
  endif

  return (.t.)

/*****************************************************************
 
 FUNCTION:
 �����..����..........�. ��⮢��  03-09-17 * 11:37:34am
 ����������......... ���� � ������ ��� ���
 ���������..........
 �����. ��������.... ��� ��ਮ��, � ���뢠� ����
                     � ������ ����, �� �������
 ����������.........
 */
function Seek_SkTtn(nl_skr, nl_DokkTtnr, dl_dEndSeek)
  local dBeg:=BOM(gDtd), dEnd:=BOM(dEndSeek), dtSkTtn:=BLANK(DATE())

  nuse('rs1')
  nuse('rs3')

  while (dBeg >= dEnd)
    DirDr=alltrim(getfield('t1', 'skr', 'cskl', 'path'))
    Pathr=gcPath_e + pathYYYYMM(dBeg) + "\" + DirDr// ���� ᪫�� ���筨��

    if (netfile('rs1', 1))// ��� 䠩��
      netuse('rs1',,, 1)
      if (netseek('t1', 'DokkTtnr'))// ��� ���
        netuse('rs3',,, 1)
        dtSkTtn:=dBeg
        exit
      endif

      nuse('rs1')
    endif

    dBeg:=ADDMONTH(dBeg, -1)
  enddo

  return (dtSkTtn)

/*****************************************************************
 
 FUNCTION:
 �����..����..........�. ��⮢��  09-14-17 * 04:56:47pm
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
function ReadMem4Dokk()
  ktar=kta
  ktasr=ktas
  TtnSdvr=getfield('t1', 'DokkTtnr,90', 'rs3', 'ssf')
  TtnKopr=kop
  napr=nap
  if (fieldpos('sdo')#0)
    TtnSdor=sdo
  endif

  if (prFrmr=1)           //.or.prFrmr=2
    if (kklr#20034)       //.or.gnAdm=1
      if (bs_sr=0)
        bs_sr=TtnSdvr-TtnSdor
      endif

    else
      if (bs_sr=0)
        //  bs_sr=sdv
        bs_sr=TtnSdvr
      endif

    endif

  else
    if (bs_sr=0)
      bs_sr=TtnSdvr         //sdv
    endif

  endif

  return (nil)

