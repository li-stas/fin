#include "inkey.ch"
// Касса
#define LF chr(10)
*************
func artno()
*************
  sele kassa
  locate for kassa=gnKassa
  if foun()
     reclock()
     artnor=artno
     if artnor=9999
        netrepl('artno','1')
     else
        netrepl('artno','artno+1')
     endif
     retu str(artnor,9)
  else
     retu ''
  endif

*******************
func chekds(p1)
// p1 1- Новый чек
*******************
  local ServerRespons:="", nRecNo
  smvzr=0
  sele dokk
  if netseek('t1','mnr,rndr')
     bs_kr=bs_k
//     if gnEntrm=0
        if !(bs_kr=361001.or.bs_kr=377001)
           wmess('Недопустимый КОП '+str(kop,3),3)
           retu .t.
        endif
        if .F. .and. gnEnt=21  // .T.->21 для Лодиса блокировка по сумме.
           if bs_kr=361001.and.prFrmr=0
              wmess('НИЗЗЯ!!!'+str(kop,3),3)
              retu .t.
           endif
        endif
//     else
//        if !(bs_kr=361001)
//           wmess('Недопустимый КОП '+str(kop,3),3)
//           retu .t.
//        endif
//     endif
     if nplp#0
        vnplpr=1
        anplp={'Нет','Да'}
        vnplpr=alert('Есть N платежки.Продолжить?',anplp)
        if vnplpr#2
           retu .t.
        endif
     endif
     errkopr=0
     do while mn=mnr.and.rnd=rndr
        if bs_k#bs_kr
           errkopr=1
           exit
        endif
        sele dokk
        skip
     enddo
     if errkopr=1
        wmess('Разные КОП в чеке',3)
        retu .t.
     endif
  endif
  if gnKassa=0
     wmess('Нет доступа к кассе',2)
     retu .t.
  endif

  sele kassa
  locate for kassa=gnKassa
  if !foun()
     wmess('Нет найдена касса',2)
     retu .t.
  endif
  reclock()
  NChekr=NChek
  if NChekr=999999
     NChekr=1
     netrepl('NChek','2')
  else
     netrepl('NChek','NChek+1')
  endif

  set cons off
  set prin to chek.txt
  set prin on
  #ifdef __CLIP__
  #else
     ?'Чек N'+' '+str(NChekr,6)
  #endif
  sele dokk
  error=.f.
  nst_rr=0
  if netseek('t1','mnr,rndr')
    smvr=0
    smv_r=0
    smv_rr=0
    ssvpr=0

    nRecNo:=RECNO()

    // проверка на возратную сумму по ТТН и одиночность проводки
    nCnt:=0; lRetured:=.f.
    do while mn=mnr.and.rnd=rndr
      ++nCnt
      If koef < 0
        lRetured:=.t.
      endif
      skip
    enddo
    If lRetured .and. nCnt > 1
      wmess('Возврат возможен только по одной ТТН.'+' найдено записей'+str(nCnt,3),3)
      retu .t.
    EndIf

    DBGoTo(nRecNo)
    do while mn=mnr.and.rnd=rndr
      ttnr := DokkTtn
      DokkSkr := DokkSk
      DokkTtnr := DokkTtn
      DocPrdr := DocPrd
      smr := bs_s
      smvr := smvr+smr
      sdvr := 0

       if fieldpos('koef')#0
         koefr:=koef
         If koefr < 0
           koefr := ABS(koefr)
         EndIf
       else
           koefr:=1
       endif

        if gnEnt=21
           nmchkr="НАПОI РIЗНI "
        else
           nmchkr="ПРОД. ХАРЧ. "
           //nmchkr="КРАБОВI ПАЛИЧКИ вес. ВМ "
        endif
        prTtnr=0
        if DokkTtnr#0.and.DokkSkr#0
           nuse('rs1')
           nuse('rs2')
           nuse('rs3')
           sele cskl
           locate for sk=DokkSkr
           if foun()
              if empty(DocPrdr)
                pathr=gcPath_d+alltrim(path)
              else
                pathr=gcPath_e + pathYYYYMM(DocPrdr)+'\'+alltrim(path)
              endif
              if netfile('rs1',1)
                 prTtnr=1
              endif
           endif
        endif
        if prTtnr=0 // нет ссылки на накладную
           #ifdef __CLIP__
             #ifdef DEBUG
                outlog(__FILE__,__LINE__,mn,rnd,bs_s)
                secr=seconds()
             #endif
             nSumSkidka:=0 ;  cMesSkidka:=""

             IF .not. (dokk->bs_k#377001) // услуги
                natr := "ПОСЛУГИ..."
                svpr :=  smr
                zenr :=  smr
                kvpr :=    1
                nTax:=2

             ELSE // товара
               MnTovr:=MnTov177r(gnEnt)
               if gnEnt=20
                 RoundKvp:=2 // до 10 грамм
               else
                 RoundKvp:=0 // целого - до бутылки
               endif

               // добавление второй налоговой группы
               nTax:=2
               kg_r=int(MnTovr/10000)
               if !empty(getfield('t1','kg_r','cgrp','nal'))  .and. kopr=169
                  nTax:=(nTax*10)+3 // 3 добавить еще одну группу
                  //nTax+=30 // 3 добавить еще одну группу
               endif

               natr:=NaT4Kass()

               IF EMPTY(natr) // не нашли кода
                 nmchkr= Iif(gnEnt=21,"НАПОI РIЗНI ","ПРОД. ХАРЧ. ")
                 natr :=  nmchkr+IIF(ttnr=0,"","ЗА TTH N'"+str(ttnr,6))
                  svpr :=  smr
                  zenr :=  smr
                  kvpr :=    1
               ELSE
                  NDSr:=round((100+gnNDS)/100,2) //  НДС  1.20  20%

                  zenr :=  getfield('t1','MnTovr','ctov','cenpr')
                  //   по ценам без НДС
                  kvp_r:=0
                  zen_r:=round(zenr,2) // окрулим до 2 тк КА
                  sm10r:=round(round(smr/(100+gnNDS),2)*100,2) // без НДС
                    TmpSvp4Trs2()
                  zenr:=zen_r; kvpr:=kvp_r

                  zenNDSr:=round(zenr*NDSr,2) // ЦЕНА ВТЧ НДС

                  kvpr:=round(smr/zenNDSr,RoundKvp)
                  zenr := zenNDSr
                  svpr := round(zenNDSr*kvpr,2)

                  if  smr # round(zenNDSr*kvpr,2)
                     // посчитать +- округление
                     nSumSkidka := smr - round(zenNDSr*kvpr,2)
                     //cMesSkidka:="округление"+;
                     cMesSkidka:=Iif(nSumSkidka > 0,"надбавка","снижка")
                  endif

               ENDIF

             ENDIF


              cAdd_Check := LTRIM(STR(nTax,2))+LF //налоговая группа ВЛОЖЕННЫЙ НДС
              //outlog(__FILE__,__LINE__,cMesSkidka,nLastRecNoTtn == rs2->(RECNO()),i=1)
              IF !EMPTY(cMesSkidka)
                cMesSkidka:="."
                cAdd_Check += ;
                cMesSkidka +LF+ ;
                LTRIM(str(nSumSkidka,9,2))+LF

              ENDIF



             if fisc_cmd(@gaKassa:nConnect,,;
                "FSTART: ADD_CHECK"+LF+;
                subs(natr,1,43) +LF+ ; // наим арт(товара c(30))
                LTRIM(artno())+LF+;//код артикуля товара
                LTRIM(STR(svpr, 9, 2))+LF+  ;//сумма(svp)
                LTRIM(STR(zenr, 9, 2))+LF+;//цена(zen*1.2)
                LTRIM(STR(kvpr, 9, 3))+LF+;//к-во(kvp)
                cAdd_Check+;
                "FEND:"+LF,;
                 gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports;
                )=0
             else
                error=.t.
                exit
             endif
             #ifdef DEBUG
               outlog(__FILE__,__LINE__,secr-seconds())
             #endif
           #else
             ?str(ttnr,6)+' '+str(smr,10,2)
           #endif
        else // prTtnr=1 // есть ссылка на накладную
           #ifdef __CLIP__
             if empty(p1) // Старый чек
                sele dokk
                #ifdef DEBUG
                  outlog(__FILE__,__LINE__,mn,rnd,bs_s)
                  secr=seconds()
                #endif
                if fisc_cmd(@gaKassa:nConnect,,;
                   "FSTART: ADD_CHECK"+LF+;
                   IIF(dokk->bs_k#377001,;
                   nmchkr+IIF(ttnr=0,"","ЗА TTH N'"+str(ttnr,6)),; // наим арт
                   "ПОСЛУГИ..."; // наим арт(товара c(30))
                      ) + LF + ;
                   LTRIM(artno())+LF+;//код артикуля товара
                   LTRIM(STR(smr, 9, 2))+LF+  ;//сумма(svp)
                   LTRIM(STR(smr, 9, 2))+LF+;//цена(zen*1.2)
                   LTRIM(STR(1, 9, 3))+LF+;//к-во(kvp)
                   STR(2,1) +LF+;//налоговая группа ВЛОЖЕННЫЙ НДС
                   "FEND:"+LF,;
                    gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports;
                   )=0
                else
                   error=.t.
                   exit
                endif
                #ifdef DEBUG
                   outlog(__FILE__,__LINE__,secr-seconds())
                #endif
             else  // Новый чек
                if empty(DocPrdr)
                  pathr=gcPath_d+alltrim(path)
                else
                  pathr=gcPath_e + pathYYYYMM(DocPrdr)+'\'+alltrim(path)
                endif

                netuse('rs1',,,1)
                netuse('rs2',,,1)
                netuse('rs3',,,1)

                nSumAkiz5:=getfield('t1','ttnr,13','rs3','ssf')
                nSm43:=getfield('t1','ttnr,43','rs3','ssf')

                kopr=getfield('t1','ttnr','rs1','kop')
                //ttnsdvr=getfield('t1','DokkTtnr','rs1','sdv')

                smvzr:=0
                lSumAkiz5Locate:=.F. //доп поиск суммы Акциза
                ksVz2() // поиск возвратов

                sele rs1
                netseek('t1','ttnr')
                pr169r:=pr169; pr139r:=pr139; pr129r:=pr129; pr177r:=pr177

                if fieldpos('MnTov177')#0
                  MnTov177r=MnTov177
                  prc177r=prc177
                else
                  MnTov177r=0
                  prc177r=0
                endif


                If pr169rEQ2(ttnr) // проверка на всернутые ТТН
                  nuse('rs2')
                  nuse('rs2m')

                  lcrtt('t1rs2','rs2')
                  lindx('t1rs2','rs2')
                  lcrtt('t1rs2m','rs2')
                  lindx('t1rs2m','rs2')

                  sm10r=getfield('t1','ttnr,10','rs3','ssf') // сумма товара
                  kvp_r:=0

                  Crt_trs2()
                  AddRec2Trs2(rs1->(mk169 +  mk129 + mk139)) // mk177 +
                  sele trs2
                  aRecTRs2:=trs2->({npp,nat,ukt,upu,kod,kol,zen,sm})
                  close trs2

                  // заменили
                  use t1rs2 alias rs2 new Exclusive
                  append from trs2
                  netrepl('npp,nat,ukt,upu,kod,kvp,zen,svp',;
                          aRecTRs2)
                  repl ttn with ttnr

                EndIf

                sele rs2
                if netseek('t1','ttnr')
                   ssvpr:=0 // сумма TOTAL без НДС по ценам без НДС
                   ssvpNDSr:=0 // сумма TOTAL _C_ НДС по ценам с НДС

                   nRecNo:=RECNO()
                   nLastRecNoTtn:=RECNO() //запись в которую можно вставить скидку
                   nSvpMax:=0 //макс сумма строки ТТН
                   ssvpr:=0 // сумма TOTAL _C_ НДС по цена без НДС
                   NDSr:=round((100+gnNDS)/100,2) //  НДС  1.20  20%
                   //если КА с ВТЧ_НДС, то цену можно взять с 3 знаками, все равно будет округление в строках
                   nRoundZen:=iif(gaKassa:KoNDS = 1,3,2)

                   // проверим на наличие цены в 1 коп.
                   If !lRetured // не сторно чек
                     do while ttn=ttnr
                       zenr:=roun(zen,nRoundZen)
                       zenNDSr:=round(zenr*NDSr,2) // ЦЕНА ВТЧ НДС
                       If zenNDSr = 0.01
                         wmess('Цена 1 коп. ТТН '+str(ttn,6)+' тов.'+str(MnTov,7),3)
                         retu .t.
                       EndIf
                       sele rs2
                       skip
                     enddo
                   EndIf

                   DBGoTo(nRecNo)
                   do while ttn=ttnr
                      if int(MnTov/10000)<2
                         if int(MnTov/10000)=0
                            if getfield('t1','ttnr,12','rs3','ssf')=0 // Нет ндс на тару
                               skip
                               loop
                            endif
                         else
                            skip
                            loop
                         endif
                      endif
                      kvpr:=kvp
                      MnTovr:=MnTov
                      zenr:=roun(zen,nRoundZen)
                      zenNDSr:=round(zenr*NDSr,2) // ЦЕНА ВТЧ НДС
                      //макс сумма строки ТТН
                      If .T. .and. roun(zenr*kvpr,2) > nSvpMax
                        nSvpMax:=roun(zenr*kvpr,2)
                        nLastRecNoTtn:=RECNO()
                      else
                        //nLastRecNoTtn:=RECNO() //запись в которую можно вставить скидку
                      EndIf

                      if koefr#0
                         kvpr=roun(kvpr*koefr,3)
                      endif
                      //делим к-во
                      akvpr:={}
                      If kvpr > 100 .and. mod(kvpr,1) <> 0
                        AADD(akvpr,INT(kvpr/1))
                        AADD(akvpr,mod(kvpr,1))
                      Else
                        akvpr:={kvpr}
                      EndIf
                      for i:=1 to len(akvpr)

                        kvpr:=akvpr[i]
                        svpr:=roun(zenr*kvpr,2)
                        ssvpr:=ssvpr+svpr // сумма TOTAL БЕЗ НДС

                        svpNDSr:=round(zenNDSr*kvpr,2) // сумма  ВТЧ НДС
                        ssvpNDSr+=svpNDSr // сумма TOTAL _C_ НДС
                      next


                      ///// повторный поиск Суммы Акциза ///
                      if .not. lSumAkiz5Locate
                        kg_r=int(MnTovr/10000)
                        if !empty(getfield('t1','kg_r','cgrp','nal')) ;
                              .and. kopr=169 .and. nSumAkiz5=0
                          lSumAkiz5Locate:=.T.
                        endif
                      endif
                      ///

                      skip
                   enddo

                   IF lSumAkiz5Locate
                     rs3->(__dbLocate({|| ttnr=ttn .and. ksz=13 }))
                     If rs3->(found())
                       nSumAkiz5:=rs3->ssf
                     else
                       wmess('Нет Суммы Акциза в ТТН Ошибка!!! Повторите поже',0)
                     EndIf
                   ENDIF

                   nSumSkidka:=0 ;  cMesSkidka:=""

                   if koefr = 1
                     // берем только товарную часть услуги НЕ УЧИТЫВАЮТСЯ
                     // сумма TOTAL БЕЗ НДС
                     ssvpr = round(ssvpr + nSm43,2)
                     smr := round(ssvpr * NDSr,2)
                   else
                     // взять сумму котору затребовали частичной оплаты
                     smr := smr // smr:= bs_s
                   endif

                   if smr # ssvpNDSr // добавили НДС и сравнили

                      nSumSkidka:= smr - ssvpNDSr
                      //cMesSkidka:="округление"+;
                      cMesSkidka:=Iif(nSumSkidka > 0,"надбавка","снижка")
                        // добавить в последний товар  разницу
                   endif



                   // формирование чека
                   aSum:={  ssvpNDSr,  ssvpr}
                   ssvpr:=0
                   ssvpNDSr:=0
                   DBGOTO(nRecNo)
                   do while ttn=ttnr
                      MnTovr=MnTov
                      if int(MnTov/10000)<2
                         if int(MnTov/10000)=0
                            if getfield('t1','ttnr,12','rs3','ssf')=0 // Нет ндс на тару
                               skip
                               loop
                            endif
                         else
                            skip
                            loop
                         endif
                      endif

                      natr:=NaT4Kass()
                      //natr=getfield('t1','MnTovr','ctov','nat')

                      zenr=roun(zen,nRoundZen)
                      zenNDSr:=round(zenr*NDSr,2) // ЦЕНА ВТЧ НДС
                      kvpr=kvp
                      sele rs2
                      if kvpr=0
                         sele rs2; skip
                         loop
                      endif
                      if koefr#0
                         kvpr=roun(kvpr*koefr,3)
                      endif
                      //делим к-во
                      akvpr:={}
                      If kvpr > 100 .and. mod(kvpr,1) <> 0
                        AADD(akvpr,INT(kvpr/1))
                        AADD(akvpr,mod(kvpr,1))
                      Else
                        akvpr:={kvpr}
                      EndIf
                      for i:=1 to len(akvpr)
                        kvpr:=akvpr[i]

                        svpr:=roun(zenr*kvpr,2)
                        ssvpr:=ssvpr+svpr // сумма TOTAL БЕЗ НДС

                        svpNDSr:=round(zenNDSr*kvpr,2) // сумма  ВТЧ НДС
                        ssvpNDSr+=svpNDSr // сумма TOTAL _C_ НДС

                        If gaKassa:KoNDS = 1 //КА только цена втч НДС
                          svpr := svpNDSr
                          zenr := zenNDSr
                          kvpr := kvpr

                         // добавление второй налоговой группы
                         nTax:=2
                         kg_r=int(MnTovr/10000)
                         if !empty(getfield('t1','kg_r','cgrp','nal')) ;
                            .and. kopr=169 .and. nSumAkiz5=0

                             nTax:=(nTax*10)+3 // 3 добавить еще одну группу

                             wmess('Нет Суммы Акциза в ТТН Ошибка!!! Повторите поже',0)
                             error=.t.
                             exit

                         endif

                           cAdd_Check := LTRIM(STR(nTax,2))+LF //налоговая группа ВЛОЖЕННЫЙ НДС
                          //outlog(__FILE__,__LINE__,cMesSkidka,nLastRecNoTtn == rs2->(RECNO()),i=1)
                          IF !EMPTY(cMesSkidka) ;
                            .and. nLastRecNoTtn == rs2->(RECNO()) ;
                            .and. i=1

                            cMesSkidka:="."
                            cAdd_Check += ;
                            cMesSkidka +LF+ ;
                            LTRIM(str(nSumSkidka,9,2))+LF

                          ENDIF
                        Else
                          cAdd_Check := STR(3,1)+LF //налоговая группа НАЛОЖЕННЫ НДС
                        EndIf

                        if fisc_cmd(@gaKassa:nConnect,,;
                           "FSTART: ADD_CHECK"+LF+;
                            subs(natr,1,43) +LF+ ; // наим арт(товара c(30))
                           LTRIM(artno())+LF+;//код артикуля товара
                           LTRIM(str(svpr,9,2))+LF+  ;//сумма(svp)
                           LTRIM(str(zenr,9,2))+LF+;//цена(zen*1.2) !!!!????
                           LTRIM(str(kvpr,9,3))+LF+;//к-во(kvp)
                           cAdd_Check+;
                           "FEND:"+LF,;
                            gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports;
                           )=0

                            //outlog(__FILE__,__LINE__,str(MnTovr,7),subs(natr,1,43)+' '+str(kvpr,9,3)+' '+str(zenr,9,2)+' '+str(svpr,9,2))
                            IF !EMPTY(cMesSkidka);
                              .and. nLastRecNoTtn == rs2->(RECNO()) ;
                              .and. i=1
                               //outlog(__FILE__,__LINE__,"cMesSkidka",nSumSkidka,  aSum[1], ROUND(aSum[2]*1.2,2), aSum[2], cMesSkidka)
                               cMesSkidka:=""
                            ELSE
                              //outlog(__FILE__,__LINE__)
                            ENDIF
                        else
                           error=.t.
                           exit
                        endif

                      next
                      If error
                        exit
                      EndIf
                      sele rs2
                      skip
                   enddo
                  if kopr=169 .and. nSumAkiz5 # 0
                    if koefr#0
                        kvpr:=1
                        kvpr:=roun(kvpr*koefr,3)
                        nSumAkiz5:=roun(nSumAkiz5*kvpr,2)
                        // nSumAkiz5:=round(Ceiling(ssf13_1r * 10^RndSdvr)/10^RndSdvr, RndSdvr)
                        kvpr:=1
                    endif

                    natr := "ДОДАТКОВИЙ АКИЗНИЙ СБIP 5%"
                    svpr :=  nSumAkiz5
                    zenr :=  nSumAkiz5
                    kvpr := 1
                    nTax := 1 // НДС 0%

                    ssvpNDSr += nSumAkiz5

                    cAdd_Check := LTRIM(STR(nTax,2))+LF //налоговая группа ВЛОЖЕННЫЙ НДС

                    if fisc_cmd(@gaKassa:nConnect,,;
                        "FSTART: ADD_CHECK"+LF+;
                        subs(natr,1,43) +LF+ ; // наим арт(товара c(30))
                        LTRIM(artno())+LF+;//код артикуля товара
                        LTRIM(str(svpr,9,2))+LF+  ;//сумма(svp)
                        LTRIM(str(zenr,9,2))+LF+;//цена(zen*1.2) !!!!????
                        LTRIM(str(kvpr,9,3))+LF+;//к-во(kvp)
                        cAdd_Check+;
                        "FEND:"+LF,;
                        gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports;
                       )=0


                    else
                        error=.t.
                    endif
                  endif
                  sndsr=ssvpr*1.2
                  smndsr=ssvpr*0.2
                  ?'Товар'+space(46)+str(ssvpr,9,2)
                  ?'НДС  '+space(46)+str(smndsr,9,2)
                  ?'Всего'+space(46)+str(sndsr,9,2)+' '+str(smr,9,2)
                else
                  wmess('Нет ТТН Ошибка!!',0)
                endif
                nuse('rs3')
                nuse('rs2')
                nuse('rs1')
             endif
           #else
             if empty(DocPrdr)
                pathr=gcPath_d+alltrim(path)
             else
                pathr=gcPath_e + pathYYYYMM(DocPrdr)+'\'+alltrim(path)
             endif
             netuse('rs1',,,1)
             //ttnsdvr=getfield('t1','DokkTtnr','rs1','sdv')
             netuse('rs2',,,1)
             netuse('rs3',,,1)
             kopr=getfield('t1','ttnr','rs1','kop')
             smvzr=0
             ksvz2()
             sele rs2
             ssvpr=0
             if netseek('t1','ttnr')
                ?'TTN '+str(ttnr,6)
                do while ttn=ttnr
                   MnTovr=MnTov
                   ktlr=ktl
                   if int(MnTov/10000)<2
                      if int(MnTov/10000)=0
                         if getfield('t1','ttnr,12','rs3','ssf')=0 // Нет ндс на тару
                            skip
                            loop
                         endif
                      else
                         skip
                         loop
                      endif
                   endif
                   natr:=NaT4Kass()
                   //natr=getfield('t1','MnTovr','ctov','nat')
                   kvpr=kvp
                   if kvpr=0
                      sele rs2
                      skip
                      loop
                   endif
                   if koefr#0
                      kvpr=roun(kvpr*koefr,3)
                   endif
                   sele rs2
                   zenr=roun(zen,2)
                   svpr=roun(zenr*kvpr,2)
                   ssvpr=ssvpr+svpr
                   ?subs(natr,1,43)+' '+str(kvpr,9,3)+' '+str(zenr,9,2)+' '+str(svpr,9,2)+' '+str(kfr,9,3)
                   sele rs2
                   skip
                enddo
                sndsr=ssvpr*1.2
                smndsr=ssvpr*0.2
                ?'Товар'+space(46)+str(ssvpr,9,2)
                ?'НДС  '+space(46)+str(smndsr,9,2)
                ?'Всего'+space(46)+str(sndsr,9,2)+' '+str(smr,9,2)
             endif
             nuse('rs1')
             nuse('rs2')
             nuse('rs3')
             nuse('ksvz2')
           #endif
        endif
        sele dokk
        nst_rr=nst_rr+1
        skip
        if eof()
           exit
        endif
     enddo
     #ifdef __CLIP__
        #ifdef DEBUG
        //outlog(__FILE__,__LINE__,"nst_rr=",nst_rr,"ssvpr=",ssvpr)
        secr=seconds()
        #endif
        if error
           fisc_cmd(@gaKassa:nConnect,,;
                  "FSTART: FREE_CHECK"+LF+;
                  "FEND:"+LF,;
                   gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports ;
                 )
        else
          if prTtnr=1
            If gaKassa:KoNDS = 1 //КА только цена втч НДС
              //outlog(__FILE__,__LINE__,"ssvpNDSr  nSumSkidka",ssvpNDSr , nSumSkidka)
              If round(smvr - (ssvpNDSr + nSumSkidka),2) <> 0
                 outlog(__FILE__,__LINE__,"ssvpNDSr  nSumSkidka S1 S_dokk NChek",ssvpNDSr , nSumSkidka, ssvpNDSr + nSumSkidka, smvr, NChekr)
              EndIf
              smvr:=ssvpNDSr + nSumSkidka
            else
              smvr:=ssvpr
            endif
            //outlog(__FILE__,__LINE__,"gaKassa:KoNDS",gaKassa:KoNDS,smvr)

          endif
           IF (fisc_cmd(@gaKassa:nConnect,@ServerRespons,;
               "FSTART: MARIA_PRCHECK"+LF+;
                gaKassa:scCashMen+LF+;
                gaKassa:MRA_PASSWD+LF+;
                "01"+LF+;//отдел
                IIF(lRetured,'1','0')+LF+                                     ;//на реализацию
                LTRIM(STR(smvr, 10, 2))+LF+     ;//сумма по чеку
                LTRIM(STR(0, 10, 2))+LF+;//сумма тара
                LTRIM(STR(0, 10, 2))+LF+;//сумма по платежному чеку
                LTRIM(STR(0, 10, 2))+LF+;//сумма по кредиту
                LTRIM(STR(smvr, 10, 2))+LF+;//сумма по наличными
                IIF(.T.,;
                   IIF(.F.,;
                     PADR("ВН.N ЧЕКА ",10)+;
                     PADL(LTRIM(STR(NChekr ,6)),6,"_")+" "+;
                     PADR("ТТН ",4)+;
                     PADL(LTRIM(STR(DokkTtnr ,6)),6,"_")+;
                     SPACE(13)+LF+;
                     "",;
                     PADL(LTRIM(STR(NChekr ,6)),6,"_")+" "+;
                     PADR("N ",3)+;
                     PADL(LTRIM(STR(DokkTtnr ,6)),6,"_")+" "+;
                     IIF(kklr=20034,"Конечный покупатель",getfield("t1","kklr","kln","nkl"))+;
                     SPACE(0)+LF+;
                     "";
                  ),;
                   "")+;//текст после фискальной информации
                   "FEND:"+LF,                                  ;
                   gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports              ;
                  )=0                                            ;
              )
                    //////////// регистрируем продажу  ////////
                    if !(dirchange('fiscrptday')=0)
                      dirmake('fiscrptday')
                    endif
                    dirchange(gcPath_l)
                    FCreate('fiscrptday\'+'MARIA_PRCHECK.txt')
                    //////////// END - регистрируем продажу  ////////

               error=.f.
               //outlog(__FILE__,__LINE__,ServerRespons)
            ELSE              //ошибка завершения чека
               fisc_cmd(@gaKassa:nConnect,,                       ;
                    "FSTART: FREE_CHECK"+LF+   ;
                    "FEND:"+LF,                     ;
                    gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE, gaKassa:Host, gaKassa:Ports ;
                    )
               wmess('Ошибка!! '+ProcName()+'('+ltrim(str(ProcLine(),5))+')',0)
               error=.t.
           ENDIF
        endif
        #ifdef DEBUG
        //outlog(__FILE__,__LINE__,'print_chek_time=',secr-seconds())
        #endif
     #endif
     if !error
        sele dokk
        if netseek('t1','mnr,rndr')
           do while mn=mnr.and.rnd=rndr
              netrepl('NChek,nplp','NChekr,NChekr')
              sele dokk
              skip
           enddo
        endif
        sele doks
        netrepl('NChek,nplp','NChekr,NChekr')
     endif
  endif
  set prin off
  set prin to
  set cons on
  retu .t.

*************
func ksotch()
  *************
  if gnEnt=20
     do case
        case bsr=301001
             skr=228
        case bsr=301003
             skr=300
        case bsr=301004
             skr=400
        case bsr=301005
             skr=500
        case bsr=301006
             skr=600
     endc
  else
     do case
        case bsr=301001
             skr=232
        case bsr=301004
             skr=700
     endc
  endif
  sele cskl
  locate for sk=skr
  pathr=gcPath_d+alltrim(path)
  netuse('rs1',,,1)
  netuse('rs3',,,1)
  if select('votch')#0
     sele votch
     use
  endif
  erase votch.dbf
  crtt('votch','f:nn c:n(2) f:cnn c:c(30)')
  sele 0
  use votch
  appe blank
  repl nn with 1,cnn with '169 ТТН'
  appe blank
  repl nn with 2,cnn with '160 ТТН'
  appe blank
  repl nn with 3,cnn with '161 ТТН'
  appe blank
  repl nn with 4,cnn with 'Клиент чек сумма'
  appe blank
  repl nn with 5,cnn with 'Клиент сумма'
  appe blank
  repl nn with 6,cnn with '169 эксп/маршр '
  sele votch
  go top
  rcvotchr=recn()
  do while .t.
     sele votch
     go rcvotchr
     rcvotchr=slcf('votch',,,,,"e:nn h:'NN' c:n(2) e:cnn h:'Отчет' c:c(30)",,,,,,,'Отчеты')
     if lastkey()=K_ESC
        exit
     endif
     go rcvotchr
     nnr=nn
     do case
        case nnr=1
             kotkop(169)
        case nnr=2
             kotkop(160)
        case nnr=3
             kotkop(161)
        case nnr=4
             kotklch()
        case nnr=5
             kotkl()
        case nnr=6
             kem()
     endc
  enddo
  nuse('rs1')
  nuse('rs3')
  retu .t.
**************
func kotklch()
  ***************
  alpt={'lpt1','lpt2','Файл'}
  vlpt=alert('ПЕЧАТЬ',alpt)
  do case
     case vlpt=1
          set prin to lpt1
     case vlpt=2
          set prin to lpt2
     case vlpt=3
          set prin to ksotch.txt
  endc

  set cons off

  if !empty(gcPrn)
     ?chr(27)+'E'+chr(27)+'&l1h26a0O'+chr(27)+chr(27)+'(3R'+chr(27)+'(s0p21.00h0s0b4099T'+chr(27)  // Книжная А4
  else
     ?chr(27)+chr(77)+chr(15)
  endif

  set prin on
  sele doks
  if netseek('t1','mnr')
     ?'Отчет по кассе за '+dtoc(ddcr)
     ?dtoc(date())+' '+time()
     ?'┌───────┬────────────────────────────────────────┬──────┬───┬────────────┐'
     ?'│'+'  Код  '+'│'+space(14)+'Наименование'+space(14)+'│'+'  Чек '+'│'+'Снс'+'│'+'    Сумма   '+'│'
     ?'├───────┼────────────────────────────────────────┼──────┼───┼────────────┤'
     ssdir=0
     smsncr=0
     sncr=9999
     do while mn=mnr
        if NChek=0
           skip
           loop
        endif
        if snc#sncr
           if sncr#9999
              ?'Всего по сеансу '+str(sncr,3)+space(38)+str(smsncr,12,2)
           endif
           smsncr=0
        endif

        cSnc:=str(_FIELD->Snc,3)
        cSnc:="   "

        ?' '+str(doks->kkl,7)+' '+subs(getfield('t1','doks->kkl','kln','nkl'),1,40)+' '+str(doks->NChek,6)+' '+cSnc+' '+str(doks->ssd,12,2)
        ssdir=ssdir+doks->ssd
        smsncr=smsncr+doks->ssd
        sele doks
        skip
     enddo
     ?'Итого'+space(52)+str(ssdir,12,2)
  endif
  set cons on
  set prin off
  set prin to txt.txt
  retu .t.

****************
func kotkop(p1)
  ****************
  kopr=p1
  alpt={'lpt1','lpt2','Файл'}
  vlpt=alert('ПЕЧАТЬ',alpt)
  do case
     case vlpt=1
          set prin to lpt1
     case vlpt=2
          set prin to lpt2
     case vlpt=3
          do case
             case nnr=1
                  set prin to ksotch1.txt
             case nnr=2
                  set prin to ksotch2.txt
             case nnr=3
                  set prin to ksotch3.txt
             case nnr=4
                  set prin to ksotch4.txt
             case nnr=5
                  set prin to ksotch5.txt
             case nnr=6
                  set prin to ksotch6.txt
          endc
  endc

  set cons off

  if !empty(gcPrn)
     ?chr(27)+'E'+chr(27)+'&l1h26a0O'+chr(27)+chr(27)+'(3R'+chr(27)+'(s0p21.00h0s0b4099T'+chr(27)  // Книжная А4
  else
     ?chr(27)+chr(77)+chr(15)
  endif

  set prin on
  sele doks
  kklr=99999999
  if netseek('t1','mnr')
     ?'Отчет по '+str(kopr,3)+' '+dtoc(ddcr)
     ?dtoc(date())+' '+time()
     ?'┌──────┬────────────┬────────────┬─┬──────┬───┐'
     ?'│'+' ТТН  '+'│'+'   СуммаК   '+'│'+'   СуммаТ   '+'│'+'П'+'│'+'  Чек '+'│'+'Снс'+'│'
     ?'├──────┼────────────┼────────────┼─┼──────┼───┤'
     ssdir=0
     sdvir=0
     do while mn=mnr
        if NChek=0
           skip
           loop
        endif
        rndr=rnd
        NChekr=NChek
        sncr=snc
        sele dokk
        if netseek('t1','mnr,rndr')
           do while mn=mnr.and.rnd=rndr
              DokkTtnr=DokkTtn
              if DokkTtnr=0
                 skip
                 loop
              endif
              kop_r=getfield('t1','DokkTtnr','rs1','kop')
              if kop_r#kopr
                 sele dokk
                 skip
                 loop
              endif
  //            sdvr=getfield('t1','DokkTtnr','rs1','sdv')
              sdvr=getfield('t1','DokkTtnr,90','rs3','ssf')
  //            ssf13r=getfield('t1','DokkTtnr,13','rs3','ssf')
              przr=getfield('t1','DokkTtnr','rs1','prz')
              ?' '+str(dokk->DokkTtn,6)+' '+str(dokk->bs_s,12,2)+' '+str(sdvr,12,2)+' '+str(przr,1)+' '+str(NChekr,6)+' '+str(sncr,3)
              if dokk->bs_s#sdvr
                 ??' *'
              endif
              ssdir=ssdir+dokk->bs_s
              sdvir=sdvir+sdvr //+ssf13r
              sele dokk
              skip
           enddo
        endif
        sele doks
        skip
     enddo
     ?'Итого'+space(3)+str(ssdir,12,2)+' '+str(sdvir,12,2)+' '+str(ssdir-sdvir,12,2)
  endif



  set cons on
  set prin off
  set prin to txt.txt
  if vlpt=3
     do case
        case nnr=1
             edfile('ksotch1.txt')
        case nnr=2
             edfile('ksotch2.txt')
        case nnr=3
             edfile('ksotch3.txt')
        case nnr=4
             edfile('ksotch4.txt')
        case nnr=5
             edfile('ksotch5.txt')
        case nnr=6
             edfile('ksotch6.txt')
     endc
  endif
  retu .t.

************
func kotkl()
  ************
  retu .t.

******************
func xzot(p1)
  ******************
  // p1  1 - x ; 2 -z
  if select('doks')=0
     netuse('doks')
     prdoksr=0
  else
     prdoksr=1
  endif
  if select('kassa')#0
     sele kassa
     if fieldpos('nzot')#0
        if p1=1 // x
           nxotr=nxot
           netrepl('nxot','nxot+1')
           sele doks
           if prdoksr=0
              go top
              do while !eof()
                 if NChek=0.or.nxot#0.or.nzot#0
                    skip
                    loop
                 endif
                 netrepl('nxot','nxotr')
                 skip
              enddo
           else
              if netseek('t1','mnr')
                 do while mn=mnr
                    if NChek=0.or.nxot#0.or.nzot#0
                       skip
                       loop
                    endif
                    netrepl('nxot','nxotr')
                    skip
                 enddo
              endif
           endif
        endif
        if p1=2 // z
           nzotr=nzot
           netrepl('nzot,artno','nzot+1,1')
           sele doks
           if prdoks=0
              go top
              do while !eof()
                 if NChek=0.or.nzot#0
                    skip
                    loop
                 endif
                 netrepl('nzot','nzotr')
                 skip
              enddo
           else
              if netseek('t1','mnr')
                 do while mn=mnr
                    if NChek=0.or.nzot#0
                       skip
                       loop
                    endif
                    netrepl('nzot','nzotr')
                    skip
                 enddo
              endif
           endif
        endif
     endif
  else
     wmess('Не открыта таблица KASSA',2)
  endif
  if prdoksr=0
     nuse('doks')
  endif
  retu .t.

************
func kem()
  ************
  store 0 to kecs_r,mrsh_r,kecsr,mrshr,mkemr
  dmrsh_r=ddcr
  dmrshr=ddcr
  clkem=setcolor('n/w,n/bg')
  wkem=wopen(8,20,13,60)
  wbox(1)
  do while .t.
     @ 0,1 say 'Экспедитор' get kecs_r pict '9999'
     @ 1,1 say 'Маршрут   ' get mrsh_r pict '999999'
     @ 2,1 say 'Дата      ' get dmrsh_r
     read
     fior=getfield('t1','kecs_r','s_tag','fio')
     @ 0,20 say alltrim(fior)
     if lastkey()=K_ESC
        exit
     endif
     @ 3,1 prom 'LPT1'
     @ 3,col()+1 prom 'LPT2'
     @ 3,col()+1 prom 'LPT3'
     @ 3,col()+1 prom 'Файл'
     menu to mkemr
     if lastkey()=K_ESC
        exit
     endif

     do case
        case mkemr=1
             set prin to lpt1
        case mkemr=2
             set prin to lpt2
        case mkemr=3
             set prin to lpt3
        case mkemr=4
             set prin to kem.txt
     endc

     set cons off

     if !empty(gcPrn)
        ?chr(27)+'E'+chr(27)+'&l1h26a0O'+chr(27)+chr(27)+'(3R'+chr(27)+'(s0p21.00h0s0b4099T'+chr(27)  // Книжная А4
     else
        ?chr(27)+chr(77)+chr(15)
     endif

     set prin on
     kems()
     set prin off
     set prin to txt.txt
     set cons on
     if mkemr=4
        wselect(0)
        edfile('kem.txt')
        wselect(wkem)
     endif
  enddo
  wclose(wkem)
  setcolor(clkem)
  retu .t.

*************
func kems()
  *************
  netuse('cmrsh')
  crtt('tkem','f:kecs c:n(4) f:mrsh c:n(6) f:ttn c:n(6) f:prz c:n(1) f:sdv c:n(12,2) f:necs c:c(30) f:dmrsh c:d(10)')
  sele 0
  use tkem excl
  inde on str(kecs,4)+str(mrsh,6)+str(ttn,6) tag t1
  if gnEntRm=0
     skr=228
  else
     do case
        case Rmsk=3
             skr=300
        case Rmsk=4
             skr=400
        case Rmsk=5
             skr=500
        case Rmsk=6
             skr=600
     endc
  endi
  sele cskl
  locate for sk=skr
  if foun()
     pathr=gcPath_d+alltrim(path)
     netuse('rs1',,,1)
     do while !eof()
        if kop#169
           sele rs1
           skip
           loop
        endif
        mrshr=mrsh
        if mrshr=0
           sele rs1
           skip
           loop
        endif
        if !netseek('t2','mrshr','cmrsh')
           sele rs1
           skip
           loop
        endif

        if mrsh_r#0
           if mrshr#mrsh_r
              sele rs1
              skip
              loop
           endif
        endif

        ttnr=ttn
        sele cmrsh
        if netseek('t2','mrshr')
           dmrshr=dmrsh
           przmr=prz
           if przmr#2
              sele rs1
              skip
              loop
           endif
           kecsr=kecs
           necsr=getfield('t1','kecsr','s_tag','fio')
        else
           sele rs1
           skip
           loop
        endif

        if mrsh_r=0
           if kecs_r#0
              if kecsr#kecs_r
                 sele rs1
                 skip
                 loop
              endif
              if !empty(dmrsh_r)
                 if dmrshr#dmrsh_r
                    sele rs1
                    skip
                    loop
                 endif
              else
                 if bom(dmrshr)#bom(ddcr)
                    sele rs1
                    skip
                    loop
                 endif
              endif
           else
              sele rs1
              skip
              loop
           endif
        endif

        sele rs1
        przr=prz
        sdvr=getfield('t1','ttnr,90','rs3','ssf')
  //      sdvr=sdv
        sele tkem
        appe blank
        repl kecs with kecsr,;
             mrsh with mrshr,;
             ttn with ttnr,;
             sdv with sdvr,;
             necs with necsr,;
             dmrsh with dmrshr
        sele rs1
        skip
     enddo
     nuse('rs1')
     sele tkem
     copy to kem
     sele 0
     use kem
     kecs_rr=9999
     mrsh_rr=999999
     store 0 to smer,smmr,smir
     ?'Отчет по 169 эксп/маршр '+dtoc(ddcr)
     ?dtoc(date())+' '+time()
     ?'┌──────┬────────────┬─┐'
     ?'│'+' ТТН  '+'│'+'   Сумма    '+'│'+'П'+'│'
     ?'├──────┼────────────┼─┤'
  *wait
     do while !eof()
        if kecs_rr#9999
           if kecs#kecs_rr
              ?'Итого по экспедитору '+str(smer,12,2)
              ?'>>Экспедитор'+' '+necs
              smer=0
              kecs_rr=kecs
           endif
        else
           if kecs#kecs_rr
              ?'>>Экспедитор'+' '+necs
              kecs_rr=kecs
          endif
        endif
        if mrsh_rr#999999
           if mrsh#mrsh_rr
              ?'Итого по маршруту    '+str(smmr,12,2)
              ?'>Маршрут'+' '+str(mrsh,6)+' '+dtoc(dmrsh)
              smmr=0
              mrsh_rr=mrsh
           endif
        else
           if mrsh#mrsh_rr
              ?'>Маршрут'+' '+str(mrsh,6)+' '+dtoc(dmrsh)
              mrsh_rr=mrsh
          endif
        endif
        ?' '+str(ttn,6)+' '+str(sdv,12,2)+' '+str(przr,1)
        smer=smer+sdv
        smmr=smmr+sdv
        smir=smir+sdv
        sele kem
        skip
     enddo
     ?'Итого по экспедитору '+str(smer,12,2)
     ?'Итого по маршруту    '+str(smmr,12,2)
     ?'Итого по отчету      '+str(smir,12,2)
     eject
  else
     wmess('Не найден склад',3)
  endif
  nuse('tkem')
  nuse('kem')
  nuse('cmrsh')
  erase tkem.dbf
  erase tkem.cdx
  retu .t.

****************************
func ksvz2()
  // Поиск возвратов на TTN
  ****************************
  local yyr,mmr,mm1r,mm2r
  if .t. //gnEnt#21
     retu .t.
  endif
  if select('ksvz2')#0
     sele ksvz2
     use
  endif
  ddcr=getfield('t1','DokkTtnr','rs1','ddc')
  crtt('ksvz2','f:ktl c:n(9) f:kf c:n(12,3) f:zen c:n(10,3)')
  sele 0
  use ksvz2
  if !empty(ddcr)
     for yyr=year(ddcr) to year(date())
         pathgr=gcPath_e+'g'+str(yyr,4)+'\'
         do case
            case year(ddcr)=year(date())
                 mm1r=month(ddcr)
                 mm2r=month(date())
            case yyr=year(ddcr)
                 mm1r=month(ddcr)
                 mm2r=12
            case yyr=year(date())
                 mm1r=1
                 mm2r=month(date())
            othe
                 mm1r=1
                 mm2r=12
        endc
        for mmr=mm1r to mm2r
            pathmr=pathgr+'m'+iif(mmr<10,'0'+str(mmr,1),str(mmr,2))+'\'
            sele cskl
            locate for sk=DokkSkr
            if foun()
               pathr=pathmr+alltrim(path)
               if !netfile('pr1',1)
                  loop
               endif
               netuse('pr1','pr1vz',,1)
               netuse('pr2','pr2vz',,1)
               sele pr1vz
               set orde to tag t7 // ttnvz,kop
               if netseek('t7','DokkTtnr,108')
                  do while ttnvz=DokkTtnr.and.kop=108
                     if prz=0
                        skip
                        loop
                     endif
                     mn_r=mn
                     sele pr2vz
                     if netseek('t1','mn_r')
                        do while mn=mn_r
                           ktlr=ktl
                           kfr=kf
                           zenr=ozen
                           sele ksvz2
                           locate for ktl=ktlr
                           if !foun()
                              netadd()
                              netrepl('ktl,zen','ktlr,zenr')
                           endif
                           netrepl('kf','kf+kfr')
                           sele pr2vz
                           skip
                        enddo
                     endif
                     sele pr1vz
                     skip
                  enddo
               endif
               nuse('pr1vz')
               nuse('pr2vz')
            endif
        next
     next
  endif
  retu .t.


/*****************************************************************
 
 FUNCTION:
 АВТОР..ДАТА..........С. Литовка  04-08-20 * 10:16:30pm
 НАЗНАЧЕНИЕ.........
 ПАРАМЕТРЫ..........
 ВОЗВР. ЗНАЧЕНИЕ....
 ПРИМЕЧАНИЯ.........
 */
STATIC FUNCTION NaT4Kass()
  If gnEnt = 21 // лодис
    Uktr:=getfield('t1','MnTovr','ctov','Ukt')
    If !Empty(Uktr)
      Uktr:=allt(Uktr)+"#"
    EndIf
    natr:=getfield('t1','MnTovr','ctov','nam')
    If Empty(natr)
      natr:=getfield('t1','MnTovr','ctov','nat')
    EndIf
    natr:=Uktr + natr

  Else
    natr=getfield('t1','MnTovr','ctov','nat')
  EndIf

  RETURN ( natr )
