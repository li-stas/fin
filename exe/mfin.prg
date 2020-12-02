#include "inkey.ch"
*********************************************
   if lastkey()=13
      setcolor("g/n,n/g,,,")
      do case
         case i=3 //.and.(gnAdm=1.or.gnEnt=21.and.DkKlnr=1.or.gnEnt#21)  // .and. (gnAdm=1 .or. DkKlnr=1)
            do case
               case pozicion=1
                    s_bs()
               case pozicion=2
                    s_DkKln()
               case pozicion=3
                    s_operb()
               case pozicion=4
                    s_tipd()
               case pozicion=5
                    s_soper()
               case pozicion=6
                    s_dclr()
               case pozicion=7
                    s_podr()
               case pozicion=8
                    s_zatr()
               case pozicion=9
                    s_izder()
               case pozicion=10
                    s_tara()
               case pozicion=11
                    s_kln()
               case pozicion=12
                    s_lic()
               case pozicion=13
                    s_sgrp()
               case pozicion=14
                    s_lice()
               case pozicion=15
                    s_vo()
               case pozicion=16
                    s_vop()
               case pozicion=17
                    s_tcen()
               case pozicion=18
                    s_banks()
               case pozicion=19
                    s_otd()
               case pozicion=20
                    mfochk()  // Счета в банках
               case pozicion=21
                    fop()     // Формы оплаты
               case pozicion=22
                    vdnal()   // Тип документа s_tcen.prg
            endcase

         case i=4
            do case
              case pozicion=1   //Касса
                IF inikassa()
                  adokz(1)
                  nuse()
                ELSE
                  adokz(1)
                ENDIF
              case pozicion=2    //Касса сервис
                IF inikassa()
                  #ifdef __CLIP__
                  MariaQ(gaKassa:nConnect, gaKassa:scCashMen, gaKassa:MRA_PASSWD, gaKassa:snTimeOut,;
                  gaKassa:Host, gaKassa:Ports)
                  If file('MARIA_DAY_REPORT.txt')
                    //////// удалить файл продаж  ///////
                    if !(dirchange('fiscrptday')=0)
                      dirmake('fiscrptday')
                    endif
                    dirchange(gcPath_l)
                    filedelete('fiscrptday\'+'MARIA_PRCHECK.txt')
                    filedelete('MARIA_DAY_REPORT.txt')
                    ///////// end ////////////////////////
                  EndIf
                  #endif
                  nuse()
                endif
              case pozicion=3   //Банк
                    adokz(2)
              case pozicion=4   //Все
                    adokz(3)
              case pozicion=5   //Передача
                   if gnAdm=1.or.gnRrm=1
                      rmmain(1)
                   endif
              case pozicion=6   //Прием
                   if gnAdm=1.or.gnRrm=1
                      rmmain(2)
                   endif
               case pozicion=7
                  prdzo()
               case pozicion=8
                  ksfin() // Контрольная сумма crDkKln.prg
               case pozicion=9
                  rmprot() // Протокол rmsdrc
               case pozicion=10
                   kklopl() // odkp
            endcase

         case i=5
            do case
               case pozicion=1
                  order()
               case pozicion=2
                  dk_bs()
               case pozicion=3
                  bl_s()
               case pozicion=4
                  reestr()
               case pozicion=5
                  oborkl()
               case pozicion=6
                  regord()
               case pozicion=7
                  glav()
               case pozicion=8 // Взаимозачет(204001) reestr.prg
                  vz204()
            endcase

         case i=6
            do case
               case pozicion=1
                    os_ob()
               case pozicion=2
                    odkp()
               case pozicion=3
                    vbanks()
               case pozicion=4
                    odkpg()
               case pozicion=5 // Просмотр проводок prd.prg
                    ShowProv()
               case pozicion=6 // MDALL
                    shmdall()  // glav.prg
               case pozicion=7 // НН
                    EntNn()  // glav.prg  НН выданные
               case pozicion=8 // НН полученные
                    EntNnP()  // prd.prg
               case pozicion=9.and.gnEnt=21 // Формы 1-оа,1-ра
                   oapa() // prd.prg "Формы 1-OA,1-PA"
            endcase

         case i=7
            do case
               case pozicion=1
                    bindx()
               case pozicion=2
                   If Who = 3
                      if alltrim(uprlr)=gcSprl.or.gnEnt=5
                         vbr=0
                         avb:={'Да','Нет'}
                         vbr=alert('Это переворот месяца.Вы уверены?',avb)
                         if vbr=1
                            rost()
                         endif
                      endif
                   EndIf
               case pozicion=3
                    if gnAdm=1
                       delstorn()   // (crbsdokk.prg)
                    endif
*                 PereBsZap2()
               case pozicion=4
                    dokkkta()
               case pozicion=5
                    crbsdokk()
               case pozicion=6
                    CrDkKln() //BSт DkKlnнт
               case pozicion=7
                    crdokz()
               case pozicion=8
                    chkdokko() // dokkkta
               case pozicion=9
                    dokktest()
               case pozicion=10
                    nDkKln() // переност остатков с указанного начального переиода
               case pozicion=11
   //                    if gnAdm=1.or.gnKto=782
                       CrOpl() // zdocall
   //                  zzdoc()
   //                  endif
               case pozicion=12 // DkKln пер
                    if gnAdm=1
                       CrKlnPrd() // crDkKln
                    endif
               case pozicion=13
                    if gnAdm=1
                       crmoddoc() // libfcn
                    endif
               case pozicion=14
                    if gnAdm=1
                       crdokk3() // crDkKln
                    endif
               case pozicion=15
                    if gnAdm=1
                       crdknap() // gnadm
                    endif
               case pozicion=16
                    if gnAdm=1
                      //CrDKKln361()
                      wmess('CrDKKln361() в разработае')
                    endif
            endcase

      endcase
      keyboard chr(5)
   endi
   return

*******************
static Function inikassa()
  LOCAL ServerResponce, lRetu
  #ifdef __CLIP__
     if !empty(gaKassa)
        return .t.
     endif
  #endif
  netuse('kassa')
  if fieldpos('nzot')#0
     if nzot=0
        netrepl('nzot,nxot','1,1')
     endif
  endif
  netuse('kassae')
  #ifdef __CLIP__
     locate for ALLTRIM(gcNNETNAME)=ALLTRIM(RemoteHost) .OR. ALLTRIM(gcNNETNAME)=ALLTRIM(SSH_CLIENT)
  #else
     gnKassa=1
  #endif
  IF FOUND()
     gnKassa=kassa
     #ifdef __CLIP__
       gaKassa:=dbRead()
  **********************
       sele kassa
       locate for kassa=gnKassa
       gaKassa:Host=host
       gaKassa:Ports=ports
       gaKassa:Konds=konds
       /*
       *if gnEnt=21
       *   wmess(host+' '+str(ports,5))
       *endif
       */
  **********************
       cRes_Buf:=""
       cErr_Buf:=""

       #ifdef GREP_USER
         SYSCMD("/bin/cat /etc/passwd |grep $USER", "", @cRes_Buf, @cErr_Buf)
         gaKassa:scCashMen:=TOKEN(cRes_Buf,":",5)
         gaKassa:scCashMen:=LEFT(gaKassa:scCashMen,5)
       #else
         gaKassa:scCashMen:=UPPER(gcName)
       #endif

       gaKassa:MRA_PASSWD:= "1111111111"
       gaKassa:snTimeOut:=60000
       gaKassa:SERVER_ANSWER_MAXSIZE:=8096
       gaKassa:Host:=gaKassa:Host

       gaKassa:nConnect := ;
       fisc_open_server(gaKassa:Host, gaKassa:Ports, @ServerResponce, gaKassa:snTimeOut, gaKassa:SERVER_ANSWER_MAXSIZE)
       /*
       *if gnEnt=21
       *   wmess(host+' '+str(ports,5)+' '+gaKassa:Ports)
       *endif
       */
       IF (!EMPTY(gaKassa:nConnect))
          @ MAXROW()-1, 0 SAY PADR(XTOC(ServerResponce)+" "+;
          XTOC(gaKassa:Host)+" "+ALLTRIM(gaKassa:Ports)+" "+gaKassa:scCashMen, MAXCOL()) COLOR "N/W"
          inkey(3)
          lRetu:= .t.
       ELSE
          gaKassa:nConnect:=-999
          @ MAXROW()-1, 0 SAY PADR(ServerResponce, MAXCOL()) COLOR "W/R,N/W"
          inkey(0)
          lRetu:= .f.
       ENDIF
       outlog(__FILE__,__LINE__,gaKassa:Host, gaKassa:Ports,gaKassa:scCashMen,gaKassa:nConnect)
       outlog(__FILE__,__LINE__,ServerResponce)
  //     inkey(0)
     #endif
  ELSE
    /*
     *     nuse('kassa')
     *     nuse('kassae')
     *if gnEnt=21
     *   wmess('Нет кассы')
     *endif
     */
     #ifdef __CLIP__
       outlog(__FILE__,__LINE__,"нет кассы для этого рабочего места",gcNNETNAME)
     #endif
    lRetu:= .f.
  ENDIF
  return lRetu
