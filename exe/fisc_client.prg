#include "inkey.ch"
#include "common.ch"
#include "fisc.ch"
#define FISC_MAX_CONNECT 10
#define SERVER_ANSWER_MAXSIZE 8096
#define LF CHR( 10 )
#define CR CHR( 13 )

#ifdef TEST

LOCAL nBytes, cHost, nPort, nTimeOut
LOCAL nL
LOCAL nConnect,nFError, cFErrorStr
LOCAL ServerResponce

CLEAR SCREEN

cHost:="localhost"
nPort:=7654
nTimeOut:=60*10^3
nBytes:=1024

  QOUT("Test for fisc_client")

  nConnect := fisc_open_server( cHost, nPort, @ServerResponce, nTimeOut, SERVER_ANSWER_MAXSIZE )
        ?"nConnect",nConnect
        QOUT("�⢥�",ServerResponce)
  IF nConnect >= 0
    fisc_write_to_server(nConnect,;
    "FSTART: MARIA_NULL"+LF+"loginqwer"+LF+"1111111111"+LF+"FEND:"+LF+"")

    //�⠥�, �� ��諮 � ����
    nL:=fisc_read_from_server(nConnect,@ServerResponce,nTimeOut,SERVER_ANSWER_MAXSIZE)
    IF nL>=0

      IF VAL(ServerResponce)=0
        ?"nConnect",nConnect
        QOUT("�⢥�",ServerResponce)
      ELSE
        ?"nConnect",nConnect
        QOUT("�⢥�",ServerResponce,fisc_Error_Maria(ServerResponce))
      ENDIF

    ELSE
      //����஢뢥� �訡��
      @ MAXROW(), 0 SAY fisc_Error(nL)
    ENDIF

    fisc_close_server(nConnect)
    WAIT
    //RUN ("./mariq")
  ENDIF
#endif


/*****************************************************************
 
 FUNCTION: fisc_open_server()
 �����..����..........�. ��⮢��   03-04-05 * 00:23:23am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_open_server(cHost,nPort,cBuf,nTimeOut,nBytes)
  LOCAL nConnect
  LOCAL nFError, cFErrorStr, nL, i
  LOCAL aTypeModel

  nFError:=0
  aTypeModel:={}
  nConnect:=-1

  FOR i:=1 TO FISC_MAX_CONNECT
    nConnect:=TCPCONNECT(cHost,nPort,nTimeOut) //22) //7654)
    IF nConnect > -1
      //�������� � ���ᨢ ��騩 ᮥ������� aAllCon
      EXIT
    ELSE
      nFError:=FERROR()
      cFErrorStr:=FERRORSTR()
      cBuf:=ALLTRIM(STR(nFError))+","+cFErrorStr
      outlog(__FILE__,__LINE__,"Connect error to host",nFError,cFErrorStr)
      outlog(__FILE__,__LINE__,cHost,nPort,nTimeOut)
ENDIF
  NEXT
  IF nConnect > -1
    cBuf:=SPACE(nBytes,.T.)
    nL:=TCPREAD(nConnect,@cBuf,nBytes,nTimeOut)
    IF nL = -1
      nConnect := nL
      outlog(__FILE__,__LINE__,"TCPREAD","Read from host error",nL,nFError, cFErrorStr)
      return FISC_ERROR_READ_DATA
    ELSE
      cBuf:=LEFT(cBuf,nL)
      IF ASC(RIGHT(cBuf,1))<=13
        cBuf:=LEFT(cBuf,nL-1)
      ENDIF
//�஢�ਬ �� ���⠫�
      IF "OK" $ cBuf
        TOKENINIT(@cBuf," ")
        TOKENNEXT()
        DO WHILE !TOKENEND()
          AADD(aTypeModel,{nConnect,TOKENNEXT()})
        ENDDO
      ELSE
        nConnect:=-1
      ENDIF
    ENDIF
  ELSE
    return FISC_ERROR_CONNECT_HOST
  ENDIF

  RETURN ( nConnect )

/*****************************************************************
 
 FUNCTION: fisc_write_to_server
 �����..����..........�. ��⮢��  03-04-05 * 01:05:48am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_write_to_server(nConnect,cBuf)
  LOCAL nL
  //�஢�ઠ �� ���ᨬ��쭮� ������祭�� ��
  //����� ��ॣ���஢�� aAllCon
  IF !( nConnect >= 0)
    return FISC_ERROR_INVALID_HANDLE
  ELSE
    nL:=TCPWRITE( nConnect, cBuf )
    IF nL = -1
      outlog(3,__FILE__,__LINE__,"TCPWRITE",nL)
      return FISC_ERROR_SEND_HOST
    ELSE
      IF nL != LEN(cBuf)
        outlog(3,__FILE__,__LINE__,"Send to host error (data lost) ",nL)
        return FISC_ERROR_SEND_DATA
      ENDIF
    ENDIF
  ENDIF
  RETURN ( nL )

/*****************************************************************
 
 FUNCTION: fisc_read_from_server
 �����..����..........�. ��⮢��  03-04-05 * 01:29:29am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_read_from_server(nConnect,cBuf,nTimeOut,nBytes)
  LOCAL nL
  LOCAL nFError, cFErrorStr

  //�஢�ઠ �� ���ᨬ��쭮� ������祭�� ��
  //����� ��ॣ���஢�� aAllCon
  IF !( nConnect >= 0)
    return FISC_ERROR_INVALID_HANDLE
  ELSE

    cBuf:=SPACE(nBytes) //,.T.)
    nL:=TCPREAD(nConnect,@cBuf,nBytes,nTimeOut)

    IF nL = -1
      nFError   :=FERROR()
      cFErrorStr:=FERRORSTR()
      outlog(3,__FILE__,__LINE__,"TCPREAD","Read from host error",nL,nFError, cFErrorStr)
      return FISC_ERROR_READ_DATA
    ELSE
      cBuf:=LEFT(cBuf,nL)
      IF ASC(RIGHT(cBuf,1))<=13
        cBuf:=LEFT(cBuf,nL-1)
      ENDIF
    ENDIF

  ENDIF
  RETURN ( nL )

/*****************************************************************
 
 FUNCTION: fisc_close_server
 �����..����..........�. ��⮢��  03-04-05 * 01:44:17am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_close_server(nConnect)
  //�஢�ઠ �� ���ᨬ��쭮� ������祭�� ��
  //����� ��ॣ���஢�� aAllCon
  IF !( nConnect >= 0)
    return FISC_ERROR_INVALID_HANDLE
  ELSE
    fisc_write_to_server(nConnect,"FQUIT:"+LF)
    //㤠���� �� ���ᨢ� ���� ������祭�� aAllCon
  ENDIF
  RETURN ( NIL )

/*****************************************************************
 
 FUNCTION: fisc_Error
 �����..����..........�. ��⮢��  03-07-05 * 02:29:35pm
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_Error(nError)
  LOCAL afisc_Error, nElem
  afisc_Error:={;
  {FISC_ERROR_NONE              ,[��� �訡��                                               ]},;//   0
  {FISC_ERROR_HOST_NOT_FOUND    ,[��� �� ������                                           ]},;//  -1
  {FISC_ERROR_SERVICE_NOT_FOUND ,[��� ⠪��� ����� ����                                  ]},;//  -2
  {FISC_ERROR_CREATE_SOCKET     ,[�� ����� ᮧ���� ᮪��                                   ]},;//  -3
  {FISC_ERROR_CONNECT_HOST      ,[�� ����� ᮥ�������� � �ࢥ஬                          ]},;//  -4
  {FISC_ERROR_SEND_HOST         ,[�� ᬮ� ��᫠�� �����                                   ]},;//  -5
  {FISC_ERROR_SEND_DATA         ,[�� �६� ���뫪� ᮮ�饭�� ࠧ�ࢠ���� �⥢�� ᮥ�������]},;//  -6
  {FISC_ERROR_READ_DATA         ,[�� ᬮ� ����� �����                                  ]},;//  -7
  {FISC_ERROR_LARGE_BLOCK       ,[ࠧ��� ���� ��� �ਥ�� ������ ����� 祬 ����室���    ]},;//  -8
  {FISC_ERROR_INVALID_HANDLE    ,[�� ���� �ࢥ� ��� ���ࠢ���� ���ਯ��              ]},;//  -9
  {FISC_ERROR_NO_MORE_HANDLE    ,[�� ����⨨ �ࢥ� �����稫��� ���ਯ���             ]};// -10
  }

  IF !EMPTY(nElem:=ASCAN( afisc_Error, {|aElem| aElem[1] = nError }))
    outlog(__LINE__,__FILE__,nElem,afisc_Error[nElem,2])
    RETURN afisc_Error[nElem,2]
  ELSE
    outlog(__LINE__,__FILE__,nElem,[�訡�� �� ��।�����])
    RETURN [�訡�� �� ��।�����                                     ]
  ENDIF
  RETURN ( "" )

/*****************************************************************
 
 FUNCTION: fisc_Error_Maria
 �����..����..........�. ��⮢��  03-08-05 * 03:25:39am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_Error_Maria(xBuf,cCmd)
  LOCAL afisc_Error_Maria, nElem, cBuf
  cBuf:=xBuf
  afisc_Error_Maria:=;
  {;
    {41, [HARDPAPER: ��i�稫��� 祪��� ��� ����஫쭠 ���i窠]},;
    {42, [HARDSENSOR: ���稪 �ࠩ�쮣� ��������� ������� ������� �� �����]},;
    {43, [HARDPOINT: �����i���� ������� ������� �� �����]},;
    {44, [HARDTXD: ������� �i�i� ��'離� (����஫� ��୮��i)]},;
    {45, [HARDTIMER: ���ਯ��⨬� ���祭�� ���]},;
    {46, [HARDMEMORY: ������� ������ �� �i᪠�쭮� ���'��i]},;
    {47, [HARDLCD: ���ࠢ��� ��ᯫ��]},;
    {48, [SHUTDOWN: ����� ��������� � ��i筨� ��稭]},;
    {49, [SOFTBLOCK: �������� ���� �����]},;
    {50, [SOFTNREP: �����i��� ������� Z-��i�]},;
    {51, [SOFTSYSLOCK: ���ਯ��⨬� ��������� ���� "�I��������"]},;
    {52, [SOFTCOMMAN: ���i���� �������]},;
    {53, [SOFTPROTOC: ������� �� �i����i��� ��⮪���]},;
    {54, [SOFTZREPOR: Z-��i� �� ��ମ����� �१ ����i�]},;
    {55, [SOFTMFULL: ��९������� �i᪠�쭮� ���'��i]},;
    {56, [SOFTTIMER: ���ਯ��⨬�� ��: ���� ��⠭�쮣� Z-��i��]},;
    {57, [SOFTPARAM: ���ਯ��⨬�� ��ࠬ��� �������]},;
    {58, [SOFTUPAS: �����i��� ������i� ����]},;
    {59, [SOFTCHECK: ���i�i ���祭�� ��ࠬ���i�]},;
    {60, [SOFTFACT: �� �������� ������i���� �����]},;
    {61, [SOFTSLWORK: �����i��� ��⠭���� ���� � ��������� "������"]},;
    {62, [SOFTSLPROG: �����i��� ��⠭���� ���� � ��������� "�������������"]},;
    {63, [SOFTSLZREP: �����i��� ��⠭���� ���� � ��������� "X-��I�"]},;
    {64, [SOFTSLNREP: �����i��� ��⠭���� ���� � ��������� "Z-��I�"]},;
    {65, [SOFTREPL: ���祭�� ��� ���ணࠬ�����]},;
    {66, [SOFTOVER: ��९������� ஧�來���i]},;
    {67, [SOFTNEED: ���ਯ��⨬�� १���� ��ਣ㢠��� ���i����� ������]},;
    {68, [SOFTACTIVE: ����⨢�i ��� ��� १����]},;
    {69, [SOFTFMTEST: ��誮����� �᭮��i �i᪠��i ४�i���]},;
    {70, [SOFTOPTEST: ��誮����� ����i �i᪠��i ���i]},;
    {71, [SOFT24HOUR: ����� �த��������� �i��� 24 �����]},;
    {72, [HARDUCCLOW: ���쪠 ����㣠 ��������]},;
    {73, [HARDCUTTER: ���ࠢ�i��� ������ �i��i����� ���i窨]},;
    {74, [HARDPCONTR: ���ࠢ�i��� ����஫�� ��'離� � ���]},;
    {75, [SOFTDIFART: �������쭠 �஡� ��९ணࠬ㢠��� ��⨪㫠]},;
    {76, [SOFTBADART: ���ਯ��⨬�� ����� ��⨪㫠]},;
    {77, [SOFTCOPY: ��������� 祪]},;
    {78, [SOFTOVART: ������� ������ FISC/ARFI ��� 祪�]},;
    {79, [SOFTNOTAV: ������� ��⨢i���i� ���]},;
    {80, [SOFTBADDISC: �㬠 ������ �i��� �� ��� ������]},;
    {81, [SOFTINUSE  : ����।�� ���������� �� �����襭�]},;
    {82, [SOFTOVPIST: ��ॢ�饭� �i��i��� ��⨢i������� ���]},;
    {83, [SOFTBADCS: ���iୠ ����஫쭠 �㬠]},;
    {84, [SOFTARTMODE: ���ਯ��⨬� �����i� ��� ������ ०��� ��⨪�쭮� ⠡���i]},;
    {85, [SOFTTHPAS: ���i୨� ��஫� �㭪�i� �孮���i筮�� �஫���]};
  }
  IF VALTYPE(xBuf)="C"
    xBuf:=VAL(ALLTRIM(xBuf))
  ENDIF
  IF !EMPTY(nElem:=ASCAN( afisc_Error_Maria, {|aElem| aElem[1] = xBuf }))
    outlog(__LINE__,__FILE__,nElem,afisc_Error_Maria[nElem,2],cCmd)
    RETURN afisc_Error_Maria[nElem,2]
  ELSE
    outlog(__LINE__,__FILE__,nElem,[�訡�� �� ��।�����]+","+XTOC(cBuf),cCmd)
    RETURN [�訡�� �� ��।����� ]+","+XTOC(cBuf)
  ENDIF
  RETURN ( NIL )

/*****************************************************************
 
 FUNCTION: fisc_cmd
 �����..����..........�. ��⮢��  03-12-05 * 08:08:03pm
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION fisc_cmd( nConnect, ServerResponce, cCmd, nTimeOut, nBytes, cHost,nPort, lShowError, nRowShow, nColShow)
  LOCAL  nL
  DEFAULT lShowError TO TRUE, nRowShow TO MAXROW()-1, nColShow TO 0

  nL:=fisc_write_to_server(nConnect,cCmd)
  //outlog(__FILE__,__LINE__,nl,cCmd)
  IF nL >= 0

    nL:=fisc_read_from_server(nConnect,@ServerResponce,nTimeOut,nBytes)
    //outlog(__FILE__,__LINE__,nl,ServerResponce)

    IF nL >= 0
      IF LEFT(ServerResponce,1)="0"
        nL:=0
        IF lShowError
          @ nRowShow, 0 SAY PADR("",MAXCOL()) COLOR "GR+/W"
        ENDIF
      ELSE
        IF LEFT(ServerResponce,2)="-2"
          outlog(__FILE__,__LINE__,nl,ServerResponce)
          //���⠭���� �����祭��
          fisc_close_server(nConnect)
          nConnect:=fisc_open_server(cHost,nPort,@ServerResponce,nTimeOut,nBytes)
          IF ( !EMPTY( nConnect ) )
            @ nRowShow, nColShow SAY PADR( ServerResponce, MAXCOL() ) COLOR "N/W"
          ELSE
            @ nRowShow, nColShow SAY PADR( ServerResponce, MAXCOL() ) COLOR "W/R,N/W"
          ENDIF
          outlog(__FILE__,__LINE__,nl,ServerResponce)
        ELSE
          IF lShowError
            @ nRowShow, nColShow SAY PADR(ServerResponce+":"+fisc_Error_Maria(ServerResponce,cCmd),MAXCOL()) COLOR "GR+/W"
          ENDIF
        ENDIF
      ENDIF
    ELSE
      IF lShowError
        //����஢뢥� �訡��
        @ nRowShow, nColShow SAY PADR(fisc_Error(nL,cCmd),MAXCOL()) COLOR "W/R"
      ENDIF
    ENDIF

  ELSE
    IF lShowError
      //����஢뢥� �訡��
      @ nRowShow, nColShow SAY PADR(fisc_Error(nL,cCmd),MAXCOL()) COLOR "W/R"
    ENDIF
  ENDIF

  RETURN ( nL )
/*****************************************************************
 
 FUNCTION:
 �����..����..........�. ��⮢��  03-13-05 * 09:22:41am
 ����������.........
 ���������..........
 �����. ��������....
 ����������.........
 */
FUNCTION MariaQ( nConnect, cCashMen, cMariaPasswd, nTimeOut, scHost, snPort )
  LOCAL aChoice, aChoiceLog, nChoice, nVibor
  LOCAL cScreen, cScr1
  LOCAL y1
  LOCAL nScrollType, nSumInOut, tTimeReal
  LOCAL dDataEnd,dDataBeg
  LOCAL nNumBeg, nNumEnd

  nScrollType:=0

  SET COLOR TO G/N, GR+/N,,, +G/N
  SAVE SCREEN TO cScreen
  CLEAR SCREEN

  aChoice:={                                              ;
             "1. �������� ���⮩ 祪",                  ;
             "2. ����� ��᫥����� 祪�",                  ;
             "3. �ண���� 祪���� �����",                 ;
             "4. ��㦥��� ����/�뢮� ����筮��",        ;
             " ",                                         ;
             "5. ������� ���� ��� ���㫥��� (�-����)",  ;
             "6. ��ਮ���᪨� ����� (���� ��� �����)", ;
             "7. ������� ���� � ���㫥���� (Z-����)",   ;
             " ",                                         ;
             "8. ������� ���� � �६�",                  ;
             " ",                                         ;
             "9. �������樮���� ���ଠ�� � ��������", ;
             "0. ��ନ஢���� ���譥�� ���� 祪�",        ;
             "A. ���ணࠬ��஢��� �����",                ;
             "B. ��ࠪ��᪠ ���⭮� ������",          ;
             "C. ������� ��� ॠ�쭮�� �६���"         ;
           }
  aChoiceLog:={}
  AEVAL( aChoice, { || AADD( aChoiceLog, TRUE ) } )
  WHILE ( TRUE )
    aChoiceLog[ 5 ]:=FALSE
    aChoiceLog[ 9 ]:=FALSE
    aChoiceLog[ 11 ]:=FALSE

    y1:=1
    DispBox( y1, 3, y1+17, 47, "BG+/N", 2 )
    @ y1, 3+1 SAY "[ �ணࠬ��஢���� ���� ]" COLOR "+BG/N"
    nChoice:=ACHOICE( y1+1, 3+2, y1+16, 46, aChoice, aChoiceLog )
    //#endif

    SAVE SCREEN TO cScr1

    DO CASE
    CASE ( nChoice = 0 )
      RESTORE SCREEN FROM cScreen
      EXIT
    CASE ( nChoice = 1 )
      //"1. �������� ���⮩ 祪",;
      IF ( fisc_cmd( @nConnect,,                       ;
                     "FSTART: MARIA_NULL"+LF+        ;
                     cCashMen+LF+         ;
                     cMariaPasswd+LF+                  ;
                     "FEND:"+LF,                     ;
                     nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                   )#0                               ;
         )
        ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
      ENDIF
      CLEAR TYPEAHEAD
    CASE ( nChoice = 2 )
      // "2. ����� ��᫥����� 祪�",                 ;
      IF ( fisc_cmd( @nConnect,,                       ;
                     "FSTART: MARIA_COPY"+LF+        ;
                     cCashMen+LF+         ;
                     cMariaPasswd+LF+                  ;
                     "FEND:"+LF,                     ;
                     nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                   )#0                               ;
         )
        ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
      ENDIF
      CLEAR TYPEAHEAD

    CASE ( nChoice = 3 )
      //"3. �ண���� 祪���� �����",;
      //SET COLOR TO "G/N, N/W"
      @ y1+16+2, 3+1 SAY "�ண���� 祪���� ����� ��"         ;
       GET nScrollType PICT "99" COLOR "G/N, N/W" RANG 0, 30
      @ y1+16+2, COL()+1 SAY "��"
      SET CURSOR ON; READ; SET CURSOR OFF
      IF ( LASTKEY()=K_ENTER .AND. nScrollType # 0 )
        IF ( fisc_cmd( @nConnect,,                            ;
                       "FSTART: MARIA_LINEFEED"+LF+         ;
                       cCashMen+LF+              ;
                       cMariaPasswd+LF+                       ;
                       LTRIM( STR( nScrollType*8, 3 ) )+LF+ ;
                       "FEND:"+LF,                          ;
                       nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort      ;
                     )#0                                    ;
           )
          ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
        ENDIF

      ENDIF

    CASE ( nChoice = 4 )
      //"4. ��㦥��� ����/�뢮� ����筮��",;
      nSumInOut:=0
      @ y1+16+2, 3+1 SAY "������ �㬬�"               ;
       GET nSumInOut PICT "999999.99" COLOR "G/N, N/W"
      @ y1+16+2, COL()+1 SAY " ������⥫쭠� - ��������, ����⥫쭠� - �������"
      SET CURSOR ON; READ; SET CURSOR OFF
      IF ( LASTKEY()=K_ENTER .AND. nSumInOut # 0 )
        IF ( fisc_cmd( @nConnect,,                       ;
                       "FSTART: MARIA_SLUG"+LF+        ;
                       cCashMen+LF+         ;
                       cMariaPasswd+LF+                  ;
                       LTRIM( STR( nSumInOut ) )+LF+   ;
                       "FEND:"+LF,                     ;
                       nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                     )#0                               ;
           )
          ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
        ENDIF

      ENDIF

    CASE ( nChoice = 5 )
    //  " ",;
    CASE ( nChoice = 6 )
      //"5. ������� ���� ��� ���㫥��� (�-����)",;
      IF ( fisc_cmd( @nConnect,,                       ;
                     "FSTART: MARIA_DAY_REPORT"+LF+  ;
                     cCashMen+LF+         ;
                     cMariaPasswd+LF+                  ;
                     "0"+LF+                         ;
                     "FEND:"+LF,                     ;
                     nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                   )#0                               ;
         )
        ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
      ENDIF

    CASE ( nChoice = 7 )
    //"6. ���� �� 㪠����� ��ਮ�",;
      nVibor:=ALERT(                                         ;
                     "��ਮ���᪨� ����� (���� ��� �����)", ;
                     { "�� ��⠬", "�� ����ࠬ" }, "N/W,GR+/BG" ;
                   )
      DO CASE
      CASE nVibor = 1
        dDataBeg:=BOM(DATE())
        dDataEnd:=EOM(DATE())
        @ y1+16+2, 3+1 SAY "��� ��砫� ��ਮ��"         ;
         GET dDataBeg COLOR "G/N, N/W" VALID !EMPTY(dDataBeg)
        @ y1+16+3, 3+1 SAY "���  ���� ��ਮ��"         ;
         GET dDataEnd COLOR "G/N, N/W" VALID !EMPTY(dDataEnd) .AND. dDataEnd>=dDataBeg
        SET CURSOR ON; READ; SET CURSOR OFF

        IF ( LASTKEY()=K_ENTER  )
          IF ( fisc_cmd( @nConnect,,                       ;
                         "FSTART: MARIA_DATE_REPORT"+LF+  ;
                         cCashMen+LF+         ;
                         cMariaPasswd+LF+                  ;
                         LTRIM(STR(  DAY(dDataBeg),2))+LF+ ;
                         LTRIM(STR(MONTH(dDataBeg),2))+LF+ ;
                         LTRIM(STR( YEAR(dDataBeg),4))+LF+ ;
                         LTRIM(STR(  DAY(dDataEnd),2))+LF+ ;
                         LTRIM(STR(MONTH(dDataEnd),2))+LF+ ;
                         LTRIM(STR( YEAR(dDataEnd),4))+LF+ ;
                         "FEND:"+LF,                     ;
                         nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                       )#0                               ;
             )
            ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
          ENDIF
        ENDIF
      CASE nVibor = 2
        nNumBeg:=0
        nNumEnd:=1
        @ y1+16+2, 3+1 SAY "����� ��砫쭮�� ����"         ;
         GET nNumBeg COLOR "G/N, N/W" PICT "@K 9999" VALID !EMPTY(nNumBeg) RANGE 0,9999
        @ y1+16+3, 3+1 SAY "����� ����筮�� ����"         ;
         GET nNumEnd COLOR "G/N, N/W" PICT "@K 9999" VALID !EMPTY(nNumBeg) .AND. nNumEnd>=nNumBeg RANGE 0,9999
        SET CURSOR ON; READ; SET CURSOR OFF

        IF ( LASTKEY()=K_ENTER  )
          IF ( fisc_cmd( @nConnect,,                       ;
                         "FSTART: MARIA_NUM_REPORT"+LF+  ;
                         cCashMen+LF+         ;
                         cMariaPasswd+LF+                  ;
                         LTRIM(STR(nNumBeg,4))+LF+ ;
                         LTRIM(STR(nNumEnd,4))+LF+ ;
                         "FEND:"+LF,                     ;
                         nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                       )#0                               ;
             )
            ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
          ENDIF
        ENDIF
      OTHERWISE

      ENDCASE

    CASE ( nChoice = 8 )
      //"7. ������� ���� � ���㫥���� (Z-����)",;
      nVibor:=ALERT(                                         ;
                     "������� ���� � ���㫥���� (Z-����)", ;
                     { "�⬥����", "���㫨��" }, "GR+/R,N/W" ;
                   )
      IF ( nVibor = 2 )
        IF ( fisc_cmd( @nConnect,,                       ;
                       "FSTART: MARIA_DAY_REPORT"+LF+  ;
                       cCashMen+LF+         ;
                       cMariaPasswd+LF+                  ;
                       "1"+LF+                         ;
                       "FEND:"+LF,                     ;
                       nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                     )#0                               ;
           )
          ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
          FCREATE('MARIA_DAY_REPORT.txt')  
        ENDIF

      ENDIF

    CASE ( nChoice = 9 )
    //" ",;
    CASE ( nChoice = 10 )
    //"8. ������� ���� � �६�",;
    CASE ( nChoice = 11 )
    //" ",;
    CASE ( nChoice = 12 )
    //"9. �������樮���� ���ଠ�� � ��������",;
    CASE ( nChoice = 13 )
    //"0. ��ନ஢���� ���譥�� ���� 祪�",;
    CASE ( nChoice = 14 )
    //"A. ���ணࠬ��஢��� �����",;
    CASE ( nChoice = 15 )
    //"B. ��ࠪ��᪠ ���⭮� ������",;
    CASE ( nChoice = 16 )
    //"C. ������� ��� ॠ�쭮�� �६���";
    tTimeReal:=TIME()
      @ y1+16+2, 3+1 SAY "������ ⥪�騥 �६� ��:��:��"         ;
       GET tTimeReal PICT "99:99:99" COLOR "G/N, N/W" VALID TIMEVALID( tTimeReal )
      SET CURSOR ON; READ; SET CURSOR OFF

      IF ( LASTKEY()=K_ENTER  )
      nVibor:=ALERT(                                         ;
                     "������� ⥪�� �६�?;"+;
                     "��������!;"+;
                     "������ �������� �믮����� 1 ࠧ ��⪨ ��᫥ Z-����;"+;
                     "��������� �६��� ����� ��⠢���� �� ����� 祬 +-90 ���.", ;
                     { "�⬥����", "�������" }, "GR+/R,N/W" ;
                   )
      IF ( nVibor = 2 )
        IF ( fisc_cmd( @nConnect,,                       ;
                       "FSTART: MARIA_TIME"+LF+  ;
                       cCashMen+LF+         ;
                       cMariaPasswd+LF+                  ;
                       SUBSTR(tTimeReal,1,2)+LF+       ;//���
                       SUBSTR(tTimeReal,4,2)+LF+       ;//������
                       SUBSTR(tTimeReal,7,2)+LF+       ;//ᥪ㭤�
                       "FEND:"+LF,                     ;
                       nTimeOut, SERVER_ANSWER_MAXSIZE, scHost, snPort ;
                     )#0                               ;
           )
          ALERT( "�訡�� �믮������ �������", { "��� �த������� ������ ������� Enter..." }, "GR+/R,N/W" )
        ENDIF
        ENDIF

      ENDIF
    ENDCASE

    RESTORE SCREEN FROM cScr1

  ENDDO

  RETURN ( NIL )

