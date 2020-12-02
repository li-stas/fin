/***********************************************************
 * �����    : ViewFileText.prg
 * �����    : 0.0
 * ����     :
 * ���      : 03/13/95
 * �������   :
 * �ਬ�砭��: ����� ��ࠡ�⠭ �⨫�⮩ CF ���ᨨ 2.02
 */

#include "fileio.ch"

//#define CR chr(13)
//#define LF chr(10)
//#define CRLF chr(13)+chr(10)

//������ ������ � 䠩��
#define FTELL(handle) fseek(handle, 0, FS_RELATIVE)

STATIC nHandle              //���ਯ�� ���뢠����� 䠩��
STATIC cLine                //������ ��ப�
//STATIC nFileSize            //������ 䠩��
STATIC nPos := 1

/***********************************************************
 * ViewFileText() -->
 *   ��ࠬ���� : ��� 䠩��
 *   �����頥�:
 */
FUNCTION ViewFileText(fname)
  LOCAL oTbr, oTbc ,cScr
  LOCAL nMAXROW, nMAXCOL, nSetColor
  nMAXROW:=MAXROW()-1
  nMAXCOL:=MAXCOL()
  nSetColor:=SETCOLOR(("BG+/B,BG+/B"))

  IF (lFileInit(fname))
    SAVE SCREEN TO cScr
    //CLEAR SCREEN
    @ 0, 0 CLEAR TO nMAXROW, nMAXCOL
    @ 0, 0 TO nMAXROW, nMAXCOL
    SETPOS(0, 1)
    //DISPOUT("[ "+fName+" ]", "RG+/B")
    oTbr := tbrowsenew(1, 1, nMAXROW-1, nMAXCOL-1)
    oTbc := tbColumnNew(, { || SUBSTR(cCurLine(), nPos) })
    oTbc:width := nMAXCOL-1 //78
    oTbr:addcolumn(oTbc)
    oTbr:colorSpec := "BG+/B,BG+/B"
    oTbr:goTopBlock := { || GetFirst() }
    oTbr:goBottomBlock := { || GetLast() }
    oTbr:skipBlock := { | n | skipper(n) }
    myBrowse(oTbr)
    CLEAR SCREEN
    RESTORE SCREEN FROM cScr
    SETCOLOR(nSetColor)

  ENDIF

  RETURN (NIL)

/***********************************************************
 * cCurLine() -->  ������ ⥪�饩 ��ப�
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION cCurLine
  RETURN (cLine)
#define MAX_LINE_LEN 256

/***********************************************************
 * lFileInit() -->
 *   ����ணࠬ�� ���樠����樨 - ����⨥ 䠩��, ��⠭����
 *   nFileSize
 *   � �ਥ� ��ࢮ� ��ப�
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION lFileInit(nFname)

  IF (nHandle := fopen(nFname, 0)) # 0
    fseek(nHandle, 0, FS_END)
    //nFileSize := FTELL(nHandle)
    GetFirst()
  ENDIF

  RETURN (nHandle != 0)

/***********************************************************
 * GetFirst() --> �⥭�� ��ࢮ� ��ப� � ������ �� � ��६����� cline
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION GetFirst
  fseek(nHandle, 0, FS_SET)
  lfreadln(nHandle, @cline)
  fseek(nHandle, 0, FS_SET)

  RETURN (NIL)

/***********************************************************
 * lfreadln() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION lfreadln(nHandle, cBuffer)

  LOCAL nEol, nRead, nSavePos

  cBuffer := space(MAX_LINE_LEN)

  //���砫� ��࠭�� ⥪�騩 㪠��⥫� 䠩��
  nSavePos = FTELL(nHandle)
  nRead = fread(nHandle, @cBuffer, MAX_LINE_LEN)

  IF (nEol := at(CRLF, substr(cBuffer, 1, nRead))) = 0
    //��९������� ��ப� ��� ����� 䠩�� (EOF)
  //Cline ᮤ�ন� ���������� ��� ��ப�
  ELSE
    //�����㥬 ������ �� ���� ��ப�
    cBuffer := substr(cBuffer, 1, nEol - 1)
    //��⠭�������� 㪠��⥫� 䠩�� �� ᫥������ ��ப�
    fseek(nHandle, nSavePos + nEol + 1, FS_SET)
  ENDIF

  RETURN (nRead != 0)

/***********************************************************
 * GetLast() -->
 * �⥭�� ��᫥���� ��ப� 䠩�� � ������ �� � ��६�����
 *  cline
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION GetLast

  fseek(nHandle, 0, FS_END)
  lGoPrevLn()

  RETURN (NIL)

/***********************************************************
 * skipper() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION skipper(n)
  //�ய�� n ��ப 䠩��. n ����� ���� ��� ������⥫��, ⠪
  //� ����⥫�� �᫮�

  LOCAL nSkipped := 0

  IF (n > 0)
    WHILE (nSkipped != n .AND. lGoNextLn())
      nSkipped++
    ENDDO

  ELSE
    WHILE (nSkipped != n .AND. lGoPrevLn())
      nSkipped--
    ENDDO

  ENDIF

  RETURN (nSkipped)

/***********************************************************
 * lGoNextLn() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION lGoNextLn()
  //��⠥��� ��३� � ᫥���饩 ��ப� 䠩��

  //�����頥� .T. �᫨ ����⪠ 㤠���� � .F. � ��⨢��� ��砥

  LOCAL nSavePos := FTELL(nHandle), cBuff := "", lMoved, nNewPos

  fseek(nHandle, len(cLine) + 2, FS_RELATIVE)
  nNewPos := FTELL(nHandle)
  IF (lfreadLn(nHandle, @cBuff))
    lMoved := .T.
    cLine := cBuff
    fseek(nHandle, nNewPos, FS_SET)
  ELSE
    lMoved := .F.
    fseek(nHandle, nSavePos, FS_SET)
  ENDIF

  RETURN (lMoved)

/***********************************************************
 * lGoPrevLn() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION lGoPrevLn()
  //��⠥��� ��३� � �।��饩 ��ப� 䠩��
  //�����頥� .T. �᫨ ����⪠ 㤠���� � .F. � ��⨢��� ��砥

  LOCAL nOrigPos := FTELL(nHandle), nMaxRead, nNewPos, ;
   lMoved, cBuff, nWhereCrLf, nPrev, cTemp
  cTemp := "  "
  IF (nOrigPos == 0)
    lMoved := .F.
  ELSE
    lMoved := .T.
    //�஢�ਬ CR/LF � �।���� ���� ᨬ�����
    fseek(nHandle, -2, FS_RELATIVE)
    fread(nHandle, @cTemp, 2)
    IF (cTemp == CRLF)
      fseek(nHandle, -2, FS_RELATIVE)
    ENDIF

    nMaxRead := MIN(MAX_LINE_LEN, FTELL(nHandle))
    cBuff := space(nMaxRead)
    nNewPos := fseek(nHandle, -nMaxRead, FS_RELATIVE)
    fread(nHandle, @cBuff, nMaxRead)
    nWhereCrLf := rat(CRLF, cBuff)
    IF (nWhereCrLf = 0)
      nPrev := nNewPos
      cLine := cBuff
    ELSE
      nPrev := nNewPos + nWhereCrLf + 1
      cline := substr(cBuff, nWhereCrLf + 2)
    ENDIF

    fseek(nHandle, nPrev, FS_SET)
  ENDIF

  RETURN (lMoved)

#include "inkey.ch"

/***********************************************************
 * myBrowse() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION myBrowse(oTbr)
//�� 㬮�砭�� �ࠢ����� ��室���� � ������ ����ணࠬ��,
//���� ���짮��⥫� �� ������ Esc � Enter

LOCAL nKey, lExitRequested := .F., bAction

WHILE (!lExitRequested)
  WHILE (nextkey() == 0 .AND. !oTbr:stabilize())
  ENDDO

  nKey := inkey(0)
  IF (!stdMeth(nKey, oTbr))
    DO CASE
    CASE (bAction := SETKEY(nKey)) != NIL
      EVAL(bAction, PROCNAME(), PROCLINE(), READVAR())
    CASE (nKey == K_ESC .OR. nKey == K_ENTER)
      lExitRequested := .T.

    ENDCASE

  ENDIF
ENDDO
  RETURN (NIL)

/***********************************************************
 * stdMeth() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION stdMeth(nKey, oTbr)

  LOCAL lKeyHandled := .T.

  DO CASE
  CASE  nKey == K_DOWN
     oTbr:down()
  CASE  nKey == K_UP
     oTbr:up()
CASE  nKey == K_PGDN
    oTbr:pagedown()
CASE  nKey == K_PGUP
     oTbr:pageup()
CASE  nKey == K_CTRL_PGUP
    oTbr:gotop()
CASE  nKey == K_CTRL_PGDN
    oTbr:gobottom()
  CASE  nKey == K_RIGHT
    oTbr:right()
    MovePos(nKey, oTbr)
  CASE (nKey == K_LEFT)
    oTbr:left()
    MovePos(nKey, oTbr)
CASE  nKey == K_HOME
    oTbr:home()
    MovePos(nKey, oTbr)
CASE  nKey == K_END
    oTbr:END()
    MovePos(nKey, oTbr)
CASE  nKey == K_CTRL_LEFT
    oTbr:panleft()
CASE  nKey == K_CTRL_RIGHT
    oTbr:panright()
CASE  nKey == K_CTRL_HOME
    oTbr:panhome()
CASE  nKey == K_CTRL_END
    oTbr:panend()
  OTHERWISE; lKeyHandled := .F.
  ENDCASE

  RETURN (lKeyHandled)

/***********************************************************
 * MovePos() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
STATIC FUNCTION MovePos(nKey, oTbr)
  DO CASE
  CASE (nKey == K_LEFT)
    IF (nPos > 1)
      nPos--
      oTbr:refreshAll()
    ENDIF

  CASE (nKey == K_RIGHT)
    IF (nPos < len(cCurLine()))
      nPos++
      oTbr:refreshAll()
    ENDIF

  CASE (nKey == K_HOME)
    nPos := 1
    oTbr:refreshAll()
  CASE (nKey == K_END)
    nPos := len(cCurLine())
    oTbr:refreshAll()
  ENDCASE

  RETURN (nPos)

