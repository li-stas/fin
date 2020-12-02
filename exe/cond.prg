/*****************************************************************
 FUNCTION: Condget(oGet)
 �����..............�. ��⮢��
 ����...............27.01.96 * 12:24:54
 ����������.........���� �����᪨� ���祭�� � ��������� ����� �஡��� ��� �㪢
 ���������.......... oGet
 �����. ��������.... Variable  ���� ��ꥪ�
 ����������......... �� RGEN ��뢠����  ��⥬ ����� @..CONDGET � #Include "CondGet.ch"
 */

#include "Getexit.ch"
#Include "Common.ch"
#Include "InKey.ch"

STATIC slUpdated := .F.

/***********************************************************
 * Condget() -->
 *   ��ࠬ���� :
 *   �����頥�:
 */
FUNCTION Condget(oGet)

  LOCAL vVar := oGet:Block
  LOCAL sStr := IIF(oGet:Varget(), "�� ", "���")

  oGet:Block := { | _bUndef |                                          ;
                  sStr:=IIF(EVAL(vVar) .AND. sStr == "���",      ;//
                             "�� ",                                 ;//
                             IIF(!EVAL(vVar) .AND. sStr == "�� ",;// �� ����୮� �室�
                                  "���",                            ;// �����஢�� ���祭��
                                  sStr                              ;// �. � . ��६����� GET ����� �������� ᢮� ���祭��
                               )                                   ;//
                          ),                                       ;//
                  IIF(PCOUNT() > 0,                                   ;
                       (EVAL(vVar, (sStr := _bUndef) == "�� ")), ;
                       NIL                                             ;
                    ), sStr                                           ;
                }
  oGet:Reader := { | _bUndef |Condreader(_bUndef) }
  IF (oGet:changed)
    slUpdated := .T.
  ENDIF
  oGet:Display()

  RETURN (oGet)

/***********************************************************
 * Condreader
 *   ��ࠬ����: oGet
 */
STATIC PROCEDURE Condreader(oGet)

  LOCAL vVar := oGet:Exitstate()
  LOCAL nNum, _Undef

  IF (Getprevali(oGet))

    oGet:Setfocus()

    WHILE (oGet:Exitstate() == GE_NOEXIT)

      WHILE (oGet:Exitstate() == GE_NOEXIT)

        IF (_Undef := SetKey(nNum := Inkey(0))) <> NIL

          Getdosetke(_Undef, oGet)
          LOOP

        ENDIF

        IF (nNum == K_SPACE)

          oGet:VarPut(IF(oGet:VarGet() <> "�� ", "�� ", "���"))
          oGet:changed := TRUE

        ELSEIF (CHR(nNum) $ "YyTt��")

          oGet:VarPut("�� ")
          oGet:changed := TRUE

        ELSEIF (CHR(nNum) $ "NnFf��")

          oGet:VarPut("���")
          oGet:changed := TRUE

        ELSEIF (nNum < 32 .OR. nNum > 255)

          GetApplyKey(oGet, nNum)

        ELSE

          LOOP

        ENDIF

        oGet:UpdateBuffer()

      ENDDO

      IF (!GetPostValidate(oGet))

        oGet:ExitState := GE_NOEXIT

      ENDIF

    ENDDO

    oGet:KillFocus()

  ENDIF

  RETURN

/***
*
*  Updated()
*
*/
FUNCTION UpdatedCondGet()
   RETURN slUpdated
