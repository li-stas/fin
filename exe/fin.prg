para exer
clea
if empty(exer)
   ?'������� START.BAT'
   inkey(3)
   retu
endif
if upper(exer)#'S'
   ?'������� START.BAT'
   inkey(3)
   retu
endif


gnArm=2

priv menu[7]
MENU[3] := "��ࠢ�筨��"
MENU[4] := "�����⮢��"
MENU[5] := "�����"
MENU[6] := "���⪨"
MENU[7] := "��ࢨ�"

lmenur=len(menu)  // ������⢮ pad
priv CL_L[lmenur]  // ������ pad
priv sizam[lmenur] // ������⢮ bar


priv menu3[22]
MENU3[1]   := "���        "
MENU3[2]   := "����.�����⮢"
MENU3[3]   := "���.�஢���� "
MENU3[4]   := "���� ����.  "
*MENU3[5]   := "������       "
MENU3[5]   := "���.�஢���� "
MENU3[6]   := "���.���� ����"
MENU3[7]   := "���ࠧ�������"
MENU3[8]   := "����.�����  "
MENU3[9]  := "����প�     "
MENU3[10]  := "�����.��   "
MENU3[11]  := "�������      "
MENU3[12]  := "��業���     "
MENU3[13]  := "��㯯� ⮢��"
MENU3[14]  := "���� ��業���"
MENU3[15]  := "����.��室  "
MENU3[16]  := "����.��室  "
MENU3[17]  := "���� 業     "
MENU3[18]  := "�����        "
MENU3[19]  := "�⤥��"
MENU3[20]  := "��� � ����."
MENU3[21]  := "���� ������ "
MENU3[22]  := "��� ���㬥��"

priv menu4[10]
MENU4[1] := "���� 301XXX     "
MENU4[2] := "���� �ࢨ�     "
MENU4[3] := "����  311XXX     "
MENU4[4] := "�� ���        "
MENU4[5] := "��।��         "
MENU4[6] := "�ਥ�            "
MENU4[7] := "����/��� ��ਮ��"
MENU4[8] := "����஫쭠� �㬬�"
MENU4[9] := "��⮪��         "
MENU4[10] := "�����           "

priv menu5[8]
MENU5[1] := "��ୠ�-�थ�         "
MENU5[2] := "������� - �।���� "
MENU5[3] := "������ ������     "
MENU5[4] := "������ ���. ���    "
MENU5[5] := "����.�����.�� ������."
MENU5[6] := "���. ���ᮢ�� �थ஢"
MENU5[7] := "������� �����        "
MENU5[8] := "����������(204001)  "

priv menu6[9]
MENU6[1] := "��ᬮ�� ���⪮�  "
MENU6[2] := "������� �����⮢   "
MENU6[3] := "������᪠� �믨᪠ "
MENU6[4] := "������ �� ���      "
MENU6[5] := "��ᬮ�� �஢����  "
MENU6[6] := "������. ���㬥��� "
MENU6[7] := "�� �뤠���        "
MENU6[8] := "�� ����祭��      "
MENU6[9] := "���� 1-OA,1-PA"

priv menu7[16]
MENU7[1] := " 1.�������� "
MENU7[2] := " 2.��ॢ���  "
MENU7[3] := " 3.����.��୮"
MENU7[4] := " 4.DOKK       "
MENU7[5] := " 5.BS�        "
MENU7[6] := " 6.BS� DKKLN��"
MENU7[7] := " 7.DOKS,DOKZ  "
MENU7[8] := " 8.DOKKO      "
MENU7[9] := " 9.���� ��.��."
MENU7[10] := "10.DKKLN � ���"
MENU7[11] := "11.���᠎����"
MENU7[12] := "12.DKKLN ���  "
MENU7[13] := "13.MODDOC ����"
MENU7[14] := "14.DOKK(O)361 "
MENU7[15] := "15.DKNAP ���  "
MENU7[16] := "16.DKKLN 361  "


SIZAM[3] := len(menu3)
SIZAM[4] := len(menu4)
SIZAM[5] := len(menu5)
SIZAM[6] := len(menu6)
SIZAM[7] := len(menu7)

maine()
