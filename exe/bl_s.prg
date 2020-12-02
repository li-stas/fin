save screen to sckl
set devi to print
set print on
if !empty(gcPrn)
   ?chr(27)+'E'+chr(27)+'&l1h26a0O'+chr(27)+chr(27)+'(3R'+chr(27)+'(s0p21.00h0s0b4099T'+chr(27)  // Š­¨¦­ ï €4
else
   ?chr(27)+chr(77)+chr(15)
endif
*set cons off
?'                                                ¡®à®â­ë© ¡ « ­á'
?'                                            ¯à¥¤¯à¨ïâ¨ï ­  '+dtoc(gdTd)
?
?'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
?'³®¬¥à  ³     ¨¬¥­®¢ ­¨¥    ³      ‚å®¤ïé¥¥ á «ì¤®      ³           ¡®à®âë         ³      ˆáå®¤ïé¥¥ á «ì¤®     ³'
?'³ ¡/c   ³        ¡/á         ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
?'³       ³                    ³    „¥¡¥â    ³    Šà¥¤¨â   ³    „¥¡¥â    ³    Šà¥¤¨â   ³    „¥¡¥â    ³    Šà¥¤¨â   ³'
?'ÃÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´'

netUse('bs')
go top
d6=0
d5=0
subbs1=-200
subvxd=0
subvxk=0
subobd=0
subobk=0
subisd=0
subisk=0
i=0
do while .not.EOF()
   if bs#0
      subbs=val(substr(str(bs,6),1,3))
      if subbs#subbs1
         if i>1
            if subvxd=0
               subvxd1=space(13)
            else
               subvxd1=str(subvxd,13,2)
            endif
            if subvxk=0
               subvxk1=space(13)
            else
               subvxk1=str(subvxk,13,2)
            endif
            if subobd=0
               subobd1=space(13)
            else
               subobd1=str(subobd,13,2)
            endif
            if subobk=0
               subobk1=space(13)
            else
               subobk1=str(subobk,13,2)
            endif
            if subisd=0
               subisd1=space(13)
            else
               subisd1=str(subisd,13,2)
            endif
            if subisk=0
               subisk1=space(13)
            else
               subisk1=str(subisk,13,2)
            endif
            ?
            ?'  ˆâ®£® ¯® '+str(subbs1,3)+' áç¥âã :       '+subvxd1+' '+subvxk1+' '+subobd1+' '+subobk1+' '+subisd1+' '+subisk1
            ?
         endif
         i=0
         subvxd=0
         subvxk=0
         subobd=0
         subobk=0
         subisd=0
         subisk=0
         subbs1=subbs
      endif
      f=' '
      f=f+' '+str(bs,6,0)+' '+nbs+' '
      if dn=0
         ff=space(13)
      else
         ff=str(dn,13,2)
      endif
      subvxd=subvxd+dn
      f=f+ff+' '
      if kn=0
         ff=space(13)
      else
         ff=str(kn,13,2)
      endif
      subvxk=subvxk+kn
      f=f+ff+' '
      if db=0
         ff=space(13)
      else
         ff=str(db,13,2)
      endif
      subobd=subobd+db
      f=f+ff+' '
      if kr=0
         ff=space(13)
      else
         ff=str(kr,13,2)
      endif
      subobk=subobk+kr
      f=f+ff+' '
      dro=dn+db
      kro=kn+kr
      ras=kro-dro
      if ras>0
         ff=space(13)+' '+str(abs(ras),13,2)+' '
         d6=d6+abs(ras)
         subisk=subisk+abs(ras)
      endif
      if ras<0
         ff=str(abs(ras),13,2)+' '+space(13)+' '
         d5=d5+abs(ras)
         subisd=subisd+abs(ras)
      endif
      if ras=0
         ff=space(13)+' '+space(13)+' '
      endif
      f=f+ff
      ?f
      i++
   endif
   skip
enddo
?'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
go top
sum dn to d1
d1=str(d1,13,2)
sum kn to d2
d2=str(d2,13,2)
sum db to d3
d3=str(d3,13,2)
sum kr to d4
d4=str(d4,13,2)
d5=str(d5,13,2)
d6=str(d6,13,2)
?'        ˆâ®£® ¡ « ­á :      '+d1+' '+d2+' '+d3+' '+d4+' '+d5+' '+d6
?
nuse('bs')
//set print off
// *set cons on
//set devi to screen
ClosePrintFile()
rest screen from sckl
