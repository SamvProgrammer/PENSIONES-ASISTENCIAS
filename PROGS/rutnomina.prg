**
FUNCTION Folio
 LPARAMETERS npar, naccion
 LOCAL calias, nfol
 calias = SELECT()
 USE SHARED Data\Tablas\Folios ORDER 1 IN 0
 IF naccion='+'
    nfol = IIF(SEEK(npar, 'Folios', 1), folios.folio+1, 0)
 ELSE
    nfol = IIF(SEEK(npar, 'Folios', 1), folios.folio-1, 0)
 ENDIF
 UPDATE Folios SET folio = nfol WHERE clave==npar
 USE IN folios
 SELECT (calias)
 RETURN nfol
ENDFUNC
**
FUNCTION Crypt
 LPARAMETERS cadena
 LOCAL i, llave, nsalida, cad
 cad = ''
 cadena = ALLTRIM(cadena)
 FOR i = LEN(cadena) TO 1 STEP -1
    cad = cad+SUBSTR(cadena, i, 1)
 ENDFOR
 cadena = cad
 nsalida = ''
 llave = 5
 FOR i = 1 TO LEN(cadena)
    car = ASC(SUBSTR(cadena, i, 1))
    num = CHR(BITXOR(car, 5))
    nsalida = nsalida+num
 ENDFOR
 RETURN nsalida
ENDFUNC
**
FUNCTION F_Acceso
 LPARAMETERS nmenu
 LOCAL nsi
 IF STR(nvermenu, 1)$nmenu
    nsi = .F.
 ELSE
    nsi = .T.
 ENDIF
 RETURN nsi
ENDFUNC
**
FUNCTION fnConver
 LPARAMETERS _cantid
 LOCAL mmil, mill, mile, peso, _ret, mdec, s, _can1, _can2
 mdec = (_cantid-INT(_cantid))*100
 s = ALLTRIM(STR(INT(_cantid)))
 DO WHILE LEN(s)<12
    s = '0'+s
 ENDDO
 mmil = VAL(SUBSTR(s, 1, 3))
 mill = VAL(SUBSTR(s, 4, 3))
 mile = VAL(SUBSTR(s, 7, 3))
 peso = VAL(SUBSTR(s, 10, 3))
 STORE '' TO _can1, _can2
 IF mmil<>0
    _can1 = _dam(mmil)+'MIL '
 ENDIF
 IF mill<>0
    _can1 = _can1+IIF(mill=1, _dam(mill)+'MILLON', _dam(mill)+'MILLONES')
 ENDIF
 IF mile<>0
    _can1 = IIF(mill>0, _can1+' '+_dam(mile)+'MIL', _dam(mile)+'MIL')
 ENDIF
 _can2 = _dam(peso)
 _can3 = '('+_can1+' '+TRIM(_can2)+IIF(mill<>0 .AND. mile=0 .AND. peso=0, 'DE PESOS ', IIF(_cantid>1.00  .OR. _cantid<1.00 , ' PESOS ', ' PESO '))+IIF(mdec<10 .AND.  .NOT. mdec=0, '0'+ALLTRIM(STR(mdec)), IIF(mdec=0, '00', IIF(mdec>9, ALLTRIM(STR(mdec)), '')))+'/100 M.N.)'
 RETURN _can3
ENDFUNC
**
FUNCTION _Dam
 LPARAMETERS _dto
 LOCAL unid, dece, _cen, extr, s3, cvn, cen, dec, uni, nini, nfin
 s = ALLTRIM(STR(_dto, 3, 0))
 s3 = ''
 DIMENSION unid(9), dece(7), _cen(9), extr(20)
 STORE 'UN' TO unid(1)
 STORE 'DOS' TO unid(2)
 STORE 'TRES' TO unid(3)
 STORE 'CUATRO' TO unid(4)
 STORE 'CINCO' TO unid(5)
 STORE 'SEIS' TO unid(6)
 STORE 'SIETE' TO unid(7)
 STORE 'OCHO' TO unid(8)
 STORE 'NUEVE' TO unid(9)
 STORE 'TREINTA' TO dece(1)
 STORE 'CUARENTA' TO dece(2)
 STORE 'CINCUENTA' TO dece(3)
 STORE 'SESENTA' TO dece(4)
 STORE 'SETENTA' TO dece(5)
 STORE 'OCHENTA' TO dece(6)
 STORE 'NOVENTA' TO dece(7)
 STORE 'CIENTO' TO _cen(1)
 STORE 'DOSCIENTOS' TO _cen(2)
 STORE 'TRESCIENTOS' TO _cen(3)
 STORE 'CUATROCIENTOS' TO _cen(4)
 STORE 'QUINIENTOS' TO _cen(5)
 STORE 'SEISCIENTOS' TO _cen(6)
 STORE 'SETECIENTOS' TO _cen(7)
 STORE 'OCHOCIENTOS' TO _cen(8)
 STORE 'NOVECIENTOS' TO _cen(9)
 STORE 'DIEZ' TO extr(1)
 STORE 'ONCE' TO extr(2)
 STORE 'DOCE' TO extr(3)
 STORE 'TRECE' TO extr(4)
 STORE 'CATORCE' TO extr(5)
 STORE 'QUINCE' TO extr(6)
 STORE 'DIECISEIS' TO extr(7)
 STORE 'DIECISIETE' TO extr(8)
 STORE 'DIECIOCHO' TO extr(9)
 STORE 'DIECINUEVE' TO extr(10)
 STORE 'VEINTE' TO extr(11)
 STORE 'VEINTIUN' TO extr(12)
 STORE 'VEINTIDOS' TO extr(13)
 STORE 'VEINTITRES' TO extr(14)
 STORE 'VEINTICUATRO' TO extr(15)
 STORE 'VEINTICINCO' TO extr(16)
 STORE 'VEINTISEIS' TO extr(17)
 STORE 'VEINTISIETE' TO extr(18)
 STORE 'VEINTIOCHO' TO extr(19)
 STORE 'VEINTINUEVE' TO extr(20)
 DO WHILE LEN(s)<3
    s = '0'+s
 ENDDO
 cvn = VAL(SUBSTR(s, 2, 2))
 cen = VAL(SUBSTR(s, 1, 1))
 dec = VAL(SUBSTR(s, 2, 1))
 uni = VAL(SUBSTR(s, 3, 1))
 s3 = IIF(cen>0, _cen(cen)+' ', s3)
 s3 = IIF(dec>2, s3+dece(dec-2)+' ', s3)
 s3 = IIF(dec>2 .AND. uni>0, s3+'Y ', s3)
 s3 = IIF(cvn>9 .AND. cvn<30, s3+extr(cvn-9)+' ', IIF(uni>0, s3+unid(uni)+' ', s3))
 s3 = IIF(_dto=100, 'CIEN ', s3)
 RETURN s3
ENDFUNC
**
FUNCTION fnFecha
 LPARAMETERS nfecha
 LOCAL nmes
 DIMENSION nmes(12)
 nmes(1) = 'Enero'
 nmes(2) = 'Febrero'
 nmes(3) = 'Marzo'
 nmes(4) = 'Abril'
 nmes(5) = 'Mayo'
 nmes(6) = 'Junio'
 nmes(7) = 'Julio'
 nmes(8) = 'Agosto'
 nmes(9) = 'Septiembre'
 nmes(10) = 'Octubre'
 nmes(11) = 'Noviembre'
 nmes(12) = 'Diciembre'
 IF  .NOT. EMPTY(nfecha)
    nfecha = IIF(DAY(nfecha)<10, '0', '')+ALLTRIM(STR(DAY(nfecha)))+' de '+nmes(MONTH(nfecha))+IIF(YEAR(nfecha)<2000, ' de ', ' de ')+STR(YEAR(nfecha), 4)
 ELSE
    nfecha = '  /  /  '
 ENDIF
 RETURN nfecha
ENDFUNC
**
FUNCTION fnMes
 LPARAMETERS xmes
 DIMENSION ames(12)
 ames(1) = 'Enero'
 ames(2) = 'Febrero'
 ames(3) = 'Marzo'
 ames(4) = 'Abril'
 ames(5) = 'Mayo'
 ames(6) = 'Junio'
 ames(7) = 'Julio'
 ames(8) = 'Agosto'
 ames(9) = 'Septiembre'
 ames(10) = 'Octubre'
 ames(11) = 'Noviembre'
 ames(12) = 'Diciembre'
 RETURN (ames(xmes))
ENDFUNC
**
PROCEDURE PorDefinir
 MESSAGEBOX('Proceso en desarrollo...', 0)
 RETURN
ENDPROC
**
FUNCTION FORMFECHA
 LPARAMETERS dfecha
 LOCAL ameses, cfecha
 DIMENSION ameses[12]
 SET DATE german
 ameses[1] = "ENE"
 ameses[2] = "FEB"
 ameses[3] = "MAR"
 ameses[4] = "ABR"
 ameses[5] = "MAY"
 ameses[6] = "JUN"
 ameses[7] = "JUL"
 ameses[8] = "AGO"
 ameses[9] = "SEP"
 ameses[10] = "OCT"
 ameses[11] = "NOV"
 ameses[12] = "DIC"
 cfecha = STR(DAY(dfecha), 2)+"."+ameses(MONTH(dfecha))+"."+RIGHT(STR(YEAR(dfecha)), 2)
 RETURN cfecha
ENDFUNC
**
PROCEDURE DESGLOSE
 LPARAMETERS arg1
 LOCAL local1, local2, local3[15, 2], local4[15]
 local4[1] = 100
 local4[2] = 50
 local4[3] = 20
 local4[4] = 10
 local4[5] = 5
 local4[6] = 2
 local4[7] = 1
 local4[8] = 0.5 
 local4[9] = 0.2 
 local4[10] = 0.1 
 local4[11] = 0.05 
 local4[12] = 0.02 
 local4[13] = 0.01 
 local4[14] = 0.005 
 local4[15] = 0.001 
 FOR local1 = 1 TO ALEN(local4)
    local3[local1, 1] = local4(local1)
    local3[local1, 2] = 0
    local2 = ALLTRIM(STR(arg1/local4(local1), 11, 3))
    local3[local1, 2] = VAL(LEFT(local2, RAT(".", local2)-1))
    arg1 = ROUND(arg1-local3(local1, 2)*local4(local1), 3)
 ENDFOR
 FOR i = 1 TO 15
    amonedas(i, 1) = local3(i, 1)
    amonedas(i, 2) = local3(i, 2)
 ENDFOR
 RETURN
ENDPROC
**
PROCEDURE fConectar
 LPARAMETERS nsi
 IF nsi
    cstrconec = "driver={PostgreSQL ANSI};Port=5432"+";Server=localhost"+";Database=nomina"+";Uid=postgres"+";Pwd=maestro"
    = SQLSETPROP(0, "DispLogin", 3)
    nhandle = SQLSTRINGCONNECT(cstrconec)
 ELSE
    = SQLDISCONNECT(nhandle)
 ENDIF
 RETURN
ENDPROC
**
PROCEDURE Sobres
 LPARAMETERS dfechaini, dfechafin, nemp, ntipo
 LOCAL ppan, aempleado, cjpp, nnumjpp, crfc, nsobres
 LOCAL cnombre, apercep, adeduc, ntotaltot, ntotalsob, asobres
 DIMENSION adeduc[50, 2], apercep[50, 2]
 PUBLIC amonedas[15, 2]
 nsobres = ''
 CLOSE TABLE ALL
 CREATE TABLE Temp\Sobres FREE (campo C (200))
 USE data\tablas\perded ORDER clave IN 0
 USE data\tablas\nominew ALIAS nominew ORDER nomina IN 0
 USE data\tablas\maestro ORDER nomina IN 0
 SELECT sobres
 IF  .NOT. EMPTY(nemp)
    APPEND BLANK
    APPEND BLANK
 ENDIF
 SELECT nominew
 SET RELATION TO STR(clave, 3) INTO perded
 DIMENSION asobres[3, 5]
 asobres[1, 1] = " (Jpp== 'JUB' .Or. Jpp== 'PDO' .Or. Jpp== 'PTA' ) "
 asobres[1, 2] = "Listados\SobJUB.Txt"
 asobres[1, 3] = 0
 asobres[1, 4] = 0
 asobres[1, 5] = "JUBILADOS CENTRO"
 asobres[2, 1] = " (Jpp== 'JUF' .Or. Jpp== 'PDF' .Or. Jpp== 'PTF') .And. NomElec=='S' "
 asobres[2, 2] = "Listados\SobJUF.Txt"
 asobres[2, 3] = 0
 asobres[2, 4] = 0
 asobres[2, 5] = "JUBILADOS FORANEOS"
 asobres[3, 1] = " Jpp== 'PEA' "
 asobres[3, 2] = "Listados\SobPEA.Txt"
 asobres[3, 3] = 0
 asobres[3, 4] = 0
 asobres[3, 5] = "PENSION ALIMENTICIA"
 FOR k = 1 TO IIF(EMPTY(nemp), 3, 1)
    cfiltro = asobres(k, 1)
    carchivo = asobres(k, 2)
    nlin = 0
    ntotaltot = 0
    ntotalsob = 0
    SELECT maestro
    IF  .NOT. EMPTY(nemp)
       SET FILTER TO jpp=ntipo AND INLIST(num,&nemp)
    ELSE
       SET FILTER TO &cfiltro AND superviven='S'
    ENDIF
    GOTO TOP
    ntantos = 0
    DO WHILE ( .NOT. EOF())
       nsobres = nsobres+CHR(13)+STR(num, 10)+' '+nombre
       cjpp = jpp
       nnumjpp = num
       cnombre = nombre
       crfc = rfc
       ctipoemp1 = LEFT(maestro.categ, 20)
       ctipoemp2 = LEFT(maestro.categ, 20)
       ndeduc = 0
       npercep = 0
       FOR xy = 1 TO 50
          adeduc[xy, 1] = ''
          adeduc[xy, 2] = ''
          apercep[xy, 1] = ''
          apercep[xy, 2] = ''
       ENDFOR
       SELECT nominew
       IF  .NOT. SEEK(cjpp+STR(nnumjpp, 6))
          SELECT maestro
          SKIP
          LOOP
       ENDIF
       ntotalsob = ntotalsob+1
       DO WHILE (cjpp==jpp .AND. nnumjpp==numjpp)
          IF (monto>0)
             xy = 1
             IF (clave<60)
                FOR xy = 1 TO 50
                   IF EMPTY(apercep(xy, 1))
                      EXIT
                   ENDIF
                ENDFOR
                npercep = npercep+monto
                apercep[xy, 1] = STR(clave, 3)+" "+perded.descri
                apercep[xy, 2] = monto
             ELSE
                FOR xy = 1 TO 50
                   IF EMPTY(adeduc(xy, 1))
                      EXIT
                   ENDIF
                ENDFOR
                ndeduc = ndeduc+monto
                adeduc[xy, 1] = "!"+STR(clave, 3)+" "+perded.descri+" "+IIF(pago1==0, "    ", STR(pago1, 3)+"->")+IIF(pago2==0, "    ", STR(pago2, 3)+"->")+IIF(pago3==0, "    ", STR(pago3, 3)+"->")+IIF(pago4==0, "   ", STR(pago4, 3)+"/")+IIF(pagot==0, "    ", STR(pagot, 3))
                adeduc[xy, 2] = monto
             ENDIF
          ENDIF
          SKIP
       ENDDO
       nmayor = 0
       FOR i = 1 TO 50
          IF  .NOT. EMPTY(apercep(i, 1)) .OR.  .NOT. EMPTY(adeduc(i, 1))
             nmayor = i
          ENDIF
       ENDFOR
       nmayor = IIF(nmayor=1, 2, nmayor)
       SELECT sobres
       FOR i = 1 TO nmayor
          IF (i==1 .OR. MOD(i, 10)==0)
             IF (i>1)
                nlin = nlin+10
                FOR xxx = 1 TO 10
                   APPEND BLANK
                ENDFOR
             ENDIF
             IF ntantos=11
                nlin = 1
                ntantos = 0
             ENDIF
             ntantos = ntantos+1
             APPEND BLANK
             REPLACE campo WITH SPACE(6)+STR(nnumjpp, 6)+' '+cnombre+' '+ctipoemp1+SPACE(1)+crfc+SPACE(34)+STR(nnumjpp, 6)+' '+cnombre+" "+TRIM(ctipoemp1)+" "+crfc
             APPEND BLANK
             REPLACE campo WITH SPACE(8)+"EVITE SUSPENSION DE PAGO FIRME SUPERVIVENCIA. ART. 99 FRACC. V LEY DE PENSIONES"
             nlin = nlin+2
             APPEND BLANK
             REPLACE campo WITH SPACE(25)+ctipoemp2+SPACE(25)+"PAGO DEL "+formfecha(dfechaini)+" AL "+formfecha(dfechafin)+SPACE(68)+"PAGO DEL "+formfecha(dfechaini)+" AL "+formfecha(dfechafin)
             nlin = nlin+3
             APPEND BLANK
             APPEND BLANK
          ENDIF
          APPEND BLANK
          REPLACE campo WITH SPACE(25)+IIF(EMPTY(apercep(i, 1)), SPACE(19), apercep(i, 1))+' '+IIF(EMPTY(adeduc(i, 1)), SPACE(40), adeduc(i, 1))+IIF(EMPTY(apercep(i, 1)), SPACE(11), TRANSFORM(apercep(i, 2), '99999999.99'))+SPACE(3)+TRANSFORM(adeduc(i, 2), '99999999.99')
          IF (i==1)
             REPLACE campo WITH RTRIM(campo)+IIF(EMPTY(adeduc(i, 1)), SPACE(38), SPACE(24))+"PERCEPCIONES:..........."+SPACE(25)+TRANSFORM(npercep, ' 999,999.99')
          ENDIF
          IF (i==2)
             IF EMPTY(campo)
                REPLACE campo WITH SPACE(134)+"DEDUCCIONES:............"+SPACE(25)+TRANSFORM(ndeduc, ' 999,999.99')
             ELSE
                REPLACE campo WITH RTRIM(campo)+IIF(EMPTY(adeduc(i, 1)), SPACE(38), SPACE(24))+"DEDUCCIONES:............"+SPACE(25)+TRANSFORM(ndeduc, ' 999,999.99')
             ENDIF
          ENDIF
          nlin = nlin+1
       ENDFOR
       nunidad = VAL(RIGHT(ALLTRIM(STR(nmayor)), 1))+(LEN(ALLTRIM(STR(nmayor)))-1)
       nlin = nlin+(10-nunidad)
       nbl = 10-nunidad
       FOR xxx = 0 TO nbl
          APPEND BLANK
       ENDFOR
       REPLACE campo WITH SPACE(85)+TRANSFORM(npercep, ' 999,999.99')+'   '+TRANSFORM(ndeduc, ' 999,999.99')+SPACE(75)+TRANSFORM(npercep-ndeduc, '999,999.99')
       nlin = nlin+2
       desglose(npercep-ndeduc)
       APPEND BLANK
       APPEND BLANK
       REPLACE campo WITH SPACE(5)+TRANSFORM(amonedas(1, 2), '999')+TRANSFORM(amonedas(2, 2), '999')+TRANSFORM(amonedas(3, 2), '999')+' '+TRANSFORM(amonedas(4, 2), '999')+TRANSFORM(amonedas(5, 2), '999')+TRANSFORM(amonedas(6, 2), '999')+' '+TRANSFORM(amonedas(7, 2), '999')+TRANSFORM(amonedas(8, 2), '999')+TRANSFORM(amonedas(9, 2), '999')+' '+TRANSFORM(amonedas(10, 2), '999')+TRANSFORM(amonedas(11, 2), '999')
       REPLACE campo WITH RTRIM(campo)+SPACE(3)+"DIRECCION DE PENSIONES/*/GOBIERNO DEL ESTADO DE OAXACA"+' '+TRANSFORM(npercep-ndeduc, ' 999,999.99')+SPACE(13)+TRANSFORM(dfechaini, "E")+'-'+TRANSFORM(dfechafin, "E")+' '+"DIRECCION DE PENSIONES"+SPACE(17)+TRANSFORM(npercep-ndeduc, ' 999,999.99')
       nlin = nlin+7
       ntotaltot = ntotaltot+(npercep-ndeduc)
       FOR xxx = 1 TO 6
          APPEND BLANK
       ENDFOR
       SELECT maestro
       SKIP
    ENDDO
    IF EMPTY(nemp)
       SELECT maestro
       SET FILTER TO
       desglose(ntotaltot)
       nlin = nlin+5
       SELECT sobres
       FOR xxx = 1 TO 5
          APPEND BLANK
       ENDFOR
       REPLACE campo WITH SPACE(39)+STR(amonedas(1, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(1, 1))), 5)+" = "+TRANSFORM(amonedas(1, 1)*amonedas(1, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(9, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(9, 1), 5, 1)), 5)+" = "+TRANSFORM(amonedas(9, 1)*amonedas(9, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(2, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(2, 1))), 5)+" = "+TRANSFORM(amonedas(2, 1)*amonedas(2, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(10, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(10, 1), 5, 1)), 5)+" = "+TRANSFORM(amonedas(10, 1)*amonedas(10, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(3, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(3, 1))), 5)+" = "+TRANSFORM(amonedas(3, 1)*amonedas(3, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(11, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(11, 1), 5, 2)), 5)+" = "+TRANSFORM(amonedas(11, 1)*amonedas(11, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(4, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(4, 1))), 5)+" = "+TRANSFORM(amonedas(4, 1)*amonedas(4, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(12, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(12, 1), 5, 2)), 5)+" = "+TRANSFORM(amonedas(12, 1)*amonedas(12, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(5, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(5, 1))), 5)+" = "+TRANSFORM(amonedas(5, 1)*amonedas(5, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(13, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(13, 1), 5, 2)), 5)+" = "+TRANSFORM(amonedas(13, 1)*amonedas(13, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(6, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(6, 1))), 5)+" = "+TRANSFORM(amonedas(6, 1)*amonedas(6, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(14, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(14, 1), 5, 3)), 5)+" = "+TRANSFORM(amonedas(14, 1)*amonedas(14, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(7, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(7, 1))), 5)+" = "+TRANSFORM(amonedas(7, 1)*amonedas(7, 2), " 99,999,999.99")+SPACE(4)+STR(amonedas(15, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(15, 1), 5, 3)), 5)+" = "+TRANSFORM(amonedas(15, 1)*amonedas(15, 2), " 99,999,999.99")
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(39)+STR(amonedas(8, 2), 8)+" x "+PADR(ALLTRIM(STR(amonedas(8, 1), 5, 1)), 5)+" = "+TRANSFORM(amonedas(8, 1)*amonedas(8, 2), " 99,999,999.99")+SPACE(9)+"----------------------------"
       nlin = nlin+1
       APPEND BLANK
       REPLACE campo WITH SPACE(80)+" T O T A L    = "+TRANSFORM(ntotaltot, "99,999,999.99")
       nlin = nlin+16
    ENDIF
    asobres[k, 3] = ntotalsob
    asobres[k, 4] = ntotaltot
    SELECT sobres
    IF EMPTY(nemp)
       narch = asobres(k, 2)
       COPY TO (narch)	&&DELIMITED WITH 
       ZAP
    ENDIF
 ENDFOR
 SELECT maestro
 SET RELATION TO
 SELECT nominew
 SET RELATION TO
 SET DATE BRITISH
 SELECT sobres
 IF  .NOT. EMPTY(nemp)
    IF MESSAGEBOX('SE IMPRIMIRAN LOS SOBRES DE NOMINA DE: '+CHR(13)+nsobres+CHR(13)+CHR(13)+'Desea Continuar...????', 033, '')=1
       COPY TO lpt2 &&DELIMITED WITH 
    ENDIF
 ENDIF
 RETURN
ENDPROC
**
FUNCTION F_Tablas
 LPARAMETERS nmes, nanio
 LOCAL usa_tabla
 CLOSE TABLE ALL
 IF EMPTY(usa_indice)
    usa_indice = 'Temp\_'+RIGHT(SYS(2015), 5)
 ENDIF
 usa_tabla = 'Nomi'+STR(nanio, 4)+'\Maes'+STR(nanio, 4)+IIF(nmes<10, '0', '')+TRIM(STR(nmes))+'.Dbf'
 IF  .NOT. FILE(usa_tabla)
    MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+usa_tabla)
    RETURN .F.
 ENDIF
 USE (usa_tabla) ALIAS maestro IN 0
 SELECT maestro
 INDEX ON jpp+STR(num, 6) TO (usa_indice+'1')
 usa_tabla = 'Nomi'+STR(nanio, 4)+'\Nomi'+STR(nanio, 4)+IIF(nmes<10, '0', '')+TRIM(STR(nmes))+'.Dbf'
 IF  .NOT. FILE(usa_tabla)
    MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+usa_tabla)
    RETURN .F.
 ENDIF
 USE (usa_tabla) ALIAS nominew IN 0
 SELECT nominew
 INDEX ON jpp+STR(numjpp, 6)+STR(clave, 3)+STR(secuen, 1) TO (usa_indice+'2')
 usa_tabla = 'Nomi'+STR(nanio, 4)+'\Perd'+STR(nanio, 4)+IIF(nmes<10, '0', '')+TRIM(STR(nmes))+'.Dbf'
 IF  .NOT. FILE(usa_tabla)
    MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+usa_tabla)
    RETURN .F.
 ENDIF
 USE (usa_tabla) ALIAS perded IN 0
 SELECT perded
 INDEX ON STR(clave, 3) TO (usa_indice+'3')
 RETURN .T.
ENDFUNC
**
FUNCTION f_Dir
 LPARAMETERS _anio
 LOCAL nruta
 nruta = CURDIR()+"NOMI"+_anio+"\"
 IF  .NOT. DIRECTORY(nruta)
    MESSAGEBOX('No se encuentra el Directorio de Respaldo:'+CHR(13)+CHR(13)+nruta, 64, 'Directorio no existe...')
    RETURN .F.
 ELSE
    RETURN .T.
 ENDIF
ENDFUNC
**
FUNCTION f_Opciones
 LPARAMETERS _mes, _anio
 LOCAL nruta, nnomina
 nruta = CURDIR()+"NOMI"+_anio+"\"
 nnomina = "Nomi"+_anio+_mes+".Dbf"
 IF FILE(nruta+nnomina)
    RETURN .T.
 ELSE
    RETURN .F.
 ENDIF
ENDFUNC
**
PROCEDURE Incrementar
 CLOSE TABLE ALL
 USE Data\tablas\nominew
 IF MESSAGEBOX('ESTA SEGURO DE REALIZAR EL INCREMENTO...', 036, 'Aviso...')=6
    WAIT WINDOW NOWAIT 'INCREMENTANDO...'
    SELECT nominew
    GOTO TOP
    DO WHILE ( .NOT. EOF())
       DO CASE
          CASE pago1>0 .AND. pago2>0 .AND. pago3>0 .AND. pago4>0
             REPLACE pago1 WITH pago1+1
             REPLACE pago2 WITH pago2+1
             REPLACE pago3 WITH pago3+1
             REPLACE pago4 WITH pago4+1
          CASE pago2>0 .AND. pago3>0 .AND. pago4>0
             REPLACE pago2 WITH pago2+1
             REPLACE pago3 WITH pago3+1
             REPLACE pago4 WITH pago4+1
          CASE pago3>0 .AND. pago4>0
             REPLACE pago3 WITH pago3+1
             REPLACE pago4 WITH pago4+1
          CASE pago4>0
             REPLACE pago4 WITH pago4+1
       ENDCASE
       IF (pago1>pagot .OR. pago2>pagot .OR. pago3>pagot .OR. pago4>pagot)
          DELETE
       ENDIF
       SKIP
    ENDDO
    MESSAGEBOX('INCREMENTO FINALIZADO...', 016, 'Aviso.')
 ENDIF
 CLOSE TABLE ALL
 RETURN
ENDPROC
**
FUNCTION fMes
 LPARAMETERS xmes
 LOCAL nmes
 DIMENSION nmes(12)
 nmes(1) = 'Enero'
 nmes(2) = 'Febrero'
 nmes(3) = 'Marzo'
 nmes(4) = 'Abril'
 nmes(5) = 'Mayo'
 nmes(6) = 'Junio'
 nmes(7) = 'Julio'
 nmes(8) = 'Agosto'
 nmes(9) = 'Septiembre'
 nmes(10) = 'Octubre'
 nmes(11) = 'Noviembre'
 nmes(12) = 'Diciembre'
 RETURN nmes(xmes)
ENDFUNC
**
FUNCTION fHorario
 LPARAMETERS ngrupo
 LOCAL xhora
 DO CASE
    CASE ngrupo=1
       xhora = '9:00 - 15:00'
    CASE ngrupo=2
       xhora = '7:00 - 13:00'
    CASE ngrupo=3
       xhora = '8:00 - 14:00'
    CASE ngrupo=4
       xhora = '10:00 - 16:00'
    CASE ngrupo=5
       xhora = '8:00 - 16:00'
    CASE ngrupo=6
       xhora = '7:00 - 15:00'
    CASE ngrupo=7
       xhora = '9:00 - 17:00'
    OTHERWISE
       xhora = 'No Existe'
 ENDCASE
 RETURN xhora
ENDFUNC
**
PROCEDURE FnProcesar
 LOCAL ne, nf
 WAIT WINDOW NOWAIT 'Revisando Observaciones duplicadas...'
 CLOSE TABLE ALL
 USE EXCLUSIVE Data\vInsidencias
 INDEX ON STR(VAL(id), 6)+DTOC(CTOD(ALLTRIM(fecha))) TO Temp\Faltas
 GOTO TOP
 DO WHILE  .NOT. EOF()
    ne = VAL(id)
    nf = fecha
    SKIP
    IF ne=VAL(id) .AND. nf=fecha
       DELETE
    ENDIF
 ENDDO
 PACK
 CLOSE TABLE ALL
 RETURN
ENDPROC
**
