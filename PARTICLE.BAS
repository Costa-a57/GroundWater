' ATTENZIONE!! CONTROLLA L'APPROSSIMAZIONE DELLA RIGA 350 E 640 SU YT E XT
100 DEFINT I-N: KEY OFF: OPTION BASE 1: GOSUB 650
120 PRINT "---  Plane steady Groundwater Flow"
130 PRINT "---  Dispersion by particle tracking random model"
' ----------------- CALL FRATTPC (FORTRAN PROGRAM)
'
 
'             SHELL "FRATTPC": CLS : NSCR = 2
 
'
150 PRINT "---  Stream lines and particles on screen": PRINT
160 DIM AX AS DOUBLE, AY AS DOUBLE, XP AS DOUBLE, YP AS DOUBLE
170 DIM POR(3400) AS DOUBLE, DT AS DOUBLE, DTF AS DOUBLE, VEL(3400)
    DIM N%(3400, 2), A, YB AS DOUBLE, XB AS DOUBLE, YA AS DOUBLE, XA AS DOUBLE
    DIM U AS DOUBLE, V AS DOUBLE, X AS DOUBLE, Y AS DOUBLE, IT AS DOUBLE, TR AS DOUBLE, TRR AS DOUBLE
    DIM NNO AS DOUBLE, NNV1 AS DOUBLE, NNV2 AS DOUBLE, DKa, Ra
200 OPEN "RIPARTE" FOR INPUT AS #1
    OPEN "RIESCE" FOR OUTPUT AS #2
    OPEN "TEMPO" FOR OUTPUT AS #5
    NLX = 640: NLY = 200: SXY = 2: NSCR = 2
210 INPUT #1, XT, YT, NX, NY
    INPUT #1, DT, DKa
'    DT = .5
    PRINT DT, DKa
    DELTAX = XT / (NX - 1)
    DELTAY = YT / (NY - 1)
    NNO = 0: NO = 0: Ra = 1
    NNV1 = 0
    NNV2 = 0
    K1 = 1
    NL = NX * (NY - 1) + NY * (NX - 1):
    PRINT , NL
       FOR I = 1 TO NL
       INPUT #1, N(I, 1), N(I, 2), POR(I), VEL(I)
       NEXT I
    CLOSE (1)
250 SX = NLX / XT: SY = SXY * NLY / YT: IF SY < SX THEN SX = SY
260 SY = SX / (SXY + .1): TR = 0: TRR = 0: IT = 1: GOSUB 650
270 PRINT "Stream lines and particles on screen": PRINT : PRINT
280 COLOR 0, 7: PRINT " B "; : COLOR 7, 0: PRINT " .... Begin": PRINT
290 COLOR 0, 7: PRINT " S "; : COLOR 7, 0: PRINT " .... Stop": PRINT
300 GOSUB 670: CLS : SCREEN NSCR
310 LINE (0, 0)-(NLX - 1, 0): LINE (NLX - 1, 0)-(NLX - 1, NLY - 1)
320 LINE (NLX - 1, NLY - 1)-(0, NLY - 1): LINE (0, NLY - 1)-(0, 0)
330 B$ = INKEY$: IF B$ = "s" OR B$ = "S" THEN SCREEN 0: END
333 SCREEN 0: PRINT " INDICE DEL SOURCE (VERTICI ESCLUSI): ": INPUT INDICE
    OPEN "COVARDAT" FOR INPUT AS #8
    FOR I = 1 TO NX * NY
    INPUT #8, X0, Y0
    PRINT , X0; Y0
    IF I = INDICE THEN GOTO 334
    NEXT I
'    PRINT " ORDINATA DEL NODO SOURCE ":INPUT Y0:
334 CLOSE (8): Y0 = YT - Y0
    PRINT " N. PARTICELLE RILASCIATE ": INPUT NSOURCE: SCREEN NSCR
340 AX = X0: AY = Y0
'341 LINE (0, 0)-(XT * SX - 1, 0): LINE (XT * SX - 1, 0)-(XT * SX - 1, YT * SY - 1)
'342 LINE (XT * SX - 1, YT * SY - 1)-(0, YT * SY - 1): LINE (0, YT * SY - 1)-(0, 0)
'
'   ATTENZIONE, RICORDA CHE XB E YB SONO IL RIFERIMENTO INIZIALE
'
    XB = AX: YB = AY
'    STOP
'    XB = AX + K1 * .001: YB = AY - K1 * .001
    U = 0: V = 0: SOMMA = 0
    NNO = 0: NNV1 = 0: NNV2 = 0: NO = 0
    FOR I = 1 TO NL
    IF INDICE < NY OR I > (NX - 1) * NY THEN GOTO 345
         IF N(I, 1) = INDICE AND I < NL / 2 THEN
           JV1 = I: J = JV1: JV2 = J - 1: V = VEL(J): U = 0: GOTO 350
'         ELSEIF N(I, 2) = INDICE AND I < NL / 2 THEN
'                      JV2 = I: J = JV2: V = VEL(J): U = 0: GOTO 350
         END IF
345      IF N(I, 1) = INDICE AND I > NL / 2 THEN
                      J0 = I: JV1 = N(I, 2) - 1: JV2 = JV1 - 1: J = J0: U = VEL(J): V = 0: GOTO 350
         ELSEIF N(I, 2) = INDICE AND I > NL / 2 THEN
                      J0 = I: JV1 = N(I, 2) - (NY - 1): JV2 = JV1 - 1: J = J0: U = VEL(J): V = 0: GOTO 350
         END IF
    NEXT I
350 XA = AX: YA = AY: GOSUB 680: IF YB > YT - YT * .001 THEN J = J + 1: GOTO 641
    'STOP
    IF YB < 0! THEN V = 0: GOTO 641
    IF XB < 0! THEN : U = 0: GOTO 641
    IF ABS(U * DT) > DELTAX AND NNO = 2 THEN XB = AX + DELTAX
    IF ABS(V * DT) > DELTAY AND NNV1 = 2 THEN YB = AY - DELTAY
    IF ABS(V * DT) > DELTAY AND NNV2 = 2 THEN YB = AY + DELTAY
    IF NNV1 > 100000 OR NNV2 > 100000 OR NNO > 100000 GOTO 641
460 PSET (SX * XB, SY * YB): AX = XB: AY = YB
640 IT = IT + 1: IF XB < XT - XT * .001 GOTO 350
641 K1 = K1 + 1: PRINT #2, J
    PRINT #2, "NUME. PART.: ", K1 - 1, "NUME. DI DT", IT - 1, TR, TRR
'vc    IF J = 629 OR J = 630 OR J = 210 OR J = 211 THEN PRINT #5, TR, TRR: 'CLS
'    IF J = 2331 OR J = 2332 OR J = 423 OR J = 422 OR J = 2334 OR J = 2333 OR J = 463 OR J = 464 THEN PRINT #5, TR, TRR: 'CLS
'   IF J = 1194 OR J = 1226 OR J = 1195 OR J = 1227 OR J = 443 OR J = 444 OR J = 445 THEN PRINT #5, TR
'bisceglie     IF J = 1862 OR J = 1863 OR J = 1162 OR J = 1163 THEN PRINT #5, TR, TRR
'Colucci
' fioschi
' IF J = 1184 OR J = 1185 OR J = 228 OR J = 229 OR J = 250 THEN PRINT #5, TR, TRR
'fico
'IF J = 1088 OR J = 1089 OR J = 231 OR J = 232 THEN PRINT #5, TR, TRR
'corato
    IF J = 760 THEN PRINT #5, TR, TRR
    IF K1 <= NSOURCE THEN TR = 0: TRR = 0: IT = 1: DTF = 0: L = 0: XB = X0: YB = Y0: GOTO 340
 
      K1 = 1
      SCREEN 0: PRINT " CONTINUA (s/n)": INPUT C$: SCREEN NSCR
      IF C$ = "s" OR C$ = "S" THEN GOTO 333
      CLOSE (2)
      CLOSE (5)
      SCREEN 0: PRINT "OK  hai finito !!!"
    END
 
 
 
650 CLS : LOCATE 1, 26, 1: COLOR 0, 7: PRINT " Transport by Particle Tracking ";
660 COLOR 7, 0: PRINT : PRINT : RETURN
670 A$ = INKEY$: IF A$ = "B" OR A$ = "b" THEN RETURN ELSE 670
680 REM
    'STOP
690 Y = YA: X = XA
    GOSUB 710
    IF DTF > 0 THEN
    YB = Y - V * DTF
    XB = X + U * DTF
      ELSE
       YB = Y - V * DT
       XB = X + U * DT
    END IF
    RETURN
710 REM  Calculation of velocities
720 IF V = 0 THEN GOTO 800
    IF V > 0 THEN GOTO 810
    IF V < 0 THEN GOTO 820
800 NNO = NNO + 1: IF ABS(U * DT * NNO) < DELTAX THEN X = XB: V = 0: U = VEL(J0): RETURN
     'STOP
     IF L = 1 THEN GOTO 8001
     DTF = ABS((DELTAX - ABS(U * DT * (NNO - 1))) / U): L = 1
       IF SOMMA > 0 THEN
        'STOP
        TR = TR + POR(J0) * DELTAX / (VEL(J0) * SOMMA)
        TRR = TRR + DELTAX / ABS(VEL(J0))
        Ra = 1 + 2 * DKa * VEL(J0) / (POR(J0) * .00001)
        TR = TR * Ra
        ELSE
        TRR = TRR + DELTAX / ABS(VEL(J0))
'        TR = TR + TRR
       END IF
' serve per v. transito      IF J = 2770 OR J = 2734 OR J = 2806 OR J = 2771 THEN GOTO 641
    PRINT #2, "ITER. N.: ", IT, TR, TRR, "  DELTA FINALE SUL LATO: ", J0, " ", USING "#.###"; DTF: J = J0: RETURN
'vc    J = J0: RETURN
8001    IF XB > XT - .001 THEN NNO = 1: J = J + 1: GOTO 641
        DTF = 0
        L = 0
     NO = NO + 1
'STOP
    FOR I = 1 TO NL / 2
    IF VEL(J0) > 0 THEN
      IF N(I, 2) = N(J0, 2) THEN JJV2 = I: JJV1 = JJV2 + 1: GOTO 801
    ELSEIF N(I, 2) = N(J0, 1) THEN
      JJV2 = I: JJV1 = JJV2 + 1: GOTO 801
    END IF
    NEXT I
801 NNO = 1
'    IF K1 = 7 THEN STOP
              IF VEL(J0) < 0 GOTO 805
    IF POR(JJV2) < 0 AND POR(JJV1) >= 0 THEN
        SOMMA = ABS(POR(J0 + 1)) + ABS(POR(JJV1)) + ABS(POR(JJV2))
        PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB <= .0001 THEN SOMMA = SOMMA - ABS(POR(JJV1)): PESO2 = .000001
        PESO3 = ABS(POR(JJV2) / SOMMA): IF YB >= YT THEN SOMMA = SOMMA - ABS(POR(JJV2)): PESO3 = .000001
        PESO1 = ABS(POR(J0 + 1)) / SOMMA
           ELSEIF POR(JJV2) >= 0 THEN
        SOMMA = ABS(POR(J0 + 1)) + ABS(POR(JJV1))
        PESO1 = ABS(POR(J0 + 1)) / SOMMA
        PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB >= YT OR YB <= .0001 THEN AR = 0: GOTO 850
        PESO3 = .000001
           ELSE
        SOMMA = ABS(POR(J0 + 1)) + ABS(POR(JJV2))
        PESO1 = ABS(POR(J0 + 1)) / SOMMA
        PESO2 = .000001
        PESO3 = ABS(POR(JJV2)) / SOMMA
           END IF
               IF POR(JJV1) < 0 AND POR(JJV2) > 0 THEN
               AR = 0: IF VEL(J0 + 1) < 0 THEN NNO = 1: GOTO 641
               GOTO 850
               END IF
           IF POR(JJV1) < 0 AND POR(JJV2) < 0 THEN
           SOMMA = ABS(POR(J0 + 1)) + ABS(POR(JJV2))
           PESO1 = POR(J0 + 1) / SOMMA
           PESO3 = ABS(POR(JJV2)) / SOMMA: PESO2 = .000001
           END IF
             IF POR(J0 + 1) <= 0 THEN
                IF POR(JJV1) > 0 THEN AR = 1: GOTO 850
                IF POR(JJV1) < 0 THEN AR = 2: GOTO 1000
                  IF POR(JJV2) < 0 AND POR(JJV1) >= 0 THEN
                  SOMMA = ABS(POR(JJV1)) + ABS(POR(JJV2))
                  PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB <= .0001 THEN SOMMA = SOMMA - ABS(POR(JJV1)): PESO2 = .000001
                  PESO3 = ABS(POR(JJV2) / SOMMA): IF YB >= YT THEN SOMMA = SOMMA - ABS(POR(JJV2)): PESO3 = .000001
                  PESO1 = .000001
                  END IF
             END IF
             GOTO 849
805  IF POR(JJV2) < 0 AND POR(JJV1) >= 0 THEN
        SOMMA = ABS(POR(J0 - 1)) + ABS(POR(JJV1)) + ABS(POR(JJV2))
        PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB <= .0001 THEN SOMMA = SOMMA - ABS(POR(JJV1)): PESO2 = .000001
        PESO3 = ABS(POR(JJV2) / SOMMA): IF YB >= YT THEN SOMMA = SOMMA - ABS(POR(JJV2)): PESO3 = .000001
        PESO1 = ABS(POR(J0 - 1)) / SOMMA
           ELSEIF POR(JJV2) >= 0 THEN
        SOMMA = ABS(POR(J0 - 1)) + ABS(POR(JJV1))
        PESO1 = ABS(POR(J0 - 1)) / SOMMA
        PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB >= YT OR YB <= .0001 THEN AR = 0: GOTO 850
        PESO3 = .000001
        IF VEL(J0 - 1) > 0 THEN NNO = 1: GOTO 641
           ELSE
        SOMMA = ABS(POR(J0 - 1)) + ABS(POR(JJV2))
        PESO1 = ABS(POR(J0 - 1)) / SOMMA
        PESO2 = .000001
        PESO3 = ABS(POR(JJV2)) / SOMMA
           END IF
           IF POR(JJV1) < 0 AND POR(JJV2) > 0 THEN AR = 0: GOTO 850
           IF POR(JJV1) < 0 AND POR(JJV2) < 0 THEN
           SOMMA = ABS(POR(J0 - 1)) + ABS(POR(JJV2))
           PESO1 = POR(J0 - 1) / SOMMA
           PESO3 = ABS(POR(JJV2)) / SOMMA: PESO2 = .000001
           END IF
             IF POR(J0 - 1) <= 0 THEN
                IF POR(JJV1) > 0 THEN AR = 1: GOTO 850
                IF POR(JJV1) < 0 THEN AR = 2: GOTO 1000
                  IF POR(JJV2) < 0 AND POR(JJV1) >= 0 THEN
                  SOMMA = ABS(POR(JJV1)) + ABS(POR(JJV2))
                  PESO2 = ABS(POR(JJV1)) / SOMMA: IF YB <= .0001 THEN SOMMA = SOMMA - ABS(POR(JJV1)): PESO2 = .000001
                  PESO3 = ABS(POR(JJV2) / SOMMA): IF YB >= YT THEN SOMMA = SOMMA - ABS(POR(JJV2)): PESO3 = .000001
                  PESO1 = .000001
                  END IF
             END IF
849
    OPEN "PROB.DAT" FOR OUTPUT AS #3
    IF PESO1 = 1 THEN PESO1 = PESO1 - .000001
    IF PESO2 = 1 THEN PESO2 = PESO2 - .000001
    IF PESO3 = 1 THEN PESO3 = PESO3 - .000001
    PRINT #3, USING "#.######"; ABS(1 - (PESO2 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO2))
    A = RND: PRINT #3, USING "#.#########"; A
    CLOSE (3)
'
' CHIAMA IL SOTTOPROGRAMMA PER IL CALCOLO DEL RANDOM PESATO
'
   SHELL "RANPESA"
   OPEN "RANRES" FOR INPUT AS #4
   INPUT #4, AR: CLOSE (4)
850    IF VEL(J0) < 0 AND AR = 0 THEN
       J = J0 - 1
       ELSEIF VEL(J0) > 0 AND AR = 0 THEN
       J = J0 + 1
       END IF
851    IF AR = 0 THEN U = VEL(J): V = 0: J0 = J: PRINT #2, J: RETURN
'vc 851    IF AR = 0 THEN U = VEL(J): V = 0: J0 = J: RETURN
 
    IF AR <> 1 THEN GOTO 1000
'    J2 = N(J0 - (NX - 2), 1): U = 0:
'    FOR I = 1 TO NL:
'    IF N(I, 2) = J2 THEN J = I: GOTO 860
'    NEXT I
860 J = JJV1: V = VEL(J): U = 0
    IF V < 0 THEN AR = 0: GOTO 851
    PRINT #2, J
    RETURN
 
1000    IF AR <> 2 THEN RETURN
'J3 = N(J0 + NX, 1): U = 0:
'    FOR I = 1 TO NL:
'    IF N(I, 1) = J3 THEN J = I: GOTO 870
'    NEXT I:
870  J = JJV2: V = VEL(J): U = 0
    IF V > 0 THEN AR = 0: GOTO 851
    PRINT #2, J
    RETURN
'
'   CONTINUA ANDANDO IN ALTO
'
'
810 NNV1 = NNV1 + 1: IF (V * DT * NNV1) <= DELTAY THEN Y = YB: X = XB: U = 0: V = VEL(J): RETURN
     IF L = 1 THEN GOTO 811
     'STOP
     IF SOMMA > 0 THEN
     TR = TR + POR(J) * DELTAX / (VEL(J) * SOMMA)
     TRR = TRR + DELTAX / ABS(VEL(J))
     Ra = 1 + 2 * DKa * VEL(J) / (POR(J) * .00001)
     TR = TR * Ra
     ELSE
     TRR = TRR + DELTAX / ABS(VEL(J))
     TR = TR + TRR
     END IF
     DTF = (DELTAY - (V * DT * (NNV1 - 1))) / V: L = 1
     IF J = 1129 OR J = 1130 OR J = 1172 OR J = 1086 THEN GOTO 641 'CLS
     PRINT #2, "ITER. N.: ", IT, TR, TRR, " DT FINALE SUL LATO: ", J, " ", USING "#.###"; DTF: RETURN
      RETURN
811  DTF = 0
     L = 0
     'STOP
 
    NNV1 = 1
             FOR I = 1 TO NL
               IF N(J, 1) = N(I, 2) AND I > NL / 2 THEN J0 = I: GOTO 8111
             NEXT I
8111 IF YB <= .0001 THEN GOTO 641
 
    IF POR(J + 1) > 0 AND POR(J0 - NX + 2) > 0 THEN
    SOMMA = ABS(POR(J + 1)) + POR(J0 - NX + 2)
            IF POR(J0 - NX + 1) >= 0 THEN
            PESO3 = .000001
            ELSE
            SOMMA = SOMMA + ABS(POR(J0 - NX + 1))
            PESO3 = ABS(POR(J0 - NX + 1)) / SOMMA
            END IF
    PESO1 = POR(J0 - NX + 2) / SOMMA: PESO2 = POR(J + 1) / SOMMA
    ELSE
          IF POR(J0 - NX + 1) >= 0 AND POR(J + 1) <= 0 THEN
              IF VEL(J0 - NX + 2) <= 0 THEN
              GOTO 641
              ELSE
              AR = 0: GOTO 840
              END IF
            END IF
          IF POR(J0 - NX + 1) >= 0 AND POR(J + 1) > 0 THEN AR = 1: GOTO 840
          IF POR(J0 - NX + 1) <= 0 AND POR(J + 1) > 0 THEN SOMMA = ABS(POR(J0 - NX + 1)) + POR(J + 1): PESO3 = ABS(POR(J0 - NX + 1)) / SOMMA: PESO2 = POR(J + 1) / SOMMA: PESO1 = .000001
          IF POR(J0 - NX + 1) <= 0 AND POR(J0 - NX + 2) > 0 THEN : SOMMA = ABS(POR(J0 - NX + 1)) + POR(J0 - NX + 2): PESO3 = ABS(POR(J0 - NX + 1)) / SOMMA: PESO3 = POR(J0 - NX + 2) / SOMMA: PESO2 = .000001
            IF POR(J0 - NX + 2) < 0 AND POR(J + 1) < 0 THEN
            AR = 2
            IF VEL(J0 - NX + 1) > 0 THEN NNV1 = 1: GOTO 641
            GOTO 840
            END IF
     END IF
    OPEN "PROB.DAT" FOR OUTPUT AS #3
    IF PESO1 = 1 THEN PESO1 = PESO1 - .000001
    IF PESO2 = 1 THEN PESO2 = PESO2 - .000001
    IF PESO3 = 1 THEN PESO3 = PESO3 - .000001
    PRINT #3, USING "#.######"; ABS(1 - (PESO2 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO2))
    A = RND: PRINT #3, USING "#.#########"; A
    CLOSE (3)
'
' CHIAMA IL SOTTOPROGRAMMA PER IL CALCOLO DEL RANDOM PESATO
'
   SHELL "RANPESA"
   OPEN "RANRES" FOR INPUT AS #4
   INPUT #4, AR: CLOSE (4)
840 IF AR = 0 THEN J = J0 - (NX - 2): U = VEL(J):  V = 0: J0 = J: PRINT #2, J: RETURN
    IF AR = 2 THEN J = J0 - (NX - 2) - 1: U = VEL(J):  V = 0: J0 = J: PRINT #2, J: RETURN
    IF AR = 1 THEN J = J + 1: U = 0: V = VEL(J):  PRINT #2, J: IF V < 0 THEN AR = 0: J0 = J0 - (NX - 2): GOTO 840
'840 IF AR = 0 THEN J = J0 - (NX - 2): U = VEL(J):  V = 0: J0 = J: RETURN
'    IF AR = 2 THEN J = J0 - (NX - 2) - 1: U = VEL(J):  V = 0: J0 = J: RETURN
'    IF AR = 1 THEN J = J + 1: U = 0: V = VEL(J): IF V < 0 THEN AR = 0: J0 = J0 - (NX - 2): GOTO 840
   
    RETURN
'
'   CONTINUA ANDANDO VERSO IL BASSO
'
 
820 NNV2 = NNV2 + 1: IF ABS(V * DT * NNV2) <= DELTAY THEN Y = YB: X = XB: U = 0: V = VEL(J): RETURN
     'STOP
     IF L = 1 THEN GOTO 821
     'STOP
      IF SOMMA > 0 THEN
     TR = TR + POR(J) * DELTAX / (VEL(J) * SOMMA)
     TRR = TRR + DELTAX / ABS(VEL(J))
     Ra = 1 + 2 * DKa * VEL(J) / (POR(J) * .00001)
      ELSE
      TRR = TRR + DELTAX / ABS(VEL(J))
      TR = TR + TRR
      END IF
     TR = TR * Ra
     DTF = (DELTAY - ABS(V * DT * (NNV2 - 1))) / ABS(V): L = 1
     PRINT #2, "ITER. N.: ", IT, TR, TRR, "  DELTA FINALE SUL LATO: ", J, " ", USING "#.###"; DTF: RETURN
      RETURN
821  DTF = 0
     L = 0
     NNV2 = 1
       IF YB >= YT THEN GOTO 641
'        AR = 0
'        FOR I = 1 TO NL
'        IF N(I, 1) = N(J, 2) AND I > NL / 2 THEN J0 = I: J = J0: V = 0: U = VEL(J): PRINT #2, J0: RETURN
'        NEXT I
'        END IF
        FOR I = 1 TO NL
        IF N(I, 2) = N(J, 2) AND I > NL / 2 THEN J0 = I: GOTO 822
        NEXT I
822 IF POR(J - 1) < 0 AND POR(J0 + NX) > 0 THEN
    SOMMA = ABS(POR(J - 1)) + POR(J0 + NX)
            IF POR(J0 + NX - 1) >= 0 THEN
            PESO3 = .000001
            ELSE
            PESO3 = ABS(POR(J0 + NX - 1)) / (SOMMA + ABS(POR(J0 + NX - 1)))
            END IF
    PESO1 = POR(J0 + NX) / SOMMA: PESO2 = ABS(POR(J - 1)) / SOMMA
    ELSE
          IF POR(J0 + NX - 1) >= 0 AND POR(J - 1) <= 0 THEN AR = 1: GOTO 830
          IF POR(J0 + NX - 1) >= 0 AND POR(J - 1) > 0 THEN
                                                      AR = 0
                       IF VEL(J0 + NX) < 0 THEN NNV2 = 1: GOTO 641
                                                      GOTO 830
                                                      END IF
          IF POR(J0 + NX - 1) <= 0 AND POR(J - 1) < 0 THEN SOMMA = ABS(POR(J0 + NX - 1)) + ABS(POR(J - 1)): PESO3 = ABS(POR(J0 + NX - 1)) / SOMMA: PESO2 = ABS(POR(J - 1)) / SOMMA: PESO1 = .000001
'          IF POR(J0 + NX - 1) <= 0 AND POR(J0 + NX) > 0 THEN SOMMA = ABS(POR(J0 - NX + 1)) + POR(J0 + NX): PESO3 = ABS(POR(J0 + NX - 1)) / SOMMA: PESO1 = POR(J0 + NX) / SOMMA: PESO2 = .000001
          IF POR(J0 + NX - 1) <= 0 AND POR(J0 + NX) > 0 THEN SOMMA = ABS(POR(J0 + NX - 1)) + POR(J0 + NX): PESO3 = ABS(POR(J0 + NX - 1)) / SOMMA: PESO1 = POR(J0 + NX) / SOMMA: PESO2 = .000001
          IF POR(J0 + NX) < 0 AND POR(J - 1) >= 0 THEN AR = 2: GOTO 830
 
    END IF
    OPEN "PROB.DAT" FOR OUTPUT AS #3
    IF PESO1 = 1 THEN PESO1 = PESO1 - .000001
    IF PESO2 = 1 THEN PESO2 = PESO2 - .000001
    IF PESO3 = 1 THEN PESO3 = PESO3 - .000001
    PRINT #3, USING "#.######"; ABS(1 - (PESO2 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO3))
    PRINT #3, USING "#.######"; ABS(1 - (PESO1 + PESO2))
    A = RND: PRINT #3, USING "#.#########"; A
    CLOSE (3)
'
' CHIAMA IL SOTTOPROGRAMMA PER IL CALCOLO DEL RANDOM PESATO
'
   SHELL "RANPESA"
   OPEN "RANRES" FOR INPUT AS #4
   INPUT #4, AR: CLOSE (4)
830  IF AR = 0 THEN J = J0 + NX: U = VEL(J): V = 0: J0 = J: PRINT #2, J: RETURN
     IF AR = 2 THEN J = J0 + NX - 1: U = VEL(J):  V = 0: J0 = J: PRINT #2, J: RETURN
     IF AR = 1 THEN J = J - 1: U = 0: V = VEL(J): PRINT #2, J
'830  IF AR = 0 THEN J = J0 + NX: U = VEL(J): V = 0: J0 = J: RETURN
'     IF AR = 2 THEN J = J0 + NX - 1: U = VEL(J):  V = 0: J0 = J: RETURN
'     IF AR = 1 THEN J = J - 1: U = 0: V = VEL(J)
    
     IF V > 0 THEN
     AR = 0: J0 = J0 + NX - 1
      PRINT #2, J0
      GOTO 830
      ELSE RETURN
      END IF
      RETURN
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

