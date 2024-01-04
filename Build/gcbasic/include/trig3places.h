;Copyright 2014-2024 Thomas Henry
;
;Include File: Trig3Places.h
;Trigonometric Functions for GCBASIC
;Thomas Henry --- 4/17/2014 - 2017

;Sine, Cosine and Tangent, accurate to 3 decimal places.

;These functions are completely general: the arguments may
;be positive, negative or zero, and of any size, in one
;degree increments. Keep in mind that tangent is not defined
;for 90 degrees, 270 degrees and all of their coterminal
;angles. It is the calling program's responsibility to
;avoid these. Also be aware that tangent of 89 degrees and
;all of its coterminals, overflows.

;Input parameters and function values are integers:
;trig_arg1, trig_arg2, trig_sign, sin, cos, tan

;The following local variables are used:

dim trig_i as byte
dim trig_val as word
dim trig_sign as integer

;A fair amount of casting, explicit and implicit occurs.

;----- Subroutines

function trig_ref(in trig_arg1 as integer) as integer
  ;create reference angle (0 to 90) for the argument

  if (trig_arg1 > 270) then           ;Quadrant IV
    trig_ref = 360 - trig_arg1
  else
    if (trig_arg1 > 180) then         ;Quadrant III
      trig_ref = trig_arg1 - 180
    else                              ;Quadrant II
      if (trig_arg1 > 90) then
        trig_ref = 180 - trig_arg1
      else
        trig_ref = trig_arg1          ;Quadrant I by default
      end if
    end if
  end if
end function

;-----

function sin(in trig_arg2 as integer) as integer
  ;return sine of angle

  if trig_arg2 < 0 then               ;sine is an odd function,
    trig_sign = -1                    ;so change sign of result
    trig_arg2 = -1 * trig_arg2        ;and negate negative angle
  else
    trig_sign = 1                     ;else a positive angle
  end if

  trig_arg2 = trig_arg2 mod 360       ;reduce to 0 to 359 degrees

  if trig_arg2 > 180 then
    trig_sign = -1 * trig_sign        ;negative in III and IV
  end if

  trig_arg2 = trig_ref(trig_arg2)     ;get the reference angle

  trig_i = [byte]trig_arg2+1          ;index into the table
  readTable sineTab, trig_i, trig_val
  sin = trig_sign * [integer]trig_val ;create final result
end function

;-----

function cos(in trig_arg2 as integer) as integer
  ;return cosine of angle

  if trig_arg2 < 0 then               ;cosine is an even function,
    trig_arg2 = -1 * trig_arg2        ;so negate negative angle
  end if

  trig_arg2 = trig_arg2 mod 360       ;reduce to 0 to 359 degrees

  trig_sign = 1                       ;assume result is positive
  if trig_arg2>90 and trig_arg2<270 then
    trig_sign = -1                    ;but negative in II and III
  end if

  trig_arg2 = trig_ref(trig_arg2)     ;get the trig_reference angle
  trig_arg2 = [integer]90 - trig_arg2 ;use cofunction identity

  trig_i = [byte]trig_arg2+1          ;index into the table
  readTable sineTab, trig_i, trig_val
  cos = trig_sign * [integer]trig_val  ;create final result
end function

;-----

function tan(in trig_arg2 as integer) as integer
  ;return tangent of angle

  if trig_arg2 < 0 then               ;tangent is an odd function,
    trig_sign = -1                    ;so change sign of result
    trig_arg2 = -1 * trig_arg2        ;and negate negative angle
  else
    trig_sign = 1                     ;else a positive angle
  end if

  trig_arg2 = trig_arg2 mod 360       ;reduce to 0 to 359 degrees

  if (trig_arg2>90 and trig_arg2<180) or (trig_arg2>270) then
    trig_sign = -1 * trig_sign        ;negative in II and IV
  end if

  trig_arg2 = trig_ref(trig_arg2)     ;get the reference angle

  trig_i = [byte]trig_arg2+1          ;index into the table
  readTable tanTab, trig_i, trig_val
  tan = trig_sign * [integer]trig_val ;create final result
end function

;----- Data

table sineTab as word
  ;Sine data for 0 through 90 degrees, to 3 places, scaled up
  ;by 1000. Cosine is derived from this table as well.
  0
  17
  35
  52
  70
  87
  105
  122
  139
  156
  174
  191
  208
  225
  242
  259
  276
  292
  309
  326
  342
  358
  375
  391
  407
  423
  438
  454
  469
  485
  500
  515
  530
  545
  559
  574
  588
  602
  616
  629
  643
  656
  669
  682
  695
  707
  719
  731
  743
  755
  766
  777
  788
  799
  809
  819
  829
  839
  848
  857
  866
  875
  883
  891
  899
  906
  914
  921
  927
  934
  940
  946
  951
  956
  961
  966
  970
  974
  978
  982
  985
  988
  990
  993
  995
  996
  998
  999
  999
  1000
  1000
end table

table tanTab as word
  ;Tangent data for 0 through 90 degrees, to 3 places, scaled up
  ;by 1000.
  0
  17
  35
  52
  70
  87
  105
  123
  141
  158
  176
  194
  213
  231
  249
  268
  287
  306
  325
  344
  364
  384
  404
  424
  445
  466
  488
  510
  532
  554
  577
  601
  625
  649
  675
  700
  727
  754
  781
  810
  839
  869
  900
  933
  966
  1000
  1036
  1072
  1111
  1150
  1192
  1235
  1280
  1327
  1376
  1428
  1483
  1540
  1600
  1664
  1732
  1804
  1881
  1963
  2050
  2145
  2246
  2356
  2475
  2605
  2747
  2904
  3078
  3271
  3487
  3732
  4011
  4331
  4705
  5145
  5671
  6314
  7115
  8144
  9514
  11430
  14301
  19081
  28636
  65535 ;tan(89) overflows, but is set to largest value
  65535 ;tan(90) is undefined, but is set to largest value
end table
