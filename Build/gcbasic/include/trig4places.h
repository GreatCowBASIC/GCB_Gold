;Copyright 2014-2024 Thomas Henry
;
;Include File: Trig4Places.h
;Trigonometric Functions for GCBASIC
;Thomas Henry --- 4/17/2014 - 2017

;Sine and Cosine, accurate to 4 decimal places.

;These functions are completely general: the arguments may
;be positive, negative or zero, and of any size, in one
;degree increments.

;The tangent is not available at this precision since it
;grows large and exceeds what the Integer data type is
;capable of expressing in GCBASIC.

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
  ;Sine data for 0 through 90 degrees, to 4 places, scaled up
  ;by 10000. Cosine is derived from this table as well.
  ;Because tangent grows so large, its values exceed what
  ;can be reasonably stored at this precision and is excluded.
  0
  175
  349
  523
  698
  872
  1045
  1219
  1392
  1564
  1736
  1908
  2079
  2250
  2419
  2588
  2756
  2924
  3090
  3256
  3420
  3584
  3746
  3907
  4067
  4226
  4384
  4540
  4695
  4848
  5000
  5150
  5299
  5446
  5592
  5736
  5878
  6018
  6157
  6293
  6428
  6561
  6691
  6820
  6947
  7071
  7193
  7314
  7431
  7547
  7660
  7771
  7880
  7986
  8090
  8192
  8290
  8387
  8480
  8572
  8660
  8746
  8829
  8910
  8988
  9063
  9135
  9205
  9272
  9336
  9397
  9455
  9511
  9563
  9613
  9659
  9703
  9744
  9781
  9816
  9848
  9877
  9903
  9925
  9945
  9962
  9976
  9986
  9994
  9998
  10000
end table
