;Copyright 2014-2025 Thomas Henry
;
;Include File: Trig2Places.h
;Trigonometric Functions for GCBASIC
;Thomas Henry --- 4/17/2014 - 2017

;Sine, Cosine and Tangent, accurate to 2 decimal places.

;These functions are completely general: the arguments may
;be positive, negative or zero, and of any size, in one
;degree increments. Keep in mind that tangent is not defined
;for 90 degrees, 270 degrees and all of their coterminal
;angles. It is the calling program's responsibility to
;avoid these.

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

table sineTab as byte
  ;Sine data for 0 through 90 degrees, to 2 places, scaled up
  ;by 100. Cosine is derived from this table as well.
  0
  2
  3
  5
  7
  9
  10
  12
  14
  16
  17
  19
  21
  22
  24
  26
  28
  29
  31
  33
  34
  36
  37
  39
  41
  42
  44
  45
  47
  48
  50
  52
  53
  54
  56
  57
  59
  60
  62
  63
  64
  66
  67
  68
  69
  71
  72
  73
  74
  75
  77
  78
  79
  80
  81
  82
  83
  84
  85
  86
  87
  87
  88
  89
  90
  91
  91
  92
  93
  93
  94
  95
  95
  96
  96
  97
  97
  97
  98
  98
  98
  99
  99
  99
  99
  100
  100
  100
  100
  100
  100
end table

table tanTab as word
  ;Tangent data for 0 through 90 degrees, to 2 places, scaled up
  ;by 100.
  0
  2
  3
  5
  7
  9
  11
  12
  14
  16
  18
  19
  21
  23
  25
  27
  29
  31
  32
  34
  36
  38
  40
  42
  45
  47
  49
  51
  53
  55
  58
  60
  62
  65
  67
  70
  73
  75
  78
  81
  84
  87
  90
  93
  97
  100
  104
  107
  111
  115
  119
  123
  128
  133
  138
  143
  148
  154
  160
  166
  173
  180
  188
  196
  205
  214
  225
  236
  248
  261
  275
  290
  308
  327
  349
  373
  401
  433
  470
  514
  567
  631
  712
  814
  951
  1143
  1430
  1908
  2864
  5729
  65535 ;tan(90) is undefined, but is set to largest value
end table
