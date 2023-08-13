unit PID;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure PID_Init;
procedure PID_Reset;
procedure PID_LoadCoefficients;
procedure PID_NewImage;

var
  thirdRAImageDrift, thirdDECImageDrift: double;
  previousRAImageDrift, previousDECImageDrift: double;
  previousNsec: double;

  RA_der0, DEC_der0: double;
  RA_der1, DEC_der1: double;

  RA_Kp, DEC_Kp: double;
  RA_Ki, DEC_Ki: double;
  RA_Kd, DEC_Kd: double;
  RA_nFilt, DEC_nFilt: double;

  RA_new_rate, DEC_new_rate: double;

  RA_tau, DEC_tau: double;
  RA_alpha, DEC_alpha: double;

  RA_filtder, DEC_filtder: double;
  RA_new_rate_Dfilt, DEC_new_rate_Dfilt: double;

implementation

uses Main, global;

//******************************************************************************
// Init
// Called at start of program
//******************************************************************************
procedure PID_Init;
begin
  // do a reset
  PID_Reset;
end;

//******************************************************************************
// Reset
// Called at start of new observing run
//******************************************************************************
procedure PID_Reset;
begin
  thirdRAImageDrift := 0;
  previousRAImageDrift := 0;

  thirdDECImageDrift := 0;
  previousDECImageDrift := 0;

  previousNsec := 0;

  RA_der0 := 0.0;
  RA_der1 := 0.0;

  DEC_der0 := 0.0;
  DEC_der1 := 0.0;

end;

//******************************************************************************
// Load Coefficients
// Called when new image received
//******************************************************************************
procedure PID_LoadCoefficients;
begin
  RA_Kp := StrToFloatDef(Main_Form.KpRA_MaskEdit.EditText,0.00000);
  DEC_Kp := StrToFloatDef(Main_Form.KpDEC_MaskEdit.EditText,0.00000);

  RA_Ki := StrToFloatDef(Main_Form.KiRA_MaskEdit.EditText,0.00000);
  DEC_Ki := StrToFloatDef(Main_Form.KiDEC_MaskEdit.EditText,0.00000);

  RA_Kd := StrToFloatDef(Main_Form.KdRA_MaskEdit.EditText,0.00000);
  DEC_Kd := StrToFloatDef(Main_Form.KdDEC_MaskEdit.EditText,0.00000);

  RA_nFilt := StrToFloatDef(Main_Form.nFiltRA_MaskEdit.EditText,0.00000);
  DEC_nFilt := StrToFloatDef(Main_Form.nFiltDEC_MaskEdit.EditText,0.00000);

end;

//******************************************************************************
// New image
//******************************************************************************
procedure PID_NewImage;
var
  a, b, c, d, e, f: double;

begin
  if nImagesUsed = 0 then exit;
  PID_LoadCoefficients;

  if nSec <= 0 then
  begin
    RA_new_rate :=  RA_Ki * RAp.ImageShift;
    DEC_new_rate := DEC_Ki * DECp.ImageShift;
    exit;
  end;

  a := RA_Kp * (RAp.ImageDrift / nSec);
  b := RA_Ki * RAp.ImageShift;
  c := RAp.ImageDrift / nSec;
  d := previousRAImageDrift / previousNsec;
  e := (c - d) / nSec;
  f := RA_Kd * e;
  RA_new_rate :=  a + b + f;

  a := DEC_Kp * (DECp.ImageDrift / nSec);
  b := DEC_Ki * DECp.ImageShift;
  c := DECp.ImageDrift / nSec;
  d := previousDECImageDrift / previousNsec;
  e := (c - d) / nSec;
  f := DEC_Kd * e;
  DEC_new_rate :=  a + b + f;

  if nImagesUsed < 3 then exit;

  // derivative filtered correction
  // RA
  d := RA_Kp * RA_nfilt;
  if not(d = 0.0) then
    RA_tau := RA_Kd / d
  else
    RA_tau := 0.0;

  d := 2.0 * RA_tau;
  if not(d = 0.0) then
    RA_alpha := nSec / d
  else
    RA_alpha := 0.0;

  RA_der1 := RA_der0;
  if not(previousNsec = 0.0) then
  begin
    a := (RAp.ImageDrift - previousRAImageDrift) / nSec;
    b := (previousRAImageDrift - thirdRAImageDrift) / previousNsec;
    c := a - b;
    RA_der0 := RA_Kd * c;
  end
  else
    RA_der0 := 0.0;

  d := RA_alpha + 1;
  if not(d = 0.0) then
  begin
    a := RA_alpha / d;
    b := RA_der0 + RA_der1;
    c := (RA_alpha - 1) / d;
    e := c * RA_der1;
    RA_filtder := (a * b) - e;
  end
  else
    RA_filtder := 0.0;

  a := (RA_Kp * RAp.ImageDrift) / nSec;
  b := RA_Ki * RAp.ImageShift;
  RA_new_rate_Dfilt :=  a + b + RA_filtder;

  // DEC

end;


end.

