unit Mount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics;

procedure Mount_Open;
procedure Mount_Interface(theInterface:integer);
procedure Mount_UpdateStatus;
procedure Mount_SetTrackingRates(rRA,rDEC: double);

implementation

Uses Main, Ascom, Global;

procedure UpdateIndicators; forward;
procedure Reset; forward;

var
  mountLogFileName: string;


//******************************************************************************
// Open
//******************************************************************************
procedure Mount_Open;
begin
  Ascom_Open;
  mountLogFileName := Main_Form.Images_DirectoryEdit.Text + '\ATrack Mount Log ' + FormatDateTime('ddmmyyyyhhnnss',now) + '.txt';
end;

//******************************************************************************
// Select mount interface
//******************************************************************************
procedure Mount_Interface(theInterface:integer);
begin
  Main_Form.Mount_Timer.enabled := false;
  Ascom_Connect;
  if aMount.connected then
    Main_Form.MountInterfaceASCOM_BCButton.down := true
  else
    Main_Form.MountInterfaceASCOM_BCButton.down := false;

// 1 second status timer
  if aMount.connected then
    Main_Form.Mount_Timer.enabled := true;

  UpdateIndicators;

end;

//******************************************************************************
// Set tracking rates
//******************************************************************************
procedure Mount_SetTrackingRates(rRA, rDEC: double);
var
  dRA, dDEC: double;
  r: double;

begin

// not assigned
  if aMount.mType = mtNONE then
  begin
    exit;
  end;

// not connected
  if not aMount.connected then
  begin
    exit;
  end;

// slewing
  if aMount.slewing then
  begin
    exit;
  end;

// not tracking
//  if not aMount.tracking then
//  begin
//    exit;
//    MountLog('Set Tracking Rate: not tracking');
//  end;

// parked
  if aMount.atPark then
  begin
    exit;
  end;

// init
  dRA := rRA;
  dDEC := rDEC;

// limit check
  r := StrToFloatDef(Main_Form.CorrectionLimitRA_MaskEdit.EditText,0.0);
  if abs(dRA) > r then
    if dRA < 0 then
      dRA := -r
    else
      dRA := r;

  r := StrToFloatDef(Main_Form.CorrectionLimitDEC_MaskEdit.EditText,0.0);
  if abs(dDEC) > r then
    if dDEC < 0 then
      dDEC := -r
    else
      dDEC := r;

// set tracking rate
  ASCOM_SetRATrackingRate(dRA);
  ASCOM_SetDECTrackingRate(dDEC);

// update status
  Mount_UpdateStatus;
  UpdateIndicators;

end;


//******************************************************************************
// update status
// called from timer event
//******************************************************************************
procedure Mount_UpdateStatus;
begin
// ASCOM mount
  ASCOM_Status;

// update indicators
  UpdateIndicators;

end;

//******************************************************************************
// update mount indicators
//******************************************************************************
procedure UpdateIndicators;
var
  fColor: TColor;

begin
// init
  if aMount.connected then
    fColor := clLime
  else
    fColor := clRed;

// select buttons
  if aMount.mType = mtNone then
  begin
    Main_Form.MountInterfaceASCOM_BCButton.down := false;
  end;

  if aMount.mType = mtAscom then
  begin
    Main_Form.TelescopeDriver_Edit.Font.color := clLime;
    Main_Form.MountInterfaceASCOM_BCButton.down := true;
  end;

  if aMount.mType = mtSkyX then

// Parked LED
  if aMount.atPark then
    Main_Form.MountParked_Edit.Font.color := clLime
  else
    Main_Form.MountParked_Edit.Font.color := clRed;
  if aMount.mType = mtNone then
    Main_Form.MountParked_Edit.Font.color := clRed;

// Tracking LED
  if aMount.Tracking then
    Main_Form.MountTracking_Edit.Font.color := clLime
  else
    Main_Form.MountTracking_Edit.Font.color := clRed;
  if aMount.mType = mtNone then
    Main_Form.MountTracking_Edit.Font.color := clRed;

// Slewing LED
  if aMount.Slewing then
    Main_Form.MountSlewing_Edit.Font.color := clLime
  else
    Main_Form.MountSlewing_Edit.Font.color := clRed;
  if aMount.mType = mtNone then
    Main_Form.MountSlewing_Edit.Font.color := clRed;

// can set RA rate
  Main_Form.MountAdaptiveTrackingRA_Edit.Font.color := fColor;
  Main_Form.MountAdaptiveTrackingRA_Edit.Text := aMount.canSetRaRate;

// can set DEC rate
  Main_Form.MountAdaptiveTrackingDEC_Edit.Font.color := fColor;
  Main_Form.MountAdaptiveTrackingDEC_Edit.Text := aMount.canSetRaRate;

// LST
  Main_Form.MountLST_Edit.Font.color := fColor;
  Main_Form.MountLST_Edit.Text := copy(global_RAtoString(aMount.LST),1,8);

// ASCOM RA
  Main_Form.MountRA_Edit.Font.color := fColor;
  Main_Form.MountRA_Edit.Text := global_RAtoString(aMount.RA);

// ASCOM DEC
  Main_Form.MountDEC_Edit.Font.color := fColor;
  Main_Form.MountDEC_Edit.Text := global_DECtoString(aMount.DEC);

// HA
  Main_Form.MountHA_Edit.Font.color := fColor;
  Main_Form.MountHA_Edit.Text := global_HAtoString(aMount.HA);
  Main_Form.MountHA_Edit1.Font.color := fColor;
  Main_Form.MountHA_Edit1.Text := Main_Form.MountHA_Edit.text;

// AZM
  Main_Form.MountAZM_Edit.Font.color := fColor;
  Main_Form.MountAZM_Edit.Text := global_AZMtoString(aMount.AZM);

// ALT
  Main_Form.MountALT_Edit.Font.color := fColor;
  Main_Form.MountALT_Edit.Text := global_ALTtoString(aMount.ALT);

// side of pier
  Main_Form.MountPierSide_Edit.Font.color := fColor;
  case aMount.SideOfPier of
     EAST: Main_Form.MountPierSide_Edit.Text := 'Pier East';
     WEST: Main_Form.MountPierSide_Edit.Text := 'Pier West';
  else
    Main_Form.MountPierSide_Edit.Text := 'Unknown';
  end;

  Main_Form.DriveTrackingRateRA_Edit.Font.Color := fColor;
  Main_Form.DriveTrackingRateDEC_Edit.Font.Color := fColor;

// RA rates
  Main_Form.MountRArate_Edit.Font.color := fColor;
  Main_Form.MountRArate_Edit.Text := formatfloat('00.00000',aMount.raRate);
  Main_Form.DriveTrackingRateRA_Edit.text := Main_Form.MountRArate_Edit.Text;

// DEC rates
  Main_Form.MountDECrate_Edit.Font.color := fColor;
  Main_Form.MountDECrate_Edit.Text := formatfloat('00.00000',aMount.decRate);
  Main_Form.DriveTrackingRateDEC_Edit.text := Main_Form.MountDECrate_Edit.Text;

end;

//******************************************************************************
// reset
//******************************************************************************
procedure Reset;
begin
  aMount.canSetRaRate := ' ';
  aMount.canSetDecRate := ' ';
  aMount.tracking := false;
  aMount.slewing := false;
  aMount.LST := 0.0;
  aMount.HA := 0.0;
  aMount.AZM := 0.0;
  aMount.ALT := 0.0;
  aMount.sideofpier := 2;
  aMount.RA := 0.0;
  aMount.DEC := 0.0;
  aMount.raRate := 0.0;
  aMount.decRate := 0.0;
  aMount.connected := false;

  UpdateIndicators;
end;


end.

