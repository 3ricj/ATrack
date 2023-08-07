unit Ascom;

interface
uses
  SysUtils, Classes, Graphics, Controls,
  Dialogs, StdCtrls, ExtCtrls, Variants, COMObj, COMCtrls;

procedure Ascom_Open;
procedure Ascom_Chooser;
procedure Ascom_Connect;
procedure Ascom_Status;
procedure Ascom_SetRATrackingRate(r:double);
procedure Ascom_SetDECTrackingRate(r:double);

Var
  AscomMountDriver: string;
  AscomMount: variant;


implementation

uses Main, global;


//******************************************************************************
// Open
//******************************************************************************
procedure Ascom_Open;
begin
  AscomMountDriver := Main_Form.TelescopeDriver_Edit.text;
end;

//******************************************************************************
// Mount chooser
//******************************************************************************
procedure Ascom_Chooser;
var
  V: variant;
  s: wideString;

begin
  AscomMountDriver := 'Telescope';
  try
    V := CreateOleObject('Ascom.Utilities.Chooser');
  except
    ShowMessage('Connection to ASCOM chooser failed');
    exit;
  end;

  V.devicetype:=widestring('Telescope');
  s := V.Choose(widestring(Main_Form.TelescopeDriver_Edit.text));
  AscomMountDriver := AnsiString(s);
  Main_Form.TelescopeDriver_Edit.text := AscomMountDriver;
  V := unassigned;

end;

//******************************************************************************
// Telescope driver connect
//******************************************************************************
procedure Ascom_Connect;
begin
// mount not ASCOM
  if aMount.mType = mtSkyX then
  begin
    ShowMessage('Mount currently assigned to SkyX, please disconnect first');
    exit;
  end;

// cannot connect if unassigned
  if AscomMountDriver = '' then exit;

// toggle state
  if not VarIsEmpty(AscomMount) then
  begin
    AscomMount.connected:=false;
    AscomMount := Unassigned;
    aMount.mType := mtNONE;
    aMount.connected := false;
  end
  else
  begin
    AscomMount := CreateOleObject(AscomMountDriver);
    AscomMount.connected:=true;
    aMount.connected := true;
    aMount.mType := mtASCOM;
  end;

end;

//**********************************************************************
// Mount Status
//**********************************************************************
procedure ASCOM_Status;
begin
// ignore it
  if not (aMount.mType = mtASCOM) then exit;

// not connected
  if VarIsEmpty(AscomMount) then exit;

// update mount parameters
  aMount.atPark := AscomMount.atPark;
  aMount.slewing := AscomMount.slewing;
  aMount.tracking := AscomMount.tracking;

  if AscomMount.canSetRightAscensionRate then
    aMount.canSetRaRate := 'YES'
  else
  aMount.canSetRaRate := 'NO';

  if AscomMount.canSetDeclinationRate then
    aMount.canSetDecRate := 'YES'
  else
  aMount.canSetDecRate := 'NO';

  aMount.RA := AscomMount.RightAscension;
  aMount.DEC := AscomMount.Declination;
  aMount.LST := AscomMount.SiderealTime;
  aMount.AZM := AscomMount.Azimuth;
  aMount.ALT := AscomMount.Altitude;
  aMount.HA := global_RAToHA(aMount.RA,aMount.LST);
  aMount.sideofpier := AscomMount.sideofpier;
  aMount.raRate := AscomMount.RightAscensionRate * 15.0; // convert RA Sec to arcsec
  aMount.decRate := AscomMount.DeclinationRate;

end;



//******************************************************************************
// set ra rate
//******************************************************************************
procedure Ascom_SetRATrackingRate(r:double);
begin
// ignore it
  if not (aMount.mType = mtASCOM) then exit;

// not connected
  if not aMount.connected then exit;

// update drive
  AscomMount.RightAscensionRate := r / 15.0;  // arcsec to RA Sec per sidereal second

end;

//******************************************************************************
// set dec rate (input is arcseconds / SI second)
//******************************************************************************
procedure Ascom_SetDECTrackingRate(r:double);
begin
// ignore it
  if not (aMount.mType = mtASCOM) then exit;

// not connected
  if not aMount.connected then exit;

// update drive
  AscomMount.DeclinationRate := r;

end;

end.
