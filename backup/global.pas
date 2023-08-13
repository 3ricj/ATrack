unit Global;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DateUtils, Graphics;

type
  TMount = record
    mType: integer;  // NONE, ASCOM, SKYX
    connected: boolean;
    atPark: boolean;
    slewing: boolean;
    tracking: boolean;
    canSetRaRate: string;
    canSetDecRate: string;
    LST: double;
    HA: double;
    AZM: double;
    ALT: double;
    sideofpier: integer;
    RA: double;
    DEC: double;
    raRate: double;
    decRate: double;
  end;

  TImage = record
    fileName: string;
    solved: boolean;
    RAhours: double;     // hours (from pinpoint)
    DECdegrees: double;  // degrees (from pinpoint)
    TelRA: double;
    TelDEC: double;
    UTCDateTime: double; // image time (MJD)
    jd: double;          // julian date
    dateS: string;
    timeS: string;
    nCatalogStars: integer;
    nImageStars: integer;
    airmass: double;
    ExposureInterval: real;
    MatchFitOrder: integer;
    FullWidthHalfMax: double;
    ImageBackgroundMean: double;
    ImageBackgroundSigma: double;
    MatchAvgResidual: double;
    MatchRMSResidual: double;
  end;

  TTelescope = record
    shiftRA: double;
    shiftDEC: double;
    driftRA: double;
    driftDEC: double;
  end;

  TTracking = Record
    status: string;
    savStatus: string;

    trate: double;        // calculated tracking rate used for next image
    savTrate: double;

    imageShift: double;  // RA=Sec, DEC=arcsec
    imageDrift: double;  // RA=Sec, DEC=arcsec

    base: double;        // base rate (calculated)
    model: double;       // model rate (calculated)
    drift: double;       // drift correction (calculated)
    cent: double;        // centering correction (calculated)
    centTime: integer;   // centering correction duration

    abase: double;        // base rate (actual used)
    amodel: double;       // model rate  (actual used)
    adrift: double;       // drift correction (actual used)
    acent: double;        // centering correction (actual used)

    centDistanceMoved: double;    // distance mount moved during centering
    accumDistanceMoved: double;

  end;


const
  TAB = chr($9);
  CR  = chr(13);
  LF  = chr(10);

  mtNONE = 0;
  mtASCOM = 1;
  mtSKYX = 2;

  UNKNOWN = -1;
  EAST = 0;
  WEST = 1;

  function global_RAStringToDouble(raStr: string):double;
  function global_DECStringToDouble(decStr: string): double;
  function global_LONStringToDouble(lonStr: string):double;
  function global_RAToHA(theRA,theLST:double): double;

  function global_HAtoString(HA:double):string;
  function global_RAtoString(RA:double):string;
  function global_DECtoString(DEC:double):string;
  function global_AZMtoString(azm:double):string;
  function global_ALTtoString(alt:double):string;

  function global_GetField(s: string;f: byte;c: char;All: boolean):string;

  function global_LocalSiderealTime(JD: double): double;
  procedure global_ProcessImage;

  var
    NetworkCmdStart: boolean;
    NetworkCmdStop: boolean;

    ATrackRunning: boolean;
    StopRequest: boolean;
    RunRequest: boolean;

    COMStartFlag: boolean;
    COMStopFlag: boolean;
    COMCenterFlag: boolean;

    aMount: TMount;
    PlateSolveError: string;
    FilteringDelay: integer;
    FilteringRA: integer;
    FilteringDEC: integer;
    obsLON: double;
    RAp, DECp: TTracking;

    newImageFileName: string;
    newImage: TImage;
    referenceImage: TImage; // reference image, used to calculate image shift
    prevImage: TImage;      // last image received

    nNewImages: integer;
    nImagesProcessed: integer;
    nPlateSolveFails: integer;
    nImagesUsed: integer;
    nImagesRemaining: integer;

    dt: double;             // this is the time from the previous image to the new image (delphi TDate)
    nSec: double;           // convert dt to nseconds
    StartTime: TDateTime;  // start time of the run session
    eTime: TDateTime;      // elapsed time of run session

    sideOfPier: integer;   // from ASCOM, used to watch for pier flip

    thirdRAImageDrift, previousRAImageDrift: double;
    thirdDECImageDrift, previousDECImageDrift: double;
    prevousNsec: double;


implementation

uses Main, Pinpoint, PID;

FUNCTION GSTIME(JDUT1: EXTENDED ): EXTENDED; forward;
FUNCTION MODFUNC(XVal,Modby: EXTENDED ) : EXTENDED; forward;


//**********************************************************************
// RA String to Double (hours)
// hh mm ss.0000000
// 12 45 7
//**********************************************************************
function global_RAStringToDouble(raStr: string):double;
var
  hours: integer;
  mins: integer;
  secs: real;
  r: double;
  s: string;

begin
// trim
  s := trim(raStr);

// hours
  hours := StrToIntDef(Global_GetField(s,1,' ',false),0);
  if hours < 0 then hours := 0;
  if hours > 23 then hours := 23;

// mins
  mins := StrToIntDef(Global_GetField(s,2,' ',false),0);
  if mins < 0 then mins := 0;
  if mins > 59 then mins := 59;

// secs
  secs := StrToFloatDef(Global_GetField(s,3,' ',true),0);
  if secs < 0.0 then secs := 0.0;
  if secs >= 60.0 then secs := 59.99999;

// decimal hours
  r := hours + (mins/60.0) + (secs/3600.0);

// done
  result := r;

end;



//**********************************************************************
// DEC String to Double (degrees)
// -dd mm ss.s
// 123 56 8
//**********************************************************************
function global_DECStringToDouble(decStr: string): double;
var
  degs: integer;
  mins: integer;
  secs: real;
  r: double;
  s: string;
  sign: real;

begin
// trim
  s := trim(decStr);

// degs
  degs := StrToIntDef(Global_GetField(s,1,' ',false),0);
  if degs < 0 then
    sign := -1.0
  else
    sign := 1.0;
  degs := abs(degs);
  if degs > 89 then degs := 89;

// mins
  mins := StrToIntDef(Global_GetField(s,2,' ',false),0);
  if mins < 0 then mins := 0;
  if mins > 59 then mins := 59;

// secs
  secs := StrToFloatDef(Global_GetField(s,3,' ',true),0);
  if secs < 0.0 then secs := 0.0;
  if secs >= 60.0 then secs := 59.99999;

// degrees
  r := degs + (mins/60.0) + (secs/3600.0);
  if r < 0.0 then r := 0.0;
  if r > 360.0 then r := 359.99999;

// done

  result := r * sign;

end;



function global_HAtoString(HA:double):string;
var
  hours, minutes: integer;
  seconds: double;
  r: double;

begin
// abs value
  r := abs(ha);

// hours
  hours := trunc(r);

// minutes
  r := (r - hours) * 60.0;
  minutes := trunc(r);

// seconds
  seconds := (r - minutes) * 60.0;

  if ha < 0.0 then
    result := '-'
  else
    result := ' ';
  result := result + formatFloat('00',hours) + ' ' +
            formatFloat('00',minutes) + ' ' +
            formatFloat('00',seconds);

end;



function global_RAtoString(RA:double):string;
var
  hours, minutes: integer;
  seconds: double;
  r: double;

begin
// abs value
  r := abs(ra);

// hours
  hours := trunc(r);

// minutes
  r := (r - hours) * 60.0;
  minutes := trunc(r);

// seconds
  seconds := (r - minutes) * 60.0;

  result := formatFloat('00',hours) + ' ' +
            formatFloat('00',minutes) + ' ' +
            formatFloat('00.00',seconds);

end;



function global_DECtoString(DEC:double):string;
var
  degrees, minutes: integer;
  seconds: double;
  r: double;

begin
// abs value
  r := abs(dec);

// degrees
  degrees := trunc(r);

// minutes
  r := (r - degrees) * 60.0;
  minutes := trunc(r);

// seconds
  seconds := (r - minutes) * 60.0;

  if dec < 0.0 then
    result := '-'
  else
    result := '+';
  result := result + formatFloat('00',degrees) + ' ' +
            formatFloat('00',minutes) + ' ' +
            formatFloat('00.0',seconds);

end;


function global_AZMtoString(AZM:double):string;
var
  degrees, minutes: integer;
  seconds: double;
  r: double;

begin
// abs value
  r := abs(AZM);

// hours
  degrees := trunc(r);

// minutes
  r := (r - degrees) * 60.0;
  minutes := trunc(r);

// seconds
  seconds := (r - minutes) * 60.0;

  result := formatFloat('000',degrees) + ' ' +
            formatFloat('00',minutes) + ' ' +
            formatFloat('00.00',seconds);

if length(result) > 12 then
  result := '0';

end;



function global_ALTtoString(ALT:double):string;
var
  degrees, minutes: integer;
  seconds: double;
  r: double;

begin
// abs value
  r := abs(alt);

// degrees
  degrees := trunc(r);

// minutes
  r := (r - degrees) * 60.0;
  minutes := trunc(r);

// seconds
  seconds := (r - minutes) * 60.0;

  if alt < 0.0 then
    result := '-'
  else
    result := ' ';
  result := result + formatFloat('00',degrees) + ' ' +
            formatFloat('00',minutes) + ' ' +
            formatFloat('00.00',seconds);

end;


//**********************************************************************
// Get Field
//   string containing the field
//   field number
//   field delimter character
//   copy remainder of message
//**********************************************************************
function global_GetField(s: string;f: byte;c: char;All: boolean):string;
var
  n: word;

begin
// initialize result
  result := '';

// exit if null string
  if length(s) = 0 then exit;

// field # check
  if f < 1 then exit;

// locate field
  dec(f);
  n := 1;
  while (n <= length(s)) do
  begin
    if f = 0 then break;
    if (s[n] = c) then dec(f);
    inc(n);
  end;

// not found
  if n > length(s) then exit;

// copy over the field
  while n <= length(s) do
  begin
    if (not All) and
       (s[n] = c) then break;
    result := result + s[n];
    inc(n);
  end;
end;



{ ------------------------------------------------------------------------------
|
|                           FUNCTION GSTIME
|
|  This FUNCTION finds the Greenwich SIDEREAL time.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    JDUT1       - Julian Date in UT1             days from 4713 BC
|
|  OutPuts       :
|    GSTIME      - Greenwich SIDEREAL Time        0 to 2Pi rad
|
|  Locals        :
|    Temp        - Temporary variable for reals   rad
|    TUT1        - Julian Centuries from the
|                  Jan 1, 2000 12 h epoch (UT1)
|
|  Coupling      :
|    REALMOD     - MOD FUNCTION for REAL variables
|
|  References    :
|    Vallado       2007, 194, Eq 3-45
 ----------------------------------------------------------------------------- }

FUNCTION GSTIME(JDUT1: EXTENDED ): EXTENDED;
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
     Deg2Rad    : EXTENDED =     Pi/180.0;
   VAR
     Temp, TUT1 : EXTENDED;
   BEGIN
     TUT1:= ( JDUT1 - 2451545.0 ) / 36525.0;
     Temp:= - 6.2E-6*TUT1*TUT1*TUT1
            + 0.093104*TUT1*TUT1
            + (876600.0*3600 + 8640184.812866)*TUT1
            + 67310.54841;  {sec }
     Temp:= MODFUNC( Temp*Deg2Rad/240.0,TwoPi ); { 360/86400 = 1/240, to deg, to rad }

     { ------------------------ Check quadrants --------------------- }
     IF Temp < 0.0 THEN
         Temp:= Temp + TwoPi;

     GSTIME:= (RadToDeg(Temp)/360.0)*24.0;

   END; { FUNCTION GSTIME }



{ ------------------------------------------------------------------------------
|
|                           FUNCTION MODFUNC
|
|  This function performs the MOD operation for REALs.
|
|  Algorithm     : Assign a temporary variable
|                  Subtract off an INTEGER number of values while the xval is
|                     too large
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - Value to MOD
|    ModBy       - Value to MOD with
|
|  OutPuts       :
|    MODFUNC     - Result                         -ModBy <=  Answer  <= +ModBy
|
|  Locals        :
|    TempValue   - Temporary EXTENDED value
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION MODFUNC(XVal,Modby: EXTENDED ) : EXTENDED;
   VAR
       TempValue: EXTENDED;
   BEGIN
     TempValue := XVal;
     WHILE ABS(TempValue) > ModBy DO
         TempValue:= TempValue - INT(XVal/ModBy)*ModBy;
     MODFUNC:= TempValue;
   END;  { FUNCTION MODFUNC }



//*****************************************************************************
// Local Sidereal Time (given Julian Date)
//*****************************************************************************
function global_LocalSiderealTime(JD: double): double;
var
  gst: double;
  offset: double;
  lst: double;

begin
// greenwhich sidereal time
  gst := GSTIME(jd);

// offset in hours from greenwhich
  offset := (obsLon/360.0) * 24.0;

// local sidereal time
  lst := gst + offset;
  if lst >= 24.0 then
    lst := lst - 24.0;

// done
  result := lst;

end;


//**********************************************************************
// LON String to Double (degrees)
// ddd mm ss.00
// 123 45 67890
//**********************************************************************
function global_LONStringToDouble(lonStr: string):double;
var
  degrees: integer;
  mins: integer;
  secs: real;
  r: double;
  s: string;

begin
// trim
  s := trim(lonStr);

// degrees
  degrees := StrToIntDef(Global_GetField(s,1,' ',false),0);
  if degrees > 180 then degrees := 180;

// mins
  mins := StrToIntDef(Global_GetField(s,2,' ',false),0);
  if mins < 0 then mins := 0;
  if mins > 59 then mins := 59;

// secs
  secs := StrToFloatDef(Global_GetField(s,3,' ',true),0);
  if secs < 0.0 then secs := 0.0;
  if secs >= 60.0 then secs := 59.99999;

// decimal degrees
  r := degrees + (mins/60.0) + (secs/3600.0);
  if r > 180.0 then r := 180.0;

// done
  result := r;

end;

//******************************************************************************
// ProcessImage
//******************************************************************************
procedure Global_ProcessImage;
var
  r: double;

begin
// filename
  newImage.fileName := newImageFileName;

// plate solve new image
  if not PinPoint_PlateSolve then
  begin
    inc(nPlateSolveFails);
    exit;
  end;

// prepare PID
  thirdRAImageDrift := previousRAImageDrift;
  previousRAImageDrift := RAp.ImageDrift;

  thirdDECImageDrift := previousDECImageDrift;
  previousDECImageDrift := DECp.ImageDrift;

  prevousNsec = nSec;

// update counter
  inc(nImagesUsed);

// update image data
  main_Form.nImageStars_Edit.text := IntToStr(newImage.nImageStars);
  main_Form.ImageFWHM_Edit.text := formatfloat('0.00',newImage.FullWidthHalfMax);
  main_Form.ImageBackground_Edit.text := formatfloat('0.00',newImage.ImageBackgroundMean);

// first image
  if not prevImage.solved then
    prevImage := newImage;

// create Reference Image
  if (not referenceImage.solved) then
  begin
    referenceImage := newImage;
    Main_Form.CenterOnTargetRA_MaskEdit.text := Global_RAToString(referenceImage.RAhours);
    Main_Form.CenterOnTargetRA_MaskEdit.font.color := clLime;

    Main_Form.CenterOnTargetDEC_MaskEdit.text := Global_DECToString(referenceImage.DECdegrees);
    Main_Form.CenterOnTargetDEC_MaskEdit.font.color := clLime;
  end;

// time since previous image (adjust time to midpoint of the image)
  dt := (newImage.UTCDateTime + (newImage.ExposureInterval/2.0)) - (prevImage.UTCDateTime+(prevImage.ExposureInterval/2.0));
  nSec := dt * 86400.0;


// image shift = new image pos - reference image pos
  RAp.ImageShift := 0;
  DECp.ImageShift := 0;
  if referenceImage.solved then
  begin
    r := (newImage.RAhours - referenceImage.RAhours);
    if abs(r) > 12.0 then
      if r < 0.0 then
        r := r + 24.0
      else
        r := r - 24.0;
    RAp.ImageShift := r * 3600.0 * 15.0;
    DECp.ImageShift := (newImage.DECdegrees - referenceImage.DECdegrees) * 3600.0;
  end;

// image drift = new image pos - prev image pos
  RAp.ImageDrift := 0;
  DECp.ImageDrift := 0;
  if nImagesUsed > 1 then
  begin
    r := (newImage.RAhours - prevImage.RAhours);
    if abs(r) > 12.0 then
      if r < 0.0 then
        r := r + 24.0
      else
        r := r - 24.0;
    RAp.ImageDrift := r * 3600.0 * 15.0;
    DECp.ImageDrift := (newImage.DECdegrees - prevImage.DECdegrees) * 3600.0;
  end;

end;

//******************************************************************************
// RA and LST to Hour Angle
//******************************************************************************
function global_RAToHA(theRA,theLST:double): double;
var
  r,rLST,rTRA: double;

begin
  rLST := theLST;
  rTRA := theRA;

  if rTRA < rLST then rTRA := rTRA + 24.0;

  r := rTRA - rLST;

  if r > 12.0 then
    r := 24.0 - r
  else
    r := -r;

  result := r;

end;

end.

