unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, EditBtn, MaskEdit, Grids, Buttons, BCButton, BCMDButton, BCListBox,
  BCTrackbarUpdown, DateUtils, InterfaceBase, IdDayTimeUDP, IdUDPClient,
  IdUDPServer, windows, fileUtil, IdSocketHandle, IdGlobal;

type

  { TMain_Form }

  TMain_Form = class(TForm)
    ATStatus_Label: TLabel;
    BackgroundTileSize_MaskEdit: TMaskEdit;
    BaseDEC_Edit: TEdit;
    nFiltDEC_MaskEdit: TMaskEdit;
    nFiltRA_MaskEdit: TMaskEdit;
    Label139: TLabel;
    Label140: TLabel;
    Label55: TLabel;
    PIDDECCorrection_Edit: TEdit;
    BaseDriveTrackingRatesRESET_BCButton: TBCButton;
    BaseDriveTrackingRatesSET_BCButton: TBCButton;
    BaseRA_Edit: TEdit;
    PIDRACorrection_Edit: TEdit;
    BaseTrackingRateDEC_MaskEdit: TMaskEdit;
    BaseTrackingRateRA_MaskEdit: TMaskEdit;
    Base_DEC_CheckBox: TCheckBox;
    Base_RA_CheckBox: TCheckBox;
    catalog_combobox: TComboBox;
    CatExpansion_MaskEdit: TMaskEdit;
    CatMinMag_MaskEdit: TMaskEdit;
    catPath_DirectoryEdit: TDirectoryEdit;
    CenteringDECTimer_Edit: TEdit;
    CenteringDEC_Edit: TEdit;
    CenteringRate_MaskEdit: TMaskEdit;
    CenteringRATimer_Edit: TEdit;
    CenteringRA_Edit: TEdit;
    Centering_BCButton: TBCButton;
    Centering_DEC_CheckBox: TCheckBox;
    Centering_RA_CheckBox: TCheckBox;
    CenterOnTargetDEC_MaskEdit: TMaskEdit;
    CenterOnTargetRA_MaskEdit: TMaskEdit;
    CenterOnTarget_CheckBox: TCheckBox;
    CentroidAlgorithm_combobox: TComboBox;
    Choose_BCButton: TBCButton;
    ConvolveGaussian_CheckBox: TCheckBox;
    ConvolveLog_CheckBox: TCheckBox;
    CorrectionLimitDEC_MaskEdit: TMaskEdit;
    PIDDECFilteredCorrection_Edit: TEdit;
    PIDRAFilteredCorrection_Edit: TEdit;
    Label116: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label127: TLabel;
    Label128: TLabel;
    Label129: TLabel;
    Label132: TLabel;
    Label135: TLabel;
    Label137: TLabel;
    Label153: TLabel;
    Label163: TLabel;
    PIDTrackingCorrection_CheckBox: TCheckBox;
    KpDEC_MaskEdit: TMaskEdit;
    KiDEC_MaskEdit: TMaskEdit;
    KdDEC_MaskEdit: TMaskEdit;
    CorrectionLimitRA_MaskEdit: TMaskEdit;
    KpRA_MaskEdit: TMaskEdit;
    KiRA_MaskEdit: TMaskEdit;
    KdRA_MaskEdit: TMaskEdit;
    delayCounter_Edit: TEdit;
    DriftCorrectionDEC_Edit: TEdit;
    DriftCorrectionRateLimitDEC_MaskEdit: TMaskEdit;
    DriftCorrectionRateLimitRA_MaskEdit: TMaskEdit;
    DriftCorrectionRA_Edit: TEdit;
    DriftCorrection_BCButton: TBCButton;
    Drift_DEC_CheckBox: TCheckBox;
    Drift_RA_CheckBox: TCheckBox;
    DriveTrackingRateDEC_Edit: TEdit;
    DriveTrackingRateRA_Edit: TEdit;
    dT_Edit: TEdit;
    ElapsedTime_Edit: TEdit;
    ExclusionBorder_MaskEdit: TMaskEdit;
    FilterFWHM_MaskEdit: TMaskEdit;
    ForceCenteringTime_MaskEdit: TMaskEdit;
    ForceTargetCentering_BCButton: TBCButton;
    HotPixelsThreshold_MaskEdit: TMaskEdit;
    hPlateScale_MaskEdit: TMaskEdit;
    IdUDPServer1: TIdUDPServer;
    ATrack_Timer: TTimer;
    ImageBackground_Edit: TEdit;
    ImageCoordinatesDEC_Edit: TEdit;
    ImageCoordinatesRA_Edit: TEdit;
    ImageFWHM_Edit: TEdit;
    ImageMask_Edit: TEdit;
    ImageShift1stImageDEC_Edit: TEdit;
    ImageShift1stImageRA_Edit: TEdit;
    ImageShiftPrevImageDEC_Edit: TEdit;
    ImageShiftPrevImageRA_Edit: TEdit;
    ImagesSubFolder_Edit: TEdit;
    Images_DirectoryEdit: TDirectoryEdit;
    ImageTime_Edit: TEdit;
    InnerAperture_MaskEdit: TMaskEdit;
    InvertDEC_CheckBox: TCheckBox;
    InvertRA_CheckBox: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label12: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Label126: TLabel;
    Label13: TLabel;
    Label130: TLabel;
    Label131: TLabel;
    Label133: TLabel;
    Label134: TLabel;
    Label136: TLabel;
    Label138: TLabel;
    Label14: TLabel;
    Label144: TLabel;
    Label15: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label152: TLabel;
    Label156: TLabel;
    Label159: TLabel;
    Label16: TLabel;
    Label160: TLabel;
    Label161: TLabel;
    Label162: TLabel;
    DirMonitorWait_Timer: TTimer;
    CenteringDEC_Timer: TTimer;
    Label111: TLabel;
    Label167: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label8: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label9: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    LoadProfile_BCButton: TBCButton;
    Log_StringGrid: TStringGrid;
    maxMag_MaskEdit: TMaskEdit;
    maxSolveStars_MaskEdit: TMaskEdit;
    maxSolveTime_MaskEdit: TMaskEdit;
    NetworkLog_Memo: TMemo;
    MinMatchStars_MaskEdit: TMaskEdit;
    MinStarSize_MaskEdit: TMaskEdit;
    MountAdaptiveTrackingDEC_Edit: TEdit;
    MountAdaptiveTrackingRA_Edit: TEdit;
    MountALT_Edit: TEdit;
    MountAZM_Edit: TEdit;
    MountDECrate_Edit: TEdit;
    MountDEC_Edit: TEdit;
    MountHA_Edit: TEdit;
    MountHA_Edit1: TEdit;
    MountInterfaceASCOM_BCButton: TBCButton;
    MountLST_Edit: TEdit;
    MountParked_Edit: TEdit;
    MountPierSide_Edit: TEdit;
    MountPollingRate_MaskEdit: TMaskEdit;
    MountRArate_Edit: TEdit;
    MountRA_Edit: TEdit;
    MountSlewing_Edit: TEdit;
    MountTracking_Edit: TEdit;
    nImagesProcessed_Edit: TEdit;
    nImageStars_Edit: TEdit;
    nImagesUsed_Edit: TEdit;
    nNewImages_Edit: TEdit;
    nPlateSolveFails_Edit: TEdit;
    OpenDialog: TOpenDialog;
    OuterAperture_MaskEdit: TMaskEdit;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PIDDerivitaveFiltering_CheckBox1: TCheckBox;
    Profile_Edit: TEdit;
    ProjectionType_ComboBox: TComboBox;
    RemoveHotPixels_CheckBox: TCheckBox;
    RUN_Button: TButton;
    RUN_Panel: TPanel;
    SaveDialog: TSaveDialog;
    Mount_Timer: TTimer;
    SaveProfile_BCButton: TBCButton;
    Session_Timer: TTimer;
    CenteringRA_Timer: TTimer;
    DecS_MaskEdit1: TMaskEdit;
    ImagesFolder_DirectoryEdit1: TDirectoryEdit;
    SigmaAboveMean_MaskEdit: TMaskEdit;
    SkipNImages_MaskEdit: TMaskEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TelescopeDriver_Edit: TEdit;
    UseFaintStars_CheckBox: TCheckBox;
    UsePinPoint_CheckBox: TCheckBox;
    UseSExtractor_CheckBox: TCheckBox;
    vPlateScale_MaskEdit: TMaskEdit;
    WatchFolder_BCButton: TBCButton;
    procedure ATrack_TimerTimer(Sender: TObject);
    procedure ForceTargetCentering_BCButtonButtonClick(Sender: TObject);
    procedure IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure RUN_ButtonClick(Sender: TObject);
    procedure ToggleTrackingCorrections_CheckBoxClick(Sender: TObject);
    procedure Mount_TimerTimer(Sender: TObject);
    procedure BaseDriveTrackingRatesRESET_BCButtonClick(Sender: TObject);
    procedure CenteringDEC_TimerTimer(Sender: TObject);
    procedure CenteringRA_TimerTimer(Sender: TObject);
    procedure DirMonitorWait_TimerTimer(Sender: TObject);
    procedure FunctionToggle_BCButtonClick(Sender: TObject);
    procedure MountInterface_BCButtonClick(Sender: TObject);
    procedure Session_TimerTimer(Sender: TObject);
    procedure ToggleColor_BCButtonClick(Sender: TObject);
    procedure FixNegativeSign_MaskEditChange(Sender: TObject);
    procedure Choose_BCButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BaseDriveTrackingRatesSET_BCButtonClick(Sender: TObject);
    procedure LoadProfile_BCButtonClick(Sender: TObject);
    procedure SaveProfile_BCButtonClick(Sender: TObject);
  private
    FEventHandler: PEventHandler;
    FWaitHandle: THandle;
    procedure ClearEventHandler;
    procedure DirChanged(AData: PtrInt; AFlags: DWORD);
  public
  end;

var
  Main_Form: TMain_Form;
  logFileName: string;
  processingImage: boolean;
  F_Log: TextFile;
  logFileOpen: boolean;
  skipNImages: integer;

procedure Main_Log(s: string);
procedure Main_StartDirectoryWatch;
procedure Main_StopDirectoryWatch;

implementation

{$R *.lfm}
uses Config, Run, Centering, Drift, Ascom, PID,
     Pinpoint, Profiles, Global, Mount, Network;

var
  oldFileList, newFileList: TStringList;
  watchFolder: string;

{ TMain_Form }

//******************************************************************************
// Form Create
//******************************************************************************
procedure TMain_Form.FormCreate(Sender: TObject);
begin

// init
  ATrackRunning := false;
  StopRequest := false;
  RunRequest := false;
  oldFileList := TStringList.create;
  newFileList := TStringList.create;

// mount type
  aMount.mType := mtNONE;

// load previous configuration
  Config_Load;

// Subsystems
  Profiles_Init;
  Run_Open;
  PID_Init;
  Drift_Init;
  Centering_Init;
  PinPoint_Open;
  Mount_Open;
  Network_Open;

// version
// Caption := 'Crystal Lake Observatory - ATrack (v' + GetFileVersion + ')';

// start system timer
  ATrack_Timer.enabled := true;

end;

//******************************************************************************
//  Form Close
//******************************************************************************
procedure TMain_Form.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Network_Close;
  logFileOpen := false;
  Config_Close;
  PinPoint_Close;
  oldFileList.free;
  newFileList.free;
end;

//******************************************************************************
// Set Base Tracking rates
//******************************************************************************
procedure TMain_Form.BaseDriveTrackingRatesSET_BCButtonClick(Sender: TObject);
begin
  Run_SetBase;
end;

//******************************************************************************
// Load Profile
//******************************************************************************
procedure TMain_Form.LoadProfile_BCButtonClick(Sender: TObject);
var
  r: double;

begin
// read profile file
  Profiles_Load;

// set base tracking rate
  r := StrToFloatDef(Main_Form.BaseTrackingRateRA_MaskEdit.editText,0);
  RAp.base := r;

  r := StrToFloatDef(Main_Form.BaseTrackingRateDEC_MaskEdit.editText,0);
  DECp.base := r;

  Run_UpdateIndicators;
  Run_UpdateTrackingRates;

end;

//******************************************************************************
// Log add entry
//******************************************************************************
procedure Main_Log(s: string);
begin
  if not logFileOpen then exit;
  WriteLn(F_Log, FormatDateTime('hh.mm.ss',now) + TAB + s);
end;

//******************************************************************************
// Save Profile
//******************************************************************************
procedure TMain_Form.SaveProfile_BCButtonClick(Sender: TObject);
begin
  Profiles_Save;
end;

//******************************************************************************
// Mount Timer event
//******************************************************************************
procedure TMain_Form.Mount_TimerTimer(Sender: TObject);
begin
  Mount_UpdateStatus;
  Main_Form.Mount_Timer.Interval := StrToIntDef(Main_Form.MountPollingRate_MaskEdit.EditText,1) * 1000;

end;

//******************************************************************************
// Reset base tracking rates
//******************************************************************************
procedure TMain_Form.BaseDriveTrackingRatesRESET_BCButtonClick(Sender: TObject);
var
  r: double;

begin
  Main_Form.BaseTrackingRateRA_MaskEdit.text := '0';
  r := StrToFloatDef(Main_Form.BaseTrackingRateRA_MaskEdit.editText,0);
  RAp.base := r;

  Main_Form.BaseTrackingRateDEC_MaskEdit.text := '0';
  r := StrToFloatDef(Main_Form.BaseTrackingRateDEC_MaskEdit.editText,0);
  DECp.base := r;

  Run_UpdateIndicators;
  Run_UpdateTrackingRates;

end;

//******************************************************************************
// DEC Centering timer event
//******************************************************************************
procedure TMain_Form.CenteringDEC_TimerTimer(Sender: TObject);
begin
  Centering_TimeoutDEC;
end;

//******************************************************************************
// RA Centering timer event
//******************************************************************************
procedure TMain_Form.CenteringRA_TimerTimer(Sender: TObject);
begin
  Centering_TimeoutRA;
end;

//******************************************************************************
// Watch Folder wait timer event
//******************************************************************************
procedure TMain_Form.DirMonitorWait_TimerTimer(Sender: TObject);
begin
// disable timer
  DirMonitorWait_Timer.Enabled := false;

// process new image
  Run_NewImage;

end;

//******************************************************************************
// Toggle color of BC buttons
//******************************************************************************
procedure TMain_Form.ToggleColor_BCButtonClick(Sender: TObject);
begin
  with Sender as TBCButton do
    if Down then Down := false
  else
    Down := true;
end;

//******************************************************************************
// Fix negative sign on mask edit
//******************************************************************************
procedure TMain_Form.FixNegativeSign_MaskEditChange(Sender: TObject);
var
  s: string;

begin
  with Sender as TMaskEdit do
  begin
    s := Text;
    if not (s[1] = '-') then s[1] := '+';
    Text := s;
  end;
end;

//******************************************************************************
// ASCOM chooser event
//******************************************************************************
procedure TMain_Form.Choose_BCButtonClick(Sender: TObject);
begin
  ASCOM_Chooser;
end;

//******************************************************************************
//  Function toggles
//******************************************************************************
procedure TMain_Form.FunctionToggle_BCButtonClick(Sender: TObject);
begin
  with Sender as TBCButton do
    if down then
      down := false
    else
      down := true;

  if Sender = Centering_BCButton then
    Centering_ButtonEvent;

  Run_UpdateTrackingRates;

end;

//******************************************************************************
// Mount select
//******************************************************************************
procedure TMain_Form.MountInterface_BCButtonClick(Sender: TObject);
begin
  Mount_Interface((Sender as TBCButton).tag);
end;

//******************************************************************************
// Session timer event
//******************************************************************************
procedure TMain_Form.Session_TimerTimer(Sender: TObject);
begin
  Run_UpdateElapsedTime;
end;

//******************************************************************************
// Toggle Tracking corrections
//******************************************************************************
procedure TMain_Form.ToggleTrackingCorrections_CheckBoxClick(Sender: TObject);
begin
  Run_UpdateTrackingRates;

end;

//******************************************************************************
// ATrack system timer
//******************************************************************************
procedure TMain_Form.ATrack_TimerTimer(Sender: TObject);
begin
  if NetworkCmdStart then Network_Start;
  if NetworkCmdStop then Network_Stop;

  if StopRequest then
    Run_Stop;

  if RunRequest then
    Run_Start;

  if ATrackRunning then
    RUN_Panel.color := clLime
  else
    RUN_Panel.color := clRed;

  if COMStartFlag then
  begin
    COMStartFlag := false;
    RUN_Start;
  end;
  if COMStopFlag then
  begin
    COMStopFlag := false;
    RUN_Stop;
  end;
  if COMCenterFlag then
  begin
    COMCenterFlag := false;
    Centering_ForceCentering;
  end;
end;

//******************************************************************************
// Force center
//******************************************************************************
procedure TMain_Form.ForceTargetCentering_BCButtonButtonClick(Sender: TObject);
begin
  Centering_ForceCentering;
end;

procedure TMain_Form.IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  s: string;

begin
  s := BytesToString(Adata,0,100);
  Network_RxData(s);

end;

//******************************************************************************
// RUN button click
//******************************************************************************
procedure TMain_Form.RUN_ButtonClick(Sender: TObject);
begin
  if ATrackRunning then
    StopRequest := true
  else
    RunRequest := true;

end;

procedure Main_StartDirectoryWatch;
begin
  if not Assigned(Main_Form.FEventHandler) then
  begin
    watchFolder := Main_Form.Images_DirectoryEdit.Text;
    if not DirectoryExists(watchFolder) then
      CreateDir(watchFolder);
    watchFolder := watchFolder + '\' + Main_Form.ImagesSubFolder_Edit.Text + '\';
    if not DirectoryExists(watchFolder) then
      CreateDir(watchFolder);

    Main_Form.FWaitHandle := FindFirstChangeNotification(pchar(watchFolder), True, FILE_NOTIFY_CHANGE_FILE_NAME);
    if Main_Form.FWaitHandle <> INVALID_HANDLE_VALUE then
      Main_Form.FEventHandler := WidgetSet.AddEventHandler(Main_Form.FWaitHandle, 0, @Main_Form.DirChanged, 0);
  end;
  oldFileList.clear;
  newFileList.clear;
  FindAllFIles(oldFileList,watchFolder,'*.fit;*.FIT',false);
end;

procedure TMain_Form.ClearEventHandler;
begin
  if Assigned(FEventHandler) then
  begin
    FindCloseChangeNotification(FWaitHandle);
    WidgetSet.RemoveEventHandler(FEventHandler);
  end;
end;

procedure Main_StopDirectoryWatch;
begin
  Main_Form.ClearEventHandler;
end;

procedure TMain_Form.DirChanged(AData: PtrInt; AFlags: DWORD);
var
  n: integer;
  s: string;

label waitForNextImage;

begin
// get folder list of files
  FindAllFIles(newFileList,watchFolder, '*.fit;*.FIT;*.fits',false);

// must be at least 1 new image
  if newFileList.count = 0 then goto waitForNextImage;

// assume new one is last in list
  s := newFileList[newFileList.count-1];

// first image
  n := oldFileList.count;
  if n = 0 then
  begin
    newImageFileName := s;
    DirMonitorWait_Timer.Enabled := true;
    goto waitForNextImage;
  end;

// 1 old image
  if n = 1 then
  begin
    if s = oldFileList[0] then goto waitForNextImage;
    newImageFileName := s;
    DirMonitorWait_Timer.Enabled := true;
    goto waitForNextImage
  end;

// not in last list
  n := oldFileList.IndexOf(s);
  if n = -1 then
  begin
    newImageFileName := s;
    DirMonitorWait_Timer.Enabled := true;
  end;

// update old file list
waitForNextImage:
  oldFileList.text := newFileList.text;
  newFileList.clear;
  if not FindNextChangeNotification(FWaitHandle) then
    ClearEventHandler;
end;

end.

