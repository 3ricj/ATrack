unit Profiles;

interface

uses Dialogs, sysUtils;

procedure Profiles_Init;
procedure Profiles_Save;
procedure Profiles_Load;


implementation

uses Main, Config, Global;


procedure Profiles_Init;
begin
//   currentProfileDec := -200;
end;

procedure Profiles_Load;
begin
// init
  Main_Form.OpenDialog.DefaultExt := '.ini';
  Main_Form.OpenDialog.Filter := 'ATrack Profiles|*.prf|All Files|*.*';
  Main_Form.OpenDialog.InitialDir := ExtractFileDir(Main_Form.Profile_Edit.text);
  Main_Form.OpenDialog.FileName := ExtractFileName(Main_Form.Profile_Edit.text);

// load
  if not Main_Form.OpenDialog.execute then exit;

// update
  Main_Form.Profile_Edit.text := Main_Form.OpenDialog.FileName;
  Config_Read(Main_Form.OpenDialog.FileName);

end;

procedure Profiles_Save;
begin
//init
  Main_Form.SaveDialog.DefaultExt := '.ini';
  Main_Form.SaveDialog.Filter := 'ATrack Profiles|*.prf|All Files|*.*';
  Main_Form.SaveDialog.InitialDir := ExtractFileDir(Main_Form.Profile_Edit.text);
  Main_Form.SaveDialog.FileName := ExtractFileName(Main_Form.Profile_Edit.text);

// save
  if not Main_Form.SaveDialog.execute then exit;
  Main_Form.Profile_Edit.text := Main_Form.SaveDialog.FileName;
  Config_Write(Main_Form.SaveDialog.FileName);
end;

end.
