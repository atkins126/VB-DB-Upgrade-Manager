unit Main_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Vcl.Dialogs, System.Actions, Vcl.ActnList, System.IOUtils, System.Win.Registry,

  BaseLayout_Frm, CommonValues, VBCommonValues, CommonFunction,

  FireDAC.Phys.IBWrapper,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinsDefaultPainters, cxImageList, dxLayoutLookAndFeels, cxClasses, cxStyles,
  dxLayoutContainer, dxLayoutControl, dxBar, dxScreenTip, dxSkinsForm, cxMemo,
  dxCustomHint, cxHint, cxContainer, cxEdit, cxProgressBar, dxStatusBar, cxTextEdit,
  dxLayoutcxEditAdapters, cxButtonEdit, cxBarEditItem, dxRibbonSkins,
  dxRibbonCustomizationForm, dxRibbon, cxCurrencyEdit, cxDropDownEdit,
  cxCheckBox;

type
  TMainFrm = class(TBaseLayoutFrm)
    barManager: TdxBarManager;
    batToolbar: TdxBar;
    btnExit: TdxBarLargeButton;
    btnUpgrade: TdxBarLargeButton;
    actExit: TAction;
    actUpgrade: TAction;
    repScreenTip: TdxScreenTipRepository;
    tipExit: TdxScreenTip;
    tipUpgrade: TdxScreenTip;
    styHintController: TcxHintStyleController;
    sknController: TdxSkinController;
    styReadOnly: TcxEditStyleController;
    memUpgradeScript: TcxMemo;
    litScript: TdxLayoutItem;
    edtUpgradeFileName: TcxBarEditItem;
    dlgFileOpen: TOpenDialog;
    edtCurrentDBVersion: TcxBarEditItem;
    ribMainTab1: TdxRibbonTab;
    ribMain: TdxRibbon;
    sbrMain: TdxStatusBar;
    edtUpgradeVersion: TcxBarEditItem;
    edtDBFileName: TcxBarEditItem;
    prgUpgrade: TcxProgressBar;
    litProgress: TdxLayoutItem;
    btnHistory: TdxBarLargeButton;
    tipUpgradeHistory: TdxScreenTip;
    actHistgory: TAction;
    cbxBackupDB: TcxBarEditItem;
    procedure actExitExecute(Sender: TObject);
    procedure DoUpgradeDB(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtUpgradeFileNamePropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DoUpgradehistory(Sender: TObject);
    procedure cbxBackupDBPropertiesEditValueChanged(Sender: TObject);
  private
    { Private declarations }
    FStatementCount: Integer;
    FSQLStatement: TStringList;
    FCommand: string;
    FDBversion: Integer;
    FUpgradeVersion: Integer;
    FUpgrading: Boolean;
    FBackupFolder: String;

    procedure LoadScriptFile(DBVersion: Integer);
    procedure UpdateApplicationSkin(SkinResourceFileName, SkinName: string);
    function GetDBVersion: Integer;
    function GetUpgradeVersion(CurrentDBVersion: Integer): Integer;
    procedure UpdateDBVersion(NewVersionNo: Integer);
    procedure BackupDB(DBName, BackupName: string);
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

const
  DB_VERSION = 'SELECT DB_VERSION FROM DB_INFO';
  NEW_DB_VERSION = 'UPDATE DB_INFO SET DB_VERSION = %d;';
  UPDATE_UPGRADE_HISTORY = 'INSERT INTO DB_UPGRADE_HISTORY(DB_VERSION) VALUES(%d)';

implementation

{$R *.dfm}

uses
  RUtils,
  Backup_DM,
  UpgraeHistory_Frm,
  MsgDialog_Frm;

procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  inherited;
  CanClose := not FUpgrading;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
var
  RegKey: TRegistry;
begin
  inherited;
  Caption := 'VB Database Upgrade Manager';
  layMain.Align := alClient;
  layMain.LayoutLookAndFeel := lafCustomSkin;
  sbrMain.Panels[1].Text := 'Ready to upgrade database.';
  FUpgrading := False;
  prgUpgrade.Position := 0;
  FSQLStatement := TStringList.Create;
//  FSQLStatement.Delimiter :=  PIPE;
  FSQLStatement := CreateStringList(PIPE, DOUBLE_QUOTE);
  Application.HintPause := 0;
  Application.HintShortPause := 0;
  Application.HintHidePause := 150000;
  styHintController.HintHidePause := 15000;

  if BackupDM = nil then
    BackupDM := TBackupDM.Create(nil);

  if MsgDialogFrm = nil then
    MsgDialogFrm := TMsgDialogFrm.Create(nil);

  RegKey := TRegistry.Create(KEY_ALL_ACCESS or KEY_WRITE or KEY_WOW64_64KEY);
  RegKey.RootKey := HKEY_CURRENT_USER;

  try
    RegKey.OpenKey(KEY_DATABASE, True);

    if not RegKey.ValueExists('Backup DB Before Running Upgrade Process') then
      RegKey.WriteBool('Backup DB Before Running Upgrade Process', True);

    cbxBackupDB.EditValue := RegKey.ReadBool('Backup DB Before Running Upgrade Process');
    FBackupFolder :=  RegKey.ReadBool('Backup Folder');
    RegKey.CloseKey;
  finally
    RegKey.Free;
  end;

end;

procedure TMainFrm.FormShow(Sender: TObject);
var
  SkinResourceFileName, SkinName: string;
begin
  inherited;
  BackupDM.ShellResource := BackupDM.GetShellResource;
  SkinResourceFileName := RESOURCE_FOLDER + SKIN_RESOURCE_FILE;
  SkinName := BackupDM.ShellResource.SkinName;

  if Length(Trim(SkinName)) = 0 then
    SkinName := DEFAULT_SKIN_NAME;

  UpdateApplicationSkin(SkinResourceFileName, SkinName);
//  styReadOnly.Style.Color := RootLookAndFeel.SkinPainter.DefaultContentColor;
  FDBversion := GetDBVersion;
  FUpgradeVersion := GetUpgradeVersion(GetDBVersion);
//  FUpgradeVersion := FDBversion + 1;
  edtCurrentDBVersion.EditValue := FDBVersion {.ToString};
  edtUpgradeVersion.EditValue := FUpgradeVersion {.ToString};
  LoadScriptFile(FUpgradeVersion);
  edtDBFileName.EditValue := BackupDM.DBFileName;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  inherited;
  FSQLStatement.Free;

//  if MsgDialogFrm <> nil then
//    FreeAndNil(MsgDialogFrm);

  if Assigned(BackupDM) then
    FreeAndNil(BackupDM);
end;

function TMainFrm.GetDBVersion: Integer;
begin
  BackupDM.qrySQL.Open(DB_VERSION);
  Result := BackupDM.qrySQL.FieldByName('DB_VERSION').Asinteger;
end;

function TMainFrm.GetUpgradeVersion(CurrentDBVersion: Integer): Integer;
var
  FileName: string;
  FileFound: Boolean;
begin
  Result := CurrentDBVersion + 1;

  repeat
    FileName := DB_UPGRADE_SCRIPT_FOLDER + Result.ToString + '.ini';
    FileFound := TFile.Exists(Filename);

    if FileFound then
      Inc(Result);
  until
    not FileFound;

  Dec(Result);
end;

procedure TMainFrm.actExitExecute(Sender: TObject);
begin
  inherited;
  MainFrm.Close;
end;

procedure TMainFrm.LoadScriptFile(DBVersion: Integer);
var
  I: Integer;
  FileName: string;
begin
  inherited;
  FileName := DB_UPGRADE_SCRIPT_FOLDER + DBVersion.ToString + '.ini';

  if not TFile.Exists(FileName) then
    raise EFileNotFoundException.Create('File not found');

  memUpgradeScript.Lines.LoadFromFile(FileName);
  FStatementCount := 0;
  memUpgradeScript.Lines.BeginUpdate;

  try
    for I := 0 to memUpgradeScript.Lines.Count - 1 do
    begin
      if SameText(memUpgradeScript.Lines[I], 'EO_STATEMENT') then
        Inc(FStatementCount);
    end;

    litScript.CaptionOptions.Text := FStatementCount.ToString + ' Upgrade scripts to process';
    edtUpgradeFileName.EditValue := FileName;
  finally
    memUpgradeScript.Lines.EndUpdate;

    if BackupDM.conFB.Connected then
      BackupDM.conFB.Close;
  end;
end;

procedure TMainFrm.UpdateApplicationSkin(SkinResourceFileName, SkinName: string);
begin
  sknController.BeginUpdate;
  try
    sknController.NativeStyle := False;
    sknController.UseSkins := True;
    if dxSkinsUserSkinLoadFromFile(SkinResourceFileName, SkinName) then
    begin
      sknController.SkinName := 'UserSkin';
      RootLookAndFeel.SkinName := 'UserSkin';
      barManager.LookAndFeel.SkinName := 'UserSkin';
      lafCustomSkin.LookAndFeel.SkinName := 'UserSkin';
    end
    else
    begin
      sknController.SkinName := DEFAULT_SKIN_NAME;
      RootLookAndFeel.SkinName := DEFAULT_SKIN_NAME;
      barManager.LookAndFeel.SkinName := DEFAULT_SKIN_NAME;
      lafCustomSkin.LookAndFeel.SkinName := DEFAULT_SKIN_NAME;
    end;
  finally
    sknController.Refresh;
    sknController.EndUpdate;
  end;
end;

procedure TMainFrm.BackupDB(DBName, BackupName: string);
begin
  Screen.Cursor := crHourglass;
  BackupDM.dbBackup.DriverLink := BackupDM.FDPhysIBDriverLink;
  BackupDM.dbBackup.UserName := 'sysdba';
  BackupDM.dbBackup.Password := 'masterkey';
  BackupDM.dbBackup.Host := 'localhost';
  BackupDM.dbBackup.protocol := iplocal;

  {TODO: Implement ability to backup/Restore a remote DB}
//  BackupDM.dbBackup.Host := edthostname.text;
//  case lucprotocol.itemindex of
//    0: BackupDM.dbBackup.protocol := iplocal;
//    1: BackupDM.dbBackup.protocol := iptcpip;
//  end;

  BackupDM.dbBackup.database := DBName;
  BackupDM.dbBackup.BackupFiles.text := BackupName;

  try
  if TFile.Exists(BackupName) then
  TFile.Delete(BackupName);
    BackupDM.dbBackup.Backup;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.cbxBackupDBPropertiesEditValueChanged(Sender: TObject);
var
  RegKey: TRegistry;
begin
  inherited;
  RegKey := TRegistry.Create(KEY_ALL_ACCESS or KEY_WRITE or KEY_WOW64_64KEY);
  RegKey.RootKey := HKEY_CURRENT_USER;

  try
    RegKey.OpenKey(KEY_DATABASE, True);
    cbxBackupDB.EditValue := RegKey.ReadBool('Backup DB Before Running Upgrade Process');
    RegKey.CloseKey;
  finally
    RegKey.Free;
  end;
end;

procedure TMainFrm.DoUpgradeDB(Sender: TObject);
var
  I, J, Counter, NextVersion: Integer;
begin
  inherited;
  if FDBversion = FUpgradeVersion then
    raise EValidateException.Create('The latest database upgrade (Ver: ' +
      FDBversion.ToString + ') has already been applied.');

  Beep;
  if DisplayMsg(
    Application.Title,
    'DB Upgrade Confirmation',
    'You are about to upgrade the Shell database to version: ' + FUpgradeVersion.ToString +
    '. This process cannot be stopped once it has started!' + CRLF + CRLF +
    'Are you sure you want to proceed with the database upgrade process?.',
    mtConfirmation,
    [mbYes, mbNo]
    ) = mrNo then
    Exit;


Screen.Cursor := crHourglass;

if cbxBackupDB.EditValue then
BackupDB(edtDBFileName.EditValue, FBackupFolder+ 'VBTemp.fbk');

  FUpgrading := True;
  actExit.Enabled := not FUpgrading;
  actUpgrade.Enabled := not FUpgrading;
  actHistgory.Enabled := not FUpgrading;
  Self.Update;

  if not BackupDM.conFB.Connected then
    BackupDM.conFB.Open;

  sbrMain.Panels[1].Text := 'Dabatase upgrade in progress...';
  NextVersion := FDBversion + 1;

  try
    for I := FDBversion + 1 to FUpgradeVersion do
    begin
      Counter := 0;
      prgUpgrade.Position := 0;
      prgUpgrade.Properties.Max := FStatementCount;
      LoadScriptFile(NextVersion);

      BackupDM.scrGeneric.SQLScripts.Clear;

      for J := 0 to memUpgradeScript.Lines.Count - 1 do
      begin
        if SameText(memUpgradeScript.Lines[J], 'EOF') then
          Break;

        if not (SameText(Trim(memUpgradeScript.Lines[J]), 'EO_STATEMENT'))
          and (Length(Trim(memUpgradeScript.Lines[J])) > 0) then
          FSQLStatement.Add(memUpgradeScript.Lines[J]);

        if SameText(memUpgradeScript.Lines[J], 'EO_STATEMENT') then
        begin
          Inc(Counter);
          prgUpgrade.Position := Counter;
          prgUpgrade.Update;
          sbrMain.Panels[1].Text := 'Running script ' + Counter.ToString + ' of ' + FStatementCount.ToString;
          sbrMain.Update;
          FCommand := FSQLStatement.Text;
          BackupDM.scrGeneric.SQLScripts.Add;
          BackupDM.scrGeneric.SQLScripts[0].SQL.Add(FCommand);
          BackupDM.scrGeneric.ValidateAll;
          BackupDM.scrGeneric.ExecuteAll;
          BackupDM.scrGeneric.SQLScripts.Clear;
          FCommand := '';
          FSQLStatement.Clear;
        end;
      end;
      Inc(NextVersion);
    end;

    FDBversion := FUpgradeVersion;
    UpdateDBVersion(FUpgradeVersion);
    BackupDM.conFB.Close;
    ShowMessage('Database successfully upgraded to version: ' + FUpgradeversion.ToString);
    prgUpgrade.Position := 0;
    sbrMain.Panels[1].Text := 'Ready to upgrade database.';
    memUpgradeScript.Lines.Clear;
  finally
    FUpgrading := False;
    actExit.Enabled := not FUpgrading;
    actUpgrade.Enabled := not FUpgrading;
    actHistgory.Enabled := not FUpgrading;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.DoUpgradehistory(Sender: TObject);
begin
  inherited;
  if UpgraeHistoryFrm = nil then
    UpgraeHistoryFrm := TUpgraeHistoryFrm.Create(nil);

  UpgraeHistoryFrm.ShowModal;
  UpgraeHistoryFrm.Close;
  FreeAndNil(UpgraeHistoryFrm);
end;

procedure TMainFrm.UpdateDBVersion(NewVersionNo: Integer);
begin
  BackupDM.cmdGeneric.CommandText.Clear;
  BackupDM.cmdGeneric.CommandText.Add(Format(NEW_DB_VERSION, [NewVersionNo]));
  BackupDM.cmdGeneric.Execute;

  BackupDM.cmdGeneric.CommandText.Clear;
  BackupDM.cmdGeneric.CommandText.Add(Format(UPDATE_UPGRADE_HISTORY, [NewVersionNo]));
  BackupDM.cmdGeneric.Execute;
end;

procedure TMainFrm.edtUpgradeFileNamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  inherited;
//  if dlgFileOpen.Execute then
//  begin
//    edtUpgradeFileName.EditValue := dlgFileOpen.FileName;
//  end;
end;

end.

