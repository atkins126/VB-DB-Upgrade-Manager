unit UpgraeHistory_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.ImgList, cxImageList, Vcl.Controls, Vcl.Dialogs,
  System.Actions, Vcl.ActnList, System.IOUtils,

  BaseLayout_Frm, CommonValues, VBCommonValues,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinsDefaultPainters, System.ImageList, dxLayoutLookAndFeels, cxClasses,
  cxStyles, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxBar,
  cxTextEdit, cxMemo, dxLayoutcxEditAdapters, cxButtonEdit, dxScreenTip,
  dxCustomHint, cxHint, cxBarEditItem, cxDropDownEdit, Vcl.Menus, Vcl.StdCtrls,
  cxButtons, dxBarExtItems, cxMaskEdit;

type
  TUpgraeHistoryFrm = class(TBaseLayoutFrm)
    memUpgradeScript: TcxMemo;
    styReadOnly: TcxEditStyleController;
    barManager: TdxBarManager;
    barToolbar: TdxBar;
    litToolbar: TdxLayoutItem;
    litHistory: TdxLayoutItem;
    docToolbar: TdxBarDockControl;
    btnExit: TdxBarLargeButton;
    actExit: TAction;
    repScreenTip: TdxScreenTipRepository;
    tipExit: TdxScreenTip;
    styHintController: TcxHintStyleController;
    dlgFileOpen: TOpenDialog;
    cntScriptLocation: TdxBarControlContainerItem;
    cntUpgradeVersion: TdxBarControlContainerItem;
    edtUpgradeScriptLocation: TcxTextEdit;
    lucScriptFile: TcxComboBox;
    lblUpgradeLocation: TdxBarStatic;
    lblScript: TdxBarStatic;
    procedure DoExitForm(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lucScriptFilePropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure LoadScriptFile(DBVersion: Integer);
  public
    { Public declarations }
  end;

var
  UpgraeHistoryFrm: TUpgraeHistoryFrm;

implementation

{$R *.dfm}

uses
  RUtils;

procedure TUpgraeHistoryFrm.FormCreate(Sender: TObject);
var
  FileList: TStringList;
  AComboBox: TcxComboBox;
begin
  Caption := 'DB Upgrade History';
  layMain.Align := alClient;
  layMain.LayoutLookAndFeel := lafCustomSkin;
  FileList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);
  edtUpgradeScriptLocation.EditValue := DB_UPGRADE_SCRIPT_FOLDER;

  try
    GetFileList(DB_UPGRADE_SCRIPT_FOLDER, '*.in*', FileList);
//    GetFileListEx(DB_UPGRADE_SCRIPT_FOLDER, '*.ini', FileList);
    TcxComboBoxProperties(lucScriptFile.Properties).Items := FileList;
    lucScriptFile.ItemIndex := 0;
//    AComboBox := TcxBarEditItemControl(lucScriptFile.Links[0].Control).Edit as TcxComboBox;
//    AComboBox.ItemIndex := 0;
  finally
    FileList.Free;
  end;
end;

procedure TUpgraeHistoryFrm.FormShow(Sender: TObject);
begin
    lucScriptFile.SetFocus;
  memUpgradeScript.SetFocus;
end;

procedure TUpgraeHistoryFrm.LoadScriptFile(DBVersion: Integer);
var
  FileName: string;
begin
  FileName := DB_UPGRADE_SCRIPT_FOLDER + DBVersion.ToString + '.ini';

  if not TFile.Exists(FileName) then
    raise EFileNotFoundException.Create('File not found');

  memUpgradeScript.Lines.LoadFromFile(FileName);
end;

procedure TUpgraeHistoryFrm.lucScriptFilePropertiesChange(Sender: TObject);
var
  ScriptFileName: string;
begin
  if lucScriptFile.ItemIndex < 0 then
    Exit;

  ScriptFileName := TPath.GetFileNameWithoutExtension(lucScriptFile.Text);
  LoadScriptFile(StrToint(ScriptFileName));
  litHistory.CaptionOptions.Text := 'Script processed for DB version: ' + ScriptFileName;
end;

procedure TUpgraeHistoryFrm.DoExitForm(Sender: TObject);
begin
  Self.ModalResult := mrOK;
end;

end.

