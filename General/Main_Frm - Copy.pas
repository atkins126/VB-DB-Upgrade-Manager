unit Main_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, System.Actions, Vcl.ActnList, Vcl.Menus,
  Vcl.StdCtrls, Vcl.Controls, Vcl.Dialogs, Base_Frm, System.ImageList,
  Vcl.ImgList, System.IOUtils, Vcl.ComCtrls,

  CommonValues, VBCommonValues,

  cxImageList, cxGraphics, dxLayoutLookAndFeels, cxClasses, cxStyles, dxSkinsCore,
  dxSkinsDefaultPainters, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxRibbonSkins, dxRibbonCustomizationForm, dxSkinsForm, dxRibbon, dxBar, cxEdit,
  dxLayoutContainer, dxLayoutControl, dxStatusBar, dxRibbonStatusBar, cxButtons,
  cxContainer, cxTextEdit, cxMemo, cxProgressBar, cxLabel, cxMaskEdit,
  cxSpinEdit;

type
  TMainFrm = class(TBaseFrm)
    barManager: TdxBarManager;
    ribMainTab1: TdxRibbonTab;
    ribMain: TdxRibbon;
    sknController: TdxSkinController;
    actExit: TAction;
    actUpdateDB: TAction;
    barToolbar: TdxBar;
    btnExit: TdxBarLargeButton;
    btnUpdate: TdxBarLargeButton;
    layMain: TdxLayoutControl;
    layMainGroup_Root: TdxLayoutGroup;
    sbrMain: TdxRibbonStatusBar;
    memUpdateScript: TcxMemo;
    btnUpgradeDB: TcxButton;
    btnLoadScript: TcxButton;
    sbrMainContainer1: TdxStatusBarContainerControl;
    lblStatementCount: TcxLabel;
    btnTest: TcxButton;
    spnCount: TcxSpinEdit;
    prgUpgrade: TcxProgressBar;
    procedure DoExitApp(Sender: TObject);
    procedure DoUpdateDB(Sender: TObject);
    procedure btnLoadScriptClick(Sender: TObject);
    procedure btnUpgradeDBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStatementCount: Integer;
    FSQLStatement: TStringList;
    FCommand: string;

    procedure HandleProgress(var MyMsg: TMessage); message WM_DOWNLOAD_PROGRESS;
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

uses
  RUtils, UG_DM;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'VB Database Upgrade Manager';
  sbrMain.Panels[1].Text := 'Ready to upgrade database.';
  prgUpgrade.Visible := False;
  prgUpgrade.Position := 0;
  FSQLStatement := CreateStringList(PIPE, SINGLE_QUOTE);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  inherited;
  FSQLStatement.Free;
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  inherited;
    UGDM.ShellResource := VBBaseDM.GetShellResource;
    SkinResourceFileName := RESOURCE_FOLDER + SKIN_RESOURCE_FILE;
    SkinName := TSDM.ShellResource.SkinName;
end;

procedure TMainFrm.btnLoadScriptClick(Sender: TObject);
var
  FileName: string;
  I: Integer;
begin
  inherited;
  FileName := 'C:\Apps\VB\DB Update 13.ini';

  if not TFile.Exists(FileName) then
    raise EFileNotFoundException.Create('File not found');

  memUpdateScript.Lines.LoadFromFile(FileName);
  FStatementCount := 0;
  memUpdateScript.Lines.BeginUpdate;

  try
    for I := 0 to memUpdateScript.Lines.Count - 1 do
    begin
      if SameText(memUpdateScript.Lines[I], 'EO_STATEMENT') then
        Inc(FStatementCount);
    end;
    lblStatementCount.Caption := FStatementCount.ToString;
  finally
    memUpdateScript.Lines.EndUpdate;
  end;
end;

procedure TMainFrm.btnTestClick(Sender: TObject);
begin
  inherited;
  prgUpgrade.Visible := True;
  prgUpgrade.Position := spnCount.Value;
end;

procedure TMainFrm.btnUpgradeDBClick(Sender: TObject);
var
  I, Counter: Integer;
  CounterStr: string;
begin
  inherited;
  sbrMain.Panels[1].Text := 'Dabatase upgrade in progress...';
  memUpdateScript.Lines.BeginUpdate;
  Counter := 0;
  prgUpgrade.Position := 0;
  prgUpgrade.Visible := True;
  prgUpgrade.Properties.Max := FStatementCount;

  try
    for I := 1 to memUpdateScript.Lines.Count - 1 do
    begin
      if SameText(memUpdateScript.Lines[I], 'EOF') then
        Break;

      if not (SameText(memUpdateScript.Lines[I], 'EO_STATEMENT'))
        and (Length(Trim(memUpdateScript.Lines[I])) > 0) then
        FSQLStatement.Add(memUpdateScript.Lines[I]);

      if SameText(memUpdateScript.Lines[I], 'EO_STATEMENT') then
      begin
        Inc(Counter);

        prgUpgrade.Position := Counter; // Trunc(Counter / FStatementCount * 100);
        prgUpgrade.Update;

//        CounterStr := FloatToStr(Counter / FStatementCount * 100);
//        SendMessage(MainFrm.Handle, WM_DOWNLOAD_PROGRESS, DWORD(PChar(CounterStr)), 0);
        sbrMain.Panels[2].Text := 'Running script ' + Counter.ToString + ' of ' + FStatementCount.ToString;
        sbrMain.Update;
//        Sleep(50);
        FCommand := FSQLStatement.DelimitedText;
      // Execute SQL script here
        FCommand := '';
        FSQLStatement.Clear;
      end;
    end;
  finally
//    prgUpgrade.Visible := False;
    memUpdateScript.Lines.EndUpdate;
  end;
  ShowMessage('Done');
end;

procedure TMainFrm.DoExitApp(Sender: TObject);
begin
//  inherited;
  MainFrm.Close;
end;

procedure TMainFrm.DoUpdateDB(Sender: TObject);
begin
  inherited;
//
end;

procedure TMainFrm.HandleProgress(var MyMsg: TMessage);
var
  Progress: Extended;
begin
  inherited;
  Progress := StrToFloat(PChar(MyMsg.WParam));
  try
    prgUpgrade.Position := Trunc(Progress);
    prgUpgrade.Update;
//    Update;
  finally
    MyMsg.Result := 1;
  end;
end;

end.

