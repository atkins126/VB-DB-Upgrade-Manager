program DBUpgradeManager;

uses
  Vcl.Forms,
  CommonValues in '..\..\..\..\Lib\CommonValues.pas',
  VBCommonValues in '..\..\Lib\VBCommonValues.pas',
  RUtils in '..\..\..\..\Lib\RUtils.pas',
  Base_Frm in '..\..\..\..\Lib\Base_Frm.pas' {BaseFrm},
  BaseLayout_Frm in '..\..\..\..\Lib\BaseLayout_Frm.pas' {BaseLayoutFrm},
  Backup_DM in '..\Backup Manager\Data Modules\Backup_DM.pas' {BackupDM: TDataModule},
  Main_Frm in 'General\Main_Frm.pas' {MainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DB Upgrade Manager';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
