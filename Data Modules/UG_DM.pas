unit UG_DM;

interface

uses
  System.SysUtils, System.Classes, VBBase_DM, Data.DBXDataSnap, Data.DBXCommon,
  IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.Phys.MSSQLDef, FireDAC.DApt, FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL, FireDAC.Comp.UI, Data.DB, FireDAC.Comp.DataSet,
  Data.SqlExpr;

type
  TUGDM = class(TVBBaseDM)
    conFB: TFDConnection;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    FDStanStorageBinLink: TFDStanStorageBinLink;
    qrySQL: TFDQuery;
    cmdGeneric: TFDCommand;
    sprGeneric: TFDStoredProc;
    trnFB: TFDTransaction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UGDM: TUGDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
