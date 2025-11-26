unit FmConexion;

interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TDataModule1 = class(TDataModule)
    ADOConnection1: TADOConnection;
    ADOQuery1: TADOQuery;
    ADOTableEmpleados: TADOTable;
    ADOQueryLogin: TADOQuery;
    ADOQueryBuscar: TADOQuery;
    ADOQueryActualizar: TADOQuery;
    ADOQueryTotalLab: TADOQuery;
    ADOQueryBatchTable: TADOQuery;
    ADOQueryEmpleados: TADOQuery;
    ADOQueryEmpleadoUpdate: TADOQuery;
    ADOQueryOrdenFab: TADOQuery;
    ADOQueryEmpleadosDataUser: TStringField;
    ADOQueryEmpleadosDataPassword: TBlobField;
    ADOQueryEmpleadosNombre: TStringField;
    ADOQueryOrdenFabLote: TWideStringField;
    ADOQueryOrdenFabSumCantTeorica: TBCDField;
    ADOQueryOrdenFabSumCantReal: TBCDField;
    ADOQueryOrdenFabDensidad: TBCDField;
    ADOQueryOrdenFabVolTeorico: TBCDField;
    ADOQueryOrdenFabVolReal: TBCDField;
    ADOQueryOrdenFabFecha: TDateField;
    ADOQueryOrdenFabHora: TWideStringField;
    ADOQueryOrdenFabOrdenFab: TIntegerField;
    ADOQueryOrdenFabCodigoSAP: TIntegerField;
    ADOQueryOrdenFabSPare: TWideStringField;
    ADOQueryTotalLabLote: TWideStringField;
    ADOQueryTotalLabSumCantTeorica: TBCDField;
    ADOQueryTotalLabSumCantReal: TBCDField;
    ADOQueryTotalLabDensidad: TBCDField;
    ADOQueryTotalLabVolTeorico: TBCDField;
    ADOQueryTotalLabVolReal: TBCDField;
    ADOQueryTotalLabFecha: TDateField;
    ADOQueryTotalLabHora: TWideStringField;
    ADOQueryTotalLabOrdenFab: TIntegerField;
    ADOQueryTotalLabCodigoSAP: TIntegerField;
    ADOQueryTotalLabSPare: TWideStringField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
DataModule1 : TDataModule1;

implementation

{$R *.dfm}

end.
