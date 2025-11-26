unit FmPruebas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, ADODB, StdCtrls;

type
  TFormPruebas = class(TForm)
    ADOQuery1: TADOQuery;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ADOQuery1OrdenFab: TStringField;
    ADOQuery1Fecha: TDateField;
    ADOQuery1Hora: TStringField;
    ADOQuery1Producto: TStringField;
    ADOQuery1Cantidad: TStringField;
    ADOQuery1DescMateriaPrima: TStringField;
    ADOQuery1CantTeorica: TBCDField;
    ADOQuery1CantReal: TBCDField;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPruebas: TFormPruebas;

implementation

{$R *.dfm}


procedure TFormPruebas.Button1Click(Sender: TObject);
var
OrdenFabq: string;
begin
  OrdenFabq := '20241022001';
    // Cerrar la consulta y limpiar SQL
    adoquery1.Close;
    adoquery1.SQL.Clear;

    // Define la consulta con un parámetro nombrado
    //adoquery1.SQL.Add('SELECT * FROM BatchTable WHERE OrdenFab = :OrdenFab');
    adoquery1.SQL.add('SELECT * FROM aditivos.batchtable where OrdenFab like '+QuotedStr(OrdenFabq+'%'));

    // Asignar el valor al parámetro
    //adoquery1.Parameters.ParamByName('OrdenFab').Value := OrdenFab;

    // Abrir la consulta
    adoquery1.Open;
end;

end.
