unit FmConsulta;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DB, Grids, DBGrids, ActiveX, ComObj,
  ExtCtrls, Buttons, VariablesGlobales;

type
  TFormConsulta = class(TForm)
    TxtFechaInicio: TDateTimePicker;
    TxtFechaFin: TDateTimePicker;
    BtnBuscar: TButton;
    DBGrid1: TDBGrid;
    DataSourceDOF: TDataSource;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    BtnExportXls: TSpeedButton;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    procedure BtnBuscarClick(Sender: TObject);
    procedure BtnExportXlsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConsulta: TFormConsulta;

implementation

uses FmConexion;

{$R *.dfm}

procedure TFormConsulta.BtnBuscarClick(Sender: TObject);
var
  DateValueInicio, DateValueFin: TDateTime;
begin
  DateValueInicio := TxtFechaInicio.Date;
  DateValueFin := TxtFechaFin.Date;

  try
    // Verifica que la fecha final no sea menor que la inicial
    if DateValueFin < DateValueInicio then
    begin
      ShowMessage('La fecha final no puede ser menor que la fecha inicial.');
      Exit; // Salir si las fechas no son válidas
    end;

    // Verifica que la conexión esté abierta
    if not DataModule1.ADOConnection1.Connected then
      DataModule1.ADOConnection1.Open;

    // Prepara la consulta
    DataModule1.ADOQuery1.Close;
    DataModule1.ADOQuery1.SQL.Clear;

    // Modifica la consulta para que considere solo la fecha
    DataModule1.ADOQuery1.SQL.Add('SELECT * FROM BatchTable ');
    DataModule1.ADOQuery1.SQL.Add('WHERE CAST(Fecha AS DATE) between '+QuotedStr(formatdatetime('yyyy/mm/dd', DateValueInicio))+' AND '+QuotedStr(formatdatetime('yyyy/mm/dd', DateValueFin)));

    //DataModule1.ADOQuery1.SQL.Add('SELECT * FROM BatchTable WHERE CAST(Fecha AS DATE) >= :FechaInicio AND CAST(Fecha AS DATE) <= :FechaFin');

    // Asigna parámetros
    //DataModule1.ADOQuery1.Parameters.ParamByName('FechaInicio').Value := DateValueInicio;
    //DataModule1.ADOQuery1.Parameters.ParamByName('FechaFin').Value := DateValueFin;

    // Ejecuta la consulta
    DataModule1.ADOQuery1.Open;

    // Actualiza el DBGrid
    DataSourceDOF.DataSet := DataModule1.ADOQuery1;
    DBGrid1.DataSource := DataSourceDOF;

    if DataModule1.ADOQuery1.RecordCount = 0 then
      ShowMessage('No hay registros disponibles.');

  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TFormConsulta.BtnExportXlsClick(Sender: TObject);
var
  ExcelApp: OLEVariant;
  HeaderRange: OLEVariant;
  i, j: Integer;
begin
  try
    // Inicializa OLE
    CoInitialize(nil);
    try
      // Crea una nueva instancia de Excel
      ExcelApp := CreateOleObject('Excel.Application');
      try
        ExcelApp.Visible := True; // Muestra Excel

        // Agrega un nuevo libro
        ExcelApp.Workbooks.Add;

        // Escribir encabezados
        for i := 0 to DataModule1.ADOQuery1.FieldCount - 1 do
          ExcelApp.Cells[1, i + 1] := DataModule1.ADOQuery1.Fields[i].FieldName;

        // Escribir datos
        DataModule1.ADOQuery1.First; // Comienza desde el primer registro
        for i := 0 to DataModule1.ADOQuery1.RecordCount - 1 do
        begin
          for j := 0 to DataModule1.ADOQuery1.FieldCount - 1 do
            ExcelApp.Cells[i + 2, j + 1] := DataModule1.ADOQuery1.Fields[j].AsString;

          DataModule1.ADOQuery1.Next; // Mueve al siguiente registro
        end;

        // Formatear encabezados
        HeaderRange := ExcelApp.Range[ExcelApp.Cells[1, 1], ExcelApp.Cells[1, DataModule1.ADOQuery1.FieldCount]];
        HeaderRange.Font.Bold := True;
        HeaderRange.Interior.Color := $CCCCCC; // Color de fondo gris claro

      finally
        // Libera el objeto Excel
        ExcelApp := Unassigned;
      end;

    finally
      // Finaliza OLE
      CoUninitialize;
    end;

  except
    on E: Exception do
      ShowMessage('Error al exportar a Excel: ' + E.Message);
  end;
end;

end.
