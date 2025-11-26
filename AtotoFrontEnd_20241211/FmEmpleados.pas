unit FmEmpleados;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, DB, Grids, DBGrids, ExtCtrls, StdCtrls, Mask, DBCtrls, ADODB;

type
  TModoEdicion = (meNavegacion, meAgregar, meModificar); // Tipo para identificar el modo de edición
  TByteArray = array of Byte; // Definir un tipo para el arreglo de bytes

type
  TFormEmpleados = class(TForm)
    DBGridUsuarios: TDBGrid;
    DataSourceUsuarios: TDataSource;
    BtnAgregar: TSpeedButton;
    BtnEliminar: TSpeedButton;
    BtnGuardar: TSpeedButton;
    BtnModificar: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Label4: TLabel;
    Image2: TImage;
    BtnPrimerReg: TSpeedButton;
    BtnRegAnterior: TSpeedButton;
    BtnRegSiguiente: TSpeedButton;
    BtnUltimoReg: TSpeedButton;
    EditDataPassword: TEdit;
    EditDataUser: TEdit;
    EditNombre: TEdit;
    procedure FormShow(Sender: TObject);
    procedure CargarPrimerRegistro;
    procedure CargarRegistroAnterior;
    procedure CargarRegistroSiguiente;
    procedure CargarUltimoRegistro;
    procedure BtnAgregarClick(Sender: TObject);
    procedure BtnModificarClick(Sender: TObject);
    procedure BtnGuardarClick(Sender: TObject);
    procedure BtnEliminarClick(Sender: TObject);
    procedure BtnPrimerRegClick(Sender: TObject);
    procedure BtnRegAnteriorClick(Sender: TObject);
    procedure BtnRegSiguienteClick(Sender: TObject);
    procedure BtnUltimoRegClick(Sender: TObject);
    procedure DataSourceUsuariosDataChange(Sender: TObject; Field: TField);

  private
    procedure ActualizarCamposDesdeEmpleados;
    procedure ActualizarEstadoCampos;
    function StringToVarBinary(const S: string): TByteArray;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEmpleados: TFormEmpleados;
  ModoEdicion: TModoEdicion;

implementation

uses FmConexion;

{$R *.dfm}

procedure TFormEmpleados.FormShow(Sender: TObject);
begin
  // Abrir la consulta para obtener los registros
  DataModule1.ADOQueryEmpleados.Open;

  // Verificar si hay datos
  if not DataModule1.ADOQueryEmpleados.IsEmpty then
  begin
    // Cargar el primer registro en los campos Edit
    CargarPrimerRegistro;
    ModoEdicion := meNavegacion;
    ActualizarEstadoCampos; // Cambiar el estado de los campos y botones según el modo
  end
  else
  begin
    ShowMessage('No se encontraron datos en la tabla de empleados.');
  end;
end;

procedure TFormEmpleados.CargarPrimerRegistro;
begin
  if not DataModule1.ADOQueryEmpleados.IsEmpty then
  begin
    DataModule1.ADOQueryEmpleados.First;
    ActualizarCamposDesdeEmpleados;
  end
  else
    ShowMessage('No hay registros disponibles');
end;

procedure TFormEmpleados.CargarRegistroAnterior;
begin
  if not DataModule1.ADOQueryEmpleados.Bof then
  begin
    DataModule1.ADOQueryEmpleados.Prior;
    ActualizarCamposDesdeEmpleados;
  end
  else
    ShowMessage('No hay registros disponibles');
end;

procedure TFormEmpleados.CargarRegistroSiguiente;
begin
  if not DataModule1.ADOQueryEmpleados.Eof then
  begin
    DataModule1.ADOQueryEmpleados.Next;
    ActualizarCamposDesdeEmpleados;
  end
  else
    ShowMessage('No hay registros disponibles');
end;

procedure TFormEmpleados.CargarUltimoRegistro;
begin
  if not DataModule1.ADOQueryEmpleados.IsEmpty then
  begin
    DataModule1.ADOQueryEmpleados.Last;
    ActualizarCamposDesdeEmpleados;
  end
  else
    ShowMessage('No hay registros disponibles');
end;

procedure TFormEmpleados.ActualizarCamposDesdeEmpleados;
var
  PasswordStream: TMemoryStream;
  PasswordString: AnsiString;
begin
  EditDataUser.Text := DataModule1.ADOQueryEmpleados.FieldByName('DataUser').AsString;
  EditNombre.Text := DataModule1.ADOQueryEmpleados.FieldByName('Nombre').AsString;

  // Leer la contraseña en binario y convertirla a texto
  PasswordStream := TMemoryStream.Create;
  try
    (DataModule1.ADOQueryEmpleados.FieldByName('DataPassword') as TBlobField).SaveToStream(PasswordStream);
    SetLength(PasswordString, PasswordStream.Size);
    PasswordStream.Position := 0;
    PasswordStream.Read(PAnsiChar(PasswordString)^, PasswordStream.Size);
    EditDataPassword.Text := string(PasswordString);
  finally
    PasswordStream.Free;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// P R O C E D I M I E N T O S   P A R A   B O T O N E S

procedure TFormEmpleados.ActualizarEstadoCampos;
begin
  // Habilitar campos solo en modos de agregar o modificar
  EditDataUser.Enabled := (ModoEdicion <> meNavegacion);
  EditDataPassword.Enabled := (ModoEdicion <> meNavegacion);
  EditNombre.Enabled := (ModoEdicion <> meNavegacion);
  BtnGuardar.Enabled := (ModoEdicion <> meNavegacion);
end;

// Botón Agregar: prepara el formulario para un nuevo registro
procedure TFormEmpleados.BtnAgregarClick(Sender: TObject);
begin
  // Establecer modo de edición en agregar y limpiar campos
EditDataUser.Enabled := True;
    EditDataPassword.Enabled := True;
    EditNombre.Enabled := True;
    ModoEdicion := meAgregar;; // Cambiar el modo a "Agregar"
    ActualizarEstadoCampos;
    ModoEdicion := meAgregar;
    EditDataUser.Clear;
    EditDataPassword.Clear;
    EditNombre.Clear;

    // Establecer foco en el primer campo de entrada
    EditDataUser.SetFocus;
end;

// Botón Modificar: habilita la edición del registro actual
procedure TFormEmpleados.BtnModificarClick(Sender: TObject);
begin
  if not DataModule1.ADOQueryEmpleados.IsEmpty then
  begin
    EditDataUser.Enabled := True;
    EditDataPassword.Enabled := True;
    EditNombre.Enabled := True;
    EditDataUser.SetFocus;
    ModoEdicion := meModificar; // Cambiar el modo a "Modificar"
    ActualizarEstadoCampos;
  end
  else
    ShowMessage('No hay registros para modificar.');
end;

// Botón Guardar: guarda los cambios en la base de datos según el modo
procedure TFormEmpleados.BtnGuardarClick(Sender: TObject);
var
  PasswordBytes: TByteArray;
  SQLQuery: string;
  DTPassword: TMemoryStream;
  BlobAsHex: string;
  Buffer: PByte;
  i: Integer;
begin
  try
    // Convertir la contraseña de texto a un arreglo de bytes
    PasswordBytes := StringToVarBinary(EditDataPassword.Text);

    // Verificamos si estamos en modo agregar o modificar
    case ModoEdicion of
      meAgregar:
      begin
          // Inicializar el stream para almacenar el array de bytes
        DTPassword := TMemoryStream.Create;
        try
          // Escribir los datos al stream
          DTPassword.WriteBuffer(PasswordBytes[0], Length(PasswordBytes));

          // Convertir el contenido del Stream a una representación hexadecimal
          BlobAsHex := '';
          Buffer := DTPassword.Memory;
          for i := 0 to DTPassword.Size - 1 do
          begin
            BlobAsHex := BlobAsHex + IntToHex(Buffer^, 2);
            Inc(Buffer); // Avanzar al siguiente byte
          end;

          // Construir la consulta SQL para insertar un nuevo usuario
          SQLQuery := 'INSERT INTO Empleados (DataUser, DataPassword, Nombre) ' +
                      'VALUES (''' + EditDataUser.Text + ''', 0x' + BlobAsHex + ', ''' + EditNombre.Text + ''')';

          // Ejecutar la consulta
          DataModule1.ADOQueryEmpleadoUpdate.SQL.Text := SQLQuery;
          DataModule1.ADOQueryEmpleadoUpdate.ExecSQL;

          ShowMessage('Usuario agregado exitosamente.');
        finally
          DTPassword.Free;
        end;
      end;

      meModificar:
        begin
          // Inicializar el stream para almacenar el array de bytes
          DTPassword := TMemoryStream.Create;
          try
            // Escribir los datos al stream
            DTPassword.WriteBuffer(PasswordBytes[0], Length(PasswordBytes));

            // Convertir el contenido del Stream a una representación hexadecimal
            BlobAsHex := '';
            Buffer := DTPassword.Memory;
            for i := 0 to DTPassword.Size - 1 do
            begin
              BlobAsHex := BlobAsHex + IntToHex(Buffer^, 2);
              Inc(Buffer); // Avanzar al siguiente byte
            end;

            // Construir la consulta SQL para actualizar un usuario existente
            SQLQuery := 'UPDATE Empleados SET ' +
                        'DataPassword = 0x' + BlobAsHex + ', ' +
                        'Nombre = ''' + EditNombre.Text + ''' ' +
                        'WHERE DataUser = ''' + EditDataUser.Text + '''';

            // Ejecutar la consulta
            DataModule1.ADOQueryEmpleadoUpdate.SQL.Text := SQLQuery;
            DataModule1.ADOQueryEmpleadoUpdate.ExecSQL;

            ShowMessage('Usuario modificado exitosamente.');
          finally
            DTPassword.Free;
          end;
        end;
    end;

    // Volver al modo de navegación y actualizar el estado de los campos
    ModoEdicion := meNavegacion;
    ActualizarEstadoCampos;

    // Si se quiere recargar los datos después de la operación, usa ADOQueryEmpleados:
    DataModule1.ADOQueryEmpleados.Close;
    DataModule1.ADOQueryEmpleados.Open; // Para recargar los registros del DBGrid

  except
    on E: Exception do
      ShowMessage('Error al guardar: ' + E.Message);
  end;
end;

// Botón Eliminar: elimina el registro actual con confirmación
procedure TFormEmpleados.BtnEliminarClick(Sender: TObject);
begin
  if not DataModule1.ADOQueryEmpleados.IsEmpty then
  begin
    if MessageDlg('¿Estás seguro de que deseas eliminar este usuario?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      try
        DataModule1.ADOQueryEmpleados.Delete;
        ShowMessage('Usuario eliminado.');
      except
        on E: Exception do
          ShowMessage('Error al eliminar: ' + E.Message);
      end;
    end;
  end
  else
    ShowMessage('No hay registros para eliminar.');
end;

// Eventos de los botones

procedure TFormEmpleados.BtnPrimerRegClick(Sender: TObject);
begin
  CargarPrimerRegistro;
end;

procedure TFormEmpleados.BtnRegAnteriorClick(Sender: TObject);
begin
  CargarRegistroAnterior;
end;

procedure TFormEmpleados.BtnRegSiguienteClick(Sender: TObject);
begin
  CargarRegistroSiguiente;
end;

procedure TFormEmpleados.BtnUltimoRegClick(Sender: TObject);
begin
  CargarUltimoRegistro;
end;

procedure TFormEmpleados.DataSourceUsuariosDataChange(Sender: TObject;
  Field: TField);
var
  BlobStream: TMemoryStream;
  PasswordString: AnsiString;
begin
  if Field = nil then
  begin
    BlobStream := TMemoryStream.Create;
    try
      if not DataModule1.ADOQueryEmpleados.FieldByName('DataPassword').IsNull then
      begin
        // Leer los datos del campo BLOB
        (DataModule1.ADOQueryEmpleados.FieldByName('DataPassword') as TBlobField).SaveToStream(BlobStream);
        SetLength(PasswordString, BlobStream.Size);
        BlobStream.Position := 0;
        BlobStream.ReadBuffer(PasswordString[1], BlobStream.Size);

        // Mostrar en el campo de texto
        EditDataPassword.Text := String(PasswordString);
      end
      else
        EditDataPassword.Text := '';
    finally
      BlobStream.Free;
    end;
  end;
end;

function TFormEmpleados.StringToVarBinary(const S: string): TByteArray;
var
  I: Integer;
begin
  SetLength(Result, Length(S)); // Asignamos el tamaño del arreglo
  for I := 1 to Length(S) do
    Result[I - 1] := Ord(S[I]);  // Convertimos el string a bytes
end;

end.
