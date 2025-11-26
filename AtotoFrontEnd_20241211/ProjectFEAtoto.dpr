program ProjectFEAtoto;

uses
  Forms,
  FmMenu in 'FmMenu.pas' {FormMenu},
  FmConexion in 'FmConexion.pas' {DataModule1: TDataModule},
  FmConsulta in 'FmConsulta.pas' {FormConsulta},
  FmLogin in 'FmLogin.pas' {FormLogin},
  FrmConsultas in 'FrmConsultas.pas' {FrameConsultas: TFrame},
  VariablesGlobales in 'VariablesGlobales.pas',
  FmLaboratorio in 'FmLaboratorio.pas' {FormLaboratorio},
  FMConsultasDetalle in 'FMConsultasDetalle.pas' {FormConsultasDetalle},
  FmEmpleados in 'FmEmpleados.pas' {FormEmpleados},
  FmOrdenesFab in 'FmOrdenesFab.pas' {FormOrdenesFab},
  FmPruebas in 'FmPruebas.pas' {FormPruebas};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMenu, FormMenu);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormConsulta, FormConsulta);
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormLaboratorio, FormLaboratorio);
  Application.CreateForm(TFormConsultasDetalle, FormConsultasDetalle);
  Application.CreateForm(TFormEmpleados, FormEmpleados);
  Application.CreateForm(TFormOrdenesFab, FormOrdenesFab);
  Application.CreateForm(TFormPruebas, FormPruebas);
  Application.Run;
end.
