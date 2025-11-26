object FormPruebas: TFormPruebas
  Left = 192
  Top = 118
  Width = 1305
  Height = 591
  Caption = 'FormPruebas'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 248
    Top = 240
    Width = 841
    Height = 257
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 832
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ADOQuery1: TADOQuery
    ConnectionString = 
      'Provider=MSDASQL;Password=ADITIVOS;Persist Security Info=True;Us' +
      'er ID=ADITIVOS;Data Source=DBAditivos32;Initial Catalog=aditivos'
    Parameters = <>
    SQL.Strings = (
      'SELECT * FROM BatchTable')
    Left = 1024
    Top = 144
    object ADOQuery1OrdenFab: TStringField
      FieldName = 'OrdenFab'
      FixedChar = True
      Size = 12
    end
    object ADOQuery1Fecha: TDateField
      FieldName = 'Fecha'
    end
    object ADOQuery1Hora: TStringField
      FieldName = 'Hora'
      FixedChar = True
      Size = 5
    end
    object ADOQuery1Producto: TStringField
      FieldName = 'Producto'
      FixedChar = True
      Size = 25
    end
    object ADOQuery1Cantidad: TStringField
      FieldName = 'Cantidad'
      FixedChar = True
      Size = 10
    end
    object ADOQuery1DescMateriaPrima: TStringField
      FieldName = 'DescMateriaPrima'
      FixedChar = True
      Size = 25
    end
    object ADOQuery1CantTeorica: TBCDField
      FieldName = 'CantTeorica'
      Precision = 8
      Size = 2
    end
    object ADOQuery1CantReal: TBCDField
      FieldName = 'CantReal'
      Precision = 8
      Size = 2
    end
  end
  object DataSource1: TDataSource
    DataSet = ADOQuery1
    Left = 1024
    Top = 200
  end
end
