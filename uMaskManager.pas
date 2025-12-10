unit uMaskManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  // Item de regra: Tabela + Campo + Máscara
  TRegraMascara = class(TCollectionItem)
  private
    FTabela: string;
    FCampo: string;
    FDisplay: string;
    FEdit: string;
  published
    property Tabela: string read FTabela write FTabela;
    property Campo: string read FCampo write FCampo;
    property DisplayFormat: string read FDisplay write FDisplay;
    property EditFormat: string read FEdit write FEdit;
  end;

  // Coleção de regras
  TListaRegras = class(TCollection)
  public
    function AddRegra(const ATabela, ACampo, ADisplay, AEdit: string): TRegraMascara;
  end;

  // Estrutura auxiliar para guardar dataset + evento original
  TDataSetInfo = class
  public
    DataSet: TDataSet;
    OldAfterOpen: TDataSetNotifyEvent;
  end;

  // Componente principal
  TMaskManager = class(TComponent)
  private
    FDataSets: TList;
    FMascaraPadrao: string;
    FRegras: TListaRegras;
    procedure InternalAfterOpen(DataSet: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegistrarDataSet(ADataSet: TDataSet);
  published
    property MascaraPadrao: string read FMascaraPadrao write FMascaraPadrao;
    property Regras: TListaRegras read FRegras write FRegras;
  end;

procedure Register;

implementation

{ TListaRegras }

function TListaRegras.AddRegra(const ATabela, ACampo, ADisplay, AEdit: string): TRegraMascara;
begin
  Result := TRegraMascara(Add);
  Result.Tabela := ATabela;
  Result.Campo := ACampo;
  Result.DisplayFormat := ADisplay;
  Result.EditFormat := AEdit;
end;

{ TMaskManager }

constructor TMaskManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSets := TList.Create;
  FRegras := TListaRegras.Create(TRegraMascara);
  FMascaraPadrao := '0.00'; // padrão inicial
end;

destructor TMaskManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDataSets.Count - 1 do
    TObject(FDataSets[i]).Free;
  FDataSets.Free;
  FRegras.Free;
  inherited Destroy;
end;

procedure TMaskManager.RegistrarDataSet(ADataSet: TDataSet);
var
  Info: TDataSetInfo;
begin
  Info := TDataSetInfo.Create;
  Info.DataSet := ADataSet;
  Info.OldAfterOpen := ADataSet.AfterOpen; // guarda o evento original
  ADataSet.AfterOpen := @InternalAfterOpen; // substitui pelo nosso
  FDataSets.Add(Info);
end;

procedure TMaskManager.InternalAfterOpen(DataSet: TDataSet);
var
  f: TField;
  regra: TRegraMascara;
  i, k: Integer;
  Info: TDataSetInfo;
begin
  // aplica máscara padrão
  for f in DataSet.Fields do
  begin
    if (f is TFloatField) or (f is TBCDField) or (f is TFMTBCDField) then
    begin
      TFloatField(f).DisplayFormat := FMascaraPadrao;
      TFloatField(f).EditFormat := FMascaraPadrao;
    end;

    // procura regra específica
    for k := 0 to FRegras.Count - 1 do
    begin
      regra := TRegraMascara(FRegras.Items[k]);
      if SameText(regra.Tabela, DataSet.Name) and SameText(regra.Campo, f.FieldName) then
      begin
        if regra.DisplayFormat <> '' then
          TFloatField(f).DisplayFormat := regra.DisplayFormat;
        if regra.EditFormat <> '' then
          TFloatField(f).EditFormat := regra.EditFormat;
      end;
    end;
  end;

  // chama o evento original, se existir
  for i := 0 to FDataSets.Count - 1 do
  begin
    Info := TDataSetInfo(FDataSets[i]);
    if Info.DataSet = DataSet then
    begin
      if Assigned(Info.OldAfterOpen) then
        Info.OldAfterOpen(DataSet);
      Break;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Utilitarios', [TMaskManager]);
end;

end.
