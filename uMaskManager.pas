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
    property Campo: string read FCampo write FCampo; // corrigido
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
    FMascaraPadraoDisplay: string;
    FMascaraPadraoEdit: string;
    FRegras: TListaRegras;
    procedure InternalAfterOpen(DataSet: TDataSet);
    procedure ApplyFormatsToField(f: TField; const DisplayFmt, EditFmt: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegistrarDataSet(ADataSet: TDataSet);
  published
    property MascaraPadraoDisplay: string read FMascaraPadraoDisplay write FMascaraPadraoDisplay;
    property MascaraPadraoEdit: string read FMascaraPadraoEdit write FMascaraPadraoEdit;
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
  FMascaraPadraoDisplay := '0.00'; // padrão inicial
  FMascaraPadraoEdit := '0.00';
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

procedure TMaskManager.ApplyFormatsToField(f: TField; const DisplayFmt, EditFmt: string);
begin
  if f is TFloatField then
  begin
    if DisplayFmt <> '' then TFloatField(f).DisplayFormat := DisplayFmt;
    if EditFmt <> '' then TFloatField(f).EditFormat := EditFmt;
  end
  else if f is TBCDField then
  begin
    if DisplayFmt <> '' then TBCDField(f).DisplayFormat := DisplayFmt;
    if EditFmt <> '' then TBCDField(f).EditFormat := EditFmt;
  end
  else if f is TFMTBCDField then
  begin
    if DisplayFmt <> '' then TFMTBCDField(f).DisplayFormat := DisplayFmt;
    if EditFmt <> '' then TFMTBCDField(f).EditFormat := EditFmt;
  end;
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
    ApplyFormatsToField(f, FMascaraPadraoDisplay, FMascaraPadraoEdit);

  // aplica regras específicas
  for f in DataSet.Fields do
  begin
    for k := 0 to FRegras.Count - 1 do
    begin
      regra := TRegraMascara(FRegras.Items[k]);
      if SameText(regra.Tabela, DataSet.Name) and SameText(regra.Campo, f.FieldName) then
        ApplyFormatsToField(f, regra.DisplayFormat, regra.EditFormat);
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
  RegisterComponents('VSComponents', [TMaskManager]); // ajustado para sua paleta
end;

end.

