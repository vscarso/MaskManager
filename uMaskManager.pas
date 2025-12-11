unit uMaskManager;

// Autor: Vitor Scarso
// Descrição: Componente para aplicar DisplayFormat/EditFormat em campos numéricos,
// com regras por DataSet (Name do componente) + FieldName, normalização case-insensitive,
// ordenação e exportação/importação.
// Paleta: VSComponents

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  // Item de regra: Tabela + Campo + Máscaras
  TRegraMascara = class(TCollectionItem)
  private
    FTabela: string;   // armazenado normalizado (UpperCase)
    FCampo: string;    // armazenado normalizado (UpperCase)
    FDisplay: string;
    FEdit: string;
    procedure SetTabela(const Value: string);
    procedure SetCampo(const Value: string);
  protected
    function GetDisplayName: string; override; // "Campo (Tabela)" no Object Inspector
  published
    property Tabela: string read FTabela write SetTabela;    // Name do componente DataSet (case-insensitive)
    property Campo: string read FCampo write SetCampo;       // FieldName (case-insensitive)
    property DisplayFormat: string read FDisplay write FDisplay;
    property EditFormat: string read FEdit write FEdit;
  end;

  // Coleção de regras
  TListaRegras = class(TCollection)
  public
    function Adicionar(const ATabela, ACampo, ADisplay, AEdit: string): TRegraMascara;
    procedure OrdenarPorCampo;
    procedure OrdenarPorTabela;
    procedure ExportarCSV(const Arquivo: string);
    procedure ExportarINI(const Arquivo: string);
    procedure ImportarCSV(const Arquivo: string);
    procedure ImportarINI(const Arquivo: string);
    procedure Salvar(const Arquivo: string);   // atalho para ExportarINI
    procedure Carregar(const Arquivo: string); // atalho para ImportarINI
  end;

  // Guarda dataset + evento original
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
    procedure AplicarFormatos(f: TField; const DisplayFmt, EditFmt: string);
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

{ TRegraMascara }

procedure TRegraMascara.SetTabela(const Value: string);
begin
  FTabela := UpperCase(Trim(Value));
end;

procedure TRegraMascara.SetCampo(const Value: string);
begin
  FCampo := UpperCase(Trim(Value));
end;

function TRegraMascara.GetDisplayName: string;
begin
  if (FCampo <> '') and (FTabela <> '') then
    Result := FCampo + ' (' + FTabela + ')'
  else if FCampo <> '' then
    Result := FCampo
  else
    Result := inherited GetDisplayName;
end;

{ TListaRegras }

function TListaRegras.Adicionar(const ATabela, ACampo, ADisplay, AEdit: string): TRegraMascara;
begin
  Result := TRegraMascara(Add);
  Result.Tabela := ATabela;   // setter normaliza
  Result.Campo := ACampo;     // setter normaliza
  Result.DisplayFormat := ADisplay;
  Result.EditFormat := AEdit;
end;

procedure TListaRegras.OrdenarPorCampo;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupAccept;
    for i := 0 to Count - 1 do
      SL.AddObject(TRegraMascara(Items[i]).Campo, Items[i]);
    Clear;
    for i := 0 to SL.Count - 1 do
      Add.Assign(TRegraMascara(SL.Objects[i]));
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.OrdenarPorTabela;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupAccept;
    for i := 0 to Count - 1 do
      SL.AddObject(TRegraMascara(Items[i]).Tabela, Items[i]);
    Clear;
    for i := 0 to SL.Count - 1 do
      Add.Assign(TRegraMascara(SL.Objects[i]));
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.ExportarCSV(const Arquivo: string);
var
  SL: TStringList;
  i: Integer;
  R: TRegraMascara;
begin
  SL := TStringList.Create;
  try
    SL.Add('Campo;Tabela;DisplayFormat;EditFormat');
    for i := 0 to Count - 1 do
    begin
      R := TRegraMascara(Items[i]);
      SL.Add(Format('%s;%s;%s;%s', [R.Campo, R.Tabela, R.DisplayFormat, R.EditFormat]));
    end;
    SL.SaveToFile(Arquivo);
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.ExportarINI(const Arquivo: string);
var
  SL: TStringList;
  i: Integer;
  R: TRegraMascara;
begin
  SL := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      R := TRegraMascara(Items[i]);
      SL.Add('[' + IntToStr(i + 1) + ']');
      SL.Add('Campo=' + R.Campo);
      SL.Add('Tabela=' + R.Tabela);
      SL.Add('DisplayFormat=' + R.DisplayFormat);
      SL.Add('EditFormat=' + R.EditFormat);
      SL.Add('');
    end;
    SL.SaveToFile(Arquivo);
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.ImportarCSV(const Arquivo: string);
var
  SL: TStringList;
  i: Integer;
  P: TStringArray;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Arquivo);
    Clear;
    // espera cabeçalho na primeira linha: Campo;Tabela;DisplayFormat;EditFormat
    for i := 1 to SL.Count - 1 do
    begin
      P := SL[i].Split([';']);
      if Length(P) >= 4 then
        Adicionar(P[1], P[0], P[2], P[3]); // Tabela, Campo, Display, Edit
    end;
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.ImportarINI(const Arquivo: string);
var
  SL: TStringList;
  i: Integer;
  Campo, Tabela, Disp, Edit: string;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Arquivo);
    Clear;
    Campo := ''; Tabela := ''; Disp := ''; Edit := '';
    for i := 0 to SL.Count - 1 do
    begin
      if Pos('Campo=', SL[i]) = 1 then Campo := Copy(SL[i], 7, MaxInt);
      if Pos('Tabela=', SL[i]) = 1 then Tabela := Copy(SL[i], 8, MaxInt);
      if Pos('DisplayFormat=', SL[i]) = 1 then Disp := Copy(SL[i], 14, MaxInt);
      if Pos('EditFormat=', SL[i]) = 1 then
      begin
        Edit := Copy(SL[i], 12, MaxInt);
        Adicionar(Tabela, Campo, Disp, Edit);
        Campo := ''; Tabela := ''; Disp := ''; Edit := '';
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TListaRegras.Salvar(const Arquivo: string);
begin
  ExportarINI(Arquivo);
end;

procedure TListaRegras.Carregar(const Arquivo: string);
begin
  ImportarINI(Arquivo);
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

procedure TMaskManager.AplicarFormatos(f: TField; const DisplayFmt, EditFmt: string);
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
  DSNameUpper, FieldUpper: string;
begin
  // aplica máscara padrão
  for f in DataSet.Fields do
    AplicarFormatos(f, FMascaraPadraoDisplay, FMascaraPadraoEdit);

  // normaliza nome do DataSet para comparação
  DSNameUpper := UpperCase(Trim(DataSet.Name));

  // aplica regras específicas
  for f in DataSet.Fields do
  begin
    FieldUpper := UpperCase(Trim(f.FieldName));
    for k := 0 to FRegras.Count - 1 do
    begin
      regra := TRegraMascara(FRegras.Items[k]);
      if (regra.Tabela = DSNameUpper) and (regra.Campo = FieldUpper) then
        AplicarFormatos(f, regra.DisplayFormat, regra.EditFormat);
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
  RegisterComponents('VSComponents', [TMaskManager]); // paleta ajustada
end;

end.

