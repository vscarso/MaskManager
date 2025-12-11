unit uRegrasEditorForm;

// Autor: Vitor Scarso
// Formulário visual do editor de regras para TMaskManager.
// Requer uMaskManager na uses e dependências LCL + IDEIntf no pacote.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, Dialogs,
  uMaskManager;

type
  TRegrasEditorForm = class(TForm)
    lvRegras: TListView;
    btnAdicionar: TButton;
    btnEditar: TButton;
    btnRemover: TButton;
    btnOrdenarCampo: TButton;
    btnOrdenarTabela: TButton;
    btnExportarCSV: TButton;
    btnExportarINI: TButton;
    btnImportarCSV: TButton;
    btnImportarINI: TButton;
    btnFechar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lvRegrasColumnClick(Sender: TObject; Column: TListColumn);
    procedure btnAdicionarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnRemoverClick(Sender: TObject);
    procedure btnOrdenarCampoClick(Sender: TObject);
    procedure btnOrdenarTabelaClick(Sender: TObject);
    procedure btnExportarCSVClick(Sender: TObject);
    procedure btnExportarINIClick(Sender: TObject);
    procedure btnImportarCSVClick(Sender: TObject);
    procedure btnImportarINIClick(Sender: TObject);
    procedure btnFecharClick(Sender: TObject);
  private
    FRegras: TListaRegras;
    procedure CarregarLista;
    procedure AtualizarItem(AItem: TListItem; ARegra: TRegraMascara);
    function RegraSelecionada: TRegraMascara;
  public
    procedure EditarRegras(ARegras: TListaRegras);
  end;

implementation

{$R *.lfm}

procedure TRegrasEditorForm.FormCreate(Sender: TObject);
begin
  lvRegras.ViewStyle := vsReport;
  lvRegras.ReadOnly := True;
  lvRegras.RowSelect := True;
  lvRegras.HideSelection := False;
end;

procedure TRegrasEditorForm.EditarRegras(ARegras: TListaRegras);
begin
  FRegras := ARegras;
  CarregarLista;
  ShowModal;
end;

procedure TRegrasEditorForm.CarregarLista;
var
  i: Integer;
  Item: TListItem;
  R: TRegraMascara;
begin
  lvRegras.Items.Clear;
  if FRegras = nil then Exit;
  for i := 0 to FRegras.Count - 1 do
  begin
    R := TRegraMascara(FRegras.Items[i]);
    Item := lvRegras.Items.Add;
    AtualizarItem(Item, R);
    Item.Data := R;
  end;
end;

procedure TRegrasEditorForm.AtualizarItem(AItem: TListItem; ARegra: TRegraMascara);
begin
  AItem.Caption := ARegra.Campo;
  AItem.SubItems.Clear;
  AItem.SubItems.Add(ARegra.Tabela);
  AItem.SubItems.Add(ARegra.DisplayFormat);
  AItem.SubItems.Add(ARegra.EditFormat);
end;

function TRegrasEditorForm.RegraSelecionada: TRegraMascara;
begin
  Result := nil;
  if (lvRegras.Selected <> nil) and (lvRegras.Selected.Data <> nil) then
    Result := TRegraMascara(lvRegras.Selected.Data);
end;

procedure TRegrasEditorForm.lvRegrasColumnClick(Sender: TObject; Column: TListColumn);
begin
  lvRegras.SortType := stText;
  lvRegras.AlphaSort;
end;

procedure TRegrasEditorForm.btnAdicionarClick(Sender: TObject);
var
  R: TRegraMascara;
  Campo, Tabela, Disp, Edit: string;
  Item: TListItem;
begin
  Campo := '';
  Tabela := '';
  Disp := '';
  Edit := '';

  if InputQuery('Adicionar regra', 'Campo (FieldName):', Campo) then
  begin
    if InputQuery('Adicionar regra', 'Tabela (Name do DataSet):', Tabela) then
    begin
      InputQuery('Adicionar regra', 'DisplayFormat:', Disp);
      InputQuery('Adicionar regra', 'EditFormat:', Edit);

      R := FRegras.Adicionar(Tabela, Campo, Disp, Edit);
      Item := lvRegras.Items.Add;
      AtualizarItem(Item, R);
      Item.Data := R;
    end;
  end;
end;

procedure TRegrasEditorForm.btnEditarClick(Sender: TObject);
var
  R: TRegraMascara;
  Campo, Tabela, Disp, Edit: string;
begin
  R := RegraSelecionada;
  if R = nil then Exit;

  Campo := R.Campo;
  Tabela := R.Tabela;
  Disp := R.DisplayFormat;
  Edit := R.EditFormat;

  if InputQuery('Editar regra', 'Campo (FieldName):', Campo) then
  begin
    if InputQuery('Editar regra', 'Tabela (Name do DataSet):', Tabela) then
    begin
      InputQuery('Editar regra', 'DisplayFormat:', Disp);
      InputQuery('Editar regra', 'EditFormat:', Edit);

      R.Campo := Campo;
      R.Tabela := Tabela;
      R.DisplayFormat := Disp;
      R.EditFormat := Edit;

      AtualizarItem(lvRegras.Selected, R);
    end;
  end;
end;

procedure TRegrasEditorForm.btnRemoverClick(Sender: TObject);
var
  idx: Integer;
begin
  if lvRegras.Selected = nil then Exit;
  idx := lvRegras.Selected.Index;
  if (idx >= 0) and (idx < FRegras.Count) then
  begin
    FRegras.Delete(idx);
    lvRegras.Items.Delete(idx);
  end;
end;

procedure TRegrasEditorForm.btnOrdenarCampoClick(Sender: TObject);
begin
  FRegras.OrdenarPorCampo;
  CarregarLista;
end;

procedure TRegrasEditorForm.btnOrdenarTabelaClick(Sender: TObject);
begin
  FRegras.OrdenarPorTabela;
  CarregarLista;
end;

procedure TRegrasEditorForm.btnExportarCSVClick(Sender: TObject);
var
  D: TSaveDialog;
begin
  D := TSaveDialog.Create(nil);
  try
    D.Title := 'Exportar CSV';
    D.Filter := 'CSV|*.csv|Todos|*.*';
    D.DefaultExt := 'csv';
    D.FileName := 'Regras.csv';
    if D.Execute then
      FRegras.ExportarCSV(D.FileName);
  finally
    D.Free;
  end;
end;

procedure TRegrasEditorForm.btnExportarINIClick(Sender: TObject);
var
  D: TSaveDialog;
begin
  D := TSaveDialog.Create(nil);
  try
    D.Title := 'Exportar INI';
    D.Filter := 'INI|*.ini|Todos|*.*';
    D.DefaultExt := 'ini';
    D.FileName := 'Regras.ini';
    if D.Execute then
      FRegras.ExportarINI(D.FileName);
  finally
    D.Free;
  end;
end;

procedure TRegrasEditorForm.btnImportarCSVClick(Sender: TObject);
var
  D: TOpenDialog;
begin
  D := TOpenDialog.Create(nil);
  try
    D.Title := 'Importar CSV';
    D.Filter := 'CSV|*.csv|Todos|*.*';
    if D.Execute then
    begin
      FRegras.ImportarCSV(D.FileName);
      CarregarLista;
    end;
  finally
    D.Free;
  end;
end;

procedure TRegrasEditorForm.btnImportarINIClick(Sender: TObject);
var
  D: TOpenDialog;
begin
  D := TOpenDialog.Create(nil);
  try
    D.Title := 'Importar INI';
    D.Filter := 'INI|*.ini|Todos|*.*';
    if D.Execute then
    begin
      FRegras.ImportarINI(D.FileName);
      CarregarLista;
    end;
  finally
    D.Free;
  end;
end;

procedure TRegrasEditorForm.btnFecharClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

