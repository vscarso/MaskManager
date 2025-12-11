unit uSelecionarCampo;

// Autor: Vitor Scarso
// Formulário auxiliar para selecionar DataSet e Field sem digitar manualmente.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DB;

type

  { TSelecionarCampoForm }

  TSelecionarCampoForm = class(TForm)
    BtnCancelar: TButton;
    cbDataSets: TComboBox;
    cbCampos: TComboBox;
    btnOK: TButton;
    DataSet: TLabel;
    Label1: TLabel;
    lblDataSet: TLabel;
    lblCampo: TLabel;
    procedure BtnCancelarChange(Sender: TObject);
    procedure cbCamposChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbDataSetsChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    FOwnerComp: TComponent;
    FDataSetSelecionado: string;
    FCampoSelecionado: string;
    procedure CarregarDataSets;
    procedure CarregarCampos(const NomeDataSet: string);
  public
    class function SelecionarCampo(AOwner: TComponent; out NomeDataSet, NomeCampo: string): Boolean;
  end;

implementation

{$R *.lfm}

procedure TSelecionarCampoForm.FormCreate(Sender: TObject);
begin
  CarregarDataSets;
end;

procedure TSelecionarCampoForm.BtnCancelarChange(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

procedure TSelecionarCampoForm.cbCamposChange(Sender: TObject);
begin

end;

procedure TSelecionarCampoForm.CarregarDataSets;
var
  i: Integer;
begin
  cbDataSets.Items.Clear;
  if FOwnerComp = nil then Exit;

  for i := 0 to FOwnerComp.ComponentCount - 1 do
    if FOwnerComp.Components[i] is TDataSet then
      cbDataSets.Items.Add(FOwnerComp.Components[i].Name);
end;

procedure TSelecionarCampoForm.CarregarCampos(const NomeDataSet: string);
var
  ds: TDataSet;
  i: Integer;
begin
  cbCampos.Items.Clear;
  ds := TDataSet(FOwnerComp.FindComponent(NomeDataSet));
  if ds = nil then Exit;

  for i := 0 to ds.Fields.Count - 1 do
    cbCampos.Items.Add(ds.Fields[i].FieldName);
end;

procedure TSelecionarCampoForm.cbDataSetsChange(Sender: TObject);
begin
  CarregarCampos(cbDataSets.Text);
end;

procedure TSelecionarCampoForm.btnOKClick(Sender: TObject);
begin
  FDataSetSelecionado := cbDataSets.Text;
  FCampoSelecionado := cbCampos.Text;
  ModalResult := mrOk;
end;

procedure TSelecionarCampoForm.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TSelecionarCampoForm.SelecionarCampo(AOwner: TComponent; out NomeDataSet, NomeCampo: string): Boolean;
var
  Frm: TSelecionarCampoForm;
begin
  Frm := TSelecionarCampoForm.Create(nil);
  try
    Frm.FOwnerComp := AOwner;
    if Frm.ShowModal = mrOk then
    begin
      NomeDataSet := Frm.FDataSetSelecionado;
      NomeCampo := Frm.FCampoSelecionado;
      Result := True;
    end
    else
      Result := False;
  finally
    Frm.Free;
  end;
end;

end.

