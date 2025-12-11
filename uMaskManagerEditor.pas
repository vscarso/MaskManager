unit uMaskManagerEditor;

// Autor: Vitor Scarso
// Editor de propriedade para a coleção "Regras" do TMaskManager.
// Abre o formulário visual TRegrasEditorForm (uRegrasEditorForm).

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, uMaskManager, uRegrasEditorForm;

type
  // Property editor para a coleção "Regras"
  TRegrasPropertyEditor = class(TCollectionPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  // Substitui o editor padrão da propriedade "Regras" por nosso editor visual
  RegisterPropertyEditor(TypeInfo(TListaRegras), TMaskManager, 'Regras', TRegrasPropertyEditor);
end;

{ TRegrasPropertyEditor }

procedure TRegrasPropertyEditor.Edit;
var
  Frm: TRegrasEditorForm;
begin
  Frm := TRegrasEditorForm.Create(nil);
  try
    Frm.EditarRegras(TListaRegras(GetObjectValue));
  finally
    Frm.Free;
  end;
end;

function TRegrasPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TRegrasPropertyEditor.GetValue: ansistring;
begin
  Result := Format('(%d regras)', [TListaRegras(GetObjectValue).Count]);
end;

end.

