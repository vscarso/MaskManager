{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MaskManager;

{$warn 5023 off : no warning about unused units}
interface

uses
  uSelecionarCampo, uMaskManager, uMaskManagerEditor, uRegrasEditorForm, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uMaskManager', @uMaskManager.Register);
  RegisterUnit('uMaskManagerEditor', @uMaskManagerEditor.Register);
end;

initialization
  RegisterPackage('MaskManager', @Register);
end.
