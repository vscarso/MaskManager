# TMaskManager

Componente para Lazarus/FPC criado por **Vitor Scarso**.  
Objetivo: aplicar automaticamente `DisplayFormat` e `EditFormat` em campos numéricos de `TDataSet`, com regras configuráveis por DataSet + FieldName.

## ✨ Funcionalidades

- Aplica máscaras de exibição/edição em campos numéricos (`TFloatField`, `TBCDField`, `TFMTBCDField`).
- Regras configuráveis por **nome do componente DataSet** + **FieldName** (case-insensitive).
- Máscaras padrão configuráveis (`MascaraPadraoDisplay`, `MascaraPadraoEdit`).
- Editor visual integrado ao Lazarus (Object Inspector).
- Exportação e importação de regras em **CSV** e **INI**.
- Ordenação de regras por campo ou por tabela.

## 📦 Instalação

1. Adicione as units ao seu projeto/pacote:
   - `uMaskManager.pas`
   - `uMaskManagerEditor.pas`
   - `uRegrasEditorForm.pas`
2. Instale o pacote no Lazarus.
3. O componente aparecerá na paleta **VSComponents**.

## 🛠️ Uso

1. Coloque um `TMaskManager` no seu Form ou DataModule.
2. Registre os DataSets que devem receber máscaras:
   ```pascal
   MaskManager.RegistrarDataSet(FDNfeItens);
   MaskManager.RegistrarDataSet(FDProdutos);
Configure as regras pelo Object Inspector:

Propriedade Regras → abre o editor visual.

Informe o nome do componente DataSet (ex.: FDNfeItens) e o campo (ex.: VALORTOTAL).

Defina DisplayFormat e EditFormat.

Ao abrir o DataSet (AfterOpen), o TMaskManager aplica automaticamente as máscaras.

📂 Exportação/Importação
CSV: gera arquivo com colunas Campo;Tabela;DisplayFormat;EditFormat.

INI: gera arquivo com seções numeradas e chaves Campo, Tabela, DisplayFormat, EditFormat.

📋 Exemplo
pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  MaskManager.RegistrarDataSet(FDNfeItens);

  // Adiciona regra manualmente
  MaskManager.Regras.Adicionar('FDNfeItens', 'VALORTOTAL', '#,##0.00', '0.00');
end;
⚠️ Observações
Os nomes de DataSet e campos são tratados sem diferença de maiúsculas/minúsculas.

Caracteres como _ são preservados (IMPOSTO_IBSUF funciona normalmente).

Se o campo estiver nulo, o DisplayFormat não substitui por 0. Para isso, use DEFAULT 0 no banco ou o evento OnGetText do campo.

📜 Licença
Este projeto é distribuído sob a licença MIT. Sinta-se livre para usar, modificar e compartilhar.
