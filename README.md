📘 TMaskManager
Autor: Vitor Scarso Licença: MIT

✨ Descrição
O TMaskManager é um componente para Lazarus que aplica máscaras de exibição (DisplayFormat) e máscaras de edição (EditFormat) em campos numéricos (Float, BCD, FMTBCD) de datasets (TZQuery, TQuery, etc.) de forma centralizada e automática.

Ele foi criado para simplificar a manutenção de projetos que possuem muitas queries, evitando a necessidade de configurar máscaras campo por campo.

🚀 Funcionalidades
✅ Máscara padrão para todos os campos numéricos.

✅ Regras específicas por DataSet + Campo.

✅ Configuração direta pelo Object Inspector.

✅ Aplicação automática ao abrir o dataset (sem precisar chamar manualmente).

✅ Encadeamento de eventos: não sobrescreve o AfterOpen original do dataset.

✅ Suporte a DisplayFormat e EditFormat (visualização e edição).

⚙️ Instalação
Crie um arquivo chamado uMaskManager.pas e cole o código da unit.

No Lazarus, vá em Pacotes → Novo pacote.

Adicione a unit uMaskManager.pas ao pacote.

Compile e instale o pacote.

O componente TMaskManager aparecerá na paleta VSComponents.

🔧 Propriedades
MascaraPadraoDisplay Define a máscara padrão aplicada a todos os campos numéricos para exibição. Exemplo: #,##0.00.

MascaraPadraoEdit Define a máscara padrão aplicada a todos os campos numéricos para edição. Exemplo: 0.00.

Regras Coleção de regras específicas. Cada regra possui:

Tabela: Name do componente DataSet no Lazarus (ex.: FDNfeItens). ⚠️ Importante: não é o nome da tabela do banco de dados, mas sim o Name do componente no Object Inspector.

Campo: nome do campo (FieldName) exatamente como aparece no FieldsEditor ou no DBGrid.

DisplayFormat: máscara de exibição (ex.: #,##0.000).

EditFormat: máscara de edição (ex.: 0.###).

📋 Métodos
RegistrarDataSet(ADataSet: TDataSet) Registra um dataset para que o MaskManager aplique máscaras nele. O componente intercepta o AfterOpen e aplica as regras automaticamente.

🧑‍💻 Exemplo de uso
pascal
procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  // Configuração padrão
  MaskManager1.MascaraPadraoDisplay := '#,##0.00';
  MaskManager1.MascaraPadraoEdit := '0.00';

  // Regras específicas
  MaskManager1.Regras.AddRegra('FDNfeItens', 'QTD', '0.000', '0.###');
  MaskManager1.Regras.AddRegra('FDNfe', 'VALORALIQUOTA', '#,##0.0000', '0.0000');

  // Registrar datasets (Name do componente, não nome da tabela)
  MaskManager1.RegistrarDataSet(FDNfe);
  MaskManager1.RegistrarDataSet(FDNfeItens);
end;
🎯 Benefícios
Centralização: todas as máscaras ficam em um único componente.

Flexibilidade: regras específicas por DataSet + Campo.

Manutenção fácil: basta alterar no MaskManager.

Integração com Lazarus: configuração pelo Object Inspector.

Automático: não precisa mais chamar AplicarMascaras manualmente.

Completo: suporta DisplayFormat e EditFormat.

👨‍💻 Autor
Criado por Vitor Scarso Publicado sob licença MIT.
