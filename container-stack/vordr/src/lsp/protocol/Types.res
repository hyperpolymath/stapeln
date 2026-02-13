// SPDX-License-Identifier: PMPL-1.0-or-later
// LSP Protocol Types for Vörðr

// LSP base protocol types
type position = {
  line: int,
  character: int,
}

type range = {
  start: position,
  \"end": position,
}

type location = {
  uri: string,
  range: range,
}

type textDocumentIdentifier = {
  uri: string,
}

type versionedTextDocumentIdentifier = {
  uri: string,
  version: int,
}

type textDocumentItem = {
  uri: string,
  languageId: string,
  version: int,
  text: string,
}

// Diagnostic severity
type diagnosticSeverity =
  | @as(1) Error
  | @as(2) Warning
  | @as(3) Information
  | @as(4) Hint

type diagnostic = {
  range: range,
  severity: option<diagnosticSeverity>,
  code: option<string>,
  source: option<string>,
  message: string,
  relatedInformation: option<array<diagnosticRelatedInformation>>,
}

and diagnosticRelatedInformation = {
  location: location,
  message: string,
}

// Completion types
type completionItemKind =
  | @as(1) Text
  | @as(2) Method
  | @as(3) Function
  | @as(4) Constructor
  | @as(5) Field
  | @as(6) Variable
  | @as(7) Class
  | @as(8) Interface
  | @as(9) Module
  | @as(10) Property
  | @as(11) Unit
  | @as(12) Value
  | @as(13) Enum
  | @as(14) Keyword
  | @as(15) Snippet
  | @as(16) Color
  | @as(17) File
  | @as(18) Reference
  | @as(19) Folder
  | @as(20) EnumMember
  | @as(21) Constant
  | @as(22) Struct
  | @as(23) Event
  | @as(24) Operator
  | @as(25) TypeParameter

type completionItem = {
  label: string,
  kind: option<completionItemKind>,
  detail: option<string>,
  documentation: option<string>,
  deprecated: option<bool>,
  preselect: option<bool>,
  sortText: option<string>,
  filterText: option<string>,
  insertText: option<string>,
  textEdit: option<textEdit>,
}

and textEdit = {
  range: range,
  newText: string,
}

// Hover types
type markedString =
  | Language({language: string, value: string})
  | String(string)

type hoverContents =
  | MarkedString(markedString)
  | MarkedStringArray(array<markedString>)
  | MarkupContent({kind: string, value: string})

type hover = {
  contents: hoverContents,
  range: option<range>,
}

// Code Action types
type codeActionKind =
  | @as("") Empty
  | @as("quickfix") QuickFix
  | @as("refactor") Refactor
  | @as("refactor.extract") RefactorExtract
  | @as("refactor.inline") RefactorInline
  | @as("refactor.rewrite") RefactorRewrite
  | @as("source") Source
  | @as("source.organizeImports") SourceOrganizeImports

type codeAction = {
  title: string,
  kind: option<codeActionKind>,
  diagnostics: option<array<diagnostic>>,
  isPreferred: option<bool>,
  edit: option<workspaceEdit>,
  command: option<command>,
}

and workspaceEdit = {
  changes: option<Js.Dict.t<array<textEdit>>>,
}

and command = {
  title: string,
  command: string,
  arguments: option<array<Js.Json.t>>,
}

// Server capabilities
type textDocumentSyncKind =
  | @as(0) None
  | @as(1) Full
  | @as(2) Incremental

type completionOptions = {
  resolveProvider: option<bool>,
  triggerCharacters: option<array<string>>,
}

type hoverOptions = {
  workDoneProgress: option<bool>,
}

type codeActionOptions = {
  codeActionKinds: option<array<codeActionKind>>,
  workDoneProgress: option<bool>,
}

type serverCapabilities = {
  textDocumentSync: option<textDocumentSyncKind>,
  completionProvider: option<completionOptions>,
  hoverProvider: option<hoverOptions>,
  diagnosticProvider: option<bool>,
  codeActionProvider: option<codeActionOptions>,
}

// Initialize types
type clientInfo = {
  name: string,
  version: option<string>,
}

type initializeParams = {
  processId: option<int>,
  clientInfo: option<clientInfo>,
  rootUri: option<string>,
  capabilities: clientCapabilities,
}

and clientCapabilities = {
  textDocument: option<textDocumentClientCapabilities>,
  workspace: option<workspaceClientCapabilities>,
}

and textDocumentClientCapabilities = {
  completion: option<completionClientCapabilities>,
  hover: option<hoverClientCapabilities>,
  diagnostic: option<diagnosticClientCapabilities>,
  codeAction: option<codeActionClientCapabilities>,
}

and completionClientCapabilities = {
  completionItem: option<completionItemClientCapabilities>,
}

and completionItemClientCapabilities = {
  snippetSupport: option<bool>,
}

and hoverClientCapabilities = {
  contentFormat: option<array<string>>,
}

and diagnosticClientCapabilities = {
  dynamicRegistration: option<bool>,
}

and codeActionClientCapabilities = {
  codeActionLiteralSupport: option<codeActionLiteralSupport>,
}

and codeActionLiteralSupport = {
  codeActionKind: option<codeActionKindLiteralSupport>,
}

and codeActionKindLiteralSupport = {
  valueSet: array<codeActionKind>,
}

and workspaceClientCapabilities = {
  applyEdit: option<bool>,
  workspaceEdit: option<workspaceEditClientCapabilities>,
}

and workspaceEditClientCapabilities = {
  documentChanges: option<bool>,
}

type initializeResult = {
  capabilities: serverCapabilities,
  serverInfo: option<serverInfo>,
}

and serverInfo = {
  name: string,
  version: option<string>,
}

// Vörðr-specific document types
type containerDocumentType =
  | @as("ctp") CtpBundle        // .ctp container bundle
  | @as("gatekeeper") Gatekeeper // Gatekeeper policy
  | @as("compose") Compose       // Docker Compose file
  | @as("dockerfile") Dockerfile // Dockerfile

type containerDocument = {
  uri: string,
  documentType: containerDocumentType,
  version: int,
  content: string,
}
