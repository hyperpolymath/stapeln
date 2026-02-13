// SPDX-License-Identifier: PMPL-1.0-or-later
// LSP Server Implementation for Vörðr

open Types

// Server state
type state = {
  mutable initialized: bool,
  mutable documents: Js.Dict.t<containerDocument>,
  mutable clientCapabilities: option<clientCapabilities>,
}

let make = (): state => {
  initialized: false,
  documents: Js.Dict.empty(),
  clientCapabilities: None,
}

// JSON-RPC types
type jsonRpcRequest = {
  jsonrpc: string,
  method: string,
  params: option<Js.Json.t>,
  id: option<Js.Json.t>,
}

type jsonRpcResponse = {
  jsonrpc: string,
  id: option<Js.Json.t>,
  result: option<Js.Json.t>,
  error: option<jsonRpcError>,
}

and jsonRpcError = {
  code: int,
  message: string,
  data: option<Js.Json.t>,
}

// Helper functions
let success = (id: option<Js.Json.t>, result: Js.Json.t): jsonRpcResponse => {
  {jsonrpc: "2.0", id, result: Some(result), error: None}
}

let error = (id: option<Js.Json.t>, code: int, message: string): jsonRpcResponse => {
  {jsonrpc: "2.0", id, result: None, error: Some({code, message, data: None})}
}

// Initialize handler
let handleInitialize = (state: state, params: Js.Json.t, id: option<Js.Json.t>): jsonRpcResponse => {
  state.initialized = true

  // Parse client capabilities (simplified - would need full decoder in production)
  state.clientCapabilities = None

  let capabilities: serverCapabilities = {
    textDocumentSync: Some(Full),
    completionProvider: Some({
      resolveProvider: Some(false),
      triggerCharacters: Some([":", "/", "@"]),
    }),
    hoverProvider: Some({
      workDoneProgress: Some(false),
    }),
    diagnosticProvider: Some(true),
    codeActionProvider: Some({
      codeActionKinds: Some([QuickFix, Refactor]),
      workDoneProgress: Some(false),
    }),
  }

  let result = Js.Dict.fromArray([
    (
      "capabilities",
      %raw(`{
        textDocumentSync: 1,
        completionProvider: {
          resolveProvider: false,
          triggerCharacters: [":", "/", "@"]
        },
        hoverProvider: true,
        diagnosticProvider: true,
        codeActionProvider: {
          codeActionKinds: ["quickfix", "refactor"]
        }
      }`),
    ),
    (
      "serverInfo",
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("vordr-lsp")),
          ("version", Js.Json.string("0.1.0")),
        ]),
      ),
    ),
  ])

  success(id, Js.Json.object_(result))
}

// Text document handlers
let handleDidOpen = (state: state, params: Js.Json.t): option<jsonRpcResponse> => {
  // Parse textDocument from params
  // Store in state.documents
  // Return None for notifications (no response needed)
  None
}

let handleDidChange = (state: state, params: Js.Json.t): option<jsonRpcResponse> => {
  // Update document in state.documents
  None
}

let handleDidClose = (state: state, params: Js.Json.t): option<jsonRpcResponse> => {
  // Remove document from state.documents
  None
}

// Completion handler
let handleCompletion = (state: state, params: Js.Json.t, id: option<Js.Json.t>): jsonRpcResponse => {
  // Provide completions for:
  // - Container image names (from registry)
  // - Image tags
  // - Gatekeeper policy fields
  // - .ctp bundle fields

  let items = [
    Js.Json.object_(
      Js.Dict.fromArray([
        ("label", Js.Json.string("alpine:latest")),
        ("kind", Js.Json.number(12.0)), // Value
        ("detail", Js.Json.string("Alpine Linux base image")),
        ("insertText", Js.Json.string("alpine:latest")),
      ]),
    ),
    Js.Json.object_(
      Js.Dict.fromArray([
        ("label", Js.Json.string("ubuntu:22.04")),
        ("kind", Js.Json.number(12.0)),
        ("detail", Js.Json.string("Ubuntu 22.04 LTS")),
        ("insertText", Js.Json.string("ubuntu:22.04")),
      ]),
    ),
  ]

  success(id, Js.Json.array(items))
}

// Hover handler
let handleHover = (state: state, params: Js.Json.t, id: option<Js.Json.t>): jsonRpcResponse => {
  // Provide hover information for:
  // - Image names (show metadata, signature status)
  // - Policy fields (show documentation)
  // - .ctp bundle fields (show bundle info)

  let hoverContent = Js.Dict.fromArray([
    (
      "contents",
      Js.Json.object_(
        Js.Dict.fromArray([
          ("kind", Js.Json.string("markdown")),
          (
            "value",
            Js.Json.string(
              "**Container Image**\n\nVerified: ✓\nSignature: Ed25519\nDigest: sha256:abc123...",
            ),
          ),
        ]),
      ),
    ),
  ])

  success(id, Js.Json.object_(hoverContent))
}

// Diagnostic handler
let handleDiagnostic = (state: state, params: Js.Json.t, id: option<Js.Json.t>): jsonRpcResponse => {
  // Provide diagnostics for:
  // - Invalid Gatekeeper policies
  // - Unsigned/unverified images
  // - Malformed .ctp bundles
  // - Security issues

  let diagnostics = [
    Js.Json.object_(
      Js.Dict.fromArray([
        (
          "range",
          Js.Json.object_(
            Js.Dict.fromArray([
              (
                "start",
                Js.Json.object_(
                  Js.Dict.fromArray([
                    ("line", Js.Json.number(5.0)),
                    ("character", Js.Json.number(10.0)),
                  ]),
                ),
              ),
              (
                "end",
                Js.Json.object_(
                  Js.Dict.fromArray([
                    ("line", Js.Json.number(5.0)),
                    ("character", Js.Json.number(30.0)),
                  ]),
                ),
              ),
            ]),
          ),
        ),
        ("severity", Js.Json.number(1.0)), // Error
        ("code", Js.Json.string("unsigned-image")),
        ("source", Js.Json.string("vordr")),
        ("message", Js.Json.string("Container image is not signed or verified")),
      ]),
    ),
  ]

  let result = Js.Dict.fromArray([("items", Js.Json.array(diagnostics))])

  success(id, Js.Json.object_(result))
}

// Code Action handler
let handleCodeAction = (state: state, params: Js.Json.t, id: option<Js.Json.t>): jsonRpcResponse => {
  // Provide code actions for:
  // - Sign unsigned images
  // - Fix policy violations
  // - Update image to verified version

  let actions = [
    Js.Json.object_(
      Js.Dict.fromArray([
        ("title", Js.Json.string("Sign image with Cerro Torre")),
        ("kind", Js.Json.string("quickfix")),
        (
          "command",
          Js.Json.object_(
            Js.Dict.fromArray([
              ("title", Js.Json.string("Sign image")),
              ("command", Js.Json.string("vordr.signImage")),
              ("arguments", Js.Json.array([Js.Json.string("alpine:latest")])),
            ]),
          ),
        ),
      ]),
    ),
  ]

  success(id, Js.Json.array(actions))
}

// Main request handler
let handleRequest = (state: state, request: jsonRpcRequest): option<jsonRpcResponse> => {
  Js.Console.log2("LSP →", request.method)

  switch request.method {
  | "initialize" =>
    Some(
      handleInitialize(
        state,
        request.params->Belt.Option.getWithDefault(Js.Json.null),
        request.id,
      ),
    )
  | "initialized" => None // Notification, no response
  | "shutdown" => Some(success(request.id, Js.Json.null))
  | "exit" => None

  // Text Document Synchronization
  | "textDocument/didOpen" =>
    handleDidOpen(state, request.params->Belt.Option.getWithDefault(Js.Json.null))
  | "textDocument/didChange" =>
    handleDidChange(state, request.params->Belt.Option.getWithDefault(Js.Json.null))
  | "textDocument/didClose" =>
    handleDidClose(state, request.params->Belt.Option.getWithDefault(Js.Json.null))

  // Language Features
  | "textDocument/completion" =>
    Some(
      handleCompletion(
        state,
        request.params->Belt.Option.getWithDefault(Js.Json.null),
        request.id,
      ),
    )
  | "textDocument/hover" =>
    Some(
      handleHover(state, request.params->Belt.Option.getWithDefault(Js.Json.null), request.id),
    )
  | "textDocument/diagnostic" =>
    Some(
      handleDiagnostic(
        state,
        request.params->Belt.Option.getWithDefault(Js.Json.null),
        request.id,
      ),
    )
  | "textDocument/codeAction" =>
    Some(
      handleCodeAction(
        state,
        request.params->Belt.Option.getWithDefault(Js.Json.null),
        request.id,
      ),
    )

  | _ => Some(error(request.id, -32601, `Method not found: ${request.method}`))
  }
}
