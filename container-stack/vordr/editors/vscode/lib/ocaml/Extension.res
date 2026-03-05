// SPDX-License-Identifier: PMPL-1.0-or-later
/**
 * Vörðr VSCode Extension
 * Fully ported to ReScript v12
 */

module VsCode = {
  type disposable = {"dispose": unit => unit}

  type extensionContext = {
    subscriptions: array<disposable>,
    asAbsolutePath: string => string,
    extensionPath: string,
  }

  type textDocument = {
    languageId: string,
    fileName: string,
  }

  type textEditor = {document: textDocument}

  module Window = {
    @module("vscode") @scope("window")
    external showInputBox: {"prompt": string, "placeHolder": string} => promise<option<string>> =
      "showInputBox"

    @module("vscode") @scope("window")
    external registerCommand: (string, unit => promise<unit>) => disposable = "registerCommand"

    @module("vscode") @scope("window") @val
    external activeTextEditor: option<textEditor> = "activeTextEditor"
  }

  module Workspace = {
    type configuration = {get: string => bool}

    @module("vscode") @scope("workspace")
    external getConfiguration: string => configuration = "getConfiguration"

    @module("vscode") @scope("workspace")
    external createFileSystemWatcher: string => disposable = "createFileSystemWatcher"
  }
}

module Lsp = {
  type languageClient
  type serverOptions = {
    run: {"command": string, "args": array<string>, "transport": int},
    debug: {"command": string, "args": array<string>, "transport": int},
  }
  type clientOptions = {
    documentSelector: array<{"scheme": string, "language": string}>,
    synchronize: {"fileEvents": VsCode.disposable},
  }

  @module("vscode-languageclient/node") @new
  external makeLanguageClient: (string, string, serverOptions, clientOptions) => languageClient =
    "LanguageClient"

  @send external start: languageClient => unit = "start"
  @send external stop: languageClient => promise<unit> = "stop"
  @send
  external sendRequest: (languageClient, string, {"command": string, "arguments": array<string>}) => promise<
    unit,
  > = "sendRequest"
}

module Path = {
  @module("path") external join: (string, string) => string = "join"
}

let client: ref<option<Lsp.languageClient>> = ref(None)

let signImage = async () => {
  switch (VsCode.Window.activeTextEditor, client.contents) {
  | (Some(_editor), Some(c)) => {
      let imageName = await VsCode.Window.showInputBox({
        "prompt": "Enter container image name",
        "placeHolder": "alpine:latest",
      })

      switch imageName {
      | Some(name) =>
        await Lsp.sendRequest(
          c,
          "workspace/executeCommand",
          {"command": "vordr.signImage", "arguments": [name]},
        )
      | None => ()
      }
    }
  | _ => ()
  }
}

let verifyImage = async () => {
  switch client.contents {
  | Some(c) => {
      let imageName = await VsCode.Window.showInputBox({
        "prompt": "Enter container image name",
        "placeHolder": "alpine:latest",
      })

      switch imageName {
      | Some(name) =>
        await Lsp.sendRequest(
          c,
          "workspace/executeCommand",
          {"command": "vordr.verifyImage", "arguments": [name]},
        )
      | None => ()
      }
    }
  | _ => ()
  }
}

let activate = (context: VsCode.extensionContext) => {
  let config = VsCode.Workspace.getConfiguration("vordr")

  if config.get("lsp.enable") {
    let lspPath = Path.join(context.extensionPath, "../../src/lsp/Main.res.js")

    let serverOptions: Lsp.serverOptions = {
      run: {
        "command": "deno",
        "args": ["run", "--allow-read", "--allow-write", lspPath],
        "transport": 1, // TransportKind.stdio
      },
      debug: {
        "command": "deno",
        "args": ["run", "--allow-read", "--allow-write", lspPath],
        "transport": 1,
      },
    }

    let clientOptions: Lsp.clientOptions = {
      documentSelector: [
        {"scheme": "file", "language": "ctp"},
        {"scheme": "file", "language": "gatekeeper"},
        {"scheme": "file", "language": "dockerfile"},
      ],
      synchronize: {
        "fileEvents": VsCode.Workspace.createFileSystemWatcher(
          "**/.{ctp,gatekeeper.yaml,gatekeeper.yml}",
        ),
      },
    }

    let c = Lsp.makeLanguageClient("vordrLsp", "Vörðr Language Server", serverOptions, clientOptions)

    client := Some(c)
    Lsp.start(c)

    // Register commands
    let _ = Array.push(context.subscriptions, VsCode.Window.registerCommand("vordr.signImage", signImage))
    let _ = Array.push(
      context.subscriptions,
      VsCode.Window.registerCommand("vordr.verifyImage", verifyImage),
    )
  }
}

let deactivate = () => {
  switch client.contents {
  | None => Promise.resolve()
  | Some(c) => Lsp.stop(c)
  }
}
