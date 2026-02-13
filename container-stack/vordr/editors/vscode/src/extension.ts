// SPDX-License-Identifier: PMPL-1.0-or-later
// VS Code extension for Vörðr LSP

import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // Get configuration
  const config = workspace.getConfiguration('vordr');

  if (!config.get('lsp.enable')) {
    return;
  }

  // LSP server command (use Deno to run ReScript-compiled LSP)
  const serverCommand = 'deno';
  const serverArgs = [
    'run',
    '--allow-read',
    '--allow-write',
    path.join(context.extensionPath, '../../src/lsp/Main.res.js'),
  ];

  // Server options
  const serverOptions: ServerOptions = {
    run: { command: serverCommand, args: serverArgs, transport: TransportKind.stdio },
    debug: { command: serverCommand, args: serverArgs, transport: TransportKind.stdio },
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    // Register for .ctp, Gatekeeper policies, Dockerfiles
    documentSelector: [
      { scheme: 'file', language: 'ctp' },
      { scheme: 'file', language: 'gatekeeper' },
      { scheme: 'file', language: 'dockerfile' },
      { scheme: 'file', pattern: '**/*.ctp' },
      { scheme: 'file', pattern: '**/*.gatekeeper.yaml' },
      { scheme: 'file', pattern: '**/*.gatekeeper.yml' },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/.{ctp,gatekeeper.yaml,gatekeeper.yml}'),
    },
  };

  // Create and start language client
  client = new LanguageClient(
    'vordrLsp',
    'Vörðr Language Server',
    serverOptions,
    clientOptions
  );

  // Start the client (which launches the server)
  client.start();

  // Register commands
  context.subscriptions.push(
    window.registerCommand('vordr.signImage', async () => {
      const editor = window.activeTextEditor;
      if (!editor) return;

      const document = editor.document;
      const imageName = await window.showInputBox({
        prompt: 'Enter container image name',
        placeHolder: 'alpine:latest',
      });

      if (imageName) {
        await client.sendRequest('workspace/executeCommand', {
          command: 'vordr.signImage',
          arguments: [imageName],
        });
      }
    })
  );

  context.subscriptions.push(
    window.registerCommand('vordr.verifyImage', async () => {
      const imageName = await window.showInputBox({
        prompt: 'Enter container image name',
        placeHolder: 'alpine:latest',
      });

      if (imageName) {
        await client.sendRequest('workspace/executeCommand', {
          command: 'vordr.verifyImage',
          arguments: [imageName],
        });
      }
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
