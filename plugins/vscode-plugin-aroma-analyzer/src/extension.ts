// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { homedir } from 'os';
import * as path from 'path'
import { cwd } from 'process';
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';
import { workerData } from 'worker_threads';

let client: LanguageClient;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	let serverOptions: ServerOptions = {
		command: "cargo",
		options: {
			cwd: path.join(homedir(), "Workspace", "aroma")
		},
		args: ["run", "--bin=aroma-analyzer", "--", "-vv", "--no-ansi"], transport: {
			kind: TransportKind.socket,
			port: 9473
		}
	};
	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'aroma' }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.aroma")
		}

	};

	client = new LanguageClient(
		'aroma-ls',
		'aroma-ls',
		serverOptions,
		clientOptions
	);

	client.start();

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "aroma-analyzer" is now active!');
}

// This method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
