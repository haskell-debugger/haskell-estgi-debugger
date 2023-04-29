// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as Net from 'net';
import { randomBytes } from 'crypto';
import { tmpdir } from 'os';
import { join } from 'path';
import { platform } from 'process';
import { ProviderResult } from 'vscode';


// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "external-stg-debugger" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable = vscode.commands.registerCommand('external-stg-debugger.helloWorld', () => {
		// The code you place here will be executed every time your command is executed
		// Display a message box to the user
		vscode.window.showInformationMessage('Hello World from external-stg-debugger!');
	});
    console.log('foo');

	runDebugger (context, new MockDebugAdapterServerDescriptorFactory());

	context.subscriptions.push(disposable);
}

export function runDebugger (context: vscode.ExtensionContext, factory: MockDebugAdapterServerDescriptorFactory) {

	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('dap-extension', factory));
	console.log('made it to runDebugger');

	vscode.debug.onDidChangeBreakpoints((e) => {
		console.log(e, 'breakpoints changed hit');
	});

	vscode.debug.onDidChangeActiveDebugSession((e) => {
		console.log(e, 'active debug session hit');
	});

	vscode.debug.onDidReceiveDebugSessionCustomEvent((e) => {
		console.log(e, 'custom event received hit')
	});

}

// This method is called when your extension is deactivated
export function deactivate() {}


class MockDebugAdapterServerDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {

	public server?: vscode.DebugAdapterServer;

	createDebugAdapterDescriptor(session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {

		this.server = new vscode.DebugAdapterServer(4711, 'localhost');
		return this.server;
	}

	dispose() {
		console.log('in dispose MockAdapterDescriptorFactory');
	}
}
