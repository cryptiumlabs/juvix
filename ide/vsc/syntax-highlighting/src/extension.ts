
import * as vscode from 'vscode';

const MATCH_NOTHING_RE = /$^/;

export function activate(context: vscode.ExtensionContext) {

    vscode.languages.setLanguageConfiguration('juvix', {
        onEnterRules: [
            {
                beforeText: vscode.workspace.getConfiguration('juvix').indentationRules.enabled
                ? /(\bif\b(?!')(.(?!then))*|\b(then|else|m?do|of|let|in|where)\b(?!')|=|->|>>=|>=>|=<<|(^(data)( |\t)+(\w|')+( |\t)*))( |\t)*$/
                : MATCH_NOTHING_RE,
                action: { indentAction: vscode.IndentAction.Indent }
            }
        ],
        wordPattern: /([\w'_][\w'_\d]*)|([0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)/
    })
}
