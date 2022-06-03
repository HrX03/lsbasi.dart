import 'dart:io';

import 'package:args/args.dart';
import 'package:dart_interpreter/ast.dart';
import 'package:dart_interpreter/config.dart';

void main(List<String> args) async {
  final ArgParser argParser = ArgParser();
  argParser.addFlag('scope', negatable: false);
  argParser.addFlag('stack', negatable: false);
  final ArgResults results = argParser.parse(args);
  InterpreterConfig.instance.logScopeChanges = results['scope'] as bool;
  InterpreterConfig.instance.logStackChanges = results['stack'] as bool;

  final String input = await File(results.rest.first).readAsString();
  final Parser semParser = Parser(input);
  final AST ast = semParser.parse();

  final SemanticAnalyzer symtabBuilder = SemanticAnalyzer();
  final Interpreter interpreter = Interpreter();

  symtabBuilder.visit(ast);
  interpreter.interpret(ast);
}
