import 'package:dart_interpreter/common.dart';
import 'package:dart_interpreter/error.dart';

class AST {
  const AST();
}

class Program extends AST {
  final String name;
  final Block block;

  const Program(this.name, this.block);
}

class Block extends AST {
  final List<Decl> declarations;
  final Compound compound;

  const Block(this.declarations, this.compound);
}

class Decl extends AST {
  const Decl();
}

class ProcedureDecl extends Decl {
  final String name;
  final List<Param> params;
  final Block block;

  const ProcedureDecl(this.name, this.params, this.block);
}

class VarDecl extends Decl {
  final Var varNode;
  final VarType typeNode;

  const VarDecl(this.varNode, this.typeNode);
}

class ProcedureCall extends AST {
  final String procName;
  final List<AST> params;
  final Token token;
  late ProcedureSymbol procSymbol;

  ProcedureCall(this.procName, this.params, this.token);
}

class VarType extends AST {
  final Token<String> token;
  String get value => token.value;

  const VarType(this.token);
}

class Param extends AST {
  final Var varNode;
  final VarType typeNode;

  const Param(this.varNode, this.typeNode);
}

class Compound extends AST {
  final List<AST> children = [];

  Compound();
}

class Assign extends AST {
  final Var left;
  final Token<String> op;
  final AST right;

  const Assign(this.left, this.op, this.right);
}

class UnaryOp extends AST {
  final Token<String> op;
  final AST expr;

  const UnaryOp(this.op, this.expr);
}

class BinOp extends AST {
  final AST left;
  final Token<String> op;
  final AST right;

  const BinOp(this.left, this.op, this.right);
}

class Num extends AST {
  final Token<num> token;
  num get value => token.value;

  const Num(this.token);
}

class Var extends AST {
  final Token<String> token;
  String get value => token.value;

  const Var(this.token);
}

class NoOp extends AST {
  const NoOp();
}

class Parser {
  final Lexer lexer;
  late Token currentToken;

  Parser(String text) : lexer = Lexer(text) {
    currentToken = lexer.getNextToken();
  }

  Parser.raw(this.lexer) {
    currentToken = lexer.getNextToken();
  }

  Never error(ErrorCode code, Token token) {
    throwErr(ParserError(code, token));
  }

  void eat(TokenType type) {
    if (currentToken.type == type) {
      currentToken = lexer.getNextToken();
    } else {
      error(ErrorCode.unexpectedToken, currentToken);
    }
  }

  AST program() {
    eat(TokenType.program);
    final Var varNode = variable();
    final String progName = varNode.value;
    eat(TokenType.semi);

    final AST programNode = Program(progName, block());
    eat(TokenType.dot);
    return programNode;
  }

  Block block() {
    final List<Decl> declarationNodes = declarations();
    final Compound compoundStatements = compoundStatement();

    return Block(declarationNodes, compoundStatements);
  }

  List<Decl> declarations() {
    final List<Decl> declarations = [];

    while (true) {
      if (currentToken.type == TokenType.variable) {
        eat(TokenType.variable);

        while (currentToken.type == TokenType.id) {
          declarations.addAll(variableDeclaration());
          eat(TokenType.semi);
        }
      } else if (currentToken.type == TokenType.procedure) {
        declarations.add(procedureDeclaration());
      } else {
        break;
      }
    }

    return declarations;
  }

  ProcedureDecl procedureDeclaration() {
    eat(TokenType.procedure);
    final String name = currentToken.value;
    eat(TokenType.id);

    final List<Param> parameters;
    if (currentToken.type == TokenType.lparen) {
      eat(TokenType.lparen);
      parameters = formalParameterList();
      eat(TokenType.rparen);
    } else {
      parameters = [];
    }
    eat(TokenType.semi);

    final ProcedureDecl procDecl = ProcedureDecl(name, parameters, block());
    eat(TokenType.semi);
    return procDecl;
  }

  List<Param> formalParameterList() {
    if (currentToken.type != TokenType.id) return [];

    final List<Param> params = [...formalParameters()];

    while (currentToken.type == TokenType.semi) {
      eat(TokenType.semi);
      params.addAll(formalParameters());
    }

    return params;
  }

  List<Param> formalParameters() {
    final Token initialVar = currentToken;
    eat(TokenType.id);

    final List<Var> varNodes = [Var(initialVar as Token<String>)];

    while (currentToken.type == TokenType.comma) {
      eat(TokenType.comma);
      final Token currentVar = currentToken;
      eat(TokenType.id);
      varNodes.add(Var(currentVar as Token<String>));
    }

    eat(TokenType.colon);

    final VarType typeNode = typeSpec();

    return varNodes.map((e) => Param(e, typeNode)).toList();
  }

  List<VarDecl> variableDeclaration() {
    final Token initialVar = currentToken;
    eat(TokenType.id);

    final List<Var> varNodes = [Var(initialVar as Token<String>)];

    while (currentToken.type == TokenType.comma) {
      eat(TokenType.comma);
      final Token currentVar = currentToken;
      eat(TokenType.id);
      varNodes.add(Var(currentVar as Token<String>));
    }

    eat(TokenType.colon);

    final VarType typeNode = typeSpec();

    return varNodes.map((e) => VarDecl(e, typeNode)).toList();
  }

  VarType typeSpec() {
    final Token token = currentToken;

    switch (token.type) {
      case TokenType.integer:
        eat(TokenType.integer);
        break;
      case TokenType.real:
        eat(TokenType.real);
        break;
      default:
        error(
          ErrorCode.unexpectedToken,
          currentToken,
        );
    }

    return VarType(token as Token<String>);
  }

  Compound compoundStatement() {
    eat(TokenType.begin);
    final List<AST> nodes = statementList();
    eat(TokenType.end);

    return Compound()..children.addAll(nodes);
  }

  List<AST> statementList() {
    final AST node = statement();
    final List<AST> results = [node];

    while (currentToken.type == TokenType.semi) {
      eat(TokenType.semi);
      results.add(statement());
    }

    if (currentToken.type == TokenType.id) {
      error(ErrorCode.unexpectedToken, currentToken);
    }

    return results;
  }

  AST assignmentStatement() {
    final Var left = variable();
    final Token token = currentToken;
    eat(TokenType.assign);
    final AST right = expr();

    return Assign(left, token as Token<String>, right);
  }

  AST proccallStatement() {
    final Token token = currentToken;

    eat(TokenType.id);
    final String procName = token.value as String;

    eat(TokenType.lparen);

    final List<AST> params = [];

    if (currentToken.type != TokenType.rparen) {
      params.add(expr());
    }

    while (currentToken.type == TokenType.comma) {
      eat(TokenType.comma);
      params.add(expr());
    }

    eat(TokenType.rparen);

    return ProcedureCall(procName, params, token);
  }

  AST statement() {
    switch (currentToken.type) {
      case TokenType.begin:
        return compoundStatement();
      case TokenType.id:
        if (lexer.currentChar == "(") {
          return proccallStatement();
        }

        return assignmentStatement();
      default:
        return empty();
    }
  }

  Var variable() {
    final Token token = currentToken;
    eat(TokenType.id);
    return Var(token as Token<String>);
  }

  NoOp empty() => const NoOp();

  AST factor() {
    final Token token = currentToken;

    switch (currentToken.type) {
      case TokenType.plus:
        eat(TokenType.plus);
        return UnaryOp(token as Token<String>, factor());
      case TokenType.minus:
        eat(TokenType.minus);
        return UnaryOp(token as Token<String>, factor());
      case TokenType.integerConst:
        eat(TokenType.integerConst);
        return Num(token as Token<num>);
      case TokenType.realConst:
        eat(TokenType.realConst);
        return Num(token as Token<num>);
      case TokenType.lparen:
        eat(TokenType.lparen);
        final AST node = expr();
        eat(TokenType.rparen);
        return node;
      default:
        return variable();
    }
  }

  AST term() {
    AST node = factor();

    while ([TokenType.mul, TokenType.integerDiv, TokenType.floatDiv]
        .contains(currentToken.type)) {
      final Token<String> token = currentToken as Token<String>;

      if (token.type == TokenType.mul) {
        eat(TokenType.mul);
      } else if (token.type == TokenType.integerDiv) {
        eat(TokenType.integerDiv);
      } else if (token.type == TokenType.floatDiv) {
        eat(TokenType.floatDiv);
      }

      node = BinOp(node, token, factor());
    }

    return node;
  }

  AST expr() {
    AST node = term();

    while ([TokenType.plus, TokenType.minus].contains(currentToken.type)) {
      final Token<String> token = currentToken as Token<String>;

      if (token.type == TokenType.plus) {
        eat(TokenType.plus);
      } else if (token.type == TokenType.minus) {
        eat(TokenType.minus);
      }

      node = BinOp(node, token, term());
    }

    return node;
  }

  AST parse() {
    final AST node = program();

    if (currentToken.type != TokenType.eof) {
      error(ErrorCode.unexpectedToken, currentToken);
    }

    return node;
  }
}

typedef Visitor<R, T extends AST> = R Function(T node);

abstract class NodeVisitor<R> {
  final Map<Type, VisitorWrapper> _visitors = {};

  NodeVisitor() {
    register();
  }

  void register();

  R visit(AST node) {
    VisitorWrapper? nodeVisitor = _visitors[node.runtimeType];
    if (nodeVisitor == null) genericVisit(node);

    return nodeVisitor(node);
  }

  void registerVisitor<T extends AST>(Visitor<R, T> visitor) {
    _visitors[T] = VisitorWrapper<R, T>(visitor);
  }

  Never genericVisit(AST node) {
    throw Exception('No visitor found for node ${node.runtimeType}');
  }
}

mixin NodeVisitorMixin<R> on NodeVisitor<R> {
  @override
  void register() {
    registerVisitor<Program>(visitProgram);
    registerVisitor<Block>(visitBlock);
    registerVisitor<VarDecl>(visitVarDecl);
    registerVisitor<VarType>(visitVarType);
    registerVisitor<Compound>(visitCompound);
    registerVisitor<NoOp>(visitNoOp);
    registerVisitor<Assign>(visitAssign);
    registerVisitor<Var>(visitVar);
    registerVisitor<BinOp>(visitBinOp);
    registerVisitor<UnaryOp>(visitUnaryOp);
    registerVisitor<Num>(visitNum);
    registerVisitor<ProcedureDecl>(visitProcedureDecl);
    registerVisitor<ProcedureCall>(visitProcedureCall);
  }

  R visitProgram(Program node);
  R visitBlock(Block node);
  R visitVarDecl(VarDecl node);
  R visitVarType(VarType node);
  R visitCompound(Compound node);
  R visitNoOp(NoOp node);
  R visitAssign(Assign node);
  R visitVar(Var node);
  R visitBinOp(BinOp node);
  R visitUnaryOp(UnaryOp node);
  R visitNum(Num node);
  R visitProcedureDecl(ProcedureDecl node);
  R visitProcedureCall(ProcedureCall node);
}

class VisitorWrapper<R, T extends AST> {
  final Visitor<R, T> visitor;

  const VisitorWrapper(this.visitor);

  R call(T node) => visitor(node);
}

class Interpreter extends NodeVisitor with NodeVisitorMixin {
  final CallStack callStack = CallStack();

  Interpreter();

  @override
  void visitProgram(Program node) {
    callStack.push(ActivationRecord(node.name, ARType.program, 1));

    logStackChange('ENTER: PROGRAM ${node.name}');
    logStackChange(callStack);

    visit(node.block);

    logStackChange('LEAVE: PROGRAM ${node.name}');
    logStackChange(callStack);

    callStack.pop();
  }

  @override
  void visitBlock(Block node) {
    for (final Decl declaration in node.declarations) {
      visit(declaration);
    }

    visit(node.compound);
  }

  @override
  void visitVarDecl(VarDecl node) {}

  @override
  void visitVarType(VarType node) {}

  @override
  void visitCompound(Compound node) {
    for (final AST child in node.children) {
      visit(child);
    }
  }

  @override
  void visitNoOp(NoOp node) {}

  @override
  void visitAssign(Assign node) {
    final ActivationRecord record = callStack.peek();
    record[node.left.value] = visit(node.right);
  }

  @override
  num visitVar(Var node) {
    final ActivationRecord record = callStack.peek();
    final num? val = record[node.value] as num?;

    if (val == null) {
      throw Exception("The variable '${node.value}' was not found");
    }

    return val;
  }

  @override
  num visitBinOp(BinOp node) {
    switch (node.op.type) {
      case TokenType.plus:
        return visit(node.left) + visit(node.right);
      case TokenType.minus:
        return visit(node.left) - visit(node.right);
      case TokenType.mul:
        return visit(node.left) * visit(node.right);
      case TokenType.integerDiv:
        return visit(node.left) ~/ visit(node.right);
      case TokenType.floatDiv:
        return visit(node.left) / visit(node.right);
      default:
        throw Exception("Impossible");
    }
  }

  @override
  int visitUnaryOp(UnaryOp node) {
    switch (node.op.type) {
      case TokenType.plus:
        return visit(node.expr);
      case TokenType.minus:
        return -visit(node.expr);
      default:
        throw Exception("Impossible");
    }
  }

  @override
  num visitNum(Num node) {
    return node.value;
  }

  @override
  void visitProcedureDecl(ProcedureDecl node) {}

  @override
  void visitProcedureCall(ProcedureCall node) {
    final ActivationRecord record = ActivationRecord(
      node.procName,
      ARType.procedure,
      node.procSymbol.scopeLevel + 1,
    );

    final Map<AST, VarSymbol> builtParams =
        Map.fromIterables(node.params, node.procSymbol.params);

    builtParams.forEach((actualParam, formalParam) {
      record[formalParam.name] = visit(actualParam);
    });

    callStack.push(record);

    logStackChange('ENTER: PROCEDURE ${node.procName}');
    logStackChange(callStack);

    if (node.procSymbol.body != null) visit(node.procSymbol.body!);

    logStackChange('LEAVE: PROCEDURE ${node.procName}');
    logStackChange(callStack);

    callStack.pop();
  }

  void interpret(AST node) => visit(node);
}

class SemanticAnalyzer extends NodeVisitor<void> with NodeVisitorMixin<void> {
  ScopedSymbolTable currentScope = ScopedSymbolTable('builtin', 0)
    ..defineBuiltins();

  Never error(ErrorCode code, Token token) {
    throwErr(SemanticError(code, token));
  }

  @override
  void visitBlock(Block node) {
    for (final Decl declaration in node.declarations) {
      visit(declaration);
    }

    visit(node.compound);
  }

  @override
  void visitProgram(Program node) {
    currentScope.insert(ProgramSymbol(node.name));
    logScopeChange('ENTER SCOPE global');
    final ScopedSymbolTable globalScope =
        currentScope = ScopedSymbolTable('global', 1, currentScope);

    visit(node.block);

    logScopeChange(globalScope);
    currentScope = currentScope.enclosingScope!;
    logScopeChange('LEAVE SCOPE global');

    logScopeChange(currentScope);
  }

  @override
  void visitBinOp(BinOp node) {
    visit(node.left);
    visit(node.right);
  }

  @override
  void visitNum(Num node) {}

  @override
  void visitNoOp(NoOp node) {}

  @override
  void visitUnaryOp(UnaryOp node) {
    visit(node.expr);
  }

  @override
  void visitCompound(Compound node) {
    node.children.forEach(visit);
  }

  @override
  void visitVarDecl(VarDecl node) {
    final BuiltinTypeSymbol? typeSymbol =
        currentScope.lookup(node.typeNode.value);
    if (typeSymbol == null) {
      throw Exception('Type ${node.typeNode.value} not found');
    }

    final VarSymbol varSymbol = VarSymbol(node.varNode.value, typeSymbol);

    if (currentScope.lookup(node.varNode.value, currentScopeOnly: true) !=
        null) {
      error(ErrorCode.duplicateId, node.varNode.token);
    }

    currentScope.insert(varSymbol);
  }

  @override
  void visitAssign(Assign node) {
    final VarSymbol? varSymbol = currentScope.lookup(node.left.value);

    if (varSymbol == null) {
      error(ErrorCode.idNotFound, node.left.token);
    }

    visit(node.right);
  }

  @override
  void visitVar(Var node) {
    final VarSymbol? varSymbol = currentScope.lookup(node.value);

    if (varSymbol == null) {
      error(ErrorCode.idNotFound, node.token);
    }
  }

  @override
  void visitVarType(VarType node) {}

  @override
  void visitProcedureDecl(ProcedureDecl node) {
    final ProcedureSymbol procSym = ProcedureSymbol(node.name);
    currentScope.insert(procSym);

    logScopeChange('ENTER SCOPE ${node.name}');
    final ScopedSymbolTable procScope = ScopedSymbolTable(
      node.name,
      currentScope.scopeLevel + 1,
      currentScope,
    );
    currentScope = procScope;

    for (final Param param in node.params) {
      final BuiltinTypeSymbol paramType =
          currentScope.lookup(param.typeNode.value)!;
      final String paramName = param.varNode.value;
      final VarSymbol varSymbol = VarSymbol(paramName, paramType);
      currentScope.insert(varSymbol);
      procSym.params.add(varSymbol);
    }

    visit(node.block);

    logScopeChange(procScope);
    currentScope = currentScope.enclosingScope!;
    logScopeChange('LEAVE SCOPE ${node.name}');

    procSym.body = node.block;
  }

  @override
  void visitProcedureCall(ProcedureCall node) {
    final ProcedureSymbol? proc =
        currentScope.lookup<ProcedureSymbol>(node.token.value);

    if (proc == null) {
      error(ErrorCode.procNotFound, node.token);
    }

    if (proc.params.length != node.params.length) {
      error(ErrorCode.wrongParamsNum, node.token);
    }

    for (final AST node in node.params) {
      visit(node);
    }

    node.procSymbol = proc;
  }
}

class CallStack {
  final List<ActivationRecord> _records = [];

  void push(ActivationRecord record) {
    _records.add(record);
  }

  ActivationRecord pop() => _records.removeLast();

  ActivationRecord peek() => _records.last;

  @override
  String toString() {
    return 'CALL STACK\n${_records.reversed.map((e) => e.toString()).join('\n')}';
  }
}

class ActivationRecord {
  final String name;
  final ARType type;
  final int nestingLevel;
  final Map<String, Object> members = {};

  ActivationRecord(this.name, this.type, this.nestingLevel);

  void operator []=(String name, Object value) => members[name] = value;
  Object? operator [](String name) => members[name];

  @override
  String toString() {
    final StringBuffer buffer = StringBuffer();

    buffer.writeln('$nestingLevel: ${type.name.toUpperCase()} $name');

    members.forEach((name, value) {
      buffer.writeln('    $name: $value');
    });

    return buffer.toString();
  }
}

enum ARType {
  program,
  procedure,
}
