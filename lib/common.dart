import 'package:dart_interpreter/ast.dart';
import 'package:dart_interpreter/config.dart';
import 'package:dart_interpreter/error.dart';

final RegExp _spaceRegex = RegExp(r"\s");
final RegExp _alphaRegex = RegExp("[a-z]|[A-Z]|[_]");
final RegExp _alphaNumRegex = RegExp("[a-z]|[A-Z]|[0-9]|[_]");

void logScopeChange(Object? msg) {
  if (!InterpreterConfig.instance.logScopeChanges) return;

  print(msg);
}

void logStackChange(Object? msg) {
  if (!InterpreterConfig.instance.logStackChanges) return;

  print(msg);
}

class Token<T> {
  final TokenType<T> type;
  final T value;
  final int line;
  final int column;

  const Token(this.type, this.value, this.line, this.column);

  @override
  String toString() {
    return 'Token($type, $value, pos=$line:$column)';
  }
}

enum TokenType<T> {
  // operators
  plus<String>('+'),
  minus<String>('-'),
  mul<String>('*'),
  floatDiv<String>('/'),
  lparen<String>('('),
  rparen<String>(')'),
  dot<String>('.'),
  semi<String>(';'),
  colon<String>(':'),
  comma<String>(','),

  // keywords
  program<String>('PROGRAM'),
  integer<String>('INTEGER'),
  real<String>('REAL'),
  integerDiv<String>('DIV'),
  variable<String>('VAR'),
  procedure<String>('PROCEDURE'),
  begin<String>('BEGIN'),
  end<String>('END'),

  // misc
  eof<void>('EOF'),
  id<String>('ID'),
  assign<String>(':='),
  integerConst<int>('INTEGER_CONST'),
  realConst<double>('REAL_CONST');

  final String str;

  const TokenType(this.str);
}

class Symbol {
  final String name;
  final BuiltinTypeSymbol? type;
  int scopeLevel = 0;

  Symbol(this.name, [this.type]);
}

class BuiltinTypeSymbol extends Symbol {
  BuiltinTypeSymbol(super.name);

  @override
  String toString() => "<$runtimeType(name='$name')>";
}

class VarSymbol extends Symbol {
  VarSymbol(String name, BuiltinTypeSymbol type) : super(name, type);

  @override
  String toString() => "<$runtimeType(name='$name', type='${type!.name}')>";
}

class ProgramSymbol extends Symbol {
  ProgramSymbol(super.name);

  @override
  String toString() => "<$runtimeType(name='$name')>";
}

class ProcedureSymbol extends Symbol {
  final List<VarSymbol> params = [];
  Block? body;

  ProcedureSymbol(super.name);

  @override
  String toString() => "<$runtimeType(name='$name', parameters='$params')>";
}

class ScopedSymbolTable {
  final String scopeName;
  final int scopeLevel;
  final ScopedSymbolTable? enclosingScope;

  ScopedSymbolTable(this.scopeName, this.scopeLevel, [this.enclosingScope]);

  final Map<String, Symbol> _symbols = {};

  void defineBuiltins() {
    insert(BuiltinTypeSymbol('INTEGER'));
    insert(BuiltinTypeSymbol('REAL'));
  }

  void insert(Symbol symbol) {
    logScopeChange('Insert: ${symbol.name}');
    symbol.scopeLevel = scopeLevel;
    _symbols[symbol.name] = symbol;
  }

  T? lookup<T extends Symbol>(String name, {bool currentScopeOnly = false}) {
    logScopeChange('Lookup: $name (SCOPE $scopeName)');

    if (_symbols[name] is! T?) {
      throw Exception('The symbol $name doesn\'t match the expected type $T');
    }

    if (_symbols[name] != null) {
      return _symbols[name] as T;
    }

    if (enclosingScope != null && !currentScopeOnly) {
      return enclosingScope?.lookup(name);
    }

    return null;
  }

  @override
  String toString() {
    const int subHeadersLength = 24;

    final StringBuffer buf = StringBuffer();

    buf.writeln();
    buf.writeln('SYMBOL TABLE');
    buf.writeln('=' * subHeadersLength);
    buf.writeln('Name: $scopeName');
    buf.writeln('Level: $scopeLevel');
    buf.writeln('Enclosing scope: ${enclosingScope?.scopeName}');
    buf.writeln('Scope contents');
    buf.writeln('-' * subHeadersLength);
    buf.writeAll(_symbols.entries.map((e) => '\t${e.key}: ${e.value}'), '\n');
    buf.writeln();

    return buf.toString();
  }
}

class Lexer {
  final String text;
  int position = 0;
  int column = 1;
  int line = 1;

  Map<String, TokenType<String>> get _reservedKeywords {
    final List<TokenType<String>> keywords = TokenType.values
        .sublist(TokenType.program.index, TokenType.end.index + 1)
        .cast<TokenType<String>>();

    return {
      for (final TokenType<String> token in keywords)
        token.str.toLowerCase(): token,
    };
  }

  String? get currentChar {
    if (position > text.length - 1) return null;

    return text[position];
  }

  Lexer(this.text);

  Never error() {
    throwErr(
      LexerError("Lexer error on '$currentChar' line: $line column: $column"),
    );
  }

  Token<T> _buildToken<T>(TokenType<T> type, T value) =>
      Token<T>(type, value, line, column);

  void advance() {
    if (currentChar == null) return;

    if (currentChar == '\n') {
      line += 1;
      column = 0;
    }

    position += 1;
    column += 1;
  }

  String? peek() {
    final int peekPos = position + 1;
    if (peekPos > text.length - 1) return null;

    return text[peekPos];
  }

  void skipWhitespace() {
    while (currentChar != null && _spaceRegex.hasMatch(currentChar!)) {
      advance();
    }
  }

  void skipComment() {
    while (currentChar != "}") {
      advance();
    }

    advance();
  }

  Token<num> number() {
    String result = "";
    while (currentChar != null && int.tryParse(currentChar ?? '') != null) {
      result += currentChar!;
      advance();
    }

    if (currentChar == ".") {
      result += currentChar!;
      advance();

      while (currentChar != null && int.tryParse(currentChar ?? '') != null) {
        result += currentChar!;
        advance();
      }

      return _buildToken(TokenType.realConst, double.parse(result));
    }

    return _buildToken(TokenType.integerConst, int.parse(result));
  }

  Token<String> _id() {
    String result = '';

    while (currentChar != null && _alphaNumRegex.hasMatch(currentChar!)) {
      result += currentChar!;
      advance();
    }
    final TokenType<String>? keyword = _reservedKeywords[result.toLowerCase()];

    final Token<String> token = keyword != null
        ? Token(keyword, keyword.str, line, column)
        : _buildToken(TokenType.id, result);

    return token;
  }

  Token getNextToken() {
    while (currentChar != null) {
      if (_spaceRegex.hasMatch(currentChar!)) {
        skipWhitespace();
        continue;
      }

      if (currentChar == "{") {
        advance();
        skipComment();
        continue;
      }

      if (_alphaRegex.hasMatch(currentChar!)) {
        return _id();
      }

      if (int.tryParse(currentChar!) != null) {
        return number();
      }

      if (currentChar == ":" && peek() == "=") {
        advance();
        advance();
        return _buildToken<String>(TokenType.assign, ':=');
      }

      final TokenType<String> tokenType = TokenType.values.firstWhere(
        (e) => e.str == currentChar,
        orElse: error,
      ) as TokenType<String>;

      final Token<String> token = _buildToken(tokenType, tokenType.str);

      advance();
      return token;
    }

    return _buildToken(TokenType.eof, null);
  }
}
