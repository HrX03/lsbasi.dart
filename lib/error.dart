import 'package:dart_interpreter/common.dart';

Never throwErr(Object error) =>
    Error.throwWithStackTrace(error, StackTrace.empty);

enum ErrorCode {
  unexpectedToken('Unexpected token'),
  idNotFound('Identifier not found'),
  procNotFound('Procedure not found'),
  duplicateId('Duplicate identifier found'),
  wrongParamsNum('Wrong number of arguments');

  final String msg;

  const ErrorCode(this.msg);
}

class BaseError implements Exception {
  final String message;

  const BaseError(this.message);

  @override
  String toString() => '$runtimeType: $message';
}

class LexerError extends BaseError {
  const LexerError(super.message);
}

class ParserError extends BaseError {
  final ErrorCode errorCode;
  final Token token;

  @override
  String get message => '$token -> ${errorCode.msg}';

  const ParserError(this.errorCode, this.token) : super('');
}

class SemanticError extends BaseError {
  final ErrorCode errorCode;
  final Token token;

  @override
  String get message => '$token -> ${errorCode.msg}';

  const SemanticError(this.errorCode, this.token) : super('');
}
