class InterpreterConfig {
  InterpreterConfig._();

  static final InterpreterConfig instance = InterpreterConfig._();

  bool logScopeChanges = false;
  bool logStackChanges = false;
}
