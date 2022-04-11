namespace Languages

module CPWriter =
    let toStr (moduleAst: AstCP.Module) =
        let (ident, importList, declSeq, statementSeq, statementSeq, ident) = moduleAst
        return "MODULE ident ";" [ImportList] DeclSeq
        		[BEGIN StatementSeq]
        		[CLOSE StatementSeq] END ident "."."

