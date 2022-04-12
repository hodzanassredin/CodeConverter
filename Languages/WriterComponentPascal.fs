namespace Languages

module CPWriter =
    let writeImportList (ast: AstCP.ImportList) = 
        printf "IMPORT "
        let writei (rename, name) = 
            rename |> Option.iter (printf "%s :=")
            printf "%s" name

        AstCP.iter writei (fun ()->printf ",") ast
    let writeIdent(ast:AstCP.ident) = 
        printf "%s" ast
    let writeIdentList(ast:AstCP.IdentList) = ()
    let writeIdentDef(ast:AstCP.IdentDef) = ()
    let writeConstExpr(ast:AstCP.ConstExpr) = ()
    let writeType(ast:AstCP.Type) = ()
    let writeConstDecl((identDef, constExpr):AstCP.ConstDecl) = 
        writeIdentDef identDef 
        printf "=" 
        writeConstExpr constExpr
    let writeTypeDecl((identDef, tp):AstCP.TypeDecl) = 
        writeIdentDef identDef 
        printf "=" 
        writeType tp
    let writeVarDecl((identList, tp):AstCP.VarDecl) = 
        writeIdentList identList 
        printf ":" 
        writeType tp
    let writeForwardDecl(ast:AstCP.ForwardDecl) = ()
    let writeReciever(ast:AstCP.Receiver) = ()
    let writeFormalPars(ast:AstCP.FormalPars) = ()
    let writeMethAttributes(ast:AstCP.MethAttributes) = ()
    let writeStatementSeq (ast: AstCP.StatementSeq) = ()
    let rec writeDeclSeq1 (ast: AstCP.DeclSeq1) = 
        match ast with  
        | AstCP.DeclSeq1.CONST(cdl) -> List.iter writeConstDecl cdl
        | AstCP.DeclSeq1.TYPE(tdl)-> List.iter writeTypeDecl tdl
        | AstCP.DeclSeq1.VAR(vdl)-> List.iter writeVarDecl vdl
    and writeDeclSeq2 (ast: AstCP.DeclSeq2) = 
        match ast with  
        | AstCP.DeclSeq2.Forward(dl) -> writeForwardDecl dl
        | AstCP.DeclSeq2.Proc(dl)-> writeProcDecl dl
    and writeDeclSeq ((ds1, ds2): AstCP.DeclSeq) = 
        List.iter writeDeclSeq1 ds1
        List.iter writeDeclSeq2 ds2
    and writeProcDecl((receiver, identDef, formalPars, methAttributes, body):AstCP.ProcDecl) = 
        printf "PROCEDURE "
        Option.iter writeReciever receiver
        writeIdentDef identDef
        Option.iter writeFormalPars formalPars
        writeMethAttributes methAttributes
        Option.iter (fun (declSeq, statementSeq, ident)-> printf ";"
                                                          writeDeclSeq declSeq
                                                          Option.iter (fun x-> printf " BEGIN ";writeStatementSeq x) statementSeq
                                                          writeIdent ident) body
    let writeModule ((ident, importList, declSeq, beginStatementSeq, endstatementSeq): AstCP.Module) =
        printfn "MODULE %s;" ident
        importList |> Option.iter writeImportList
        writeDeclSeq declSeq
        beginStatementSeq |> Option.iter (fun sq -> printf "BEGIN"
                                                    writeStatementSeq sq)
        endstatementSeq |> Option.iter (fun sq -> printf "CLOSE"
                                                  writeStatementSeq sq)
        printfn "END %s." ident

