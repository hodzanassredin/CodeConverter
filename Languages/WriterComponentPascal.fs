namespace Languages

module CPWriter =
    let writeImportList (ast: AstCP.ImportList) = 
        printf "IMPORT "
        let writei (rename, name) = 
            rename |> Option.iter (printf "%s :=")
            printf "%s" name

        AstCP.iter writei (fun ()->printf ",") ast
    let writeIdentDef(ast:AstCP.IdentDef) = ()
    let writeConstExpr(ast:AstCP.ConstExpr) = ()
    let writeConstDecl((identDef, constExpr):AstCP.ConstDecl) = 
        writeIdentDef identDef 
        printf "=" 
        writeConstExpr constExpr
    let writeTypeDecl(ast:AstCP.TypeDecl) = ()
    let writeVarDecl(ast:AstCP.VarDecl) = ()
    let writeForwardDecl(ast:AstCP.ForwardDecl) = ()
    let writeProcDecl(ast:AstCP.ProcDecl) = ()
    let writeDeclSeq1 (ast: AstCP.DeclSeq1) = 
        match ast with  
        | AstCP.DeclSeq1.CONST(cdl) -> List.iter writeConstDecl cdl
        | AstCP.DeclSeq1.TYPE(tdl)-> List.iter writeTypeDecl tdl
        | AstCP.DeclSeq1.VAR(vdl)-> List.iter writeVarDecl vdl
    let writeDeclSeq2 (ast: AstCP.DeclSeq2) = 
        match ast with  
        | AstCP.DeclSeq2.Forward(dl) -> writeForwardDecl dl
        | AstCP.DeclSeq2.Proc(dl)-> writeProcDecl dl
    let writeDeclSeq ((ds1, ds2): AstCP.DeclSeq) = 
        List.iter writeDeclSeq1 ds1
        List.iter writeDeclSeq2 ds2
    let writeStatementSeq (ast: AstCP.StatementSeq) = ()
    let writeModule (ast: AstCP.Module) =
        let (ident, importList, declSeq, beginStatementSeq, endstatementSeq) = ast
        printfn "MODULE %s;" ident
        importList |> Option.iter writeImportList
        writeDeclSeq declSeq
        beginStatementSeq |> Option.iter (fun sq -> printf "BEGIN"
                                                    writeStatementSeq sq)
        endstatementSeq |> Option.iter (fun sq -> printf "CLOSE"
                                                  writeStatementSeq sq)
        printfn "END %s." ident

