namespace Languages

module CPWriter =

    let p = printf "%s"
    let inline (++) (f: 'a -> unit) (f2: 'b -> unit) (v,v2) : unit = f v; f2 v2
    let inline (+++) (f: 'a * 'b -> unit) (f2: 'c -> unit) (v1, v2, v3) : unit = f (v1,v2); f2 v3
    let inline (++++) (f: 'a * 'b* 'c -> unit) (f2: 'd -> unit) (v1, v2, v3, v4) : unit = f (v1,v2,v3); f2 v4
    let inline (+<) (f: 'a -> unit) (s: string) v : unit = f v; p s
    let inline (+>) (s: string) f2 v : unit = p s; f2 v
    let optional f = Option.iter f
    let inList f = List.iter f

    let inOneOrManyList joinStr f = AstCP.iter f (fun () -> printf joinStr) 
    let writeIdent(ast:AstCP.ident) = printf "%s" ast

    let writeImportList (ast: AstCP.ImportList) : unit = inOneOrManyList "," ("IMPORT " +> (optional (writeIdent +< ":=")) ++ writeIdent) ast
    let rec writeIdentList(ast:AstCP.IdentList) = ()
    and writeIdentDef(ast:AstCP.IdentDef) = ()
    and writeConstExpr(ast:AstCP.ConstExpr) = ()
    and writeQualident(ast:AstCP.Qualident) = ()
    and writeFieldList(ast:AstCP.FieldList) = optional (writeIdentList +< ":" ++ writeType)
    and writeType(ast:AstCP.Type) = 
        match ast with 
        | AstCP.Type.Simple(qualident) -> writeQualident qualident
        | AstCP.Type.ARRAY(constExprOneOrManyOption, tp) -> ("ARRAY" +> (optional (inOneOrManyList "," writeConstExpr)) ++ ("OF" +> writeType)) (constExprOneOrManyOption, tp)
        | AstCP.Type.RECORD(recordPrefixOption, qualidentOption, fieldListOneOrMany) -> 
            let writePrefix prefix =
                match prefix with  
                 | AstCP.RecordPrefix.ABSTRACT -> p "ABSTRACT"
                 | AstCP.RecordPrefix.EXTENSIBLE -> p "EXTENSIBLE"
                 | AstCP.RecordPrefix.LIMITED -> p "LIMITED"
            (optional writePrefix +< "RECORD" ++ (optional ("(" +> writeQualident +< ")")) +++ (inOneOrManyList ";" writeFieldList +< "END")) (recordPrefixOption, qualidentOption, fieldListOneOrMany)
        | AstCP.Type.POINTER(tp) -> ("POINTER TO" +> writeType) tp
        | AstCP.Type.PROCEDURE(formalPars) -> ("PROCEDURE" +> optional writeFormalPars) formalPars
    and writeConstDecl(ast:AstCP.ConstDecl) = ((writeIdentDef +< "=") ++ writeConstExpr) ast
    and writeTypeDecl(ast:AstCP.TypeDecl) = ((writeIdentDef +< "=") ++ writeType) ast
    and writeVarDecl(ast:AstCP.VarDecl) = ((writeIdentList +< ":") ++ writeType) ast
    and writeReciever(ast:AstCP.Receiver) : unit= 
        let writePrefix prefix =
            match prefix with  
            | AstCP.RecieverPrefix.IN -> p "IN"
            | AstCP.RecieverPrefix.VAR -> p "VAR"
        ("(" +> (optional writePrefix) ++ (writeIdent +< ":") +++ (writeIdent +< ")")) ast
    and writeFPSection(ast:AstCP.FPSection) : unit= 
        let writePrefix prefix =
            match prefix with  
            | AstCP.FPSectionPrefix.IN -> p "IN"
            | AstCP.FPSectionPrefix.OUT -> p "OUT"
            | AstCP.FPSectionPrefix.VAR -> p "VAR"
        (optional writePrefix ++ (inOneOrManyList "," writeIdent) +++ (":" +>writeType)) ast
    and writeFormalPars(ast:AstCP.FormalPars) : unit = 
        ("(" +> (optional (inOneOrManyList ";" writeFPSection)) +< ")" ++ optional (":" +> writeType)) ast
    and writeMethAttributes(ast:AstCP.MethAttributes) : unit= 
        let writeNew (n:AstCP.ProcNew) : unit =  p ", NEW"
        let writeKind (k:AstCP.ProcKind) : unit=
            match k with  
            | AstCP.ProcKind.Abstract -> p "ABSTRACT"
            | AstCP.ProcKind.Empty -> p "EMPTY"
            | AstCP.ProcKind.Extensible -> p "EXTENSIBLE"
        (optional writeNew ++ optional writeKind) ast
    and writeForwardDecl(ast:AstCP.ForwardDecl) : unit = 
        (("PROCEDURE" + " ^ ") +> (optional writeReciever) ++ writeIdentDef +++ (optional writeFormalPars) ++++ writeMethAttributes) ast
    and writeStatement(ast:AstCP.Statement) : unit = ()
    and writeStatementSeq (ast: AstCP.StatementSeq) : unit = (inOneOrManyList ";" writeStatement) ast
    and writeDeclSeq1 (ast: AstCP.DeclSeq1) = 
        match ast with  
        | AstCP.DeclSeq1.CONST(cdl) -> inList (writeConstDecl +< ";") cdl
        | AstCP.DeclSeq1.TYPE(tdl)-> inList (writeTypeDecl +< ";") tdl
        | AstCP.DeclSeq1.VAR(vdl)-> inList (writeVarDecl +< ";") vdl
    and writeDeclSeq2 (ast: AstCP.DeclSeq2) = 
        match ast with  
        | AstCP.DeclSeq2.Forward(dl) -> writeForwardDecl dl
        | AstCP.DeclSeq2.Proc(dl)-> writeProcDecl dl
    and writeDeclSeq (ast: AstCP.DeclSeq) : unit= (inList writeDeclSeq1 ++ inList writeDeclSeq2) ast
    and writeProcDecl((receiver, identDef, formalPars, methAttributes, body):AstCP.ProcDecl) : unit= 
        ("PROCEDURE " +> (optional writeReciever)) receiver
        writeIdentDef identDef
        optional writeFormalPars formalPars
        writeMethAttributes methAttributes
        let f = (";" +> writeDeclSeq) ++ optional (" BEGIN " +> writeStatementSeq)
        optional (f +++ (" END " +> writeIdent)) body
    and writeModule ((ident, importList, declSeq, beginStatementSeq, endstatementSeq): AstCP.Module) =
        ("MODULE " +> writeIdent +< ";") ident
        optional writeImportList importList
        writeDeclSeq declSeq
        optional ("BEGIN" +> writeStatementSeq) beginStatementSeq
        optional ("CLOSE" +> writeStatementSeq) endstatementSeq
        ("END " +> writeIdent) ident

