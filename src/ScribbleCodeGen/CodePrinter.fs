namespace ScribbleCodeGen

open System.IO
open System.CodeDom.Compiler

open CodeGen

module CodePrinter =

    let moduleName = ref "ScribbleGenerated"
    let fileName = ref "ScribbleGenerated.fs"

    let indent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent + 1

    let unindent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent - 1

    let writeln (writer: IndentedTextWriter) (str: string) =
        writer.Write(str)
        writer.WriteLine()

    let writeObject (writer: IndentedTextWriter) isFirst name obj =
        if isFirst then
            writeln writer (sprintf "type %s() = class" name)
        else
            writeln writer (sprintf "and %s() = class" name)
        indent writer
        List.iter (writeln writer) obj.members
        List.iter (writeln writer) obj.methods
        writeln writer "end"
        unindent writer

    let writeUnion (writer: IndentedTextWriter) isFirst name union =
        if isFirst then
            writeln writer (sprintf "type %s =" name)
        else
            writeln writer (sprintf "and %s =" name)
        indent writer
        List.iter (fun unioncase -> writeln writer (sprintf "| %s" unioncase)) union
        unindent writer

    let writeTypeDef (writer: IndentedTextWriter) isFirst (name, typeDef) =
        match typeDef with
        | Union u -> writeUnion writer isFirst name u
        | Object o -> writeObject writer isFirst name o

    let writeContents (writer: IndentedTextWriter) (content: Content) =
        let content = Map.toList content
        match content with
            | [] -> ()
            | first :: rest ->
                writeTypeDef writer true first
                List.iter (writeTypeDef writer false) rest

    let generateCode (cfsm : CFSM) protocol localRole eventStyleApi =
        use fileWriter = new StreamWriter(!fileName)
        use writer = new IndentedTextWriter(fileWriter)
        writeln writer (sprintf "module %s%s%s" !moduleName  protocol localRole)
        writeln writer ("(* This file is GENERATED, do not modify manually *)")
        let content = generateCodeContent cfsm eventStyleApi
        if not eventStyleApi
        then
            let init, _ = cfsm
            writeContents writer content
            writeln writer (sprintf "let init = %s" (mkStateName init))
        else
            ()
        writer.Flush()
        ()
