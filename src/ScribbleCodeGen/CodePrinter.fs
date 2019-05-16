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

    let writeTypeDefPreamble (writer: IndentedTextWriter) isFirst name content =
        let preamble =
            if isFirst then "type" else "and"
        writeln writer (sprintf "%s %s%s" preamble name content)

    let writeObject (writer: IndentedTextWriter) isFirst name obj =
        writeTypeDefPreamble writer isFirst name "() = class"
        indent writer
        List.iter (writeln writer) obj.members
        List.iter (writeln writer) obj.methods
        writeln writer "end"
        unindent writer

    let writeUnion (writer: IndentedTextWriter) isFirst name union =
        writeTypeDefPreamble writer isFirst name " ="
        indent writer
        List.iter (fun unioncase -> writeln writer (sprintf "| %s" unioncase)) union
        unindent writer

    let writeRecord (writer: IndentedTextWriter) isFirst name record =
        if List.isEmpty record
        then
            (* F# doesn't allow empty record *)
            writeTypeDefPreamble writer isFirst name " = unit"
        else
            writeTypeDefPreamble writer isFirst name " = {"
            indent writer
            List.iter (fun (field, fieldType) -> writeln writer (sprintf "%s : %s" field fieldType)) record
            writeln writer "}"
            unindent writer

    let writeTypeDef (writer: IndentedTextWriter) isFirst (name, typeDef) =
        match typeDef with
        | Union u -> writeUnion writer isFirst name u
        | Object o -> writeObject writer isFirst name o
        | Record r -> writeRecord writer isFirst name r

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
        writeContents writer content
        if not eventStyleApi
        then
            let init, _ = cfsm
            writeln writer (sprintf "let init = %s" (mkStateName init))
        else
            writeln writer "let run (callbacks : Callbacks) = failwith \"TODO\""
        writer.Flush()
        ()
