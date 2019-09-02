namespace ScribbleCodeGen

open System.IO
open System.CodeDom.Compiler

open CodeGenCommon
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

    let writeUnionCase (writer: IndentedTextWriter) (tag, fieldTypes, refinement) =
        let refinement =
            match refinement with
            | Some r -> sprintf "[<Refined(\"%s\")>] " r
            | None -> ""
        let fields =
            match fieldTypes with
            | [] -> ""
            | fields -> sprintf " of %s" (String.concat " * " (Seq.ofList fields))
        writeln writer (sprintf "| %s%s%s" tag refinement fields)

    let writeUnion (writer: IndentedTextWriter) isFirst name union =
        writeTypeDefPreamble writer isFirst name " ="
        indent writer
        List.iter (writeUnionCase writer) union
        unindent writer

    let writeRecordItem (writer: IndentedTextWriter) (field, fieldType, refinement) =
        let refinementAttribute =
            match refinement with
            | Some refinement -> sprintf "[<Refined(\"%s\")>] " refinement
            | None -> ""
        writeln writer (sprintf "%s%s : %s" refinementAttribute field fieldType)

    let writeRecord (writer: IndentedTextWriter) isFirst name record =
        if List.isEmpty record
        then
            (* F# doesn't allow empty record *)
            writeTypeDefPreamble writer isFirst name " = unit"
        else
            writeTypeDefPreamble writer isFirst name " = {"
            indent writer
            List.iter (writeRecordItem writer) record
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

    let generateCode (cfsm : CFSM) protocol localRole legacyApi =
        use fileWriter = new StreamWriter(!fileName)
        use writer = new IndentedTextWriter(fileWriter)
        writeln writer (sprintf "module %s%s%s" !moduleName  protocol localRole)
        writeln writer ("(* This file is GENERATED, do not modify manually *)")
        writeln writer ("open FluidTypes.Annotations")
        let content = generateCodeContent cfsm legacyApi localRole
        List.iter (writeContents writer) content
        if legacyApi
        then
            let init, _, _ = cfsm
            writeln writer (sprintf "let init = %s" (mkStateName init))
        else
            (* TODO *)
            writeln writer (sprintf "let run (callbacks : Callbacks%s) = ()" localRole)
        writer.Flush()
        ()
