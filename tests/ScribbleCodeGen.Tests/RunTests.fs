namespace ScribbleCodeGen.Tests

open Expecto
open System.Text.RegularExpressions
open System.IO
open ScribbleCodeGen

module Tests = 
    let fixQuotes stuff =
        (* DotParser has issues parsing escaped quotes, we replace them with single quotes *)
        (* This can be removed after https://github.com/auduchinok/DotParser/pull/6 is merged *)
        Regex.Replace(stuff, "\\\\\"", "$")

    let tmpPath = Path.GetTempPath()

    let count = ref 1
    let runCodeGen content protocol localRole codeGenMode recursiveRefinement outFileName = 
        let tmpFile = sprintf "%s/%s" tmpPath outFileName
        CodePrinter.fileName := tmpFile
        count := !count + 1
        let content = fixQuotes content
        Library.processScribbleOutput content protocol localRole codeGenMode recursiveRefinement
        tmpFile
    
    let cmpContent expected actual =
        let exp = File.ReadAllText(expected)
        let act = File.ReadAllText(actual)
        Expect.equal act exp "Output should match expectation files"

    let discoverTests () =
        let rec findInPath path : seq<string> =
            let dirs = Directory.GetDirectories(path)
            let nested = Seq.collect findInPath dirs 
            let files = Directory.GetFiles(path)
            let inputFiles = Seq.filter (fun (f : string) -> Path.GetExtension(f) = ".in") files
            Seq.append nested inputFiles
        let oldTestFiles = findInPath "tests/resources/ext-annot"
        let newTestFiles = findInPath "tests/resources/dev-rhu1-assrt"
        printfn "Found test files %A %A" oldTestFiles newTestFiles
        oldTestFiles, newTestFiles

    let makeTestCase files recursiveRefinement =
        let makeSingleTestCase (f:string) codeGenMode = 
            let dirName = Path.GetDirectoryName(f)
            let filename = Path.GetFileNameWithoutExtension(f)
            let protocol = Seq.takeWhile ((<>) '_') filename |> Seq.map string |> String.concat ""
            let localRole = Seq.skipWhile ((<>) '_') filename |> Seq.tail |> Seq.map string |> String.concat ""
            let content = File.ReadAllText(f)
            let outExtension = if codeGenMode = FStar then ".fst" else ".fs"
            let testCaseName = if codeGenMode = FStar then "F*" else "F#"
            testCase testCaseName <| fun () -> 
                let outFileName = sprintf "%s%s%s%s" "Generated" protocol localRole outExtension
                let expected = sprintf "%s/%s" dirName outFileName
                let actual = runCodeGen content protocol localRole codeGenMode recursiveRefinement outFileName
                cmpContent expected actual
        let makeTestCasesForFile f =
            testSequenced <| testList f [
                makeSingleTestCase f FStar;
                makeSingleTestCase f EventApi;
            ]
        Seq.map makeTestCasesForFile files |> List.ofSeq

    [<Tests>]
    let test =
        let oldTests, newTests = discoverTests ()
        testSequenced <| testList "All" [
            testList "Old Scribble" (makeTestCase oldTests false)
            testList "New Scribble" (makeTestCase newTests true)
        ]

module RunTests =

    [<EntryPoint>]
    let main args =
        runTestsInAssembly defaultConfig args

