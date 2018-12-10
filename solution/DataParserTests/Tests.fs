namespace DataParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open DataParser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidInputReturnsADataEntry () =
        let input = "fit in ice cubes 1.0"
        let expected = ("fit in", "ice cubes", float 1.0)
        let result = parseData input
        match result with
        | Some warr ->
            Assert.AreEqual(expected, warr)
        | None ->
            Assert.IsTrue false
