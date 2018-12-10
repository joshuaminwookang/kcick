namespace ProjectParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidQuestionReturnsAQuery () =
        let input = "How many doughnuts are sold in New York City every week?"
        let expected = (HowMany "How many", Main "doughnuts", SoldIn "sold in", Compare ("New York City", " every week" ))
        let result = parse input
        match result with
        | Some warr ->
            Assert.AreEqual(expected, warr)
        | None ->
            Assert.IsTrue false

