module FMutant.SampleTests

open NUnit.Framework
open FMutant.Sample

[<TestFixture>]
type SampleTests() =

    [<Test>]
    member _.``add - adds two positive numbers``() = Assert.That(add 2 3, Is.EqualTo(5))

    [<Test>]
    member _.``add - adds zero``() =
        Assert.That(add 10 0, Is.EqualTo(10))
        Assert.That(add 0 0, Is.EqualTo(0))

    [<Test>]
    member _.``add - adds negative numbers``() =
        Assert.That(add -2 -3, Is.EqualTo(-5))
        Assert.That(add 5 -4, Is.EqualTo(1))

    [<Test>]
    member _.``isLessThanConst - returns true for values greater than -9``() =
        Assert.That(isLessThanConst 0, Is.True)
        Assert.That(isLessThanConst 10, Is.True)
        Assert.That(isLessThanConst -8, Is.True)

    [<Test>]
    member _.``isLessThanConst - returns false for values less than or equal to -9``() =
        Assert.That(isLessThanConst -9, Is.False)
        Assert.That(isLessThanConst -10, Is.False)
        Assert.That(isLessThanConst -100, Is.False)

    [<Test>]
    member _.``andOperator - logical AND truth table``() =
        Assert.That(andOperator true true, Is.True)
        Assert.That(andOperator true false, Is.False)
        Assert.That(andOperator false true, Is.False)
        Assert.That(andOperator false false, Is.False)

    [<Test>]
    member _.``alwaysTrue - always returns true regardless of input``() =
        Assert.That(alwaysTrue true, Is.True)
        Assert.That(alwaysTrue false, Is.True)

    [<Test>]
    member _.``division - divides positive numbers``() =
        Assert.That(division 10 2, Is.EqualTo(5))
        Assert.That(division 9 3, Is.EqualTo(3))

    [<Test>]
    member _.``division - divides by 1``() =
        Assert.That(division 42 1, Is.EqualTo(42))

    [<Test>]
    member _.``division - divides by negative number``() =
        Assert.That(division 10 -2, Is.EqualTo(-5))

    [<Test>]
    member _.``division - divides negative numbers``() =
        Assert.That(division -10 -2, Is.EqualTo(5))

    [<Test>]
    member _.``divisionByZero - throws DivideByZeroException``() =
        Assert.Throws<System.DivideByZeroException>(fun () -> divisionByZero 5 |> ignore)
        |> ignore

    [<Test>]
    member _.``multiplication - multiplies positive numbers``() =
        Assert.That(multiplication 2 3, Is.EqualTo(6))
        Assert.That(multiplication 5 5, Is.EqualTo(25))

    [<Test>]
    member _.``multiplication - multiplies by zero``() =
        Assert.That(multiplication 10 0, Is.EqualTo(0))
        Assert.That(multiplication 0 10, Is.EqualTo(0))

    [<Test>]
    member _.``multiplication - multiplies negative numbers``() =
        Assert.That(multiplication -2 -3, Is.EqualTo(6))
        Assert.That(multiplication 5 -3, Is.EqualTo(-15))
        Assert.That(multiplication -4 3, Is.EqualTo(-12))
