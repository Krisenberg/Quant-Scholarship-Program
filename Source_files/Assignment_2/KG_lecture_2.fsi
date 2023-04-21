// Lets define simple library of parsers combinators (https://en.wikipedia.org/wiki/Parser_combinator)
// Main building blocks is a function char seq -> option ('t * char seq)
open System.Runtime.CompilerServices


type Parser<'t> = seq<char> -> option<'t * seq<char>>

// Let's decode the signature: 
//    1. "->" indicates that type of the parser is a function
//    2. Left handside of "->" is the type of argument of the parser function. It's: seq<char>. So the parser will consume a sequence of characters
//    3. Right handside of "->" is the type of the result. It's an option, so we can either get some result of underlying type (the 't * seq<char>) or get None.
//       Optionality of the result corresponds to the success or failure of the Parser processing of the given input
//    4. The inner type of result : 't * seq<char> is a tuple holding result of type 't and the rest of the input (the part that was not consumed by the parser)

// NOTE: Parser<'t> result is an option!. You can use Option.map and option.bind on it
// NOTE: Parser<'t> is function. you Can pipe input parameter into Parser: "fooBar" |> fooBarParser. Or compose it with >> : fooBarParser >> Option.map ( ...) 
// Let's define example:
module Parsers = 
    let pChar c (input: seq<char>) =       
        if Seq.isEmpty input |> not && Seq.head input = c
        then Some (c, Seq.skip 1 input)             
        else None
    

    // function pChar eats given character c from the head of the input
    // NOTE: The result of partially applied function Parsers.char 'a' is of type seq<char> -> option<char * seq<char>> which is Parser<char>!
    // NOTE: we could define it equvalently as 
    let char2 c : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not && Seq.head input = c
            then Some (c, Seq.skip 1 input)             
            else None
    // We can execute it by (we benefit that string is a seq<char>):
// Parsers.pChar 'f' "fooBar";;
// Parsers.pChar 'o' "fooBar";;
    //    val it : (char * seq<char>) option = Some ('f', seq ['o'; 'o'; 'B'; 'a'; ...])

// Excercise 1.
//    a. Define function 'pAny' that eats single (any) character from the input Parser<char>
//    b. Define function 'pDigit' that parses single digit from the input and returns Parser<int>
//    c. Define function 'pSpace' that eats all spaces from the begining of the input input  
//    d. Define function 'pWord w' that tries to parse w at the begining of the input
    let pAny : Parser<char> =  
            fun (input: seq<char>) ->      
                if Seq.isEmpty input |> not
                then Some (Seq.head input, Seq.skip 1 input)             
                else None
    
    let pDigit : Parser<int> =  
            fun (input: seq<char>) ->      
                if Seq.isEmpty input |> not && System.Char.IsDigit (Seq.head input)
                then Some (int (Seq.head input) - int '0', Seq.skip 1 input)             
                else None
    let pSpace (input: seq<char>) =
        if Seq.isEmpty input |> not
            then Some (' ', input |> Seq.skipWhile (fun x -> x = ' '))             
        else None
    let pWord (word: seq<char>) (input: seq<char>) =
        let rec innerHelper w inp =
            if Seq.isEmpty w |> not
                then if Seq.isEmpty inp |> not && Seq.head w = Seq.head inp 
                        then innerHelper(Seq.skip 1 w)(Seq.skip 1 inp)
                        else None
                else Some (word, inp)
        innerHelper word input
        

    

Parsers.pAny "fooBar";;
Parsers.pAny "0xBar";;
Parsers.pDigit "12345";;
Parsers.pSpace "12345";;
Parsers.pSpace "   12345 4";;
Parsers.pSpace " 8  12345 4";;
Parsers.pWord "foo" "fooBar";;


// Till now we defined a few primitive parsers. Next step is to define higher order function that are able to combine them (combinators)
// Here is example

module Combinators = 
    //returns result of first parser if the whole input is sucessfully parsed by the combinator
    let combineL (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  //run the first parser ..
        | Some (result, restOfInput) -> // ... and check the result
            match p2 restOfInput with // if successed run the second parser
            | Some (_, restOfInput2) -> Some (result, restOfInput2) // and if success return result of first parser
            | None -> None // second parser failed
        | _ -> None // first parser failed 

    // combineL combines two parsers and if both succeded returns result of the first one
    // here's usage example:
    //    > let result: Parser<char> = Combinators.combineL (Parsers.pChar 'a') (Parsers.pChar 'b');;
    //    > result "bb";;
    //    val it : (char * seq<char>) option = None
    //    > result "ab";;
    //    val it : (char * seq<char>) option = Some ('a', seq [])  // NOTE: value here is only 'a' but whole input was eaten by the combined parsers
    //    > result "ac";;
    //    val it : (char * seq<char>) option = None

// Excercise 2.
//    a. Define 'combineR p1 p2' similar to combineL but return result of second parser
//    b. Try to rewrite combineR using Option.bind. Hint: Did you use >> ? .
//    c. Define 'combine p1 p2' that produce tuple of result from both parsers
//    e. Is it possible to rewrite comblineL and combine the same way combineR was in part b.?
//    e. Define 'orP p1 p2' which will atempt the first parser and if fail attempt second. or should fail if both p1 and p2 fails 
//    d. Define 'map p f' which will map the result of a praser with function f: Hint: Use Option.map
    let combineR (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with
        | Some (_, restOfInput) -> p2 restOfInput
        | _ -> None

    let combineR2 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        input |> (p1 >> Option.map (fun (x,y) -> y) >> (Option.bind p2))

    let combineR3 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        input |> (p1 >> Option.bind (fun (x,y) -> p2 y))

    let combine (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with
        | Some (result, restOfInput) ->
            match p2 restOfInput with
            | Some (result2, restOfInput2) -> Some ((result, result2), restOfInput2)
            | None -> None
        | _ -> None

    let combineL2 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        let result1 = input |> (p1 >> Option.map (fun (x,y) -> x) >> Option.get)
        input |> (p1 >> Option.map (fun (x,y) -> y) >> (Option.bind p2) >> Option.map (fun (x,y) -> (result1, y)))

    let combine2 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        let result1 = input |> (p1 >> Option.map (fun (x,y) -> x) >> Option.get)
        input |> (p1 >> Option.map (fun (x,y) -> y) >> (Option.bind p2) >> Option.map (fun (x,y) -> ((result1,x), y)))

    let orP (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with
        | Some x -> Some x
        | None -> p2 input
    
    let map (p: Parser<'t1>) f (input: seq<char>) =
        p input |> Option.map (fun (x,y) -> (f x, y))


let result: Parser<char> = Combinators.combineR (Parsers.pChar 'a') (Parsers.pChar 'b');;
result "bb";;
result "ab";;
result "ac";;

/// For clarity let's define set of infix operators for the methods defined above
module Operators = 
    open Parsers
    open Combinators
    let (.>>) : Parser<'a> -> Parser<'b> -> Parser<'a> = combineL
    let (>>.) : Parser<'a> -> Parser<'b> -> Parser<'b> = combineR 
    let (.>>.) : Parser<'a> -> Parser<'b> -> Parser<'a*'b> = combine
    let (|>>) : Parser<'a> -> ('a -> 'b) -> Parser<'b> = map
    let (<|>) : Parser<'a> -> Parser<'a> -> Parser<'a> = orP


// here is an example of operator usage = 
    let addParser : Parser<int> = pDigit .>> pChar '+' .>>. pDigit |>> fun (x,y) -> x + y
// First we parse for a digit followed by a '+' char. Since we use .>> (combineL) the result holds only the parsed digit.
// Next we parse another digit and glue the result into tuple of ints with .>>. (combine combinator). 
// Finally the int tuple is maped through sum function. Let's try to execute the parser:
Operators.addParser "1+9";;
//        val it : (int * seq<char>) option = Some (10, seq [])   

// Excercise 3.
// here is simple gramma of arithmentic expression:
// < expression > ::= < term > + < expression > 
//                  | < term > - < expression > 
//                  | < term >
// < term > ::= < factor > * < term > 
//            | < factor > / < term > 
//            | < factor >
// < factor > ::= (< expression >) | < digit > 
// Write parser that computes the result of arithmetic expression string.
module Expressions = 
    open Parsers
    open Operators

    // here is module with some helper parsers and functions
    [<AutoOpen>]
    module helpers = 
        let pPlus: Parser<_> = pChar '+'
        let pMinus: Parser<_> = pChar '-'
        let pMult: Parser<_> = pChar '*'
        let pDiv: Parser<_> = pChar '/'
        let pOpen: Parser<_> = pChar '('
        let pClose: Parser<_> = pChar ')'


        let sum (a,b) = a+b;     
        let minus (a,b) = a-b;
        let mul (a,b) = a*b;
        let div (a,b) = a/b;
    
    // to help you start of I prepare scaffoling for the solution: 
    let rec expression (input:seq<char>) : option<int * seq<char>>= 
        let pExpression = (term .>> pPlus .>>. expression |>> sum ) // the case for summing term with an expression. It coresponds to < term > + < expression > part of the grammar 
                          <|> (term .>> pMinus .>>. expression |>> minus )// TODO: put a case for a term - expression it should correspond to < term > - < expression > 
                          <|> term// TODO: put a case for single term < term >
        input |> pExpression // here we trigger parsers on the input

    and term (input:seq<char>) : option<int * seq<char>> = 
        let pTerm = (factor .>> pMult .>>. term |>> mul)// TODO: mutliplication 
                    <|> (factor .>> pDiv .>>. term |>> div)// TODO: or divide 
                    <|> factor// TODO: or factor 
        input |> pTerm
    
    and factor  (input:seq<char>) : option<int * seq<char>>  =  
        let pFactor = (pOpen >>. expression .>> pClose) // TODO: expression in parenthesis or a digit
                      <|> pDigit
        input |> pFactor

// Excercise 4:Write some test for different expresions

let testExpressionParser (input:seq<char>)=
    input |> Expressions.expression |> function
        | Some (number, restOfInput) -> printfn "Result of parsing process is: %d" number
        | None -> printfn "Failure during parsing process."

testExpressionParser "1+5"
testExpressionParser "8/2+6"
testExpressionParser "8/(2+6)"
testExpressionParser "1*2*3*4*5"
testExpressionParser "((1+2+3+4*6)*4)/(4*5-5)"
testExpressionParser "(1+2+3+4*6)*4"
testExpressionParser "((1+2+3+4*6)*4)"
testExpressionParser "((1+2+3+4*6)*4)/(3*5)"


// Excercise 5: The parser above is sensitive to whitespaces. "5 + 6" Won't parse. Extent the parse so it ignores whitespaces.
module Expressions2 = 
    open Parsers
    open Operators

    // here is module with some helper parsers and functions
    [<AutoOpen>]
    module helpers = 
        let pPlus: Parser<_> = pChar '+'
        let pMinus: Parser<_> = pChar '-'
        let pMult: Parser<_> = pChar '*'
        let pDiv: Parser<_> = pChar '/'
        let pOpen: Parser<_> = pChar '('
        let pClose: Parser<_> = pChar ')'
        //let pSpace: Parser<_> = pSpace


        let sum (a,b) = a+b;     
        let minus (a,b) = a-b;
        let mul (a,b) = a*b;
        let div (a,b) = a/b;
    

    let parseIgnoringSpaces (grammaElem1: Parser<int>) (grammaElem2: Parser<int>) (midParser: Parser<_>) (operation: (int * int) -> int) : Parser<int> =
        pSpace >>. grammaElem1 .>> pSpace .>> midParser .>> pSpace .>>. grammaElem2 |>> operation

    let rec expression (input:seq<char>) : option<int * seq<char>> = 
        let pExpression = parseIgnoringSpaces term expression pPlus sum 
                          <|> parseIgnoringSpaces term expression pMinus minus 
                          <|> (pSpace >>. term)
        input |> pExpression

    and term (input:seq<char>) : option<int * seq<char>> = 
        let pTerm = parseIgnoringSpaces factor term pMult mul
                    <|> parseIgnoringSpaces factor term pDiv div 
                    <|> (pSpace >>. factor)
        input |> pTerm
    
    and factor  (input:seq<char>) : option<int * seq<char>>  =  
        let pFactor = pSpace >>. pOpen >>. pSpace >>. expression .>> pSpace .>> pClose
                      <|> (pSpace >>. pDigit)
        input |> pFactor

let testExpressionParser2 (input:seq<char>)=
    input |> Expressions2.expression |> function
        | Some (number, restOfInput) -> printfn "Result of parsing process is: %d" number
                                        if Seq.isEmpty restOfInput |> not then printfn "Rest of input is: %s" (string restOfInput)
        | None -> printfn "Failure during parsing process."

testExpressionParser2 "1 + 5"
testExpressionParser2 "  8 / 2  + 6"
testExpressionParser2 "8/ (  2+6  )"
testExpressionParser2 "1 * 2  *     3 * 4 *   5 "
testExpressionParser2 "( (1+2+3+4*6) * 4) / (4*5 - 5)"
testExpressionParser2 "(1+2+3+4*6)*4"
testExpressionParser2 "(( 1+2+3+4*6 )*4)"
testExpressionParser2 "((1+2+3+4*6)*4)/(3*5)"
testExpressionParser2 "5+7foobar"
testExpressionParser2 "5+foo7bar"
testExpressionParser2 " (2+ 2   /2)  *   (( 2 +  (8 * 1) - 8))  - 3 * 2 +   1"



// Excercise 6. There is a bug in the parser above. "5+7foobar" is valid expression. Write parser endInput : Parser<_> which success on empty input. Create safeExpression which combines 
// expression and endInput so that "5+7foobar" is no longer valid input.

let endInput (input: seq<char>): option<unit * seq<char>> =
    if Seq.isEmpty input
        then Some ((), input)
    else None

let safeExpression (input: seq<char>): option<int * seq<char>> =
    input |> (Expressions2.expression >> 
        Option.bind (fun (result, restOfInput) -> Option.map (fun _ -> (result, restOfInput)) (endInput restOfInput))
    )

let testExpressionParser3 (input:seq<char>)=
    input |> safeExpression |> function
        | Some (number, restOfInput) -> printfn "Result of parsing process is: %d" number
                                        if Seq.isEmpty restOfInput |> not then printfn "Rest of input is: %s" (string restOfInput)
        | None -> printfn "Failure during parsing process."

testExpressionParser2 "5+7foobar"
testExpressionParser3 "5+7foobar"