namespace Homeworks
module Homework1 =
        (* TASK 2 *)
    let fst (x,_,_) = x
    //let fst1 (x,y,z) = x

    let mid (_,y,_) = y
    //let mid1 (x,y,z) = y

    let lst (_,_,z) = z
    //let lst1 (x,y,z) = z

        (* TASK 3 *)

    (*
    let rec factorial n =
        if n < 0 then -1
        else
            if n = 0 || n = 1 then 1
            else n * factorial (n-1)

    let binomialCoefficient n k =
        (factorial n) / ((factorial k) * factorial (n-k))

    let pascal (n: int64) =
        let rec lineProcessing (list: int64 list) =
            match list with
            | [elem] -> [1L]
            | elem1 :: elem2 :: tail -> (elem1 + elem2) :: lineProcessing (elem2 :: tail)
        let rec pascalTemp (n: int64) =
            match n with
            | 0L -> [1L]
            | _ -> 1L :: lineProcessing(pascalTemp (n-1L))
        if n < 0L then []
        else pascalTemp n
    *)

    let rec factorial (n: int64): int64 =
        if n < 0L then -1L
        else
            if n = 0L || n = 1L then 1L
            else n * factorial (n-1L)

    let binomialCoefficient n k =
        (factorial n) / ((factorial k) * factorial (n-k))

    let pascal n =
        let rec pascalTemp k =
            if k = n then [1L]
            else (binomialCoefficient n k) :: pascalTemp (k+1L)
        pascalTemp 0L

        (* TASK 4 *)
    type Faculties = | PPT | IZ | IT | EFM | MAT

    type Student = {StudentName: string; GPA: float; Faculty: Faculties}

    let s1 = {StudentName = "Adam"; GPA = 3.621; Faculty = EFM}
    let s2 = {StudentName = "Jakub"; GPA = 3.273; Faculty = IZ}
    let s3 = {StudentName = "Aleksandra"; GPA = 5.107; Faculty = IT}
    let s4 = {StudentName = "Tomasz"; GPA = 4.942; Faculty = MAT}
    let s5 = {StudentName = "Wiktor"; GPA = 4.013; Faculty = IZ}
    let s6 = {StudentName = "Karolina"; GPA = 3.863; Faculty = EFM}
    let s7 = {StudentName = "Magdalena"; GPA = 4.478; Faculty = MAT}
    let s8 = {StudentName = "Bartlomiej"; GPA = 3.438; Faculty = IT}
    let s9 = {StudentName = "Emilia"; GPA = 3.226; Faculty = EFM}
    let s10 = {StudentName = "Cezary"; GPA = 5.346; Faculty = PPT}

    let exampleList = [s1;s2;s3;s4;s5;s6;s7;s8;s9;s10]

    let rec printIT list =
        match list with
        | [] -> ()
        | head :: tail when head.Faculty = IT -> 
            printf "\n%+A\n" head
            printIT tail
        | head :: tail -> printIT tail

    let calculateAvgGPA list faculty =
        let rec calculateSumNum sum number list =
            match list with
            | [] -> (sum, number)
            | head :: tail when head.Faculty = faculty ->
                calculateSumNum (sum + head.GPA) (number + 1) tail
            | head :: tail -> calculateSumNum sum number tail
        let (sum, number) = calculateSumNum (0.0) 0 list
        sum / (float number)

        (* TASK 5 *)

    let perm (stringList: list<string>) =
        let rec insert elem list =
            match list with
            | [] -> []
            | head :: tail -> (elem :: head) :: insert elem tail 
        let rec permHelper left right =
            match right with
            | [] -> [left]
            | [element] -> insert element (permHelper [] left)
            | head :: tail -> insert head (permHelper [] (left@tail)) @ permHelper (head::left) (tail)
        let rec addNumbers index list =
            match list with
            | [] -> []
            | head :: tail -> (index, head) :: addNumbers (index+1) tail
        addNumbers 1 (permHelper [] stringList)


    let sq = seq { for i in 1..10 do yield 2*i}

    let sq2 = sq |> Seq.iter (fun i -> printfn "%d" i)


