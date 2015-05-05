// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
#light
open System
     
//Coordinates blocked by metal sheets, edit accordingly.
let sheets = [(0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0);
                (0,1);(1,1);(2,1);(3,1);(4,1);(5,1);(6,1);
                (0,2);(1,2);
                (0,3);(1,3);(2,3);(3,3);(4,3);
                (0,4);(1,4);(4,4);(5,4);(6,4);(7,4);(8,4);
                (0,5);(1,5);
                (4,6);(6,6);(8,6);
                (1,8);(2,8);(3,8);(4,8);(5,8);(6,8);(7,8);(8,8);
                (3,9);(4,9);(5,9);(6,9);(7,9);(8,9)]
     
//Creates a grid based on size
let createGrid x y =
    [for horizontal in 0..x do
        for vertical in 0..y do
            yield (horizontal,vertical)]
     
//Takes a coordinate and generates its neighbours
let generateNeighbours (x,y) domain =
    let directions = [(0,1);(0,-1);(1,0);(-1,0)]
    let temp =
        [for (ax,by) in directions do
            if List.exists (fun elem -> elem = (x+ax,y+by)) domain then
                yield (x+ax,y+by)]
        |> List.filter (fun test -> not(List.exists (fun elem -> elem = test) sheets))
    temp
     
//Pairs every coordinate with neighbours
let pairNeighbours domain =
    let result =
        [for coord in domain do
            if not(List.exists (fun elem -> elem = coord) sheets) then
                yield (coord,generateNeighbours coord domain)]
    result
     
//Returns list of neighbours for a coord
let getNeighbours coord (domain: ((int*int)*(int*int) list) list) =
    let test = domain
            |> List.filter (fun (x,y) -> x = coord)
            |> List.head
    match test with
    | (coord, neighbours) -> neighbours
     
//Returns the first element of a 2-element tuple
let getXYFromPair pair =
    match pair with
    | (coord, neighbours) -> coord
     
//Returns second element of a 2-element tuple
let getSecond pair =
    match pair with
    | (x, y) -> y
     
//Adds an elemnt 'b' to the tail of list 'a'
let rec append a b =
    match a, b with
    | [], ys -> ys
    | x::xs, ys -> x::append xs ys
     
//Retrieves the last element of a list
let rec getLast = function
    | hd :: [] -> hd
    | hd :: tl -> getLast tl
    | _ -> failwith "Empty list."
     
//Returns list with last element removed
let rec trim input =
    let lastElem = getLast input
    let result = List.filter (fun elem -> not(elem = lastElem)) input
    result
           
//Recursive print
let rec printRec = function
    | head :: [] -> printfn "%A" head
    | head :: tail ->
        printfn "%A" head
        printRec tail
    | _ -> failwith "Empty list."
     
//Call this class to solve the maze!    
//Params: (length & height of maze) , (start point) , (end point)  
let mazeSolver xSize ySize (xInput:int, yInput:int) (xGoal:int, yGoal:int) =
    let domain = createGrid xSize ySize
    let searchSpace = pairNeighbours domain
    let input = (xInput, yInput)
    let goal = (xGoal, yGoal)
     
    let mutable openList = [(input,getNeighbours input searchSpace)]
    let mutable visitedList = [input]
    let mutable closedList = [(input,input)]
    let mutable goalWithParent = ((0,0),(0,0)) //Initialised mutable to prevent out of scope in While loop
     
    //Explores around starting coord like an expanding water ripple
    while not(openList.IsEmpty) do
        let mutable current = getLast openList
        openList <- trim openList
        let mutable currentXY = getXYFromPair current
        let mutable neighbours = getNeighbours currentXY searchSpace
     
        for neighbour in neighbours do
            if not(List.exists(fun elem -> elem = neighbour) visitedList) then
                let neighboursOfN = getNeighbours neighbour searchSpace
                openList <- (neighbour, neighboursOfN)::openList
                visitedList <- (neighbour)::visitedList
                closedList <- (neighbour,currentXY)::closedList
            if neighbour = goal then
                goalWithParent <- (neighbour, currentXY)
                openList <- List.empty
           
    //Backtracks from goal
    let mutable currentXY = goalWithParent
    let mutable path = [getXYFromPair currentXY]
    while currentXY <> (input,input) do
        let currentParent = getSecond currentXY
        path <- currentParent::path
        //let mutable index = 0
        //index <- List.findIndex (List.exists(fun (a,b) -> (x,y) = currentParent) closedList) closedList
        let element = List.filter (fun (x,y) -> x = currentParent) closedList
        currentXY <- List.head element
    printfn"Solution from %A to %A" input goal
    printRec path
     
[<EntryPoint>]
let main argv =
    mazeSolver 9 9 (0,8) (7,0)
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

