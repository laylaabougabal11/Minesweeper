type Cell= (Int,Int)
data MyState= Null| S Cell [Cell] String MyState deriving (Show,Eq)

up :: MyState -> MyState
up (S (x,y) xs s oldState) = if x-1 < 0 then Null else 
	(S (x-1,y) xs "up" (S (x,y) xs s oldState) )

down :: MyState -> MyState	
down (S (x,y) xs s oldState) = if (x+1) > 3 then Null else
	(S (x+1 , y) xs "down" (S (x,y) xs s oldState) )

left :: MyState -> MyState
left (S (x,y) xs s oldState) = if (y-1) < 0 then Null else
	S (x , y-1) xs "left" (S (x,y) xs s oldState) 
	
right :: MyState -> MyState
right (S (x,y) xs s oldState) = if (y+1) > 3 then Null else
	S (x , y+1) xs "right" (S (x,y) xs s oldState) 




rXY _ [] = []
rXY h (t:ts) =
	if h == t 
	then rXY h ts
	else t : rXY h ts	

collect:: MyState -> MyState
collect (S (x,y) xs s oldState) = 
	if (elem (x,y) xs) == True
	then (S (x,y) (rXY (x,y) xs) "collect" (S (x,y) xs s oldState))
	else Null


stateUp :: MyState -> [MyState]
stateUp (S (x,y) xs s oldState) = if up (S (x,y) xs s oldState) /= Null 
	then [up (S (x,y) xs s oldState)]
	else []

stateDown :: MyState -> [MyState]
stateDown (S (x,y) xs s oldState) = if down (S (x,y) xs s oldState) /= Null 
	then [down (S (x,y) xs s oldState)]
	else []

stateLeft :: MyState -> [MyState]
stateLeft (S (x,y) xs s oldState) = if left (S (x,y) xs s oldState) /= Null 
	then [left (S (x,y) xs s oldState)]
	else []

stateRight :: MyState -> [MyState]
stateRight (S (x,y) xs s oldState) = if right (S (x,y) xs s oldState) /= Null 
	then [right (S (x,y) xs s oldState)]
	else []

stateCollect :: MyState -> [MyState]
stateCollect (S (x,y) xs s oldState) = if collect (S (x,y) xs s oldState) /= Null 
	then [collect (S (x,y) xs s oldState)]
	else []

	
nextMyStates :: MyState->[MyState]
nextMyStates myState = stateUp myState++stateDown myState++stateLeft myState++stateRight myState++stateCollect myState
	

isGoal::MyState->Bool
isGoal (S (x,y) xs s myState) = if length xs == 0 then True else False
 
 
search::[MyState]->MyState
search [] = Null
search (h:t) = 
	if isGoal(h) == True 	
	then h
	else search(t++nextMyStates h)
		

constructSolution:: MyState ->[String]
constructSolution Null= []
constructSolution ( S (x,y) xs s oldState) = if s  /= "" 
	then s : constructSolution (oldState)
	else []



solve :: Cell->[Cell]->[String]
solve (x,y) xs = reverse (constructSolution (search (nextMyStates(S (x,y) xs "" Null))))