/*
*	print_List -> prints the seats obtained by each party
*/

print_List([]).

print_List([[PartyId,_,Seats,_]|R]):-
	format("Party ~c  -> Seats: ~d", [PartyId,Seats]), nl,
	print_List(R).

/*	
*	print_difference -> prints the difference between the results obtained by the 2 methods
*/

print_difference([],[]).

print_difference([[PartyId,_,Seats, _]|T], [[Seats2]|T2]):-
	X is Seats-Seats2 ,
	X > 0,
	format("Party: ~c ->  Diference: +~d seats(-s)", [PartyId,X]), nl,
	print_difference(T,T2).

print_difference([[PartyId,_,Seats, _]|T], [[Seats2]|T2]):-
	X is Seats-Seats2 ,
	X = 0,
	format("Party: ~c ->  Diference: same", [PartyId,X]), nl,
	print_difference(T,T2).

print_difference([[PartyId,_,Seats, _]|T], [[Seats2]|T2]):-
	X is Seats-Seats2 ,
	X < 0,
	format("Party: ~c ->  Diference: ~d seats(-s)", [PartyId,X]), nl,
	print_difference(T,T2).

/*	
*	max -> searches in a list for the party with the most votes and "returns" the ID of that party
*	In case of a tie, the party with least mandates is chosen.
*						
*/

max([],ID,_,ID,_).

max([[_,Votes,_, _]|T],IDMax,Max,ID,Tie_breaker1):-
	Votes < Max,
	max(T,IDMax,Max,ID,Tie_breaker1).

max([[Party,Votes,Seats,_]|T],IDMax,Max,_,_):- 
	Votes > Max,
	max(T,IDMax,Votes,Party,Seats).

max([[_,Votes,Seats, _]|T],IDMax,Max,ID,Tie_breaker1):- 
	Max = Votes,
	Tie_breaker1 < Seats,
	max(T,IDMax,Max,ID,Tie_breaker1).

max([[Party,Votes,Seats, _]|T],IDMax,Max, _,Tie_breaker1):- 
	Max = Votes,
	Tie_breaker1 > Seats,
	max(T,IDMax,Votes,Party,Seats).

max([[Party,Votes,Seats, _]|T],IDMax,Max,ID,Tie_breaker1):- 
	Max = Votes,
	Tie_breaker1 = Seats,
	X is random(2),
	tie_breaker(X,[[Party,Votes,Seats, _]|T],IDMax,Max,ID,Tie_breaker1).

/*
*	tie_breaker -> If there are party with the same number of votes and seats then the party will be randomly choosen
*	with the help of the variable X
*
*/

tie_breaker(X,[[PartyId,Votes,Seats,_]|T],IDMax,_,_,_):-
	X = 0,
	max(T,IDMax,Votes,PartyId,Seats).
	

tie_breaker(X,[[_,_,_,_]|T],IDMax,Max,ID,tie_breaker):-
	X = 1,
	max(T,IDMax,Max,ID,tie_breaker).

/*
*	set_seats -> After the party with most votes is found the predicate set_seats increments 1 seat to its total.
*				The variable multiplier is 1 for the Hondt method and 2 for Sainte-Laguë.
*				A new list will store the updated values.
*/

set_seats([],_,_,_).


set_seats([[PartyId,Votes,Seats, Divisor]|T],Max,[[PartyId,Votes,Seats, Divisor]|T2],Multiplier):-
	Max =\= PartyId,
	set_seats(T,Max,T2,Multiplier).


set_seats([[PartyId,Votes,Seats, Divisor]|T],Max,[[PartyId,Votes2,Seats2, Divisor2]|T2],Multiplier):-
	Max =:= PartyId,
	VotesTemp is Votes * Divisor,
	Divisor2 is Divisor + 1 * Multiplier,
	Votes2 is VotesTemp/Divisor2,
	Seats2 is Seats + 1,
	set_seats(T,Max,T2,Multiplier).
	
/*
*	restore_list -> after the Hondt method is applied the initial values are restore for each party to 
*					be used with the Sainte-Laguë method
*					The seats obtained by each party with the Hondt method are store in a new list([Seats|T3])
*					to be used later when comparing the results of both methods
*/

restore_list([],_,_).

restore_list([[PartyId,Votes,Seats, Divisor]|T],[[PartyId,Votes2,0,1]|T2], [[Seats]|T3]):-
	Votes2 is Votes * Divisor,
	restore_list(T,T2,T3).

/*
*	saint / hondt -> predicates that apply both methods to the data. 
*					The updated values for each party are stored in a new list(NewList)
*/


saint(List,_,0,Seats_hondt):- 
	write("************************************************************\n"),
	write("Seats according to Sainte-Laguë method"),nl,
	write("************************************************************\n"),
	print_List(List),
	write("************************************************************\n"),
	write("Difference of seats (from Sainte-Laguë method perspective):"),nl,
	write("************************************************************\n"),
	print_difference(List,Seats_hondt).

saint([[PartyId,Votes,Seats,Divisor]|T],PartiesTotal,SeatsTotal,Seats_hondt):-
	SeatsTotal > 0,
	max([[PartyId,Votes,Seats, Divisor]|T],Max,0,PartyId, Seats),
	length(NewList,PartiesTotal),
	set_seats([[PartyId,Votes,Seats,Divisor]|T],Max,NewList,2),
	Y is  SeatsTotal-1,
	saint(NewList, PartiesTotal,Y,Seats_hondt).

hondt(List,PartiesTotal,0,SeatsTotal):- 
	write("********************************************************\n"),
	write("Seats according to d'Hond method"),nl,
	write("********************************************************\n"),
	length(NewList,PartiesTotal),
	length(Seats_hondt,PartiesTotal),
	restore_list(List,NewList,Seats_hondt),
	print_List(List),
	saint(NewList,PartiesTotal,SeatsTotal,Seats_hondt).

hondt([[PartyId,Votes,Seats,Divisor]|T],PartiesTotal,SeatsTotal,Temp):-
	SeatsTotal > 0,
	max([[PartyId,Votes,Seats, Divisor]|T], Max, 0, PartyId, Seats),
	length(NewList,PartiesTotal),
	set_seats([[PartyId,Votes,Seats, Divisor]|T], Max, NewList,1),
	Y is  SeatsTotal-1,
	hondt(NewList, PartiesTotal,Y,Temp).
	
/*
*	validate_votes / validate -> verifies if the values inputed by the user are valid 
*							(integer or float bigger than 0.0)
*				
*/

validate_votes(Value,FinalValue):-
	integer(Value),
	Temp is float(Value),
	validate_votes(Temp, FinalValue).

validate_votes(Value,FinalValue):-
	float(Value),
	Value >= 0.0,
	FinalValue is Value.

validate_votes(Value, FinalValue):-
	float(Value),
	Value < 0.0,
	write('Invalid value!'),nl,
	read(Novo),
	validate_votes(Novo,FinalValue).

validate_votes(Value, FinalValue):-
	\+ float(Value),
	\+integer(Value),
	write('Invalid value!'),nl,
	read(Novo),
	validate_votes(Novo,FinalValue).

validate(Value, FinalValue):-
	integer(Value),
	Value > 0,
	FinalValue is Value.

validate(Value, FinalValue):-
	integer(Value),
	Value =< 0,
	write('Invalid value!'),nl,
	read(Novo),
	validate(Novo,FinalValue).

validate(Value, FinalValue):-
	\+ integer(Value),
	write('Invalid value!'),nl,
	read(Novo),
	validate(Novo,FinalValue).

/*
*	create_list -> Initialize all party with the votes inputed by the user
*/

create_list([],_).

create_list([[PartyId,Votes,0, 1]|T], PartyId) :-
	format('Enter the votes for party ~c?: ', PartyId),
	read(A),
	validate_votes(A, Votes),
	Y is PartyId + 1,
	create_list(T, Y).

seats. :-
	write('Enter the number of parties:'),
	read(Temp),
  	validate(Temp, Parties),
	length(List,Parties),
	write('Enter the number of seats to be contested:'),
	read(Temp2),
	validate(Temp2, Seats),
	create_list(List, 65),
	hondt(List,Parties,Seats,Seats).





