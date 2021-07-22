-module(prog).
%-import(string, [len/1, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1]).
%-define(ShiftAmount, -1). %Might need this constant global variable
-export([main/0]).

main() ->
	% your code goes here
	TestStr1 = "IBM",
	TestStr2 = "Hello World",
	TestStr3 = "This is a test",
	TestStr4 = "Hi my name is Robbie",
	TestStr5 = "WandaVision",
	TestStr6 = "Abed",
	TestStr7 = "The Mandalorian",
	TestStr8 = "Wow I learned Erlang I think",
	
	TestStrList = [TestStr1,TestStr2,TestStr3,TestStr4,TestStr5,TestStr6,TestStr7,TestStr8],
	
	ShiftAmount = (-1),
	MaxShift = 25,
	SolveNum = 0,
	
	io:fwrite("Original Strings:~n"),
	io:fwrite("~p~n", [TestStrList]),
	
	io:fwrite("~nEncrypted Strings:~n"),
	EncryptedList = encrypt(TestStrList, ShiftAmount),
	io:fwrite("~p~n", [EncryptedList]),
	
	io:fwrite("~nDecrypted Strings:~n"),
	DecryptedList = decrypt(EncryptedList, -ShiftAmount),
	io:fwrite("~p~n", [DecryptedList]),
	
	io:fwrite("~nSolve:~n"),
	SolvedList = EncryptedList,
	io:fwrite("~p~n", [SolvedList]),
	SolvedList = solve(SolvedList, ShiftAmount, MaxShift, SolveNum).
	%io:fwrite("~p~n", [SolvedList]).
	
%encrypt([]) -> [];
%encrypt([H|T]) -> [enShift(H)|encrypt(T)].

% Encrypt
encrypt(TestStrList, ShiftAmount) ->
	lists:map(fun(TestStr) -> pickShift(TestStr, ShiftAmount) end, TestStrList).
% Decrypt
decrypt(EncryptedList, ShiftAmount) ->
	lists:map(fun(TestStr) -> pickShift(TestStr, ShiftAmount) end, EncryptedList).
% Solve
solve(SolvedList, ShiftAmount, MaxShift, SolveNum) ->
	%SolvedList = lists:append(SolvedList, lists:map(fun(TestStr) -> pickShift(TestStr, ShiftAmount) end, SolvedList)),
	NewList = lists:map(fun(TestStr) -> pickShift(TestStr, ShiftAmount) end, SolvedList),
	io:fwrite("~p~n", [NewList]),
	%io:fwrite("~p~n Caesar ", MaxShift - SolveNum, " : ", [EncryptedList]),
	
	if
		%Recursive call if SolveNum does not equal MaxShift Value
		SolveNum /= MaxShift -> solve(NewList, ShiftAmount, MaxShift, (SolveNum + 1));
		%Default to return NewList
		true -> NewList
	end.

% Decide whether shift is positive of negative and then decide which shift function to call
pickShift(TestStr, ShiftAmount) ->
	if
		% If shift amount is negative
		ShiftAmount < 0 -> lists:map(fun(Char) -> negShift(Char, ShiftAmount) end, TestStr);
		% If shift amount is positive
		ShiftAmount > 0 -> lists:map(fun(Char) -> posShift(Char, ShiftAmount) end, TestStr);
		true -> TestStr
	end.

% Shifting if ShiftAmount is a negative value
negShift(Character, ShiftAmount) ->
	%io:fwrite("~c~n", [Character]),
	if
		% shift character based on what the character is
		Character == $A -> Character + ShiftAmount + 26;
		Character == $a -> Character + ShiftAmount + 26;
		% includes everything but A
		(Character > $A) and (Character =< $Z) -> Character + ShiftAmount;
		(Character > $a) and (Character =< $z) -> Character + ShiftAmount;
		true -> Character
	end.
% Shifting if ShiftAmount is a positive value
posShift(Character, ShiftAmount) ->
	if
		% shift character based on what the character is
		Character == $Z -> Character + ShiftAmount - 26;
		Character == $z -> Character + ShiftAmount - 26;
		% includes everything but Z
		((Character >= $A) and (Character < $Z)) -> Character + ShiftAmount;
		((Character >= $a) and (Character < $z)) -> Character + ShiftAmount;
		true -> Character
	end.
