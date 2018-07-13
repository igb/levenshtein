-module(levenshtein).
-export([levenshtein_distance/2]).

levenshtein_distance(Source, Target)  ->
    Score=lists:append([[X || X <- lists:seq(0, length(Target))]],   [lists:append([Y],[n || X <- lists:seq(1, length(Source) - 1)]) || Y <- lists:seq(1, length(Source))]),
    NewScore=levenshtein_distance_source(Source, Target, Score, 1, 1),
    get_score(length(Source), length(Target), NewScore).


levenshtein_distance_source([H|Source], Target, Score, I, J)->
    NewScore=levenshtein_distance_target(Source, H, Target, Score, I, J),
    levenshtein_distance_source(Source, Target, NewScore, I+1, J);
levenshtein_distance_source([],_,Score, _, _) ->
    Score.
    
levenshtein_distance_target(Source, S, [H|Target], Score, I, J)->    
    ScoreValue=min(min(get_score(I - 1, J, Score) + 1, get_score(I, J - 1, Score) + 1), get_score(I - 1, J - 1, Score) + compare(S, H)),
    NewScore=update_score(I,J, Score,ScoreValue),
    levenshtein_distance_target(Source, S, Target, NewScore, I, J + 1);
levenshtein_distance_target(_, _, [], Score, _, _)->
    Score.
    
get_score(I, J, Score)->
    lists:nth(J+1, lists:nth(I+1, Score)).

update_score(I, J, Score, Value)->
    {IFirst, [IMiddle|ILast]}=lists:split(I, Score),
    {JFirst, JRemainder}=lists:split(J, IMiddle),
    
    case JRemainder of
	[] ->JLast=[];
	_ -> [_|JLast]=JRemainder
    end,
        
    NewJ=lists:append(JFirst,lists:append([Value], JLast)),
    lists:append(IFirst,lists:append([NewJ], ILast)).
    
compare(X,Y)->
    case X==Y of
	true->
	    0;
	false -> 1
    end.
