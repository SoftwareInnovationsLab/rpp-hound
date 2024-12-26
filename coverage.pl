:- use_module(library(pita)).
:- pita.
:- begin_lpad.

% Convert the intra-procedural CFG edges into inter-procedural edges
call_edge(X,Y) :- calls(_,_,X), cfg_edge(X,Y).

% Collect all execution paths.
show_path(Start,End,Path) :- show_path(Start,End,[0],[],Path).

% Hit the end of the path
show_path(X,X,[C|_],T,Path) :- reverse([(X,C)|T],Path).

% Hit the depth limit
show_path(X,_,[C|_],T,Path) :- \+db(less_than_depth(T)), reverse([(X,C)|T],Path).

% Standard intra-procedural CFG edge
show_path(X,Z,[C|Calls],T,Path) :- 
    db(less_than_depth(T)),
    cfg_edge(X,Y), 
    \+call_edge(X,Y), 
    not(member((X,C),T)), 
    show_path(Y,Z,[C|Calls],[(X,C)|T],Path).

% Inter-procedural edge; update the callsite context
show_path(X,Z,[C|Calls],T,Path) :- 
    db(less_than_depth(T)),
    calls(_,Y,X), 
    not(member((X,C),T)), 
    show_path(Y,Z,[X,C|Calls],[(X,C)|T],Path).

% Inter-procedural back-edge; update the callsite context
show_path(X,Z,[C|Calls],T,Path) :- 
    db(less_than_depth(T)),
    calls(_,M,C), 
    returns(M,X), 
    cfg_edge(C,Y), 
    not(member((X,C),T)), 
    show_path(Y,Z,Calls,[(X,C)|T],Path).

% All loops are treated as a single path.
show_path(X,Z,[C|Calls],T,Path) :- 
    db(less_than_depth(T)),
    in_loop_cond(X,L),
    loop(L),
    branch(L,Y,1),
    not(member((L,C),T)),
    show_path(Y,Z,[C|Calls],[(L,C)|T],Path).
 
show_path(X,Z,[C|Calls],T,Path) :- 
    db(less_than_depth(T)),
    in_loop_cond(X,L),
    loop(L),
    branch(L,Y,0),
    member((L,C),T),
    show_path(Y,Z,[C|Calls],[(L,C)|T],Path).

:- end_lpad.

less_than_depth(List) :- depth_limit(Limit), length(List, Len), Len < Limit.

% setup output directory
create_output_directory :- output_dir(Dir), (exists_directory(Dir) -> true ; make_directory(Dir)).

create_output_stream(Index, Stream) :-
    output_dir(Dir),
    atomics_to_string([Dir, "path_", Index], Path),
    open(Path, write, Stream).

% Calculate path probabilities and extract branch conditions for N rarest paths.
% Each path will begin and end with the main function entry and exit.
find_rare_paths(N,Total) :- find_rare_paths(N,"main",Total).
find_rare_paths(N,Func,Total) :- 
    create_output_directory,
    prob(method(Start,Func),_),
    prob(returns(Start,End),_),
    findall((Prob,Path), prob(show_path(Start,End,Path),Prob), Res), 
    length(Res,Total),
    sort(Res,Sorted), 
    get_rare_paths(Sorted,0,N).
    
% Iterates through rare paths 1 through N and writes the branch predicates to a file.
get_rare_paths([(Prob,Path)|PathsTail],Index,N) :-
    % Prob < 0.05,
    create_output_stream(Index, Stream),
    % write(Stream, "Path probability: "),
    write(Stream, Prob),
    write(Stream, "\n"),
    get_path_conditions(Path, Stream),
    close(Stream),
    NextIndex is Index+1, 
    get_rare_paths(PathsTail,NextIndex,N),
    fail.
get_rare_paths(_,_,_).

write_tf(C, Stream) :- C = 1, write(Stream, "T\n").
write_tf(C, Stream) :- C = 0, write(Stream, "F\n").

write_trace(Node, C, Stream) :-
    prob(location(Node,File,_,Line),_),
    atomics_to_string([File, ",", Line, ","], Output),
    write(Stream, Output),
    write_tf(C, Stream).
write_trace(_,_,_,_).

get_path_conditions([(Node,Ctx)|Tail], Stream) :-
    prob(returns(_,Node),Prob), 
    Prob > 0 ->
    write(Stream, "_,_,Return\n"),
    get_path_conditions(Tail, Stream)
    ;
    member((A,Ctx), Tail), 
    prob(branch(Node,A,C),Prob), 
    Prob > 0
    ->
    write_trace(Node, C, Stream),
    get_path_conditions(Tail, Stream)
    ;
    get_path_conditions(Tail, Stream),
    fail.

get_path_conditions([_],_).
get_path_conditions(_,_).
