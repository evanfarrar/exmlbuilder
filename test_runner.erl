-module(test_runner).
-export([run/1, test/0, pretty_print/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Start Tests                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
  "hey" = drop_beam_extension("hey.beam"),
  horray = test_promise(),
  horray = test_collect_results(),
  horray.
  
test_promise() ->
  promise((fun() -> sleep(5000), second end), self()),
  promise((fun() -> sleep(1000), first end), self()),
  receive
    first -> horray;
    second -> exit(failed)
  end.

test_collect_results() ->
  promise((fun() -> {pass, blah} end), self()),
  promise((fun() -> sleep(1000), {fail, blah} end), self()),
  [{fail, blah}, {pass, blah}] = collect_results(2),
  horray.

%% This is really only used for testing
sleep(T) ->
  receive
  after T -> void
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   End Tests                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

drop_beam_extension("maeb."++String) -> lists:reverse(String);
drop_beam_extension(String) -> drop_beam_extension(lists:reverse(String)).


promise(Funct, From) ->
  spawn(fun() ->  promise_bg(Funct,From) end).

promise_bg(Funct,From) -> From ! Funct().

run(all) ->
  {ok,List} = file:list_dir("./"),  
  BeamFiles = lists:filter(fun(Name) -> lists:suffix("beam",Name) end, List),
  ModuleNames = lists:map(fun drop_beam_extension/1, BeamFiles),
  Run_in_bg = fun(Module) -> promise(fun() -> run(Module) end, self()) end, 
  lists:map(Run_in_bg, ModuleNames),
  collect_results(length(ModuleNames));
run(SingleTest) when is_list(SingleTest) ->
  run(list_to_atom(SingleTest));
run(SingleTest) ->
  try apply(SingleTest, test, []) of
    horray -> {pass, SingleTest};
    _ -> {fail, SingleTest}
  catch
    _:_ -> {fail, SingleTest}
  end.
  
collect_results(Number) ->
  collect_results(Number, []).

collect_results(0, Acc) -> Acc;
collect_results(Num, Acc) -> 
  Res = receive
    {fail, Module} -> {fail, Module};
    {pass, Module} -> {pass, Module}
  end,
  collect_results(Num-1, [Res|Acc]).


pretty_print({Status, Module}) ->
  io:format("~20s:", [Module]),
  io:format("~s\n", [Status]);
pretty_print(StatusTuplesList) ->
  io:format("Test results:\n\n"),
  lists:map(fun pretty_print/1, StatusTuplesList),
  Results = lists:map(fun({Status,_}) -> Status end, StatusTuplesList),
  Failures = lists:filter(fun(E) -> E == fail end,Results),
  io:format("\n~p tests, ~p failures\n\n", [length(Results), length(Failures)]).
