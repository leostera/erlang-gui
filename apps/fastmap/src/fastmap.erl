-module(fastmap).

-export([
         new/0,
         test/0,
         insert/3,
         as_list/1
        ]).

new() -> fastmap_native:new().

insert(M, K, V) -> fastmap_native:insert(M, K, term_to_binary(V)).

as_list(M) -> lists:map( fun ({K, V}) -> {K, erlang:binary_to_term(V)} end
                       , fastmap_native:as_list(M)).

test() ->
  M = new(),
  {FillTime, _ } = timer:tc(fun () ->
  Self = self(),
  Pids = lists:map(
    fun (Idx) ->
        spawn(
          fun () ->
              process_flag(priority, high),
              insert( M
                      , { Idx
                        , Idx+1
                        , Idx+2
                        }
                      , Idx),
              Self ! done
          end)
    end, lists:seq(1, 100000)),
  await_results(length(Pids))
                            end),
  {ListTime, _} = timer:tc(fun () -> as_list(M) end),
  {M, FillTime, ListTime}.

await_results(0) -> ok;
await_results(C) -> receive done -> await_results(C-1) end.
