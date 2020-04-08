-module(eu_rect).

-compile([export_all]).

make(X, Y, W, H)
  when is_number(X) and is_number(Y)
   and is_number(W) and is_number(H) ->
  {ok, {{X, Y}, {W, H}}}.

is_point_within({{X, Y}, {W, H}}, {Px, Py})
  when is_number(X) and is_number(Y)
   and is_number(W) and is_number(H)
   and is_number(Px) and is_number(Py)
   and (W >= 0) and (H >= 0) ->
  (X =< Px) and (Px =< (X+W)) and (Y =< Py) and (Py =< (Y+W)).
