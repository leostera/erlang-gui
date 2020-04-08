-module(eu_quadtree).

-compile([export_all]).

new({X, Y}) -> new(#{ x_min => 0, x_max => X
                    , y_min => 0, y_max => Y
                    });
new(#{ x_min := X0, x_max := X1
     , y_min := Y0, y_max := Y1
     }=Limits)
  when is_number(X0) and is_number(X1)
   and is_number(Y0) and is_number(Y1) ->

  Quadrants = #{ 0 => {{X0, X0+X1/2}, {Y0, Y0+Y1/2}}
               , 1 => {{X0, X0+X1/2}, {Y0+Y1/2, Y0+Y1}}
               , 2 => {{X0+X1/2, X0+X1}, {Y0, Y0+Y1/2}}
               , 3 => {{X0+X1/2, X0+X1}, {Y0+Y1/2, Y0+Y1}}
               },

  Limits#{ quadrants => Quadrants }.

quadrant(Idx, #{ quadrants := Qs}) when (0 =< Idx) and (Idx =< 8) -> maps:get(Idx, Qs).

quadrants(#{ quadrants := Q}) -> Q.

member({X, Y}, #{ quadrants := Qs }) ->
  maps:fold(fun (Idx, {{MinX, MaxX}, {MinY, MaxY}}, miss)
                  when (MinX =< X) and (X =< MaxX)
                   and (MinY =< Y) and (Y =< MaxY) -> {hit, Idx};
                  (_, _, Acc) -> Acc
              end, miss, Qs).
