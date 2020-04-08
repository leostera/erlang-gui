-module(eu_octree).

-compile([export_all]).

new({{X0, X1}, {Y0, Y1}, {Z0, Z1}}=Limits)
  when is_number(X0) and is_number(X1)
   and is_number(Y0) and is_number(Y1)
   and is_number(Z0) and is_number(Z1) ->
  Octants = make_octants(Limits),
  NestedOcts = maps:fold(fun (Idx, Lims, Octs) ->
                             Octs#{ Idx => make_octants(Lims) }
                         end, #{}, Octants),
  #{ limits => Limits
   , octants => NestedOcts };
new({X, Y, Z}) -> new({{0, X}, {0, Y}, {0, Z}}).

limits({Id, _}, #{ octants := Os }) ->
  #{ limits := Limits } = maps:get(Id, Os),
  Limits.

octant(0, S) -> octant({0, 0}, S);
octant({Idx1, Idx2}, #{ octants := Os }) ->
  #{ limits := Limits } = maps:get(Idx1, Os),
  io:format("octant: ~p\n", [Limits]),
  Limits.

octants(#{ octants := O}) -> O.

member({X, Y, Z}, #{ octants := Os }) ->
  maps:fold(
    fun (_, _, {hit, Idxs}) -> {hit, Idxs};
        (Idx, SubOcts, miss) -> maps:fold(
          fun (SubIdx, {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}}, miss)
                when (MinX =< X) and (X =< MaxX)
                 and (MinY =< Y) and (Y =< MaxY)
                 and (MinZ =< Z) and (Z =< MaxZ) -> {hit, {Idx, SubIdx}};
              (_, _, Acc) -> Acc
          end, miss, SubOcts)
    end, miss, Os).

make_octants(Limits={{X0, X1}, {Y0, Y1}, {Z0, Z1}}) ->
  #{ 0 => {{X0, X0+X1/2}, {Y0, Y0+Y1/2}, {Z0, Z0+Z1/2}}
   , 1 => {{X0, X0+X1/2}, {Y0, Y0+Y1/2}, {Z0+Z1/2, Z0+Z1}}

   , 2 => {{X0, X0+X1/2}, {Y0+Y1/2, Y0+Y1}, {Z0, Z0+Z1/2}}
   , 3 => {{X0, X0+X1/2}, {Y0+Y1/2, Y0+Y1}, {Z0+Z1/2, Z0+Z1}}

   , 4 => {{X0+X1/2, X0+X1}, {Y0, Y0+Y1/2}, {Z0, Z0+Z1/2}}
   , 5 => {{X0+X1/2, X0+X1}, {Y0, Y0+Y1/2}, {Z0+Z1/2, Z0+Z1}}

   , 6 => {{X0+X1/2, X0+X1}, {Y0+Y1/2, Y0+Y1}, {Z0, Z0+Z1/2}}
   , 7 => {{X0+X1/2, X0+X1}, {Y0+Y1/2, Y0+Y1}, {Z0+Z1/2, Z0+Z1}}

   , limits => Limits
   }.


linear_idx_to_pair(0 ) -> {0, 0};
linear_idx_to_pair(1 ) -> {0, 1};
linear_idx_to_pair(2 ) -> {0, 2};
linear_idx_to_pair(3 ) -> {0, 3};
linear_idx_to_pair(4 ) -> {0, 4};
linear_idx_to_pair(5 ) -> {0, 5};
linear_idx_to_pair(6 ) -> {0, 6};
linear_idx_to_pair(7 ) -> {0, 7};
linear_idx_to_pair(8 ) -> {1, 0};
linear_idx_to_pair(9 ) -> {1, 1};
linear_idx_to_pair(10) -> {1, 2};
linear_idx_to_pair(11) -> {1, 3};
linear_idx_to_pair(12) -> {1, 4};
linear_idx_to_pair(13) -> {1, 5};
linear_idx_to_pair(14) -> {1, 6};
linear_idx_to_pair(15) -> {1, 7};
linear_idx_to_pair(16) -> {2, 0};
linear_idx_to_pair(17) -> {2, 1};
linear_idx_to_pair(18) -> {2, 2};
linear_idx_to_pair(19) -> {2, 3};
linear_idx_to_pair(20) -> {2, 4};
linear_idx_to_pair(21) -> {2, 5};
linear_idx_to_pair(22) -> {2, 6};
linear_idx_to_pair(23) -> {2, 7};
linear_idx_to_pair(24) -> {3, 0};
linear_idx_to_pair(25) -> {3, 1};
linear_idx_to_pair(26) -> {3, 2};
linear_idx_to_pair(27) -> {3, 3};
linear_idx_to_pair(28) -> {3, 4};
linear_idx_to_pair(29) -> {3, 5};
linear_idx_to_pair(30) -> {3, 6};
linear_idx_to_pair(31) -> {3, 7};
linear_idx_to_pair(32) -> {4, 0};
linear_idx_to_pair(33) -> {4, 1};
linear_idx_to_pair(34) -> {4, 2};
linear_idx_to_pair(35) -> {4, 3};
linear_idx_to_pair(36) -> {4, 4};
linear_idx_to_pair(37) -> {4, 5};
linear_idx_to_pair(38) -> {4, 6};
linear_idx_to_pair(39) -> {4, 7};
linear_idx_to_pair(40) -> {5, 0};
linear_idx_to_pair(41) -> {5, 1};
linear_idx_to_pair(42) -> {5, 2};
linear_idx_to_pair(43) -> {5, 3};
linear_idx_to_pair(44) -> {5, 4};
linear_idx_to_pair(45) -> {5, 5};
linear_idx_to_pair(46) -> {5, 6};
linear_idx_to_pair(47) -> {5, 7};
linear_idx_to_pair(48) -> {6, 0};
linear_idx_to_pair(49) -> {6, 1};
linear_idx_to_pair(50) -> {6, 2};
linear_idx_to_pair(51) -> {6, 3};
linear_idx_to_pair(52) -> {6, 4};
linear_idx_to_pair(53) -> {6, 5};
linear_idx_to_pair(54) -> {6, 6};
linear_idx_to_pair(55) -> {6, 7};
linear_idx_to_pair(56) -> {7, 0};
linear_idx_to_pair(57) -> {7, 1};
linear_idx_to_pair(58) -> {7, 2};
linear_idx_to_pair(59) -> {7, 3};
linear_idx_to_pair(60) -> {7, 4};
linear_idx_to_pair(61) -> {7, 5};
linear_idx_to_pair(62) -> {7, 6};
linear_idx_to_pair(63) -> {7, 7}.
