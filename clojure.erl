-module(clojure).
-compile(export_all).

is_hash([{_, _}]) ->
  true;
is_hash([{_, _}|T]) ->
  is_hash(T).

is_digit($0) -> true;
is_digit($1) -> true;
is_digit($2) -> true;
is_digit($3) -> true;
is_digit($4) -> true;
is_digit($5) -> true;
is_digit($6) -> true;
is_digit($7) -> true;
is_digit($8) -> true;
is_digit($9) -> true;
is_digit(_) -> false.

read_string_dispatch_unicode(T) ->
  read_string_dispatch_unicode(T, "").

read_string_dispatch_unicode(T, X) when length(X) == 4 ->
  {I, []} = string:to_integer(X),
  {I, T};
read_string_dispatch_unicode([H|T], X) ->
  case is_digit(H) of
    true  ->
      read_string_dispatch_unicode(T, X ++ [H]);
    false ->
      {I, []} = string:to_integer(X),
      {I, [H|T]}
  end.

read_string_dispatch([$u|T]) ->
  read_string_dispatch_unicode(T);
read_string_dispatch([$n|T]) ->
  {$\n, T};
read_string_dispatch([$t|T]) ->
  {$\t, T};
read_string_dispatch([$r|T]) ->
  {$\r, T};
read_string_dispatch([$b|T]) ->
  {$\b, T};
read_string_dispatch([$a|T]) ->
  {$\a, T};
read_string_dispatch([$v|T]) ->
  {$\v, T};
read_string_dispatch([H|T]) ->
  {H, T}.

read_string(X) -> read_string(X, "").
read_string([$"|T], R) -> {R, T};
read_string([$\\|T], R) ->
  {H, NT} = read_string_dispatch(T),
  read_string(NT, R ++ [H]);
read_string([H|T], R) ->
  read_string(T, R ++ [H]).

base_convert(Base, Num) when is_integer(Base) ->
  base_convert(integer_to_list(Base), Num);
base_convert(Base, [$-|Num]) when is_list(Base) ->
  -base_convert(Base, Num);
base_convert(Base, Num) when is_list(Base) ->
  Format = "~" ++ Base ++ "u",
  {ok, [Res], []} = io_lib:fread(Format, Num),
  Res.

read_4hex(T) -> read_4hex(T, "").
read_4hex(T, R) when length(R) =:= 4 ->
  {base_convert(16, R), T};
read_4hex([H|T], R) when H >= $0, H =< $9; H >= $A, H =< $F; H >= $a, H =< $f ->
  read_4hex(T, R ++ [H]).

read_octal(T) -> read_octal(T, "").
read_octal([H|T], R) when H >= $0, H < $8 ->
  read_octal(T, R ++ [H]);
read_octal(T, R) when length(R) > 0 ->
  {base_convert(8, R), T}.

parse_baseint(T) ->
  parse_baseint(T, []).

parse_baseint([H|T], R) when H >= $0, H =< $9; H >= $a, H =< $v; H >= $A, H =< $V ->
  parse_baseint(T, R ++ [H]);
parse_baseint(T, R) ->
  {R, T}.

parse_int(T) ->
  parse_int(T, []).

parse_int([H|T], R) when H >= $0, H =< $9 ->
  parse_int(T, R ++ [H]);
parse_int(T, R) ->
  {R, T}.

parse_mantissa([$+|T]) ->
  parse_mantissa(T);
parse_mantissa([$-|T]) ->
  {M, NT} = parse_mantissa(T, []),
  {math:pow(10, -M), NT};
parse_mantissa(T) ->
  {M, NT} = parse_mantissa(T, []),
  {math:pow(10, M), NT}.

parse_mantissa([$M|T], R) ->
  {base_convert(10, R) ,T};
parse_mantissa([H|T], R) when H >= $0, H =< $9 ->
  parse_mantissa(T, R ++ [H]);
parse_mantissa(T, R) ->
  {base_convert(10, R), T}.

parse_num(T, [$+]) -> parse_num(T, []);


parse_num([$0, $X|T], [$-]) -> parse_num([$x|T], [$-]);
parse_num([$0, $X|T], []) -> parse_num([$x|T], []);
parse_num([$x|T], R) ->
  {N, NT} = read_4hex(T),
  case R of
    []    -> {N, NT};
    [$-]  -> {-N, NT}
  end;


parse_num([$0|T], "-") ->
  {N, NT} = read_octal(T),
  {-N, NT};
parse_num(T, "0") ->
  read_octal(T);


parse_num([$R|T], R) -> parse_num([$r|T], R);
parse_num([$r|T], R) ->
  {I, NT} = parse_baseint(T),
  case R of
    [$-|NR] -> {-base_convert(base_convert(10, NR), I), NT};
    _       -> {base_convert(base_convert(10, R), I), NT}
  end;


parse_num([$n|T], R) -> {base_convert(10, R), T};
parse_num([$N|T], R) -> {base_convert(10, R), T};


parse_num([$E|T], R) ->
  parse_num([$e|T], R);
parse_num([$e|T], R) ->
  {M, NT} = parse_mantissa(T),
  {base_convert(10, R) * M, NT};

parse_num([$.|T], R) ->
  {D, NT} = parse_int(T),
  {ok, [Float], []} = io_lib:fread("~f", R ++ [$.|D]),

  case NT of
    [$M|Trail] -> {Float, Trail};
    _ -> {Float, NT}
  end;


parse_num([$/, $-|T], R) ->
  {DS, NT} = parse_int(T),
  {{ratio, base_convert(10, R), -base_convert(10, DS)}, NT};
parse_num([$/|T], R) ->
  {DS, NT} = parse_int(T),
  {{ratio, base_convert(10, R), base_convert(10, DS)}, NT};


parse_num([], R) ->
  {base_convert(10, R), []};
parse_num([H|T], R) when H >= $0, H =< $9 ->
  parse_num(T, R ++ [H]);
parse_num(T, R) ->
  {base_convert(10, R), T}.

parse_keyword(T) -> parse_keyword(T, []).
parse_keyword([H|T], R) when H =:= $\ ; H =:= $,; H =:= $"; H =:= ${; H =:= $}; H =:= $(;
    H =:= $); H =:= $[; H =:= $]; H =:= $#; H =:= $'; H =:= $'; H =:= $^; H =:= $@; H =:= $`;
    H =:= $~; H =:= $\\; H =:= $;; H =:= $\n; H =:= $\r; H =:= $\t ->
  {list_to_atom(R), [H|T]};
parse_keyword([], R) ->  {list_to_atom(R), []};
parse_keyword([H|T], R) ->  parse_keyword(T, R ++ [H]).

parse_char([$u|T]) ->
  try
    {H, NT} = read_4hex(T),
    {[H], NT}
  catch
    _:_ -> {"u", T}
  end;
parse_char([$o|T]) ->
  try
    {H, NT} = read_octal(T),
    {[H], NT}
  catch
    _:_ -> {"o", T}
  end;
parse_char([H|T]) ->
  {[H], T}.

parse_rfc3339(T) ->
  {ok, [Year, Month, Day, Hours, Mins, Secs, Op, ZHours, ZMins], []} =
    io_lib:fread("~d-~d-~dT~d:~d:~d~c~d:~d", T),

  calendar:universal_time_to_local_time(
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hours, Mins, Secs}}) +
        case Op of
          "+" -> -(((ZHours * 60) + ZMins) * 60);
          "-" -> (((ZHours * 60) + ZMins) * 60)
        end)).


skip_spaces([H|T]) when H =:= $\ ; H =:= $,; H =:= $\r; H =:= $\n; H =:= $\t ->
  skip_spaces(T);
skip_spaces(T) -> T.


read_until(T, E) -> read_until(T, E, "").
read_until([H|T], H, R) -> {R, T};
read_until([H|T], E, R) -> read_until(T, E, R ++ [H]).


parse_delim(Text, Delim) -> parse_delim(Text, Delim, []).
parse_delim([D|T], D, V) -> {V, T};
parse_delim(T, D, V) ->
  try
    {E, NT} = decode(T),
    parse_delim(NT, D, V ++ [E])
  catch
    _:_ ->
      [D|NNT] = skip_spaces(T),
      {V, NNT}
  end.


array_to_hash(V) -> array_to_hash(V, []).
array_to_hash([K, V|R], H) ->
  array_to_hash(R, H ++ [{K, V}]);
array_to_hash([], H) ->
  dict:from_list(H).


read_re(T) -> read_re(T, []).
read_re([$"|T], Re) ->
  {ok, MP} = re:compile(Re, [unicode, extended]),
  {MP, T};
read_re([$\\, $"|T], Re) -> read_re(T, Re ++ [$"]);
read_re([H|T], Re) -> read_re(T, Re ++ [H]).


dispatch([$i, $n, $s, $t|T]) ->
  [$"|NT] = skip_spaces(T),
  {H, NNT} = read_until(NT, $"),
  {parse_rfc3339(H), NNT};

dispatch([${|T]) ->
  {Set, NT} = parse_delim(T, $}),
  {sets:from_list(Set), NT};

dispatch([$"|T]) ->
  read_re(T).



decode([$\ |T]) ->
  decode(T);
decode([$,|T]) ->
  decode(T);
decode([$\r|T]) ->
  decode(T);
decode([$\n|T]) ->
  decode(T);
decode([$\t|T]) ->
  decode(T);
decode([$"|T]) ->
  {S, NT} = read_string(T),
  {term_to_binary(S), NT};
decode([$t, $r, $u, $e|T]) ->
  {true, T};
decode([$f, $a, $l, $s, $e|T]) ->
  {false, T};
decode([$n, $i, $l|T]) ->
  {nil, T};
decode([H|T]) when H =:= $-; H =:= $+ ->
  parse_num(T, [H]);
decode([H|T]) when H >= $0, H =< $9 ->
  parse_num([H|T], []);
decode([$:|T]) ->
  parse_keyword(T);
decode([$\\|T]) ->
  parse_char(T);
decode([$[|T]) ->
  parse_delim(T, $]);
decode([$(|T]) ->
  {V, NT} = parse_delim(T, $)),
  {list_to_tuple(V), NT};
decode([${|T]) ->
  {V, NT} = parse_delim(T, $}),
  {array_to_hash(V), NT};
decode([$#|T]) ->
  dispatch(T).

parse(T) ->
  {Res, Rest} = decode(T),
  [] = skip_spaces(Rest),
  Res.

