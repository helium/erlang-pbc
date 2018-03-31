-module(erlang_pbc).
-export([group_new/1, element_new/2, element_to_string/1, element_random/1, element_add/2, element_sub/2, element_mul/2, element_div/2, element_pow_zn/2]).
-on_load(init/0).

-define(APPNAME, erlang_pbc).
-define(LIBNAME, erlang_pbc).

-define(SS512,
<<"type a
q 8780710799663312522437781984754049815806883199414208211028653399266475630880222957078625179422662221423155858769582317459277713367317481324925129998224791
h 12016012264891146079388821366740534204802954401251311822919615131047207289359704531102844802183906537786776
r 730750818665451621361119245571504901405976559617
exp2 159
exp1 107
sign1 1
sign0 1">>).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
                 {error, bad_name} ->
                     case filelib:is_dir(filename:join(["..", priv])) of
                         true ->
                             filename:join(["..", priv, ?LIBNAME]);
                         _ ->
                             filename:join([priv, ?LIBNAME])
                     end;
                 Dir ->
                     filename:join(Dir, ?LIBNAME)
             end,
    erlang:load_nif(SoName, 0).


group_new('SS512') ->
    group_new_nif(?SS512);
group_new(Other) ->
    group_new_nif(Other).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

group_new_nif(_) ->
    not_loaded(?LINE).

element_new(_, _) ->
    not_loaded(?LINE).

element_to_string(_) ->
    not_loaded(?LINE).

element_random(_) ->
    not_loaded(?LINE).

element_add(_, _) ->
    not_loaded(?LINE).

element_sub(_, _) ->
    not_loaded(?LINE).

element_mul(_, _) ->
    not_loaded(?LINE).

element_div(_, _) ->
    not_loaded(?LINE).

element_pow_zn(_, _) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
