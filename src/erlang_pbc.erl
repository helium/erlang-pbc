-module(erlang_pbc).
-export([group_new/1, group_order/1, element_new/2, element_to_string/1, element_random/1, element_add/2, element_sub/2, element_mul/2, element_div/2, element_pow/2, element_set/2, element_from_hash/2, element_to_binary/1, binary_to_element/2, element_cmp/2, element_pairing/2]).
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

element_set(E, X) when is_integer(X) ->
    element_set_mpz_nif(E, pack_int(X)).

element_pow(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_pow_mpz(E, pack_int(X));
element_pow(E, X) ->
    element_pow_zn(E, X).

element_add(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_add_nif(E, element_set(E, X));
element_add(E, X) ->
    element_add_nif(E, X).

element_mul(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_mul_mpz_nif(E, pack_int(X));
element_mul(E, X) ->
    element_mul_nif(E, X).

element_sub(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_sub_nif(E, pack_int(X));
element_sub(E, X) ->
    element_sub_nif(E, X).

element_div(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_div_nif(E, pack_int(X));
element_div(E, X) ->
    element_div_nif(E, X).

element_from_hash(E, {digest, Bin}) when is_binary(Bin) ->
    %% already a hash, trust the user knows what they're doing
    element_from_hash_nif(E, Bin);
element_from_hash(E, Bin) when is_binary(Bin) ->
    %% ok, we need to hash it in some magic way
    %% TODO charm uses the first 2 bytes to hold block size and hash prefix
    Order = group_order(E),
    case Order < 256 of
        true ->
            %% ok, we have enough bits in the hash to satisfy the group order
            element_from_hash_nif(E, crypto:hash(sha256, Bin));
        false ->
            %% TODO apply variable size hash technique
            erlang:error(not_implemented_yet)
    end.

pack_int(X) ->
    Int = pack_int(abs(X), []),
    %% first byte is a sign byte
    Sign = case X < 0 of
               true ->
                   16#ff;
               false ->
                   0
           end,
    <<Sign:8/integer-unsigned, Int/binary>>.

pack_int(X, Acc) when X < 4294967296 ->
    list_to_binary([<<X:32/integer-unsigned-big>>|Acc]);
pack_int(X, Acc) ->
    Y = X bsr 32,
    Z = X band 16#ffffffff,
    pack_int(Y, [<<Z:32/integer-unsigned-big>>|Acc]).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

group_order(_) ->
    not_loaded(?LINE).

group_new_nif(_) ->
    not_loaded(?LINE).

element_new(_, _) ->
    not_loaded(?LINE).

element_to_string(_) ->
    not_loaded(?LINE).

element_random(_) ->
    not_loaded(?LINE).

element_add_nif(_, _) ->
    not_loaded(?LINE).

element_sub_nif(_, _) ->
    not_loaded(?LINE).

element_mul_nif(_, _) ->
    not_loaded(?LINE).

element_mul_mpz_nif(_, _) ->
    not_loaded(?LINE).

element_div_nif(_, _) ->
    not_loaded(?LINE).

element_pow_zn(_, _) ->
    not_loaded(?LINE).
    
element_pow_mpz(_, _) ->
    not_loaded(?LINE).

element_set_mpz_nif(_, _) ->
    not_loaded(?LINE).

element_from_hash_nif(_, _) ->
    not_loaded(?LINE).

element_to_binary(_) ->
    not_loaded(?LINE).

binary_to_element(_, _) ->
    not_loaded(?LINE).

element_cmp(_, _) ->
    not_loaded(?LINE).

element_pairing(_, _) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
