-module(erlang_pbc).
-export([group_new/1, group_order/1, element_new/2, element_to_string/1, element_random/1, element_add/2, element_sub/2, element_mul/2, element_div/2, element_pow/2, element_set/2, element_from_hash/2, element_to_binary/1, binary_to_element/2, element_cmp/2, element_pairing/2, pairing_is_symmetric/1, element_pp_init/1]).
-on_load(init/0).

-type pbc_element() :: reference().
-type group() :: reference().

-export_type([pbc_element/0, group/0]).

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

-define(MNT224,
<<"type d
q 15028799613985034465755506450771565229282832217860390155996483840017
n 15028799613985034465755506450771561352583254744125520639296541195021
h 1
r 15028799613985034465755506450771561352583254744125520639296541195021
a 1871224163624666631860092489128939059944978347142292177323825642096
b 9795501723343380547144152006776653149306466138012730640114125605701
k 6
nk 11522474695025217370062603013790980334538096429455689114222024912184432319228393204650383661781864806076247259556378350541669994344878430136202714945761488385890619925553457668158504202786580559970945936657636855346713598888067516214634859330554634505767198415857150479345944721710356274047707536156296215573412763735135600953865419000398920292535215757291539307525639675204597938919504807427238735811520
hk 51014915936684265604900487195256160848193571244274648855332475661658304506316301006112887177277345010864012988127829655449256424871024500368597989462373813062189274150916552689262852603254011248502356041206544262755481779137398040376281542938513970473990787064615734720
coeff0 11975189258259697166257037825227536931446707944682470951111859446192
coeff1 13433042200347934827742738095249546804006687562088254057411901362771
coeff2 8327464521117791238079105175448122006759863625508043495770887411614
nqr 142721363302176037340346936780070353538541593770301992936740616924">>).

-define(MNT159,
<<"type d
q 625852803282871856053922297323874661378036491717
n 625852803282871856053923088432465995634661283063
h 3
r 208617601094290618684641029477488665211553761021
a 581595782028432961150765424293919699975513269268
b 517921465817243828776542439081147840953753552322
k 6
nk 60094290356408407130984161127310078516360031868417968262992864809623507269833854678414046779817844853757026858774966331434198257512457993293271849043664655146443229029069463392046837830267994222789160047337432075266619082657640364986415435746294498140589844832666082434658532589211525696
hk 1380801711862212484403205699005242141541629761433899149236405232528956996854655261075303661691995273080620762287276051361446528504633283152278831183711301329765591450680250000592437612973269056
coeff0 472731500571015189154958232321864199355792223347
coeff1 352243926696145937581894994871017455453604730246
coeff2 289113341693870057212775990719504267185772707305
nqr 431211441436589568382088865288592347194866189652">>).


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

-spec group_new(atom()) -> group().
group_new('SS512') ->
    group_new_nif(?SS512);
group_new('MNT224') ->
    group_new_nif(?MNT224);
group_new('MNT159') ->
    group_new_nif(?MNT159);
group_new(Other) ->
    group_new_nif(Other).

-spec element_set(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_set(E, X) when is_integer(X) ->
    element_set_mpz_nif(E, pack_int(X)).

-spec element_pow(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_pow(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_pow_mpz(E, element_set(E, X));
element_pow(E, X) ->
    element_pow_zn(E, X).

-spec element_add(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_add(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_add_nif(E, element_set(E, X));
element_add(E, X) ->
    element_add_nif(E, X).

-spec element_mul(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_mul(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_mul_mpz_nif(E, element_set(E, X));
element_mul(E, X) ->
    element_mul_nif(E, X).

-spec element_sub(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_sub(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_sub_nif(E, element_set(E, X));
element_sub(E, X) ->
    element_sub_nif(E, X).

-spec element_div(pbc_element(), pbc_element() | integer()) -> pbc_element().
element_div(E, X) when is_integer(X) ->
    %% TODO pass in a flag if the number is negative
    element_div_nif(E, element_set(E, X));
element_div(E, X) ->
    element_div_nif(E, X).

-spec element_from_hash(pbc_element(), {digest, binary()} | binary()) -> pbc_element().
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

-spec pack_int(integer()) -> binary().
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

-spec pack_int(integer(), list()) -> binary().
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

pairing_is_symmetric(_) ->
    not_loaded(?LINE).

element_pp_init(_) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
