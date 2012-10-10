%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).

-include("couchbeam.hrl").

-spec encode(ejson()) -> binary().

%%
%% What? This seems like a recipe for disaster. Two different formats accepted? Lets just do the one and true
%%

encode(D) ->
    jsx:encode(D).

decode(D) ->
    jsx:decode(D).
    


