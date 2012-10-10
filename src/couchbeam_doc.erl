%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_doc).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include("couchbeam.hrl").

%%-export([set_value/3, get_value/2, get_value/3,
%%         delete_value/2, extend/2, extend/3]).
-export([get_id/1, get_rev/1, get_idrev/1, is_saved/1]).

%% @spec get_id(Doc::json_obj()) -> binary()
%% @doc get document id.
get_id(Doc) ->
    jsx:get_value(<<"_id">>, Doc).

%% @spec get_rev(Doc::json_obj()) -> binary()
%% @doc get document revision.
get_rev(Doc) ->
    jsx:get_value(<<"_rev">>, Doc).

%% @spec get_idrev(Doc::json_obj()) -> {DocId, DocRev}
%% @doc get  a tuple containing docucment id and revision.
get_idrev(Doc) ->
    DocId = jsx:get_value(<<"_id">>, Doc),
    DocRev = jsx:get_value(<<"_rev">>, Doc),
    {DocId, DocRev}.

%% @spec is_saved(Doc::json_obj()) -> boolean()
%% @doc If document have been saved (revision is defined) return true,
%% else, return false.
is_saved(Doc) ->
    case jsx:get_value(<<"_rev">>, Doc) of
        undefined -> false;
        _ -> true
    end.


