%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2015 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Scan and tokenize Modron specifications.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Definitions.
LOWER        = [a-z]
UPPER        = [A-Z]
DIGIT        = [0-9]
WS           = [\s\t\n]+

LCID         = {LOWER}({LOWER}|{DIGIT}|_)*
UCID         = {UPPER}({UPPER}|{DIGIT}|_)*

DOT          = \.{WS}
COMMENT      = \%.*
EOF          = \$end

Rules.
EntryActions : {token, {'EntryActions', TokenLine}}.
ExitActions  : {token, {'ExitActions',  TokenLine}}.

{LCID}       : {token, {lcid, TokenLine, list_to_atom(TokenChars)}}.
{UCID}       : {token, {ucid, TokenLine, list_to_atom(TokenChars)}}.

:            : {token, {':',  TokenLine}}.
->           : {token, {'->', TokenLine}}.
,            : {token, {',',  TokenLine}}.
{DOT}        : {token, {'.',  TokenLine}}.

{WS}         : skip_token.
{COMMENT}    : skip_token.
{EOF}        : {end_token, {'$end', TokenLine}}.

Erlang code.
-export([lex/1]).

-spec lex(string()) -> _.
%% @doc Lexes `File'. See EOF above.
lex(Lines) ->
  case string(Lines ++ "$end") of
    {ok, Tokens, _Line} -> {ok, Tokens};
    {error, Rsn, Line}  -> {error, {Line, Rsn}}
  end.

%%% eof
