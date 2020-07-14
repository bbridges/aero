%%% Bootstrap compiler for the Aero programming language.

-module(aero).

-export([compile/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

compile(InputFile) ->
  InputResult = file:read_file(InputFile),
  TokensResult = tokenize(InputResult),
  AstResult = parse(TokensResult),
  TransformResult = transform(AstResult),
  ExFileResult = write_ex(TransformResult, InputFile),
  ex_compile(ExFileResult).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Wrappers to allow for a compile pipeline operation.

tokenize({ok, Input}) ->
  case aero_lexer:tokenize(Input) of
    {ok, Tokens, _EndLine} -> {ok, Tokens};
    {error, _} = Error -> Error
  end;
tokenize({error, _} = Error) ->
  Error.

parse({ok, Tokens}) -> aero_parser:parse(Tokens);
parse({error, _} = Error) -> Error.

transform({ok, Ast}) -> {ok, aero_transform:transform(Ast)};
transform({error, _} = Error) -> Error.

write_ex({ok, Transformed}, InputFile) ->
  ExDir = filename:join([aero_compile_env:out_dir(), "ex"]),
  ExFile = filename:join([ExDir, filename:basename(InputFile, ".aero") ++ ".ex"]),
  ExSource = 'Elixir.Macro':to_string(Transformed),
  Formatted = 'Elixir.Code':'format_string!'(ExSource),
  case filelib:ensure_dir(ExFile) of
    ok ->
      case file:write_file(ExFile, Formatted) of
        ok -> {ok, ExFile};
        {error, _} = Error -> Error
      end;
    {error, _} = Error -> Error
  end;
write_ex({error, _} = Error, _) ->
  Error.

ex_compile({ok, ExFile}) ->
  BeamDir = filename:join([aero_compile_env:out_dir(), "ebin"]),
  case filelib:ensure_dir(filename:join([BeamDir, "."])) of
    ok ->
      case 'Elixir.Kernel.ParallelCompiler':compile_to_path([list_to_binary(ExFile)],
                                                            list_to_binary(BeamDir)) of
        {ok, _, Warnings} -> {ok, Warnings};
        {error, Errors, Warnings} -> {error, {Errors, Warnings}}
      end;
    {error, _} = Error -> Error
  end;

ex_compile({error, _} = Error) ->
  Error.
